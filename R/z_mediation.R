# file: R/z_mediation.R  (with full roxygen docs)

#' z_mediation: Cox 中介分析（连续 + 可选分位分类），返回可发表表格
#'
#' @description
#' 面向生存结局（Cox）的轻量中介分析封装。
#' - 连续暴露：基于 **regmedint::regmedint(yreg="survCox")** 估计总效应 (TE) 与中介比例 (PM)，并给出 P 值。
#' - 分位分类：可选把暴露按分位数切组（Q1 为参照），给出 Cox 的效应与 `mediation::mediate()` 的间接效应比例。
#'
#' 统一输出为数据框列：`exposure, est_ci, p, med_prop, med_p`
#' （随后用于排版的列名在返回的 `gt` 对象中再映射为论文风格）。
#'
#' @param data `data.frame` 或 `data.table`。分析数据。
#' @param exposure 字符串，暴露列（连续数值）。
#' @param mediator 字符串，中介列。`mreg="linear"` 时为连续，`mreg="logistic"` 时为二元（0/1）。
#' @param time_col 字符串，随访时间列。
#' @param event_col 字符串，事件状态列（1=事件，0=删失）。
#' @param covars 字符向量，协变量。需为数值或二分类；字符/因子会自动转为 0/1（后者按字母序较大者为 1）。
#' @param mreg `c("linear","logistic")`，中介回归类型（对应 `regmedint::mreg`）。
#' @param a0,a1 数值，可选；连续暴露对比的两个取值。默认用 `a_pct` 的分位数。
#' @param a_pct 数值向量，默认 `c(0.25,0.75)`；当 `a0/a1` 缺省时，取该分位数作为对比。
#' @param m_cde 数值，受控直接效应时中介的固定取值；默认取中位数。
#' @param c_cond 命名向量/列表，协变量评估水平（与 `covars` 对齐）；未提供时数值取中位数，二元取 1。
#' @param interaction 逻辑值，是否包含 A×M 交互（传给 `regmedint`）。
#' @param show_hr 逻辑值，是否把 `est_ci` 显示为 HR(95%CI)（默认 FALSE，显示 log-HR）。
#' @param digits 整数，小数位数（默认 2）。
#' @param include_quantiles 逻辑值，是否追加按分位分类（Q1 参照）的结果。
#' @param q 整数，分位数个数（默认 4）。若分位点重复会自动降级（并在 `diag$quantile_used` 记录）。
#'
#' @return 一个列表：
#' \itemize{
#'   \item `table`：数据框，列为 `exposure, est_ci, p, med_prop, med_p`。
#'   \item `gt`：若安装 `gt`，返回美化后的 gt 表（可 `gt::gtsave()` 导出）。
#'   \item `fit_cont`：`regmedint` 连续暴露模型对象。
#'   \item `diag`：诊断信息（样本量、使用的分位、a0/a1、m_cde、c_cond、分组事件数等）。
#'   \item `cat_models`：分位分类各组 `mediation::mediate` 对象（若 `include_quantiles=TRUE`）。
#' }
#'
#' @details
#' - 所有 `rbind` 前统一使用安全列名以避免 `match.names(...)` 异常。
#' - 分位切组时若断点重复导致组数不足，会自动降级直至 ≥2 组；仍不足则跳过分类分析。
#' - `mediation::mediate()` 失败会返回 NA，并在 `diag$mediate_errors_cat` 记录错误。
#' - `est_ci` 的展示由 `show_hr` 控制：`FALSE` 为 log-HR(95%CI)，`TRUE` 为 HR(95%CI)。
#'
#' @section 依赖:
#' 必需：\pkg{survival}、\pkg{regmedint}；若使用分位分类需要 \pkg{mediation}；若希望返回排版表需要 \pkg{gt}。
#'
#' @examples
#' \dontrun{
#' # 连续暴露 + 中介（最小示例）
#' res <- z_mediation(
#'   data      = d,
#'   exposure  = "twa_benzene",
#'   mediator  = "PhenoAge",
#'   time_col  = "AR_time",
#'   event_col = "AR_status",
#'   covars    = c("gender"),
#'   mreg      = "linear",
#'   include_quantiles = TRUE,  # 同时输出 Q1~Q4
#'   q = 4,
#'   show_hr   = FALSE          # TRUE 显示 HR(95%CI)
#' )
#' res$table        # 主表
#' res$gt           # gt 表（可 gtsave 导出）
#' res$diag         # 诊断信息
#' }
#'
#' @export
#' @importFrom survival Surv coxph
#' @importFrom regmedint regmedint
#' @importFrom stats quantile median confint coef pnorm
z_mediation <- function(
    data,
    exposure,
    mediator,
    time_col,
    event_col,
    covars     = NULL,
    mreg       = c("linear","logistic"),
    a0 = NULL, a1 = NULL, a_pct = c(0.25,0.75),
    m_cde = NULL,
    c_cond = NULL,
    interaction = TRUE,
    show_hr = FALSE,
    digits = 2,
    include_quantiles = FALSE,
    q = 4
){
  mreg <- match.arg(mreg)
  
  if ("data.table" %in% class(data)) data <- as.data.frame(data, stringsAsFactors = FALSE)
  stopifnot(is.data.frame(data))
  need <- unique(c(exposure, mediator, covars, time_col, event_col))
  miss <- setdiff(need, names(data)); if (length(miss)) stop("缺少列: ", paste(miss, collapse=", "))
  
  df <- data[, need, drop = FALSE]
  df <- stats::na.omit(df)
  df[[time_col]]  <- as.numeric(df[[time_col]])
  df[[event_col]] <- as.integer(df[[event_col]])
  df[[exposure]]  <- as.numeric(df[[exposure]])
  df[[mediator]]  <- as.numeric(df[[mediator]])
  
  encode01 <- function(v){
    if (is.numeric(v)) return(v)
    v0 <- as.character(v); lv <- sort(unique(v0[!is.na(v0)]))
    if (length(lv)==2) return(as.integer(v0==lv[2]))
    stop("协变量必须为数值或二分类；检测到水平：", paste(lv, collapse="/"))
  }
  if (length(covars)) for (cv in covars) df[[cv]] <- encode01(df[[cv]])
  
  if (is.null(a0) || is.null(a1)) {
    qs <- stats::quantile(df[[exposure]], probs = a_pct, na.rm=TRUE)
    a0 <- if (is.null(a0)) unname(qs[1]) else a0
    a1 <- if (is.null(a1)) unname(qs[2]) else a1
  }
  if (is.null(m_cde)) m_cde <- unname(stats::median(df[[mediator]], na.rm=TRUE))
  if (is.null(c_cond)) {
    c_cond <- lapply(covars, function(cv){
      x <- df[[cv]]
      if (length(unique(x))==2 && all(sort(unique(x)) %in% c(0,1))) return(1)
      stats::median(x, na.rm=TRUE)
    })
    names(c_cond) <- covars
  } else {
    if (is.null(names(c_cond)) || !all(covars %in% names(c_cond)))
      stop("c_cond 必须为命名列表/向量，且包含所有 covars。")
    c_cond <- as.list(c_cond)[covars]
  }
  
  diag <- list(n = nrow(df), a0=a0, a1=a1, a_pct=a_pct, m_cde=m_cde, c_cond=c_cond)
  
  if (!requireNamespace("regmedint", quietly = TRUE))
    stop("需要安装 'regmedint' 包。")
  fit_cont <- regmedint::regmedint(
    data      = df,
    yvar      = time_col,
    eventvar  = event_col,
    avar      = exposure,
    mvar      = mediator,
    cvar      = covars,
    a0        = a0, a1 = a1,
    m_cde     = m_cde,
    c_cond    = unname(unlist(c_cond)),
    mreg      = mreg,
    yreg      = "survCox",
    interaction = isTRUE(interaction),
    casecontrol = FALSE,
    na_omit   = TRUE
  )
  cf <- coef(fit_cont); ci <- stats::confint(fit_cont)
  te <- c(est=unname(cf["te"]), low=unname(ci["te",1]), high=unname(ci["te",2]))
  pm <- c(est=unname(cf["pm"]), low=unname(ci["pm",1]), high=unname(ci["pm",2]))
  se_from_ci <- function(lo, hi) (hi - lo) / (2*1.96)
  p_two      <- function(est, se) 2 * stats::pnorm(-abs(est/se))
  te_p <- p_two(te["est"], se_from_ci(te["low"], te["high"]))
  pm_p <- p_two(pm["est"], se_from_ci(pm["low"], pm["high"]))
  
  fmt_p <- function(p) ifelse(is.na(p), NA, ifelse(p < 0.001, "<0.001", formatC(p, digits=3, format="f")))
  fmt_eff <- function(est, lo, hi, hr=FALSE, digits=digits){
    if (hr) sprintf(paste0("%.",digits,"f (%.",digits,"f, %.",digits,"f)"), exp(est), exp(lo), exp(hi))
    else    sprintf(paste0("%.",digits,"f (%.",digits,"f, %.",digits,"f)"), est, lo, hi)
  }
  
  tbl <- data.frame(
    exposure = "Continuous variable",
    est_ci   = fmt_eff(te["est"], te["low"], te["high"], hr = isTRUE(show_hr), digits=digits),
    p        = fmt_p(te_p),
    med_prop = sprintf(paste0("%.",digits,"f"), pm["est"]*100),
    med_p    = fmt_p(pm_p),
    stringsAsFactors = FALSE
  )
  
  cat_models <- list()
  
  if (isTRUE(include_quantiles)) {
    if (!requireNamespace("mediation", quietly = TRUE))
      stop("分位分类需要安装 'mediation' 包。")
    
    q_use <- q
    repeat {
      brks <- stats::quantile(df[[exposure]], probs = seq(0,1,length.out=q_use+1), na.rm=TRUE)
      brks <- unique(brks)
      if (length(brks) >= 3 || q_use <= 2) break
      q_use <- q_use - 1
    }
    diag$quantile_used <- q_use
    df$exp_cat <- cut(df[[exposure]], breaks=brks, include.lowest=TRUE, right=TRUE)
    if (nlevels(df$exp_cat) >= 2) {
      df$exp_cat <- stats::relevel(df$exp_cat, ref = levels(df$exp_cat)[1])
      levs <- levels(df$exp_cat)
      
      tbl <- rbind(tbl, data.frame(
        exposure = paste0(levels(df$exp_cat)[1], " (Ref.)"),
        est_ci   = "0.00 (Ref.)",
        p        = "1.000",
        med_prop = NA,
        med_p    = NA,
        stringsAsFactors = FALSE
      ))
      
      for (k in 2:length(levs)) {
        lev <- levs[k]
        
        form_te <- stats::as.formula(
          paste0("survival::Surv(", time_col, ",", event_col, ") ~ exp_cat",
                 if (length(covars)) paste0(" + ", paste(covars, collapse=" + ")) else "")
        )
        fit_te <- try(survival::coxph(form_te, data=df, ties="efron"), silent=TRUE)
        beta_ci <- pval <- NA
        if (!inherits(fit_te,"try-error")) {
          sm <- summary(fit_te)$coefficients
          rn <- paste0("exp_cat", lev)
          if (rn %in% rownames(sm)) {
            b  <- sm[rn, "coef"]; se <- sm[rn, "se(coef)"]
            lo <- b - 1.96*se; hi <- b + 1.96*se
            beta_ci <- fmt_eff(b, lo, hi, hr = isTRUE(show_hr), digits=digits)
            pval    <- fmt_p(sm[rn, "Pr(>|z|)"])
          }
        }
        
        m_fit <- stats::lm(stats::as.formula(
          paste0(mediator, " ~ exp_cat",
                 if (length(covars)) paste0(" + ", paste(covars, collapse=" + ")) else "")
        ), data=df)
        y_fit <- try(survival::coxph(
          stats::as.formula(paste0("survival::Surv(", time_col, ",", event_col, ") ~ exp_cat + ",
                                   mediator,
                                   if (length(covars)) paste0(" + ", paste(covars, collapse=" + ")) else "")),
          data=df, ties="efron"), silent=TRUE)
        
        prop <- pmed <- NA
        if (!inherits(y_fit,"try-error")) {
          md <- try(mediation::mediate(m_fit, y_fit,
                                       treat="exp_cat", mediator=mediator,
                                       treat.value=lev, control.value=levs[1],
                                       sims=1000), silent=TRUE)
          if (inherits(md,"try-error")) {
            md <- try(mediation::mediate(m_fit, y_fit,
                                         treat="exp_cat", mediator=mediator,
                                         treat.value=lev, control.value=levs[1],
                                         sims=1000, boot=TRUE), silent=TRUE)
          }
          if (!inherits(md,"try-error")) {
            TE <- md$d0 + md$z0
            if (is.finite(TE) && TE!=0) prop <- 100 * (md$d0 / TE)
            pmed <- md$d0.p
            cat_models[[lev]] <- md
          } else {
            diag$mediate_errors_cat <- c(diag$mediate_errors_cat, paste(lev, ":", as.character(md)))
          }
        }
        
        tbl <- rbind(tbl, data.frame(
          exposure = paste0("Q", k),
          est_ci   = beta_ci,
          p        = pval,
          med_prop = ifelse(is.na(prop), NA, sprintf(paste0("%.",digits,"f"), prop)),
          med_p    = ifelse(is.na(pmed), NA, fmt_p(pmed)),
          stringsAsFactors = FALSE
        ))
      }
      diag$by_cat <- with(df, tapply(df[[event_col]], df$exp_cat, function(z) c(n=length(z), events=sum(z))))
    } else {
      diag$quantile_note <- "分位切割失败（变异度不足），跳过分类分析。"
    }
  }
  
  gt_obj <- NULL
  if (requireNamespace("gt", quietly = TRUE)) {
    hdr2 <- if (isTRUE(show_hr)) "HR (95% CI)" else "\u03B2 (95% CI)"
    tbl_show <- tbl
    names(tbl_show) <- c("Benzene exposure", hdr2, "P value", "Mediation proportion, %", "P value (mediation)")
    gt_obj <- gt::gt(tbl_show) |>
      gt::tab_options(table.font.size = gt::px(12)) |>
      gt::cols_align(columns = gt::everything(), align = "center") |>
      gt::tab_style(style = gt::cell_text(weight = "bold"),
                    locations = gt::cells_column_labels(gt::everything())) |>
      gt::tab_style(style = gt::cell_text(align = "left"),
                    locations = gt::cells_body(columns = 1)) |>
      gt::tab_options(table.border.top.style = "solid",
                      table.border.top.width = gt::px(1),
                      table.border.bottom.style = "solid",
                      table.border.bottom.width = gt::px(1))
  }
  
  list(table = tbl, gt = gt_obj, fit_cont = fit_cont, diag = diag, cat_models = cat_models)
}

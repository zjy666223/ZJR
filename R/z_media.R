# file: R/z_regmed_table.R
#' z_regmed_table: 通用中介分析（regmedint）→ 可发表 gt 表
#'
#' @description
#' 以 \pkg{regmedint} 为引擎，支持 Cox 生存结局与连续/二元中介。
#' 仅需传入关键列名与模型类型，即可输出与论文风格相近的表格。
#'
#' @param data data.frame/data.table，原始数据。
#' @param exposure 暴露列名（连续）。
#' @param mediator 中介列名（连续或二元）。
#' @param yreg 结局回归类型，当前支持 "survCox"。
#' @param time_col 生存时间列（yreg="survCox" 时必填）。
#' @param event_col 生存事件列（0/1；yreg="survCox" 时必填）。
#' @param covars 协变量向量；要求均为数值或二分类变量（字符/因子自动转 0/1）。
#' @param mreg 中介回归类型："linear" 或 "logistic"。
#' @param a0,a1 连续暴露的对比水平；默认按 \code{a_pct} 的分位数生成。
#' @param a_pct 长度2，对应 a0/a1 的分位数（默认 c(0.25,0.75)）。
#' @param m_cde 计算 CDE 时中介的固定值（默认中位数）。
#' @param c_cond 协变量固定值；默认数值取中位数、二分类取 1。可传命名向量/列表覆盖。
#' @param interaction 是否包含 A×M 交互（默认 TRUE）。
#' @param show_hr 输出表中的效应显示为 HR(95%CI) 而非 β(95%CI)（默认 FALSE）。
#' @param digits 小数位（默认 2）。
#'
#' @return list，包含：
#' \itemize{
#'   \item \code{table}: 整洁 data.frame（列：exposure, effect, estimate_CI, p_value）
#'   \item \code{gt}: \pkg{gt} 美观表
#'   \item \code{fit}: regmedint 对象
#'   \item \code{diag}: 诊断信息（样本量、自动选择的 a0/a1/m_cde/c_cond）
#' }
#'
#' @examples
#' \dontrun{
#' res <- z_regmed_table(
#'   data      = d,
#'   exposure  = "twa_benzene",
#'   mediator  = "PhenoAge",
#'   yreg      = "survCox",
#'   time_col  = "AR_time",
#'   event_col = "AR_status",
#'   covars    = c("gender"),
#'   mreg      = "linear",      # 或 "logistic"（若中介为0/1）
#'   a_pct     = c(0.25, 0.75), # a0/a1 自动取分位
#'   interaction = TRUE,
#'   show_hr   = FALSE
#' )
#' print(res$gt)
#' res$table
#' }
#' @export
#' @importFrom dplyr %>% transmute across where n
#' @importFrom regmedint regmedint
#' @importFrom stats quantile median pnorm
#' @importFrom gt gt tab_options cols_align tab_style cells_column_labels
#' @importFrom gt cells_body cell_text px
z_regmed_table <- function(
    data,
    exposure,
    mediator,
    yreg      = c("survCox"),
    time_col  = NULL,
    event_col = NULL,
    covars    = NULL,
    mreg      = c("linear", "logistic"),
    a0 = NULL, a1 = NULL, a_pct = c(0.25, 0.75),
    m_cde = NULL,
    c_cond = NULL,
    interaction = TRUE,
    show_hr = FALSE,
    digits = 2
){
  yreg <- match.arg(yreg)
  mreg <- match.arg(mreg)
  
  # --- 数据与列检查 ---
  if ("data.table" %in% class(data)) data <- as.data.frame(data, stringsAsFactors = FALSE)
  stopifnot(is.data.frame(data))
  req <- unique(c(exposure, mediator, covars,
                  if (yreg=="survCox") c(time_col, event_col)))
  miss <- setdiff(req, names(data))
  if (length(miss)) stop("缺少列: ", paste(miss, collapse=", "))
  
  # 仅保留所需列，去缺失
  keep <- req
  df <- data[, keep, drop=FALSE]
  df <- stats::na.omit(df)
  
  # 类型与编码
  if (yreg=="survCox") {
    df[[time_col]]  <- as.numeric(df[[time_col]])
    df[[event_col]] <- as.integer(df[[event_col]])
  }
  df[[exposure]] <- as.numeric(df[[exposure]])
  df[[mediator]] <- as.numeric(df[[mediator]])
  
  # 协变量：数值保留；二分类字符/因子→0/1
  encode01 <- function(v){
    if (is.numeric(v)) return(v)
    v0 <- as.character(v)
    lv <- sort(unique(v0[!is.na(v0)]))
    if (length(lv) == 2) return(as.integer(v0 == lv[2]))  # 大者=1
    stop("协变量必须为数值或二分类：", paste(lv, collapse="/"))
  }
  if (length(covars)) {
    for (cv in covars) df[[cv]] <- encode01(df[[cv]])
  }
  
  # a0/a1 / m_cde / c_cond
  if (is.null(a0) || is.null(a1)) {
    qs <- stats::quantile(df[[exposure]], probs = a_pct, na.rm = TRUE)
    a0 <- if (is.null(a0)) unname(qs[1]) else a0
    a1 <- if (is.null(a1)) unname(qs[2]) else a1
  }
  if (is.null(m_cde)) m_cde <- unname(stats::median(df[[mediator]], na.rm = TRUE))
  
  if (is.null(c_cond)) {
    c_cond <- lapply(covars, function(cv){
      x <- df[[cv]]
      if (length(unique(x))==2 && all(sort(unique(x)) %in% c(0,1))) return(1)  # 取 1
      stats::median(x, na.rm = TRUE)                                          # 数值中位
    })
    names(c_cond) <- covars
  } else {
    if (is.null(names(c_cond)) || !all(covars %in% names(c_cond)))
      stop("c_cond 必须为命名列表/向量，且包含所有 covars。")
    c_cond <- as.list(c_cond)[covars]
  }
  
  # --- 拟合 regmedint ---
  fit <- regmedint::regmedint(
    data      = df,
    yvar      = if (yreg=="survCox") time_col else stop("当前仅支持 yreg='survCox'"),
    eventvar  = if (yreg=="survCox") event_col else NULL,
    avar      = exposure,
    mvar      = mediator,
    cvar      = covars,
    a0        = a0,
    a1        = a1,
    m_cde     = m_cde,
    c_cond    = unname(unlist(c_cond)),
    mreg      = mreg,
    yreg      = "survCox",
    interaction = isTRUE(interaction),
    casecontrol = FALSE,
    na_omit   = TRUE
  )
  
  # --- 提取与格式化 ---
  cf <- coef(fit)
  ci <- confint(fit)
  take <- function(name) {
    c(est  = unname(cf[name]),
      low  = unname(ci[name, 1]),
      high = unname(ci[name, 2]))
  }
  # TE 与 PM
  te <- take("te")
  pm <- take("pm")
  
  se_from_ci <- function(lo, hi) (hi - lo) / (2*1.96)
  p_two <- function(est, se) 2 * stats::pnorm(-abs(est/se))
  te_p <- p_two(te["est"], se_from_ci(te["low"], te["high"]))
  pm_p <- p_two(pm["est"], se_from_ci(pm["low"], pm["high"]))
  
  fmt_p <- function(p) ifelse(is.na(p), NA, ifelse(p < 0.001, "<0.001", formatC(p, digits=3, format="f")))
  if (isTRUE(show_hr)) {
    est_str <- sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
                       exp(te["est"]), exp(te["low"]), exp(te["high"]))
  } else {
    est_str <- sprintf(paste0("%.", digits, "f (%.", digits, "f, %.", digits, "f)"),
                       te["est"], te["low"], te["high"])
  }
  
  df_out <- data.frame(
    exposure              = "Continuous variable",
    effect                = c("Total effect (TE)", "Mediation proportion (PM, %)"),
    estimate_CI           = c(est_str, sprintf(paste0("%.", digits, "f"), pm["est"]*100)),
    p_value               = c(fmt_p(te_p), fmt_p(pm_p)),
    stringsAsFactors = FALSE
  )
  
  # --- gt 表 ---
  if (!requireNamespace("gt", quietly=TRUE)) stop("需要安装 'gt' 包以生成美观表。")
  gt_tbl <- gt::gt(df_out) |>
    gt::tab_options(table.font.size = gt::px(12)) |>
    gt::cols_align(columns = everything(), align = "center") |>
    gt::tab_style(style = gt::cell_text(weight = "bold"),
                  locations = gt::cells_column_labels(everything())) |>
    gt::tab_style(style = gt::cell_text(align = "left"),
                  locations = gt::cells_body(columns = 1)) |>
    gt::tab_options(table.border.top.style = "solid",
                    table.border.top.width = gt::px(1),
                    table.border.bottom.style = "solid",
                    table.border.bottom.width = gt::px(1))
  
  diag <- list(
    n = nrow(df),
    a0 = a0, a1 = a1, a_pct = a_pct,
    m_cde = m_cde,
    c_cond = c_cond
  )
  
  list(table = df_out, gt = gt_tbl, fit = fit, diag = diag)
}

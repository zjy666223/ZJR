# file: ZJR/R/z_mediation_logit.R
#' 一键中介分析（Imai 方法；logit/probit/linear；bootstrap 或 quasi-Bayes）
#'
#' @description
#' 传入数据与关键变量，自动构建中介与结局模型并调用 \code{mediation::mediate}，
#' 输出四行表：Total effect、Mediation effect (ACME, average)、
#' Direct effect (ADE, average)、Proportion mediated (average) ，附 95%CI 和 p 值。
#' 连续暴露默认使用 \strong{p75 vs p25} 对比（可自定义）。
#'
#' @param data data.frame
#' @param exposure 暴露变量名（字符）
#' @param mediator 中介变量名（字符）
#' @param outcome 结局变量名（字符）
#' @param covariates 协变量名字符向量（数值0/1或因子均可）
#' @param outcome_family 结局模型：\code{"logit"}（默认）、\code{"probit"}、\code{"linear"}
#' @param mediator_family 中介模型：\code{"linear"}（默认）、\code{"probit"}、\code{"logit"}
#' @param treat.value 处理组暴露值（给定则覆盖分位数对比）
#' @param control.value 对照组暴露值（给定则覆盖分位数对比）
#' @param q 暴露分位数对比，默认 \code{c(0.25, 0.75)}
#' @param sims 仿真/自助次数（建议 1000–5000；冒烟可更小）
#' @param boot \code{TRUE}=bootstrap，\code{FALSE}=quasi-Bayesian
#' @param interaction 是否在结局模型加入 \code{exposure:mediator} 交互（\strong{无需}向 \code{mediate()} 传 INT）
#' @param covariate_profile 计算时固定的协变量画像（如 \code{list(age=50, ethnicity="White")}），不影响“是否调整”
#' @param ref_levels 为因子协变量指定参考水平（如 \code{list(ethnicity="White")}；对 0/1 数值无效）
#' @param digits 小数位；默认 3
#' @param p_small_cutoff 极小 p 值显示阈值；默认 \code{1e-4}
#' @param export_csv 可选 CSV 路径
#'
#' @return \code{list(table = tibble, med = mediate_object)}
#'
#' @details
#' - “调整协变量”通过把协变量同时放入中介与结局模型公式实现。
#' - \code{ref_levels} 仅对因子协变量生效；若你的协变量已是 0/1 数值，\code{ref_levels} 可留空。
#' - 若需要交互效应，设 \code{interaction=TRUE} 即会在结局公式中添加 \code{exposure:mediator} 项。
#'
#' @examples
#' \dontrun{
#' library(ZJR)
#' # 假设数据 d 包含列：twa_benzene, HD, AR_status, ethnicity, gender, age, smoking_status
#'
#' # 1) 基础：logit 结局 + 线性中介，p75 vs p25，对 ethnicity 调整并设参考组
#' res1 <- ZJR::z_mediation_logit(
#'   data = d,
#'   exposure = "twa_benzene",
#'   mediator = "HD",
#'   outcome = "AR_status",
#'   covariates = c("ethnicity"),
#'   outcome_family = "logit",
#'   mediator_family = "linear",
#'   sims = 2000, boot = TRUE,
#'   ref_levels = list(ethnicity = "White")
#' )
#' res1$table
#'
#' # 2) 多协变量（混合数值与因子）：0/1 数值协变量无需 ref_levels
#' res2 <- ZJR::z_mediation_logit(
#'   data = d,
#'   exposure = "twa_benzene", mediator = "HD", outcome = "AR_status",
#'   covariates = c("ethnicity","gender","age","smoking_status"),
#'   outcome_family = "logit", mediator_family = "linear",
#'   sims = 2000, boot = TRUE,
#'   ref_levels = list(ethnicity="White", smoking_status="Never")
#' )
#' res2$table
#'
#' # 3) 自定义对比（p90 vs p10）
#' a0 <- as.numeric(quantile(d$twa_benzene, 0.10, na.rm = TRUE))
#' a1 <- as.numeric(quantile(d$twa_benzene, 0.90, na.rm = TRUE))
#' res3 <- ZJR::z_mediation_logit(
#'   data = d, exposure = "twa_benzene", mediator = "HD", outcome = "AR_status",
#'   covariates = c("ethnicity"),
#'   outcome_family = "logit", mediator_family = "linear",
#'   treat.value = a1, control.value = a0,
#'   sims = 2000, boot = TRUE,
#'   ref_levels = list(ethnicity = "White")
#' )
#' res3$table
#'
#' # 4) 含 A×M 交互（自动加入 exposure:mediator）
#' res4 <- ZJR::z_mediation_logit(
#'   data = d, exposure = "twa_benzene", mediator = "HD", outcome = "AR_status",
#'   covariates = c("ethnicity"),
#'   outcome_family = "logit", mediator_family = "linear",
#'   interaction = TRUE,
#'   sims = 2000, boot = TRUE,
#'   ref_levels = list(ethnicity = "White")
#' )
#' res4$table
#'
#' # 5) 指定“固定画像”计算
#' res5 <- ZJR::z_mediation_logit(
#'   data = d, exposure = "twa_benzene", mediator = "HD", outcome = "AR_status",
#'   covariates = c("ethnicity","age"),
#'   outcome_family = "logit", mediator_family = "linear",
#'   sims = 2000, boot = TRUE,
#'   covariate_profile = list(ethnicity="White", age=50),
#'   ref_levels = list(ethnicity = "White")
#' )
#' res5$table
#'
#' # 6) 线性结局示例（outcome 为连续）
#' res6 <- ZJR::z_mediation_logit(
#'   data = d, exposure = "twa_benzene", mediator = "HD", outcome = "AR_status",
#'   covariates = c("ethnicity"),
#'   outcome_family = "linear", mediator_family = "linear",
#'   sims = 2000, boot = TRUE,
#'   ref_levels = list(ethnicity = "White")
#' )
#' res6$table
#'
#' # 可选：导出 CSV
#' # ZJR::z_mediation_logit(..., export_csv = "mediation_table.csv")
#' }
#'
#' @import mediation
#' @importFrom stats relevel lm glm binomial gaussian quantile as.formula na.omit
#' @importFrom utils write.csv
#' @importFrom tibble tibble
#' @export
z_mediation_logit <- function(
    data,
    exposure,
    mediator,
    outcome,
    covariates = NULL,
    outcome_family = c("logit","probit","linear"),
    mediator_family = c("linear","probit","logit"),
    treat.value = NULL,
    control.value = NULL,
    q = c(0.25, 0.75),
    sims = 2000,
    boot = TRUE,
    interaction = FALSE,
    covariate_profile = NULL,
    ref_levels = NULL,
    digits = 3,
    p_small_cutoff = 1e-4,
    export_csv = NULL
){
  stopifnot(is.data.frame(data))
  outcome_family  <- match.arg(outcome_family)
  mediator_family <- match.arg(mediator_family)
  
  need <- unique(c(exposure, mediator, outcome, covariates))
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("变量不存在：", paste(miss, collapse=", "))
  
  df <- stats::na.omit(data[, need, drop = FALSE])
  
  # 协变量：字符→因子；ref_levels 只作用于因子。0/1 数值保持原样。
  if (length(covariates)) {
    for (v in covariates) {
      if (is.character(df[[v]]) || is.logical(df[[v]])) df[[v]] <- factor(df[[v]])
      if (is.factor(df[[v]]) && !is.null(ref_levels) && !is.null(ref_levels[[v]])) {
        ref <- ref_levels[[v]]
        if (ref %in% levels(df[[v]])) df[[v]] <- stats::relevel(df[[v]], ref = ref)
      }
    }
  }
  
  fam_map <- function(f){
    switch(f,
           linear = stats::gaussian(),
           probit = stats::binomial("probit"),
           logit  = stats::binomial("logit")
    )
  }
  
  # 构造公式字符串（避免 f_m/f_y 符号保存在 call 里）
  rhs_m <- paste(c(exposure, covariates), collapse = " + ")
  rhs_y <- paste(c(mediator, exposure, covariates,
                   if (interaction) paste0(exposure, ":", mediator) else NULL),
                 collapse = " + ")
  form_m_str <- paste(mediator, "~", rhs_m)
  form_y_str <- paste(outcome,  "~", rhs_y)
  
  # 拟合（用 do.call；公式对象直接放到 call，避免 bootstrap update 找不到变量名）
  fit_m <- if (mediator_family == "linear") {
    do.call(stats::lm,  list(formula = stats::as.formula(form_m_str), data = df))
  } else {
    do.call(stats::glm, list(formula = stats::as.formula(form_m_str), data = df, family = fam_map(mediator_family)))
  }
  
  fit_y <- if (outcome_family == "linear") {
    if (!is.numeric(df[[outcome]])) df[[outcome]] <- as.numeric(df[[outcome]])
    do.call(stats::lm,  list(formula = stats::as.formula(form_y_str), data = df))
  } else {
    do.call(stats::glm, list(formula = stats::as.formula(form_y_str), data = df, family = fam_map(outcome_family)))
  }
  
  # 暴露对比
  if (!is.null(treat.value) && !is.null(control.value)) {
    a1 <- as.numeric(treat.value); a0 <- as.numeric(control.value)
  } else {
    a0 <- as.numeric(stats::quantile(df[[exposure]], q[1], na.rm = TRUE))
    a1 <- as.numeric(stats::quantile(df[[exposure]], q[2], na.rm = TRUE))
  }
  
  # 中介分析（不传 INT；交互由公式自动识别）
  med <- mediation::mediate(
    model.m = fit_m,
    model.y = fit_y,
    treat = exposure,
    mediator = mediator,
    treat.value = a1,
    control.value = a0,
    covariates = covariate_profile,
    boot = isTRUE(boot),
    sims = as.integer(sims)
  )
  
  sm <- summary(med)
  
  # 兼容字段提取
  getv  <- function(x, keys, default=NA_real_) { for (k in keys) if (!is.null(x[[k]])) return(as.numeric(x[[k]])); default }
  getv2 <- function(x, keys) { for (k in keys) if (!is.null(x[[k]])) return(as.numeric(x[[k]])); c(NA_real_, NA_real_) }
  
  te_est   <- getv(sm,  c("tau.coef","te"))
  te_ci    <- getv2(sm, c("tau.ci","te.ci"))
  te_p     <- getv(sm,  c("tau.p","te.p"))
  
  acme_est <- getv(sm,  c("d.avg","acme.avg","acme"))
  acme_ci  <- getv2(sm, c("d.avg.ci","acme.ci"))
  acme_p   <- getv(sm,  c("d.avg.p","acme.p"))
  
  ade_est  <- getv(sm,  c("z.avg","ade.avg","ade"))
  ade_ci   <- getv2(sm, c("z.avg.ci","ade.ci"))
  ade_p    <- getv(sm,  c("z.avg.p","ade.p"))
  
  pm_est   <- getv(sm,  c("n.avg","pm","prop.med.avg"))
  pm_ci    <- getv2(sm, c("n.avg.ci","pm.ci"))
  pm_p     <- getv(sm,  c("n.avg.p","pm.p"))
  
  mat <- rbind(
    c(te_est,   te_ci,   te_p),
    c(acme_est, acme_ci, acme_p),
    c(ade_est,  ade_ci,  ade_p),
    c(pm_est,   pm_ci,   pm_p)
  )
  colnames(mat) <- c("Estimate","CI_low","CI_high","P")
  
  fmt_num <- function(x) ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
  fmt_p   <- function(p) ifelse(is.na(p), NA_character_,
                                ifelse(p < p_small_cutoff, paste0("<", format(p_small_cutoff, scientific = TRUE)),
                                       sprintf(paste0("%.", digits, "f"), p)))
  
  labels <- c("Total effect","Mediation effect (average)","Direct effect (average)","Proportion mediated (average)")
  tbl <- tibble::tibble(
    `HD` = labels,
    `Estimate` = fmt_num(mat[, "Estimate"]),
    `95% CI lower` = fmt_num(mat[, "CI_low"]),
    `95% CI upper` = fmt_num(mat[, "CI_high"]),
    `P-value` = c(fmt_p(mat[1,"P"]), fmt_p(mat[2,"P"]), fmt_p(mat[3,"P"]), fmt_p(mat[4,"P"]))
  )
  
  if (!is.null(export_csv)) utils::write.csv(tbl, export_csv, row.names = FALSE)
  invisible(list(table = tbl, med = med))
}

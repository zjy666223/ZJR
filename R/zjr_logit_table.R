# file: D:/12自定义封装R包/ZJR/R/zjr_logit_table.R

#' 一键生成 Logistic 回归结果表（3 模型，支持结局翻转与分位分类）
#'
#' @description
#' 给定数据、结局、暴露与 3 组协变量，拟合 3 个二项 Logistic 回归并生成宽表：
#' 每列为一个模型（Model1/2/3），单元格为 `OR (95%CI)` 与 `p`。支持：
#' 1) 结局 0/1 映射与翻转；2) 连续暴露可按三/四分位转为分类后再建模；
#' 连续与分类的结果**合并为同一张表**（分类部分含 Reference 行）。
#'
#' @param data data.frame。
#' @param y 结局变量名（字符串）。
#' @param x 暴露变量名（字符串）。
#' @param covars_list 长度为 3 的列表，例如 `list(NULL, c("sex","age"), c("sex","ukb_drink","age"))`。
#' @param digits 小数位（OR 与 CI），默认 2。
#' @param conf 置信水平，默认 0.95。
#' @param ref 若 `x` 为分类，指定参考水平（字符串）。
#' @param y_flip 是否翻转结局 0/1（0↔1），默认 FALSE。
#' @param y_event,y_control 可选：指定原始数据中事件/对照对应的取值（如 "Yes"/"No"）。
#' @param x_cat 当 `x` 为连续变量时，是否额外按分位转为分类后再建模：`0`=不分组，`3`=三分位，`4`=四分位。默认 0。
#'
#' @return data.frame（宽表），并附加属性 `notes`（模型说明与事件编码说明）。
#' @examples
#' # 原用法（连续）
#' tab <- zjr_logit_table(
#'   d, "OA_status_num", "PM2.5_per5",
#'   list(NULL, c("sex","age"), c("sex","ukb_drink","age"))
#' )
#' tab
#'
#' # 连续 + 四分位分类，合并输出
#' tab <- zjr_logit_table(
#'   d, "OA_status_num", "PM2.5_per5",
#'   list(NULL, c("sex","age"), c("sex","ukb_drink","age")),
#'   x_cat = 4, y_flip = FALSE
#' )
#'
#' # 连续 + 三分位分类
#' tab <- zjr_logit_table(
#'   d, "OA_status_num", "PM2.5_per5",
#'   list(NULL, c("sex","age"), c("sex","ukb_drink","age")),
#'   x_cat = 3
#' )
#' @export
zjr_logit_table <- function(data, y, x, covars_list,
                            digits = 2, conf = 0.95, ref = NULL,
                            y_flip = FALSE, y_event = NULL, y_control = NULL,
                            x_cat = 0) {
  stopifnot(is.data.frame(data))
  if (!y %in% names(data)) stop(sprintf("结局变量 '%s' 不存在", y))
  if (!x %in% names(data)) stop(sprintf("暴露变量 '%s' 不存在", x))
  if (!is.list(covars_list) || length(covars_list) != 3)
    stop("covars_list 必须是长度为 3 的列表，例如 list(NULL, c('sex','age'), c('sex','ukb_drink','age'))")
  
  # ---- 结局编码与翻转 ----
  df <- data
  df[[y]] <- .binify_y(df[[y]], y_event = y_event, y_control = y_control, flip = y_flip, yname = y)
  
  # ---- 暴露（原始形态） ----
  xv <- df[[x]]
  x_is_factor0 <- is.factor(xv) || is.logical(xv) || is.character(xv)
  if (x_is_factor0) {
    if (!is.factor(xv)) xv <- factor(xv)
    if (!is.null(ref)) {
      if (!ref %in% levels(xv)) stop("指定的参考水平不在暴露变量水平中")
      xv <- stats::relevel(xv, ref = ref)
    }
    df[[x]] <- xv
  }
  
  # ---- 若需要：把连续 x 切为 3/4 分位（Q1 作为参考） ----
  do_cat <- (!x_is_factor0) && is.numeric(xv) && (x_cat %in% c(3L, 4L))
  if (do_cat) {
    x_cat_name <- paste0(x, if (x_cat == 3L) "_T3" else "_Q4")
    df[[x_cat_name]] <- .ntile_factor(xv, x_cat)  # levels: "1","2",... 参考=1
  }
  
  # ---- 连续：3 个模型 ----
  mk_fml <- function(lhs, xrhs, covs) stats::as.formula(paste(lhs, "~", paste(c(xrhs, covs), collapse = " + ")))
  fmls_con <- lapply(covars_list, function(cov) mk_fml(y, x, if (is.null(cov)) character(0) else cov))
  fits_con <- lapply(fmls_con, function(fml) stats::glm(fml, data = df, family = stats::binomial()))
  tds_con  <- lapply(fits_con, .tidy_glm, conf = conf)
  tds_con  <- lapply(tds_con, function(d) d[startsWith(d$term, x), , drop = FALSE])
  
  # ---- 分类：3 个模型（可选） ----
  if (do_cat) {
    fmls_cat <- lapply(covars_list, function(cov) mk_fml(y, x_cat_name, if (is.null(cov)) character(0) else cov))
    fits_cat <- lapply(fmls_cat, function(fml) stats::glm(fml, data = df, family = stats::binomial()))
    tds_cat  <- lapply(fits_cat, .tidy_glm, conf = conf)
    tds_cat  <- lapply(tds_cat, function(d) d[startsWith(d$term, x_cat_name), , drop = FALSE])
  }
  
  # ---- 格式化工具 ----
  digits <- as.integer(digits); if (is.na(digits) || digits < 0) stop("digits 必须为非负整数")
  fmt <- paste0("%.", digits, "f")
  fmt_or <- function(d) paste0(sprintf(fmt, d$OR), " (", sprintf(fmt, d$lower), " ~ ", sprintf(fmt, d$upper), ")")
  fmt_p  <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
  
  # ---- 连续：转为每模型一列 ----
  lab_cont <- paste0(x, " (continuous)")
  mk_col <- function(d, name) {
    data.frame(Variables = rep(lab_cont, nrow(d)),
               setNames(list(fmt_or(d)), name),
               setNames(list(fmt_p(d$p)), paste0(name, "_p")),
               check.names = FALSE, stringsAsFactors = FALSE)
  }
  cols <- Map(mk_col, tds_con, paste0("Model", 1:3))
  
  # ---- 分类：为每一模型追加块（标题行 + 参考行 + 其余水平） ----
  if (do_cat) {
    block_title <- paste0(x, if (x_cat == 3L) " tertile" else " quartile")
    add_cat_block <- function(df_col, name, d_cat) {
      labs <- sub(paste0("^", x_cat_name), "", d_cat$term)   # "2","3","4"
      # 统一列名到: Variables, <name>, <name>_p
      head <- data.frame(Variables = block_title,
                         tmp = "", p = "",
                         stringsAsFactors = FALSE)
      refrow <- data.frame(Variables = "1",
                           tmp = "1.00 (Reference)", p = "",
                           stringsAsFactors = FALSE)
      levs <- if (nrow(d_cat) > 0) data.frame(Variables = labs,
                                              tmp = fmt_or(d_cat),
                                              p = fmt_p(d_cat$p),
                                              stringsAsFactors = FALSE) else NULL
      blk <- do.call(rbind, Filter(Negate(is.null), list(head, refrow, levs)))
      names(blk)[names(blk) == "tmp"] <- name
      names(blk)[names(blk) == "p"]   <- paste0(name, "_p")
      # 与 df_col 列名对齐后再 rbind
      rbind(df_col, blk[, c("Variables", name, paste0(name, "_p"))])
    }
    cols <- Map(add_cat_block, cols, paste0("Model", 1:3), tds_cat)
  }
  
  # ---- 合并 3 模型 ----
  out <- cols[[1]]
  names(out)[names(out) == "p"] <- paste0(names(out)[2], "_p")
  for (j in 2:3) {
    b <- cols[[j]]
    names(b)[names(b) == "p"] <- paste0(names(b)[2], "_p")
    out <- merge(out, b, by = "Variables", all = TRUE, sort = FALSE)
  }
  rownames(out) <- NULL
  
  # ---- 注释 ----
  ev_note <- .event_note(data[[y]], y_event, y_control, y_flip)
  cat_note <- if (do_cat) paste0("Also modeled as ", if (x_cat == 3L) "tertiles" else "quartiles", " with level 1 as reference.") else NULL
  attr(out, "notes") <- c(
    paste0("OR: Odds Ratio, CI: ", round(conf*100), "% Confidence Interval."),
    ev_note,
    if (!is.null(cat_note)) cat_note,
    c("Model1: Crude",
      paste0("Model2: Adjust: ", paste(covars_list[[2]], collapse = ", ")),
      paste0("Model3: Adjust: ", paste(covars_list[[3]], collapse = ", ")))
  )
  out
}

# ---- helpers ---------------------------------------------------------------

# 将结局转为 0/1，并可翻转
.binify_y <- function(yv, y_event = NULL, y_control = NULL, flip = FALSE, yname = "y") {
  map <- !is.null(y_event) || !is.null(y_control)
  if (map) {
    y_chr <- as.character(yv)
    res <- rep(NA_real_, length(y_chr))
    if (!is.null(y_event))   res[y_chr == as.character(y_event)]   <- 1
    if (!is.null(y_control)) res[y_chr == as.character(y_control)] <- 0
    if (!is.null(y_event)   && !any(y_chr == as.character(y_event),   na.rm = TRUE)) warning("y_event 未在结局中出现")
    if (!is.null(y_control) && !any(y_chr == as.character(y_control), na.rm = TRUE)) warning("y_control 未在结局中出现")
  } else if (is.logical(yv)) {
    res <- as.integer(yv)
  } else if (is.factor(yv)) {
    lv <- levels(yv); res <- as.integer(yv == lv[length(lv)])
  } else if (is.numeric(yv)) {
    res <- yv; uy <- sort(unique(stats::na.omit(res))); if (!all(uy %in% c(0,1))) warning(sprintf("结局 '%s' 不是 0/1；按数值使用", yname))
  } else stop("结局必须是 0/1/逻辑/因子，或用 y_event/y_control 指定映射")
  
  if (isTRUE(flip)) {
    uy <- sort(unique(stats::na.omit(res)))
    if (!all(uy %in% c(0,1))) stop("y_flip=TRUE 仅适用于 0/1 结局；请先完成映射")
    res <- ifelse(is.na(res), NA_real_, 1 - res)
  }
  
  uy <- sort(unique(stats::na.omit(res)))
  if (!all(uy %in% c(0,1))) stop("当前映射下结局不是 0/1")
  if (length(uy) < 2) stop("当前映射下结局只含单一取值，无法拟合")
  res
}

# 整理 GLM 系数为 OR/CI/p
.tidy_glm <- function(fit, conf = 0.95) {
  co <- stats::coef(summary(fit))
  est <- co[, "Estimate"]; se <- co[, "Std. Error"]; p <- co[, "Pr(>|z|)"]
  z <- stats::qnorm((1 + conf) / 2)
  data.frame(term = rownames(co), OR = exp(est), lower = exp(est - z * se), upper = exp(est + z * se), p = p,
             stringsAsFactors = FALSE, row.names = NULL)
}

# 生成注释文本
.event_note <- function(orig_y, y_event, y_control, flip) {
  if (!is.null(y_event) || !is.null(y_control)) {
    paste0("Event=1: ", if (!is.null(y_event)) as.character(y_event) else "(unspecified)",
           "; Control=0: ", if (!is.null(y_control)) as.character(y_control) else "(unspecified)",
           if (isTRUE(flip)) "; flipped 0↔1" else "")
  } else {
    base <- "Event=1 by rule (logical→1; factor highest level→1; numeric as-is)"
    if (isTRUE(flip)) paste0(base, "; flipped 0↔1") else base
  }
}

# 将连续变量分为 n 分位（近似等人数），返回因子水平 "1".."n"，参考=1
.ntile_factor <- function(x, n) {
  idx <- order(x, na.last = NA)
  g <- rep(NA_integer_, length(x))
  if (length(idx) == 0) return(factor(g, levels = as.character(seq_len(n))))
  m <- length(idx)
  cuts <- ceiling(seq_len(m) / (m / n))
  cuts[cuts > n] <- n
  g[idx] <- cuts
  factor(g, levels = seq_len(n), labels = as.character(seq_len(n)))
}

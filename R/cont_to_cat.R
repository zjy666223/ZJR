# file: D:/12自定义封装R包/ZJR/R/cont_to_cat.R

#' 连续变量转换为分类变量/标准化
#'
#' @description
#' 将数据框中的连续变量按多种方式转换：自定义断点、按中位数/三分位/四分位/五分位分组，或转换为 z 分值。
#'
#' @param data 数据框。
#' @param colname 列名（字符串）。
#' @param method 分组方法：`"custom"`、`"median"`、`"tertile"`、`"quartile"`、`"quintile"`、`"z"`。
#' @param breaks 当 `method="custom"` 时的断点；可为数值向量或形如 `"5, 10, 15"` 的字符串。
#' @param right 区间是否右闭，传给 `cut()`；默认 `TRUE`。
#' @param include_lowest 是否包含最小端点，默认 `TRUE`。
#' @param labels 分组标签；默认自动生成（如 Q1~Qk 或 `[a,b)`）。
#' @param append 是否追加为新列（`<col>_cat` 或 `<col>_z`），`FALSE` 则覆盖原列。默认 `TRUE`。
#' @param new_name 新列名；未提供时按 `append` 与 `method` 自动命名。
#'
#' @return 返回数据框。
#' @examples
#' # 1) 自定义断点（字符串或数值向量都行）
#' d <- cont_to_cat(d, "age", method = "custom", breaks = "30, 45, 60")
#' # 2) 按中位数（二分）
#' d <- cont_to_cat(d, "PM2.5_per5", method = "median")
#' # 3) 三/四/五分位
#' d <- cont_to_cat(d, "PM10_per10", method = "quartile")   # 生成 Q1~Q4
#' d <- cont_to_cat(d, "NO2_per10",  method = "quintile")   # 生成 Q1~Q5
#' # 4) 转换为 z 分值（标准化）
#' d <- cont_to_cat(d, "SO2_per10", method = "z")
#' # 5) 覆盖原列或自定义新列名
#' d <- cont_to_cat(d, "age", method = "quartile", append = FALSE)             # 覆盖
#' d <- cont_to_cat(d, "age", method = "quartile", new_name = "age_quartile")  # 追加为指定列
#' 1
#' df <- data.frame(x = rnorm(10))
#' df1 <- cont_to_cat(df, "x", method = "median")
#' df2 <- cont_to_cat(df, "x", method = "quartile")
#' df3 <- cont_to_cat(df, "x", method = "custom", breaks = c(-1, 0, 1))
#' df4 <- cont_to_cat(df, "x", method = "z")
#' @export
cont_to_cat <- function(data, colname, method = c("custom","median","tertile","quartile","quintile","z"),
                        breaks = NULL, right = TRUE, include_lowest = TRUE, labels = NULL,
                        append = TRUE, new_name = NULL) {
  stopifnot(is.data.frame(data))
  if (!colname %in% names(data)) stop(sprintf("列 '%s' 不存在", colname))
  method <- match.arg(method)
  x <- data[[colname]]
  if (!is.numeric(x)) stop(sprintf("列 '%s' 必须为数值型以进行分组/标准化", colname))
  
  # 解析自定义断点
  parse_breaks <- function(b) {
    if (is.null(b)) return(NULL)
    if (is.character(b)) {
      b <- gsub("[，;]", ",", b)
      as.numeric(trimws(unlist(strsplit(b, ","))))
    } else as.numeric(b)
  }
  
  # 构造分位点断点
  probs_to_breaks <- function(k) {
    qs <- stats::quantile(x, probs = seq(0, 1, length.out = k + 1), na.rm = TRUE, type = 7)
    # 处理重复分位点
    uq <- unique(as.numeric(qs))
    if (length(uq) < (k + 1)) {
      # why: 所有值相同或重复过多，退化为唯一值分组
      k_eff <- max(1L, length(uq) - 1L)
      qs <- stats::quantile(x, probs = seq(0, 1, length.out = k_eff + 1), na.rm = TRUE, type = 7)
    }
    as.numeric(qs)
  }
  
  # 主体
  if (method == "z") {
    mu <- mean(x, na.rm = TRUE); sdv <- stats::sd(x, na.rm = TRUE)
    z <- if (isTRUE(all(is.na(x)))) rep(NA_real_, length(x)) else (x - mu) / ifelse(sdv == 0, NA, sdv)
    out_name <- new_name %||% paste0(colname, "_z")
    if (append) { data[[out_name]] <- z } else { data[[colname]] <- z }
    return(data)
  }
  
  # 断点准备
  if (method == "custom") {
    brks <- sort(unique(c(-Inf, parse_breaks(breaks), Inf)))
    if (length(brks) < 3) stop("自定义断点不足，至少需要两个端点以形成分组")
    if (is.null(labels)) labels <- .zjr_labels_from_breaks(brks, right = right)
  } else {
    k <- switch(method, median = 2L, tertile = 3L, quartile = 4L, quintile = 5L)
    brks <- probs_to_breaks(k)
    # 保证端点覆盖
    brks[1] <- -Inf; brks[length(brks)] <- Inf
    if (is.null(labels)) labels <- paste0("Q", seq_len(length(brks) - 1L))
  }
  
  fac <- cut(x, breaks = brks, right = right, include.lowest = include_lowest, labels = labels)
  
  out_name <- new_name %||% paste0(colname, "_cat")
  if (append) {
    data[[out_name]] <- fac
  } else {
    data[[colname]] <- fac
  }
  data
}

# internal: 生成区间标签
.zjr_labels_from_breaks <- function(brks, right = TRUE) {
  parts <- Map(function(a,b){
    if (right) sprintf("(%s, %s]", fmt(a), fmt(b)) else sprintf("[%s, %s)", fmt(a), fmt(b))
  }, brks[-length(brks)], brks[-1])
  unlist(parts)
}

fmt <- function(x) {
  if (is.infinite(x)) return(ifelse(x > 0, "+Inf", "-Inf"))
  # 控制小数显示
  formatC(x, format = "fg", digits = 6, flag = "-")
}

# infix helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

# file: D:/12自定义封装R包/ZJR/R/impute_column_mice.R
#' 多重插补（mice 封装）
#'
#' @description
#' 只对指定列进行多重插补，其他列保持原样作为预测变量使用。自动识别列类型并选择合适的
#' `mice` 方法（数值→`pmm`，二分类/逻辑→`logreg`，多分类→`polyreg`）。支持选择合并策略：
#' 返回第1套、均值/众数等。
#'
#' @param data data.frame/tibble。
#' @param colname 需要插补的列名（字符串）。
#' @param m 插补次数（mice 的 m），默认 5。
#' @param maxit 迭代次数，默认 10。
#' @param seed 随机种子，默认 123。
#' @param predictors 作为预测变量的列名字符向量；默认 `NULL` 表示使用除目标列外的所有列。
#' @param method 可指定 mice 方法（针对目标列）；`NULL` 表示自动：
#'   - numeric/integer → "pmm"；
#'   - 二分类/逻辑 → "logreg"；
#'   - 多分类(≥3) → "polyreg"。
#' @param combine 合并方式："first"（默认，返回第1套完成数据）；
#'   "mean"（数值取均值、整数取四舍五入、分类取众数）；
#'   "mode"（所有类型取众数；数值并列取平均）。
#' @param keep_indicator 是否保留插补指示列 `<col>_was_na`，默认 TRUE。
#'
#' @return list：
#'   - data：完成插补的数据框；
#'   - mids：mice 的 mids 对象；
#'   - used_method：实际使用的方法；
#'   - combine：合并策略；
#'   - predictors：使用的预测变量列名。
#'
#' @examples
#' # 对数值列 x 插补，只插补 x，其他列作为预测变量
#' res <- impute_column_mice(d, "x", m = 5, combine = "mean")
#' d_imp <- res$data
#' res2 <- impute_column_mice(d, "BMI_cat", m = 10, combine = "mode")
#' d_imp <- res2$data
#' 1
#' \dontrun{
#' library(ZJR)
#' set.seed(1)
#' d <- data.frame(x = rnorm(100), y = sample(c("A","B","C"), 100, TRUE), z = rbinom(100,1,0.5))
#' d$x[sample(100, 40)] <- NA
#' res <- impute_column_mice(d, "x", m = 5, combine = "mean")
#' head(res$data)
#' }
#' @export
impute_column_mice <- function(data, colname, m = 5, maxit = 10, seed = 123,
                               predictors = NULL, method = NULL,
                               combine = c("first", "mean", "mode"),
                               keep_indicator = TRUE) {
  if (!requireNamespace("mice", quietly = TRUE)) {
    stop("请先安装 mice 包：install.packages('mice')")
  }
  stopifnot(is.data.frame(data))
  if (!colname %in% names(data)) stop(sprintf("列 '%s' 不存在", colname))
  
  combine <- match.arg(combine)
  
  # --- 目标列与方法选择 ---
  v <- data[[colname]]
  is_binary <- function(x) {
    ux <- unique(x[!is.na(x)])
    length(ux) == 2
  }
  auto_method <- function(x) {
    if (is.logical(x) || is_binary(x)) return("logreg")
    if (is.numeric(x) || is.integer(x)) return("pmm")
    if (is.factor(x) || is.character(x)) {
      k <- length(unique(x[!is.na(x)]))
      if (k <= 2) return("logreg") else return("polyreg")
    }
    return("pmm")
  }
  used_method <- if (is.null(method)) auto_method(v) else method
  
  # mice 要求分类用 factor；这里在复制的数据中安全转换
  data_imp <- data
  if ((used_method %in% c("logreg","polyreg")) && !is.factor(data_imp[[colname]])) {
    data_imp[[colname]] <- factor(data_imp[[colname]])
  }
  
  # --- 预测矩阵等：注意尺寸必须与传给 mice 的 data 一致 ---
  if (is.null(predictors)) predictors <- setdiff(names(data_imp), colname)
  
  n <- ncol(data_imp)
  predM <- matrix(0, nrow = n, ncol = n, dimnames = list(names(data_imp), names(data_imp)))
  predM[colname, predictors] <- 1
  
  method_vec <- rep("", n); names(method_vec) <- names(data_imp)
  method_vec[colname] <- used_method
  
  whereM <- matrix(FALSE, nrow = nrow(data_imp), ncol = n, dimnames = list(NULL, names(data_imp)))
  whereM[, colname] <- is.na(data_imp[[colname]])
  
  # 重要：指示列在 mice 完成之后再添加，避免尺寸不一致
  add_indicator <- isTRUE(keep_indicator)
  
  imp <- mice::mice(data_imp, m = m, maxit = maxit, seed = seed,
                    method = method_vec, predictorMatrix = predM,
                    where = whereM, printFlag = FALSE)
  
  # --- 合并 ---
  if (combine == "first") {
    completed <- mice::complete(imp, 1)
  } else {
    comps <- mice::complete(imp, action = "all")
    completed <- comps[[1]]
    col_list <- lapply(comps, `[[`, colname)
    
    if (is.numeric(v) || is.integer(v)) {
      if (combine == "mean") {
        avg <- Reduce(`+`, lapply(col_list, function(x) as.numeric(x))) / length(col_list)
        if (is.integer(v)) avg <- as.integer(round(avg))
        completed[[colname]] <- avg
      } else {
        completed[[colname]] <- .zjr_mode_numeric(col_list)
      }
    } else { # 分类/逻辑：众数
      completed[[colname]] <- .zjr_mode_categorical(col_list)
      # 若原列是 character，转换回 character
      if (is.character(data[[colname]])) {
        completed[[colname]] <- as.character(completed[[colname]])
      }
    }
  }
  
  # 现在再安全添加插补指示列
  if (add_indicator) {
    completed[[paste0(colname, "_was_na")]] <- is.na(data[[colname]])
  }
  
  list(
    data = completed,
    mids = imp,
    used_method = used_method,
    combine = combine,
    predictors = predictors
  )
}

# --- helpers ---
# 众数（分类）
.zjr_mode_categorical <- function(col_list) {
  # col_list: list of vectors
  m <- length(col_list)
  n <- length(col_list[[1]])
  out <- vector("character", n)
  for (i in seq_len(n)) {
    vals <- vapply(col_list, function(v) as.character(v[i]), character(1))
    tab <- table(vals, useNA = "no")
    top <- names(tab)[tab == max(tab)]
    out[i] <- if (length(top) == 1) top else top[1]
  }
  out
}

# 众数（数值；并列取均值）
.zjr_mode_numeric <- function(col_list) {
  m <- length(col_list)
  n <- length(col_list[[1]])
  out <- numeric(n)
  for (i in seq_len(n)) {
    vals <- vapply(col_list, function(v) as.numeric(v[i]), numeric(1))
    # 保留小数以减少伪并列，可四舍五入到 6 位
    vals_r <- round(vals, 6)
    tab <- table(vals_r)
    top <- as.numeric(names(tab)[tab == max(tab)])
    out[i] <- if (length(top) == 1) top else mean(top)
  }
  out
}

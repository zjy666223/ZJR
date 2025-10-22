# file: R/inspect_df_vars.R
#' 检查前20列数据结构
#'
#' @description
#' 通用数据检查工具：给定数据框与列名，自动识别分类/连续变量并返回摘要；
#' 同时提供自定义的 `print()` 方法以**竖向块状**展示结果；
#' 也可一键返回适合 `View()` 的长表（含分类变量的全部水平）。
#'
#' @param df data.frame/tibble。
#' @param cols 字符向量，待检查的列名；若缺省，默认使用数据框的前 10 列。
#' @param max_cat_levels 整数，判定分类变量的最大唯一值阈值（超过则不强制判为分类），默认 20。
#' @param max_show 整数，当分类变量唯一值个数大于该阈值时，只展示前 `max_show` 个并在末尾标注总数，默认 20。
#' @param digits 小数位数，连续变量统计量的格式化精度，默认 3。
#' @param include_na_levels 逻辑值，是否在分类变量的唯一值中包含 NA，默认 FALSE。
#' @param return_table 逻辑值，若为 TRUE，返回适合 `View()` 的**长表**（不截断分类变量的水平，显示全部）；
#'        为 FALSE（默认）时返回摘要 data.frame 并使用自定义 `print()` 竖排展示。
#'
#' @return
#' - 当 `return_table = FALSE`：一个 data.frame（列：`column`, `type`, `class`, `summary`），并带有类 `inspect_df_vars`，用于竖排打印。
#' - 当 `return_table = TRUE`：一个**长表** data.frame（列：`column`, `type`, `class`, `metric`, `value`）。
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   sex = sample(c("M","F"), 20, TRUE),
#'   age = sample(18:70, 20, TRUE),
#'   bmi = rnorm(20, 24, 3),
#'   edu = sample(c("High","Bachelor","Master"), 20, TRUE),
#'   stringsAsFactors = FALSE
#' )
#' # 摘要（竖排打印）
#' inspect_df_vars(df)
#' # 返回可 View 的长表（分类变量显示全部水平）
#' tbl <- inspect_df_vars(df, return_table = TRUE); head(tbl)
inspect_df_vars <- function(df,
                            cols = NULL,
                            max_cat_levels = 20,
                            max_show = 20,
                            digits = 3,
                            include_na_levels = FALSE,
                            return_table = FALSE) {
  # 1) 校验 df
  if (!inherits(df, c("data.frame", "tbl_df"))) {
    stop("`df` must be a data.frame or tibble; got ", paste(class(df), collapse = ", "))
  }
  
  # 2) 选择列（默认前 10 列）
  if (is.null(cols)) cols <- names(df)[seq_len(min(10L, ncol(df)))]
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) stop("No valid columns in `cols` found in `df`.")
  
  # 3) 工具：格式化
  fmt_num <- function(x) if (is.finite(x)) formatC(x, format = "f", digits = digits) else NA_character_
  
  # 4a) 如果需要返回长表（不截断分类水平）
  if (isTRUE(return_table)) {
    rows <- lapply(cols, function(col) {
      vec <- df[[col]]
      klass <- class(vec)[1]
      uniq_n <- length(unique(vec))
      is_cat <- is.factor(vec) || is.character(vec) || uniq_n <= max_cat_levels
      if (is_cat) {
        u <- unique(vec)
        if (!include_na_levels) u <- u[!is.na(u)]
        data.frame(
          column = col,
          type   = "categorical",
          class  = klass,
          metric = "level",
          value  = as.character(sort(u)),
          stringsAsFactors = FALSE
        )
      } else if (is.numeric(vec) || is.integer(vec)) {
        data.frame(
          column = rep(col, 3),
          type   = rep("continuous", 3),
          class  = rep(klass, 3),
          metric = c("min","max","mean"),
          value  = c(fmt_num(suppressWarnings(min(vec, na.rm = TRUE))),
                     fmt_num(suppressWarnings(max(vec, na.rm = TRUE))),
                     fmt_num(suppressWarnings(mean(vec, na.rm = TRUE)))),
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          column = col,
          type   = klass,
          class  = klass,
          metric = "info",
          value  = "Unsupported type",
          stringsAsFactors = FALSE
        )
      }
    })
    out_tbl <- do.call(rbind, rows)
    rownames(out_tbl) <- NULL
    return(out_tbl)
  }
  
  # 4b) 返回摘要（可截断分类水平用于打印）
  res <- lapply(cols, function(col) {
    vec <- df[[col]]
    klass <- class(vec)[1]
    
    # 判定：字符/因子 或 唯一值数量较少 ⇒ 倾向分类
    uniq_n <- length(unique(vec))
    is_cat <- is.factor(vec) || is.character(vec) || uniq_n <= max_cat_levels
    
    if (is_cat) {
      u <- unique(vec)
      if (!include_na_levels) u <- u[!is.na(u)]
      n_levels <- length(u)
      vals <- sort(u)
      if (n_levels > max_show) {
        vals <- vals[1:max_show]
        info <- paste0(paste(vals, collapse = ", "),
                       " (", n_levels, " levels; showing first ", max_show, ")")
      } else {
        info <- paste(vals, collapse = ", ")
      }
      typ <- "categorical"
    } else if (is.numeric(vec) || is.integer(vec)) {
      rng_min <- suppressWarnings(min(vec, na.rm = TRUE))
      rng_max <- suppressWarnings(max(vec, na.rm = TRUE))
      mu <- suppressWarnings(mean(vec, na.rm = TRUE))
      info <- paste0("min=", fmt_num(rng_min), ", max=", fmt_num(rng_max), ", mean=", fmt_num(mu))
      typ <- "continuous"
    } else {
      info <- "Unsupported type"
      typ <- klass
    }
    
    data.frame(
      column = col,
      type = typ,
      class = klass,
      summary = info,
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, res)
  class(out) <- c("inspect_df_vars", class(out))
  out
}

#' Pretty print for `inspect_df_vars`
#'
#' 竖向块状打印，每个变量单独一组，阅读更友好。
#'
#' @param x `inspect_df_vars` 对象。
#' @param ... 保留参数。
#' @export
#' @method print inspect_df_vars
print.inspect_df_vars <- function(x, ...) {
  if (!is.data.frame(x) || !all(c("column","type","class","summary") %in% names(x))) {
    return(NextMethod())
  }
  n <- nrow(x)
  if (n == 0) {
    cat("<inspect_df_vars: empty>\n")
    return(invisible(x))
  }
  for (i in seq_len(n)) {
    cat("\n", i, ") ", x$column[i], "\n", sep = "")
    cat("    type   : ", x$type[i], "\n", sep = "")
    cat("    class  : ", x$class[i], "\n", sep = "")
    cat("    summary: ", x$summary[i], "\n", sep = "")
  }
  invisible(x)
}

# 文件: D:/12自定义封装R包/ZJR/R/var_dict.R

#' 变量字典生成器（自动赋值并 View）
#'
#' 目标：在脚本里只需敲 `var_dict(d)`，即可等价于
#' `dict <- var_dict(d); View(dict)`。
#' 函数会把结果自动赋值到全局环境的 `dict` 对象，并在交互会话中自动 `View()`。
#'
#' @param df data.frame/tibble。
#' @param max_cat_levels 整数阈值：唯一值个数 \<= `max_cat_levels` 时倾向判为分类变量；默认 20。
#' @param sample_n 示例值数量（仅用于分类变量展示），默认 5。
#' @param include_na_levels 是否在分类变量示例中包含 NA，默认 FALSE。
#' @param digits 连续变量摘要保留小数位，默认 3。
#' @param assign_name 赋值到全局环境的对象名，默认 "dict"。
#' @param view 是否自动 `View()`，默认 TRUE。
#'
#' @return 返回生成的字典 data.frame（并隐式赋值到全局环境）。
#' @examples
#' \dontrun{
#' # 一键（最省事）：
#' var_dict(d)  # 等价于 dict <- var_dict(d); View(dict)
#'
#' # 改名且不 View：
#' var_dict(d, assign_name = "mydict", view = FALSE)
#'
#' # 仅返回值，不做副作用：
#' res <- var_dict(d, view = FALSE, assign_name = "tmp")
#' }
#' @export
var_dict <- function(df,
                     max_cat_levels = 20,
                     sample_n = 5,
                     include_na_levels = FALSE,
                     digits = 3,
                     assign_name = "dict",
                     view = TRUE) {
  if (!inherits(df, c("data.frame", "tbl_df"))) {
    stop("`df` must be a data.frame or tibble; got ", paste(class(df), collapse = ", "))
  }
  
  fmt_num <- function(x) if (is.finite(x)) formatC(x, format = "f", digits = digits) else ""
  
  cols <- names(df)
  out <- lapply(cols, function(col) {
    v <- df[[col]]
    klass <- class(v)[1]
    n <- length(v)
    n_miss <- sum(is.na(v))
    prop_miss <- if (n > 0) n_miss / n else NA_real_
    
    # 唯一值统计
    u <- unique(v)
    u_cnt <- length(u)
    
    # 判定类型
    is_cat <- is.factor(v) || is.character(v) || u_cnt <= max_cat_levels
    type <- if (is_cat) "categorical" else if (is.numeric(v) || is.integer(v)) "continuous" else klass
    
    # 示例值（分类）
    examples <- ""
    if (type == "categorical") {
      u_show <- if (!include_na_levels) u[!is.na(u)] else u
      if (is.character(u_show)) u_show <- sort(u_show) else u_show <- sort(u_show, na.last = TRUE)
      if (length(u_show) > sample_n) u_show <- u_show[seq_len(sample_n)]
      examples <- paste(u_show, collapse = ", ")
    }
    
    # 连续变量统计
    vmin <- vmax <- vmean <- vsd <- ""
    if (type == "continuous") {
      vmin  <- fmt_num(suppressWarnings(min(v, na.rm = TRUE)))
      vmax  <- fmt_num(suppressWarnings(max(v, na.rm = TRUE)))
      vmean <- fmt_num(suppressWarnings(mean(v, na.rm = TRUE)))
      vsd   <- fmt_num(suppressWarnings(stats::sd(v, na.rm = TRUE)))
    }
    
    data.frame(
      column = col,
      type = type,
      class = klass,
      n = n,
      n_miss = n_miss,
      prop_miss = prop_miss,
      n_unique = u_cnt,
      examples = examples,
      min = vmin,
      max = vmax,
      mean = vmean,
      sd = vsd,
      stringsAsFactors = FALSE
    )
  })
  
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out <- out[order(out$column), ]
  
  # === 自动赋值到全局环境 + 自动 View ===
  # 始终赋值到 .GlobalEnv，以便脚本里直接看到 dict 对象
  try(assign(assign_name, out, envir = .GlobalEnv), silent = TRUE)
  if (isTRUE(view) && interactive()) {
    try(utils::View(get(assign_name, envir = .GlobalEnv)), silent = TRUE)
  }
  
  # 返回值（invisible：避免控制台重复打印大表）
  invisible(out)
}

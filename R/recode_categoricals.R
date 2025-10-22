
#' 批量将数据框中的分类变量编码为 0,1,...
#'
#' 自动识别数据框中的分类变量（字符/因子/逻辑），或按 `cols` 指定的列集合，
#' 将其按水平顺序编码为 `start, start+1, ...`。可选择是否仅识别“看起来像分类”的
#' 整数列（唯一值数不超过 `int_max_levels`）。
#'
#' @param data 数据框。
#' @param cols 需要编码的列名字符向量；默认 `NULL` 表示自动识别。
#' @param include_integer 逻辑值，是否把“看起来像分类”的整数列也一起编码。
#'   仅当该列是整数型且唯一值个数 `<= int_max_levels` 时视为分类。默认 `FALSE`。
#' @param int_max_levels 识别整数列为分类的最大水平数阈值，默认 10。
#' @param append 是否追加新列，见 `recode_categorical()`。默认 `TRUE`。
#' @param suffix 新列后缀，默认 `"_num"`。
#' @param start 起始编码，默认 0。
#' @param to_numeric 是否输出数值型列，默认 `TRUE`。
#'
#' @return 返回编码后的数据框。
#' @examples
#' d <- data.frame(sex = c("M","F","M"), edu = factor(c("low","high","low")),
#'                 grp = c(1L, 2L, 1L), x = 3.2)
#' # 自动识别字符/因子列
#' d1 <- recode_categoricals(d)
#' # 包含整数型（小水平数）一起编码
#' d2 <- recode_categoricals(d, include_integer = TRUE, int_max_levels = 5)
#' # 指定列名
#' d3 <- recode_categoricals(d, cols = c("sex","edu"), append = FALSE)
#' @export
recode_categoricals <- function(data, cols = NULL, include_integer = FALSE,
                                int_max_levels = 10, append = TRUE, suffix = "_num",
                                start = 0, to_numeric = TRUE) {
  stopifnot(is.data.frame(data))
  
  if (is.null(cols)) {
    is_char_fac <- vapply(data, function(v) is.character(v) || is.factor(v) || is.logical(v), logical(1))
    cols_auto <- names(data)[is_char_fac]
    
    if (isTRUE(include_integer)) {
      is_int_categ <- vapply(data, function(v) {
        is.integer(v) && length(unique(v[!is.na(v)])) <= int_max_levels
      }, logical(1))
      cols_auto <- union(cols_auto, names(data)[is_int_categ])
    }
    cols <- cols_auto
  }
  
  if (!length(cols)) {
    message("未识别到需要编码的列，返回原数据框。")
    return(data)
  }
  
  # 依次编码
  out <- data
  for (cn in cols) {
    out <- tryCatch(
      recode_categorical(out, cn, append = append, suffix = suffix,
                         start = start, to_numeric = to_numeric),
      error = function(e) { warning(e$message); out }
    )
  }
  out
}

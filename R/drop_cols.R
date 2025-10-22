# 文件: D:/12自定义封装R包/ZJR/R/drop_cols.R

#' 删除或保留数据框中的列
#'
#' @description
#' 批量删除或保留数据框中的列。支持精确匹配或模糊匹配。
#'
#' @param data 数据框或 tibble。
#' @param cols 列名字符向量。
#' @param keep 逻辑值，TRUE 表示只保留 cols（反选模式），FALSE 表示删除 cols（默认）。
#' @param fuzzy 逻辑值，是否启用模糊匹配（默认 FALSE）。
#' @param ignore_missing 逻辑值，是否忽略不存在的列（默认 TRUE）。
#'
#' @return 删除或保留指定列后的数据框。
#' @examples
#' # 删除列
#' df <- drop_cols(d, c("sex", "BMI_cat"))
#' 只保留列（反选
#' df <- drop_cols(d, c("age", "PM2.5_per5"), keep = TRUE)
#' 1
#' df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
#' drop_cols(df, c("a", "c"))                  # 删除 a 和 c
#' drop_cols(df, c("a", "c"), keep = TRUE)      # 只保留 a 和 c
#' drop_cols(df, "b", fuzzy = TRUE)             # 模糊匹配列名
#' @export
drop_cols <- function(data, cols, keep = FALSE, fuzzy = FALSE, ignore_missing = TRUE) {
  stopifnot(is.data.frame(data))
  if (missing(cols) || length(cols) == 0) {
    stop("请提供要删除或保留的列名向量 cols")
  }
  
  if (fuzzy) {
    matched <- unique(unlist(lapply(cols, function(pattern) {
      grep(pattern, names(data), value = TRUE)
    })))
  } else {
    matched <- intersect(names(data), cols)
  }
  
  if (!ignore_missing) {
    not_found <- setdiff(cols, names(data))
    if (length(not_found) > 0) {
      stop("以下列在数据中未找到: ", paste(not_found, collapse = ", "))
    }
  } else {
    not_found <- setdiff(cols, matched)
    if (length(not_found) > 0) {
      message("已忽略未找到的列: ", paste(not_found, collapse = ", "))
    }
  }
  
  if (keep) {
    return(data[, matched, drop = FALSE])
  } else {
    return(data[, setdiff(names(data), matched), drop = FALSE])
  }
}

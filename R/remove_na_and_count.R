
#' 删除指定列中的NA并统计删除行数
#' @description 删除数据框中指定列的NA值，并返回删除后的数据及删除信息
#' @param data 数据框
#' @param col_name 列名（字符型）
#' @return 删除NA后的数据框
#' @examples
#' df <- data.frame(a = c(1, NA, 3), b = c("x", "y", "z"))
#' remove_na_and_count(df, "a")
#' @export
remove_na_and_count <- function(data, col_name) {
  stopifnot(col_name %in% names(data))  # 确保列存在
  
  before_n <- nrow(data)
  data_clean <- data[!is.na(data[[col_name]]), ]
  after_n <- nrow(data_clean)
  removed_n <- before_n - after_n
  
  cat("删除前行数:", before_n, "\n")
  cat("删除后行数:", after_n, "\n")
  cat("删除的行数:", removed_n, "\n")
  
  return(data_clean)
}


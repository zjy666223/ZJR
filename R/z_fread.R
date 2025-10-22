# file: R/z_fread.R  (修正版)
#' z_fread: Fast and robust CSV/TSV reader (wrapper of data.table::fread)
#' @export
#' @importFrom data.table fread
z_fread <- function(path,
                    sep = ",",
                    encoding = "UTF-8",
                    na = c("", "NA", "NaN"),
                    select = NULL,
                    drop = NULL,
                    n_max = NULL,
                    as_data_frame = TRUE,
                    show_progress = FALSE) {
  if (!length(path) || is.na(path)) stop("`path` must be a non-empty string.")
  if (!file.exists(path)) stop("File not found: ", path)
  
  # 修正点：fread 的 nrows 不能是 NULL；未指定时应为 -1L（读全部）
  if (is.null(n_max)) {
    nrows <- -1L
  } else {
    if (!is.numeric(n_max) || length(n_max) != 1L || !is.finite(n_max) || n_max < 0)
      stop("`n_max` must be a single non-negative finite number.")
    nrows <- as.integer(n_max)
  }
  
  dt <- data.table::fread(
    file = path,
    sep = sep,
    encoding = encoding,
    na.strings = na,
    select = select,
    drop = drop,
    nrows = nrows,
    showProgress = show_progress
  )
  
  if (isTRUE(as_data_frame)) return(as.data.frame(dt, stringsAsFactors = FALSE))
  dt
}

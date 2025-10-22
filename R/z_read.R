#' z_read: Fast in-memory column picking from very large CSV
#'
#' 仅从超大 CSV 中按**列名模式/正则**选择需要的列并读入内存（不生成中间文件）。
#' 基于 data.table::fread 的列裁剪与多线程解析，速度快、内存小。
#'
#' @param pattern 字符串或正则，用于匹配列名（如 `"41270"` 或 `"^(20002|20003|41270)"`）。
#' @param csv_path CSV 路径；默认 `"F:/UKB/总数据.csv"`。
#' @param include_eid 是否强制包含 `eid` 列（默认 TRUE）。
#' @param eid_name `eid` 列名（默认 `"eid"`；若表头不同可改）。
#' @param n_max 仅读取前 n 行预览；默认 `Inf`（全量）。
#' @param threads 并行线程数；默认 `NULL` 表示用 (逻辑核数 - 1) 并在函数内临时设置。
#' @param encoding 文件编码，默认 `"UTF-8"`。
#' @param ignore_case 是否忽略大小写匹配列名，默认 TRUE。
#'
#' @return data.table（兼容 data.frame）
#' @examples
#' \dontrun{
#' # eid + 所有包含 41270 的列
#' d <- ZJR::z_read("41270")
#'
#' # 多个前缀
#' d <- ZJR::z_read("^(20002|20003|41270)")
#'
#' # 预览前5万行
#' d <- ZJR::z_read("41270", n_max = 50000)
#'
#' # 自定义 eid 列名
#' d <- ZJR::z_read("41270", eid_name = "eid_0_0")
#' }
#' @export
z_read <- function(
    pattern,
    csv_path    = "F:/UKB/总数据.csv",
    include_eid = TRUE,
    eid_name    = "eid",
    n_max       = Inf,
    threads     = NULL,
    encoding    = "UTF-8",
    ignore_case = TRUE
) {
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required. Please install.packages('data.table').")
  }
  if (!requireNamespace("parallel", quietly = TRUE)) {
    stop("Package 'parallel' is required. Please install.packages('parallel').")
  }
  
  # 线程：未指定则用 逻辑核数-1，并在退出时恢复
  if (is.null(threads)) {
    threads <- max(1L, parallel::detectCores(logical = TRUE) - 1L)
  }
  old_threads <- data.table::getDTthreads()
  on.exit(try(data.table::setDTthreads(old_threads), silent = TRUE), add = TRUE)
  data.table::setDTthreads(threads)
  
  # 只读表头拿列名
  if (!file.exists(csv_path)) {
    stop(sprintf("CSV not found: %s", csv_path))
  }
  header_dt <- data.table::fread(
    csv_path, nrows = 0L, encoding = encoding, showProgress = FALSE
  )
  cols <- names(header_dt)
  
  # 匹配列名
  matched <- cols[grepl(pattern, cols, ignore.case = ignore_case)]
  keep <- matched
  if (include_eid) {
    if (eid_name %in% cols) keep <- unique(c(eid_name, matched)) else
      warning(sprintf("eid column '%s' not found; proceeding without it.", eid_name))
  }
  if (length(keep) == 0L) {
    stop("No columns matched the pattern. Check 'pattern' and column names.")
  }
  
  # 仅读所需列进内存
  dt <- data.table::fread(
    csv_path,
    select = keep,
    encoding = encoding,
    showProgress = TRUE
  )
  if (is.finite(n_max)) {
    n_max <- as.integer(n_max)
    if (n_max < nrow(dt)) dt <- dt[seq_len(n_max)]
  }
  dt
}

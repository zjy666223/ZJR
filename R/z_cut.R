#' z_cut: Create continuous (scaled) and quantile categories for an exposure
#'
#' @description
#' From a raw exposure column, create:
#' - `{expose}_s`: continuous (scaled by `scale`);
#' - `{expose}_IQR`: categorical by quantile cuts.
#' Robust to NAs, duplicated quantile breaks, non-numeric input.
#'
#' @param data data.frame/data.table containing the exposure and id.
#' @param id_col character. ID column name (default "eid").
#' @param expose_col character. Exposure column name (e.g. "twa_benzene").
#' @param n_q integer. Number of quantile groups (default 4). Ignored if `breaks` provided.
#' @param breaks numeric vector of cut points (including min/max). If provided, used directly.
#' @param labels optional labels for categories. If NULL, interval labels from `cut()` are used.
#' @param right logical passed to `cut()`; default `TRUE`.
#' @param scale numeric multiplier for `{expose}_s` (default 1).
#'
#' @return data.frame with columns: `id_col`, `{expose}_s`, `{expose}_IQR`
#' @examples
#' \dontrun{
#' # 最常用：四分位 + 不缩放
#' exposure_data1 <- z_cut(exposure_data,
#'   id_col = "eid", expose_col = "twa_benzene", n_q = 4, scale = 1
#' )
#'
#' # 自定义断点+标签
#' exposure_data1 <- z_cut(exposure_data, "eid", "twa_benzene",
#'   breaks = c(0.0588, 0.354, 0.431, 0.506, 1.14),
#'   labels = c("[0.059,0.354]", "(0.354,0.431]", "(0.431,0.506]", "(0.506,1.14]"),
#'   scale = 1
#' )
#' }
#' @export
z_cut <- function(data,
                  id_col = "eid",
                  expose_col,
                  n_q = 4,
                  breaks = NULL,
                  labels = NULL,
                  right = TRUE,
                  scale = 1) {
  # --- checks ---
  if (!is.data.frame(data)) stop("`data` must be a data.frame/data.table")
  if (!id_col %in% names(data)) stop("Missing id_col: ", id_col)
  if (missing(expose_col) || !expose_col %in% names(data))
    stop("Missing expose_col in data: ", expose_col)
  
  x_raw <- data[[expose_col]]
  x_num <- suppressWarnings(as.numeric(x_raw))
  # 全 NA 或无有效数值
  if (all(is.na(x_num))) {
    stop("`expose_col` has no numeric values after coercion.")
  }
  
  # --- build breaks ---
  bx <- breaks
  if (is.null(bx)) {
    probs <- seq(0, 1, length.out = n_q + 1)
    bx <- stats::quantile(x_num, probs = probs, na.rm = TRUE, names = FALSE)
    bx <- unique(bx)
    # 断点不足以分组（例如常数列）→ 用范围包裹
    if (length(bx) < 2) {
      rng <- range(x_num, na.rm = TRUE)
      # 两端加极小抖动，确保 cut 可用
      eps <- max(1e-9, diff(rng) * 1e-6)
      bx <- c(rng[1] - eps, rng[2] + eps)
    }
  } else {
    if (is.unsorted(bx, strictly = TRUE)) bx <- sort(unique(bx))
  }
  
  # --- cut ---
  cat_var <- withCallingHandlers(
    expr = cut(x_num, breaks = bx, include.lowest = TRUE, right = right, labels = labels),
    warning = function(w) {
      # 捕获“some 'x' not between breaks”类告警，不中断
      invokeRestart("muffleWarning")
    }
  )
  
  # --- scaled continuous ---
  cont_var <- x_num * scale
  
  # --- assemble output ---
  out <- data.frame(
    data[[id_col]],
    cont_var,
    cat_var,
    check.names = FALSE
  )
  names(out) <- c(id_col, paste0(expose_col, "_s"), paste0(expose_col, "_IQR"))
  out
}

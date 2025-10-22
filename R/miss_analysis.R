# file: R/miss_analysis.R
#' 智能缺失值分析与可视化
#'
#' @description
#' res <- miss_analysis(df, top_n = 30)
#' res$table                # 缺失表
#' print(res$bar_plot)      # 缺失比例 TopN 条形图
#' print(res$heatmap)       # 缺失热力图（采样行）。
#'
#' @param df data.frame/tibble。
#' @param top_n 仅绘图：条形图中展示缺失比例最高的前 `top_n` 列；默认 NULL 表示展示全部。
#' @param make_bar 是否绘制缺失比例条形图，默认 TRUE。
#' @param make_heatmap 是否绘制缺失热力图，默认 TRUE。
#' @param heatmap_max_rows 热力图最大采样行数（避免大数据爆显存），默认 1000。
#' @param seed 采样随机种子，默认 1。
#'
#' @return list：\describe{
#'   \item{table}{data.frame，列：`column`, `n`, `n_miss`, `prop_miss`（降序）}
#'   \item{bar_plot}{ggplot 对象或 NULL}
#'   \item{heatmap}{ggplot 对象或 NULL}
#' }
#' @export
#'
#' @examples
#' res <- miss_analysis(df, top_n = 30)
#' res$table                # 缺失表
#' print(res$bar_plot)      # 缺失比例 TopN 条形图
#' print(res$heatmap)       # 缺失热力图（采样行）
#' 1
#' set.seed(1)
#' df <- data.frame(
#'   x = c(1, NA, 3, 4, NA),
#'   y = c("a","b", NA, "a", NA),
#'   z = rnorm(5)
#' )
#' res <- miss_analysis(df, top_n = 2)
#' res$table
#' if (!is.null(res$bar_plot)) print(res$bar_plot)
#' if (!is.null(res$heatmap))  print(res$heatmap)
miss_analysis <- function(df,
                          top_n = NULL,
                          make_bar = TRUE,
                          make_heatmap = TRUE,
                          heatmap_max_rows = 1000,
                          seed = 1) {
  if (!inherits(df, c("data.frame", "tbl_df"))) {
    stop("`df` must be a data.frame or tibble; got ", paste(class(df), collapse = ", "))
  }
  
  # 1) 缺失表
  n <- nrow(df)
  miss_tbl <- data.frame(
    column = names(df),
    n = n,
    n_miss = vapply(df, function(v) sum(is.na(v)), integer(1)),
    stringsAsFactors = FALSE
  )
  miss_tbl$prop_miss <- with(miss_tbl, ifelse(n > 0, n_miss / n, NA_real_))
  miss_tbl <- miss_tbl[order(-miss_tbl$prop_miss, miss_tbl$column), ]
  rownames(miss_tbl) <- NULL
  
  # 2) 条形图（可选）
  bar_plot <- NULL
  if (isTRUE(make_bar)) {
    suppressPackageStartupMessages(requireNamespace("ggplot2", quietly = TRUE))
    plot_df <- miss_tbl
    if (!is.null(top_n) && is.numeric(top_n) && top_n > 0) {
      plot_df <- head(plot_df, top_n)
    }
    plot_df$column <- factor(plot_df$column, levels = rev(plot_df$column))
    bar_plot <- ggplot2::ggplot(plot_df, ggplot2::aes(x = column, y = prop_miss)) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +
      ggplot2::labs(x = NULL, y = "Missing proportion", title = "Top missingness by variable") +
      ggplot2::theme_minimal()
  }
  
  # 3) 热力图（可选，对行进行采样）
  heatmap <- NULL
  if (isTRUE(make_heatmap) && n > 0) {
    suppressPackageStartupMessages(requireNamespace("ggplot2", quietly = TRUE))
    suppressPackageStartupMessages(requireNamespace("tidyr", quietly = TRUE))
    
    set.seed(seed)
    idx <- if (n > heatmap_max_rows) sort(sample.int(n, heatmap_max_rows)) else seq_len(n)
    miss_mat <- as.data.frame(lapply(df[idx, , drop = FALSE], function(v) as.integer(is.na(v))))
    miss_mat$.row <- seq_along(idx)
    
    long_df <- tidyr::pivot_longer(miss_mat, cols = -".row", names_to = "column", values_to = "is_na")
    
    heatmap <- ggplot2::ggplot(long_df, ggplot2::aes(x = column, y = .row, fill = is_na)) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_continuous(type = "viridis") +
      ggplot2::labs(x = NULL, y = "row (sampled)", title = "Missingness heatmap: 1 = NA, 0 = non-NA") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5))
  }
  
  list(table = miss_tbl, bar_plot = bar_plot, heatmap = heatmap)
}
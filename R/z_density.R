# file: R/z_density.R
#' z_density: Overlapping density panels with facets (publication-ready)
#'
#' @description
#' Draw side-by-side **overlapping density plots** (one panel per `facet`)
#' with transparent fills, custom colors, and journal-like theme. Designed to
#' reproduce "Regular vs Weekend" style panels.
#'
#' @param data data.frame/data.table.
#' @param x Character. Numeric column for x-axis.
#' @param group Character. Grouping column for color/fill.
#' @param facet Character. Facet column (one panel per level).
#' @param panel_order Optional character vector to fix facet order.
#' @param group_levels Optional character vector to fix group order (and palette mapping).
#' @param palette Named character vector (names must match `group_levels` or unique groups).
#'   Default: c("Remaining 5d"="#F06A6A", "Top 2d"="#1CC0C0")，若找不到名称则按顺序使用其值。
#' @param alpha Fill transparency. Default 0.40.
#' @param linewidth Density line width. Default 0.6.
#' @param adjust Kernel bandwidth adjustment (ggplot2::geom_density). Default 1.0.
#' @param xlim Numeric length-2, x-axis range (NULL = auto).
#' @param xlab,ylab Axis labels.
#' @param panel_titles Optional character vector with custom titles per facet level,
#'   recycled if length < n_panels. If NULL, uses facet level texts.
#' @param theme One of "classic","minimal","bw". Default "classic".
#' @param width,height,dpi Export size when `file` is not NULL. Defaults 10 x 4 inch, 300 dpi.
#' @param file Optional path to save figure via ggsave(); if NULL, not saved.
#'
#' @return A ggplot object (patchwork) of the combined panels.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' set.seed(1)
#' df <- dplyr::bind_rows(
#'   tibble::tibble(pattern="Regular", group="Remaining 5d", weekly_mvpa=rgamma(1500,6,0.03)),
#'   tibble::tibble(pattern="Regular", group="Top 2d",      weekly_mvpa=rgamma(1500,7.5,0.035)),
#'   tibble::tibble(pattern="Weekend", group="Remaining 5d", weekly_mvpa=rgamma(1500,5,0.028)),
#'   tibble::tibble(pattern="Weekend", group="Top 2d",      weekly_mvpa=rgamma(1500,6.5,0.03))
#' )
#'
#' p <- z_density(
#'   data  = df,
#'   x     = "weekly_mvpa",
#'   group = "group",
#'   facet = "pattern",
#'   xlab = "WEEKLY_MVPA, min",
#'   ylab = "DENSITY",
#'   xlim = c(0, 800)
#' )
#' print(p)
#' # ggplot2::ggsave("density_panels.png", p, width = 10, height = 4, dpi = 300)
#' }
#'
#' @export
#' @import ggplot2
#' @importFrom dplyr filter %>% arrange
#' @importFrom rlang .data
z_density <- function(
    data,
    x,
    group,
    facet,
    panel_order   = NULL,
    group_levels  = NULL,
    palette       = c("Remaining 5d"="#F06A6A", "Top 2d"="#1CC0C0"),
    alpha         = 0.40,
    linewidth     = 0.6,
    adjust        = 1.0,
    xlim          = NULL,
    xlab          = "WEEKLY_MVPA, min",
    ylab          = "DENSITY",
    panel_titles  = NULL,
    theme         = c("classic","minimal","bw"),
    width         = 10,
    height        = 4,
    dpi           = 300,
    file          = NULL
){
  # ---- validate ----
  if ("data.table" %in% class(data)) data.table::setDF(data)
  if (!is.data.frame(data)) stop("`data` must be a data.frame/data.table")
  need <- c(x, group, facet)
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  # ensure numeric x
  if (!is.numeric(data[[x]])) {
    xv <- suppressWarnings(as.numeric(data[[x]]))
    if (anyNA(xv)) stop("`x` must be numeric (coercion produced NA).")
    data[[x]] <- xv
  }
  
  # facet order
  f_levels <- if (is.null(panel_order)) unique(as.character(data[[facet]])) else panel_order
  data[[facet]] <- factor(as.character(data[[facet]]), levels = f_levels)
  
  # group order
  g_levels <- if (is.null(group_levels)) unique(as.character(data[[group]])) else group_levels
  data[[group]] <- factor(as.character(data[[group]]), levels = g_levels)
  
  # palette mapping
  if (is.null(names(palette))) names(palette) <- as.character(g_levels)
  if (!all(as.character(g_levels) %in% names(palette))) {
    # fallback: use as many colors as provided in order
    palette <- setNames(palette[seq_along(g_levels) %% length(palette) + (length(palette)==length(palette))*0], g_levels)
  }
  
  theme <- match.arg(theme)
  base_theme <-
    switch(theme,
           classic = ggplot2::theme_classic(base_size = 12),
           minimal = ggplot2::theme_minimal(base_size = 12),
           bw      = ggplot2::theme_bw(base_size = 12)
    ) +
    ggplot2::theme(
      legend.position = "right",
      panel.border    = ggplot2::element_blank()
    )
  
  # titles per panel
  if (is.null(panel_titles)) {
    panel_titles <- as.character(f_levels)
  }
  if (length(panel_titles) < length(f_levels)) {
    panel_titles <- rep(panel_titles, length.out = length(f_levels))
  }
  
  # helper to build one panel
  build_panel <- function(df_one, title_text, label_prefix){
    ggplot2::ggplot(df_one, ggplot2::aes(x = .data[[x]], fill = .data[[group]], color = .data[[group]])) +
      ggplot2::geom_density(alpha = alpha, linewidth = linewidth, adjust = adjust) +
      ggplot2::scale_fill_manual(values = palette, drop = FALSE) +
      ggplot2::scale_color_manual(values = palette, drop = FALSE) +
      ggplot2::labs(
        x = xlab, y = ylab,
        title = sprintf("%s %s", label_prefix, title_text),
        fill = "Group", color = "Group"
      ) +
      { if (!is.null(xlim)) ggplot2::coord_cartesian(xlim = xlim) else ggplot2::coord_cartesian() } +
      base_theme
  }
  
  # build all panels in facet order; prefix a./b./c.
  panels <- vector("list", length(f_levels))
  for (i in seq_along(f_levels)) {
    lev <- f_levels[i]
    df_i <- data[data[[facet]] == lev, , drop = FALSE]
    panels[[i]] <- build_panel(df_i, panel_titles[i], paste0(letters[i], "."))
  }
  
  # combine
  if (!requireNamespace("patchwork", quietly = TRUE))
    stop("Package 'patchwork' is required. Please install.packages('patchwork').")
  
  combined <- Reduce(`+`, panels)
  combined <- combined + patchwork::plot_layout(ncol = length(panels), widths = rep(1, length(panels)))
  
  # export
  if (!is.null(file)) {
    ggplot2::ggsave(filename = file, plot = combined, width = width, height = height, dpi = dpi)
  }
  
  combined
}

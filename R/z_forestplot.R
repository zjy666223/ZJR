#' Subgroup forest plot (publication-ready) with smart column mapping
#'
#' Create a forest plot for subgroup analysis from a tidy table.
#' Now supports **automatic column detection** (CN/EN aliases, case/space tolerant),
#' manual/partial mapping, and column-index mapping. Robust parser for
#' `HR (95% CI)` also accepts `OR`/`RR` and mixed punctuation `（ ）`,
#' square brackets \[ \], en-dash, `"to"`, etc.
#' If your `forestplot` version does not support `background` in `fpColors()`,
#' the option is ignored gracefully.
#'
#' @param data data.frame containing subgroup table.
#' @param cols \strong{Optional}. Named list mapping input columns. Any of the
#'   following keys can be provided: \code{subgroup}, \code{level}, \code{events},
#'   \code{total_py}, \code{hrci}, \code{phet}. Each value can be a column \strong{name}
#'   (character) or \strong{index} (integer). Keys you omit will be auto-detected.
#'   Examples:
#'   \itemize{
#'     \item \code{cols = list(subgroup="分组", level="水平", hrci="HR95CI")}
#'     \item \code{cols = list(hrci=5, events=3)}  (by column index)
#'   }
#' @param headers character vector for table headers. Length 5 or 6 when P column exists.
#'   Default: c("Subgroup","Level","Events","Total / Person-years",
#'              "Adjusted HR (95% CI)","P for heterogeneity").
#' @param colors named list passed to `forestplot::fpColors()`, e.g.
#'   list(box="black", lines="black", summary="black", zero="gray60",
#'        background=c("transparent","#E6EEF7")). Unsupported fields are ignored.
#' @param show_p_in_header logical; show P only on subgroup header rows (default TRUE).
#' @param left_right_border logical; draw vertical border lines at left/right (default FALSE).
#' @param border_col color of border lines (default "gray40").
#' @param xlab x-axis label (default "Adjusted HR (95% CI)"). If set to NULL,
#'   it will auto-switch to "Adjusted OR/RR (95% CI)" according to detected measure.
#' @param draw logical; draw immediately (default TRUE).
#' @param verbose logical; print diagnostics and the resolved column mapping (default FALSE).
#' @param ... additional args forwarded to `forestplot::forestplot()`
#'   (xticks, clip, graph.pos, boxsize, lwd.ci, txt_gp, hrzl_lines, graphwidth, etc.).
#'
#' @return An object returned by `forestplot::forestplot()` (invisible).
#'
#' @usage
#' # Minimal (auto-detect column names in CN/EN/aliases)
#' z_forestplot(data)
#'
#' # Manually override or partially specify mapping (name or index)
#' z_forestplot(
#'   data,
#'   cols = list(subgroup = "分组", level = "水平", events = "事件数")
#' )
#' z_forestplot(
#'   data,
#'   cols = list(hrci = 5, events = 3)  # by column index
#' )
#'
#' # Customize appearance; P值仅显示在组标题行；自动识别 HR/OR/RR 并设置 x 轴标签
#' z_forestplot(
#'   data,
#'   colors = list(box="#f0e68c", lines="#f79c42", summary="black", zero="gray60",
#'                 background=c("transparent","#E6EEF7")),
#'   xticks = c(0.5,1,2,5,10),
#'   clip = c(0.4, 20),
#'   txt_gp = forestplot::fpTxtGp(
#'     ticks = grid::gpar(cex=0.7),
#'     label = grid::gpar(cex=0.9),
#'     xlab  = grid::gpar(cex=0.9)
#'   ),
#'   left_right_border = TRUE,
#'   verbose = TRUE
#' )
#'
#' # Export high-res figures
#' z_forestplot_save("forest.pdf", data, width = 8, height = 6)
#' z_forestplot_save("forest.png", data, width = 8, height = 6, res = 300)
#'
#' @examples
#' \dontrun{
#' # See Usage. You can keep your CSV headers in Chinese; no need to rename.
#' }
#'
#' @importFrom forestplot forestplot fpColors fpTxtGp
#' @importFrom grid gpar unit grid.lines
#' @importFrom tools file_ext
#' @export
z_forestplot <- function(
    data,
    cols = NULL,
    headers = c(
      "Subgroup", "Level", "Events",
      "Total / Person-years", "Adjusted HR (95% CI)", "P for heterogeneity"
    ),
    colors  = list(box = "black", lines = "black", summary = "black", zero = "gray60"),
    show_p_in_header = TRUE,
    left_right_border = FALSE,
    border_col = "gray40",
    xlab = NULL,
    draw = TRUE,
    verbose = FALSE,
    ...
){
  stopifnot(is.data.frame(data))
  df <- data
  .nzchar <- function(x) (!is.na(x)) & nzchar(x)
  .to_chr <- function(x) if (is.null(x)) "" else as.character(x)
  .norm <- function(x) { x <- tolower(gsub("\\s+", "", x)); gsub("[^a-z0-9]", "", x) }
  syns <- list(
    subgroup = c("subgroup","group","sub_group","亚组","分组","亚组名称","组"),
    level    = c("level","category","类别","层级","水平","组别","分层","等级"),
    events   = c("events","event","cases","n","count","例数","事件数","发生数","病例数"),
    total_py = c("total/personyears","totalpersonyears","total","personyears","py",
                 "样本量","总数","总计","合计","随访人年","总/人年","人年","总样本"),
    hrci     = c("hr(95%ci)","adjustedhr(95%ci)","or(95%ci)","rr(95%ci)","hr95ci",
                 "or95ci","rr95ci","hazardratio95ci","oddsratio95ci","riskratio95ci",
                 "效应量95ci","比值比95ci","风险比95ci","estimate95ci"),
    phet     = c("p_heterogeneity","pforheterogeneity","phet","heterogeneityp",
                 "异质性p","交互作用p","交互p","p异质性")
  )
  .resolve_col <- function(val, nm) {
    if (is.null(val)) return(NULL)
    if (is.numeric(val) && length(val) == 1L) {
      idx <- as.integer(val); if (idx >= 1L && idx <= length(nm)) return(nm[idx]); return(NULL)
    }
    if (is.character(val) && length(val) == 1L) {
      if (val %in% nm) return(val)
      nn <- .norm(nm); target <- .norm(val)
      hit <- which(nn == target); if (length(hit)) return(nm[hit[1]])
      hit <- which(grepl(target, nn, fixed = TRUE)); if (length(hit)) return(nm[hit[1]])
    }
    NULL
  }
  .autodetect_col <- function(key, df) {
    nm <- names(df); nn <- .norm(nm)
    if (key %in% names(syns)) {
      cand <- .norm(syns[[key]]); hit <- which(nn %in% cand)
      if (length(hit)) return(nm[hit[1]])
      hit <- which(vapply(nn, function(x) any(grepl(paste(cand, collapse="|"), x)), logical(1)))
      if (length(hit)) return(nm[hit[1]])
    }
    if (key == "hrci") {
      pattern <- "(hr|or|rr)?\\s*\\(?\\s*\\d*\\.?\\d+\\s*[,\\-–to~]\\s*\\d*\\.?\\d+"
      text_cols <- nm[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
      if (length(text_cols)) {
        score <- sapply(text_cols, function(j){
          x <- as.character(df[[j]])
          x <- gsub("（", "(", gsub("）", ")", x))
          x <- gsub("\\[", "(", gsub("\\]", ")", x))
          x <- gsub("–|—", "-", x)
          mean(grepl(pattern, tolower(x)))
        })
        if (any(score > 0.2, na.rm = TRUE)) return(names(sort(score, decreasing = TRUE))[1])
      }
    }
    NULL
  }
  .detect_measure <- function(x) { x <- tolower(paste(x, collapse = " ")); if (grepl("\\bor\\b", x)) return("OR"); if (grepl("\\brr\\b", x)) return("RR"); "HR" }
  .parse_hrci <- function(x) {
    x <- as.character(x)
    x <- gsub("（", "(", gsub("）", ")", x))
    x <- gsub("\\[", "(", gsub("\\]", ")", x))
    x <- gsub("–|—", "-", x)
    x <- gsub("\\s+to\\s+", "-", x, ignore.case = TRUE)
    re <- "([0-9]+\\.?[0-9]*)\\s*\\(([-+]?[0-9]*\\.?[0-9]+)\\s*[,\\-]\\s*([-+]?[0-9]*\\.?[0-9]+)\\)"
    m <- regmatches(x, regexec(re, x))
    pick <- function(i) vapply(m, function(v) if (length(v) >= i) v[[i]] else NA_character_, "")
    data.frame(
      HR   = suppressWarnings(as.numeric(pick(2))),
      Low  = suppressWarnings(as.numeric(pick(3))),
      High = suppressWarnings(as.numeric(pick(4))),
      stringsAsFactors = FALSE
    )
  }
  nm <- names(df); if (is.null(cols)) cols <- list()
  want_keys <- c("subgroup","level","events","total_py","hrci","phet")
  user_map <- setNames(rep(NA_character_, length(want_keys)), want_keys)
  for (k in want_keys) if (!is.null(cols[[k]])) user_map[k] <- .resolve_col(cols[[k]], nm)
  for (k in want_keys[is.na(user_map)]) user_map[k] <- .autodetect_col(k, df)
  required <- c("subgroup","level","events","total_py","hrci")
  missing_req <- required[is.na(user_map[required])]
  if (length(missing_req)) stop(
    paste0("Cannot locate required columns: ", paste(missing_req, collapse = ", "),
           "\nTips: pass `cols = list(key = 'your_col')` where key in {",
           paste(required, collapse = ", "), "}."), call. = FALSE)
  gcol <- user_map["subgroup"]; lcol <- user_map["level"];  ecol <- user_map["events"]
  tcol <- user_map["total_py"]; hcol <- user_map["hrci"];   pcol <- user_map["phet"]
  if (isTRUE(verbose)) { cat("Resolved column mapping:\n"); print(as.list(c(subgroup=gcol, level=lcol, events=ecol, total_py=tcol, hrci=hcol, phet=pcol))) }
  hrci_txt <- .to_chr(df[[hcol]]); parsed <- .parse_hrci(hrci_txt)
  df$HR <- parsed$HR; df$Low <- parsed$Low; df$High <- parsed$High
  df[[gcol]] <- .to_chr(df[[gcol]]); df[[lcol]] <- .to_chr(df[[lcol]])
  df[[ecol]] <- .to_chr(df[[ecol]]); df[[tcol]] <- .to_chr(df[[tcol]]); df[[hcol]] <- .to_chr(df[[hcol]])
  if (!is.na(pcol)) df[[pcol]] <- .to_chr(df[[pcol]])
  header_rows_mask <- (.nzchar(df[[gcol]])) & (!.nzchar(df[[lcol]]) | is.na(df[[lcol]]))
  header_row_style <- any(header_rows_mask, na.rm = TRUE)
  if (isTRUE(verbose)) message("Header-row style detected: ", header_row_style)
  if (header_row_style) {
    d_exp <- data.frame(
      Subgroup = ifelse(.nzchar(df[[gcol]]), df[[gcol]], ""),
      Level    = ifelse(is.na(df[[lcol]]) | !.nzchar(df[[lcol]]), "", df[[lcol]]),
      Events   = df[[ecol]],
      TotalPY  = df[[tcol]],
      HR_CI    = df[[hcol]],
      Phet     = if (!is.na(pcol)) df[[pcol]] else "",
      HR       = df$HR, Low = df$Low, High = df$High,
      stringsAsFactors = FALSE
    )
    if (isTRUE(show_p_in_header) && !is.na(pcol)) d_exp$Phet[!header_rows_mask] <- ""
  } else {
    grp_id <- cumsum(.nzchar(df[[gcol]]))
    split_list <- split(df, grp_id)
    build_block <- function(gdf){
      if (nrow(gdf) == 0) return(gdf[0,])
      if (.nzchar(gdf[[gcol]][1])) {
        title_row <- data.frame(
          Subgroup = gdf[[gcol]][1], Level = "", Events = "",
          TotalPY = "", HR_CI = "",
          Phet = if (!is.na(pcol)) .to_chr(gdf[[pcol]][1]) else "",
          HR = NA_real_, Low = NA_real_, High = NA_real_,
          stringsAsFactors = FALSE
        )
        gdf[[gcol]] <- ""
        body_rows <- data.frame(
          Subgroup = gdf[[gcol]], Level = gdf[[lcol]],
          Events = gdf[[ecol]],  TotalPY = gdf[[tcol]],
          HR_CI = gdf[[hcol]],
          Phet = if (!is.na(pcol)) { if (show_p_in_header) "" else .to_chr(gdf[[pcol]]) } else "",
          HR = gdf$HR, Low = gdf$Low, High = gdf$High,
          stringsAsFactors = FALSE
        )
        out <- rbind(title_row, body_rows); rownames(out) <- NULL; out
      } else {
        body_rows <- data.frame(
          Subgroup = gdf[[gcol]], Level = gdf[[lcol]],
          Events = gdf[[ecol]],  TotalPY = gdf[[tcol]],
          HR_CI = gdf[[hcol]],
          Phet = if (!is.na(pcol)) .to_chr(gdf[[pcol]]) else "",
          HR = gdf$HR, Low = gdf$Low, High = gdf$High,
          stringsAsFactors = FALSE
        ); rownames(body_rows) <- NULL; body_rows
      }
    }
    d_exp <- do.call(rbind, lapply(split_list, build_block))
  }
  has_p <- !all(is.na(d_exp$Phet)) && any(nchar(d_exp$Phet) > 0, na.rm = TRUE)
  if (length(headers) < 5L) stop("`headers` must have at least 5 entries.")
  if (has_p && length(headers) < 6L) headers <- c(headers[1:5], "P for heterogeneity")
  measure <- .detect_measure(df[[hcol]]); if (is.null(xlab)) xlab <- sprintf("Adjusted %s (95%% CI)", measure)
  label_matrix <- cbind(
    c(headers[1], d_exp$Subgroup),
    c(headers[2], d_exp$Level),
    c(headers[3], d_exp$Events),
    c(headers[4], d_exp$TotalPY),
    c(headers[5], d_exp$HR_CI)
  ); if (has_p) label_matrix <- cbind(label_matrix, c(headers[6], d_exp$Phet))
  mean_hr  <- c(NA_real_, d_exp$HR); lower_hr <- c(NA_real_, d_exp$Low); upper_hr <- c(NA_real_, d_exp$High)
  is_summary <- c(TRUE, d_exp$Level == ""); line_end <- as.character(nrow(d_exp) + 1L)
  if (isTRUE(verbose)) message("Rows(label)=", nrow(label_matrix),
                               " | mean=", length(mean_hr),
                               " | finite HR/OR/RR present = ", any(is.finite(mean_hr), na.rm = TRUE))
  if (!any(is.finite(mean_hr), na.rm = TRUE)) stop("All effect values are NA; please check the effect-size column (HR/OR/RR with 95% CI).")
  fpcols_try <- function(cols){
    attempt <- try(do.call(forestplot::fpColors, cols), silent = TRUE)
    if (!inherits(attempt, "try-error")) return(attempt)
    keep <- cols[intersect(names(cols), c("box","lines","summary","zero"))]
    do.call(forestplot::fpColors, keep)
  }
  fp_col <- fpcols_try(colors)
  default_lines <- stats::setNames(list(grid::gpar(lty = 1), grid::gpar(lty = 1)), c("1", line_end))
  fp_obj <- forestplot::forestplot(
    labeltext = label_matrix,
    mean = mean_hr, lower = lower_hr, upper = upper_hr,
    col = fp_col, xlab = xlab,
    is.summary = is_summary,
    zero = 1, graph.pos = 5,
    boxsize = 0.3,
    lineheight = grid::unit(6, "mm"),
    colgap = grid::unit(6, "mm"),
    lwd.ci = 1.5,
    graphwidth = grid::unit(0.25, "npc"),
    hrzl_lines = default_lines,
    ...
  )
  if (isTRUE(draw)) {
    plot(fp_obj)
    if (isTRUE(left_right_border)) {
      grid::grid.lines(x = c(0.01, 0.01), y = c(0, 1), gp = grid::gpar(col = border_col, lwd = 1))
      grid::grid.lines(x = c(0.99, 0.99), y = c(0, 1), gp = grid::gpar(col = border_col, lwd = 1))
    }
  }
  invisible(fp_obj)
}

#' Save a forest plot to file (pdf/png)
#'
#' Convenience wrapper to export a forest plot produced by `z_forestplot()`.
#'
#' @param filename output path; extension decides device ("pdf" or "png").
#' @param data Either a data.frame used by `z_forestplot()`, or NULL if using `plot=`.
#'             For backward-compatibility, if you pass a forestplot object as the
#'             2nd unnamed argument, it will be treated as `plot`.
#' @param width,height numeric; in inches.
#' @param res integer; dpi for PNG (ignored for PDF). Default 300.
#' @param bg background color. Default "white".
#' @param plot Optional. A forestplot object returned by `z_forestplot(...)`.
#' @param ... further arguments passed to `z_forestplot()` when `data` is provided.
#'
#' @usage
#' # 1) 直接给数据表：函数内部会调用 z_forestplot() 再保存
#' z_forestplot_save("forest.pdf", data, width = 8, height = 6)
#'
#' # 2) 先画图，再保存（支持位置参数或命名参数）
#' p <- z_forestplot(data, draw = TRUE)
#' z_forestplot_save("forest.pdf", plot = p)
#' z_forestplot_save("forest.pdf", p)  # 向下兼容：第二个无名参数识别为 plot
#'
#' # 3) 保存 PNG
#' z_forestplot_save("forest.png", data, width = 8, height = 6, res = 300)
#'
#' @return Invisibly returns the forestplot object.
#' @importFrom grDevices pdf png dev.off
#' @export
z_forestplot_save <- function(filename, data = NULL, width = 8, height = 6,
                              res = 300, bg = "white", plot = NULL, ...) {
  stopifnot(is.character(filename), length(filename) == 1L)
  ext <- tolower(tools::file_ext(filename))
  if (is.null(plot) && !is.null(data) && !is.data.frame(data)) {
    if (inherits(data, c("forestplot", "grob", "gTree"))) { plot <- data; data <- NULL }
    else stop("`data` 需要是 data.frame；若你已生成图对象，请使用 `plot = p` 传入。", call. = FALSE)
  }
  if (ext == "pdf") {
    grDevices::pdf(filename, width = width, height = height, bg = bg); on.exit(grDevices::dev.off(), add = TRUE)
  } else if (ext == "png") {
    grDevices::png(filename, width = width * res, height = height * res, res = res, units = "px", bg = bg)
    on.exit(grDevices::dev.off(), add = TRUE)
  } else stop("Unsupported extension: ", ext, ". Use pdf or png.")
  if (!is.null(plot)) { plot(plot); return(invisible(plot)) }
  stopifnot(is.data.frame(data))
  fp <- z_forestplot(data = data, draw = TRUE, ...); invisible(fp)
}

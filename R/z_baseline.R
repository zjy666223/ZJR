#' 基线表（单一数据框 + 汇总列）
#'
#' 从一个已合并的数据框 `data` 生成基线特征表：自动识别**连续/分类**变量；
#' 连续变量显示 **Median (Q1, Q3)**；分类变量显示 **n (%)**；
#' 输出包含 **Total population** 汇总列与每个分组列，并可导出 CSV。
#'
#' @section 用法:
#' z_baseline_df(
#'   data,
#'   baseline_define,
#'   group_names,
#'   output_name = "基线表.csv",
#'   max_cat_levels = 10,
#'   digits = 2,
#'   percent_digits = 2,
#'   count_digits = 2,
#'   include_missing = FALSE,
#'   csv_encoding = "UTF-8"
#' )
#'
#' @param data `data.frame`，已包含所有待统计变量与分组列（如 `d`）。
#' @param baseline_define `character`，要进入基线表的变量名向量（按此顺序输出）。
#' @param group_names `character(1)`，分组列名（例："AR_status"）。
#' @param output_name `character`，导出文件名（CSV）。设为空串可仅返回数据框。
#' @param max_cat_levels `numeric`，数值型变量若**非缺失唯一值数** ≤ 该阈值，则按**分类**处理。
#' @param digits 连续变量数值小数位。
#' @param percent_digits 百分比小数位。
#' @param count_digits 计数保留小数位（为与示例一致，默认 2）。
#' @param include_missing `logical`，分类变量是否额外加入“Missing”一行（百分比置空）。
#' @param csv_encoding CSV 文件编码。
#'
#' @return `data.frame`：格式化后的基线表；若 `output_name` 非空，同时写出 CSV。
#'
#' @details
#' - 汇总列标题：`"Total population\nN = <总样本>¹"`；各组：`"<组别>\nN = <组内样本>¹"`。
#' - 分类变量的百分比分母为**该变量在该组的非缺失数**。
#' - 连续变量分位数使用 `quantile(..., type = 2)`。
#'
#' @examples
#' # 示例：直接调用生成“基线表.csv”
#' tbl <- z_baseline_df(
#'   data = d,
#'   baseline_define = c(
#'     "age","gender","ethnicity","BMI","education","smoking_status",
#'     "drinking_status","physical_activity","score_diet","household_income",
#'     "discoast","tdi","hypertension","diabetes","asthma","sleep_cat","twa_benzene"
#'   ),
#'   group_names = "AR_status",
#'   output_name = "基线表.csv"
#' )
#'
#' @export
z_baseline_df <- function(data,
                          baseline_define,
                          group_names,
                          output_name = "基线表.csv",
                          max_cat_levels = 10,
                          digits = 2,
                          percent_digits = 2,
                          count_digits = 2,
                          include_missing = FALSE,
                          csv_encoding = "UTF-8") {
  
  if (!group_names %in% names(data)) stop(sprintf("group_names '%s' not in data.", group_names))
  miss <- setdiff(baseline_define, names(data))
  if (length(miss)) warning("Variables not found and skipped: ", paste(miss, collapse = ", "))
  baseline_define <- intersect(baseline_define, names(data))
  if (!length(baseline_define)) stop("No valid variables in baseline_define.")
  
  # 保留出现顺序
  grp_raw <- data[[group_names]]
  grp_levels <- if (is.factor(grp_raw)) levels(grp_raw) else unique(grp_raw)
  data[[group_names]] <- factor(grp_raw, levels = grp_levels)
  
  # 表头
  fmt_int <- function(x) ifelse(is.na(x), "", formatC(as.numeric(x), big.mark = ",", format = "f", digits = 0))
  N_total <- nrow(data)
  group_counts <- table(data[[group_names]], useNA = "no")
  overall_label <- paste0("Total population\nN = ", fmt_int(N_total), "\u00B9")
  group_labels <- vapply(levels(data[[group_names]]),
                         function(g) paste0(g, "\nN = ", fmt_int(group_counts[[g]]), "\u00B9"),
                         character(1))
  all_labels <- c(overall_label, group_labels)
  
  # 辅助
  is_cont <- function(x) {
    if (inherits(x, c("Date", "POSIXct", "POSIXt"))) return(TRUE)
    if (is.numeric(x)) return(length(unique(stats::na.omit(x))) > max_cat_levels)
    FALSE
  }
  fmt_num <- function(x, k = digits) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = k, big.mark = ","))
  fmt_cnt <- function(x, k = count_digits) ifelse(is.na(x), "", formatC(as.numeric(x), format = "f", digits = k, big.mark = ","))
  fmt_pct <- function(p, k = percent_digits) ifelse(is.na(p), "", paste0(formatC(100 * as.numeric(p), format = "f", digits = k), "%"))
  qfun <- function(v, probs) stats::quantile(v, probs = probs, na.rm = TRUE, type = 2)
  
  build_cont_block <- function(var) {
    x <- data[[var]]
    if (all(is.na(x))) return(NULL)
    
    overall <- if (all(is.na(x))) "" else {
      qs <- qfun(x, c(0.25, 0.5, 0.75))
      paste0(fmt_num(qs[2]), " (", fmt_num(qs[1]), ", ", fmt_num(qs[3]), ")")
    }
    per_g <- sapply(levels(data[[group_names]]), function(g) {
      v <- x[data[[group_names]] == g]
      if (all(is.na(v))) "" else {
        qs <- qfun(v, c(0.25, 0.5, 0.75))
        paste0(fmt_num(qs[2]), " (", fmt_num(qs[1]), ", ", fmt_num(qs[3]), ")")
      }
    }, USE.NAMES = FALSE)
    
    var_row <- as.list(setNames(rep("", length(all_labels)), all_labels))
    med_row <- as.list(c(overall, per_g)); names(med_row) <- all_labels
    
    out <- rbind(
      data.frame(Characteristic = var, var_row, check.names = FALSE),
      data.frame(Characteristic = "  Median (Q1, Q3)", med_row, check.names = FALSE)
    )
    rownames(out) <- NULL
    out
  }
  
  build_cat_block <- function(var) {
    x_all <- data[[var]]
    lv <- if (is.factor(x_all)) levels(x_all) else unique(stats::na.omit(x_all))
    
    den_total <- sum(!is.na(x_all))
    den_group <- tapply(!is.na(x_all), data[[group_names]], sum)
    
    lv_use <- lv
    if (include_missing) lv_use <- c(lv_use, "Missing")
    
    make_cell <- function(n, d, is_missing = FALSE) {
      # why: 缺失行不给百分比，避免误读
      if (is_missing) return(paste0(fmt_cnt(n), " ()"))
      paste0(fmt_cnt(n), " (", fmt_pct(ifelse(d > 0, n/d, NA_real_)), ")")
    }
    
    make_row <- function(level_value) {
      if (!identical(level_value, "Missing")) {
        n_total <- sum(!is.na(x_all) & x_all == level_value)
        total_cell <- make_cell(n_total, den_total, FALSE)
      } else {
        n_total <- sum(is.na(x_all))
        total_cell <- make_cell(n_total, NA_real_, TRUE)
      }
      
      per_g_cells <- sapply(levels(data[[group_names]]), function(g) {
        idx_g <- data[[group_names]] == g
        if (!identical(level_value, "Missing")) {
          n <- sum(idx_g & !is.na(x_all) & x_all == level_value)
          make_cell(n, den_group[[g]], FALSE)
        } else {
          n <- sum(idx_g & is.na(x_all))
          make_cell(n, NA_real_, TRUE)
        }
      }, USE.NAMES = FALSE)
      
      data.frame(
        Characteristic = as.character(level_value),
        as.list(stats::setNames(as.list(c(total_cell, per_g_cells)), all_labels)),
        check.names = FALSE
      )
    }
    
    var_row <- data.frame(Characteristic = var,
                          as.list(setNames(rep("", length(all_labels)), all_labels)),
                          check.names = FALSE)
    
    rows <- lapply(lv_use, make_row)
    out <- do.call(rbind, c(list(var_row), rows))
    rownames(out) <- NULL
    out
  }
  
  res_blocks <- list()
  for (v in baseline_define) {
    x <- data[[v]]
    if (all(is.na(x))) next
    blk <- if (is_cont(x)) build_cont_block(v) else build_cat_block(v)
    if (!is.null(blk)) res_blocks[[length(res_blocks) + 1]] <- blk
  }
  if (!length(res_blocks)) stop("No usable variables to summarize.")
  
  res <- do.call(rbind, res_blocks)
  names(res) <- c("Characteristic", all_labels)
  
  res <- rbind(
    res,
    data.frame(Characteristic = "\u00B9n (%)",
               as.list(setNames(rep("", length(all_labels)), all_labels)),
               check.names = FALSE)
  )
  rownames(res) <- NULL
  
  if (!is.null(output_name) && nzchar(output_name)) {
    utils::write.csv(res, file = output_name, row.names = FALSE, fileEncoding = csv_encoding, na = "")
    message(sprintf("Baseline table written to '%s' (encoding: %s).", output_name, csv_encoding))
  }
  res
}



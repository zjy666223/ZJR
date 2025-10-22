# file: R/auto_dummy.R
#' One-click dummy encoding for categorical variables
#'
#' @description
#' 对数据框中的分类变量进行一键哑变量编码（0/1）。支持：
#' - 选择/排除列；
#' - 控制是否丢弃首个水平（防止完全共线性）；
#' - 可为 NA 生成单独的指示列；
#' - 控制是否保留原始列；
#' - 限制高基数变量（按最大水平数或仅取 Top-N 水平）；
#' - 可选输出稀疏矩阵列（需要 `Matrix` 包）。
#'
#' @param df data.frame/tibble。
#' @param cols 字符向量：需要编码的列名；默认 NULL 表示自动检测分类变量（factor/character/logical）。
#' @param exclude 字符向量：需要从编码列表中排除的列名，默认 NULL。
#' @param drop_first 逻辑值：是否丢弃每个变量的首个水平以避免完全共线性，默认 TRUE。
#' @param include_na 逻辑值：是否为缺失值生成单独的 NA 指示列，默认 FALSE。
#' @param keep_original 逻辑值：是否保留原始分类列，默认 FALSE（用哑变量替换）。
#' @param max_levels 整数或 NULL：若分类水平数大于该值则报错或跳过（见 `on_high_cardinality`）。默认 NULL（不限制）。
#' @param top_n_levels 整数或 NULL：仅对前 `top_n_levels` 个频数最高的水平生成列，其余合并为 `other`（若 `drop_first=TRUE` 会考虑 `other` 在内）。默认 NULL（不裁剪）。
#' @param on_high_cardinality 字符串：对于超过 `max_levels` 的列如何处理，取值 `"error"`、`"skip"`、`"warn-skip"`。默认 `"warn-skip"`。
#' @param sep 分隔符，用于生成列名，默认 "_"。
#' @param drop_level_name 自定义被丢弃的参考水平名称标注，仅体现在返回的 `attr(out, "dummy_info")` 元信息中，默认 "(ref)"。
#' @param sparse 逻辑值：是否以稀疏列形式返回（需要 `Matrix` 包；返回 data.frame 中含 `dgCMatrix` 列），默认 FALSE。
#' @param treat_logical_as_factor 逻辑值：是否将逻辑列当作两水平分类处理，默认 TRUE。
#'
#' @return 一个 data.frame，与输入同类；并附加属性 `dummy_info`（list），包含每个变量的水平、参考水平、生成列名等信息。
#' @export
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   id = 1:6,
#'   sex = sample(c("M","F"), 6, TRUE),
#'   smoke = factor(sample(c("Never","Former","Current"), 6, TRUE),
#'                  levels = c("Never","Former","Current")),
#'   grp = sample(LETTERS[1:5], 6, TRUE),
#'   y = rnorm(6),
#'   stringsAsFactors = FALSE
#' )
#' # 基本用法：为自动识别的分类变量生成哑变量，保留参考水平
#' out <- auto_dummy(df, drop_first = TRUE, keep_original = FALSE)
#' attr(out, "dummy_info")
#'
#' # 仅对指定列编码，并为 NA 生成指示列
#' df$grp[1] <- NA
#' out2 <- auto_dummy(df, cols = c("sex","grp"), include_na = TRUE, top_n_levels = 3)
#'
#' # 高基数列：跳过并给出警告
#' out3 <- auto_dummy(df, max_levels = 4, on_high_cardinality = "warn-skip")
auto_dummy <- function(df,
                       cols = NULL,
                       exclude = NULL,
                       drop_first = TRUE,
                       include_na = FALSE,
                       keep_original = FALSE,
                       max_levels = NULL,
                       top_n_levels = NULL,
                       on_high_cardinality = c("warn-skip","skip","error"),
                       sep = "_",
                       drop_level_name = "(ref)",
                       sparse = FALSE,
                       treat_logical_as_factor = TRUE) {
  if (!inherits(df, c("data.frame", "tbl_df"))) {
    stop("`df` must be a data.frame or tibble; got ", paste(class(df), collapse = ", "))
  }
  on_high_cardinality <- match.arg(on_high_cardinality)
  
  # 选择待编码列
  all_cols <- names(df)
  if (is.null(cols)) {
    is_cat_col <- vapply(df, function(v) {
      is.factor(v) || is.character(v) || (treat_logical_as_factor && is.logical(v))
    }, logical(1))
    cols <- all_cols[is_cat_col]
  }
  if (!is.null(exclude)) cols <- setdiff(cols, exclude)
  cols <- intersect(cols, all_cols)
  if (length(cols) == 0) {
    warning("No categorical columns selected for dummy encoding.")
    return(df)
  }
  
  # 稀疏矩阵支持
  if (isTRUE(sparse)) {
    if (!requireNamespace("Matrix", quietly = TRUE)) {
      stop("sparse=TRUE requires the 'Matrix' package.")
    }
  }
  
  # 工具：生成安全列名
  make_colnames <- function(var, lvl) {
    nm <- paste(var, lvl, sep = sep)
    nm <- gsub("[\n\r\t]", " ", nm)
    nm <- make.names(nm, unique = FALSE)
    nm
  }
  
  out <- df
  info <- list()
  
  for (col in cols) {
    v <- df[[col]]
    
    # 统一转为 factor 以获得稳定水平排序
    if (is.logical(v) && treat_logical_as_factor) {
      v <- factor(v, levels = c(FALSE, TRUE))
    } else if (is.character(v)) {
      v <- factor(v)
    } else if (is.factor(v)) {
      v <- v
    } else {
      # 非分类，跳过
      next
    }
    
    # 处理 NA 水平
    if (isTRUE(include_na)) {
      if (!any(is.na(levels(v)) || anyNA(v))) {
        # nothing
      }
      v <- addNA(v, ifany = TRUE)
      levels(v)[is.na(levels(v))] <- "NA"
    }
    
    # 高基数处理
    lvls <- levels(v)
    lvl_counts <- table(v, useNA = "no")
    # top_n 裁剪（发生在 max_levels 之前，以便减少基数）
    if (!is.null(top_n_levels) && is.finite(top_n_levels)) {
      ord <- sort(lvl_counts, decreasing = TRUE)
      keep_lvls <- names(ord)[seq_len(min(length(ord), as.integer(top_n_levels)))]
      v <- factor(ifelse(as.character(v) %in% keep_lvls, as.character(v), "other"),
                  levels = c(sort(keep_lvls), "other"))
      lvls <- levels(v)
      lvl_counts <- table(v, useNA = "no")
    }
    
    if (!is.null(max_levels) && length(lvls) > max_levels) {
      msg <- sprintf("Column '%s' has %d levels (> max_levels = %d).", col, length(lvls), max_levels)
      if (on_high_cardinality == "error") stop(msg)
      if (on_high_cardinality == "warn-skip") warning(msg, " Skipped.")
      if (on_high_cardinality %in% c("skip","warn-skip")) next
    }
    
    # 确定参考水平（首个）并生成列
    ref_level <- if (length(lvls) > 0) lvls[1] else NULL
    gen_levels <- lvls
    if (isTRUE(drop_first) && length(gen_levels) > 0) gen_levels <- gen_levels[-1]
    
    if (length(gen_levels) == 0) {
      info[[col]] <- list(levels = lvls, ref = ref_level, columns = character(0))
      next
    }
    
    # 构造 dummy 列
    dummy_mat <- sapply(gen_levels, function(lvl) as.integer(v == lvl))
    colnames(dummy_mat) <- make_colnames(col, gen_levels)
    
    # 稀疏化（可选）
    if (isTRUE(sparse)) {
      dummy_mat <- Matrix::Matrix(dummy_mat, sparse = TRUE)
    } else {
      dummy_mat <- as.data.frame(dummy_mat, stringsAsFactors = FALSE)
    }
    
    # 合并结果
    if (isTRUE(keep_original)) {
      out <- cbind(out, dummy_mat, stringsAsFactors = FALSE)
    } else {
      # 替换原列：将 out 去掉该列，再 cbind 哑变量
      keep_idx <- setdiff(names(out), col)
      out <- cbind(out[keep_idx], dummy_mat, stringsAsFactors = FALSE)
    }
    
    # 记录信息
    info[[col]] <- list(
      levels = lvls,
      counts = as.integer(lvl_counts[lvls]),
      ref = ref_level,
      ref_label = drop_level_name,
      columns = colnames(dummy_mat)
    )
  }
  
  # 确保列名唯一
  names(out) <- make.unique(names(out), sep = "_")
  attr(out, "dummy_info") <- info
  out
}

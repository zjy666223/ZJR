#' Merge multiple data frames by key columns
#'
#' @description
#' 通用合并函数：一次性按指定键（如 `eid`）把多个数据框合成一个。
#' 默认不依赖外部包，基于基 R `merge()` 与 `Reduce()` 实现。
#'
#' @param ... 一个或多个数据框（可与 `dfs` 同时使用）。
#' @param dfs 可选，数据框列表（当你已有 `list(df1, df2, ...)` 时更方便）。
#' @param by 键列名：字符向量（所有数据框同名键），或**列表**（长度与数据框数相同），
#'   其中第一个元素作为“规范键名”，其余数据框会在合并前把对应键重命名为该规范键名。
#'   例如：`by = list(c("eid","visit"), c("id","visit"), c("eid","visit"))`。
#' @param type 合并方式，`"inner"`（交集），`"left"`（保留第一个数据框全部行），`"full"`（并集）。
#' @param conflict 键外同名列的冲突处理：`"suffix"`（默认，保留并自动加后缀）、
#'   `"first"`（保留已存在列，丢弃后续数据框的同名列）、`"error"`（发现冲突即报错）。
#' @param suffixes 字符向量长度 2，成对用于一次合并的后缀（当 `conflict="suffix"` 时生效）。
#' @param sort 传递给 `merge()` 的 `sort`，默认 `FALSE` 保持输入顺序尽量稳定。
#' @return 合并后的数据框。
#' @examples
#' # 假设 df 与 df1 都有 eid：
#' # d <- z_merge(df, df1, by = "eid", type = "left")
#'
#' # 键名不同：df1 用 id 作为键，对齐到 df 的 eid
#' # d <- z_merge(df, df1, by = list("eid", "id"), type = "inner")
#'
#' # 三表、两键合并，设定规范键来自第一个数据框
#' # d <- z_merge(dfs = list(a, b, c),
#' #              by = list(c("eid","visit"), c("id","visit"), c("eid","visit")),
#' #              type = "full")
#' @export
z_merge <- function(...,
                    dfs = NULL,
                    by,
                    type = c("inner","left","full"),
                    conflict = c("suffix","first","error"),
                    suffixes = c(".x", ".y"),
                    sort = FALSE) {
  # collect inputs -------------------------------------------------------------
  items <- list(...)
  if (!is.null(dfs)) {
    if (is.list(dfs) && !is.data.frame(dfs)) items <- c(items, dfs) else items <- c(items, list(dfs))
  }
  if (length(items) < 2L) stop("Provide at least two data frames via ... or dfs.")
  is_df <- vapply(items, function(x) inherits(x, c("data.frame","tbl_df","tbl")), logical(1))
  if (any(!is_df)) stop("All inputs must be data frames.")
  items <- lapply(items, function(x) if (inherits(x, c("tbl_df","tbl"))) as.data.frame(x) else x)
  
  # keys normalization ---------------------------------------------------------
  if (is.character(by)) {
    key_can <- as.character(by)
    key_map <- rep(list(key_can), length(items))
  } else if (is.list(by) && length(by) == length(items)) {
    key_map <- lapply(by, as.character)
    key_can <- key_map[[1L]]
    if (!all(vapply(key_map, length, integer(1)) == length(key_can)))
      stop("All key specs in 'by' must have equal length.")
  } else {
    stop("'by' must be a character vector used by all data frames, or a list of the same length as inputs.")
  }
  
  # rename per-df keys to canonical names (temporary) -------------------------
  rename_keys <- function(df, from, to) {
    if (length(from) != length(to)) stop("Key length mismatch.")
    if (!all(from %in% names(df))) stop(sprintf("Missing key(s): %s", paste(setdiff(from, names(df)), collapse=",")))
    if (identical(from, to)) return(df)
    # 防覆盖：两步改名
    tmp <- paste0(".__zmerge_tmp__", seq_along(from))
    names(df)[match(from, names(df))] <- tmp
    names(df)[match(tmp, names(df))]  <- to
    df
  }
  items2 <- Map(rename_keys, items, key_map, MoreArgs = list(to = key_can))
  
  # conflict policy ------------------------------------------------------------
  conflict <- match.arg(conflict)
  drop_conflicts <- function(x, y, keys) {
    dup <- intersect(setdiff(names(y), keys), setdiff(names(x), keys))
    if (!length(dup)) return(y)
    if (conflict == "first") {
      y[, setdiff(names(y), dup), drop = FALSE]
    } else if (conflict == "error") {
      stop(sprintf("Conflicting columns: %s", paste(dup, collapse=",")))
    } else {
      y
    }
  }
  
  # merge kernel ---------------------------------------------------------------
  type <- match.arg(type)
  flags <- switch(type,
                  inner = list(all.x = FALSE, all.y = FALSE),
                  left  = list(all.x = TRUE,  all.y = FALSE),
                  full  = list(all.x = TRUE,  all.y = TRUE))
  
  step_merge <- function(acc, y, step) {
    y2 <- drop_conflicts(acc, y, key_can)
    merge(acc, y2, by = key_can, all.x = flags$all.x, all.y = flags$all.y,
          suffixes = if (conflict == "suffix") paste0(suffixes, step) else suffixes,
          sort = sort)
  }
  
  out <- items2[[1L]]
  for (i in 2:length(items2)) out <- step_merge(out, items2[[i]], step = i-1L)
  out
}

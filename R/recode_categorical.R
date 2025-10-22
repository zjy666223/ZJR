
#' 将分类变量重编码为 0,1,...
#'
#' 对数据框中的单个分类变量执行有序数值化编码：按水平顺序映射到
#' `start, start+1, ...`（默认从 0 开始）。字符列先转为因子；因子列保留其
#' 原有水平顺序。NA 保持为 NA。
#'
#' @param data 数据框。
#' @param colname 列名（字符串）。
#' @param append 是否追加新列而不覆盖原列。若 `TRUE`，新列名为
#'   `paste0(colname, suffix)`；若 `FALSE` 则覆盖原列。默认 `TRUE`。
#' @param suffix 当 `append=TRUE` 时附加在新列名后的后缀，默认 `"_num"`。
#' @param start 编码起点，默认 0。
#' @param to_numeric 是否返回数值型（`TRUE`）或返回带数值标签的因子（`FALSE`）。
#'
#' @return 返回数据框；指定列已被编码（覆盖或新增）。
#' @examples
#' d <- data.frame(sex = c("male","female","male"), edu = c("low","high","low"))
#' d <- recode_categorical(d, "sex")
#' d <- recode_categorical(d, "edu", start = 0, append = TRUE, suffix = "_id")
#' @export
recode_categorical <- function(data, colname, append = TRUE, suffix = "_num",
                               start = 0, to_numeric = TRUE) {
  if (!colname %in% names(data)) {
    stop(sprintf("列 '%s' 不存在于数据框中", colname))
  }
  
  vec <- data[[colname]]
  
  # 统一为因子：字符按出现顺序设定水平；数值型不处理（直接返回）
  if (is.factor(vec)) {
    f <- vec
  } else if (is.character(vec)) {
    f <- factor(vec, levels = unique(vec))  # 保留出现顺序
  } else if (is.logical(vec)) {
    f <- factor(vec, levels = c(FALSE, TRUE))
  } else {
    # 非字符/因子：视为数值，直接返回（不改动）
    message(sprintf("列 '%s' 不是字符/因子/逻辑，不进行编码。", colname))
    return(data)
  }
  
  k <- nlevels(f)
  codes <- seq.int(from = start, length.out = k)
  names(codes) <- levels(f)
  
  # 映射，保留 NA
  idx <- as.integer(f)               # 1..k 或 NA
  mapped <- ifelse(is.na(idx), NA, codes[idx])
  
  # 输出映射信息
  msg_map <- paste(sprintf("%s -> %s", levels(f), codes), collapse = ", ")
  message(sprintf("列 '%s' 编码映射: %s", colname, msg_map))
  
  # 目标类型
  out_col <- if (isTRUE(to_numeric)) as.numeric(mapped) else factor(mapped)
  
  # 放回数据框
  if (isTRUE(append)) {
    newname <- paste0(colname, suffix)
    if (newname %in% names(data)) {
      warning(sprintf("新列名 '%s' 已存在，将被覆盖。", newname))
    }
    data[[newname]] <- out_col
  } else {
    data[[colname]] <- out_col
  }
  data
}
# 文件: D:/12自定义封装R包/ZJR/R/keep_only.R

#' 只保留指定对象，删除全局环境中的其他对象
#'
#' @description
#' 从全局环境中删除除指定对象外的所有对象。
#'
#' @param keep_names 字符向量，要保留的对象名。
#' @param envir 环境，默认 `.GlobalEnv`。
#'
#' @return 无返回值（执行环境修改）。
#' @examples
#' keep_only(c("d", "df"))
#' keep_only(c("d", "df"))
#' ls()  # 只剩 d 和 df
#' @export
keep_only <- function(keep_names, envir = .GlobalEnv) {
  stopifnot(is.character(keep_names))
  
  all_objs <- ls(envir = envir, all.names = TRUE)
  rm_list <- setdiff(all_objs, keep_names)
  
  if (length(rm_list) > 0) {
    rm(list = rm_list, envir = envir)
    message("已删除对象: ", paste(rm_list, collapse = ", "))
  } else {
    message("没有需要删除的对象。")
  }
}

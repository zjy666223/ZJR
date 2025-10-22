# file: maint_utils.R
# 说明：
# - 不使用 library()；统一用 :: 调用，避免构建期加载问题
# - 首次使用请调用 zjr_set_dev_path("D:/12自定义封装R包/ZJR")
# - 之后任意目录：library(ZJR); zjr_update()

#' 设置 ZJR 的开发路径（仅首次需要）
#'
#' @param path 字符串，本机 ZJR 包源码的根目录（包含 DESCRIPTION、R/ 等）
#' @return 规范化后的路径（同时写入 options("ZJR.dev_path")）
#' @examples
#' \dontrun{
#' zjr_set_dev_path("D:/12自定义封装R包/ZJR")
#' }
#' @export
zjr_set_dev_path <- function(path) {
  if (missing(path) || !nzchar(path)) stop("请提供有效的包源码路径 path。")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(file.path(path, "DESCRIPTION")) ||
      !dir.exists(file.path(path, "R"))) {
    stop("路径看起来不像是 R 包根目录：", path)
  }
  options(ZJR.dev_path = path)
  message("ZJR.dev_path 已设置为：", path)
  invisible(path)
}

# ---- 内部工具 ----
.zjr_silently <- function(expr) tryCatch(invisible(force(expr)), error = function(e) invisible(NULL))

# Windows 部分环境不支持 [:ascii:]，改用 [^\x01-\x7F]
.zjr_check_non_ascii_filenames <- function(r_dir) {
  files <- list.files(r_dir, full.names = FALSE)
  bad   <- files[grepl("[^\x01-\x7F]", files)]
  if (length(bad)) {
    stop("R/ 目录存在非英文文件名：\n  - ",
         paste(bad, collapse = "\n  - "),
         "\n请改为英文文件名后再执行 zjr_update()。")
  }
  TRUE
}

#' 一键更新本地 ZJR 包
#'
#' 过程：卸载旧包与锁 → 生成文档（NAMESPACE/Rd）→ 安装（不升级依赖）→ 重新加载 → 验证导出。
#'
#' @param upgrade_deps 逻辑值，是否升级依赖（默认 FALSE，避免交互与长时间下载）
#' @param check_non_ascii 逻辑值，是否检查 R/ 下非英文文件名（默认 TRUE）
#' @return TRUE（成功时）
#' @examples
#' \dontrun{
#' # 第一次：
#' zjr_set_dev_path("D:/12自定义封装R包/ZJR")
#' # 以后每次一键更新：
#' zjr_update()
#' }
#' @export
zjr_update <- function(upgrade_deps = FALSE, check_non_ascii = TRUE) {
  pkg_path <- getOption("ZJR.dev_path", NULL)
  if (is.null(pkg_path)) {
    stop("未设置 ZJR 的开发路径。请先运行：\n  zjr_set_dev_path(\"D:/12自定义封装R包/ZJR\")")
  }
  pkg_path <- normalizePath(pkg_path, winslash = "/", mustWork = TRUE)
  
  # 0) 预检查
  r_dir <- file.path(pkg_path, "R")
  if (!dir.exists(r_dir)) stop("未找到 R/ 目录：", r_dir)
  if (isTRUE(check_non_ascii)) .zjr_check_non_ascii_filenames(r_dir)
  
  # 1) 卸载 & 清理旧安装/锁
  if ("package:ZJR" %in% search()) .zjr_silently(detach("package:ZJR", unload = TRUE, character.only = TRUE))
  if ("ZJR" %in% loadedNamespaces()) .zjr_silently(unloadNamespace("ZJR"))
  lib1 <- .libPaths()[1]
  .zjr_silently(unlink(file.path(lib1, "ZJR"), recursive = TRUE, force = TRUE))
  .zjr_silently(unlink(file.path(lib1, "00LOCK-ZJR"), recursive = TRUE, force = TRUE))
  
  # 2) 生成文档
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("需要 devtools 包。请先安装：install.packages(\"devtools\")")
  }
  devtools::document(pkg_path)
  
  # 3) 安装（默认不升级依赖，避免交互）
  upgrade_arg <- if (isTRUE(upgrade_deps)) "default" else "never"
  devtools::install(pkg_path, upgrade = upgrade_arg, quiet = FALSE)
  
  # 4) 重新加载
  suppressPackageStartupMessages(requireNamespace("ZJR", quietly = TRUE))
  library("ZJR", character.only = TRUE)
  
  # 5) 验证导出（可按需修改）
  exports <- getNamespaceExports("ZJR")
  needed  <- c("remove_na_and_count", "add_ukb_regular_activity_and_income")
  missing <- setdiff(needed, exports)
  if (length(missing)) {
    stop("下列函数未导出：", paste(missing, collapse = ", "),
         "\n可能原因：函数上方缺少 @export，或 roxygen 注释未紧贴函数定义，或 document() 报错。")
  }
  
  message("✅ ZJR 一键更新完成。当前导出：", paste(exports, collapse = ", "))
  invisible(TRUE)
}

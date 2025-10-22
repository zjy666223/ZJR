# 文件: D:/12自定义封装R包/ZJR/R/zzz.R
.onAttach <- function(libname, pkgname) {
  created_date <- as.Date("2025-08-13")
  today <- Sys.Date()
  days_since <- as.integer(difftime(today, created_date, units = "days"))
  
  # 固定信息
  msg1 <- "志存高远，厚积薄发"
  msg2 <- sprintf("ZJR 已创建 %d 天", days_since)
  packageStartupMessage(msg1, "\n", msg2)
  
  # ---- 每月彩蛋（持久化到用户缓存）----
  current_stamp <- format(today, "%Y-%m")
  cache_dir <- tryCatch(tools::R_user_dir("ZJR", which = "cache"), error = function(e) NULL)
  cache_file <- if (!is.null(cache_dir)) file.path(cache_dir, "last_egg.txt") else NULL
  
  last_stamp <- NULL
  if (!is.null(cache_file)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (file.exists(cache_file)) {
      last_stamp <- tryCatch(readLines(cache_file, warn = FALSE, encoding = "UTF-8")[1], error = function(e) NULL)
    }
  }
  should_fire <- is.null(last_stamp) || !identical(last_stamp, current_stamp)
  
  if (isTRUE(should_fire)) {
    blessings <- c(
      "愿你的代码如清风，健壮而优雅。",
      "Bug 退散，灵感常在，数据向你敞开。",
      "以终为始，所愿皆成。",
      "心怀热爱，日拱一卒。",
      "模型如山，稳如泰山；结果如泉，清澈见底。",
      "愿效率与质量并行，洞见与韧性相伴。",
      "少走弯路，多收成果。",
      "愿你每一次运行皆有收获，每一次提交皆有进步。"
    )
    set.seed(as.integer(format(today, "%Y%m")))
    bless <- sample(blessings, 1)
    
    colorize <- function(x) {
      if (requireNamespace("crayon", quietly = TRUE)) {
        styles <- list(crayon::cyan, crayon::magenta, crayon::yellow, crayon::green, crayon::blue, crayon::silver)
        s <- styles[[ (as.integer(format(today, "%m")) %% length(styles)) + 1 ]]
        return(s(paste0("【ZJR 彩蛋】", x)))
      }
      paste0("[ZJR 彩蛋] ", x)
    }
    
    packageStartupMessage(colorize(bless))
    
    if (!is.null(cache_file)) {
      tryCatch(writeLines(current_stamp, cache_file, useBytes = TRUE), error = function(e) {})
    }
  }
}

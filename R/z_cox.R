#' z_cox: Build publication-ready Cox regression table (continuous + quantiles)
#'
#' @param exposure,outcome,covar data.frame/data.table（将被转为 data.frame）
#' @param model list，如 list(model_a, model_b)
#' @param id_col,event_col,time_col,expose_s,expose_iqr 见示例
#' @param expose_name 输出暴露基名；默认来自 expose_s 去后缀
#' @param entry_time_threshold 进入研究时间阈值；保留 (time > thr | event==1)
#' @param exclude_prevalent 是否剔除 (time==0 & event==1)
#' @param time_scale "years"/"days"
#' @param cut_on NULL/"per_model"/"overall"；为 NULL 时使用已有分组
#' @param n_q 组数（分位）
#' @param ref_level 参考水平；默认第一个水平
#' @param verbose 是否打印诊断信息（事件数等）
#' @return data.frame: expose, outcome, Case_N, Control_N, Person_years,
#'         model_1.HR, model_1.95._CI, model_1.P.value, class,
#'         model_2.HR, model_2.95._CI, model_2.P.value
#' @importFrom dplyr inner_join left_join group_by summarise mutate select filter all_of
#' @importFrom survival Surv coxph
#' @importFrom stats complete.cases relevel quantile
#'
#' @usage
#' # 最小可跑示例（请把对象名与列名替换成你自己的）：
#' # - exposure: 数据框，至少含 id 与连续暴露列 + 分位分组列
#' # - outcome : 数据框，至少含 id、事件列(event_col)、随访时间列(time_col)
#' # - covar   : 数据框，至少含 id 与协变量
#' # - model   : 协变量列表，如 list(c("age","sex"), c("age","sex","BMI"))
#' # - id_col  : id 列名
#' # - event_col/time_col/expose_s/expose_iqr : 分别替换为你数据里的列名
#' z_cox(
#'   exposure = exposure_data1,            # ← 替换为你的暴露数据框
#'   outcome  = outcome_data,              # ← 替换为你的结局数据框
#'   covar    = cov_data,                  # ← 替换为你的协变量数据框
#'   model    = model_t2,                  # ← 替换为你的协变量列表
#'   id_col   = "eid",                     # ← 替换为你的 ID 列名
#'   event_col= "K760_K758_status",        # ← 替换为你的事件列
#'   time_col = "K760_K758_time",          # ← 替换为你的时间列(年)
#'   expose_s = "twa_benzene_s",           # ← 连续暴露列
#'   expose_iqr = "twa_benzene_IQR",       # ← 分位(分组)列
#'   cut_on   = "per_model",               # 每个模型内按分位重切；已有分组用 NULL
#'   n_q      = 4,                         # 4 组（四分位）
#'   time_scale = "years",                 # 如果时间是“天”，改为 "days"
#'   verbose  = FALSE
#' )
#'
#' @export
z_cox <- function(
    exposure, outcome, covar, model,
    id_col = "eid",
    event_col, time_col,
    expose_s, expose_iqr,
    expose_name = NULL,
    entry_time_threshold = 0,
    exclude_prevalent = TRUE,
    time_scale = c("years","days"),
    cut_on = NULL,
    n_q = 4,
    ref_level = NULL,
    verbose = TRUE
) {
  time_scale <- match.arg(time_scale)
  
  # ---- helpers ----
  stop_if_missing <- function(df, cols) {
    miss <- setdiff(cols, names(df))
    if (length(miss)) stop("missing columns: ", paste(miss, collapse=", "))
  }
  as_years <- function(x) if (identical(time_scale, "days")) x/365.25 else x
  fmt_ci <- function(lo, hi) sprintf("%.4g, %.4g", lo, hi)
  
  # 转义正则元字符（用于匹配 coxph 系数行名）
  escape_rx <- function(s) gsub("([][{}()+*^$|\\.?\\\\])", "\\\\\\1", s)
  
  build_surv_formula <- function(tcol, ecol, terms) {
    stats::as.formula(sprintf("survival::Surv(%s, %s) ~ %s", tcol, ecol, paste(terms, collapse=" + ")))
  }
  
  # 更稳健地抽取 HR/CI/P（支持带 []() 的因子水平）
  extract_hr <- function(fit, var_name = NULL, level = NULL) {
    sm <- tryCatch(summary(fit), error = function(e) NULL)
    if (is.null(sm) || is.null(sm$coefficients)) return(list(HR=NA, CI=NA, P=NA))
    ct <- as.data.frame(sm$coefficients)
    ci <- as.data.frame(sm$conf.int)
    rn <- rownames(ct)
    
    idx <- NA_integer_
    if (is.null(level)) {
      # 连续项：精确匹配变量名
      idx <- match(var_name, rn)
    } else {
      # 因子水平：^var\Q(level)\E$（转义括号/方括号等）
      pat <- paste0("^", escape_rx(var_name), escape_rx(level), "$")
      hit <- grep(pat, rn)
      if (length(hit) == 1) idx <- hit
    }
    
    if (is.na(idx)) return(list(HR=NA, CI=NA, P=NA))
    list(
      HR = unname(ci[idx, "exp(coef)"]),
      CI = fmt_ci(unname(ci[idx, "lower .95"]), unname(ci[idx, "upper .95"])),
      P  = unname(ct[idx, "Pr(>|z|)"])
    )
  }
  
  complete_cases <- function(df, cols) df[stats::complete.cases(df[, cols, drop=FALSE]), , drop=FALSE]
  
  level_stats <- function(df, iqr_col, tcol, ecol) {
    df$..lev <- as.character(df[[iqr_col]])
    agg <- df |>
      dplyr::group_by(..lev, .drop = FALSE) |>
      dplyr::summarise(
        Case_N = sum(.data[[ecol]] == 1, na.rm = TRUE),
        Control_N = sum(.data[[ecol]] == 0, na.rm = TRUE),
        Person_years = sum(.data[[tcol]], na.rm = TRUE),
        .groups = "drop"
      )
    names(agg)[1] <- ".lev"
    agg
  }
  
  make_iqr <- function(x, n_q, breaks_fixed=NULL) {
    if (!is.null(breaks_fixed)) {
      cut(x, breaks=breaks_fixed, include.lowest=TRUE, right=TRUE)
    } else {
      cut(x,
          breaks=stats::quantile(x, probs=seq(0,1,1/n_q), na.rm=TRUE),
          include.lowest=TRUE, right=TRUE)
    }
  }
  
  # ---- 1) 合并 + 类型标准化 ----
  exposure <- as.data.frame(exposure)
  outcome  <- as.data.frame(outcome)
  covar    <- as.data.frame(covar)
  
  stop_if_missing(exposure, c(id_col, expose_s))
  stop_if_missing(outcome,  c(id_col, event_col, time_col))
  stop_if_missing(covar,    id_col)
  
  dat <- dplyr::inner_join(exposure, outcome, by = id_col) |>
    dplyr::inner_join(covar,    by = id_col)
  
  dat[[event_col]] <- as.integer(dat[[event_col]])
  dat[[time_col]]  <- as_years(as.numeric(dat[[time_col]]))
  dat[[expose_s]]  <- suppressWarnings(as.numeric(dat[[expose_s]]))
  
  if (!missing(expose_iqr) && expose_iqr %in% names(dat) && !is.factor(dat[[expose_iqr]])) {
    dat[[expose_iqr]] <- factor(dat[[expose_iqr]], exclude = NULL)
  }
  
  # ---- 2) 入排 ----
  if (!is.null(entry_time_threshold)) {
    dat <- dat[dat[[time_col]] > entry_time_threshold | dat[[event_col]] == 1, , drop=FALSE]
  }
  if (isTRUE(exclude_prevalent)) {
    dat <- dat[!(dat[[time_col]] == 0 & dat[[event_col]] == 1), , drop=FALSE]
  }
  
  # ---- 3) 诊断：全量事件数 ----
  if (verbose) {
    cat("[z_cox] after merge & filters: N=", nrow(dat),
        ", Cases=", sum(dat[[event_col]]==1, na.rm=TRUE), "\n", sep = "")
  }
  if (sum(dat[[event_col]]==1, na.rm=TRUE) == 0) {
    # why: 没有事件，任何 Cox 都无法估计；返回占位表
    expose_name <- if (is.null(expose_name)) sub("(_s|_IQR)$", "", expose_s) else expose_name
    out0 <- data.frame(
      expose       = c(paste0(expose_name, "_s"), "Ref"),
      outcome      = event_col,
      Case_N       = 0L,
      Control_N    = sum(dat[[event_col]]==0, na.rm=TRUE),
      Person_years = sum(dat[[time_col]], na.rm=TRUE),
      `model_1.HR`      = c(NA, 1),
      `model_1.95._CI`  = c("NA, NA", "1"),
      `model_1.P.value` = c(NA, 1),
      class        = c("continuous","4_class"),
      `model_2.HR`      = NA,
      `model_2.95._CI`  = NA,
      `model_2.P.value` = NA,
      check.names = FALSE
    )
    rownames(out0) <- NULL
    return(out0)
  }
  
  # ---- 4) overall 断点（如需）----
  cut_mode <- if (is.null(cut_on)) NULL else match.arg(cut_on, c("per_model","overall"))
  overall_breaks <- NULL
  if (!is.null(cut_mode) && identical(cut_mode, "overall")) {
    overall_breaks <- stats::quantile(dat[[expose_s]], probs=seq(0,1,1/n_q), na.rm=TRUE)
  }
  
  if (is.null(expose_name)) expose_name <- sub("(_s|_IQR)$", "", expose_s)
  
  # ---- 5) 连续暴露 ----
  cont_rows <- vector("list", length(model))
  for (i in seq_along(model)) {
    keep <- unique(c(expose_s, model[[i]], event_col, time_col))
    dfi  <- complete_cases(dat, keep)
    if (verbose) {
      cat(sprintf("[z_cox] model_%d complete-cases: N=%d, Cases=%d\n",
                  i, nrow(dfi), sum(dfi[[event_col]]==1, na.rm=TRUE)))
    }
    if (sum(dfi[[event_col]]==1, na.rm=TRUE) == 0) {
      cont_rows[[i]] <- list(N_case=0L, N_ctrl=sum(dfi[[event_col]]==0, na.rm=TRUE),
                             PY=sum(dfi[[time_col]], na.rm=TRUE),
                             HR=NA, CI="NA, NA", P=NA)
      next
    }
    fit <- tryCatch(
      survival::coxph(build_surv_formula(time_col, event_col, c(expose_s, model[[i]])),
                      data = dfi, ties = "efron"),
      error = function(e) NULL
    )
    hr <- if (is.null(fit)) list(HR=NA, CI="NA, NA", P=NA) else extract_hr(fit, var_name = expose_s)
    cont_rows[[i]] <- list(N_case=sum(dfi[[event_col]]==1),
                           N_ctrl=sum(dfi[[event_col]]==0),
                           PY=sum(dfi[[time_col]]),
                           HR=hr$HR, CI=hr$CI, P=hr$P)
  }
  
  cont_tab <- data.frame(
    expose        = paste0(expose_name, "_s"),
    outcome       = event_col,
    Case_N        = cont_rows[[1]]$N_case,
    Control_N     = cont_rows[[1]]$N_ctrl,
    Person_years  = cont_rows[[1]]$PY,
    `model_1.HR`        = cont_rows[[1]]$HR,
    `model_1.95._CI`    = cont_rows[[1]]$CI,
    `model_1.P.value`   = cont_rows[[1]]$P,
    class         = "continuous",
    `model_2.HR`        = if (length(cont_rows) >= 2) cont_rows[[2]]$HR else NA_real_,
    `model_2.95._CI`    = if (length(cont_rows) >= 2) cont_rows[[2]]$CI else NA_character_,
    `model_2.P.value`   = if (length(cont_rows) >= 2) cont_rows[[2]]$P  else NA_real_,
    check.names = FALSE
  )
  
  # ---- 6) 分组暴露 ----
  make_cat_block <- function(covset) {
    keep <- unique(c(expose_s, expose_iqr, covset, event_col, time_col))
    dfi  <- complete_cases(dat, keep)
    
    # 重新分组（如需）
    if (!is.null(cut_mode)) {
      if (identical(cut_mode, "per_model")) {
        dfi[[expose_iqr]] <- make_iqr(dfi[[expose_s]], n_q = n_q)
      } else if (identical(cut_mode, "overall")) {
        dfi[[expose_iqr]] <- make_iqr(dfi[[expose_s]], breaks_fixed = overall_breaks)
      }
    }
    dfi[[expose_iqr]] <- factor(dfi[[expose_iqr]], exclude = NULL)
    
    # 参考组
    ref <- if (is.null(ref_level)) levels(dfi[[expose_iqr]])[1] else ref_level
    dfi[[expose_iqr]] <- stats::relevel(dfi[[expose_iqr]], ref = ref)
    
    stat <- level_stats(dfi, expose_iqr, time_col, event_col)
    
    if (sum(dfi[[event_col]]==1, na.rm=TRUE) == 0) {
      # 没有事件：仅输出计数，HR 置 NA，Ref=1
      rows <- list(
        data.frame(expose="Ref", outcome=event_col,
                   Case_N=stat$Case_N[match(ref, stat$.lev)],
                   Control_N=stat$Control_N[match(ref, stat$.lev)],
                   Person_years=stat$Person_years[match(ref, stat$.lev)],
                   `model.HR`=1, `model.95._CI`="1", `model.P.value`=1,
                   class="4_class", check.names = FALSE)
      )
      for (lev in setdiff(levels(dfi[[expose_iqr]]), ref)) {
        rows[[length(rows)+1]] <- data.frame(
          expose = paste0(expose_iqr, lev), outcome = event_col,
          Case_N=stat$Case_N[match(lev, stat$.lev)],
          Control_N=stat$Control_N[match(lev, stat$.lev)],
          Person_years=stat$Person_years[match(lev, stat$.lev)],
          `model.HR`=NA, `model.95._CI`="NA, NA", `model.P.value`=NA,
          class="4_class", check.names = FALSE
        )
      }
      return(do.call(rbind, rows))
    }
    
    fit  <- tryCatch(
      survival::coxph(build_surv_formula(time_col, event_col, c(expose_iqr, covset)),
                      data = dfi, ties = "efron"),
      error = function(e) NULL
    )
    
    rows <- list(
      data.frame(expose="Ref", outcome=event_col,
                 Case_N=stat$Case_N[match(ref, stat$.lev)],
                 Control_N=stat$Control_N[match(ref, stat$.lev)],
                 Person_years=stat$Person_years[match(ref, stat$.lev)],
                 `model.HR`=1, `model.95._CI`="1", `model.P.value`=1,
                 class="4_class", check.names = FALSE)
    )
    for (lev in setdiff(levels(dfi[[expose_iqr]]), ref)) {
      hr <- if (is.null(fit)) list(HR=NA, CI="NA, NA", P=NA)
      else extract_hr(fit, var_name = expose_iqr, level = as.character(lev))
      rows[[length(rows)+1]] <- data.frame(
        expose = paste0(expose_iqr, as.character(lev)), outcome = event_col,
        Case_N=stat$Case_N[match(lev, stat$.lev)],
        Control_N=stat$Control_N[match(lev, stat$.lev)],
        Person_years=stat$Person_years[match(lev, stat$.lev)],
        `model.HR`=hr$HR, `model.95._CI`=hr$CI, `model.P.value`=hr$P,
        class="4_class", check.names = FALSE
      )
    }
    do.call(rbind, rows)
  }
  
  cat_m1 <- make_cat_block(model[[1]])
  names(cat_m1)[6:8] <- c("model_1.HR","model_1.95._CI","model_1.P.value")
  
  if (length(model) >= 2) {
    cat_m2 <- make_cat_block(model[[2]])
    names(cat_m2)[6:8] <- c("model_2.HR","model_2.95._CI","model_2.P.value")
    cat_tab <- dplyr::left_join(
      cat_m1,
      cat_m2[, c("expose","model_2.HR","model_2.95._CI","model_2.P.value")],
      by = "expose"
    )
  } else {
    cat_tab <- cat_m1
    cat_tab$`model_2.HR`      <- NA_real_
    cat_tab$`model_2.95._CI`  <- NA_character_
    cat_tab$`model_2.P.value` <- NA_real_
  }
  
  out <- rbind(cont_tab, cat_tab)
  rownames(out) <- NULL
  out
}

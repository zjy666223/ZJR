# file: R/z_subgroup.R
#' z_subgroup: Cox subgroup analysis table with interaction P (heterogeneity)
#'
#' @description
#' Build a publication-ready **subgroup table** for Cox proportional hazards models.
#' It merges exposure/outcome/covariate datasets by ID, fits two prespecified
#' adjustment models (model 1 & model 2) **within each level of each subgroup**
#' and reports HR (95% CI) and P-value. It also computes an **overall
#' heterogeneity test** (likelihood ratio test of the interaction term
#' `exposure * subgroup`) on the full dataset.
#'
#' Robustness details:
#' - Accepts `data.frame` and `data.table` seamlessly (internally coerced).
#' - Auto-detects ID/time/event column names when not provided.
#' - When stratifying by a subgroup `sg`, that subgroup is **removed from the
#'   covariate set** for within-level fits (avoids singular/constant columns),
#'   while it is kept for the full-sample interaction test.
#' - Inside each subgroup level, covariates that become **constant** are pruned.
#' - Skips non-estimable strata (too few rows, no events/controls); no crash.
#'
#' @param exposure_data Data frame/table containing ID and the exposure column.
#' @param outcome_data  Data frame/table containing ID, time and event columns.
#' @param cov_data      Data frame/table containing ID, covariates and subgroup variables.
#' @param exposure_col  Character. Exposure column name (e.g. `"twa_benzene"` or `"twa_benzene_s"`).
#' @param id            Character. ID column name. If `NULL`, auto-detected
#'   among `c("eid","ID","id","participant_id")`.
#' @param time_col      Character. Time column name. If `NULL`, auto-detected
#'   among `c("all_cause_time","AR_time","time","followup_time","fu_time")`.
#' @param event_col     Character. Event column name. If `NULL`, auto-detected
#'   among `c("all_cause_status","AR_status","status","event","case")`.
#' @param subgroup_factors Character vector. Subgroup variables to iterate, e.g.
#'   `c("ethnicity","drinking_status","BMI","smoking_status",
#'     "physical_activity","diabetes","hypertension","asthma","sleep_cat")`.
#' @param model_t2      List of length 2 with covariate vectors:
#'   `list(model_1_covars, model_2_covars)`.
#' @param na_codes      Values treated as NA for subgroup factors (default UKB codes:
#'   `c(-1, -2, -3, -7, -8, -9)`).
#' @param ties          Cox ties handling, default `"efron"`.
#'
#' @return A `data.frame` with columns:
#' \itemize{
#' \item `subgroups` — subgroup name + level (concatenated)
#' \item `expose`, `outcome`
#' \item `Case_N`, `Control_N`, `Person_years`
#' \item `model_1.HR`, `model_1.95._CI`, `model_1.P.value`
#' \item `model_2.HR`, `model_2.95._CI`, `model_2.P.value`
#' \item `model_1_hets`, `model_2_hets` — P for interaction (LRT) on full data
#' }
#'
#' @section Why some NA used to appear and how it's fixed:
#' When stratifying by a subgroup (e.g., `BMI`), including the same variable in
#' the covariate set for within-level models makes it **constant** inside that
#' stratum, causing singularities or dropped columns, which led to `NA` HR/CI.
#' This function **removes the current subgroup from covariates** for the
#' within-level fits and **prunes constant covariates** per level, eliminating
#' those spurious `NA`s. The interaction P is still computed on the
#' **full sample** with `exposure * subgroup`.
#'
#' @examples
#' \dontrun{
#' # Prepare models
#' model_a <- c("age","gender")
#' model_b <- c(model_a,"ethnicity","BMI","education","household_income",
#'              "smoking_status","drinking_status","score_diet","physical_activity","discoast")
#' model_t2 <- list(model_a, model_b)
#'
#' # Minimal explicit usage (recommended to avoid auto-detect ambiguity)
#' sub_tbl <- z_subgroup(
#'   exposure_data, outcome_data, cov_data,
#'   exposure_col = "twa_benzene",
#'   id = "eid",
#'   time_col = "all_cause_time",
#'   event_col = "all_cause_status",
#'   model_t2 = model_t2,
#'   subgroup_factors = c("ethnicity","drinking_status","BMI",
#'                        "smoking_status","physical_activity",
#'                        "diabetes","hypertension","asthma","sleep_cat")
#' )
#' # data.table::fwrite(sub_tbl, "subgroup_table.csv")
#' }
#'
#' @export
#' @importFrom survival coxph Surv
#' @importFrom dplyr inner_join bind_rows
z_subgroup <- function(
    exposure_data, outcome_data, cov_data,
    exposure_col,
    id = NULL, time_col = NULL, event_col = NULL,
    subgroup_factors = c("ethnicity","drinking_status","BMI",
                         "smoking_status","physical_activity",
                         "diabetes","hypertension","asthma","sleep_cat"),
    model_t2,
    na_codes = c(-1, -2, -3, -7, -8, -9),
    ties = "efron"
){
  # ---- helpers (why: 兼容 data.table / 自动识别 / 防止奇异) ----
  .to_df <- function(x){
    if ("data.table" %in% class(x)) data.table::setDF(x)
    as.data.frame(x, stringsAsFactors = FALSE)
  }
  .detect_col <- function(df, candidates, label){
    hit <- intersect(candidates, names(df))
    if (length(hit)) return(hit[1])
    stop(sprintf("未找到 %s 列；候选：%s；现有：%s",
                 label, paste(candidates, collapse=", "),
                 paste(names(df), collapse=", ")))
  }
  .prune_const <- function(df, vars){
    vars[ vapply(vars, function(v){
      x <- df[[v]]; ux <- unique(x[!is.na(x)])
      length(ux) > 1
    }, TRUE) ]
  }
  surv_formula <- function(tcol, ecol, rhs)
    stats::as.formula(sprintf("survival::Surv(%s,%s) ~ %s", tcol, ecol, paste(rhs, collapse=" + ")))
  ci95 <- function(fit, term){
    sm <- summary(fit)
    ctab <- as.data.frame(sm$coefficients); citab <- as.data.frame(sm$conf.int)
    i <- match(term, rownames(ctab)); if (is.na(i)) return(list(HR=NA, CI=NA, P=NA))
    list(HR = unname(citab[i,"exp(coef)"]),
         CI = sprintf("%.3f, %.3f", citab[i,"lower .95"], citab[i,"upper .95"]),
         P  = unname(ctab[i,"Pr(>|z|)"]))
  }
  
  # ---- coerce & detect ----
  exposure_data <- .to_df(exposure_data)
  outcome_data  <- .to_df(outcome_data)
  cov_data      <- .to_df(cov_data)
  
  if (is.null(id))       id       <- .detect_col(outcome_data, c("eid","ID","id","participant_id"), "ID")
  if (is.null(time_col)) time_col <- .detect_col(outcome_data, c("all_cause_time","AR_time","time","followup_time","fu_time"), "time")
  if (is.null(event_col))event_col<- .detect_col(outcome_data, c("all_cause_status","AR_status","status","event","case"), "event")
  
  if (!exposure_col %in% names(exposure_data))
    stop("exposure_col not found in exposure_data: ", exposure_col)
  
  # ---- merge ----
  need_cov <- unique(c(id, unlist(model_t2), subgroup_factors))
  need_cov <- intersect(need_cov, names(cov_data))
  dat <- dplyr::inner_join(exposure_data[, c(id, exposure_col), drop=FALSE],
                           outcome_data[, c(id, time_col, event_col), drop=FALSE], by = id)
  dat <- dplyr::inner_join(dat, cov_data[, c(id, need_cov), drop=FALSE], by = id)
  
  # ---- clean ----
  for (v in intersect(names(dat), subgroup_factors)) {
    if (!is.numeric(dat[[v]])) dat[[v]][dat[[v]] %in% na_codes] <- NA
  }
  dat[[event_col]]    <- as.integer(dat[[event_col]])
  dat[[time_col]]     <- as.numeric(dat[[time_col]])
  dat[[exposure_col]] <- as.numeric(dat[[exposure_col]])
  
  out_rows <- list()
  
  for (sg in subgroup_factors) {
    if (!sg %in% names(dat)) next
    sg_all <- droplevels(as.factor(dat[[sg]]))
    levs <- levels(sg_all)
    
    # ---- interaction P (full sample) ----
    int_p <- c(NA_real_, NA_real_)
    for (mi in 1:2) {
      covars_i   <- setdiff(model_t2[[mi]], sg)  # 关键：交互检验仍加 sg，但不把 sg 作为协变量重复进入
      rhs_main   <- c(exposure_col, covars_i, sg)
      rhs_int    <- c(rhs_main, paste0(exposure_col, ":", sg))
      
      need_cols  <- unique(c(time_col, event_col, exposure_col, covars_i, sg))
      keep       <- stats::complete.cases(dat[, need_cols, drop = FALSE])
      df_i       <- dat[keep, , drop = FALSE]
      if (length(unique(na.omit(df_i[[sg]]))) < 2) { int_p[mi] <- NA; next }
      
      df_i[[sg]] <- droplevels(factor(df_i[[sg]]))
      covars_i   <- .prune_const(df_i, covars_i)  # why: 防止其它协变量在全样本中也退化为常量
      rhs_main   <- c(exposure_col, covars_i, sg)
      rhs_int    <- c(rhs_main, paste0(exposure_col, ":", sg))
      
      fit_m <- try(survival::coxph(surv_formula(time_col, event_col, rhs_main), df_i, ties = ties), silent = TRUE)
      fit_i <- try(survival::coxph(surv_formula(time_col, event_col, rhs_int ), df_i, ties = ties), silent = TRUE)
      if (inherits(fit_m,"try-error") || inherits(fit_i,"try-error")) { int_p[mi] <- NA; next }
      
      LLR <- 2 * (as.numeric(logLik(fit_i)) - as.numeric(logLik(fit_m)))
      df  <- attr(logLik(fit_i),"df") - attr(logLik(fit_m),"df")
      int_p[mi] <- stats::pchisq(LLR, df = df, lower.tail = FALSE)
    }
    
    # ---- per-level HRs (within each level; remove sg from covars; prune constants) ----
    for (lev in levs) {
      df_sub <- dat[sg_all == lev, , drop = FALSE]
      for (mi in 1:2) {
        covars_i  <- setdiff(model_t2[[mi]], sg)   # 关键：从协变量集中剔除当前分组变量
        rhs_raw   <- c(exposure_col, covars_i)
        need_cols <- unique(c(time_col, event_col, rhs_raw))
        keep      <- stats::complete.cases(df_sub[, need_cols, drop = FALSE])
        dfi       <- df_sub[keep, , drop = FALSE]
        
        Ncase <- sum(dfi[[event_col]] == 1)
        Nctrl <- sum(dfi[[event_col]] == 0)
        PY    <- sum(dfi[[time_col]])
        
        if (nrow(dfi) < 50 || Ncase < 1 || Nctrl < 1) {
          hr <- list(HR=NA,CI=NA,P=NA)
        } else {
          for (v in covars_i)
            if (is.character(dfi[[v]]) || is.factor(dfi[[v]])) dfi[[v]] <- droplevels(factor(dfi[[v]]))
          covars_i <- .prune_const(dfi, covars_i)
          rhs      <- c(exposure_col, covars_i)
          fit <- try(survival::coxph(surv_formula(time_col, event_col, rhs), data = dfi, ties = ties), silent = TRUE)
          hr  <- if (inherits(fit,"try-error")) list(HR=NA,CI=NA,P=NA) else ci95(fit, exposure_col)
        }
        
        key <- paste(sg, lev, sep="==")
        if (mi == 1) {
          out_rows[[key]] <- list(
            subgroups        = paste0(sg, lev),
            expose           = exposure_col,
            outcome          = event_col,
            Case_N           = Ncase,
            Control_N        = Nctrl,
            Person_years     = PY,
            model_1.HR       = hr$HR,
            model_1.95._CI   = hr$CI,
            model_1.P.value  = hr$P,
            model_2.HR       = NA,
            model_2.95._CI   = NA,
            model_2.P.value  = NA,
            model_1_hets     = int_p[1],
            model_2_hets     = NA
          )
        } else {
          row <- out_rows[[key]]
          row$model_2.HR      <- hr$HR
          row$model_2.95._CI  <- hr$CI
          row$model_2.P.value <- hr$P
          row$model_2_hets    <- int_p[2]
          out_rows[[key]] <- row
        }
      }
    }
  }
  
  res <- dplyr::bind_rows(lapply(out_rows, as.data.frame))
  res <- res[order(res$subgroups), ]
  rownames(res) <- NULL
  res
}

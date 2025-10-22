#' z_threshold: Two-piecewise (threshold) Cox regression summary table (robust)
#'
#' @description
#' Fit a standard Cox model and a two-piecewise Cox model with a single
#' inflection point on the exposure. The breakpoint is first searched by
#' \code{segmented::segmented()}; if that fails, a profile-likelihood grid
#' search is used as a fallback. Returns a publication-ready table.
#'
#' @param data data.frame/data.table containing analysis variables.
#' @param time_col,status_col Character. Follow-up time and event (0/1).
#' @param expose_col Character. Exposure column to search the inflection point.
#' @param covariates Character vector of covariate names (default \code{NULL}).
#' @param psi_init One of \code{"median"}, \code{"quantile25"}, \code{"quantile75"},
#'   or \code{"value"} (then use \code{psi_value}) as initial value for \code{segmented()}.
#' @param psi_value Numeric; only used when \code{psi_init="value"}.
#' @param conf_level Confidence level for CIs (default 0.95).
#' @param ties Ties handling for \code{survival::coxph} (default \code{"efron"}).
#' @param grid_points Integer. Number of candidates in fallback grid search (default 50).
#'
#' @return A data.frame with rows:
#' \itemize{
#' \item Model 1 Fitting model by standard Cox regression (HR(95%CI) P)
#' \item Model 2 Fitting model by two-piecewise Cox regression (blank)
#' \item Inflection point (estimated psi)
#' \item < psi : slope HR(95%CI)
#' \item ≥ psi : slope HR(95%CI)
#' \item P for likelihood ratio test
#' }
#' Attributes: \code{psi}, \code{fit_lin}, \code{fit_pw}.
#'
#' @examples
#' \dontrun{
#' res <- z_threshold(
#'   data = dt,
#'   time_col = "AR_time",
#'   status_col = "AR_status",
#'   expose_col = "twa_benzene",
#'   covariates = c("age","gender","ethnicity"),
#'   psi_init = "median"
#' )
#' print(res); attr(res, "psi")
#' utils::write.csv(res, "ar.csv",
#' row.names = FALSE, fileEncoding = "GB18030", na = "")
#' }
#' @export
#' @importFrom survival coxph Surv
#' @importFrom segmented segmented seg.control
#' @importFrom stats as.formula median quantile confint coef vcov qnorm pchisq logLik complete.cases
z_threshold <- function(
    data,
    time_col, status_col,
    expose_col,
    covariates = NULL,
    psi_init = c("median", "quantile25", "quantile75", "value"),
    psi_value = NULL,
    conf_level = 0.95,
    ties = "efron",
    grid_points = 50
) {
  # ---- checks & complete cases ----
  if (!is.data.frame(data)) stop("`data` must be a data.frame/data.table")
  need <- c(time_col, status_col, expose_col, covariates)
  miss <- setdiff(need, names(data))
  if (length(miss)) stop("Missing columns: ", paste(miss, collapse = ", "))
  
  dat <- data[, need, drop = FALSE]
  ok  <- stats::complete.cases(dat)
  if (!all(ok)) dat <- dat[ok, , drop = FALSE]
  
  # coerce types (why: Cox requires numeric time/event; exposure must be numeric)
  dat[[time_col]]   <- as.numeric(dat[[time_col]])
  dat[[status_col]] <- as.integer(dat[[status_col]])
  dat[[expose_col]] <- as.numeric(dat[[expose_col]])
  
  # ---- Model 1: linear Cox ----
  rhs_lin <- paste(c(expose_col, covariates), collapse = " + ")
  f_lin   <- stats::as.formula(paste0("survival::Surv(", time_col, ",", status_col, ") ~ ", rhs_lin))
  fit_lin <- survival::coxph(f_lin, data = dat, ties = ties, x = TRUE)
  
  # ---- robust initial psi in (P5, P95) ----
  x   <- dat[[expose_col]]
  qx  <- stats::quantile(x, c(.05, .50, .95), na.rm = TRUE)
  psi_init <- match.arg(psi_init)
  psi0_user <- switch(
    psi_init,
    median     = qx[2],
    quantile25 = stats::quantile(x, .25, na.rm = TRUE),
    quantile75 = stats::quantile(x, .75, na.rm = TRUE),
    value      = {
      if (is.null(psi_value)) stop("`psi_value` must be provided when psi_init='value'")
      as.numeric(psi_value)
    }
  )
  # clamp into (P5, P95) to avoid boundary failures
  eps  <- max(1e-6, diff(qx[c(1,3)]) * 1e-6)
  psi0 <- min(max(psi0_user, qx[1] + eps), qx[3] - eps)
  
  # ---- segmented() search with fallback ----
  fit_seg <- try(
    segmented::segmented(
      fit_lin,
      seg.Z   = stats::as.formula(paste0("~", expose_col)),
      psi     = list(structure(psi0, .Names = expose_col)),
      control = segmented::seg.control(n.boot = 0, it.max = 50, tol = 1e-6)
    ),
    silent = TRUE
  )
  
  if (inherits(fit_seg, "try-error")) {
    # Fallback: profile likelihood grid search in (P5, P95)
    grid <- seq(qx[1] + eps, qx[3] - eps, length.out = max(10L, as.integer(grid_points)))
    loglik <- sapply(grid, function(psi) {
      hinge <- pmax(0, dat[[expose_col]] - psi)
      hinge_name <- ".__hinge__"
      dat[[hinge_name]] <- hinge
      rhs_pw <- paste(c(expose_col, hinge_name, covariates), collapse = " + ")
      f_pw   <- stats::as.formula(paste0("survival::Surv(", time_col, ",", status_col, ") ~ ", rhs_pw))
      fit    <- try(survival::coxph(f_pw, data = dat, ties = ties), silent = TRUE)
      if (inherits(fit, "try-error")) return(NA_real_)
      as.numeric(stats::logLik(fit))
    })
    if (all(is.na(loglik))) stop("Grid search failed: all candidate fits failed.")
    psi_hat <- grid[which.max(loglik)]
  } else {
    psi_hat <- as.numeric(fit_seg$psi[1, "Est."])
  }
  
  # ---- Model 2: two-piecewise Cox ----
  hinge_name <- ".__hinge__"
  dat[[hinge_name]] <- pmax(0, dat[[expose_col]] - psi_hat)
  rhs_pw <- paste(c(expose_col, hinge_name, covariates), collapse = " + ")
  f_pw   <- stats::as.formula(paste0("survival::Surv(", time_col, ",", status_col, ") ~ ", rhs_pw))
  fit_pw <- survival::coxph(f_pw, data = dat, ties = ties, x = TRUE)
  
  # ---- Estimates ----
  # linear
  sm1 <- summary(fit_lin)$coefficients
  ci1 <- stats::confint(fit_lin, parm = expose_col, level = conf_level)
  hr1 <- unname(exp(stats::coef(fit_lin)[expose_col]))
  ci1 <- exp(ci1)
  p1  <- unname(sm1[expose_col, "Pr(>|z|)"])
  
  # piecewise slopes
  b    <- stats::coef(fit_pw)[c(expose_col, hinge_name)]
  V    <- stats::vcov(fit_pw)[c(expose_col, hinge_name), c(expose_col, hinge_name)]
  b1   <- unname(b[1])  # < psi
  b2   <- unname(b[2])  # delta slope for >= psi
  se1  <- sqrt(V[1,1])
  se2  <- sqrt(V[1,1] + V[2,2] + 2*V[1,2])  # >= psi
  zval <- stats::qnorm(1 - (1 - conf_level)/2)
  
  hr_lt <- exp(b1);             ci_lt <- exp(c(b1 - zval*se1,     b1 + zval*se1))
  hr_ge <- exp(b1 + b2);        ci_ge <- exp(c(b1 + b2 - zval*se2, b1 + b2 + zval*se2))
  
  # LR test
  LLR <- 2 * (stats::logLik(fit_pw) - stats::logLik(fit_lin))
  p_lr <- stats::pchisq(LLR, df = 1, lower.tail = FALSE)
  
  # ---- Table ----
  fmt <- function(hr, ci) sprintf("%.3f(%.3f–%.3f)", hr, ci[1], ci[2])
  out <- data.frame(
    Outcome = c(
      "Model 1 Fitting model by standard Cox regression",
      "Model 2 Fitting model by two-piecewise Cox regression",
      "Inflection point",
      sprintf("< %.3f", psi_hat),
      sprintf("≥ %.3f", psi_hat),
      "P for likelihood ratio test"
    ),
    `the effect size, 95%CI, P value` = c(
      sprintf("%s%s", fmt(hr1, ci1), signif(p1, 3)),
      "",
      sprintf("%.3f", psi_hat),
      fmt(hr_lt, ci_lt),
      fmt(hr_ge, ci_ge),
      format.pval(p_lr, digits = 3)
    ),
    check.names = FALSE
  )
  
  attr(out, "psi")     <- psi_hat
  attr(out, "fit_lin") <- fit_lin
  attr(out, "fit_pw")  <- fit_pw
  out
}

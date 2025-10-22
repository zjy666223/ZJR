# ============== z_rename(): 一键重命名单列/多列（可直接放入你的包） ==============
#' Rename columns by mapping or paired vectors (robust, base R).
#'
#' @param data A data.frame/tibble/data.table; returned class matches input.
#' @param mapping Named character or list: names are old, values are new.
#'   Example: c(old1 = "new1", old2 = "new2"). Ignored if `old`+`new` provided.
#' @param old Character vector of old names (alternative to `mapping`).
#' @param new Character vector of new names, same length and order as `old`.
#' @param strict logical. If TRUE, error on missing old names or duplicate new names.
#' @param quiet logical. If FALSE, print a concise summary of renames.
#' @return The same object with columns renamed (same class as input).
#' @examples
#' df <- data.frame(a=1,b=2,c=3)
#' z_rename(df, mapping = c(b="beta", c="gamma"))
#' z_rename(df, old=c("a","b"), new=c("alpha","beta"))
#' @export
z_rename <- function(data,
                     mapping = NULL,
                     old = NULL,
                     new = NULL,
                     strict = TRUE,
                     quiet = FALSE) {
  if (!is.data.frame(data)) stop("`data` must be data.frame-like.")
  
  # unify mapping
  if (!is.null(mapping)) {
    if (is.list(mapping)) mapping <- unlist(mapping, use.names = TRUE)
    if (is.null(names(mapping)) || any(!nzchar(names(mapping))))
      stop("`mapping` must be a *named* character vector: c(old = \"new\").")
    old <- names(mapping)
    new <- as.character(unname(mapping))
  } else {
    if (is.null(old) || is.null(new))
      stop("Provide either `mapping` or both `old` and `new`.")
    if (length(old) != length(new))
      stop("`old` and `new` must have the same length.")
  }
  
  old <- as.character(old); new <- as.character(new)
  if (any(!nzchar(new))) stop("`new` names must be non-empty.")
  
  current <- names(data)
  # strict: all old must exist
  missing_old <- setdiff(old, current)
  if (strict && length(missing_old))
    stop("Missing columns: ", paste(missing_old, collapse = ", "))
  
  # map indices (only those that exist)
  idx <- match(old, current)
  keep <- !is.na(idx)
  if (!any(keep)) {
    if (!quiet) message("No columns renamed (no `old` present in data).")
    return(data)
  }
  idx <- idx[keep]
  new_keep <- new[keep]
  
  # build candidate names
  out_names <- current
  out_names[idx] <- new_keep
  
  # handle duplicates
  if (anyDuplicated(out_names)) {
    if (strict) {
      dups <- unique(out_names[duplicated(out_names)])
      stop("Renaming would create duplicate names: ", paste(dups, collapse = ", "))
    } else {
      # stabilize to make names unique by appending ..1, ..2 only to conflicting positions
      seen <- character()
      for (i in seq_along(out_names)) {
        nm <- out_names[i]
        if (nm %in% seen) {
          k <- 1L
          cand <- paste0(nm, "..", k)
          while (cand %in% c(seen, out_names[seq_len(i-1)])) { k <- k + 1L; cand <- paste0(nm, "..", k) }
          out_names[i] <- cand
        }
        seen <- c(seen, out_names[i])
      }
    }
  }
  
  names(data) <- out_names
  if (!quiet) {
    msg <- paste0(current[idx], " -> ", names(data)[idx])
    message("Renamed: ", paste(msg, collapse="; "))
  }
  data
}

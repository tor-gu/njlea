# Internal helpers.

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Stop unless `year` is a single year njlea covers (from `min_year`)
#' @noRd
check_year_ <- function(year, argument, min_year = MIN_YEAR) {
  if (!is.numeric(year)) {
    stop("`", argument, "` must be numeric, not ", class(year)[[1L]], call. = FALSE)
  }
  if (length(year) != 1L) {
    stop("`", argument, "` must be a single year, not ", length(year), " values", call. = FALSE)
  }
  if (is.na(year) || year < min_year || year > MAX_YEAR) {
    stop("`", argument, "` must be between ", min_year, " and ", MAX_YEAR, ", not ", year,
         call. = FALSE)
  }
  invisible(as.integer(year))
}

#' Recycle `x` to length `n`, allowing only length 1 or n
#' @noRd
recycle_ <- function(x, n, argument) {
  if (length(x) == n) return(x)
  if (length(x) == 1L) return(rep(x, n))
  stop("`", argument, "` must have length 1 or ", n, ", not ", length(x), call. = FALSE)
}

#' Rows of a years table true in `year` (first_year <= year <= final_year, NA = open)
#' @noRd
in_year_ <- function(df, year) {
  df[df$first_year <= year & (is.na(df$final_year) | df$final_year >= year), , drop = FALSE]
}

#' Each agency's latest epoch (its current name)
#' @noRd
latest_epoch_ <- function() {
  njlea::agency |>
    dplyr::group_by(agency_id) |>
    dplyr::slice_max(first_year, n = 1, with_ties = FALSE) |>
    dplyr::ungroup()
}

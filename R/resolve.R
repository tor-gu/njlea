#' Resolve source spellings to agency IDs
#'
#' Looks up each (source, county, name) spelling, exactly as the source writes it, in
#' [agency_alias]. Intended for the build scripts of packages that use njlea: run with
#' `strict = TRUE` so that a new spelling stops the build and can be added to njlea.
#'
#' @param source Alias source, length 1 or the length of `name`. Must be in
#'   [alias_source].
#' @param county County as written by the source (`NA` allowed), length 1 or the length
#'   of `name`.
#' @param name Agency name as written by the source.
#' @param year Year of each record, length 1 or the length of `name`. Needed only where an
#'   alias has years (a spelling that changed meaning); ignored otherwise.
#' @param strict If `TRUE` (the default), stop with a message listing every distinct
#'   unresolved spelling. If `FALSE`, return `NA` for them, with a warning.
#' @return A character vector of agency IDs, the same length as `name`.
#' @seealso [suggest_agency()] for candidates for an unresolved spelling.
#' @examples
#' resolve_agency("njoaguof", "Atlantic County", c("Absecon City PD", "Atlantic City PD"))
#' resolve_agency("njoag-major-discipline", "State or Other Agency", "NJ State Parole Board")
#' @export
resolve_agency <- function(source, county, name, year = NULL, strict = TRUE) {
  if (!is.character(name)) stop("`name` must be a character vector", call. = FALSE)
  n <- length(name)
  if (n == 0L) return(character())
  source <- recycle_(as.character(source), n, "source")
  county <- recycle_(as.character(county), n, "county")
  year <- if (is.null(year)) rep(NA_integer_, n) else recycle_(as.integer(year), n, "year")
  unknown <- setdiff(unique(source), njlea::alias_source$source)
  if (length(unknown)) {
    stop("Unknown alias source: ", paste(unknown, collapse = ", "), ". Known sources: ",
         paste(njlea::alias_source$source, collapse = ", "), call. = FALSE)
  }

  query <- tibble::tibble(.row = seq_len(n), source = source, source_county = county,
                          source_name = name, year = year)
  hits <- dplyr::inner_join(query, njlea::agency_alias,
                            by = c("source", "source_county", "source_name"),
                            relationship = "many-to-many")
  bounded <- !is.na(hits$first_year) | !is.na(hits$final_year)
  in_range <- !is.na(hits$year) &
    (is.na(hits$first_year) | hits$year >= hits$first_year) &
    (is.na(hits$final_year) | hits$year <= hits$final_year)
  hits <- hits[!bounded | in_range, , drop = FALSE]
  hits <- dplyr::distinct(hits, .row, agency_id)
  if (anyDuplicated(hits$.row)) {
    stop("Ambiguous spelling: it resolves to more than one agency. This is a bug in njlea's ",
         "alias table.", call. = FALSE)
  }

  id <- rep(NA_character_, n)
  id[hits$.row] <- hits$agency_id
  if (anyNA(id)) {
    bad <- unique(query[is.na(id), c("source", "source_county", "source_name")])
    msg <- paste0(nrow(bad), " spelling", if (nrow(bad) > 1) "s" else "",
                  " did not resolve to an agency (source | county | name):\n",
                  paste0("  ", bad$source, " | ", bad$source_county, " | ", bad$source_name,
                         collapse = "\n"),
                  "\nUse suggest_agency() for candidates.")
    if (strict) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
  }
  id
}

#' Suggest agencies for an unresolved spelling
#'
#' Compares each (county, name) with every canonical name and source spelling in the same
#' county, using loose keys that ignore case, punctuation, abbreviations ("Twp", "Co",
#' "Dept"), type words and Major Discipline's " - X County" suffix. Only suggests; add the
#' spelling to [agency_alias] to resolve it.
#'
#' @param county County, in any common spelling ("Atlantic", "Atlantic County"); a
#'   non-county value such as "State or Other Agency" or `NA` means agencies without a
#'   county.
#' @param name Agency name.
#' @param n Maximum number of candidates per spelling.
#' @return A tibble with one row per candidate: `county`, `name` (as given), `agency_id`,
#'   `agency_name` (current) and `method` (`"normalised"`, `"loose"` or `"distance"`).
#'   A spelling with no candidate in its county gets one row with `NA` candidate.
#' @examples
#' suggest_agency("Bergen", "Ridgefield PD")
#' suggest_agency("Atlantic", "Hamilton Twp PD - Atlantic County")
#' @export
suggest_agency <- function(county, name, n = 3) {
  if (!is.character(name)) stop("`name` must be a character vector", call. = FALSE)
  county <- recycle_(as.character(county), length(name), "county")
  current <- latest_epoch_()
  pool <- dplyr::bind_rows(
    njlea::agency |> dplyr::transmute(agency_id, county_key = county, spelling = agency_name),
    njlea::agency_alias |>
      dplyr::transmute(agency_id, county_key = county_name(source_county), spelling = source_name)
  ) |>
    dplyr::distinct() |>
    dplyr::mutate(norm = norm_key(spelling), loose = loose_key(spelling))

  inputs <- unique(tibble::tibble(county = county, name = name))
  out <- lapply(seq_len(nrow(inputs)), function(i) {
    ck <- county_name(inputs$county[[i]])
    cand <- pool[(is.na(ck) & is.na(pool$county_key)) | pool$county_key %in% ck, , drop = FALSE]
    nm_norm <- norm_key(inputs$name[[i]])
    nm_loose <- loose_key(inputs$name[[i]])
    pick <- if (any(cand$norm == nm_norm)) {
      dplyr::mutate(cand[cand$norm == nm_norm, ], method = "normalised", d = 0)
    } else if (any(cand$loose == nm_loose)) {
      dplyr::mutate(cand[cand$loose == nm_loose, ], method = "loose", d = 0)
    } else if (nrow(cand)) {
      dplyr::mutate(cand, method = "distance", d = as.vector(utils::adist(nm_loose, cand$loose)))
    } else {
      cand[0, ]
    }
    pick <- pick |>
      dplyr::group_by(agency_id) |>
      dplyr::summarise(method = dplyr::first(method), d = min(d), .groups = "drop") |>
      dplyr::arrange(d, agency_id) |>
      utils::head(n)
    if (!nrow(pick)) {
      pick <- tibble::tibble(agency_id = NA_character_, method = NA_character_)
    }
    tibble::tibble(county = inputs$county[[i]], name = inputs$name[[i]],
                   agency_id = pick$agency_id, method = pick$method)
  })
  dplyr::bind_rows(out) |>
    dplyr::left_join(current |> dplyr::select(agency_id, agency_name), by = "agency_id") |>
    dplyr::select(county, name, agency_id, agency_name, method)
}

#' Check that agencies existed in the given years
#'
#' For consumer package builds: every (agency, year) in a dataset should fall within one of
#' the agency's name epochs in [agency].
#'
#' @param agency_id Agency IDs.
#' @param year Years, length 1 or the length of `agency_id`.
#' @return `invisible(TRUE)`, or an error listing every distinct (agency_id, year) pair that
#'   falls outside every epoch of its agency (or names an unknown agency).
#' @examples
#' check_agency_years(c("NJLEA0001", "NJLEA0002"), 2023)
#' @export
check_agency_years <- function(agency_id, year) {
  agency_id <- as.character(agency_id)
  year <- recycle_(as.integer(year), length(agency_id), "year")
  pairs <- unique(tibble::tibble(agency_id = agency_id, year = year))
  hits <- dplyr::inner_join(pairs, njlea::agency |> dplyr::select(agency_id, first_year, final_year),
                            by = "agency_id", relationship = "many-to-many")
  hits <- hits[!is.na(hits$year) & hits$first_year <= hits$year &
                 (is.na(hits$final_year) | hits$final_year >= hits$year), , drop = FALSE]
  bad <- dplyr::anti_join(pairs, hits, by = c("agency_id", "year"))
  if (nrow(bad)) {
    stop(nrow(bad), " (agency_id, year) pair", if (nrow(bad) > 1) "s" else "",
         " outside every epoch of the agency:\n",
         paste0("  ", bad$agency_id, " ", bad$year, collapse = "\n"), call. = FALSE)
  }
  invisible(TRUE)
}

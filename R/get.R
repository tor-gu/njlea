#' Get the agencies in existence in a year
#'
#' @param year A year from 2020 to 2026. `NULL` (the default) means the latest year
#'   njlea covers.
#' @return A tibble with one row per agency in existence in `year`: `agency_id`,
#'   `agency_name` (as of `year`), `county`, `agency_level`, `agency_function` and
#'   `multi_municipal` (`TRUE` if the agency serves more than one municipality in `year`,
#'   derived from [agency_municipality]).
#' @examples
#' get_agencies(2025)
#' @export
get_agencies <- function(year = NULL) {
  year <- check_year_(year %||% MAX_YEAR, "year")
  n_munis <- in_year_(njlea::agency_municipality, year) |>
    dplyr::distinct(agency_id, GEOID_Y2K) |>
    dplyr::count(agency_id, name = "n_munis")
  in_year_(njlea::agency, year) |>
    dplyr::left_join(n_munis, by = "agency_id") |>
    dplyr::mutate(multi_municipal = dplyr::coalesce(n_munis, 0L) > 1L) |>
    dplyr::select(agency_id, agency_name, county, agency_level, agency_function,
                  multi_municipal) |>
    dplyr::arrange(agency_id)
}

#' Get the municipalities agencies serve in a year
#'
#' @param year A year from 2020 to 2026. `NULL` (the default) means the latest year
#'   njlea covers.
#' @param geoid_year The year whose Census GEOIDs to use, from 2000 to 2026, as in
#'   [njmunicipalities::get_municipalities()]. Defaults to `year`; use 2000 for the stable
#'   `GEOID_Y2K` codes.
#' @return A tibble with one row per agency and municipality served in `year`: `agency_id`,
#'   `GEOID` (as of `geoid_year`), `municipality` and `county` (as of `year`) and
#'   `link_type` (`Home` or `Service`).
#' @details
#' NJ State Police links are not included yet. NJSP is the local police of many rural
#' municipalities (77 full-time and 24 part-time, per a 2022 bill), but no published list
#' names them. Most municipalities with no row here are NJSP-patrolled, though not all.
#' @examples
#' get_agency_municipalities(2025)
#' @export
get_agency_municipalities <- function(year = NULL, geoid_year = year) {
  year <- check_year_(year %||% MAX_YEAR, "year")
  geoid_year <- geoid_year %||% year
  check_year_(geoid_year, "geoid_year", min_year = 2000L)   # any njmunicipalities year
  y2k <- njmunicipalities::get_municipalities(year, geoid_year = 2000,
                                              geoid_ref_as_ref_column = TRUE) |>
    dplyr::select(GEOID_Y2K = GEOID_ref, GEOID_now = GEOID, county, municipality)
  ref <- njmunicipalities::get_municipalities(year, geoid_year = geoid_year,
                                              geoid_ref_as_ref_column = TRUE) |>
    dplyr::select(GEOID_now = GEOID, GEOID = GEOID_ref)
  in_year_(njlea::agency_municipality, year) |>
    dplyr::inner_join(y2k, by = "GEOID_Y2K") |>
    dplyr::left_join(ref, by = "GEOID_now") |>
    dplyr::select(agency_id, GEOID, municipality, county, link_type) |>
    dplyr::arrange(agency_id, link_type, GEOID)
}

#' Find agencies by name
#'
#' Searches canonical names and every source spelling.
#'
#' @param pattern A regular expression, matched case-insensitively.
#' @param county Optional county ("Atlantic" or "Atlantic County"): only agencies whose own
#'   county it is. State, Bi-state and Private agencies have no county.
#' @param year Optional year: only agencies in existence then, with their names as of
#'   `year`. `NULL` searches all epochs and reports current names.
#' @return A tibble: `agency_id`, `agency_name`, `county`, `matched_on` (`"name"` or the
#'   alias source) and `matched_text`.
#' @examples
#' find_agency("sheriff", county = "Bergen")
#' find_agency("juvenile justice")
#' @export
find_agency <- function(pattern, county = NULL, year = NULL) {
  if (!is.character(pattern) || length(pattern) != 1L) {
    stop("`pattern` must be a single string", call. = FALSE)
  }
  epochs <- if (is.null(year)) njlea::agency else in_year_(njlea::agency, check_year_(year, "year"))
  shown <- if (is.null(year)) latest_epoch_() else epochs
  texts <- dplyr::bind_rows(
    epochs |> dplyr::transmute(agency_id, matched_on = "name", matched_text = agency_name),
    njlea::agency_alias |>
      dplyr::filter(agency_id %in% epochs$agency_id) |>
      dplyr::transmute(agency_id, matched_on = source, matched_text = source_name)
  )
  hits <- texts[grepl(pattern, texts$matched_text, ignore.case = TRUE, perl = TRUE), ] |>
    dplyr::distinct()
  out <- hits |>
    dplyr::inner_join(shown |> dplyr::select(agency_id, agency_name, county), by = "agency_id")
  if (!is.null(county)) {
    ck <- county_name(county)
    if (anyNA(ck)) stop("Unknown county: ", county, call. = FALSE)
    out <- out[out$county %in% ck, , drop = FALSE]
  }
  out |>
    dplyr::select(agency_id, agency_name, county, matched_on, matched_text) |>
    dplyr::arrange(agency_id, matched_on != "name", matched_on, matched_text)
}

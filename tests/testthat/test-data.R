# Package tests on the shipped tables (SCHEMA.md section 4). They need only the shipped data
# and njmunicipalities; the checks against raw snapshots live in data-raw/build_registry.R.

open_final <- function(x) ifelse(is.na(x), MAX_YEAR, x)

# All (row, year) pairs a years table covers, NA final year = through MAX_YEAR.
expand_years <- function(df) {
  df$.row <- seq_len(nrow(df))
  reps <- open_final(df$final_year) - df$first_year + 1L
  out <- df[rep(df$.row, reps), , drop = FALSE]
  out$year <- unlist(Map(seq, df$first_year, open_final(df$final_year)))
  out
}

# Every row's years fall within its agency's years.
within_agency_years <- function(df) {
  span <- agency |>
    dplyr::group_by(agency_id) |>
    dplyr::summarise(a_first = min(first_year), a_final = max(open_final(final_year)))
  j <- dplyr::left_join(df, span, by = "agency_id")
  j$first_year >= j$a_first & open_final(j$final_year) <= j$a_final
}

test_that("agency: IDs, keys and years", {
  # NJLEA IDs since the one-time renumbering (decision 8; data-raw/seed_id_map.csv)
  expect_true(all(grepl("^NJLEA[0-9]{4}$", agency$agency_id)))
  expect_false(anyDuplicated(agency[c("agency_id", "first_year")]) > 0)
  expect_true(all(agency$first_year >= MIN_YEAR))
  expect_true(all(is.na(agency$final_year) | agency$final_year >= agency$first_year))
  epochs <- expand_years(agency)
  expect_false(anyDuplicated(epochs[c("agency_id", "year")]) > 0)   # no overlapping epochs
  expect_false(anyNA(agency$agency_name))
})

test_that("agency: level, function and county", {
  expect_s3_class(agency$agency_level, "factor")
  expect_s3_class(agency$agency_function, "factor")
  expect_false(anyNA(agency$agency_level))
  expect_false(anyNA(agency$agency_function))
  expect_identical(levels(agency$agency_level), AGENCY_LEVELS)
  expect_identical(levels(agency$agency_function), AGENCY_FUNCTIONS)
  # Constant across an agency's epochs. Exceptions, with reasons: none yet.
  per_agency <- agency |>
    dplyr::group_by(agency_id) |>
    dplyr::summarise(n_level = dplyr::n_distinct(agency_level),
                     n_function = dplyr::n_distinct(agency_function))
  expect_true(all(per_agency$n_level == 1 & per_agency$n_function == 1))
  # County spelled as in njmunicipalities, and present exactly for Municipal, County and
  # School district agencies (decision 9).
  expect_true(all(is.na(agency$county) | agency$county %in% njmunicipalities::counties$county))
  expect_identical(!is.na(agency$county), as.character(agency$agency_level) %in% COUNTY_LEVELS)
})

test_that("agency: hand-written notes are present", {
  # Guards against migrate_seed.R dropping notes (an ifelse() recycling bug once kept only
  # the review marker on every non-municipal agency).
  note_of <- function(id) agency$note[agency$agency_id == id]
  expect_match(note_of("NJLEA0505"), "1978")                  # Union County PD
  expect_match(note_of("NJLEA0135"), "Board of Education")    # Cherry Hill Campus Police
  expect_match(note_of("NJLEA0371"), "2022-01-01")            # Morris County Park PD merger
  expect_match(note_of("NJLEA0447"), "Sheriff's Office")      # Salem County Correctional Facility
  expect_match(note_of("NJLEA0433"), "state")                 # Paterson
  # The name review is complete (2026-11-01): no proposed names remain.
  expect_false(any(grepl("proposed, unverified", agency$note, fixed = TRUE)))
})

test_that("every table's agency_id exists and its years fall within the agency's", {
  for (tbl in list(agency_alias, agency_municipality, agency_ori)) {
    expect_true(all(tbl$agency_id %in% agency$agency_id))
  }
  dated <- function(df) df[!is.na(df$first_year), , drop = FALSE]
  expect_true(all(within_agency_years(agency_municipality)))
  expect_true(all(within_agency_years(agency_ori)))
  expect_true(all(within_agency_years(dated(agency_alias))))
})

test_that("agency_municipality: GEOIDs, Home links, notes", {
  expect_false(anyDuplicated(agency_municipality[c("agency_id", "GEOID_Y2K", "first_year")]) > 0)
  expect_identical(levels(agency_municipality$link_type), LINK_TYPES)
  links <- expand_years(agency_municipality)
  munis <- dplyr::bind_rows(lapply(sort(unique(links$year)), function(y)
    njmunicipalities::get_municipalities(y, geoid_year = 2000) |>
      dplyr::transmute(year = y, GEOID_Y2K = GEOID, muni_county = county)))
  # The GEOID exists in every year of the link (so Pine Valley links end by 2022).
  missing <- dplyr::anti_join(links, munis, by = c("year", "GEOID_Y2K"))
  expect_equal(nrow(missing), 0)
  # Every Municipal agency has exactly one Home link in each year of each epoch, and only
  # Municipal agencies have Home links.
  municipal <- expand_years(agency[agency$agency_level == "Municipal", ])
  home <- links[links$link_type == "Home", ]
  n_home <- dplyr::count(home, agency_id, year)
  j <- dplyr::left_join(municipal[c("agency_id", "year")], n_home, by = c("agency_id", "year"))
  expect_true(all(j$n %in% 1L))
  expect_true(all(home$agency_id %in% municipal$agency_id))
  # A Municipal agency's county is its Home municipality's county (decision 9).
  hc <- home |>
    dplyr::inner_join(munis, by = c("year", "GEOID_Y2K")) |>
    dplyr::inner_join(agency |> dplyr::select(agency_id, county), by = "agency_id")
  expect_identical(hc$county, hc$muni_county)
  # Service links have a source note.
  service <- agency_municipality[agency_municipality$link_type == "Service", ]
  expect_false(any(is.na(service$note) | service$note == ""))
})

test_that("agency_alias: sources, keys, coverage", {
  expect_true(all(agency_alias$source %in% alias_source$source))
  key <- agency_alias[c("source", "source_county", "source_name", "first_year")]
  expect_false(anyDuplicated(key) > 0)
  # One spelling never points at two agencies in overlapping years. With no alias years set
  # yet, that means one agency per spelling; revisit when a spelling changes meaning.
  per_spelling <- agency_alias |>
    dplyr::group_by(source, source_county, source_name) |>
    dplyr::summarise(n = dplyr::n_distinct(agency_id), dated = any(!is.na(first_year) |
                                                                      !is.na(final_year)),
                     .groups = "drop")
  expect_true(all(per_spelling$n == 1 | per_spelling$dated))
  # Universe rule (decision 1): every agency came from a source.
  expect_true(all(agency$agency_id %in% agency_alias$agency_id))
})

test_that("agency_ori: unique, well-formed", {
  expect_false(anyDuplicated(agency_ori$ori) > 0)
  expect_true(all(grepl("^[A-Z]{2}[0-9A-Z]{7}$", agency_ori$ori)))
  expect_identical(levels(agency_ori$match), ORI_MATCHES)
  expect_true(all(is.na(agency_ori$GEOID_Y2K) | grepl("^34[0-9]{8}$", agency_ori$GEOID_Y2K)))
})

test_that("alias_source: unique sources", {
  expect_false(anyDuplicated(alias_source$source) > 0)
  expect_s3_class(alias_source$retrieved, "Date")
})

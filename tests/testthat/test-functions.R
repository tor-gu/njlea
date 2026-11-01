test_that("resolve_agency resolves exact source spellings, NA county included", {
  expect_identical(
    resolve_agency("njoaguof", "Atlantic County", c("Absecon City PD", "Atlantic City PD")),
    c("NJLEA0001", "NJLEA0002"))
  # njoaguof writes state agencies with an NA county
  expect_identical(resolve_agency("njoaguof", NA, "State Parole Board"), "NJLEA0536")
  expect_identical(
    resolve_agency("njoag-major-discipline", "State or Other Agency", "NJ State Parole Board"),
    "NJLEA0536")
  # vectorised over source and county
  expect_identical(
    resolve_agency(c("njoaguof", "njoag-major-discipline"), c("Bergen County", "Bergen"),
                   c("Bergen Co Sheriffs Dept", "Bergen County Sheriffs Office")),
    c("NJLEA0023", "NJLEA0023"))
  expect_identical(resolve_agency("njoaguof", NA, character()), character())
})

test_that("resolve_agency strict error lists every distinct unresolved spelling", {
  err <- expect_error(
    resolve_agency("njoaguof", "Atlantic County",
                   c("Absecon City PD", "Nowhere PD", "Nowhere PD", "Elsewhere PD")),
    "2 spellings did not resolve")
  expect_match(conditionMessage(err), "njoaguof | Atlantic County | Nowhere PD", fixed = TRUE)
  expect_match(conditionMessage(err), "njoaguof | Atlantic County | Elsewhere PD", fixed = TRUE)
  # Spellings are exact: the county must be written as the source writes it.
  expect_error(resolve_agency("njoaguof", "Atlantic", "Absecon City PD"), "did not resolve")
})

test_that("resolve_agency non-strict gives NA with a warning", {
  expect_warning(
    id <- resolve_agency("njoaguof", "Atlantic County", c("Absecon City PD", "Nowhere PD"),
                         strict = FALSE),
    "1 spelling did not resolve")
  expect_identical(id, c("NJLEA0001", NA))
})

test_that("resolve_agency rejects unknown sources and bad lengths", {
  expect_error(resolve_agency("not-a-source", NA, "X"), "Unknown alias source")
  expect_error(resolve_agency("njoaguof", c("A", "B", "C"), c("x", "y")), "length 1 or 2")
  expect_error(resolve_agency("njoaguof", NA, 1), "character")
})

test_that("suggest_agency finds same-county candidates by loose key", {
  s <- suggest_agency("Bergen", "Ridgefield Police")   # not a source spelling
  expect_identical(s$agency_id[[1]], "NJLEA0071")
  expect_identical(s$method[[1]], "loose")
  s <- suggest_agency("Atlantic", "Hamilton Twp PD - Atlantic County")
  expect_identical(s$agency_id[[1]], "NJLEA0010")
  # Without a county: agencies with no county (state, bi-state, private)
  s <- suggest_agency("State or Other Agency", "New Jersey State Police")
  expect_identical(s$agency_id[[1]], "NJLEA0537")
  expect_identical(s$method[[1]], "normalised")
  expect_named(s, c("county", "name", "agency_id", "agency_name", "method"))
  # Falls back to edit distance, at most n candidates
  s <- suggest_agency("Bergen County", "Ridgewod Vilage PD", n = 2)
  expect_lte(nrow(s), 2)
  expect_identical(s$agency_id[[1]], "NJLEA0072")
  expect_identical(s$method[[1]], "distance")
})

test_that("check_agency_years accepts covered years and lists the rest", {
  expect_true(check_agency_years(c("NJLEA0001", "NJLEA0002"), 2023))
  err <- expect_error(check_agency_years(c("NJLEA0001", "NJLEA0001", "Z9999"), c(2019, 2019, 2023)),
                      "2 \\(agency_id, year\\) pairs")
  expect_match(conditionMessage(err), "NJLEA0001 2019", fixed = TRUE)
  expect_match(conditionMessage(err), "Z9999 2023", fixed = TRUE)
})

test_that("get_agencies returns one row per agency in the year", {
  a <- get_agencies(2025)
  expect_named(a, c("agency_id", "agency_name", "county", "agency_level", "agency_function",
                    "multi_municipal"))
  expect_false(anyDuplicated(a$agency_id) > 0)
  alive <- agency$first_year <= 2025 & (is.na(agency$final_year) | agency$final_year >= 2025)
  expect_identical(nrow(a), dplyr::n_distinct(agency$agency_id[alive]))
  expect_false("NJLEA0371" %in% a$agency_id)                       # Morris County Park PD, ended 2021
  expect_true("NJLEA0371" %in% get_agencies(2021)$agency_id)
  # Name epochs: Middlesex County College became Middlesex College on 2021-01-01
  name_in <- function(y) get_agencies(y)$agency_name[get_agencies(y)$agency_id == "NJLEA0281"]
  expect_identical(name_in(2020), "Middlesex County College Police Department")
  expect_identical(name_in(2021), "Middlesex College Police Department")
  # An end on 2024-09-01: Woodlynne PD exists through 2024; Camden County PD serves
  # Woodlynne from 2024 (overlap in the handover year)
  expect_true("NJLEA0162" %in% get_agencies(2024)$agency_id)
  expect_false("NJLEA0162" %in% get_agencies(2025)$agency_id)
  ccpd <- function(y) get_agencies(y)$multi_municipal[get_agencies(y)$agency_id == "NJLEA0132"]
  expect_false(ccpd(2023))
  expect_true(ccpd(2024))
  # Woodbury Heights PD dissolved in 2023; Woodbury PD serves Woodbury Heights from 2023
  expect_true("NJLEA0226" %in% get_agencies(2023)$agency_id)
  expect_false("NJLEA0226" %in% get_agencies(2024)$agency_id)
  wpd <- function(y) get_agencies(y)$multi_municipal[get_agencies(y)$agency_id == "NJLEA0227"]
  expect_false(wpd(2022))
  expect_true(wpd(2023))
  # A mid-year rename starts the new epoch in the year it takes effect (2025-03-17)
  yjc_in <- function(y) get_agencies(y)$agency_name[get_agencies(y)$agency_id == "NJLEA0539"]
  expect_identical(yjc_in(2024), "New Jersey Juvenile Justice Commission")
  expect_identical(yjc_in(2025), "New Jersey Youth Justice Commission")
  expect_true(a$multi_municipal[a$agency_id == "NJLEA0355"])    # Chester Township PD
  expect_false(a$multi_municipal[a$agency_id == "NJLEA0001"])
  expect_identical(get_agencies(), get_agencies(MAX_YEAR))
  expect_error(get_agencies(2019), "between 2020")
  expect_error(get_agencies(c(2021, 2022)), "single year")
  expect_error(get_agency_municipalities(2025, geoid_year = 1999), "between 2000")
})

test_that("get_agency_municipalities maps GEOIDs to the requested year", {
  m <- get_agency_municipalities(2025, geoid_year = 2000)
  expect_named(m, c("agency_id", "GEOID", "municipality", "county", "link_type"))
  expect_true(all(m$GEOID %in% agency_municipality$GEOID_Y2K))
  home <- m[m$link_type == "Home", ]
  expect_false(anyDuplicated(home$agency_id) > 0)
  expect_false(anyNA(get_agency_municipalities(2026)$GEOID))
  expect_identical(nrow(get_agency_municipalities(2025)), nrow(m))
})

test_that("find_agency searches canonical names and aliases", {
  f <- find_agency("juvenile justice")
  expect_true("NJLEA0539" %in% f$agency_id)
  # Matches the old canonical name (2020-2024 epoch) and old source spellings, and reports
  # the current name; with a year, the name as of that year.
  expect_true(all(c("name", "njoaguof") %in% f$matched_on[f$agency_id == "NJLEA0539"]))
  expect_identical(unique(f$agency_name[f$agency_id == "NJLEA0539"]), "New Jersey Youth Justice Commission")
  f24 <- find_agency("juvenile justice", year = 2024)
  expect_identical(unique(f24$agency_name[f24$agency_id == "NJLEA0539"]), "New Jersey Juvenile Justice Commission")
  f <- find_agency("sheriff", county = "Bergen")
  expect_identical(unique(f$agency_id), "NJLEA0023")
  expect_identical(f$matched_on[[1]], "name")
  expect_equal(nrow(find_agency("no such agency anywhere")), 0)
  expect_error(find_agency("x", county = "Nowhere"), "Unknown county")
})

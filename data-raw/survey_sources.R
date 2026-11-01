# Exploratory coverage survey of njlea's data sources. Not part of any build.
#
# Run from the package root (njlea/njlea): Rscript data-raw/survey_sources.R
# Needs dplyr, njmunicipalities, njoaguof (all in njlea's renv) and the pdftotext CLI
# (poppler-utils). Reads the seed, not data-raw/registry/; kept as the record of the survey.
#
# Prints a report; writes nothing except optional CSVs to tempdir().

options(width = 220)
suppressPackageStartupMessages({
  library(dplyr)
  library(njmunicipalities)
})

src <- function(...) file.path("data-raw", "sources", ...)
section <- function(title) cat("\n", strrep("=", 78), "\n", title, "\n", strrep("=", 78), "\n", sep = "")
show <- function(df, n = 100) print(as.data.frame(df), row.names = FALSE, max = n * max(1, ncol(df)))
norm <- function(x) {
  x <- tolower(x)
  x <- gsub("[.'’,]", "", x)
  x <- gsub("\\btwp\\b|\\btwsp\\b", "township", x)
  x <- gsub("\\bboro?\\b", "borough", x)
  x <- gsub("\\bco\\b", "county", x)
  x <- gsub("\\bmt\\b", "mount", x)
  x <- gsub("\\bpt\\b", "point", x)
  x <- gsub("\\bcrk\\b", "creek", x)
  # One-word spellings in the seed registry
  x <- sub("^bayhead\\b", "bay head", x)
  x <- sub("^oceangate\\b", "ocean gate", x)
  x <- sub("^peapack[- ]gladstone\\b", "peapack and gladstone", x)
  x <- gsub("\\bdept\\b", "department", x)
  x <- gsub("-", " ", x)
  gsub("\\s+", " ", trimws(x))
}
# Looser key: drop municipal-type words, "PD"/"Dept"/"Office", and Major Discipline's
# " - X County" disambiguation suffix. Used only to suggest candidates, never to resolve.
loose <- function(x) {
  x <- norm(sub(" - [A-Za-z ]+ County$", "", x))
  x <- gsub("\\b(township|borough|village|city|town|pd|police|department|office|of|the)\\b", "", x)
  gsub("\\s+", " ", trimws(x))
}
with_county <- function(x) ifelse(grepl(" County$", x) | is.na(x), x, paste(x, "County"))
pdf_text <- function(f) system2("pdftotext", c("-layout", shQuote(f), "-"), stdout = TRUE)

# ---- Load -----------------------------------------------------------------------------------

registry <- read.csv(src("seed_njoagleod", "agency_registry.csv"), colClasses = "character", na.strings = "")
alias    <- read.csv(src("seed_njoagleod", "agency_alias.csv"), colClasses = "character", na.strings = "")
agmuni   <- read.csv(src("seed_njoagleod", "agency_municipality.csv"), colClasses = "character", na.strings = "")
agency   <- registry |> group_by(agency_id) |> slice_max(first_year, n = 1) |> ungroup()

cde <- read.csv(list.files(src("fbi_cde"), "^agencies_NJ_.*\\.csv$", full.names = TRUE) |> max(),
                colClasses = "character")

# The committed agency extract of the Major Discipline xlsx (the xlsx names officers and is
# not committed; see data-raw/sources/major_discipline/README.md).
md <- read.csv(src("major_discipline", "major_discipline_agencies.csv"),
               colClasses = "character", na.strings = "", encoding = "UTF-8") |>
  distinct(County, Agency)

uof <- njoaguof::incident |>
  distinct(agency_county = as.character(agency_county), agency_name = as.character(agency_name))

munis <- get_municipalities(2020, geoid_year = 2000) |> rename(GEOID_Y2K = GEOID)
county_codes <- counties |> arrange(county) |>
  mutate(cc = sprintf("%02d", row_number()))   # NJ state county codes are alphabetical, 01-21

# Treasury NJ-1040 county/municipality codes: up to four "Name  CCMM" pairs per line.
muni_lines <- pdf_text(src("nj_muni_codes", "county-muni-codes.pdf"))
# Names are words joined by single spaces, so a match can't swallow a neighbouring column
# or a county header.
m <- regmatches(muni_lines, gregexpr("[A-Z][A-Za-z.'&-]*(?: [A-Za-z.'&-]+)*\\s+[0-2][0-9]{3}\\b",
                                     muni_lines, perl = TRUE))
muni_codes <- tibble(x = trimws(unlist(m))) |>
  transmute(treasury_name = trimws(sub("\\s+[0-9]{4}$", "", x)), ccmm = sub(".*\\s", "", x)) |>
  filter(!grepl("COUNTY|Code", treasury_name)) |>
  distinct() |>
  mutate(cc = substr(ccmm, 1, 2)) |>
  left_join(county_codes |> select(cc, county), by = "cc")

# NJSP monthly bias incident report: agency name, ORI, municipality.
bias_lines <- pdf_text(src("njsp_ucr", "20250709_Monthly_Bias_Incident_Report.pdf"))
bias <- tibble(x = grep("\\bNJ[0-9A-Z]{7}\\b", bias_lines, value = TRUE)) |>
  transmute(bias_name = trimws(sub("\\s+NJ[0-9A-Z]{7}.*", "", x)),
            ori = regmatches(x, regexpr("NJ[0-9A-Z]{7}", x))) |>
  distinct(ori, .keep_all = TRUE)

section("Sources loaded")
cat(sprintf("registry: %d agencies (%s)\n", n_distinct(registry$agency_id),
            paste(names(table(agency$agency_type)), table(agency$agency_type), collapse = ", ")))
cat(sprintf("aliases by source: %s\n", paste(names(table(alias$source)), table(alias$source), collapse = ", ")))
cat(sprintf("njoaguof agencies: %d   Major Discipline agencies: %d\n", nrow(uof), nrow(md)))
cat(sprintf("CDE ORIs: %d   bias-report ORIs: %d   Treasury muni codes: %d   munis 2020: %d\n",
            nrow(cde), nrow(bias), nrow(muni_codes), nrow(munis)))

# ---- 1. Alias resolution --------------------------------------------------------------------

section("1a. njoaguof agencies without a seed alias (source = njoaguof)")
uof_missing <- uof |> anti_join(filter(alias, source == "njoaguof"), by = c("agency_county", "agency_name"))
show(uof_missing)

section("1b. Major Discipline agencies: resolution against seed aliases")
alias_n <- alias |>
  mutate(county_n = norm(with_county(agency_county)), name_n = norm(agency_name)) |>
  distinct(county_n, name_n, agency_id)
md_res <- md |>
  mutate(county_n = norm(with_county(County)), name_n = norm(Agency)) |>
  left_join(alias_n, by = c("county_n", "name_n"), relationship = "many-to-many") |>
  group_by(County, Agency) |>
  summarise(n_ids = n_distinct(agency_id, na.rm = TRUE), agency_id = first(agency_id), .groups = "drop")
cat(sprintf("resolved to one agency_id after normalisation: %d; ambiguous: %d; unresolved: %d\n",
            sum(md_res$n_ids == 1), sum(md_res$n_ids > 1), sum(md_res$n_ids == 0)))
cat("\nUnresolved, with loose-key candidates from the registry (same county):\n")
reg_loose <- agency |> transmute(county_n = norm(agency_county), loose = loose(agency_name),
                                 candidate = paste(agency_id, agency_name))
show(md_res |> filter(n_ids == 0) |>
       mutate(county_n = norm(with_county(County)), loose = loose(Agency)) |>
       left_join(reg_loose, by = c("county_n", "loose"), relationship = "many-to-many") |>
       select(County, Agency, candidate) |> arrange(County, Agency))
if (any(md_res$n_ids > 1)) { cat("\nAmbiguous:\n"); show(filter(md_res, n_ids > 1)) }

# ---- 2. Municipal codes <-> GEOID_Y2K -------------------------------------------------------

section("2. Treasury CCMM codes matched to njmunicipalities GEOID_Y2K (2020)")
# Census names carry a type suffix ("Atlantic City city"); Treasury names usually include
# the type once ("Absecon City") but sometimes not ("Atlantic City"). Match on the full
# name first, then on the Census name minus its last word.
census <- munis |> mutate(name_n = norm(municipality), name_short = sub(" [a-z]+$", "", name_n))
# Hand overrides where the Treasury name differs from the Census name beyond abbreviation.
treasury_overrides <- c("0717" = "city of orange township", "0706" = "essex fells borough",
                        "1815" = "peapack and gladstone borough")
treas <- muni_codes |>
  mutate(name_n = ifelse(ccmm %in% names(treasury_overrides), treasury_overrides[ccmm], norm(treasury_name)))
pass1 <- inner_join(treas, census |> select(-name_short), by = c("county", "name_n"))
pass2 <- treas |> anti_join(pass1, by = "ccmm") |>
  inner_join(census |> anti_join(pass1, by = "GEOID_Y2K") |> select(-name_n),
             by = c("county", "name_n" = "name_short"))
muni_xwalk <- bind_rows(pass1, pass2) |>
  full_join(treas |> select(ccmm, t2 = treasury_name), by = "ccmm") |>
  mutate(treasury_name = coalesce(treasury_name, t2)) |> select(-t2) |>
  full_join(census |> select(GEOID_Y2K, m2 = municipality, c2 = county), by = "GEOID_Y2K") |>
  mutate(municipality = coalesce(municipality, m2), county = coalesce(county, c2)) |> select(-m2, -c2)
dups <- muni_xwalk |> filter(!is.na(ccmm), !is.na(GEOID_Y2K)) |>
  filter(duplicated(ccmm) | duplicated(GEOID_Y2K))
stopifnot(nrow(dups) == 0)
cat(sprintf("matched: %d; Treasury-only: %d; njmunicipalities-only: %d\n",
            sum(!is.na(muni_xwalk$ccmm) & !is.na(muni_xwalk$GEOID_Y2K)),
            sum(is.na(muni_xwalk$GEOID_Y2K)), sum(is.na(muni_xwalk$ccmm))))
show(muni_xwalk |> filter(is.na(ccmm) | is.na(GEOID_Y2K)) |>
       select(county, treasury_name, ccmm, municipality, GEOID_Y2K) |> arrange(county))
muni_xwalk <- muni_xwalk |> filter(!is.na(ccmm), !is.na(GEOID_Y2K)) |>
  select(ccmm, GEOID_Y2K, county, municipality)

# ---- 3. ORIs --------------------------------------------------------------------------------

section("3a. CDE agency types and ORI shapes")
cde <- cde |> mutate(cc = substr(ori, 4, 5), mm = substr(ori, 6, 7), tail2 = substr(ori, 8, 9))
show(cde |> count(agency_type_name, numeric_cc = grepl("^[0-9]{2}$", cc), tail2 = tail2 == "00"))

section("3b. Municipal agencies: ORI derived from CCMM vs. CDE and the bias report")
muni_agency <- agency |> filter(agency_type %in% c("Municipal", "Multiple Municipalities")) |>
  inner_join(agmuni |> filter(note == "home municipality") |> distinct(agency_id, GEOID_Y2K), by = "agency_id") |>
  left_join(muni_xwalk |> select(GEOID_Y2K, ccmm), by = "GEOID_Y2K") |>
  mutate(ori = ifelse(is.na(ccmm), NA, paste0("NJ0", ccmm, "00"))) |>
  left_join(cde |> select(ori, cde_name = agency_name, agency_type_name), by = "ori") |>
  left_join(bias, by = "ori")
cat(sprintf("Municipal + Multiple Municipalities agencies: %d; with derived ORI: %d; ORI in CDE: %d (CDE type City: %d); in bias report: %d\n",
            nrow(muni_agency), sum(!is.na(muni_agency$ori)), sum(!is.na(muni_agency$cde_name)),
            sum(muni_agency$agency_type_name %in% "City"), sum(!is.na(muni_agency$bias_name))))
cat("\nDerived ORI absent from CDE:\n")
show(muni_agency |> filter(is.na(cde_name)) |> select(agency_id, agency_county, agency_name, ori))
cat("\nSpot check: agency name vs. CDE name vs. bias-report name (first 15 in all three):\n")
show(muni_agency |> filter(!is.na(cde_name), !is.na(bias_name)) |>
       select(agency_name, ori, cde_name, bias_name) |> head(15))
set.seed(1)
cat("\nRandom 15 CDE matches, to eyeball name agreement:\n")
show(muni_agency |> filter(!is.na(cde_name)) |> slice_sample(n = 15) |> select(agency_name, ori, cde_name))

section("3b'. Does the CCMM-derived ORI name the same agency? Name-based match as the alternative")
# CDE names: "X Township Police Department, Y County" etc. Compare loose keys.
cde_loose <- cde |> transmute(ori, cde_name = agency_name, agency_type_name,
                              county_n = norm(with_county(tools::toTitleCase(tolower(counties)))),
                              loose = loose(sub(",.*$", "", agency_name)))
bias_loose <- bias |> transmute(bias_ori = ori, loose = loose(bias_name))
# Two keys: key_t keeps "township" (Berlin Boro vs Berlin Twp), key_l drops it (the registry
# often omits "Twp" where CDE has "Township"). Use key_t, else key_l if that is unique.
key_t <- function(x) {
  x <- norm(sub(" - [A-Za-z ]+ County$", "", sub(",.*$", "", x)))
  x <- gsub("\\b(borough|village|city|town|pd|police|department|office|of|the)\\b", "", x)
  gsub("\\s+", " ", trimws(x))
}
cde_city <- cde |> filter(agency_type_name == "City") |>
  transmute(name_ori = ori, county_n = norm(with_county(tools::toTitleCase(tolower(counties)))),
            kt = key_t(agency_name), kl = loose(sub(",.*$", "", agency_name)))
uniq_by <- function(df, k) df |> group_by(county_n, .data[[k]]) |> filter(n() == 1) |> ungroup()
ma <- muni_agency |>
  mutate(county_n = norm(agency_county), kt = key_t(agency_name), kl = loose(agency_name),
         derived_ok = !is.na(cde_name) & key_t(cde_name) == kt | (!is.na(cde_name) & loose(sub(",.*$", "", cde_name)) == kl))
ma <- ma |>
  left_join(uniq_by(cde_city, "kt") |> select(county_n, kt, ori_t = name_ori), by = c("county_n", "kt")) |>
  left_join(uniq_by(cde_city, "kl") |> select(county_n, kl, ori_l = name_ori), by = c("county_n", "kl")) |>
  mutate(name_ori = coalesce(ori_t, ori_l), n_name = as.integer(!is.na(name_ori))) |>
  left_join(bias |> select(name_ori = ori, bias_at_name = bias_name), by = "name_ori")
cat(sprintf("Derived ORI's CDE name agrees with registry name: %d of %d\n", sum(ma$derived_ok), nrow(ma)))
cat(sprintf("Unique name match in CDE (same county, City type): %d; none: %d\n",
            sum(ma$n_name == 1), sum(ma$n_name == 0)))
cat(sprintf("Name-matched ORI also in bias report: %d; of those, bias name agrees (key_t): %d\n",
            sum(!is.na(ma$bias_at_name)), sum(key_t(ma$bias_at_name) == ma$kt | loose(ma$bias_at_name) == ma$kl, na.rm = TRUE)))
cat(sprintf("Where both exist, derived ORI == name-matched ORI: %d of %d\n",
            sum(ma$ori == ma$name_ori, na.rm = TRUE), sum(!is.na(ma$name_ori))))
cat("\nDerived-vs-name disagreement by county (count of agencies where derived ORI != name-matched ORI):\n")
show(ma |> filter(!is.na(name_ori), ori != name_ori) |> count(agency_county))
cat("\nExamples of disagreement:\n")
show(ma |> filter(!is.na(name_ori), ori != name_ori) |>
       select(agency_county, agency_name, ccmm, derived_ori = ori, cde_at_derived = cde_name, name_ori, bias_at_name), n = 200)
cat("\nNo unique CDE name match:\n")
show(ma |> filter(n_name != 1) |> select(agency_county, agency_name, ori, cde_name, n_name))

section("3c. CDE ORIs with no registry agency")
# Uses the name-matched ORIs from 3b'; the CCMM-derived ORI is not reliable (see 3b').
claimed <- na.omit(ma$name_ori)
cde_city_unclaimed <- cde |> filter(agency_type_name == "City", !ori %in% claimed)
cat("City-type ORIs not claimed (by name) by a registry Municipal agency:\n")
show(cde_city_unclaimed |> select(ori, agency_name, counties, is_nibrs, nibrs_start_date))
cat("\nNon-City CDE ORIs (match by hand or rule: sheriff NJ0cc0000, prosecutor NJ0cc8900, NJSP per county):\n")
show(cde |> filter(agency_type_name != "City") |> select(ori, agency_name, agency_type_name, counties) |>
       arrange(agency_type_name, ori), n = 120)

section("3d. Registry non-Municipal agencies (need hand-assigned ORIs)")
show(agency |> filter(agency_type != "Municipal") |> select(agency_id, agency_county, agency_name, agency_type) |>
       arrange(agency_type, agency_county), n = 120)

# ---- 4. Municipality coverage ---------------------------------------------------------------

section("4. Municipalities (2020) with no registry agency link")
linked <- unique(agmuni$GEOID_Y2K)
uncovered <- munis |> filter(!GEOID_Y2K %in% linked)
cat(sprintf("%d of %d municipalities have no linked agency (NJSP coverage, shared services, or a gap).\n",
            nrow(uncovered), nrow(munis)))
cat("Of these, named by an unclaimed CDE City agency (an agency the registry may lack):\n")
show(uncovered |> mutate(county_n = norm(county), kt = key_t(municipality), kl = loose(municipality)) |>
       inner_join(cde_city_unclaimed |> transmute(ori, agency_name, kl = loose(sub(",.*$", "", agency_name)),
                                                  county_n = norm(with_county(tools::toTitleCase(tolower(counties))))),
                  by = c("county_n", "kl")) |>
       select(county, municipality, GEOID_Y2K, ori, agency_name))
cat("\nAll uncovered municipalities:\n")
show(uncovered |> select(county, municipality, GEOID_Y2K), n = 150)

section("5. Municipality changes inside 2020+")
cat("Pine Hill (", PINE_HILL_BORO_GEOID, ") and Pine Valley (", PINE_VALLEY_BORO_GEOID, ") links:\n", sep = "")
show(agmuni |> filter(GEOID_Y2K %in% c(PINE_HILL_BORO_GEOID, PINE_VALLEY_BORO_GEOID)))
cat("Princeton (Y2K borough ", PRINCETON_BORO_GEOID, ", township ", PRINCETON_TWP_GEOID, ") links:\n", sep = "")
show(agmuni |> filter(GEOID_Y2K %in% c(PRINCETON_BORO_GEOID, PRINCETON_TWP_GEOID)))
cat("Pine Valley/Princeton in the 2020 municipality list:\n")
show(munis |> filter(grepl("Pine Valley|Princeton", municipality)))

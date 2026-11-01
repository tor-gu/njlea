# Build the package data from the hand-maintained registry CSVs.
#
#   data-raw/registry/*.csv  ->  build checks  ->  data/*.rda
#
# The build checks (SCHEMA.md section 4) need the raw snapshots in data-raw/sources/, which
# aren't shipped, so they run here rather than in the package tests:
#   - every ORI is in a committed CDE snapshot
#   - where the NJSP bias report names an ORI, its name agrees with the agency's
#   - every name-matched ORI is still the unique match for the agency's canonical name
# Also reports municipal names that differ from the CDE name of their ORI (decision 10).
#
# Run from the package root: Rscript data-raw/build_registry.R
# Needs the pdftotext CLI (poppler-utils) for the bias report.

options(width = 200)
suppressPackageStartupMessages(library(dplyr))
source("R/keys.R")
source("R/constants.R")

reg <- function(name) {
  read.csv(file.path("data-raw", "registry", paste0(name, ".csv")),
           colClasses = "character", na.strings = "", encoding = "UTF-8")
}
src <- function(...) file.path("data-raw", "sources", ...)
show <- function(df) print(as.data.frame(df), row.names = FALSE)
as_factor_checked <- function(x, levels) {
  bad <- setdiff(unique(x), levels)
  if (length(bad)) stop("unknown factor values: ", paste(bad, collapse = ", "))
  factor(x, levels)
}
years <- function(df) mutate(df, first_year = as.integer(first_year),
                             final_year = as.integer(final_year))

# ---- Read and type --------------------------------------------------------------------------

agency <- reg("agency") |>
  years() |>
  mutate(agency_level = as_factor_checked(agency_level, AGENCY_LEVELS),
         agency_function = as_factor_checked(agency_function, AGENCY_FUNCTIONS)) |>
  tibble::as_tibble()

agency_alias <- reg("agency_alias") |> years() |> tibble::as_tibble()

agency_municipality <- reg("agency_municipality") |>
  years() |>
  mutate(link_type = as_factor_checked(link_type, LINK_TYPES)) |>
  select(-municipality) |>   # readability column; not shipped
  tibble::as_tibble()

agency_ori <- reg("agency_ori") |>
  years() |>
  mutate(match = as_factor_checked(match, ORI_MATCHES)) |>
  tibble::as_tibble()

alias_source <- reg("alias_source") |>
  mutate(retrieved = as.Date(retrieved)) |>
  tibble::as_tibble()

# ---- Build checks ---------------------------------------------------------------------------

failures <- character()
fail <- function(msg, df = NULL) {
  cat("\nFAIL:", msg, "\n")
  if (!is.null(df)) show(df)
  failures <<- c(failures, msg)
}

cde_files <- list.files(src("fbi_cde"), "^agencies_[A-Z]{2}_[0-9]{8}\\.csv$", full.names = TRUE)
cde_all <- bind_rows(lapply(cde_files, function(f)
  read.csv(f, colClasses = "character") |> mutate(file = basename(f))))
# Latest snapshot per state
cde <- cde_all |>
  mutate(state = substr(file, 10, 11)) |>
  group_by(state) |>
  filter(file == max(file)) |>
  ungroup()

# 1. Every ORI is in a committed CDE snapshot (rules out ORIs computed from CCMM codes).
missing_ori <- agency_ori |> filter(!ori %in% cde_all$ori)
if (nrow(missing_ori)) fail("ORIs not in any CDE snapshot", missing_ori)

# 2. NJSP bias report: where it names an ORI, the name agrees (loose keys) with the agency's
#    canonical name or one of its source spellings.
pdf_text <- function(f) system2("pdftotext", c("-layout", shQuote(f), "-"), stdout = TRUE)
bias_lines <- unlist(lapply(list.files(src("njsp_ucr"), "\\.pdf$", full.names = TRUE), pdf_text))
bias <- tibble(x = grep("\\bNJ[0-9A-Z]{7}\\b", bias_lines, value = TRUE)) |>
  transmute(bias_name = trimws(sub("\\s+NJ[0-9A-Z]{7}.*", "", x)),
            ori = regmatches(x, regexpr("NJ[0-9A-Z]{7}", x))) |>
  filter(bias_name != "") |>   # blank where the PDF wraps a line
  distinct(ori, .keep_all = TRUE)
names_of <- bind_rows(agency |> select(agency_id, nm = agency_name),
                      agency_alias |> select(agency_id, nm = source_name)) |>
  mutate(kt = township_key(nm), kl = loose_key(nm)) |>
  distinct(agency_id, kt, kl)
bias_chk <- agency_ori |>
  inner_join(bias, by = "ori") |>
  mutate(kt = township_key(bias_name), kl = loose_key(bias_name))
bias_ok <- bind_rows(semi_join(bias_chk, names_of, by = c("agency_id", "kt")),
                     semi_join(bias_chk, names_of, by = c("agency_id", "kl"))) |>
  distinct(ori)
# Same agency under a short or campus name; not a disagreement.
bias_exceptions <- tribble(
  ~ori,        ~reason,
  "NJNSP2200", "NJSP's statewide ORI, reported as 'STATE POLICE'",
  "NJ0073000", "Rutgers Newark campus ORI, reported as 'RUTGERS NEWARK CAMPUS'",
  "NJ0123000", "Rutgers New Brunswick ORI; the bias name is cut off by a PDF line wrap"
)
bias_bad <- bias_chk |> anti_join(bias_ok, by = "ori") |> anti_join(bias_exceptions, by = "ori")
cat(sprintf("bias report: %d ORIs checked, %d agree, %d documented exceptions\n",
            nrow(bias_chk), nrow(bias_ok), sum(bias_exceptions$ori %in% bias_chk$ori)))
if (nrow(bias_bad)) fail("bias report names disagree", bias_bad |> select(agency_id, ori, bias_name))

# 3. Name-matched ORIs are still the unique same-county match for the canonical name.
cde_city <- cde |>
  filter(state == "NJ", agency_type_name == "City") |>
  transmute(ori, county = county_name(counties), short = sub(",.*$", "", agency_name),
            kt = township_key(short), kl = loose_key(short))
uniq <- function(k) cde_city |> group_by(county, .data[[k]]) |> filter(n() == 1) |>
  ungroup() |> select(county, all_of(k), ori)
rematch <- agency_ori |>
  filter(match == "Name") |>
  inner_join(agency |> select(agency_id, agency_name, county), by = "agency_id") |>
  mutate(kt = township_key(agency_name), kl = loose_key(agency_name)) |>
  left_join(uniq("kt") |> rename(ori_t = ori), by = c("county", "kt")) |>
  left_join(uniq("kl") |> rename(ori_l = ori), by = c("county", "kl")) |>
  mutate(ori_now = coalesce(ori_t, ori_l))
rematch_bad <- rematch |> filter(is.na(ori_now) | ori_now != ori)
if (nrow(rematch_bad)) fail("name-matched ORIs no longer match uniquely",
                            rematch_bad |> select(agency_id, agency_name, ori, ori_now))

# Report: municipal names vs. the CDE name of their ORI. Known differences (decision 10):
# CDE's ", X County" suffix, Princeton's double space, "Carney's Point".
cde_name <- setNames(cde$agency_name, cde$ori)
known <- function(ours, theirs) {
  theirs <- sub(", [A-Za-z ]+ County$", "", theirs)
  theirs <- gsub("\\s+", " ", theirs)
  ours == theirs | ours == sub("Carney's", "Carneys", theirs)
}
muni_diff <- agency |>
  filter(agency_level == "Municipal") |>
  inner_join(agency_ori |> select(agency_id, ori), by = "agency_id") |>
  mutate(cde_name = cde_name[ori]) |>
  filter(agency_name != cde_name)
unexpected <- muni_diff |> filter(!known(agency_name, cde_name))
cat(sprintf("municipal names differing from CDE: %d (%d beyond the known artefacts)\n",
            nrow(muni_diff), nrow(unexpected)))
if (nrow(unexpected)) { cat("Review:\n"); show(unexpected |> select(agency_id, agency_name, cde_name)) }

if (length(failures)) stop(length(failures), " build check(s) failed; data/ not written")

# ---- Write ----------------------------------------------------------------------------------

usethis::use_data(agency, agency_alias, agency_municipality, agency_ori, alias_source,
                  overwrite = TRUE)

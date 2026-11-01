# One-off migration: njoagleod's seed registry -> data-raw/registry/*.csv.
#
# The registry CSVs are the hand-maintained source of truth (since 2026-11-01); this script is
# kept only as a record of how they were made (SCHEMA.md section 6). It implements
# design decisions 1-9 in TODO.md:
#   - level x function (decision 9 + SCHEMA section 2), county rule (decision 9)
#   - canonical names (SCHEMA section 5); the seed's names survive as njoag-leod-* aliases
#   - years reset to 2020 / NA (decision 8)
#   - Home/Service link types; fixes for Chester PD and Cherry Hill Campus Police
#   - the 15 corrections agencies and their njoaguof and Major Discipline aliases (decision 4)
#   - every Major Discipline spelling as an alias
#   - ORIs: name match for municipal agencies, a hand table for the rest (decision 5)
#
# Run from the package root: Rscript data-raw/migrate_seed.R
# Reads the Major Discipline agency extract (County, Agency, Year), not the xlsx, which names
# officers. (The original 2026-11-01 run read the xlsx; the extract has the same spellings.)

# Since 2026-11-01 the registry CSVs are hand-maintained, so this script would overwrite
# edits. It refuses unless NJLEA_REMIGRATE=1 (e.g. to reproduce the migration elsewhere).
if (file.exists(file.path("data-raw", "registry", "agency.csv")) &&
    Sys.getenv("NJLEA_REMIGRATE") != "1") {
  stop("data-raw/registry/ already exists and is hand-maintained; set NJLEA_REMIGRATE=1 to ",
       "overwrite it with the original migration")
}

options(width = 200)
suppressPackageStartupMessages({
  library(dplyr)
  library(njmunicipalities)
})
source("R/keys.R")

src <- function(...) file.path("data-raw", "sources", ...)
read_chr <- function(f) read.csv(f, colClasses = "character", na.strings = "", check.names = FALSE)
show <- function(df, n = 200) print(as.data.frame(df), row.names = FALSE, max = n * max(1, ncol(df)))
section <- function(title) cat("\n==== ", title, " ", strrep("=", max(0, 70 - nchar(title))), "\n", sep = "")
UNVERIFIED <- "canonical name: proposed, unverified"
# Vectorised over both arguments (a length-1 `note` used to be recycled wrongly by ifelse()).
add_note <- function(note, text) {
  note <- rep_len(note, length(text))
  ifelse(is.na(note) | note == "", text,
         ifelse(is.na(text) | text == "", note, paste0(note, "; ", text)))
}

# ---- Load -----------------------------------------------------------------------------------

seed_reg   <- read_chr(src("seed_njoagleod", "agency_registry.csv"))
seed_alias <- read_chr(src("seed_njoagleod", "agency_alias.csv"))
seed_muni  <- read_chr(src("seed_njoagleod", "agency_municipality.csv"))
leod21     <- read_chr(src("leod_2021", "2021_NJOAGLEOD_Agency_Information.csv"))
cde_file   <- max(list.files(src("fbi_cde"), "^agencies_NJ_.*\\.csv$", full.names = TRUE))
cde        <- read_chr(cde_file)
cde_de     <- read_chr(max(list.files(src("fbi_cde"), "^agencies_DE_.*\\.csv$", full.names = TRUE)))
# The committed agency extract of the Major Discipline xlsx (the xlsx names officers and is
# not committed; see data-raw/sources/major_discipline/README.md).
md <- read.csv(src("major_discipline", "major_discipline_agencies.csv"),
               colClasses = "character", na.strings = "", encoding = "UTF-8") |>
  distinct(County, Agency)
uof <- njoaguof::incident |>
  distinct(agency_county = as.character(agency_county), agency_name = as.character(agency_name))
munis <- get_municipalities(2020, geoid_year = 2000) |>
  transmute(GEOID_Y2K = GEOID, muni_county = county, municipality)
geoid_of <- function(name, cty) {
  g <- munis$GEOID_Y2K[munis$municipality == name & munis$muni_county == cty]
  stopifnot(length(g) == 1)
  g
}

stopifnot(!anyDuplicated(seed_reg$agency_id))
stopifnot(!anyDuplicated(seed_alias[c("source", "agency_county", "agency_name")]))

# ---- New agencies: corrections (decision 4) -------------------------------------------------

corr_counties <- c("Atlantic", "Burlington", "Camden", "Cumberland", "Essex", "Hudson",
                   "Middlesex", "Monmouth", "Morris", "Ocean", "Salem", "Somerset", "Warren")
new_agency <- bind_rows(
  tibble(agency_id = "A0533", agency_name = "New Jersey Department of Corrections",
         county = NA_character_, agency_level = "State", agency_function = "Corrections",
         note = UNVERIFIED),
  tibble(agency_id = "A0534", agency_name = "New Jersey State Parole Board",
         county = NA_character_, agency_level = "State", agency_function = "Corrections",
         note = UNVERIFIED),
  tibble(agency_id = sprintf("A%04d", 534 + seq_along(corr_counties)),
         county = paste(corr_counties, "County"),
         agency_name = paste(county, "Department of Corrections"),
         agency_level = "County", agency_function = "Corrections", note = NA_character_)
) |>
  # Official names checked in the user's name review (2026-11-01; SCHEMA open question 2).
  mutate(
    agency_name = case_when(
      county == "Atlantic County" ~ "Atlantic County Division of Adult Detention",
      county == "Salem County" ~ "Salem County Correctional Facility",
      .default = agency_name),
    note = case_when(
      county == "Atlantic County" ~
        paste("Name as on the Atlantic County website (a division of the Department of Public",
              "Safety; Major Discipline writes 'Atlantic County Public Safety Division of Adult",
              "Detention'). Source URL to add"),
      county == "Salem County" ~
        paste("Operated by the Salem County Sheriff's Office (A0449) as its corrections",
              "component; the sheriff's site calls it the Salem County Correctional Facility",
              "(https://www.salemcountysheriff.com/corrections/)"),
      .default = note))
corr_id <- setNames(new_agency$agency_id, coalesce(new_agency$county, new_agency$agency_name))

# ---- Level, function and canonical names ----------------------------------------------------

# Non-municipal seed agencies other than sheriffs and prosecutors. Names are proposals
# (SCHEMA section 5: the agency's official name); level per decision 9.
hand <- tribble(
  ~agency_id, ~agency_name,                                              ~agency_level,     ~agency_function,
  "A0097",    "Burlington County Bridge Commission Police",              "County",          "Authority or transit",
  "A0133",    "Camden County Police Department",                         "County",          "Police",
  "A0188",    "Essex County College Police Department",                  "County",          "College or university",
  "A0285",    "Middlesex County College Police Department",              "County",          "College or university",
  "A0312",    "Brookdale Community College Police Department",           "County",          "College or university",
  "A0374",    "Morris County Park Police Department",                    "County",          "Park and conservation",
  "A0505",    "Union County Police Department",                          "County",          "Police",
  "A0066",    "Palisades Interstate Parkway Police Department",          "Bi-state",        "Authority or transit",
  "A0139",    "Delaware River Port Authority Police Department",         "Bi-state",        "Authority or transit",
  "A0523",    "Delaware River and Bay Authority Police",                 "Bi-state",        "Authority or transit",
  "A0134",    "Cherry Hill Campus Police",                               "School district", "K-12 school",
  "A0241",    "Stevens Institute of Technology Police Department",       "Private",         "College or university",
  "A0272",    "Princeton University Department of Public Safety",        "Private",         "College or university",
  "A0336",    "Monmouth University Police Department",                   "Private",         "College or university",
  "A0018",    "Stockton University Police Department",                   "State",           "College or university",
  "A0197",    "Montclair State University Police Department",            "State",           "College or university",
  "A0198",    "New Jersey Institute of Technology Police Department",    "State",           "College or university",
  "A0223",    "Rowan University Police Department",                      "State",           "College or university",
  "A0261",    "The College of New Jersey Police Department",             "State",           "College or university",
  "A0441",    "William Paterson University Police Department",           "State",           "College or university",
  "A0493",    "Kean University Police Department",                       "State",           "College or university",
  "A0531",    "Rutgers University Police Department",                    "State",           "College or university",
  "A0524",    "New Jersey Department of Human Services Police",          "State",           "Police",
  "A0525",    "New Jersey Division of Criminal Justice",                 "State",           "Investigative",
  "A0526",    "New Jersey Division of Fish and Wildlife",                "State",           "Park and conservation",
  "A0527",    "New Jersey Division of Taxation",                         "State",           "Investigative",
  "A0528",    "New Jersey State Park Police",                            "State",           "Park and conservation",
  "A0529",    "New Jersey State Police",                                 "State",           "Police",
  "A0530",    "New Jersey Transit Police Department",                    "State",           "Authority or transit",
  "A0532",    "New Jersey Youth Justice Commission",                     "State",           "Corrections"
)
hand_notes <- c(
  A0134 = paste("Cherry Hill Public Schools' own force (Cherry Hill Board of Education); the",
                "district's boundary is Cherry Hill township's. Armed district campus officers",
                "since 2018; Cherry Hill Twp PD patrols around the schools",
                "(https://www.chclc.org/departments/security; sources in DATA_SOURCES.md)"),
  A0133 = paste("Replaced the Camden city police department in 2013 and is Camden city's",
                "local police (Service link)"),
  A0312 = paste("possible end; no documented end yet (decision 11) - no use of force;",
                "LEOD ends 2022"),
  A0374 = paste("Merged into the Morris County Sheriff's Office (A0373) on 2022-01-01",
                "(https://www.dailyrecord.com/story/news/2021/10/28/morris-county-park-police-vote-sheriffs-office/8580348002/)"),
  A0285 = "Check: the college may have been renamed Middlesex College; a dated rename opens a new epoch",
  A0505 = paste("The Union County Park Police became the Union County Police in 1978",
                "(https://ucnj.org/public-safety/division-of-police/department-history/)"),
  A0532 = paste("Formerly the Juvenile Justice Commission (LEOD 2021, njoaguof spellings).",
                "Rename date not yet documented; once it is, split into two epochs (decision 3)")
)
stopifnot(!anyDuplicated(hand$agency_id), all(hand$agency_id %in% seed_reg$agency_id))

# Municipal canonical names (decision 10, SCHEMA section 5): "<municipality> Police
# Department" from the Census name. Always keep "Township" except for 8 Essex townships known
# by bare names; drop borough/city/town/village unless needed to disambiguate within the
# county. This follows the FBI CDE convention.
type_words <- "(city|township|borough|town|village)"
bare_townships <- c("Belleville township", "Bloomfield township", "Irvington township",
                    "Montclair township", "City of Orange township",
                    "South Orange Village township", "Verona township", "West Orange township")
census_overrides <- c("City of Orange" = "Orange", "South Orange Village" = "South Orange")
muni_names <- munis |>
  mutate(base = sub(paste0("\\s+", type_words, "$"), "", municipality),
         type = ifelse(grepl(paste0("\\s", type_words, "$"), municipality),
                       sub(paste0(".*\\s", type_words, "$"), "\\1", municipality), NA),
         keep_twp = type %in% "township" & !municipality %in% bare_townships,
         name0 = ifelse(keep_twp, paste(base, "Township"), base)) |>
  group_by(muni_county) |>
  mutate(clash = name0 %in% name0[duplicated(name0)]) |>
  ungroup() |>
  mutate(name1 = ifelse(clash & !keep_twp & !is.na(type),
                        paste(base, tools::toTitleCase(type)), name0),
         name1 = coalesce(census_overrides[name1], name1),
         canon = paste(name1, "Police Department"))
stopifnot(all(bare_townships %in% munis$municipality))
# Every clash is resolved: no two municipalities in a county get the same name.
stopifnot(!anyDuplicated(muni_names[c("muni_county", "canon")]))

# Municipality links (needed for municipal names). Home/Service from the seed's notes.
links <- seed_muni |>
  transmute(agency_id, GEOID_Y2K,
            link_type = ifelse(note == "home municipality", "Home", "Service"),
            note = ifelse(note == "home municipality", NA, note))
# Chester PD (A0357) is a Chester Township department serving the borough by agreement.
chester_twp  <- geoid_of("Chester township", "Morris County")
chester_boro <- geoid_of("Chester borough", "Morris County")
chester_note <- links$note[links$agency_id == "A0357" & links$GEOID_Y2K == chester_twp]
links <- links |>
  mutate(
    link_type = case_when(agency_id == "A0357" & GEOID_Y2K == chester_twp ~ "Home",
                          agency_id == "A0357" & GEOID_Y2K == chester_boro ~ "Service",
                          .default = link_type),
    note = case_when(agency_id == "A0357" & GEOID_Y2K == chester_twp ~ NA_character_,
                     agency_id == "A0357" & GEOID_Y2K == chester_boro ~ chester_note,
                     .default = note))
camden_city <- geoid_of("Camden city", "Camden County")
stopifnot(!any(links$agency_id == "A0133"), !any(links$agency_id == "A0134"))
links <- bind_rows(links, tibble(
  agency_id = "A0133", GEOID_Y2K = camden_city, link_type = "Service",
  note = paste("Camden County Police Department has policed Camden city since 2013-05-01,",
               "replacing the city's own department",
               "(https://en.wikipedia.org/wiki/Camden_County_Police_Department)")))
home <- links |> filter(link_type == "Home")
stopifnot(!anyDuplicated(home$agency_id))

municipal_ids <- seed_reg$agency_id[seed_reg$agency_type %in% c("Municipal", "Multiple Municipalities")]
stopifnot(setequal(home$agency_id, municipal_ids))
municipal <- seed_reg |>
  filter(agency_id %in% municipal_ids) |>
  inner_join(home |> select(agency_id, GEOID_Y2K), by = "agency_id") |>
  inner_join(muni_names, by = "GEOID_Y2K") |>
  transmute(agency_id, agency_name = canon, county = muni_county, agency_level = "Municipal",
            agency_function = "Police", note = NA_character_, seed_county = agency_county)
bad_county <- municipal |> filter(county != seed_county)
if (nrow(bad_county)) { section("Municipal: seed county differs from Home county"); show(bad_county) }
stopifnot(nrow(bad_county) == 0)
municipal$seed_county <- NULL
POSSIBLE_END <- "possible end; no documented end yet (decision 11)"
municipal <- municipal |>
  mutate(note = case_when(
    agency_id == "A0433" ~   # Paterson
      paste("Under state (Attorney General) administration since March 2023;",
            "stays Municipal (decision 9)"),
    agency_id == "A0162" ~ paste(POSSIBLE_END, "- last use of force 2024-04-09; LEOD ends 2023"),
    agency_id == "A0228" ~ paste(POSSIBLE_END, "- last use of force 2023-07-02; LEOD ends 2022"),
    .default = note))
stopifnot(seed_reg$agency_name[seed_reg$agency_id == "A0433"] == "Paterson PD")

county_agency <- seed_reg |>
  filter(grepl("Sheriff|Prosecutor", agency_name)) |>
  transmute(agency_id, county = agency_county,
            agency_function = ifelse(grepl("Sheriff", agency_name), "Sheriff", "Prosecutor"),
            agency_name = paste(county, ifelse(agency_function == "Sheriff",
                                               "Sheriff's Office", "Prosecutor's Office")),
            agency_level = "County", note = UNVERIFIED) |>
  mutate(note = ifelse(agency_id == "A0373",
                       add_note(note, paste("Absorbed the Morris County Park Police (A0374) on",
                                            "2022-01-01")),
                       note))
stopifnot(nrow(county_agency) == 42, !anyDuplicated(county_agency$agency_name))

hand_agency <- hand |>
  left_join(seed_reg |> select(agency_id, agency_county), by = "agency_id") |>
  mutate(county = ifelse(agency_level %in% c("Municipal", "County", "School district"),
                         agency_county, NA_character_),
         note = add_note(UNVERIFIED, coalesce(hand_notes[agency_id], ""))) |>
  mutate(note = sub("; $", "", note)) |>
  select(-agency_county)

# Documented ends (decision 8: only these override the 2020 / NA reset).
documented_end <- c(A0374 = 2021L)   # Morris County Park PD, merged 2022-01-01
agency <- bind_rows(municipal, county_agency, hand_agency, new_agency) |>
  mutate(first_year = 2020L, final_year = unname(documented_end[agency_id])) |>
  select(agency_id, first_year, final_year, agency_name, county, agency_level,
         agency_function, note) |>
  arrange(agency_id)
# Name review by the user, completed 2026-11-01 (decision 11): every proposed name approved,
# with A0535 corrected above. The review marker comes off.
n_reviewed <- sum(grepl(UNVERIFIED, agency$note, fixed = TRUE))
agency <- agency |>
  mutate(note = sub(paste0("^", UNVERIFIED, "(; )?"), "", note), note = na_if(note, ""))
stopifnot(n_reviewed == 74)
stopifnot(nrow(agency) == nrow(seed_reg) + nrow(new_agency),
          setequal(agency$agency_id, c(seed_reg$agency_id, new_agency$agency_id)),
          !anyNA(agency[c("agency_name", "agency_level", "agency_function")]),
          !anyDuplicated(agency[c("county", "agency_name")]))

# ---- Aliases --------------------------------------------------------------------------------

leod21_type <- leod21 |> distinct(County, Agency, source_type = `Agency Type`)
stopifnot(!anyDuplicated(leod21_type[c("County", "Agency")]))
alias_seed <- seed_alias |>
  transmute(source, source_county = agency_county, source_name = agency_name, agency_id) |>
  left_join(leod21_type |> mutate(source = "njoag-leod-2021"),
            by = c("source", "source_county" = "County", "source_name" = "Agency"))

# njoaguof spellings of the corrections agencies (survey section 1a)
uof_new <- uof |>
  anti_join(seed_alias |> filter(source == "njoaguof"),
            by = c("agency_county", "agency_name")) |>
  mutate(agency_id = case_when(
    agency_name == "Department of Corrections" & is.na(agency_county) ~ corr_id["New Jersey Department of Corrections"],
    agency_name == "State Parole Board" & is.na(agency_county) ~ corr_id["New Jersey State Parole Board"],
    grepl("Corrections", agency_name) ~ corr_id[agency_county]))
stopifnot(nrow(uof_new) == 11, !anyNA(uof_new$agency_id))

# Major Discipline: every (County, Agency). First the normalised match against the seed's
# spellings, then the corrections agencies, then loose-key variants (unique same-county
# candidate), then a hand list.
alias_n <- seed_alias |>
  transmute(county_n = county_name(agency_county), name_n = norm_key(agency_name), agency_id) |>
  distinct()
md_key <- md |> mutate(county_n = county_name(County), name_n = norm_key(Agency))
md_exact <- md_key |>
  inner_join(alias_n, by = c("county_n", "name_n"), relationship = "many-to-many") |>
  distinct(County, Agency, agency_id)
stopifnot(!anyDuplicated(md_exact[c("County", "Agency")]))
md_rest <- md_key |> anti_join(md_exact, by = c("County", "Agency"))
md_corr <- md_rest |>
  filter(grepl("Corrections|Correctional|Detention|Parole", Agency)) |>
  mutate(agency_id = case_when(
    Agency == "NJ Department of Corrections" ~ corr_id["New Jersey Department of Corrections"],
    Agency == "NJ State Parole Board" ~ corr_id["New Jersey State Parole Board"],
    .default = corr_id[county_n]))
stopifnot(nrow(md_corr) == 15, !anyNA(md_corr$agency_id), !anyDuplicated(md_corr$agency_id))
md_hand <- tribble(
  ~County,                 ~Agency,                                      ~agency_id,
  "State or Other Agency", "NJ Division of Criminal Justice - Trenton", "A0525"
)
seed_loose <- seed_reg |> transmute(county_n = agency_county, loose = loose_key(agency_name), agency_id)
md_loose <- md_rest |>
  anti_join(md_corr, by = c("County", "Agency")) |>
  anti_join(md_hand, by = c("County", "Agency")) |>
  mutate(loose = loose_key(Agency)) |>
  left_join(seed_loose, by = c("county_n", "loose"), relationship = "many-to-many") |>
  group_by(County, Agency) |>
  filter(n_distinct(agency_id, na.rm = TRUE) == 1) |>
  ungroup()
# 27 variants (DATA_SOURCES.md once said 22; the survey listing has 27), each with one candidate.
stopifnot(nrow(md_loose) == 27)
md_alias <- bind_rows(md_exact, md_corr, md_hand, md_loose) |>
  distinct(County, Agency, agency_id)
stopifnot(nrow(md_alias) == nrow(md), !anyDuplicated(md_alias[c("County", "Agency")]))

agency_alias <- bind_rows(
  alias_seed,
  uof_new |> transmute(source = "njoaguof", source_county = agency_county,
                       source_name = agency_name, agency_id),
  md_alias |> transmute(source = "njoag-major-discipline", source_county = County,
                        source_name = Agency, agency_id)
) |>
  mutate(first_year = NA_integer_, final_year = NA_integer_) |>
  select(source, source_county, source_name, agency_id, first_year, final_year, source_type) |>
  arrange(source, agency_id, source_county, source_name)
stopifnot(!anyDuplicated(agency_alias[c("source", "source_county", "source_name")]),
          all(agency_alias$agency_id %in% agency$agency_id),
          all(agency$agency_id %in% agency_alias$agency_id))

# ---- Municipality links ---------------------------------------------------------------------

agency_municipality <- links |>
  left_join(munis |> select(GEOID_Y2K, municipality), by = "GEOID_Y2K") |>
  mutate(first_year = 2020L, final_year = NA_integer_) |>
  select(agency_id, GEOID_Y2K, municipality, first_year, final_year, link_type, note) |>
  arrange(agency_id, link_type, GEOID_Y2K)
stopifnot(!anyNA(agency_municipality$municipality),
          !anyDuplicated(agency_municipality[c("agency_id", "GEOID_Y2K")]),
          all(!is.na(agency_municipality$note[agency_municipality$link_type == "Service"])))

# ---- ORIs -----------------------------------------------------------------------------------

cde_names <- c(setNames(cde$agency_name, cde$ori), setNames(cde_de$agency_name, cde_de$ori))
cde_note <- function(o) {
  nm <- cde_names[o]
  stopifnot(!anyNA(nm))
  sprintf("CDE %s list: '%s'", substr(o, 1, 2), nm)
}

# Municipal agencies: unique same-county match against CDE City agencies, by the canonical
# name and by the seed name; township_key first, then loose_key where that is unique.
cde_city <- cde |>
  filter(agency_type_name == "City") |>
  transmute(ori, county = county_name(counties), short = sub(",.*$", "", agency_name),
            kt = township_key(short), kl = loose_key(short))
uniq <- function(k) cde_city |> group_by(county, .data[[k]]) |> filter(n() == 1) |> ungroup() |>
  select(county, all_of(k), ori)
name_match <- function(df, name_col) {
  df |>
    mutate(kt = township_key(.data[[name_col]]), kl = loose_key(.data[[name_col]])) |>
    left_join(uniq("kt") |> rename(ori_t = ori), by = c("county", "kt")) |>
    left_join(uniq("kl") |> rename(ori_l = ori), by = c("county", "kl")) |>
    transmute(agency_id, ori = coalesce(ori_t, ori_l))
}
muni_ag <- agency |> filter(agency_level == "Municipal") |>
  left_join(seed_reg |> select(agency_id, seed_name = agency_name), by = "agency_id")
by_canon <- name_match(muni_ag, "agency_name")
by_seed  <- name_match(muni_ag, "seed_name")
ori_cmp <- by_canon |> full_join(by_seed, by = "agency_id", suffix = c("_canon", "_seed"))
disagree <- ori_cmp |> filter(!is.na(ori_canon), !is.na(ori_seed), ori_canon != ori_seed)
ori_muni <- ori_cmp |>
  transmute(agency_id, ori = coalesce(ori_canon, ori_seed), match = "Name",
            note = ifelse(agency_id %in% disagree$agency_id,
                          paste0(cde_note(ori), "; matched on the canonical (Home-based) name; ",
                                 "the seed name matched another ORI"),
                          cde_note(ori)))
stopifnot(!anyNA(ori_muni$ori), !anyDuplicated(ori_muni$ori),
          identical(disagree$agency_id, "A0357"))   # Chester: the seed name is the borough's

county_code <- setNames(sprintf("%02d", seq_along(sort(njmunicipalities::counties$county))),
                        sort(njmunicipalities::counties$county))
ori_county <- county_agency |>
  transmute(agency_id, ori = paste0("NJ0", county_code[county],
                                    ifelse(agency_function == "Sheriff", "0000", "8900")))
geo_nb <- geoid_of("New Brunswick city", "Middlesex County")
geo_nk <- geoid_of("Newark city", "Essex County")
ori_hand <- tribble(
  ~agency_id, ~ori,        ~GEOID_Y2K,
  "A0018",    "NJ0012400", NA,
  "A0197",    "NJ0072800", NA,
  "A0198",    "NJ0072700", NA,
  "A0223",    "NJ0082600", NA,
  "A0261",    "NJ0111800", NA,
  "A0441",    "NJ0161700", NA,
  "A0493",    "NJ0202400", NA,
  "A0531",    "NJ0123000", geo_nb,
  "A0531",    "NJ0073000", geo_nk,
  "A0531",    "NJ0043900", camden_city,
  "A0188",    "NJ0073100", NA,
  "A0285",    "NJ0122600", NA,
  "A0312",    "NJ0135600", NA,
  "A0241",    "NJ009149E", NA,
  "A0272",    "NJ0115000", NA,
  "A0336",    "NJ013559E", NA,
  "A0097",    "NJ0034100", NA,
  "A0133",    "NJ0040800", NA,
  "A0374",    "NJ0149100", NA,
  "A0505",    "NJ0209100", NA,
  "A0066",    "NJNPIPP00", NA,
  "A0139",    "NJNDRPA00", NA,
  "A0523",    "DE0020100", NA,
  "A0524",    "NJ0111900", NA,
  "A0526",    "NJ0111700", NA,
  "A0528",    "NJ0225400", NA,
  "A0530",    "NJ0073200", NA,
  "A0533",    "NJ0225500", NA
)
ori_njsp <- cde |> filter(agency_type_name == "State Police") |> transmute(agency_id = "A0529", ori)
stopifnot(nrow(ori_njsp) == 22)
ori_other <- bind_rows(ori_county, ori_hand, ori_njsp) |>
  mutate(match = "Hand",
         note = cde_note(ori),
         note = ifelse(agency_id == "A0505",
                       paste(note, "(the old name; the Park Police became the Union County Police in 1978)"),
                       note))
agency_ori <- bind_rows(
  ori_muni |> mutate(GEOID_Y2K = NA_character_),
  ori_other
) |>
  # ORI years follow the agency's unless known otherwise (decision 5).
  mutate(first_year = 2020L, final_year = unname(documented_end[agency_id])) |>
  select(agency_id, ori, GEOID_Y2K, first_year, final_year, match, note) |>
  arrange(agency_id, ori)
stopifnot(!anyDuplicated(agency_ori$ori), all(agency_ori$agency_id %in% agency$agency_id),
          all(agency_ori$ori %in% c(cde$ori, cde_de$ori)))

# ---- Alias sources --------------------------------------------------------------------------

uof_dates <- range(as.Date(njoaguof::incident$incident_date_1), na.rm = TRUE)
alias_source <- tribble(
  ~source,                  ~title,                                                       ~url,                                                                   ~coverage,                 ~retrieved,
  "njoag-leod-2021",        "NJ OAG Law Enforcement Officer Diversity data, 2021 release", "https://www.njoag.gov/policerecruiting/",                              "2021",                    NA,
  "njoag-leod-2025",        "NJ OAG Law Enforcement Officer Diversity data, 2025 release", "https://www.njoag.gov/policerecruiting/",                              "2021-2025",               NA,
  "njoaguof",               "NJ OAG use of force data, via the njoaguof package",          "https://github.com/tor-gu/njoaguof",                                   paste(uof_dates, collapse = " to "), NA,
  "njoag-major-discipline", "NJ OAG Major Discipline data, 2020-2025 (combined file)",     "https://njoag.box.com/s/v93zq4oxauyb3bazpbph077fzmb2roje",             "2020-06-15 to 2025-12-31", "2026-10-31"
)
stopifnot(setequal(alias_source$source, agency_alias$source))

# ---- Write ----------------------------------------------------------------------------------

out_dir <- file.path("data-raw", "registry")
dir.create(out_dir, showWarnings = FALSE)
write_registry <- function(df, name) {
  write.csv(df, file.path(out_dir, paste0(name, ".csv")), row.names = FALSE, na = "")
  cat(sprintf("wrote %-22s %5d rows\n", paste0(name, ".csv"), nrow(df)))
}
section("Written")
write_registry(agency, "agency")
write_registry(agency_alias, "agency_alias")
write_registry(agency_municipality, "agency_municipality")
write_registry(agency_ori, "agency_ori")
write_registry(alias_source, "alias_source")

# ---- Review report --------------------------------------------------------------------------

section("Agencies by level x function")
show(agency |> count(agency_level, agency_function))
section("Aliases by source")
show(agency_alias |> count(source))
section(sprintf("Names to review (%d, note starts '%s')", sum(grepl(UNVERIFIED, agency$note)), UNVERIFIED))
show(agency |> filter(grepl(UNVERIFIED, note)) |>
       left_join(seed_reg |> select(agency_id, seed_name = agency_name), by = "agency_id") |>
       select(agency_id, agency_level, county, agency_name, seed_name))
section("Municipal names that differ from the FBI CDE name of their ORI")
cde_diff <- agency |> filter(agency_level == "Municipal") |>
  inner_join(agency_ori, by = "agency_id") |>
  mutate(cde_name = cde_names[ori]) |>
  filter(agency_name != cde_name)
cat(sprintf("%d of %d differ\n", nrow(cde_diff), sum(agency$agency_level == "Municipal")))
show(cde_diff |> select(agency_id, agency_name, cde_name))
section("Municipal ORI: canonical-name and seed-name matches disagree")
show(disagree)
section("Seed years reset to 2020 / NA (decision 8): seed agency years other than 2021-2025")
show(seed_reg |> filter(!(first_year == "2021" & final_year == "2025")) |>
       select(agency_id, agency_name, first_year, final_year))
section("Agencies without an ORI")
show(agency |> anti_join(agency_ori, by = "agency_id") |> select(agency_id, agency_name))

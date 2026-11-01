#' NJ law enforcement agencies
#'
#' One row per agency and name epoch. An agency keeps its `agency_id` across renames; a rename
#' with a known year closes one row and opens another. Years are the years the row is true
#' (the agency existed under that name), not the years a source reported it. The floor is
#' 2020: `first_year = 2020` means "vouched for from 2020".
#'
#' `agency_level` is the government that establishes and controls the agency today, or
#' `Private` if none. `county` is the agency's own county for Municipal, County and School
#' district agencies, and `NA` for State, Bi-state and Private ones.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{agency_id}{Agency ID. Opaque; never deleted, reused or repurposed once released.}
#'   \item{first_year}{First year of the epoch (>= 2020).}
#'   \item{final_year}{Final year of the epoch; `NA` means current.}
#'   \item{agency_name}{njlea's canonical name.}
#'   \item{county}{County, spelled as in `njmunicipalities::counties`, or `NA`.}
#'   \item{agency_level}{Factor: Municipal, County, State, Bi-state, School district, Private.}
#'   \item{agency_function}{Factor: Police, Sheriff, Prosecutor, Corrections, College or
#'     university, K-12 school, Park and conservation, Authority or transit, Investigative.}
#'   \item{note}{Provenance and history.}
#' }
#' @seealso [get_agencies()]
"agency"

#' Source spellings of agencies
#'
#' Every (source, county, name) spelling of an agency, exactly as the source writes it,
#' including `NA` counties. Used by [resolve_agency()].
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{source}{Source dataset; see [alias_source].}
#'   \item{source_county}{County as written by the source, or `NA`.}
#'   \item{source_name}{Agency name as written by the source.}
#'   \item{agency_id}{Agency ID.}
#'   \item{first_year, final_year}{Years the spelling means this agency; `NA` = unbounded.
#'     Set only when a spelling changes meaning.}
#'   \item{source_type}{The source's own agency type, where it has one (LEOD 2021).}
#' }
"agency_alias"

#' Municipalities served by agencies
#'
#' One row per agency, municipality and period. Municipal agencies have exactly one `Home`
#' link per year. Other links are `Service` links: a municipality the agency serves as its
#' primary local police by agreement. County-wide or statewide remits are not links.
#'
#' NJ State Police links are not included yet. NJSP is the local police of many rural
#' municipalities (77 full-time and 24 part-time, per a 2022 bill), but no published list
#' names them. Most municipalities with no row here are NJSP-patrolled, though not all.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{agency_id}{Agency ID.}
#'   \item{GEOID_Y2K}{Municipality, as its year-2000 Census GEOID (njmunicipalities).}
#'   \item{first_year, final_year}{Years the link held; `NA` final year means current.}
#'   \item{link_type}{Factor: Home or Service.}
#'   \item{note}{Source, required for Service links.}
#' }
#' @seealso [get_agency_municipalities()]
"agency_municipality"

#' FBI ORIs of agencies
#'
#' One row per agency and ORI. An agency can have several ORIs (NJ State Police has 22) or
#' none. FBI attributes are not copied; join the FBI Crime Data Explorer agency lists on
#' `ori`.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{agency_id}{Agency ID.}
#'   \item{ori}{ORI, 9 characters, any state prefix (DRBA's is `DE0020100`).}
#'   \item{GEOID_Y2K}{Set only when the ORI covers part of the agency (a Rutgers campus).}
#'   \item{first_year, final_year}{Years the ORI belongs to the agency.}
#'   \item{match}{Factor: Name (scripted name match) or Hand.}
#'   \item{note}{Provenance.}
#' }
"agency_ori"

#' Alias sources
#'
#' The source datasets whose spellings [agency_alias] records.
#'
#' @format A tibble with columns `source`, `title`, `url`, `coverage` and `retrieved`
#'   (a date, or `NA` if unknown).
"alias_source"

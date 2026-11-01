
<!-- README.md is generated from README.Rmd. Please edit that file -->

# njlea

<!-- badges: start -->

[![R-CMD-check](https://github.com/tor-gu/njlea/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tor-gu/njlea/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

njlea is a data package for R: a registry of New Jersey law enforcement
agencies, from 2020. It gives every agency a stable ID, its names over
time, its level of government and function, the municipalities it
polices, its FBI ORIs, and every spelling of it used by the NJ Office of
the Attorney General (OAG) datasets.

It exists so that packages built on OAG data, such as
[njoagleod](https://github.com/tor-gu/njoagleod) (officer diversity) and
[njoaguof](https://github.com/tor-gu/njoaguof) (use of force), can
identify agencies the same way and be joined on `agency_id` instead of
on names.

## Installation

You can install the development version of njlea from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tor-gu/njlea")
```

## What’s in it

``` r
library(njlea)

get_agencies(2025) |> count(agency_level)
#> # A tibble: 6 × 2
#>   agency_level        n
#>   <fct>           <int>
#> 1 Municipal         458
#> 2 County             60
#> 3 State              18
#> 4 Bi-state            3
#> 5 School district     1
#> 6 Private             3
```

The registry covers every agency that appears in one of its sources: the
OAG’s Law Enforcement Officer Diversity data (2021 and 2025 releases),
use of force data, and Major Discipline data. An agency that appears in
none of them, or only in the FBI’s lists, is not included.

Five tables:

- `agency`: one row per agency and name. An agency keeps its `agency_id`
  when it is renamed.
- `agency_alias`: every spelling of every agency, exactly as each source
  writes it.
- `agency_municipality`: the municipalities each agency polices, by
  year, using
  [njmunicipalities](https://github.com/tor-gu/njmunicipalities) GEOIDs.
- `agency_ori`: the agency’s FBI ORIs (the New Jersey State Police has
  22; some agencies have none).
- `alias_source`: the sources behind `agency_alias`.

## Conventions

**IDs.** Agency IDs (`NJLEA0001`, …) are opaque. Once released, an ID is
never deleted, reused or repurposed.

**Years.** `first_year` and `final_year` give the years a row is *true*
(the agency existed under that name, or the link held), not the years a
source reported it. The registry starts in 2020, so `first_year = 2020`
means “vouched for from 2020”. `final_year = NA` means current. A dated
change counts in whole years: a rename takes effect in the year of its
effective date, and an agency that ends exists through the year of its
last day.

**Level and function.** `agency_level` is the government that
establishes and controls the agency (Municipal, County, State, Bi-state,
School district), or Private if none. `agency_function` is what it does:

``` r
agency |> distinct(agency_id, agency_level, agency_function) |> count(agency_function)
#> # A tibble: 9 × 2
#>   agency_function           n
#>   <fct>                 <int>
#> 1 Police                  464
#> 2 Sheriff                  21
#> 3 Prosecutor               21
#> 4 Corrections              16
#> 5 College or university    14
#> 6 K-12 school               1
#> 7 Park and conservation     3
#> 8 Authority or transit      5
#> 9 Investigative             2
```

**County.** `county` is the agency’s own county for Municipal, County
and School district agencies, and `NA` for State, Bi-state and Private
ones.

**Names.** Municipal departments are named after their municipality,
following the FBI’s convention (“Cherry Hill Township Police
Department”, “Absecon Police Department”). Other agencies use their
official names.

## Examples

#### Agencies in a year

`get_agencies()` returns the agencies in existence in a year, with their
names as of that year. `multi_municipal` is `TRUE` for an agency that
polices more than one municipality.

``` r
get_agencies(2025) |> filter(multi_municipal)
#> # A tibble: 10 × 6
#>    agency_id agency_name                         county agency_level agency_function multi_municipal
#>    <chr>     <chr>                               <chr>  <fct>        <fct>           <lgl>          
#>  1 NJLEA0132 Camden County Police Department     Camde… County       Police          TRUE           
#>  2 NJLEA0144 Haddon Township Police Department   Camde… Municipal    Police          TRUE           
#>  3 NJLEA0166 Cape May Police Department          Cape … Municipal    Police          TRUE           
#>  4 NJLEA0212 Franklin Township Police Department Glouc… Municipal    Police          TRUE           
#>  5 NJLEA0227 Woodbury Police Department          Glouc… Municipal    Police          TRUE           
#>  6 NJLEA0228 Woolwich Township Police Department Glouc… Municipal    Police          TRUE           
#>  7 NJLEA0307 Belmar Police Department            Monmo… Municipal    Police          TRUE           
#>  8 NJLEA0355 Chester Township Police Department  Morri… Municipal    Police          TRUE           
#>  9 NJLEA0388 Washington Township Police Departm… Morri… Municipal    Police          TRUE           
#> 10 NJLEA0389 Wharton Police Department           Morri… Municipal    Police          TRUE
```

#### Finding an agency

`find_agency()` searches canonical names and every source spelling. Here
a former name finds the Youth Justice Commission, which was the Juvenile
Justice Commission until March 2025:

``` r
find_agency("juvenile justice")
#> # A tibble: 3 × 5
#>   agency_id agency_name                         county matched_on      matched_text                 
#>   <chr>     <chr>                               <chr>  <chr>           <chr>                        
#> 1 NJLEA0539 New Jersey Youth Justice Commission <NA>   name            New Jersey Juvenile Justice …
#> 2 NJLEA0539 New Jersey Youth Justice Commission <NA>   njoag-leod-2021 Juvenile Justice Commission  
#> 3 NJLEA0539 New Jersey Youth Justice Commission <NA>   njoaguof        NJ Juvenile Justice Commissi…

agency |> filter(agency_id == "NJLEA0539") |> select(first_year, final_year, agency_name)
#> # A tibble: 2 × 3
#>   first_year final_year agency_name                           
#>        <int>      <int> <chr>                                 
#> 1       2020       2024 New Jersey Juvenile Justice Commission
#> 2       2025         NA New Jersey Youth Justice Commission
```

#### Municipalities served

`get_agency_municipalities()` lists the municipalities each agency
polices in a year. A municipal department’s own municipality is its
`Home` link; a municipality it polices by agreement is a `Service` link.
Camden County Police Department polices Camden city, and took over
Woodlynne in 2024 when the Woodlynne Police Department disbanded:

``` r
get_agency_municipalities(2025) |> filter(agency_id == "NJLEA0132")
#> # A tibble: 2 × 5
#>   agency_id GEOID      municipality      county        link_type
#>   <chr>     <chr>      <chr>             <chr>         <fct>    
#> 1 NJLEA0132 3400710000 Camden city       Camden County Service  
#> 2 NJLEA0132 3400782450 Woodlynne borough Camden County Service
```

Use `geoid_year` to get GEOIDs as of another year, as in
`njmunicipalities::get_municipalities()`. `geoid_year = 2000` gives the
stable `GEOID_Y2K` codes.

#### Resolving source spellings

`resolve_agency()` is for packages that build on OAG data: it maps each
spelling, exactly as the source writes it, to an `agency_id`. With
`strict = TRUE` (the default) it stops on any spelling it doesn’t know,
listing them all, so a new spelling is caught when the consumer package
is built.

``` r
resolve_agency("njoag-major-discipline", "Bergen",
               c("Bergen County Sheriffs Office", "Ridgefield PD"))
#> [1] "NJLEA0023" "NJLEA0071"

resolve_agency("njoaguof", "Bergen County", c("Bergen Co Sheriffs Dept", "Nowhere PD"))
#> Error:
#> ! 1 spelling did not resolve to an agency (source | county | name):
#>   njoaguof | Bergen County | Nowhere PD
#> Use suggest_agency() for candidates.
```

`suggest_agency()` proposes candidates for a new spelling, and
`check_agency_years()` checks that each agency existed in the years a
dataset reports it:

``` r
suggest_agency("Bergen", "Ridgewod Vilage PD")
#> # A tibble: 3 × 5
#>   county name               agency_id agency_name                           method  
#>   <chr>  <chr>              <chr>     <chr>                                 <chr>   
#> 1 Bergen Ridgewod Vilage PD NJLEA0072 Ridgewood Police Department           distance
#> 2 Bergen Ridgewod Vilage PD NJLEA0074 River Vale Township Police Department distance
#> 3 Bergen Ridgewod Vilage PD NJLEA0070 Ridgefield Park Police Department     distance

check_agency_years(c("NJLEA0001", "NJLEA0001"), c(2023, 2019))
#> Error:
#> ! 1 (agency_id, year) pair outside every epoch of the agency:
#>   NJLEA0001 2019
```

#### Joining to FBI data

ORIs are attributes, not keys. Join `agency_ori` to FBI Crime Data
Explorer data on `ori`:

``` r
agency_ori |>
  filter(agency_id == agency$agency_id[agency$agency_name == "Rutgers University Police Department"])
#> # A tibble: 3 × 7
#>   agency_id ori       GEOID_Y2K  first_year final_year match note                                   
#>   <chr>     <chr>     <chr>           <int>      <int> <fct> <chr>                                  
#> 1 NJLEA0543 NJ0043900 3400710000       2020         NA Hand  CDE NJ list: 'Rutgers University: Camd…
#> 2 NJLEA0543 NJ0073000 3401351000       2020         NA Hand  CDE NJ list: 'Rutgers University: Newa…
#> 3 NJLEA0543 NJ0123000 3402351210       2020         NA Hand  CDE NJ list: 'Rutgers University: New …
```

## Changes since 2020

Agencies that ended, or were renamed, during the period covered:

``` r
agency |>
  group_by(agency_id) |>
  filter(n() > 1 | any(!is.na(final_year))) |>
  ungroup() |>
  select(agency_id, first_year, final_year, agency_name)
#> # A tibble: 8 × 4
#>   agency_id first_year final_year agency_name                                  
#>   <chr>          <int>      <int> <chr>                                        
#> 1 NJLEA0162       2020       2024 Woodlynne Police Department                  
#> 2 NJLEA0226       2020       2023 Woodbury Heights Police Department           
#> 3 NJLEA0281       2020       2020 Middlesex County College Police Department   
#> 4 NJLEA0281       2021         NA Middlesex College Police Department          
#> 5 NJLEA0310       2020       2023 Brookdale Community College Police Department
#> 6 NJLEA0371       2020       2021 Morris County Park Police Department         
#> 7 NJLEA0539       2020       2024 New Jersey Juvenile Justice Commission       
#> 8 NJLEA0539       2025         NA New Jersey Youth Justice Commission
```

- Morris County Park Police merged into the Morris County Sheriff’s
  Office on 2022-01-01.
- Woodbury Heights dissolved its department in July 2023; the City of
  Woodbury’s police department took over.
- Brookdale Community College’s police were replaced by the Monmouth
  County Sheriff’s Office in 2023.
- Woodlynne’s department disbanded on 2024-09-01; Camden County Police
  Department took over.
- Middlesex County College became Middlesex College on 2021-01-01.
- The Juvenile Justice Commission became the Youth Justice Commission on
  2025-03-17.

Each change’s source is in the `note` column.

## Known gaps

- **New Jersey State Police coverage.** The State Police is the local
  police of many rural municipalities (77 full-time and 24 part-time,
  according to a 2022 bill), but no published list names them, so it has
  no `Service` links yet. Most municipalities with no agency in
  `get_agency_municipalities()` are policed by the State Police, but not
  all:

  ``` r
  njmunicipalities::get_municipalities(2025) |>
    anti_join(get_agency_municipalities(2025), by = "GEOID") |>
    nrow()
  #> [1] 93
  ```

- **Agencies outside the sources.** Only agencies that appear in an OAG
  source are included. For example, Pine Valley had a small police
  department until it merged into Pine Hill in 2022, but no source
  reports it.

## Sources

``` r
alias_source |> select(source, title, coverage)
#> # A tibble: 4 × 3
#>   source                 title                                                       coverage       
#>   <chr>                  <chr>                                                       <chr>          
#> 1 njoag-leod-2021        NJ OAG Law Enforcement Officer Diversity data, 2021 release 2021           
#> 2 njoag-leod-2025        NJ OAG Law Enforcement Officer Diversity data, 2025 release 2021-2025      
#> 3 njoaguof               NJ OAG use of force data, via the njoaguof package          2020-10-01 to …
#> 4 njoag-major-discipline NJ OAG Major Discipline data, 2020-2025 (combined file)     2020-06-15 to …
```

Municipalities come from
[njmunicipalities](https://github.com/tor-gu/njmunicipalities); ORIs
from the FBI [Crime Data Explorer](https://cde.ucr.cjis.gov/) agency
lists.

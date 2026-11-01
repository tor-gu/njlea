# njlea 0.0.0.9000 (development version)

Initial version, to be released as 0.1.0: a registry of New Jersey law enforcement agencies
from 2020, for the NJ OAG dataset packages.

* 547 agencies with stable `NJLEA` IDs. A released ID is never deleted, reused or
  repurposed.
* Five tables: `agency` (name epochs, level, function, county), `agency_alias` (every
  spelling in the four alias sources: LEOD 2021 and 2025, use of force, Major Discipline),
  `agency_municipality` (Home and Service links, by njmunicipalities `GEOID_Y2K`),
  `agency_ori` (FBI ORIs) and `alias_source`.
* Build-time functions for consumer packages: `resolve_agency()`, `suggest_agency()` and
  `check_agency_years()`.
* Use-time functions: `get_agencies()`, `get_agency_municipalities()` and `find_agency()`.
* Known gap: no New Jersey State Police coverage links yet.

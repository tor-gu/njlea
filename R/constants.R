# Year range and factor levels shared by the package and data-raw/build_registry.R.

# First year njlea vouches for (decision 8) and the last year njmunicipalities covers.
# Raise MAX_YEAR together with the njmunicipalities dependency.
MIN_YEAR <- 2020L
MAX_YEAR <- 2026L

AGENCY_LEVELS <- c("Municipal", "County", "State", "Bi-state", "School district", "Private")
AGENCY_FUNCTIONS <- c("Police", "Sheriff", "Prosecutor", "Corrections",
                      "College or university", "K-12 school", "Park and conservation",
                      "Authority or transit", "Investigative")
LINK_TYPES <- c("Home", "Service")
ORI_MATCHES <- c("Name", "Hand")

# Levels whose agencies have a county (decision 9); State, Bi-state and Private have NA.
COUNTY_LEVELS <- c("Municipal", "County", "School district")

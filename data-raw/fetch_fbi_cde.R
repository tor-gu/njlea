# Snapshot the FBI Crime Data Explorer agency lists for New Jersey and its neighbours.
#
# Run by hand, rarely. The build reads the committed snapshots, never the API.
# Needs an api.data.gov key in the environment (e.g. in ~/.Renviron):
#   FBI_API_KEY=...
#
# Usage, from the package root:
#   Rscript data-raw/fetch_fbi_cde.R            # NJ, DE, PA, NY
#   Rscript data-raw/fetch_fbi_cde.R NJ DE      # just these states
#
# Neighbouring states are included because bi-state agencies can be filed under either
# state: the Delaware River and Bay Authority appears only in the DE list (DE0020100).
#
# For each state, writes data-raw/sources/fbi_cde/agencies_<ST>_<date>.json (raw response,
# unchanged) and a flattened .csv alongside it.

key <- Sys.getenv("FBI_API_KEY")
if (!nzchar(key)) stop("Set FBI_API_KEY (an api.data.gov key).")

states <- commandArgs(trailingOnly = TRUE)
if (length(states) == 0) states <- c("NJ", "DE", "PA", "NY")
states <- toupper(states)
stopifnot(all(grepl("^[A-Z]{2}$", states)))

out_dir <- file.path("data-raw", "sources", "fbi_cde")

# Row-bind data frames whose columns may differ, filling the gaps with NA.
bind_fill <- function(dfs) {
  dfs <- Filter(Negate(is.null), dfs)
  cols <- unique(unlist(lapply(dfs, names)))
  do.call(rbind, lapply(dfs, function(d) {
    d[setdiff(cols, names(d))] <- NA
    d[cols]
  }))
}

fetch_state <- function(st) {
  url <- paste0("https://api.usa.gov/crime/fbi/cde/agency/byStateAbbr/", st, "?API_KEY=", key)
  stem <- file.path(out_dir, paste0("agencies_", st, "_", format(Sys.Date(), "%Y%m%d")))
  json_file <- paste0(stem, ".json")

  tmp <- tempfile(fileext = ".json")
  utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
  raw <- readLines(tmp, warn = FALSE)
  if (grepl(key, paste(raw, collapse = ""), fixed = TRUE)) stop("Response echoes the API key.")
  invisible(file.copy(tmp, json_file, overwrite = TRUE))

  # The response is a list keyed by county; each element is a data frame of agencies.
  parsed <- jsonlite::fromJSON(json_file, simplifyVector = TRUE)
  flat <- if (is.data.frame(parsed)) parsed else bind_fill(
    lapply(names(parsed), function(grp) {
      d <- parsed[[grp]]
      if (!is.data.frame(d) || nrow(d) == 0) return(NULL)
      d[] <- lapply(d, function(col) if (is.list(col)) vapply(col, paste, "", collapse = ";") else col)
      cbind(response_group = grp, d)
    })
  )
  utils::write.csv(flat, paste0(stem, ".csv"), row.names = FALSE, na = "")
  message(st, ": ", nrow(flat), " agencies written to ", stem, ".{json,csv}")
}

for (st in states) fetch_state(st)

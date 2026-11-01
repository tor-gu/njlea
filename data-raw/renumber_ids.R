# One-off: renumber the seed's interim agency IDs (A0001...) to NJLEA IDs (decision 8).
#
# Order: by county, then canonical name (each agency's latest name), with the agencies that
# have no county (State, Bi-state, Private) last. Sorting uses the C locale so the order is
# reproducible. After this, IDs are append-only: a new agency gets the next number.
#
# Rewrites agency_id in the four registry CSVs and every "A0000"-style reference inside
# notes, and writes data-raw/seed_id_map.csv (seed_id -> agency_id) for moving njoagleod over.
# Refuses to run once the IDs are NJLEA IDs.
#
# Run from the package root: Rscript data-raw/renumber_ids.R

suppressPackageStartupMessages(library(dplyr))
reg_file <- function(name) file.path("data-raw", "registry", paste0(name, ".csv"))
read_reg <- function(name) read.csv(reg_file(name), colClasses = "character", na.strings = "",
                                    encoding = "UTF-8", check.names = FALSE)
write_reg <- function(df, name) write.csv(df, reg_file(name), row.names = FALSE, na = "",
                                          fileEncoding = "UTF-8")

agency <- read_reg("agency")
if (any(grepl("^NJLEA", agency$agency_id))) stop("IDs are already NJLEA IDs; nothing to do")
stopifnot(all(grepl("^A[0-9]{4}$", agency$agency_id)))

order_key <- agency |>
  group_by(agency_id) |>
  slice_max(as.integer(first_year), n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(agency_id, county, agency_name)
o <- order(is.na(order_key$county), order_key$county, order_key$agency_name,
           order_key$agency_id, method = "radix")
id_map <- tibble(seed_id = order_key$agency_id[o],
                 agency_id = sprintf("NJLEA%04d", seq_along(o)))
stopifnot(!anyDuplicated(id_map$seed_id), !anyDuplicated(id_map$agency_id))
new_id <- setNames(id_map$agency_id, id_map$seed_id)

# Replace every seed-ID reference in free text (notes).
remap_text <- function(x) {
  todo <- !is.na(x)
  x[todo] <- remap_text_(x[todo])
  x
}
remap_text_ <- function(x) {
  hits <- gregexpr("\\bA[0-9]{4}\\b", x)
  regmatches(x, hits) <- lapply(regmatches(x, hits), function(m) {
    out <- new_id[m]
    if (anyNA(out)) stop("note refers to an unknown ID: ", paste(m[is.na(out)], collapse = ", "))
    out
  })
  x
}

for (name in c("agency", "agency_alias", "agency_municipality", "agency_ori")) {
  df <- read_reg(name)
  stopifnot(all(df$agency_id %in% names(new_id)))
  df$agency_id <- unname(new_id[df$agency_id])
  if ("note" %in% names(df)) df$note <- remap_text(df$note)
  for (y in intersect(c("first_year", "final_year"), names(df))) df[[y]] <- as.integer(df[[y]])
  df <- df[order(df$agency_id, method = "radix"), , drop = FALSE]
  write_reg(df, name)
  cat(sprintf("rewrote %-20s %5d rows\n", name, nrow(df)))
}

write.csv(id_map, file.path("data-raw", "seed_id_map.csv"), row.names = FALSE)
cat(sprintf("wrote data-raw/seed_id_map.csv (%d IDs)\n", nrow(id_map)))

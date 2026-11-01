# Name keys for matching agency spellings. Internal.
#
# Also sourced by the data-raw scripts (migrate_seed.R, build_registry.R), so the package and
# its build share one definition. Base R only, so it can be sourced without the package
# loaded.

#' Normalise an agency or county spelling
#'
#' Lower case, punctuation dropped, common abbreviations spelled out ("Twp" -> "township",
#' "Co" -> "county", "Dept" -> "department", ...), whitespace squeezed.
#' @param x Character vector.
#' @return Character vector.
#' @noRd
norm_key <- function(x) {
  x <- tolower(x)
  x <- gsub("[.'\u2019,]", "", x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("\\btwp\\b|\\btwsp\\b", "township", x)
  x <- gsub("\\bboro?\\b", "borough", x)
  x <- gsub("\\bco\\b", "county", x)
  x <- gsub("\\bmt\\b", "mount", x)
  x <- gsub("\\bpt\\b", "point", x)
  x <- gsub("\\bcrk\\b", "creek", x)
  # One-word spellings used by some sources
  x <- sub("^bayhead\\b", "bay head", x)
  x <- sub("^oceangate\\b", "ocean gate", x)
  x <- sub("^peapack[- ]gladstone\\b", "peapack and gladstone", x)
  x <- gsub("\\bdept\\b", "department", x)
  x <- gsub("-", " ", x)
  gsub("\\s+", " ", trimws(x))
}

#' Loose key: drops municipal type words and agency words
#'
#' Also drops Major Discipline's " - X County" suffix. Used only to suggest candidates,
#' never to resolve.
#' @param x Character vector.
#' @return Character vector.
#' @noRd
loose_key <- function(x) {
  x <- norm_key(sub(" - [A-Za-z ]+ County$", "", x))
  x <- gsub("\\b(township|borough|village|city|town|pd|police|department|office|of|the)\\b", "", x)
  gsub("\\s+", " ", trimws(x))
}

#' Township-keeping key: like loose_key() but keeps "township"
#'
#' Distinguishes Berlin borough's department from Berlin township's.
#' @param x Character vector.
#' @return Character vector.
#' @noRd
township_key <- function(x) {
  x <- norm_key(sub(" - [A-Za-z ]+ County$", "", x))
  x <- gsub("\\b(borough|village|city|town|pd|police|department|office|of|the)\\b", "", x)
  gsub("\\s+", " ", trimws(x))
}

#' Spell a county the njmunicipalities way
#'
#' "Atlantic", "ATLANTIC" or "Atlantic County" -> "Atlantic County"; anything that isn't one
#' of the 21 counties ("State or Other Agency", NA) -> NA.
#' @param x Character vector.
#' @return Character vector.
#' @noRd
county_name <- function(x) {
  counties <- c("Atlantic", "Bergen", "Burlington", "Camden", "Cape May", "Cumberland",
                "Essex", "Gloucester", "Hudson", "Hunterdon", "Mercer", "Middlesex",
                "Monmouth", "Morris", "Ocean", "Passaic", "Salem", "Somerset", "Sussex",
                "Union", "Warren")
  base <- sub("(?i)\\s+county$", "", trimws(x), perl = TRUE)
  hit <- match(tolower(base), tolower(counties))
  ifelse(is.na(hit), NA_character_, paste(counties[hit], "County"))
}

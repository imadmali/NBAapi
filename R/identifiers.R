#' Get Team Identifiers for NBA
#'
#' Wrapper for \code{\link[nba]{get_teaminfo}} to get NBA team identifiers.
#'
#' @return A data frame with one column representing team IDs and another column
#'   representing team abbreviations.
#'
#' @examples
#' team_ids()
#'
#' @export
#'

team_ids <- function() {
  out <- get_teaminfo("00") %>%
    dplyr::select(TEAM_ID, ABBREVIATION) %>%
    filter(!is.na(ABBREVIATION))
  return(out)
}


#' Get Player Identifiers for NBA
#'
#' Wrapper for \code{\link[nba]{get_playerinfo}} to get all NBA player
#' identifiers.
#'
#' @param Season See \code{\link[nba]{make_url}}.
#' @param IsOnlyCurrentSeason See \code{\link[nba]{make_url}}.
#' @return A data frame returning player IDs, names, and team affiliations.
#'
#' @examples
#' player_ids(Season = "2016-17", IsOnlyCurrentSeason = "0")
#'
#' @export
#'

player_ids <- function(Season = "", IsOnlyCurrentSeason = "") {
  out <- get_playerinfo(LeagueID = "00", Season = Season, IsOnlyCurrentSeason = IsOnlyCurrentSeason) %>%
    dplyr::select(PERSON_ID, DISPLAY_FIRST_LAST, TEAM_ID, TEAM_ABBREVIATION) %>%
    filter(TEAM_ABBREVIATION != "")
  return(out)
}


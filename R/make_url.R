#' Construct NBA API URL
#'
#' Construct an NBA URL given various API arguments.
#'
#' @param datatype One of \code{"shotchartdetail"}, ...
#' @param SeasonType One of \code{"Regular+Season"},
#'   \code{"Pre+Season"},\code{"Playoffs"}, or \code{"All+Star"}.
#' @param LeagueID One of \code{"00"} (NBA) or \code{"01"} (ABA).
#' @param Season Of the format \code{"YYYY-YY"}. For example \code{"2016-17"}.
#' @param IsOnlyCurrentSeason TBD.
#' @param PlayerID Player identifier.
#' @param TeamID Team identifier.
#' @param GameID Game identifier.
#' @param ContextMeasure TBD.
#' @param PlayerPosition TBD.
#' @param DateFrom TBD.
#' @param DateTo TBD.
#' @param GameSegment TBD.
#' @param LastNGames TBD.
#' @param Location TBD.
#' @param Month One of \code{"0"} (all months), \code{"1"} to \code{"7"}
#'   (regular season), or \code{"8"}/\code{"9"} (playoffs).
#' @param OpponentTeamID TBD.
#' @param Outcome TBD.
#' @param SeasonSegment TBD.
#' @param VSConference TBD.
#' @param VSDivision TBD.
#' @param RookieYear TBD.
#' @param Period TBD.
#' @param StartPeriod TBD.
#' @param EndPeriod TBD.
#' @return NBA API url string.
#'
#' @export

make_url <- function(datatype = NULL,
                     SeasonType = "",
                     LeagueID = "",
                     Season = "",
                     IsOnlyCurrentSeason = "",
                     PlayerID = "",
                     TeamID = "",
                     GameID = "",
                     ContextMeasure = "",
                     PlayerPosition = "",
                     DateFrom = "",
                     DateTo = "",
                     GameSegment = "",
                     LastNGames = "",
                     Location = "",
                     Month = "",
                     OpponentTeamID = "",
                     Outcome = "",
                     SeasonSegment = "",
                     VSConference = "",
                     VSDivision = "",
                     RookieYear = "",
                     Period = "",
                     StartPeriod = "",
                     EndPeriod = "",
                     StartRange = "",
                     EndRange = "",
                     RangeType = "") {
  prefix <- paste0("http://stats.nba.com/stats/", datatype, "?")
  info <- list(
    SeasonType = SeasonType,
    LeagueID = LeagueID,
    Season = Season,
    IsOnlyCurrentSeason = IsOnlyCurrentSeason,
    PlayerID = PlayerID,
    TeamID = TeamID,
    GameID = GameID,
    ContextMeasure = ContextMeasure,
    PlayerPosition = PlayerPosition,
    DateFrom = DateFrom,
    DateTo = DateTo,
    GameSegment = GameSegment,
    LastNGames = LastNGames,
    Location = Location,
    Month = Month,
    OpponentTeamID = OpponentTeamID,
    Outcome = Outcome,
    SeasonSegment = SeasonSegment,
    VSConference = VSConference,
    VSDivision = VSDivision,
    RookieYear = RookieYear,
    Period = Period,
    StartPeriod = StartPeriod,
    EndPeriod = EndPeriod,
    StartRange = StartRange,
    EndRange = EndRange,
    RangeType = RangeType
  )

  info_str <- paste0(names(info), "=", unlist(info), sep = "&", collapse = "")
  str_len <- nchar(info_str)
  info_str <- substr(info_str, 1, str_len - 1)
  url_str <- paste0(prefix, info_str)
  return(url_str)
}

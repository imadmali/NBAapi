#' Get Shot Chart Data
#'
#' Get shot chart data for a player using the NBA API. See
#' \code{\link[nba]{make_url}} for details on the parameter arguments.
#'
#' @return A data.frame containing shot chart data.
#'
#' @examples
#' # Pull shot chart data for all players in the Cleveland Cavaliers
#' sc_data <- get_shotchart(
#'                      SeasonType = "Regular+Season",
#'                      LeagueID = "00",
#'                      Season = "2016-17",
#'                      PlayerID = "0",
#'                      TeamID = "1610612739",
#'                      GameID = "",
#'                      ContextMeasure = "FGA",
#'                      PlayerPosition = "",
#'                      DateFrom = "",
#'                      DateTo = "",
#'                      GameSegment = "",
#'                      LastNGames = "0",
#'                      Location = "",
#'                      Month = "0",
#'                      OpponentTeamID = "0",
#'                      Outcome = "",
#'                      SeasonSegment = "",
#'                      VSConference = "",
#'                      VSDivision = "",
#'                      RookieYear = "",
#'                      Period = "0",
#'                      StartPeriod = "",
#'                      EndPeriod = "")
#'
#' @export

get_shotchart <- function(SeasonType = "",
                          LeagueID = "",
                          Season = "",
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
                          EndPeriod) {

  url_str <- make_url(datatype = "shotchartdetail",
                      SeasonType = SeasonType,
                      LeagueID = LeagueID,
                      Season = Season,
                      IsOnlyCurrentSeason = "",
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
                      EndPeriod = EndPeriod)

  raw_game <- rjson::fromJSON(file = url_str)

  if (length(raw_game$resultSets[[1]]$rowSet) == 0)
    stop(paste0("'", raw_game$resultSets[[1]]$name, "'", " data is unavailable."))

  header <- raw_game$resultSets[[1]]$headers
  container <- list()
  for (i in 1:length(header))
    container[[i]] <- sapply(raw_game$resultSets[[1]]$rowSet, "[[", i)
  out <- do.call("data.frame", args = list(container, stringsAsFactors = FALSE))
  names(out) <- header
  return(out)
}


#' Get Play-by-Play Data (V2)
#' @param GameID Game identifier.
#' @param StartPeriod TBD.
#' @param EndPeriod TBD.
#' @param ... Arguments to code{\link[nba]{make_url}}.
#' @return A data frame containing detailed play-by-play information for a
#'   single game.
#' @examples
#' # get play-by-play information for game no. 0021300028.
#' pbp_info <- get_pbp2(GameID = "0021300028",
#'                      StartPeriod = "1",
#'                      EndPeriod = "10")
#'
#' @export

get_pbp2 <- function(GameID = "", StartPeriod = "", EndPeriod = "",
                     ...) {

  url_str <- make_url(datatype = "playbyplayv2",
                      GameID = GameID,
                      StartPeriod = StartPeriod,
                      EndPeriod = EndPeriod, ...)

  raw_game <- rjson::fromJSON(file = url_str)

  out <- json2df(raw_game)
}

#' Get Play-by-Play Data
#' @param GameID Game identifier.
#' @param StartPeriod TBD.
#' @param EndPeriod TBD.
#' @param ... Arguments to code{\link[nba]{make_url}}.
#' @return A data frame containing basic play-by-play information for a single
#'   game.
#' @examples
#' # get play-by-play information for game no. 0021300028.
#' pbp_info <- get_pbp(GameID = "0021300028",
#'                     StartPeriod = "1",
#'                     EndPeriod = "10")
#'
#' @export

get_pbp <- function(GameID = "", StartPeriod = "", EndPeriod = "",
                     ...) {

  url_str <- make_url(datatype = "playbyplay",
                      GameID = GameID,
                      StartPeriod = StartPeriod,
                      EndPeriod = EndPeriod, ...)

  raw_game <- rjson::fromJSON(file = url_str)

  out <- json2df(raw_game)
}

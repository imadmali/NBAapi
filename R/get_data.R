#' Get Shot Chart Data
#'
#' Get shot chart data for a player using the NBA API. See \code{\link[nba]{make_url}} for details on the arguments.
#'
#' @return A data.frame containing shot chart data.
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
                          Period = "") {

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
                      Period = Period)

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

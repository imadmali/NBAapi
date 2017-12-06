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
                          EndPeriod = "") {

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
#' @param ... Arguments to {\link[nba]{make_url}}.
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

  return(out)
}

#' Get Play-by-Play Data
#' @param GameID Game identifier.
#' @param StartPeriod TBD.
#' @param EndPeriod TBD.
#' @param ... Arguments to {\link[nba]{make_url}}.
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

  return(out)
}

#' Get Player Information Data
#' @param LeagueID Game identifier.
#' @param Season TBD.
#' @param IsOnlyCurrentSeason TBD.
#' @param ... Arguments to {\link[nba]{make_url}}.
#' @return A data frame containing player information in a given season.
#' @examples
#' # get player information for the 2016-17 NBA season
#' player_information <- get_playerinfo(LeagueID = "00",
#'                                      Season = "2016-17",
#'                                      IsOnlyCurrentSeason = "1")
#'
#' @export

get_playerinfo <- function(LeagueID = "",
                           Season = "",
                           IsOnlyCurrentSeason = "",
                    ...) {

  url_str <- make_url(datatype = "commonallplayers",
                      LeagueID = LeagueID,
                      SeasonType = "",
                      Season = Season,
                      IsOnlyCurrentSeason = IsOnlyCurrentSeason,
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
                      EndPeriod = "")

  raw_game <- rjson::fromJSON(file = url_str)

  out <- json2df(raw_game)

  return(out)
}


#' Get Team Roster Information
#' @param TeamID See \code{\link[nba]{make_url}}.
#' @param Season See \code{\link[nba]{make_url}}.
#' @param ... Arguments to \code{\link[nba]{make_url}}.
#' @return A data frame containing player information in a given season.
#' @examples
#' # get team roster information for Miami Heat in the 2016-17 NBA season
#' team_roster <- get_teamroster(TeamID = "1610612748",
#'                               Season = "2016-17")
#'
#' @export

get_teamroster <- function(TeamID = "",
                           Season = "",
                           ...) {

  url_str <- make_url(datatype = "commonteamroster",
                      LeagueID = "",
                      SeasonType = "",
                      Season = Season,
                      IsOnlyCurrentSeason = "",
                      PlayerID = "",
                      TeamID = TeamID,
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
                      EndPeriod = "")

  raw_game <- rjson::fromJSON(file = url_str)

  out <- json2df(raw_game)

  return(out)
}

#' Get Team Year Availability Information
#' @param LeagueID See \code{\link[nba]{make_url}}.
#' @param ... Arguments to \code{\link[nba]{make_url}}.
#' @return A data frame containing team information in a given season.
#' @examples
#' # get team information for the NBA
#' team_roster <- get_teamroster(TeamID = "1610612748",
#'                               Season = "2016-17")
#'
#' @export

get_teaminfo <- function(LeagueID = "",
                           ...) {

  url_str <- make_url(datatype = "commonteamyears",
                      LeagueID = LeagueID,
                      SeasonType = "",
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
                      EndPeriod = "")

  raw_game <- rjson::fromJSON(file = url_str)

  out <- json2df(raw_game)

  return(out)
}

#' Get Boxscore Traditional (V2)
#' @param GameID See \code{\link[nba]{make_url}}.
#' @param StartPeriod See \code{\link[nba]{make_url}}.
#' @param EndPeriod See \code{\link[nba]{make_url}}.
#' @param StartRange See \code{\link[nba]{make_url}}.
#' @param EndRange See \code{\link[nba]{make_url}}.
#' @param RangeType See \code{\link[nba]{make_url}}.
#' @param ... Arguments to \code{\link[nba]{make_url}}.
#' @return A data frame containing boxscore information (counts) for a given
#'   game.
#'
#' @examples
#' get_boxscoretraditionalv2(GameID = "0021300028",
#'                           StartPeriod = "1",
#'                           EndPeriod = "10",
#'                           StartRange = "1",
#'                           EndRange = "10",
#'                           RangeType = "1")
#'
#' @export

get_boxscoretraditionalv2 <- function(GameID = "",
                                  StartPeriod = "",
                                  EndPeriod = "",
                                  StartRange = "",
                                  EndRange = "",
                                  RangeType = "", ...) {

  url_str <- make_url(datatype = "boxscoretraditionalv2",
                      GameID = GameID,
                      StartPeriod = StartPeriod,
                      EndPeriod = EndPeriod,
                      StartRange = StartRange,
                      EndRange = EndRange,
                      RangeType = RangeType, ...)

  raw_game <- rjson::fromJSON(file = url_str)

  out <- json2df(raw_game, index = 1)

  return(out)
}



#' Get Boxscore Scoring (V2)
#' @param GameID See \code{\link[nba]{make_url}}.
#' @param StartPeriod See \code{\link[nba]{make_url}}.
#' @param EndPeriod See \code{\link[nba]{make_url}}.
#' @param StartRange See \code{\link[nba]{make_url}}.
#' @param EndRange See \code{\link[nba]{make_url}}.
#' @param RangeType See \code{\link[nba]{make_url}}.
#' @param ... Arguments to \code{\link[nba]{make_url}}.
#' @return A data frame containing boxscore information (percentages) for a
#'   given game.
#'
#' @examples
#' get_boxscorescoringv2(GameID = "0021300028",
#'                       StartPeriod = "1",
#'                       EndPeriod = "10",
#'                       StartRange = "1",
#'                       EndRange = "10",
#'                       RangeType = "1")
#'
#' @export

get_boxscorescoringv2 <- function(GameID = "",
                         StartPeriod = "",
                         EndPeriod = "",
                         StartRange = "",
                         EndRange = "",
                         RangeType = "", ...) {

  url_str <- make_url(datatype = "boxscorescoringv2",
                      GameID = GameID,
                      StartPeriod = StartPeriod,
                      EndPeriod = EndPeriod,
                      StartRange = StartRange,
                      EndRange = EndRange,
                      RangeType = RangeType, ...)

  raw_game <- rjson::fromJSON(file = url_str)

  out <- json2df(raw_game, index = 1)

  return(out)
}


#' Get Boxscore Summary (V2)
#' @param GameID See \code{\link[nba]{make_url}}.
#' @return A list containing data frames containing team/player/arena
#'   information for a given game.
#'
#' @examples
#' get_boxscoresummaryv2(GameID = "0021300028")
#'
#' @export

get_boxscoresummaryv2 <- function(GameID = "", ...) {

  url_str <- make_url(datatype = "boxscoresummaryv2",
                      GameID = GameID, ...)

  raw_game <- rjson::fromJSON(file = url_str)

  out <- list()
  for (i in 1:length(raw_game$resultSets)) {
    out[[i]] <- json2df(raw_game, index = i)
  }

  return(out)
}

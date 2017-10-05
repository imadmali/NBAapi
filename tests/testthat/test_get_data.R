context("test 'get_data' functions")

test_that("get_shotchart runs", {
  tmp <- sc_data <- get_shotchart(
    SeasonType = "Regular+Season",
    LeagueID = "00",
    Season = "2016-17",
    PlayerID = "0",
    TeamID = "1610612739",
    GameID = "",
    ContextMeasure = "FGA",
    PlayerPosition = "",
    DateFrom = "",
    DateTo = "",
    GameSegment = "",
    LastNGames = "0",
    Location = "",
    Month = "0",
    OpponentTeamID = "0",
    Outcome = "",
    SeasonSegment = "",
    VSConference = "",
    VSDivision = "",
    RookieYear = "",
    Period = "0",
    StartPeriod = "",
    EndPeriod = "")
})

test_that("get_pbp2 runs", {
  tmp <- get_pbp2(GameID = "0021300028",
                  StartPeriod = "1",
                  EndPeriod = "10")
})

test_that("get_pbp runs", {
  tmp <- get_pbp(GameID = "0021300028",
                 StartPeriod = "1",
                 EndPeriod = "10")
})

test_that("get_playerinfo runs", {
  tmp <- get_playerinfo(LeagueID = "00", Season = "2016-17", IsOnlyCurrentSeason = "1")
})

test_that("get_teamroster runs", {
  tmp <- get_teamroster(TeamID = "1610612739", Season = "2016-17")
})

test_that("get_teaminfo runs", {
  tmp <- get_teaminfo(LeagueID = "00")
  tmp <- get_teaminfo(LeagueID = "01")
})

test_that("get_boxscoretraditionalv2 runs", {
  tmp <- get_boxscoretraditionalv2(GameID = "0021600001",
                                   StartPeriod = "0",
                                   EndPeriod = "10",
                                   StartRange = "1",
                                   EndRange = "10",
                                   RangeType = "1")
})

test_that("get_boxscorescoringv2 runs", {
  tmp <- get_boxscorescoringv2(GameID = "0021600001",
                               StartPeriod = "0",
                               EndPeriod = "10",
                               StartRange = "1",
                               EndRange = "10",
                               RangeType = "1")
})

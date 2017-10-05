context("test 'make_url' functions")

test_that("No arguments constructs the correct string", {
  tmp <- make_url(datatype = NULL, SeasonType = "", LeagueID = "", Season = "",
                  IsOnlyCurrentSeason = "", PlayerID = "", TeamID = "", GameID = "",
                  ContextMeasure = "", PlayerPosition = "", DateFrom = "", DateTo = "",
                  GameSegment = "", LastNGames = "", Location = "", Month = "",
                  OpponentTeamID = "", Outcome = "", SeasonSegment = "",
                  VSConference = "", VSDivision = "", RookieYear = "", Period = "",
                  StartPeriod = "", EndPeriod = "", StartRange = "", EndRange = "",
                  RangeType = "")
  expect_equal(tmp, paste0("http://stats.nba.com/stats/?SeasonType=&LeagueID=&Season=&",
               "IsOnlyCurrentSeason=&PlayerID=&TeamID=&GameID=&ContextMeasure=&",
               "PlayerPosition=&DateFrom=&DateTo=&GameSegment=&LastNGames=&",
               "Location=&Month=&OpponentTeamID=&Outcome=&SeasonSegment=&",
               "VSConference=&VSDivision=&RookieYear=&Period=&StartPeriod=&",
               "EndPeriod=&StartRange=&EndRange=&RangeType="))
})



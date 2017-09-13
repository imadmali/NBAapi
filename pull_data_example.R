func_val <- make_url(datatype = "shotchartdetail",
         SeasonType = "Regular+Season",
         LeagueID = "00",
         Season = "2015-16",
         IsOnlyCurrentSeason = "0",
         PlayerID = "2544",
         TeamID = "1610612739",
         GameID = "",
         ContextMeasure = "FGA",
         PlayerPosition = "",
         DateFrom = "",
         DateTo = "",
         GameSegment = "",
         LastNGames = "0",
         Location = "",
         Month = "1",
         OpponentTeamID = "0",
         Outcome = "",
         SeasonSegment = "",
         VSConference = "",
         VSDivision = "",
         RookieYear = "",
         Period = "0")

true <- "http://stats.nba.com/stats/shotchartdetail?SeasonType=Regular+Season&LeagueID=00&Season=2015-16&IsOnlyCurrentSeason=0&PlayerID=2544&TeamID=1610612739&GameID=&ContextMeasure=FGA&PlayerPosition=&DateFrom=&DateTo=&GameSegment=&LastNGames=0&Location=&Month=0&OpponentTeamID=0&Outcome=&SeasonSegment=&VSConference=&VSDivision=&RookieYear=&Period=0"

func_val
true

true == func_val

sc_data <- get_shotchart(
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
head(sc_data)

# PlayerID = "0" gives all players in the team

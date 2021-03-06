% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_boxscorescoringv2}
\alias{get_boxscorescoringv2}
\title{Get Boxscore Scoring (V2)}
\usage{
get_boxscorescoringv2(GameID = "", StartPeriod = "", EndPeriod = "",
  StartRange = "", EndRange = "", RangeType = "", ...)
}
\arguments{
\item{GameID}{See \code{\link[nba]{make_url}}.}

\item{StartPeriod}{See \code{\link[nba]{make_url}}.}

\item{EndPeriod}{See \code{\link[nba]{make_url}}.}

\item{StartRange}{See \code{\link[nba]{make_url}}.}

\item{EndRange}{See \code{\link[nba]{make_url}}.}

\item{RangeType}{See \code{\link[nba]{make_url}}.}

\item{...}{Arguments to \code{\link[nba]{make_url}}.}
}
\value{
A data frame containing boxscore information (percentages) for a
  given game.
}
\description{
Get Boxscore Scoring (V2)
}
\examples{
get_boxscorescoringv2(GameID = "0021300028",
                      StartPeriod = "1",
                      EndPeriod = "10",
                      StartRange = "1",
                      EndRange = "10",
                      RangeType = "1")

}

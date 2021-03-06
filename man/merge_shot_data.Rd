% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graphics.R
\name{merge_shot_data}
\alias{merge_shot_data}
\title{Merge Shot Data with Half Court Lattice Shapefile}
\usage{
merge_shot_data(shpfile, shot_df, hex = FALSE)
}
\arguments{
\item{shpfile}{SpatialPolygonsDataFrame returned \code{from half_court_raster}.}

\item{shot_df}{Data frame object returned from \code{get_shotchart}.}
}
\value{
SpatialPolygonsDataFrame object which contains the spatial polygons
  along with additional data from(shot_df) in the data slot.
}
\description{
Merge the shot chart data with a SpatialPolygonDataFrame representing an NBA
regulation half court. This makes it useful to construct a weight matrix that
can be used in spatial modeling.
}
\examples{
# create a half court raster SpatialPolygonsDataFrame object
sp_pol <- half_court_raster()
# merge shot chart data with the spatial polygon (grid)
sp_pol <- merge_shot_data(sp_pol, NBAapi::shots_cavs)
# plot the result
## first deal with color
colors <- c("white", "black")
num_cols <- length(unique(sp_pol@data$counts))
plotclr <- colorRampPalette(colors)(num_cols)
breaks <- seq(0, max(max(unique(sp_pol@data$counts), na.rm = TRUE)), length.out = num_cols + 1)
colcode <- plotclr[findInterval(sp_pol@data$counts, vec = sort(unique(sp_pol@data$counts)))]
## now plot grid
plot(sp_pol, col = colcode, border = "transparent")
## now plot shots
draw_halfcourt(add = TRUE)

}

#' Plot Half Court
#'
#' Plot the court markings for the NBA regulation basketball half court.
#'
#' @return Plot of half court
#'
#' @examples
#' sc_data <- shots_cavs
#' # plot halfcout markings
#' draw_halfcourt()
#' # plot data
#' points(sc_data$LOC_X, sc_data$LOC_Y, cex = 0.5)
#' # highlight three-pointers
#' threes <- dplyr::filter(sc_data, SHOT_TYPE == "3PT Field Goal")
#' points(threes$LOC_X, threes$LOC_Y, cex = 0.5, col = "#FF6688")
#'
#' @export
#'

draw_halfcourt <- function(xlim = c(-300,300), ylim = c(-100,500), add = FALSE, ...) {
  if (!add) {
    plot(0, type = "n", xlim = xlim, ylim = ylim,
         # xlab = "x Position (ft)", ylab = "y Position (ft)",
         xlab = "", ylab = "",
         xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", bty = "n", ...)
  }

  rect(-300, -100, 300, 500)
  # points(0, 0, cex = 3)
  circle(0, 0, 7.5, ...)  # hoop
  # segments(47, 0, 47, 50, ...)
  # rect(-30, -7.5, 30, -8.75, col = "black", ...)
  theta1 <- acos((25 - 35 / 12) / 23.75)
  # circle(5.25, 25, 23.75, -pi / 2 + theta1, pi / 2 - theta1, TRUE, ...)
  circle(0, 142.5, 60, 0, pi, lines = TRUE)  # top of key semicircle
  circle(0, 142.5, 60, pi, 2*pi, lines = TRUE, lty = 2) # bottom of key semicircle
  circle(0, 0, 30, 0, pi, lines = TRUE)  # restricted zone
  circle(0, 0, 236, 0 + theta1, pi - theta1, TRUE, ...)  # 3pt line arc
  # circle(0, 92.5, 220, 0, pi, lines = TRUE) # 3pt line arc
  # segments(-220, -47.5, -220, 92.5)  # 3pt line left side
  # segments(220, -47.5, 220, 92.5)  # 3pt line right side
  segments(-220, -47.5, -220, 86.5)  # 3pt line left side
  segments(220, -47.5, 220, 86.5)  # 3pt line right side
  # circle(5.25, 25, 23.75, 0, pi, TRUE, ...)
  # segments(0, 35 / 12, 5.25 + 23.75 * sin(theta1), 35 / 12, ...)
  # segments(0, 50 - 35/12, 5.25 + 23.75 * sin(theta1), 50 - 35 / 12, ...)
  rect(-60, -47.5, 60, 142.5, border = "black")  # inner paint
  rect(-80, -47.5, 80, 142.5, border = "gray")  # outer paint

  # circle(5.25, 25, 23.75, -pi / 2 + theta1, pi / 2 - theta1, TRUE, ...)
}

# CHANGE THIS TO POINT SAMPLING
#' Raster Half Court
#'
#' Construct a SpatialPolygonsDataFrame obejct out of a raster layer that
#' represents the NBA regulation half court.
#'
#' @return SpatialPolygonsDataFrame representing the NBA regulation half court
#'   as a grid of rectangular polygons.
#'
#' @examples
#' # create a half court raster SpatialPolygonsDataFrame object
#' sp_pol <- half_court_raster()
#'
#' @export

half_court_raster <- function() {
  rl <- raster::raster(raster::extent(matrix( c(-300, -100, 300, 500), nrow=2)), nrow=100, ncol=100,
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  rl[] <- 1:raster::ncell(rl)
  # convert to spatialPolygonsDataFrame
  sp_pix <- as(rl, "SpatialPixelsDataFrame")
  sp_pol <- as(sp_pix, "SpatialPolygonsDataFrame")
  return(sp_pol)
}

#' Merge Shot Data with Half Court Lattice Shapefile
#'
#' Merge the shot chart data with a SpatialPolygonDataFrame representing an NBA
#' regulation half court. This makes it useful to construct a weight matrix that
#' can be used in spatial modeling.
#'
#' @param shpfile SpatialPolygonsDataFrame returned \code{from half_court_raster}.
#' @param shot_df Data frame object returned from \code{get_shotchart}.
#'
#' @return SpatialPolygonsDataFrame object which contains the spatial polygons
#'   along with additional data from(shot_df) in the data slot.
#'
#' @examples
#' # create a half court raster SpatialPolygonsDataFrame object
#' sp_pol <- half_court_raster()
#' # merge shot chart data with the spatial polygon (grid)
#' sp_pol <- merge_shot_data(sp_pol, nba::shots_cavs)
#' # plot the result
#' ## first deal with color
#' colors <- c("white", "black")
#' num_cols <- length(unique(sp_pol@data$counts))
#' plotclr <- colorRampPalette(colors)(num_cols)
#' breaks <- seq(0, max(max(unique(sp_pol@data$counts), na.rm = TRUE)), length.out = num_cols + 1)
#' colcode <- plotclr[findInterval(sp_pol@data$counts, vec = sort(unique(sp_pol@data$counts)))]
#' ## now plot grid
#' plot(sp_pol, col = colcode, border = "transparent")
#' ## now plot shots
#' draw_halfcourt(add = TRUE)
#'
#' @export

merge_shot_data <- function(shpfile, shot_df, hex = FALSE) {
  # dat <- dplyr::select(shot_df, "LOC_X", "LOC_Y", "EVENT_TYPE")
  # dat <- dat[-which(dat$LOC_Y > 500),]
  shot_coords <- which(names(shot_df) %in% c("LOC_X", "LOC_Y"))
  spdf <- SpatialPointsDataFrame(coords = shot_df[,shot_coords], data = shot_df,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  spdf <- spTransform(spdf, proj4string(shpfile))

  if (hex)
    spdf@data$loc <- over(spdf, shpfile)
  else
    spdf@data$loc <- over(spdf, shpfile)$layer

  # construct counts
  counts <- table(spdf@data$loc)

  if (hex) shpfile <- as(shpfile, "SpatialPolygonsDataFrame")

  shot_info <- group_by(spdf@data, loc) %>%
    summarise(HIT = sum(EVENT_TYPE == "Made Shot"), N = n()) %>%
    dplyr::select(loc, HIT, N) %>%
    arrange(loc)
  shot_info$FGP <- shot_info$HIT/shot_info$N

  shpfile@data$counts <- shpfile@data$HIT <- shpfile@data$N <- shpfile@data$FGP <- NA
  shpfile@data$counts[as.numeric(names(counts))] <- unname(counts)
  shpfile@data$HIT[shot_info$loc] <- shot_info$HIT
  shpfile@data$N[shot_info$loc] <- shot_info$N
  shpfile@data$FGP[shot_info$loc] <- shot_info$FGP

  return(shpfile)
}

#' Plot Hexagon Shot Chart
#'
#'
#' Plot hexagon shot chart for a player or set of players.
#'
#' @param dat Data frame object returned from {\link[nba]{get_shotchart}}.
#' @param player Player name string or vector of player name strings.
#' @param variable One of \code{"FGP"} or \code{"FGPvLeague"} to color code the
#'   hexagons according to player field goal percentage or the difference
#'   between player field goal percentage and the overall league, respectively.
#' @param cellsize Number of hexagon cells to construct along the x dimension. A
#'   larger value results in a finer lattice.
#' @param scale Logical. If set to \code{TRUE} then the hexagons are scaled
#'   according to shot frequency.
#' @param ... Additional arguments to pass to {\link[dplyr]{filter}}.
#'
#' @return Hexagons overlayed on a plot. Note that you need to run the function
#'   {\link[nba]{draw_halfcourt}} in order to plot the hexgons on the
#'   appropriate court.
#'
#' @examples
#' # plot shots by LeBron James in 2016-17
#' draw_halfcourt()
#' plot_hexes(shots_1617, player = "LeBron James", variable = "FGPvLeague",
#'            scale = T, cellsize = 15,
#'            LOC_Y <= 500)
#'
#' # omit the restricted zone
#' draw_halfcourt()
#' plot_hexes(shots_1617, player = "LeBron James", variable = "FGPvLeague",
#'            LOC_Y <= 500, SHOT_ZONE_BASIC != "Restricted Area")
#'
#'
#' @export
#'

plot_hexes <- function(dat, player, ..., variable = "FGP", cellsize = 15, scale = TRUE) {
  col_breaks <- 7
  dat_filt <- filter(dat, PLAYER_NAME %in% player, ...)
  dat_leagueomit <- filter(shots_1617, !PLAYER_NAME %in% player, ...)
  hex_grid <- nba:::half_court_hex(cellsize = cellsize)
  hex_grid_league <- hex_grid

  hex_grid <- nba:::merge_shot_data(hex_grid, dat_filt, hex = TRUE)
  if (variable == "FGPvLeague") {
    hex_grid_league <- nba:::merge_shot_data(hex_grid_league, dat_leagueomit, hex = TRUE)
    hex_grid@data$FGPvLeague <- hex_grid@data$FGP - hex_grid_league@data$FGP
  }

  indx <- which(variable == colnames(hex_grid@data))

  if (variable == "FGPvLeague") {
    # breaks <- seq(min(hex_grid@data[,indx], na.rm = TRUE), max(hex_grid@data[,indx], na.rm = TRUE), length.out = col_breaks)
    breaks <- seq(-1, 1, length.out = col_breaks)
  }
  else {
    # breaks <- seq(0, max(hex_grid@data[,indx], na.rm = TRUE), length.out = col_breaks)
    breaks <- seq(0, 1, length.out = col_breaks)
  }

  colorful <- c("#236e96", "#15b2d3", "#5abccc", "#ffbe42", "#ff7f00", "#f4543b", "#f4143b")
  # colorful_sel <- colorRampPalette(colorful)(col_breaks)
  colcode <- colorful[findInterval(hex_grid@data[,indx], breaks)]

  # scale (if applicable) and plot
  if (scale) {
    resized_hexes <- nba:::resize_hexes(hex_grid)
    plot(resized_hexes, col = colcode, border = NA, add = TRUE)
  }
  else {
    plot(hex_grid, col = colcode, border = NA, add = TRUE)
  }

  # include legend
  nba:::hex_legend(variable = variable, colorful = colorful, scale = scale)
  # title
  text(-300, 475, player, pos = 4, cex = 1.5)
  # subtitle
  if (variable == "FGP")
    text(-300, 450, "Shot Frequency and Success Rate", pos = 4, cex = 0.8)
  else
    text(-300, 450, "Shot Frequency and Performance Against Average", pos = 4, cex = 0.8)
}

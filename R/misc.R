#' Convert an NBA JSON file to a Data Frame

json2df <- function(obj, index = 1) {
  header <- obj$resultSets[[index]]$headers
  container <- list()
  for (i in 1:length(header)) {
    container[[i]] <- sapply(obj$resultSets[[index]]$rowSet, "[[", i)
    container[[i]] <- unlist(lapply(container[[i]], function(x) ifelse(is.null(x), NA, x)))
  }
  out <- do.call("data.frame", args = list(container, stringsAsFactors = FALSE))
  names(out) <- header
  return(out)
}

#' Plot a Circle
#'
#' Helper function for plotting a circle
#'

circle <- function(x, y, r, from=0, to=2*pi, lines=FALSE, ...) {
  theta <- seq(from, to, length=100)
  if (lines)
    lines(x + r * cos(theta), y + r * sin(theta), ...)
  else polygon(x + r * cos(theta), y + r * sin(theta), ...)
}

#' Half-court Hexagon Lattice
#'
#' Construct a half-court hexagon lattice for a given cell size.
#'

half_court_hex <- function(cellsize = 20) {
  rl <- raster::raster(raster::extent(matrix( c(-300, -100, 300, 500), nrow=2)), nrow=100, ncol=100,
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  rl[] <- 1:raster::ncell(rl)
  sp_pol <- as(rl, "SpatialPixelsDataFrame")
  hex_points <- sp::spsample(sp_pol, type = "hexagonal",
                             cellsize = cellsize,
                             offset = c(1,1), bb = bbox(rl))
  hex_grid <- sp::HexPoints2SpatialPolygons(hex_points)
  return(hex_grid)
}

#' Origin Hexgon
#'
#' Construct a hexagon that has the same area a hexagon polygon from
#' \code{\link[nba]{half_court_hex}} with center at the origin.
#'

new_hex <- function(hex_origin, coordinates, scale = 1) {
  out <- sweep(hex_origin * scale, 2, coordinates, `+`)
  return(out)
}

#' Resize Hexagons
#'
#' Resize hexagons according to the frequency of shots binned in the hexagon
#' cell.
#'
#' @examples
#'
#' # Construct hexagon shot chart (area/color varying)
#' hex_grid <- half_court_hex(cellsize = 20)
#' hex_grid <- merge_shot_data(hex_grid, dat_filt, hex = TRUE)
#' resized_hexes <- resize_hexes(hex_grid)
#' # Plot hexagon shot chart
#' breaks <- seq(0, max(hex_grid@data$FGP, na.rm = TRUE), length.out = 10)
#' plotclr <- c("#236e96", "#15b2d3", "#5abccc", "#ffbe42", "#ff7f00", "#f4543b", "#f4143b")
#' colcode <- plotclr[findInterval(resized_hexes@data$FGP, breaks)]
#' draw_halfcourt()
#' plot(resized_hexes, col = colcode, border = NA, add = TRUE)
#'
#'

resize_hexes <- function(hex_grid) {
  coords <- coordinates(hex_grid)
  hex_origin <- sweep(slot(slot(hex_grid@polygons[[1]], "Polygons")[[1]], "coords"), 2, abs(coords[1,]), `+`)

  resized_hexes <- hex_grid
  # min_val <- min(hex_grid@data$counts, na.rm = TRUE)
  # max_val <- max(hex_grid@data$counts, na.rm = TRUE)
  # scaling <- (hex_grid@data$counts - min_val) / (max_val - min_val)

  # scaling <- tanh(hex_grid@data$counts)
  scaling <- hex_grid@data$counts
  counts <- na.omit(unique(hex_grid@data$counts))
  len <- length(counts)
  replacement <- seq(0.1, 1, length.out = len)
  for (i in 1:len) {
    indxs <- which(counts[i] == hex_grid@data$counts)
    scaling[indxs] <- replacement[i]
  }

  for (i in 1:length(resized_hexes@polygons)) {
    if (!is.na(scaling[i])) {
      slot(slot(resized_hexes@polygons[[i]], "Polygons")[[1]], "coords") <-
        new_hex(hex_origin, coords[i,], as.numeric(as.character(scaling[i])))
    }
  }

  return(resized_hexes)
}

hex_legend <- function(variable, colorful, scale = TRUE) {
  hex_grid <- nba:::half_court_hex(cellsize = 15)
  # legend
  coords <- coordinates(hex_grid)
  hex_origin <- sweep(slot(slot(hex_grid@polygons[[1]], "Polygons")[[1]], "coords"), 2, abs(coords[1,]), `+`)
  # size legend
  if (scale) {
    slot(slot(hex_grid@polygons[[44]], "Polygons")[[1]], "coords") <-
      nba:::new_hex(hex_origin, coords[44,], 0.5)
    plot(hex_grid[44,], col = "grey", border = NA, add = TRUE)
    plot(hex_grid[5,], col = "grey", border = NA, add = TRUE)
    text(coords[44,1], coords[44,2], "Low", pos = 2, cex = 0.5, col = "#808080")
    text(coords[5,1], coords[5,2], "High", pos = 4, cex = 0.5, col = "#808080")
  }
  # color legend
  col_leg <- c(30,31,32,33,34)
  for (item in col_leg) {
    slot(slot(hex_grid@polygons[[item]], "Polygons")[[1]], "coords") <-
      nba:::new_hex(hex_origin, coords[item,], 0.9)
  }
  plot(hex_grid[30,], col = colorful[1], border = NA, add = TRUE)
  plot(hex_grid[31,], col = colorful[3], border = NA, add = TRUE)
  plot(hex_grid[32,], col = colorful[4], border = NA, add = TRUE)
  plot(hex_grid[33,], col = colorful[5], border = NA, add = TRUE)
  plot(hex_grid[34,], col = colorful[7], border = NA, add = TRUE)
  if (variable != "FGPvLeague") {
    text(coords[30,1], coords[30,2], "Low", pos = 2, cex = 0.5, col = "#808080")
    text(coords[34,1], coords[34,2], "High", pos = 4, cex = 0.5, col = "#808080")
  }
  else {
    text(coords[30,1], coords[30,2], "Below\nAverage", pos = 2, cex = 0.5, col = "#808080")
    text(coords[34,1], coords[34,2], "Above\nAverage", pos = 4, cex = 0.5, col = "#808080")
  }
}

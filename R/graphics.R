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

draw_halfcourt <- function(xlim = c(-300,300), ylim = c(-100,500), ...) {
  plot(0, type = "n", xlim = xlim, ylim = ylim,
       xlab = "x Position (ft)", ylab = "y Position (ft)",
       xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n", bty = "n")

  rect(-300, -100, 300, 500, ...)
  # points(0, 0, cex = 3)
  circle(0, 0, 7.5)
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
  rect(-60, -47.5, 60, 142.5, border = "black", ...)  # inner paint
  rect(-80, -47.5, 80, 142.5, border = "gray", ...)  # outer paint

  # circle(5.25, 25, 23.75, -pi / 2 + theta1, pi / 2 - theta1, TRUE, ...)
}

#' Plot a Circle
#'
#' Helper function for plotting a circle

circle <- function(x, y, r, from=0, to=2*pi, lines=FALSE, ...) {
  theta <- seq(from, to, length=100)
  if (lines)
    lines(x + r * cos(theta), y + r * sin(theta), ...)
  else polygon(x + r * cos(theta), y + r * sin(theta), ...)
}

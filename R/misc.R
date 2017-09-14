#' Convert an NBA JSON file to a Data Frame

json2df <- function(obj) {
  header <- obj$resultSets[[1]]$headers
  container <- list()
  for (i in 1:length(header)) {
    container[[i]] <- sapply(obj$resultSets[[1]]$rowSet, "[[", i)
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

#' Convert an NBA JSON file to a Data Frame
#' @export

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

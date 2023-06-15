#' Plot the contents of a plate
#'
#' @param plate a [`Plate`] object.
#' @param x One reagent of a plate.
#' @param title a title for the plot.
#'
#' @family Plate functions
#'
#' @return A [`ggplot2::ggplot`] object.
#'
#' @author Charles Plessy
#'
#' @export

plateMap <- function(plate, x, title=x) {

  if(isFALSE(requireNamespace('viridis', quietly = TRUE)))
    stop("Please install the viridis package to use this function.")

  if(isFALSE(requireNamespace('ggplot2')))
    stop("Please install the ggplot2 package to use this function.")

  platetools::raw_map(plate[[x]], well=rownames(plate), plate=plate@metadata$type) +
    ggplot2::ggtitle(title) +
    viridis::scale_fill_viridis(trans = "log")
}

#' @rdname plateMap
#' @export

plateMap_all <- function(plate) {
  if(isFALSE(requireNamespace('ggplot2'))) stop("Please install the ggplot2 package to use this function.")
  x <- lapply(colnames(plate), function(x) ifelse(is.na(plate[,x]), "", x))
  names(x) <- colnames(plate)
  x <- as.data.frame(x)
  x <- apply( x, 1
              , function(x)
                if(all(x=="")) {
                  NA
                } else {
                  paste(unique(x[x!=""]), collapse=",")
                })
  platetools::raw_map(x, well=rownames(plate), plate="384")
}

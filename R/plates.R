#' Well coordinates
#'
#' Objects of this class represent a position in a microwell plate.  This class
#' exists so that the validity of the coordinates can be tested (for instance
#' `M16` will be rejected for a 96-well plate).
#'
#' @slot well A position in the microwell plate.
#' @slot plateFormat A plate format.
#'
#' @examples
#' well <- Well(well = "A01", plateFormat = "384")
#' well
#' as.character(well)
#' show(well)
#' Row(well)
#'
#' @import methods
#' @export Well
#' @exportClass Well

setClass("Well",
  slots = c(
    well        = "character",
    plateFormat = "character"),
  prototype = list(
    well        = "",
    plateFormat = "undefined")
)

Well <- function(well, plateFormat = "384") {
  w <- new("Well", well = well, plateFormat = plateFormat)
  if (validObject(w)) w
}

setMethod("as.character", "Well", function(x) x@well)

setMethod("show", "Well", function(object) cat(
  paste0(object@well, " (", object@plateFormat, "-well format)"))
)

validPlateFormats <- c("undefined", "96", "384")

setValidity("Well", function(object) {
  if (! object@plateFormat %in% validPlateFormats)
    return(paste0( "Supported plate formats: "
                   , paste(sapply(validPlateFormats, dQuote), collapse = ", ")
                   , "."))
  if (object@well == "")
    return("Missing well coordinates.")
  validRow(object)
})

#' The row of a well
#'
#' Given a well object, tells in which row the well is.
#'
#' @return
#' A row name in character format.
#'
#' @examples
#' well <- Well(well = "A01", plateFormat = "384")
#' Row(well)
#'
#' @export

setGeneric("Row", function(object) standardGeneric("Row"))
setMethod("Row", "Well", function(object)
  gsub("[[:digit:]]+", "", object@well))

#' Tests the well's row name is valid
#'
#' @return TRUE if valid, otherwise returns an error
#'
#' @rdname validationFunctions
#' @examples
#' validRow(Well("A01", "96"))

validRow <- function(w) {
  pf <- w@plateFormat
  if (pf == "undefined") return(TRUE)
  validRows <- list(
     "96" = LETTERS[1:8],
    "384" = LETTERS[1:16]
  )
  r <- Row(w)
  if (! r %in% validRows[[pf]])
    stop("Row name ‘", r, "’ is invalid for plate format ", pf, ".")
  TRUE
}

#' The column of a well
#'
#' Given a well object, tells in which column the well is.
#'
#' @return
#' A column name in character format
#'
#' @rdname validationFunctions
#' @examples
#' well <- Well("A01")
#' Column(well)
#'
#' @export

setGeneric("Column", function(object) standardGeneric("Column"))
setMethod("Column", "Well", function(object)
  gsub("[[:alpha:]]+", "", object@well))

#' Tests the well's column number is valid
#'
#' @return TRUE if valid, otherwise returns an error
#'
#' @examples
#' validColumn(Well("A01"))

validColumn <- function(w) {
  pf <- w@plateFormat
  if (pf == "undefined") return(TRUE)
  validColumns <- list(
     "96" = 1:12,
    "384" = 1:24
  )
  col <- Column(w)
  if (! as.numeric(col) %in% validColumns[[pf]])
    stop("Column number ‘", col, "’ is invalid for plate format ", pf, ".")
  TRUE
}

#' Move to the next well
#'
#' From left to right, and up to down, give the position of the next well.
#'
#' @param well A \code{\link{Well}} object.
#'
#' @examples
#'
#' nextWell(Well("A12", plateFormat = "96"))
#' nextWell(Well("A12", plateFormat = "384"))
#' nextWell(Well("A24", plateFormat = "384"))
#'
#' @return A \code{\link{Well}} object.
#'
#' @export

setGeneric("nextWell", function(well) standardGeneric("nextWell"))
setMethod ("nextWell", "Well", function(well) {
  if (well@plateFormat == "undefined") stop("Can not use undefined format.")
  plateFormat <- as.numeric(well@plateFormat)
  wells <- num_to_well(1:plateFormat, plate = plateFormat)
  position <- which(wells == well@well)
  if (position >= plateFormat) stop("Next well out of range.")
  Well(well=wells[position + 1], plateFormat = well@plateFormat)
})


#' Well names of plate type
#'
#' Lists the well names of a known well plate format, from left to right
#' and top to bottom.
#'
#' @param type The name a plate type (character)
#'
#' @return
#' Returns a character vector of well names.
#'
#' @examples
#' PlateTypeToWellNames("96")
#' PlateTypeToWellNames("384")
#'
#' @importFrom platetools num_to_well
#' @export

PlateTypeToWellNames <- function(type = c("96", "384")) {
  switch(match.arg(type),
     "384" = {platetools::num_to_well(1:384,plate = 384)},
     "96" = {platetools::num_to_well(1:96,plate = 96)}
  )
}

#' A multiwell plate
#'
#' Objects of this class represent a multiwell plate.
#'
#' @slot type A plate type such as `"384"`, `"96"`, etc.`
#' @slot deadVolume The dead volume of wells in this plate.
#' @slot maxVolume The maxiumum colume of wells in this plate.
#'
#' @examples
#' plate <- Plate( type       = "384"
#'               , deadVolume = 10000
#'               , maxVolume  = 100000)
#' plate
#'
#' @importFrom S4Vectors DataFrame metadata SimpleList

.Plate <- setClass("Plate",  contains = "DataFrame")

#' @export

Plate <- function(type, deadVolume = NULL, maxVolume = NULL) {
  rownames <- PlateTypeToWellNames(type)
  metadata <- list(type = type,
                   deadVolume = deadVolume,
                   maxVolume = maxVolume)
  DF <- DataFrame(row.names = rownames)
  metadata(DF) <- metadata
  .Plate(DF)
}

setMethod("show", "Plate", function(object) cat(
  paste0( "A Plate with data about ", nrow(object), " wells "
          , "(dead volume: ", metadata(object)$deadVolume
          , "; max volume: ", metadata(object)$maxVolume, ")."))
)

#' Set the contents of a well
#'
#' @param plate A [`Plate`] object.
#' @param well A [`Well`] object.
#' @param what A reagent name (character value).
#' @param volume A volume in nanoliters (numeric value).
#'
#' @return Returns a [`Plate`] object.
#'
#' @examples
#' example("Plate-class")
#' sourcePlate <- plate
#' A01 <- Well(well = "A01", plateFormat = "384")
#' sourcePlate <- setWell(sourcePlate, A01, "dNTP", 100000)
#' sourcePlate <- setWell(sourcePlate, Well(well = "A02", plateFormat = "384"), "dNTP", 100000)
#' sourcePlate <- setWell(sourcePlate, Well(well = "A03", plateFormat = "384"), "buffer", 100000)
#' DataFrame(sourcePlate)
#' @export

setGeneric("setWell", function(plate, well, what, volume)
  standardGeneric("setWell"))

.setWell <- function(plate, well, what, volume) {
  if (!what %in% colnames(plate)) {
    emptycolumn <- DataFrame(row.names = 1:nrow(plate))
    emptycolumn[,what] <- NA
    plate <- .Plate(cbind(plate, emptycolumn))
  }
  plate[as.character(well), what] <- volume
  if (validObject(plate)) plate
}

#' @export

setMethod( "setWell", c("Plate", "Well", "character", "numeric"), .setWell)

#' @export

setMethod( "setWell", c("Plate", "Well", "character", "logical")
           , function (plate, well, what, volume) {
             if(! is.na(volume)) stop("NA expected")
             .setWell (plate, well, what, volume)
           })


#' Get reagent name
#'
#' In a source plate, get the name of the reagent contained in a given well.
#'
#' @param plate A [`Plate`] object.
#' @param well A [`Well`] object.
#'
#' @examples
#'
#' sourcePlate <- Plate(wells = DataFrame(well = platetools::num_to_well(1:384, plate = 384)))
#' sourcePlate %<>%
#'   setWell(Well(well = "A01", plateFormat = "384"), "dNTP", 100000) %>%
#'   setWell(Well(well = "A02", plateFormat = "384"), "dNTP", 100000) %>%
#'   setWell(Well(well = "A03", plateFormat = "384"), "buffer", 100000)
#'
#' sourcePlate %>% sourceReagent(Well(well = "A01"))
#' sourcePlate %>% sourceReagent(Well(well = "A03"))
#' sourcePlate %>% sourceReagent()
#'
#' @family Plate method
#'
#' @export

setGeneric("sourceReagent", function(plate, well) standardGeneric("sourceReagent"))

setMethod("sourceReagent", c("Plate", "Well"), function(plate, well) {
  wellName <- well@well
  plateTable <- plate@plate
  plateRow <- plateTable[plateTable$well == wellName, ]
  plateRow <- plateRow[, -1] # Removing the "well" column.
  index <- which(!is.na(plateRow))
  if (length(index) == 0)
    stop("No reagent in well ", wellName, ".")
  if (length(index) > 1)
    stop( "More than one reagent in well", wellName
        , ". This should not happen in source plates")
  colnames(plateRow)[index]
})

setMethod("sourceReagent", c("Plate", "missing"), function(plate, well) {
  colnames(plate@plate)[-1]
})

#' Get reagent volume
#'
#' In a given plate, get the volume of the reagent contained in a given well.
#'
#' @param plate A \code{\link{Plate}} object.
#' @param well A \code{\link{Well}} object.
#'
#' @examples
#'
#' destPlate <- Plate(plate = tibble::tibble(well = platetools::num_to_well(1:384, plate = "384")))
#' destPlate %<>%
#'   setWell(Well(well = "A01", plateFormat = "384"), "dNTP", 50) %>%
#'   setWell(Well(well = "A02", plateFormat = "384"), "dNTP", 100) %>%
#'   setWell(Well(well = "A01", plateFormat = "384"), "buffer", 50)
#'
#' destPlate %>% plateWellVolume(Well(well = "A01"))
#' destPlate %>% plateWellVolume(Well(well = "A01"), "dNTP")
#' destPlate %>% plateWellVolume(Well(well = "A01"), "buffer")
#'
#' destPlate %>% plateWellVolume(Well(well = "A02"))
#' destPlate %>% plateWellVolume(Well(well = "A02"), "dNTP")
#' destPlate %>% plateWellVolume(Well(well = "A02"), "buffer")
#'
#' @export

setGeneric("plateWellVolume", function(plate, well, what) standardGeneric("plateWellVolume"))

setMethod("plateWellVolume", c("Plate", "Well", "missing"), function(plate, well, what) {
  if (ncol(plate@plate) == 1) return(0)
  wellName   <- well@well
  plateTable <- plate@plate
  plateRow   <- plateTable[plateTable$well == wellName, ]
  plateRow   <- plateRow[, -1] # Removing the "well" column.
  sum(plateRow, na.rm = TRUE)
})

setMethod("plateWellVolume", c("Plate", "Well", "character"), function(plate, well, what) {
  if (ncol(plate@plate) == 1) return(0)
  wellName   <- well@well
  plateTable <- plate@plate
  plateRow   <- plateTable[plateTable$well == wellName, ]
  plateRow   <- plateRow[, -1] # Removing the "well" column.
  vol <- plateRow[[what]]
  if(is.null(vol)) return(0)
  if(is.na(vol)) return(0)
  vol
})


#' Find a well that can provide enough reagent
#'
#' Given a \code{\link{Plate}} object, check if the reagent is available, and
#' if yes, in which well.
#'
#' @param object A \code{\link{Plate}} object.
#' @param reagent A reagent name.
#' @param start A \code{\link{Well}} object (to avoid backtracking).
#'
#' @return a \code{\link{Well}} object.
#'
#' @examples
#' sourcePlate <- Plate(plate = tibble::tibble(well = platetools::num_to_well(1:384, plate = "384")))
#' sourcePlate %<>%
#'   setWell(Well(well = "A01", plateFormat = "384"), "dNTP", 100000) %>%
#'   setWell(Well(well = "A02", plateFormat = "384"), "dNTP", 100000) %>%
#'   setWell(Well(well = "A03", plateFormat = "384"), "buffer", 100000)
#'
#' seekReagent(sourcePlate, "buffer")
#'
#' @export

setGeneric("seekReagent", function(object, reagent, start) standardGeneric("seekReagent"))

setMethod ("seekReagent", c("Plate", "character", "Well"), function(object, reagent, start) {
  plateTable <- object@plate
  # Truncate the table to the start position
  plateTable <- plateTable[which(plateTable$well == start@well):nrow(plateTable),]
  # Remove empty wells
  plateTable <- plateTable[!as.vector(is.na(plateTable[,reagent])),]
  well <- plateTable$well[1]
  if (is.na(well)) return(NA)
  Well(well=well)
})

setMethod ("seekReagent", c("Plate", "character", "missing"), function(object, reagent, start) {
  seekReagent(object, reagent, Well(well="A01", plateFormat = "undefined"))
})

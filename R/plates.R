#' Well coordinates
#'
#' Objects of this class represent a position in a microwell plate.  This class
#' exists so that the validity of the coordinates can be tested (for instance
#' `M16` will be rejected for a 96-well plate).
#'
#' @slot well A position in the microwell plate.
#' @slot plateFormat A plate format.
#'
#' @family Well functions
#'
#' @examples
#' well <- Well("A01", plateFormat = "384")
#' well
#' as.character(well)
#' show(well)
#' Row(well)
#' Column(well)
#'
#' @rdname Well
#' @import methods
#' @aliases Well
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

#' @rdname Well-class
#' @param x a [`Well`] object
#' @export

setMethod("as.character", "Well", function(x) x@well)

#' @rdname Well-class
#' @param object a [`Well`] object
#' @export

setMethod("show", "Well", function(object) cat(
  paste0(object@well, " (", object@plateFormat, "-well format)"))
)

validPlateFormats <- c("undefined", "6", "96", "384")

setValidity("Well", function(object) {
  if (! object@plateFormat %in% validPlateFormats)
    return(paste0( "Supported plate formats: "
                   , paste(sapply(validPlateFormats, dQuote), collapse = ", ")
                   , "."))
  if (object@well == "")
    return("Missing well coordinates.")
  validRow(object)
  validColumn(object)
})

#' The row of a well
#'
#' Given a well object, tells in which row the well is.
#'
#' @param object The [`Well`] object from which row coordinates are extracted.
#'
#' @family Well functions
#'
#' @return
#' A row name in character format.
#'
#' @examples
#' well <- Well(well = "A01", plateFormat = "384")
#' Row(well)
#'
#' @family Plate functions
#' @rdname Well-class
#' @export

setGeneric("Row", function(object) standardGeneric("Row"))

#' @rdname Well-class
#' @export

setMethod("Row", "Well", function(object)
  gsub("[[:digit:]]+", "", object@well))

#' Tests the well's row name is valid
#'
#' @return TRUE if valid, otherwise returns an error
#' @param w A [`Well`].
#'
#' @family Well functions
#'
#' @rdname validationFunctions

validRow <- function(w) {
  pf <- w@plateFormat
  if (pf == "undefined") return(TRUE)
  validRows <- list(
      "6" = LETTERS[1:2],
     "96" = LETTERS[1:8],
    "384" = LETTERS[1:16]
  )
  r <- Row(w)
  if (! r %in% validRows[[pf]])
    stop("Row name ", sQuote(r), " is invalid for plate format ", pf, ".")
  TRUE
}

#' The column of a well
#'
#' Given a well object, tells in which column the well is.
#'
#' @param object The [`Well`] object from which column coordinates are extracted.
#'
#' @family Well functions
#'
#' @return
#' A column name in character format
#'
#' @family Plate functions
#'
#' @examples
#' well <- Well("A01")
#' Column(well)
#'
#' @rdname Well-class
#' @export

setGeneric("Column", function(object) standardGeneric("Column"))

#' @rdname Well-class
#' @export
#'
setMethod("Column", "Well", function(object)
  gsub("[[:alpha:]]+", "", object@well))

#' Tests the well's column number is valid
#'
#' @param w A [`Well`].
#' @return TRUE if valid, otherwise returns an error
#'
#' @rdname validationFunctions

validColumn <- function(w) {
  pf <- w@plateFormat
  if (pf == "undefined") return(TRUE)
  validColumns <- list(
      "6" = 1:03,
     "96" = 1:12,
    "384" = 1:24
  )
  col <- Column(w)
  if (! as.numeric(col) %in% validColumns[[pf]])
    stop("Column number ", sQuote(col), " is invalid for plate format ", pf, ".")
  TRUE
}

#' Move to the next well
#'
#' From left to right, and up to down, give the position of the next well.
#'
#' @param well A [`Well`] object.
#'
#' @family Well functions
#'
#' @examples
#'
#' nextWell(Well("A12", plateFormat =  "96"))
#' nextWell(Well("A12", plateFormat = "384"))
#' nextWell(Well("A24", plateFormat = "384"))
#' nextWell(Well("A03", plateFormat =   "6"))
#'
#' @return A [`Well`] object.
#'
#' @family Plate functions
#'
#' @export

setGeneric("nextWell", function(well) standardGeneric("nextWell"))

#' @rdname nextWell
#' @export
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
#' @family Plate functions
#' @family Well functions
#'
#' @examples
#' PlateTypeToWellNames("6")
#' PlateTypeToWellNames("96")
#' PlateTypeToWellNames("384")
#'
#' @importFrom platetools num_to_well
#' @export

PlateTypeToWellNames <- function(type = c("6", "96", "384")) {
  switch(match.arg(type),
     "384" = {platetools::num_to_well(1:384,plate = 384)},
      "96" = {platetools::num_to_well(1:96, plate =  96)},
       "6" = {platetools::num_to_well(1:6,  plate =   6)}
  )
}

#' A multiwell plate
#'
#' Objects of this class represent a multiwell plate.
#'
#' @slot type A plate type such as `"384"`, `"96"`, etc.`
#' @slot deadVolume The dead volume of wells in this plate.
#' @slot maxVolume The maximum volume of wells in this plate.
#'
#' @examples
#' plate <- Plate( type       = "384"
#'               , deadVolume = 10000
#'               , maxVolume  = 100000)
#' plate
#'
#' @family Plate functions
#'
#' @aliases Plate
#' @importFrom S4Vectors DataFrame metadata metadata<- SimpleList

.Plate <- setClass("Plate",  contains = "DFrame")

#' @aliases Plate
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

#' @rdname Plate-class
#' @param x A [`Plate`] object.
#' @param n The number of wells to be displayed.
#' @param ... arguments to be passed to or from other methods.
#' @importFrom utils head
#' @export

setMethod("head", "Plate", function(x, n = 6L, ...) {
  head(DataFrame(x), n = n, ...)
})

#' @rdname Plate-class
#' @export

setMethod("as.character", "Plate", function(x) {
  deadVolume <- ifelse(is.null(metadata(x)$deadVolume), "unspecified", metadata(x)$deadVolume)
  maxVolume  <- ifelse(is.null(metadata(x)$maxVolume),  "unspecified", metadata(x)$maxVolume)
  paste0( "A Plate with data about ", nrow(x), " wells "
          , "(dead volume: ", deadVolume
          , "; max volume: ", maxVolume, ").")
})

#' @rdname Plate-class
#' @param object A [`Plate`] object
#' @export

setMethod("show", "Plate", function(object) cat(as.character(object)))

#' Set the contents of a well
#'
#' @param plate A [`Plate`] object.
#' @param well A [`Well`] object.
#' @param what A reagent name (character value).
#' @param volume A volume in nanoliters (numeric value).
#'
#' @return Returns a [`Plate`] object.
#'
#' @family Plate functions
#'
#' @examples
#' sourcePlate <- Plate("384", deadVolume = 1e4, maxVolume = 1e5)
#' A01 <- Well("A01")
#' sourcePlate <- setWell(sourcePlate, A01, "dNTP", 100000)
#' sourcePlate <- setWell(sourcePlate, Well("A02"), "dNTP",   100000)
#' sourcePlate <- setWell(sourcePlate, Well("A03"), "buffer", 100000)
#' head(sourcePlate)
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

#' @rdname setWell
#' @export

setMethod( "setWell", c("Plate", "Well", "character", "numeric"), .setWell)

#' @rdname setWell
#' @export

setMethod( "setWell", c("Plate", "Well", "character", "logical")
           , function (plate, well, what, volume) {
             if(! is.na(volume)) stop("NA expected")
             .setWell (plate, well, what, volume)
           })


#' Set values in rectangular areas of a plate
#'
#' Updates a [`Plate`] object representing a multiwell plate, by setting a given
#' value for all wells in a block or a list of blocks defined by the well
#' coordinates of their upper-left and bottom-right corners.
#'
#' This function wraps [`platetools::set_block()`].
#'
#' @param plate A [`Plate`] object representing a multiwell plate.
#' @param block Coordinates of a rectangular block (such as \dQuote{A01~B02}),
#'        or a vector of coordinates.
#' @param what A reagent name.
#' @param value The value to set.
#' @param add Add to the volume instead of overriding it.
#'
#' @return Returns the `Plate` object, where the values for the wells indicated
#' in the blocks have been updated.
#'
#' @examples
#' p <- Plate("96")
#' head(p)
#'
#' p <- p |>
#'   set_block(c("A01~B02", "A05~D05"), "dNTP", 0.25) |>
#'   set_block(    "A03",               "dNTP", 0.50)
#' head(p)
#'
#' # Be careful with the column names
#' p <- set_block(p, "A01~H12", "Mg2+", 3.0)
#' head(p)
#'
#' p <- set_block(p, "A02~A03", "dNTP", 1, add = TRUE)
#' head(p)
#'
#' @family Plate functions
#' @family Dispensing functions
#'
#' @export

setGeneric("set_block", function(plate, block, what, value, add = FALSE)
  standardGeneric("set_block"))

#' @rdname set_block
#' @export

setMethod( "set_block", c("Plate", "character", "character", "numeric")
         , function(plate, block, what, value, add) {
  plate$well <- rownames(plate)
  if (isTRUE(add)) {
    oldVolumes <- plate[,what]
    oldVolumes[is.na(oldVolumes)] <- 0
    plate <- platetools::set_block(plate, block, what, value)
    plate[,what] <- plate[,what] + oldVolumes
  } else {
    plate <- platetools::set_block(plate, block, what, value)
  }
  plate$well <- NULL
  plate
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
#' sourcePlate <- Plate( type       = "384"
#'                     , deadVolume = 10000
#'                     , maxVolume  = 100000)
#'
#' sourcePlate <- sourcePlate |>
#'   setWell(Well("A01"), "dNTP",   100000) |>
#'   setWell(Well("A02"), "dNTP",   100000) |>
#'   setWell(Well("A03"), "buffer", 100000)
#'
#' head(sourcePlate)
#'
#' sourcePlate |> sourceReagent(Well("A01"))
#' sourcePlate |> sourceReagent(Well("A03"))
#' sourcePlate |> sourceReagent()
#'
#' @family Plate functions
#'
#' @export

setGeneric("sourceReagent", function(plate, well) standardGeneric("sourceReagent"))

#' @rdname sourceReagent
#' @export

setMethod("sourceReagent", c("Plate", "Well"), function(plate, well) {
  wellName <- well@well
  plateTable <- DataFrame(plate)
  plateRow <- plateTable[wellName, ]
  index <- which(!is.na(plateRow))
  if (length(index) == 0)
    stop("No reagent in well ", wellName, ".")
  if (length(index) > 1)
    stop( "More than one reagent in well", wellName
        , ". This should not happen in source plates")
  colnames(plateRow)[index]
})

#' @rdname sourceReagent
#' @export

setMethod("sourceReagent", c("Plate", "missing"), function(plate, well) {
  colnames(DataFrame(plate))
})

#' Get reagent volume
#'
#' In a given plate, get the volume of the reagent contained in a given well.
#'
#' @param plate A [`Plate`] object.
#' @param well A [`Well`] object.
#' @param what The name of a reagent.
#'
#' @family Plate functions
#'
#' @examples
#'
#' destPlate <- Plate( type       = "384"
#'                   , deadVolume = 10000
#'                   , maxVolume  = 100000)
#' destPlate <- destPlate |>
#'   setWell(Well("A01"), "dNTP",   50) |>
#'   setWell(Well("A02"), "dNTP",  100) |>
#'   setWell(Well("A01"), "buffer", 50)
#'
#' destPlate |> plateWellVolume(Well("A01"))
#' destPlate |> plateWellVolume(Well("A01"), "dNTP")
#' destPlate |> plateWellVolume(Well("A01"), "buffer")
#'
#' destPlate |> plateWellVolume(Well("A02"))
#' destPlate |> plateWellVolume(Well("A02"), "dNTP")
#' destPlate |> plateWellVolume(Well("A02"), "buffer")
#'
#' @export

setGeneric("plateWellVolume", function(plate, well, what) standardGeneric("plateWellVolume"))

#' @rdname plateWellVolume
#' @export

setMethod("plateWellVolume", c("Plate", "Well", "missing"), function(plate, well, what) {
  wellName   <- well@well
  plateTable <- DataFrame(plate)
  if (ncol(plateTable) == 0) return(0)
  plateRow   <- plateTable[wellName,]
  sum(unlist(plateRow), na.rm = TRUE)
})

#' @rdname plateWellVolume
#' @export

setMethod("plateWellVolume", c("Plate", "Well", "character"), function(plate, well, what) {
  wellName   <- well@well
  plateTable <- DataFrame(plate)
  if (ncol(plateTable) == 0) return(0)
  if (ncol(plateTable) == 1) {
    vol      <- plateTable[wellName,]
  } else {
    plateRow <- plateTable[wellName,]
    vol <- plateRow[[what]]
  }
  if(is.null(vol)) return(0)
  if(is.na(vol)) return(0)
  vol
})


#' Find a well that can provide enough reagent
#'
#' Given a [`Plate`] object, check if the reagent is available, and
#' if yes, in which well.  The search can be restricted to exclude the wells
#' positioned before (in alphanumeric order) an arbitrary position, to avoid
#' backtracking to wells already emptired.
#'
#' @param object A [`Plate`] object.
#' @param reagent A reagent name.
#' @param start A [`Well`] object (to avoid backtracking).
#'
#' @return a `Well` object.
#'
#' @family Plate functions
#'
#' @examples
#' seekReagent(examplePlate, "buffer")
#'
#' @export

setGeneric("seekReagent", function(object, reagent, start) standardGeneric("seekReagent"))

#' @rdname seekReagent
#' @export

setMethod ("seekReagent", c("Plate", "character", "Well"), function(object, reagent, start) {
  plateTable <- DataFrame(object)
  # Truncate the table to the start position
  plateTable <- plateTable[which(rownames(plateTable) == as.character(start)):nrow(plateTable),]
  # Remove empty wells
  plateTable <- plateTable[!as.vector(is.na(plateTable[,reagent])),]
  well <- rownames(plateTable)[1]
  if (is.na(well)) return(NA)
  Well(well, plateFormat = metadata(object)$type)
})

#' @rdname seekReagent
#' @export

setMethod ("seekReagent", c("Plate", "character", "missing"), function(object, reagent, start) {
  seekReagent(object, reagent, Well(well="A01", plateFormat = "undefined"))
})


#' Randomise reaction coordinates
#'
#' Randomise the coordinates of the reactions in a plate, so that reactions with
#' similar parameters are less likely to be affected by the same spatial
#' technical biases.  This is also useful when performing plate replicates.
#' Finally, the coordinates of negative controls will usually make a
#' non-symmetric pattern that helps to detect when a plate was rotated
#' accidentally.
#'
#' @note Working with random seeds is tricky.  In particular, pay attention that
#' if you set the random seed just before running the `randomise` function, its
#' results will become deterministic.
#'
#' @param plate A [`Plate`] object.
#' @param seed An integer number to set the seed (optional).
#' @param vector A (usually random) vector to sort the plate with (optional).
#'
#' @return a `Plate` object in which the randomisation vector was stored in
#' its `metadata` slot.
#'
#' @author Charles Plessy
#'
#' @family Plate functions
#'
#' @examples
#' # Toy example of a 6-well plate containing a gradient increase of a reagent
#' p <- Plate(type = "6") |> set_block("A01~B03", "reagent", 1:6 * 10)
#' head(p)
#'
#' # Setting the same seed returns the same randomisation
#' randomise(p, seed = 1111) |> head()
#' randomise(p, seed = 1111) |> head()
#'
#' # Not providing a seed returns a really random order
#' randomise(p) |> head()
#' randomise(p) |> head()
#'
#' # The random order can also be passed with the vector argument
#' randomise(p, vector = sample(1:6)) |> head()
#'
#' # This means also that order can also be forced
#' randomise(p, vector = 6:1) |> head()
#'
#' # The randomisation vector is stored in the Plate object's metadata
#' p <- randomise(p)
#' p@metadata$randomisation_vector
#'
#' @export

setGeneric("randomise", function(plate, seed, vector) standardGeneric("randomise"))

#' @rdname randomise
#' @importFrom stats runif
#' @export

setMethod ("randomise", c("Plate", "missing", "missing"), function(plate, seed, vector) {
  randomise(plate, seed = floor(stats::runif(1) * 1000000000))
})

#' @rdname randomise
#' @export

setMethod ("randomise", c("Plate", "numeric", "missing"), function(plate, seed, vector) {
  set.seed(seed)
  vector <- sample(1:nrow(plate))
  randomise(plate, vector = vector)
})

#' @rdname randomise
#' @export

setMethod ("randomise", c("Plate", "missing", "numeric"), function(plate, seed, vector) {
  if(length(vector) != nrow(plate)) stop("The randomisation vector's length is different from the number of wells in the plate.")
  plate <- plate[vector, ,drop = FALSE]
  plate@metadata$randomisation_vector <- vector
  rownames(plate) <- rownames(plate)[order(vector)]
  plate
})

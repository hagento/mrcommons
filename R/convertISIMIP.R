#' @title convertISIMIP
#' @description convert data to ISO country level
#' 
#' NOTE: For the purpose of calculating HDD/CDD data for EDGE-B, this function was
#' edited such that the subtype may also be a file name consisting of the 
#' variables "tas", "population", "rsds", "sfc" or "huss". (author: Hagen Tockhorn)
#'
#' @param x MAgPIE object on cellular level
#' @param subtype data subtype
#'
#' @importFrom magclass collapseNames collapseDim dimSums
#'
#' @return MAgPIE object on country level
#'
#' @author Jan Philipp Dietrich, Felicitas Beier
#' @seealso [readSource()]
#'
#' @examples
#' \dontrun{
#' a <- readSource("ISIMIP", convert = TRUE)
#' }
#'
convertISIMIP <- function(x, subtype) {
  if (grepl("^airww", subtype)) {
    landarea <- setYears(collapseNames(dimSums(readSource("LUH2v2", subtype = "states",
                                                          convert = "onlycorrect")[, "y1995", ], dim = 3)), NULL)
    landarea <- collapseDim(landarea, dim = "iso")
    weight   <- landarea
    weight   <- add_columns(weight, dim = 1, addnm = "178p75.-49p25", fill = 0) # add missing weight
    
    return(toolAggregateCell2Country(x, weight = weight, fill = 0))
  }
  
  
  
  # EDGE-B PART ----------------------------------------------------------------
  
  # fill dates for unnamed data
  fillDates <- function(r, filename, pop = FALSE) {
    if (grepl(".nc|.nc4", filename)) {
      filename <- gsub(".nc|.nc4", "", filename)}
    
    yStart <- stringr::str_sub(filename, -9, -6)
    n <- raster::nlayers(r)
    
    if (!pop) {
      dStart <- as.Date(paste0(yStart, "-1-1"))
      dates <- seq.Date(dStart, by = "day", length.out = n)
    } else {
      dates <- seq.Date(yStart, by = "year", length.out = n)
    }
    
    # fill dates
    names(r) <- dates
    return(r)
  }
  
  # variable units
  unitMapping <- list(
    "tas"     = "K",
    "rsds"    = "Wm-2",
    "sfcWind" = "ms-1",
    "huss"    = "kgkg-1"
  )
  # browser()
  
  edgeVars <- c("tas", "rsds", "sfcWind", "huss", "pop")
  
  if (grepl(paste(edgeVars, collapse = "|"), subtype)) {
    var <- edgeVars[stringr::str_detect(subtype, edgeVars)]
    
    if (var == "pop") {
      x <- fillDates(x, subtype, pop = TRUE)
    } else {
      x <- fillDates(x, subtype)
    }
    
    return(list(x = x,
                class = "RasterBrick",
                unit = unitMapping[var]))
  }
  
  else stop("Aggregation rule for given subtype \"", subtype, "\" not defined!")
  
  
}


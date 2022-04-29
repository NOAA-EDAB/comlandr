#' Assigns points to polygon
#'
#' Assign observer data (points, lat and lon) to designated regions (polygons) from a shape file.
#'
#'
# @inheritParams strat_prep
#' @param na.keep Boolean. Logical value to indicate whether original strata names
#'  should be retained.
#'
#' @return Returns a \code{comdiscData} data.table with one additional column labeled
#'  with the value of \code{areaDescription}
#'
#' \item{areaDescription}{The name of the region (found in \code{areaPolygon})
#'  that a record in \code{surveyData} is assigned to}
#'
#' @importFrom magrittr "%>%"
#'
#'@family comdisc
#'
#' @export


assign_area <- function (comdiscData, areaPolygon, areaDescription, na.keep = F) {

    # transform Regional Shape file using lambert conformal conic coordinate ref system
    crs <- "+proj=lcc +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-72 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
    
    areas <- areaPolygon %>%
    dplyr::rename(areaDescription = areaDescription) %>%
    sf::st_transform(., crs)

    #Need unique link3, lat lon column to make this work
    comdiscData[, linkLL := paste0(LINK3, LAT, LON)]
    #Should probably do this in the raw data pull
    #remove stations missing lat or lon
    comdiscData <- comdiscData[!is.na(LAT), ]
    comdiscData <- comdiscData[!is.na(LON), ]
    
    # find unique stations and transform to required crs
    locations <- comdiscData %>%
    dplyr::select(linkLL, LAT, LON) %>%
    dplyr::distinct() %>%
    sf::st_as_sf(., coords = c("LON","LAT"), crs=4326) %>%
    sf::st_transform(., crs)


    # Intersect the locations with the polygon
    # Assigns locations with polygons
    location_area <- sf::st_join(locations, areas, join = sf::st_intersects) %>%
    dplyr::select(names(locations), areaDescription) %>%
    sf::st_drop_geometry() %>%
    dplyr::arrange(linkLL)

    # Join observer data with locations (which now are assigned to an area based on the shape file)
    master <- base::merge(comdiscData, location_area, by = c("linkLL")) %>%
    dplyr::rename(!!areaDescription := areaDescription)

    # check to see if we want to keep points that fall outside of all of the polygons found in the shape file
    if (!(na.keep)) { # removes all points that fall outside of the areas defined by the polygons in stratum
    master <- master %>%
        dplyr::filter(!is.na(get(areaDescription))) %>%
        data.table::as.data.table()
  }
    #Drop linkLL column
    master[, linkLL := NULL]

  return(master[])

}


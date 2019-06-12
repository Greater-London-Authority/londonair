#' @title getMetaAQE
#' @description Get the meta data for all Air Quality England (AQE) monitoring sites in London
#' @param borough_sf A simple-features data frame containing London Borough polygons. , Default: NULL
#' @return A data frame containing meta data for all AQE monitoring sites in London
#' @details If borough_sf is not provided local authority name and inner/outer London category will not be returned.
#' London Borough boundary data is availabe from the \href{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}{London DataStore}
#' and \href{https://www.ordnancesurvey.co.uk/business-and-government/products/boundaryline.html}{Ordnance Survey}.
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # Without borough_sf
#' 
#' meta_data <- getMetaAQE()
#' 
#' # With borough_sf
#' boroughs <- st_read("London_Borough.shp")
#' meta_data <- getMetaAQE(borough_sf = boroughs)
#'  }
#' }
#' @rdname getMetaAQE
#' @export 
#' @import checkmate
#' @import sf
#' @import XML
#' @import dplyr
#' @importFrom stringr str_to_title
#' @importFrom stats setNames
getMetaAQE <- function(borough_sf = NULL) {

################################# Checks #######################################

  if (!(checkmate::test_null(borough_sf))) {
    checkmate::assert_class(borough_sf, "data.frame")
    checkmate::assert_names(
      x = names(borough_sf),
      must.include = c("LocalAuthorityName", "InnerOuterLondon")
    )
    borough_crs <- sf::st_crs(borough_sf)
  } else {
    warning("InnerOuterLondon and LocalAuthorityName
            variable will not be included as borough_sf not supplied")
  }

########################### Fetch xml data from url ############################

  aqe_sites_url <-
    "http://acer.aeat.com/gla-cleaner-air/api/v1/gla-cleaner-air/v1/site"

  # Parse the XML data to a dataframe
  sites <- aqe_sites_url %>%
    XML::xmlParse(file = .) %>%
    XML::xmlToList(node = .)

###################### Extract data for each site into df ######################

  df <- data.frame()

  for (i in seq_along(sites)) {
    site_data <- sites[[i]]
    site_data <- lapply(site_data, as.data.frame.list, stringsAsFactors = FALSE)
    attr <- site_data[[length(site_data)]]
    specs <- site_data[1:(length(site_data) - 1)] %>%
      do.call(rbind, .)
    site_data <- cbind(attr, specs)
    rownames(site_data) <- NULL
    df <- rbind(df, site_data)
  }

############################## Clean and tidy data #############################

  df <- df  %>%
    dplyr::mutate(Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           SiteCode = gsub("00", "", SiteCode),
           Network = "AQE",
           SiteType = stringr::str_to_title(SiteType),
           DateMeasurementFinished = as.Date(DateMeasurementFinished),
           DateMeasurementStarted = as.Date(DateMeasurementStarted)) %>%
    dplyr::filter(!(is.na(Longitude))) %>%
    dplyr::select(SiteName, SiteCode, SiteType, Latitude, Longitude,
                  SpeciesCode, DateMeasurementStarted, DateMeasurementFinished,
                  SiteLink, Network)

######################### Add borough data if provided #########################
  if (!(is.null(borough_sf))) {
    df <- df %>%
      sf::st_as_sf(., coords = c("Longitude", "Latitude"),
               crs = 4326, remove = FALSE) %>%
      sf::st_transform(borough_crs) %>%
      sf::st_join(., borough_sf) %>%
      sf::st_set_geometry(NULL) %>%
      as.data.frame() %>%
      dplyr::select(LocalAuthorityName, SiteName, SiteCode, SiteType,
                    InnerOuterLondon, Latitude, Longitude, SpeciesCode,
                    DateMeasurementStarted, DateMeasurementFinished, SiteLink,
                    Network) %>%
      dplyr::mutate(LocalAuthorityName = ifelse(is.na(LocalAuthorityName),
                                         "Outside Greater London",
                                         LocalAuthorityName),
             InnerOuterLondon = ifelse(is.na(InnerOuterLondon),
                                       "Outside Greater London",
                                       InnerOuterLondon))
  }

########################## Switch names to lower case ##########################

  df <- df %>%
    stats::setNames(gsub("([a-z]){1}([A-Z]){1}", "\\1_\\2", names(.))) %>%
    stats::setNames(tolower(names(.))) %>%
    dplyr::rename(site = site_name, code = site_code)

############################### Return dataframe ###############################

  return(df)

}

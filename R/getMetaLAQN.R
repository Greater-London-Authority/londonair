#' @title getMetaLAQN
#' @description Get the meta data for all London Air Quality Network (LAQN) monitoring sites in London
#' @param borough_sf A simple-features data frame containing London Borough polygons. , Default: NULL
#' @return A data frame containing meta data for all LAQN monitoring sites in London
#' @details If borough_sf is not provided local authority name and inner/outer London category will not be returned.
#' London Borough boundary data is availabe from the \href{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}{London DataStore}
#' and \href{https://www.ordnancesurvey.co.uk/business-and-government/products/boundaryline.html}{Ordnance Survey}.
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # Without borough_sf
#' 
#' meta_data <- getMetaLAQN()
#' 
#' # With borough_sf
#' boroughs <- st_read("London_Borough.shp")
#' meta_data <- getMetaLAQN(borough_sf = boroughs)
#'  }
#' }
#' @rdname getMetaLAQN
#' @export 
#' @import checkmate
#' @importFrom jsonlite fromJSON
#' @import dplyr
#' @importFrom tidyr uncount
#' @importFrom stringr str_to_title
#' @importFrom stats setNames
getMetaLAQN <- function(borough_sf = NULL) {

#################################### Checks ####################################

  if (!(checkmate::test_null(borough_sf))) {
    checkmate::assert_class(borough_sf, "data.frame")
    checkmate::assert_names(
      x = names(borough_sf),
      must.include = c("LocalAuthorityName", "InnerOuterLondon")
    )
  } else {
    warning("InnerOuterLondon variable will
             not be included as borough_sf not supplied")
  }

############################## Fetch json from URL #############################

  laqn_sites_url <-
    "http://api.erg.kcl.ac.uk/AirQuality/Information/MonitoringSiteSpecies/GroupName=London/Json"

  # Fetch json and convert to dataframe
  sites <- jsonlite::fromJSON(laqn_sites_url, flatten = FALSE)
  sites <- sites[["Sites"]][["Site"]]

  # Clean
  names(sites) <- gsub("@", "", names(sites))
  sites <- sites %>%
    dplyr::select(LocalAuthorityName, SiteName, SiteCode, SiteType,
           Latitude, Longitude, Species, SiteLink)

###################### Extract data for each site into df ######################

  df <- data.frame()

  for (row in seq_len(nrow(sites))) {
    site_data <- sites[row, ]
    site_info <- site_data %>%
      dplyr::select(-Species)
    species_info <- site_data[["Species"]][[1]]
    if (!(is.data.frame(species_info))) {
      species_info <- species_info %>%
        as.data.frame.list(stringsAsFactors = FALSE)
    }
    names(species_info) <- gsub("@|^X\\.", "", names(species_info))
    num_spec <- nrow(species_info)
    site_data <- site_info %>%
      dplyr::mutate(freq = num_spec) %>%
      tidyr::uncount(., freq) %>%
      cbind(., species_info)
    site_data <- site_data %>%
      dplyr::select(-SpeciesDescription)
    df <- df %>%
      rbind(., site_data)
  }

############################## Clean and tidy data #############################

  df <- df %>%
    dplyr::mutate(LocalAuthorityName = gsub("(Kingston|Richmond)",
                                     "\\1 upon Thames",
                                     LocalAuthorityName),
           Latitude = as.numeric(Latitude),
           Longitude = as.numeric(Longitude),
           LocalAuthorityName = ifelse(is.na(LocalAuthorityName),
                                       "Outside Greater London",
                                       LocalAuthorityName),
           Network = "LAQN",
           SiteType = stringr::str_to_title(SiteType),
           DateMeasurementFinished = as.Date(DateMeasurementFinished),
           DateMeasurementStarted = as.Date(DateMeasurementStarted)) %>%
    dplyr::filter(!(is.na(Longitude))) %>%
    dplyr::select(LocalAuthorityName, SiteName, SiteCode, SiteType,
           Latitude, Longitude, SpeciesCode, DateMeasurementStarted,
           DateMeasurementFinished, SiteLink, Network)

######################### Add borough data if provided #########################

  if (!(is.null(borough_sf))) {
    df <- df %>%
      dplyr::left_join(.,
                       as.data.frame(borough_sf),
                       by = "LocalAuthorityName") %>%
      dplyr::mutate(InnerOuterLondon = ifelse(is.na(InnerOuterLondon),
                                       "Outside Greater London",
                                       InnerOuterLondon)) %>%
      dplyr::select(LocalAuthorityName, SiteName, SiteCode, SiteType,
                    InnerOuterLondon, Latitude, Longitude, SpeciesCode,
                    DateMeasurementStarted, DateMeasurementFinished, SiteLink,
                    Network)
  }

################################## Sort names ##################################

  df <- df %>%
    stats::setNames(gsub("([a-z]){1}([A-Z]){1}", "\\1_\\2", names(.))) %>%
    stats::setNames(tolower(names(.))) %>%
    dplyr::rename(site = site_name, code = site_code)

############################## Return dataframe  ###############################

  return(df)

}

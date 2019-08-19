#' @title getMetaLondon
#' @description Get the meta data for all air quality monitoring sites in London
#' @param borough_sf A simple-features data frame containing London Borough polygons. , Default: NULL
#' @return A data frame containing meta data for all air quality monitoring sites in London
#' @details If borough_sf is not provided local authority name and inner/outer London category will not be returned.
#' London Borough boundary data is availabe from the \href{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}{London DataStore}
#' and \href{https://www.ordnancesurvey.co.uk/business-and-government/products/boundaryline.html}{Ordnance Survey}.
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # Without borough_sf
#' 
#' meta_data <- getMetaLondon()
#' 
#' # With borough_sf
#' boroughs <- st_read("London_Borough.shp")
#' meta_data <- getMetaLondon(borough_sf = boroughs)
#'  }
#' }
#' @rdname getMetaLondon
#' @export 
#' @import checkmate
#' @import dplyr
getMetaLondon <- function(borough_sf = NULL) {

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

######################### Fetch all sets of data ##############################

  aqe_sites <- suppressWarnings(getMetaAQE(borough_sf))

  laqn_sites <- suppressWarnings(getMetaLAQN(borough_sf))

  breathe_sites <- suppressWarnings(getMetaBreathe(borough_sf))

############################ Sort and drop dups ################################

  all_sites <- dplyr::bind_rows(aqe_sites, laqn_sites, breathe_sites) %>%
    dplyr::arrange(dplyr::desc(network)) %>%
    dplyr::distinct(code, species_code, .keep_all = TRUE) %>%
    dplyr::arrange(local_authority_name, code, species_code)

############################### Return dataframe ###############################

  return(all_sites)

}

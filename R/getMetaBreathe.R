#' @title getMetaBreathe
#' @description Get the meta data for all Breathe London monitoring sites in London
#' @param borough_sf A simple-features data frame containing London Borough polygons. , Default: NULL
#' @return A data frame containing meta data for all Breathe London monitoring sites in London
#' @details If borough_sf is not provided local authority name and inner/outer London category will not be returned.
#' London Borough boundary data is availabe from the \href{https://data.london.gov.uk/dataset/statistical-gis-boundary-files-london}{London DataStore}
#' and \href{https://www.ordnancesurvey.co.uk/business-and-government/products/boundaryline.html}{Ordnance Survey}.
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # Without borough_sf
#' 
#' meta_data <- getMetaBreathe()
#' 
#' # With borough_sf
#' boroughs <- st_read("London_Borough.shp")
#' meta_data <- getMetaBreathe(borough_sf = boroughs)
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{read.table}}
#'  \code{\link[lubridate]{ymd_hms}}
#' @rdname getMetaBreathe
#' @export 
#' @import checkmate
#' @importFrom utils read.csv
#' @import dplyr
#' @importFrom lubridate dmy_hm
getMetaBreathe <- function(borough_sf = NULL) {

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

  datastore_url <-
    "https://data.london.gov.uk/download/breathe-london-aqmesh-pods/b910476d-250e-41e9-a925-974a61b7f061/meta_data.csv"

  df <- utils::read.csv(datastore_url, stringsAsFactors = FALSE)

  df <- df %>%
    # All NO2 for now
    dplyr::mutate(species_code = "NO2",
           network = "Breathe",
           date_measurement_started = lubridate::dmy_hm(start_date_utc),
           date_measurement_finished = lubridate::dmy_hm(end_date_utc)) %>%
    dplyr::select(local_authority_name = borough,
           site = location_name,
           code = pod_id_location,
           site_type = type, ulez,
           latitude, longitude, species_code,
           date_measurement_started,
           date_measurement_finished,
           network, scaling_method, distance_from_road,
           height)


######################### Add borough data if provided #########################

  if (!(is.null(borough_sf))) {
    df <- df %>%
      dplyr::left_join(
        .,
        as.data.frame(borough_sf),
        by = c("local_authority_name" = "LocalAuthorityName")) %>%
      dplyr::mutate(inner_outer_london = ifelse(is.na(InnerOuterLondon),
                                                "Outside Greater London",
                                                InnerOuterLondon)) %>%
      dplyr::select(local_authority_name, site, code,
             site_type, inner_outer_london, ulez,
             latitude, longitude, species_code,
             date_measurement_started,
             date_measurement_finished,
             network, scaling_method, distance_from_road,
             height)

  }

  return(df)

}

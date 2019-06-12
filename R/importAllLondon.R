#' @title importAllLondon
#' @description Import air quality data from across London for specified parameters 
#' @param start_date Inclusive start date of data request. Must be a date object, Default: Sys.Date() - 1
#' @param end_date Inclusive end date of data request. Must be date object, Default: Sys.Date() - 1
#' @param sites character vector of site codes or 'all' to fetch all available sites, Default: 'all'
#' @param species character vector of species codes or 'all' to fetch all available species, Default: 'all'
#' @param borough_sf A simple-features data frame containing London Borough polygons, Default: NULL
#' @param meta_data Meta data dataframe, as fetched by importAllLondon etc. If not provided will be fetched, Default: NULL
#' @param verbose logical. Include site meta data in returned data frame, Default: TRUE
#' @return A data frame of hourly monitoring results
#' @details If borough_sf is not provided, local authority name and inner/outer london category will not be returned.
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # For all sites/species:
#' df <- importAllLondon(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'))
#' 
#' # For specified sites and species:
#' df <- importAllLondon(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'),
#' sites = c("BG1", "WL1", "MY1"),
#' species = c("NO2", "PM10", "PM2.5"))
#' 
#' # Providing meta data
#' meta_data <- getMetaAllLondon()
#' df <- importAllLondon(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'),
#' sites = c("BG1", "WL1", "MY1"),
#' species = c("NO2", "PM10", "PM2.5"),
#' meta_data = meta_data)
#'  }
#' }
#' @seealso 
#'  
#' @rdname importAllLondon
#' @export 
#' @import checkmate
#' @import dplyr
importAllLondon <- function(start_date = Sys.Date() - 1,
                            end_date = Sys.Date() - 1,
                            sites = "all", species = "all",
                            borough_sf = NULL, meta_data = NULL,
                            verbose = TRUE) {

################################## Checks ######################################

  # Get meta data if not provided
  if (checkmate::test_null(meta_data)) {
    meta_data <- getMetaAllLondon(borough_sf)
  } else {
    checkmate::assert_class(meta_data, "data.frame")
    checkmate::assert_names(x = names(meta_data),
                            must.include = c(
                              "site", "code",
                              "site_type", "latitude",
                              "longitude", "species_code",
                              "date_measurement_started",
                              "date_measurement_finished", "network"))

  }

  # Split sites into LAQN and AQE
  # While checking they're valid
  if (checkmate::test_character(sites, pattern =  "^all$",
                                ignore.case = TRUE)) {
    laqn_sites <- "all"
    aqe_sites <- meta_data %>%
      dplyr::filter(network == "AQE") %>%
      dplyr::pull(code) %>%
      unique()
  } else {
    possible_sites <- meta_data %>%
      dplyr::pull(code) %>%
      unique()
    sites <- toupper(sites)
    checkmate::assert_character(sites, all.missing = FALSE, min.len = 1)
    checkmate::assert_subset(sites, possible_sites)
    laqn_sites <- meta_data %>%
      dplyr::filter(code %in% sites) %>%
      dplyr::filter(network == "LAQN") %>%
      dplyr::pull(code) %>%
      unique()
    aqe_sites <- meta_data %>%
      dplyr::filter(code %in% sites) %>%
      dplyr::filter(network == "AQE") %>%
      dplyr::pull(code) %>%
      unique()
  }

############################ Fetch both sets of data ###########################

  if (!(is.null(laqn_sites)) & (length(laqn_sites) > 0)) {

    laqn <- importLAQN(sites = laqn_sites, start_date = start_date,
                       end_date = end_date, species = species,
                       borough_sf = borough_sf,
                       meta_data = meta_data %>%
                         dplyr::filter(network == "LAQN"),
                       verbose = verbose) %>%
      dplyr::mutate(network = "LAQN")

  } else {
    laqn <- data.frame()
  }

  if (!is.null(aqe_sites) & (length(aqe_sites) > 0)) {

    aqe <- importAQE(sites = aqe_sites,
                     start_date = start_date, end_date = end_date,
                     species = species, borough_sf = borough_sf,
                     meta_data = meta_data %>%
                       dplyr::filter(network == "AQE"), verbose = verbose) %>%
           dplyr::mutate(network = "AQE")

  } else {
    aqe <- data.frame()
  }

  df <- dplyr::bind_rows(laqn, aqe)

################################## Clean data ##################################

  # Get list of all species fetched
  all_species <- df %>%
    dplyr::select(-date_time_gmt, -code, -site, -site_type,
           -dplyr::matches(
             "local_authority_name|inner_outer_london|latitude|longitude|network")) %>%
    names()

  # Select relevant data
  df <- df %>%
    dplyr::select(date_time_gmt, code, site,
           suppressWarnings(dplyr::one_of(all_species)),
           site_type,
           suppressWarnings(dplyr::one_of(
             c("local_authority_name", "inner_outer_london", "network")
             )),
           suppressWarnings(dplyr::one_of(c("latitude", "longitude"))))

############################## Return data frame ###############################

  return(df)

}

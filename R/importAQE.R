#' @title importAQE
#' @description Import air quality data from across the Air Quality England (AQE) network for specified parameters 
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
#' df <- importAQE(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'))
#' 
#' # For specified sites and species:
#' df <- importAllLondon(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'),
#' sites = c("HIL1", "T55"),
#' species = c("NO2", "PM10", "PM2.5"))
#' 
#' # Providing meta data
#' meta_data <- getMetaAllLondon()
#' df <- importAllLondon(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'),
#' sites = c(c("HIL1", "T55")),
#' species = c("NO2", "PM10", "PM2.5"),
#' meta_data = meta_data)
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{read.table}}
#'  \code{\link[lubridate]{period}}
#'  \code{\link[stats]{setNames}}
#' @rdname importAQE
#' @export 
#' @import checkmate
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom lubridate hours
#' @importFrom stats setNames
importAQE <- function(start_date = Sys.Date() - 1, end_date = Sys.Date() - 1,
                      sites = "all", species = "all",
                      borough_sf = NULL, meta_data = NULL, verbose = TRUE) {

#################################### Checks ####################################

  # Get meta data if not provided
  if (checkmate::test_null(meta_data)) {
    meta_data <- getMetaAQE(borough_sf)
  } else {
    checkmate::assert_class(meta_data, "data.frame")
    checkmate::assert_names(x = names(meta_data),
                            must.include = c(
                              "site", "code",
                              "site_type", "latitude",
                              "longitude", "species_code",
                              "date_measurement_started",
                              "date_measurement_finished", "network"))

    meta_data <- meta_data %>%
      dplyr::filter(network == "AQE")
  }

  if (checkmate::test_string(sites, pattern = "^all$", ignore.case = TRUE)) {
    get_all_sites <- TRUE
  } else {
    get_all_sites <- FALSE
    sites <- toupper(sites)
    checkmate::assert_character(sites)
    checkmate::assert_subset(x = sites, choices = meta_data %>%
                               dplyr::pull(code) %>%
                               unique())
  }

  checkmate::assert_date(start_date,
                         lower = meta_data %>%
                           dplyr::pull(date_measurement_started) %>%
                           min(., na.rm = TRUE),
                         upper = min(Sys.Date(), end_date),
                         any.missing = FALSE,
                         min.len = 1,
                         max.len = 1)
  checkmate::assert_date(end_date,
                         lower = meta_data %>%
                           dplyr::pull(date_measurement_started) %>%
                           min(., na.rm = TRUE) %>%
                           max(., start_date),
                         upper = Sys.Date(),
                         any.missing = FALSE,
                         min.len = 1,
                         max.len = 1)

  if (checkmate::test_string(species, pattern = "^all$", ignore.case = TRUE)) {
    get_all_species <- TRUE
  } else {
    get_all_species <- FALSE
    species <- toupper(species)
    checkmate::assert_character(species)
    checkmate::assert_subset(species, meta_data %>%
                               dplyr::pull(species_code) %>%
                               unique())
  }

  checkmate::assert_logical(verbose)

############################## Prep for API calls ##############################

  # Get list of species
  all_species <- meta_data %>%
    dplyr::pull(species_code) %>%
    unique()

  if (get_all_species) {
    species <- all_species
  }

  # Make inclusive of end date
  end_date_str <- format(end_date + 1, "%Y-%m-%d")
  start_date_str <- format(start_date, "%Y-%m-%d")

  # Get basic meta data for sites
  site_info <- meta_data %>%
    dplyr::filter(species_code %in% species) %>%
    dplyr::filter(date_measurement_started <= end_date) %>%
    dplyr::filter(date_measurement_finished >= start_date
           | is.na(date_measurement_finished)) %>%
    dplyr::select(-species_code, -dplyr::contains("date_measurement")) %>%
    unique()

  if (!(get_all_sites)) {
    site_info <- site_info %>%
      dplyr::filter(code %in% sites)
  }

  # List of site codes to fetch
  sites_to_get <- site_info %>%
    dplyr::pull(code) %>%
    unique()

  # Warn about any missing sites
  if (!(get_all_sites)) {
    for (site in sites) {
      if (!(site %in% sites_to_get)) {
        warning(
          paste0("No data for site ", site, " for given dates and species")
        )
      }
    }
  }

###################################### Call API ################################

  aqe_url <-
    "http://acer.aeat.com/gla-cleaner-air/api/v1/gla-cleaner-air/v1/site"

  df <- data.frame()

  # Fetch data for each site
  for (site_code in sites_to_get) {
    query_url <- paste(aqe_url,
                       site_code,
                       start_date_str,
                       end_date_str,
                       sep = "/")
    # Warn if no data
    data <- tryCatch(utils::read.csv(query_url,
                              fileEncoding = "UTF-8-BOM",
                              stringsAsFactors = FALSE),
                     error = function(cond) {
                       if (!(get_all_sites)) {
                         message("No data available for site")
                       }
                       return(NULL)
                     },
                     warning = function(cond) {
                       if (!(get_all_sites)) {
                         message(paste("No data available for site", site_code))
                       }
                       return(NULL)
                     }
    )
    # Clean site data
    if (!(checkmate::test_null(data))) {
      site_name <- meta_data %>%
        dplyr::filter(code == site_code) %>%
        dplyr::pull(site) %>%
        unique() %>%
        gsub(" |-|'", "\\.", .)
      names(data) <- data %>%
        names() %>%
        gsub("MeasurementDateGMT", "date_time_gmt", .) %>%
        gsub(site_name, "", .) %>%
        gsub("^\\.\\.", "", .) %>%
        gsub("\\.\\.[A-Za-z\\.]+$", "", .)

      names(data) <-  data %>%
        names() %>%
        gsub("\\.\\..+$", "", .)

      data <- data %>%
        dplyr::mutate(code = site_code)

      # Add to main data frame
      df <- dplyr::bind_rows(df, data)
    }
  }

############################ Clean any fetched data ############################

  if (nrow(df) > 0) {
    df <- df %>%
      # Add meta data
      dplyr::left_join(., site_info, by = "code") %>%
      dplyr::mutate(latitude = as.numeric(latitude),
             longitude = as.numeric(longitude),
             # Adjust time stamp to match LAQN
             date_time_gmt = (as.POSIXct(date_time_gmt,
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "GMT")
                              - lubridate::hours(1))) %>%
      dplyr::select(date_time_gmt, code, site,
             suppressWarnings(dplyr::one_of(species)),
             site_type,
             dplyr::matches("uthority|uter"),
             latitude, longitude) %>%
      # Convert to snake_case
      stats::setNames(gsub("([a-z]){1}([A-Z]){1}", "\\1_\\2", names(.))) %>%
      dplyr::mutate_at(dplyr::vars(suppressWarnings(dplyr::one_of(species))),
                ~ round(., digits = 1)) %>%
      stats::setNames(tolower(names(.))) %>%
      stats::setNames(gsub("pm25", "pm2\\.5", names(.))) %>%
      dplyr::mutate(latitude = round(latitude, digits = 6),
             longitude = round(longitude, digits = 6))
    # If verbose = FALSE drop some meta data
    if (!verbose) {
      df <- df %>%
        dplyr::select(-site_type, -dplyr::starts_with("local"),
                      -dplyr::starts_with("inner"), -latitude, -longitude)
    }
  }

############################### Return dataframe ###############################

  return(df)

}

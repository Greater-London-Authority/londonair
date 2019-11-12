#' @title importBreathe
#' @description Import air quality data from across the Breathe London Network for specified parameters
#' @param start_date Inclusive start date of data request. Must be a date object, Default: Sys.Date() - 1
#' @param end_date Inclusive end date of data request. Must be date object, Default: Sys.Date() - 1
#' @param sites character vector of site codes or 'all' to fetch all available sites, Default: 'all'
#' @param species character vector of species codes or 'all' to fetch all available species, Default: 'all'
#' @param borough_sf A simple-features data frame containing London Borough polygons. , Default: NULL
#' @param meta_data Meta data dataframe, as fetched by importLondon etc. If not provided will be fetched, Default: NULL
#' @param verbose logical. Include site meta data in returned data frame, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#' # For all sites/species:
#' df <- importBreathe(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'))
#'
#' # For specified sites and species (only NO2 available for Breathe London):
#' df <- importBreathe(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'),
#' sites = c("2245", "16245"),
#' species = "NO2")
#'
#' # Providing meta data
#' meta_data <- getMetaLondon()
#' df <- importLondon(start_date = as.Date('2019-01-01'),
#' end_date = as.Date('2019-01-07'),
#' sites = c(c("GB6", "CR5")),
#' species = c("NO2", "PM10", "PM2.5"),
#' meta_data = meta_data)
#'  }
#' }
#' @seealso 
#'  \code{\link[utils]{read.table}}
#' @rdname importBreathe
#' @export 
#' @import checkmate
#' @import dplyr
#' @importFrom utils read.csv
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate hours
#' @importFrom lubridate days
#' @importFrom stats setNames
#' @importFrom json fromJSON
importBreathe <- function(start_date = Sys.Date() - 1,
                          end_date = Sys.Date() - 1,
                          sites = "all", species = "all", borough_sf = NULL,
                          meta_data = NULL, verbose = TRUE) {

#################################### Checks ####################################

  # Get meta data if not provided
  if (checkmate::test_null(meta_data)) {
    meta_data <- getMetaBreathe(borough_sf)
  } else {
    checkmate::assert_class(meta_data, "data.frame")
    checkmate::assert_names(x = names(meta_data),
                            must.include = c(
                              "local_authority_name", "site", "code",
                              "site_type", "latitude",
                              "longitude", "species_code",
                              "date_measurement_started",
                              "date_measurement_finished", "network"))

    meta_data <- meta_data %>%
      dplyr::filter(network == "Breathe")
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
    # Only NO2 available for now, but don't throw error
    checkmate::assert_subset(species, c('CO','NO2','O3','PM10','PM25','SO2'))
  }

  checkmate::assert_logical(verbose)

########################## Work out what data we need ##########################

  all_species <- meta_data %>%
    dplyr::pull(species_code) %>%
    unique()

  if (get_all_species) {
    species <- all_species
  }

  site_info <- meta_data %>%
    dplyr::filter(species_code %in% species) %>%
    dplyr::filter(date_measurement_started <= end_date) %>%
    dplyr::filter(date_measurement_finished >= start_date
                  | is.na(date_measurement_finished)) %>%
    dplyr::select(-species_code, -dplyr::contains("date_measurement"), -site) %>%
    dplyr::distinct()

  if (!(get_all_sites)) {
    site_info <- site_info %>%
      dplyr::filter(code %in% sites)
  }

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

############################## Get data from data store ########################
  
  dataset_url <- "https://data.london.gov.uk/api/dataset/breathe-london-aqmesh-pods"
  json_info <- rjson::fromJSON(file = dataset_url)
  table_ref <- names(json_info$resources$`267507cc-9740-4ea7-be05-4d6ae16a5e4a`$tables)

  base_url <- paste("https://data.london.gov.uk/api/table", table_ref, "export.csv?", sep = "/")
  
  # Construct SQL query for datastore API

  sql_select_query <- "sql=SELECT * FROM dataset WHERE"

  num_returned <- 5000
  offset <- 0

  df <- data.frame()

  while (num_returned == 5000) {
    sql_where_query <- paste0(" pod_id_location IN (",
                           paste(sites_to_get, collapse = ", "),
                           ") AND date_utc >= '", start_date - 1, "'",
                           " AND date_utc <= '", end_date + 1, "' OFFSET ",
                           format(offset, scientific = FALSE), " ROWS")

    full_url <- paste0(base_url, sql_select_query, sql_where_query)

    qdf <- utils::read.csv(full_url, stringsAsFactors = FALSE)

    df <- dplyr::bind_rows(df, qdf)

    num_returned <- nrow(qdf)
    offset <- offset + 5000
  }


  df <- df %>%
    select(-matches("^.*\\.\\.")) %>%
    # change date format to get the same as LAQN and redo filter
    dplyr::mutate(date_time_gmt = (lubridate::ymd_hms(date_utc) 
                                   - lubridate::hours(1)),
                  code = as.character(pod_id_location)) %>%
    dplyr::filter(date_time_gmt >= start_date) %>%
    dplyr::filter(date_time_gmt < end_date + lubridate::days(1)) %>%
    stats::setNames(gsub("pm2_5", "pm25", names(.))) %>%
    stats::setNames(gsub("_ugm3", "", names(.))) %>%
    dplyr::select(date_time_gmt, code, site = location_name,
                  matches(paste(tolower(species), collapse = "|")))

  if (verbose) {
    df <- df %>%
      dplyr::left_join(., site_info, by = "code")
  }

############################### Return dataframe ###############################

  return(df)
}

#' @param borough_sf A simple-features data frame containing London Borough polygons. 
#' Must contain columns "LocalAuthorityName" and "InnerOuterLondon", Default: NULL
#' @param end_date Inclusive end date of data request. Must be date object, Default: Sys.Date() - 1
#' @param meta_data Meta data dataframe, as fetched by importAllLondon etc. If not provided will be fetched, Default: NULL
#' @param sites character vector of site codes or 'all' to fetch all available sites, Default: 'all'
#' @param species character vector of species codes or 'all' to fetch all available species, Default: 'all'
#' @param start_date Inclusive start date of data request. Must be a date object, Default: Sys.Date() - 1
#' @param verbose logical. Include site meta data in returned data frame, Default: TRUE

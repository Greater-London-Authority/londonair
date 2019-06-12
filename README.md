londonair
================

This package provides tools to download air quality data for London.

Installation
------------

To install `londonair` from github you need to use the `devtools` package.

If you do not have `devtools` installed, you will need to run the commented out line of code as well.

``` r
#install.packages("devtools")
devtools::install_github("LiRogers/londonair")
library(londonair)
```

Package functions
-----------------

londonair contains functions that fetch meta data and monitoring data for air quality monitoring sites across the whole of London, or for the 2 main sensor networks, the London Air Quality Network (LAQN) and the Air Quality England (AQE) network.

### Site meta data

Meta data can be fetched as follows:

``` r
meta_data <- getMetaLondon()

meta_data %>%
  head()
```

    ##                                    site code site_type latitude longitude
    ## 1     Barking and Dagenham - Rush Green  BG1  Suburban 51.56375  0.177891
    ## 2     Barking and Dagenham - Rush Green  BG1  Suburban 51.56375  0.177891
    ## 3 Barking and Dagenham - Scrattons Farm  BG2  Suburban 51.52939  0.132857
    ## 4 Barking and Dagenham - Scrattons Farm  BG2  Suburban 51.52939  0.132857
    ## 5   Barking and Dagenham - North Street  BG3  Kerbside 51.54044  0.074418
    ## 6              Barnet - Tally Ho Corner  BN1  Kerbside 51.61467 -0.176607
    ##   species_code date_measurement_started date_measurement_finished
    ## 1          NO2               2008-01-01                      <NA>
    ## 2          SO2               1999-10-23                      <NA>
    ## 3          NO2               2007-11-21                      <NA>
    ## 4         PM10               1999-10-17                      <NA>
    ## 5          NO2               2008-01-01                2011-05-25
    ## 6          NO2               2008-01-01                2012-04-20
    ##                                                           site_link
    ## 1 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG1
    ## 2 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG1
    ## 3 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG2
    ## 4 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG2
    ## 5 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG3
    ## 6 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BN1
    ##   network local_authority_name
    ## 1    LAQN Barking and Dagenham
    ## 2    LAQN Barking and Dagenham
    ## 3    LAQN Barking and Dagenham
    ## 4    LAQN Barking and Dagenham
    ## 5    LAQN Barking and Dagenham
    ## 6    LAQN               Barnet

This gets meta data for all air quality monitoring sites across London. The equivalent functions for the 2 networks are `getMetaLAQN()` and `getMetaAQE()`.

### Monitoring data

Hourly monitoring data can be fetched as follows:

``` r
# For all sites and species:
df <- importLondon(start_date = as.Date('2019-01-01'),
                      end_date = as.Date('2019-01-07'))

# For specified sites and species:
df <- importLondon(start_date = as.Date('2019-01-01'),
                      end_date = as.Date('2019-01-07'),
                      sites = c("BG1", "WL1", "MY1"),
                      species = c("NO2", "PM10", "PM25"))

# Providing meta data - this will speed up your code if you're making multiple calls to importLondon
meta_data <- getMetaLondon()
df <- importLondon(start_date = as.Date('2019-01-01'),
                      end_date = as.Date('2019-01-07'),
                      sites = c("BG1", "WL1", "MY1"),
                      species = c("NO2", "PM10", "PM25"),
                      meta_data = meta_data)

df %>% 
  head()
```

    ##         date_time_gmt code                              site  no2 pm10
    ## 1 2019-01-01 00:00:00  BG1 Barking and Dagenham - Rush Green   NA   NA
    ## 2 2019-01-01 00:00:00  MY1     Westminster - Marylebone Road 50.9 35.5
    ## 3 2019-01-01 01:00:00  BG1 Barking and Dagenham - Rush Green 10.4   NA
    ## 4 2019-01-01 01:00:00  MY1     Westminster - Marylebone Road 46.7 25.2
    ## 5 2019-01-01 02:00:00  BG1 Barking and Dagenham - Rush Green  6.3   NA
    ## 6 2019-01-01 02:00:00  MY1     Westminster - Marylebone Road 43.7 15.4
    ##   site_type local_authority_name network latitude longitude
    ## 1  Suburban Barking and Dagenham    LAQN 51.56375  0.177891
    ## 2  Kerbside          Westminster    LAQN 51.52254 -0.154590
    ## 3  Suburban Barking and Dagenham    LAQN 51.56375  0.177891
    ## 4  Kerbside          Westminster    LAQN 51.52254 -0.154590
    ## 5  Suburban Barking and Dagenham    LAQN 51.56375  0.177891
    ## 6  Kerbside          Westminster    LAQN 51.52254 -0.154590

This will work for sites in both networks. To fetch the data from just one of the networks the equivalent functions are `importLAQN()` and `importAQE()`, which have the same parameters as `importLondon()`.

The format of the monitoring data returned will work with the utility functions included in the [OpenAir R package](http://davidcarslaw.github.io/openair/).

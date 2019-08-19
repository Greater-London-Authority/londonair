londonair
================

This package provides tools to download air quality data for London.

## Installation

To install `londonair` from github you need to use the `devtools`
package.

If you do not have `devtools` installed, you will need to run the
commented out line of code as well.

``` r
#install.packages("devtools")
devtools::install_github("LiRogers/londonair")
library(londonair)
```

## Package functions

londonair contains functions that fetch meta data and monitoring data
for air quality monitoring sites across the whole of London, or for the
3 main sensor networks, the London Air Quality Network (LAQN), Breathe
London, and the Air Quality England (AQE) network.

### Site meta data

Meta data can be fetched as follows:

``` r
meta_data <- getMetaLondon()

meta_data %>%
  head()
```

    ##                                    site   code site_type latitude
    ## 1                             Wood Lane  21245  Roadside 51.56137
    ## 2                        Station Parade  37245    Urban  51.53906
    ## 3                    Woolwich Manor Way 892450  Roadside 51.51590
    ## 4     Barking and Dagenham - Rush Green    BG1  Suburban 51.56375
    ## 5     Barking and Dagenham - Rush Green    BG1  Suburban 51.56375
    ## 6 Barking and Dagenham - Scrattons Farm    BG2  Suburban 51.52939
    ##   longitude species_code date_measurement_started
    ## 1  0.148582          NO2               2018-10-18
    ## 2  0.080874          NO2               2018-10-12
    ## 3  0.060587          NO2               2018-10-25
    ## 4  0.177891          NO2               2008-01-01
    ## 5  0.177891          SO2               1999-10-23
    ## 6  0.132857          NO2               2007-11-21
    ##   date_measurement_finished
    ## 1                2019-07-01
    ## 2                2019-07-01
    ## 3                2019-01-03
    ## 4                      <NA>
    ## 5                      <NA>
    ## 6                      <NA>
    ##                                                           site_link
    ## 1                                                              <NA>
    ## 2                                                              <NA>
    ## 3                                                              <NA>
    ## 4 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG1
    ## 5 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG1
    ## 6 http://www.londonair.org.uk/london/asp/publicdetails.asp?site=BG2
    ##   network local_authority_name ulez                    scaling_method
    ## 1 Breathe   Barking & Dagenham   No Initial Reference Site Colocation
    ## 2 Breathe   Barking & Dagenham   No Initial Reference Site Colocation
    ## 3 Breathe   Barking & Dagenham   No               Gold Pod Colocation
    ## 4    LAQN Barking and Dagenham <NA>                              <NA>
    ## 5    LAQN Barking and Dagenham <NA>                              <NA>
    ## 6    LAQN Barking and Dagenham <NA>                              <NA>
    ##   distance_from_road height
    ## 1              0.500  3.000
    ## 2              3.567  3.114
    ## 3              0.100  3.300
    ## 4                 NA     NA
    ## 5                 NA     NA
    ## 6                 NA     NA

This gets meta data for all air quality monitoring sites across London.
The equivalent functions for the 3 networks are `getMetaLAQN()`,
`getMetaBreathe()` and `getMetaAQE()`.

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

This will work for sites in both networks. To fetch the data from just
one of the networks the equivalent functions are `importLAQN()`,
`importBreathe()` and `importAQE()`, which have the same parameters as
`importLondon()`.

The format of the monitoring data returned will work with the utility
functions included in the [OpenAir R
package](http://davidcarslaw.github.io/openair/).

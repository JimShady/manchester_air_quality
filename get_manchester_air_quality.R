library(rvest)
library(stringr)
library(raster)

years       <- 2015:2030
authorities <- data.frame(authority_name = c('Bolton', 'Bury', 'Oldham', 'Rochdale', 'Stockport', 'Tameside',
                                             'Trafford', 'Wigan', 'Manchester', 'Salford'),
                          authority_id   = c(24, 42, 191, 209, 260, 270,
                                             286, 313, 155, 220),
                          stringsAsFactors = F)

session       <- html_session("https://uk-air.defra.gov.uk/data/laqm-background-maps?year=2015")
form          <- html_form(session)[[2]]

## Start the years loop.
for (i in 1:length(years)){
  
  year <- years[i]
  print(year)
  
  ## Start the authorities loop
  for (j in 1:nrow(authorities)) {
    
    authority <- authorities[j,'authority_id']
    print(authority)
    
    ## PM25 data
    filledform          <- set_values(form,
                                `bkgrd-la`    = authority,
                                `bkgrd-pollutant` = 'pm25',
                                `bkgrd-year`      = year)
    temp_pm25_session   <- submit_form(session, filledform)
    temp_pm25_data      <- read.csv(url(temp_pm25_session$url), skip=5)
    temp_pm25_data$year <- year
    names(temp_pm25_data) <- gsub('[0-9]+', '', names(temp_pm25_data))
    
    if (i == 1 & j == 1) {pm25_data <- temp_pm25_data} else {pm25_data <- rbind(pm25_data, temp_pm25_data)}
    
    ## NO2 data
    filledform     <- set_values(form,
                                `bkgrd-la`    = authority,
                                `bkgrd-pollutant` = 'no2',
                                `bkgrd-year`      = year)
    temp_no2_session   <- submit_form(session, filledform)
    temp_no2_data      <- read.csv(url(temp_no2_session$url), skip=5)
    temp_no2_data$year <- year
    names(temp_no2_data) <- gsub('[0-9]+', '', names(temp_no2_data))
    
    
    if (i == 1 & j == 1) {no2_data <- temp_no2_data} else {no2_data <- rbind(no2_data, temp_no2_data)}
    
  } ## End the authorities loop
  
} ## End the years loop

rm(temp_no2_data, temp_pm25_data, session, temp_no2_session, temp_pm25_session, i, j, year, authorities, form, filledform)

coordinates(no2_data) <- ~ x + y
gridded(no2_data)     <- TRUE
no2_data              <- raster(no2_data)


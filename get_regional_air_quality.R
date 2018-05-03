rm(list=ls())

print('Loading any missing libraries')

devtools::install_github("tidyverse/ggplot2")

library(rvest)
library(stringr)
library(raster)
library(rgdal)
library(rgeos)
library(ggplot2)
library(sf)
library(viridis)

ukgrid      <- "+init=epsg:27700"
latlong     <- "+init=epsg:4326"


# Wales, MIdland, Northern England
years       <- c(2011,2015,2020,2025,2030)
regions     <- c('Midlands', 'Northern_England', 'Wales')

authorities <- data.frame(authority_name = c('Bolton', 'Bury', 'Oldham', 'Rochdale', 'Stockport', 'Tameside',
                                             'Trafford', 'Wigan', 'Manchester', 'Salford', 'Cheshire East',  'Rossendale'),
                          stringsAsFactors = F)

print('getting a geojson of UK wards from governmant data portal')

url     <- 'https://opendata.arcgis.com/datasets/d5c9c1d89a5a44e9a7f88f182ffe5ba2_2.geojson'
wards   <- readOGR(dsn = url, layer = "d5c9c1d89a5a44e9a7f88f182ffe5ba2_2")
wards   <- spTransform(wards, ukgrid)
wards   <- wards[wards$lad16nm %in% authorities$authority_name,]

print('getting a geojson of UK local authorities from data portal')

url             <- 'https://opendata.arcgis.com/datasets/8edafbe3276d4b56aec60991cbddda50_4.geojson'
auth_boundary   <- readOGR(dsn = url, layer = "8edafbe3276d4b56aec60991cbddda50_4")
auth_boundary   <- spTransform(auth_boundary, ukgrid)
auth_boundary   <- auth_boundary[auth_boundary$lad15nm %in% authorities$authority_name,]

## Link my session to the form I'm going to download data from
session_2015       <- html_session("https://uk-air.defra.gov.uk/data/laqm-background-maps?year=2015")
form_2015          <- html_form(session_2015)[[3]]

session_2011       <- html_session("https://uk-air.defra.gov.uk/data/laqm-background-maps?year=2011")
form_2011          <- html_form(session_2011)[[3]]

print('Getting data from the DEFRA website for the authorities defined above')

## Start the years loop.
for (i in 1:length(years)){
  
  year <- years[i]
  print(year)
  
  ## Start the authorities loop
  for (j in 1:length(regions)) {
    
    region                <- regions[j]
    print(paste0(year, ' - ', region))
    
    ## PM25 data
    if (year == 2011) { 
      form <- form_2011; session <- session_2011
    } else {
      form <- form_2015; session <- session_2015
    }
    
    filledform            <- set_values(form,
                                `bkgrd-region`    = region,
                                `bkgrd-pollutant` = 'pm25',
                                `bkgrd-year`      = year)
    temp_pm25_session     <- submit_form(session, filledform)
    temp_pm25_data        <- read.csv(url(temp_pm25_session$url), skip=5)
    temp_pm25_data$year   <- year
    names(temp_pm25_data) <- gsub('[0-9]+', '', names(temp_pm25_data))
    
    if (i == 1 & j == 1) {pm25_data <- temp_pm25_data} else {pm25_data <- rbind(pm25_data, temp_pm25_data)}
    
    ## NO2 data
    filledform     <- set_values(form,
                                 `bkgrd-region`    = region,
                                 `bkgrd-pollutant` = 'no2',
                                 `bkgrd-year`      = year)
    temp_no2_session   <- submit_form(session, filledform)
    temp_no2_data      <- read.csv(url(temp_no2_session$url), skip=5)
    temp_no2_data$year <- year
    names(temp_no2_data) <- gsub('[0-9]+', '', names(temp_no2_data))
    
    
    if (i == 1 & j == 1) {no2_data <- temp_no2_data} else {no2_data <- rbind(no2_data, temp_no2_data)}
    
  } ## End the authorities loop
  
} ## End the years loop

rm(temp_no2_data, temp_pm25_data, session, temp_no2_session, temp_pm25_session, i, j, year, form, filledform, form_2011, form_2015, session_2011, session_2015)

## All good to this point. Now need the ward boundaries. Instead of using the shapefile from
## David going to get my own, hopefully this is ok...


## Now need to make rasters from the points. Tricky as multiple years and pollutants
## Do some tidying up first

no2_data          <- no2_data[,c('x', 'y', 'Total_NO_', 'year')]
names(no2_data)   <- c('x', 'y', 'no2', 'year')

pm25_data          <- pm25_data[,c('x', 'y', 'Total_PM._', 'PM_secondary_', 'Residual.Salt_', 'year')]
names(pm25_data)   <- c('x', 'y', 'pm25_total', 'pm25_secondary', 'pm25_residual_salt', 'year')

## Making a raster file of the wider Manchester area for each pollutant, for each year
print('Making NO2 raster stack')

## Now a loop to make a raster stack of the NO2 data, one layer for each year
for (i in 1:length(years)) {
  
  year <- years[i]
  
  one_year                  <- no2_data[no2_data$year == year,]
  one_year$year             <- NULL
  names(one_year)[3]        <- 'no2'
  coordinates(one_year)     <- ~ x + y
  crs(one_year)             <- CRS(ukgrid)
  gridded(one_year)         <- TRUE
  one_year                  <- raster(one_year)
  
  if (i == 1) {no2_raster   <- one_year} else {
    no2_raster <- stack(no2_raster, crop(one_year, no2_raster))
    }
  
  print(year)
  
  rm(one_year)
  
}

names(no2_raster) <- years

## Now a loop to make a raster stack of the PM25 data, one layer for each year/source
print('Making PM25 raster stack')

sources <- c('pm25_total', 'pm25_secondary', 'pm25_residual_salt')

counter <- 1
## Start years loop
for (i in 1:length(years)) {
  
  year <- years[i]
  
  # Start sources loop
  for (j in 1:length(sources)) {
    
    one_source <- sources[j]
    
    print(paste0(year, ' - ', one_source))
    
    one_year                  <- pm25_data[pm25_data$year == year,c('x','y','year',one_source)]
    one_year$year             <- NULL
    names(one_year)[3]        <- paste0(year, '_', one_source)
    coordinates(one_year)     <- ~ x + y
    crs(one_year)             <- CRS(ukgrid)
    gridded(one_year)         <- TRUE
    one_year                  <- raster(one_year)
    
    if (i == 1 & j ==1) {pm25_raster   <- one_year} else {
      pm25_raster <- stack(pm25_raster, crop(one_year, pm25_raster))
    }
    
    names(pm25_raster)[counter]     <- paste0(year, '_', one_source)
    counter <- counter+1
    
  }
  
}
## Now extract the data for NO2 and put it in the Wards file as new columns
print('Extracting mean-area-weighted NO2 data for each year:Ward ')

for (i in 1:length(names(no2_raster))) {
  
  col_name <- paste0(as.character(names(no2_raster)[i]), '_', 'no2_total')
  
  wards@data[,col_name] <- extract(no2_raster[[as.character(names(no2_raster)[i])]],
                                   wards,
                                   weights=TRUE,
                                   fun=mean,
                                   na.rm = T)
  
  print(col_name)
  
}

## Now extract the data for PM25 and put it in the Wards file as new columns
print('Extracting mean-area-weighted PM25 data for each year:Ward:source ')

for (i in 1:length(names(pm25_raster))) {
  
  col_name <- as.character(names(pm25_raster)[i])
  
  wards@data[,col_name] <- extract(pm25_raster[[as.character(names(pm25_raster)[i])]],
                                   wards,
                                   weights=TRUE,
                                   fun=mean,
                                   na.rm = T)
  
  print(col_name)
  
}

## Bit of tidying up
print('Tidying up')

names(wards)          <- gsub('X', '', names(wards))
wards$objectid        <- NULL
wards$wd16nmw         <- NULL
wards$lad16cd         <- NULL
wards$bng_e           <- NULL
wards$bng_n           <- NULL
wards$long            <- NULL
wards$lat             <- NULL
wards$st_areashape    <- NULL
wards$st_lengthshape  <- NULL

wards_csv             <- data.frame(wards)
names(wards_csv)      <- gsub('X', '', names(wards_csv))
write.csv(wards_csv, file ="wards.csv",row.names=FALSE)
rm(wards_csv)

## Get it ready for plotting
names(wards)          <- gsub('X', '', names(wards))
wards                 <- st_as_sf(wards)

## Colour scales
pm25_laei2013_breaks  <- 4:15 #12
pm25_laei2013_colours <- pm25_laei2013_colours #11
pm25_laei2013_labels  <- c('< 4', as.character(5:15)) #15

pm25_maps_list        <- c('2011_pm25_total','2015_pm25_total','2020_pm25_total','2025_pm25_total', '2030_pm25_total')
pm25_plot_list        <- list()

no2_maps_list        <- c('2011_no2_total','2015_no2_total','2020_no2_total','2025_no2_total', '2030_no2_total')
no2_plot_list        <- list()


source('https://raw.githubusercontent.com/KCL-ERG/colour_schemes/master/no2_laei2013_colours_breaks.R')
source('https://raw.githubusercontent.com/KCL-ERG/colour_schemes/master/pm25_laei2013_colours_breaks.R')

for (i in 1:length(pm25_maps_list)) {
  
  t <- quote('test')
  
  pm25_plot_list[[i]]   <- ggplot(wards) +
                            geom_sf(aes(fill = round(wards[[no2_maps_list[1]]][,1]),1)) +
                            scale_fill_viridis() +
                            theme_bw()
}

print('All done, writing to a (large) csv')

write.csv(wards, file ="wards.csv",row.names=FALSE)

library(rvest)

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
  
  ## Start the authorities loop
  for (j in 1:nrow(authorities)) {
    
    authority <- authorities[j,'authority_id']
    
    filledform    <- set_values(form,
                                `bkgrd-la`    = authority,
                                `bkgrd-pollutant` = 'pm25',
                                `bkgrd-year`      = year)
    session       <- submit_form(session, filledform)
    data          <- read.csv(url(session$url), skip=5)
    
    
  } ## End the authorities loop
  
} ## End the years loop



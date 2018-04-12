library(httr)
library(RCurl)
library(devtools)
library(RHTMLForms)

web             <- GET("https://uk-air.defra.gov.uk/data/laqm-background-maps?year=2015")
doc              <- htmlParse(web, asText = TRUE)
description <- getHTMLFormDescription(doc)
my_func     <- createFunction(description[[2]], 'https://uk-air.defra.gov.uk/data/laqm-background-maps?year=2015')

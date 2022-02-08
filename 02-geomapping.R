# MPBF
# 16/11/2021
# Adapted from au-cyberdeepdive repo
# Dependencies: main_oct21.R output
#########################################################################################

library(rgdal)
# load ggplot2
library(ggplot2)
library(tidyverse)
library(leaflet)

## Load 'curated' DSPT file
data <- read_csv("./outputs/data_DSPTmetric_20211021.csv")

## Load shapefile
# Source: https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en/explore?location=52.950000%2C-2.000000%2C7.02
ccg_spdf <- readOGR("./Inputs/shapefile/Clinical_Commissioning_Groups_(April_2020)_EN_BFC_V2.shp")

proj4string(ccg_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

ccg_spdf@proj4string # check system

ccg_spdf <- ccg_spdf %>% sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system



##Load correspondence from ODS to ONS codes
# https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2020-names-and-codes-in-england/explore

lkp_ccg <- read.csv('./Inputs/Clinical_Commissioning_Groups_(April_2020)_Names_and_Codes_in_England.csv')

# Join DSPT data and ONS code
data <- data %>% left_join(lkp_ccg %>% select(c("CCG20CD","CCG20CDH")),by=c("Code"="CCG20CDH"))

# Select relevant columns of data
data_red <- data %>% select(Final.Status,CCG20CD,Code,Successor.Code)

# Join the reduced DSPT info with the shapefile
ccg_spdf@data <- left_join(ccg_spdf@data,data_red,by=c("ccg20cd"="CCG20CD"))

dspt2021levels=c("20/21 Standards Exceeded","20/21 Standards Met","20/21 Approaching Standards","20/21 Standards Not Met","Not Published")

# Make Status a categorical
ccg_spdf@data$Final.Status = factor(ccg_spdf@data$Final.Status,dspt2021levels)

# Choose a palette
my_palette <- rev(colorspace::rainbow_hcl(5))

catpal <- colorFactor(my_palette, ccg_spdf@data$Final.Status) 

## Plotting
# Prepare the text for tooltips:
mytext <- paste(
  "CCG code (ODS): ", ccg_spdf@data$Code,"<br/>",
  "CCG code (ONS): ", ccg_spdf@data$ccg20cd,"<br/>",
  "CCG Successor code (ODS): ", ccg_spdf@data$Successor.Code,"<br/>",
  "CCG name: ", ccg_spdf@data$ccg20nm,"<br/>", 
  "DSPT Status: ", ccg_spdf@data$Final.Status, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


# Final Map
m<-leaflet(ccg_spdf) %>% 
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons( 
    fillColor = ~catpal(Final.Status), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="black", 
    weight=0.5,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=catpal, values=~Final.Status, opacity=0.9, title = "20/21 DSPT Status (CCG)", position = "bottomleft" )

m  

# save the widget in a html file if needed.
library(htmlwidgets)
saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choropleth_DSPT_CCG_",Sys.Date(),".html"))





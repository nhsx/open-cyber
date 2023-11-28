
# MPBF
# 16/11/2021
# Adapted from au-cyberdeepdive repo
# Dependencies: main_jan21.R output
#########################################################################################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(leaflet)
### Load libraries
library(tidyverse)
library(readxl)
library(openxlsx)
library(gtsummary)
library(gt)
library(here)
library(webshot)
library(aod) # for wald.test
library(summarytools) # for ctable
library(ggpubr) # to use ggboxplot
#install.packages(c("httr", "jsonlite"))
library(httr)
library(jsonlite)
library(lubridate)
library(xtable)
library(plotly)
library(htmlwidgets)
library(leaflet.minicharts)
library(leaflet)
library(sf)
library(sp)
library(st)

#############################################
# DSPT curated file
#############################################


## Load 'curated' DSPT file
data <- read.csv("./data/DSPT Snapshots/22_23/ICB_dspt_snapshot_16_11_23.csv")

dsptlevels=c("22/23 Standards Exceeded","22/23 Standards Met","22/23 Approaching Standards","22/23 Standards Not Met","22/23 Not Published")

# Make Status a categorical
data$Short.Status = factor(data$Status,dsptlevels)

#############################################
# STP shapefile
#############################################

## Load STP shapefile
# Source: https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en/explore?location=52.950000%2C-2.000000%2C7.02
stp_spdf <- read_sf("./Inputs/shapefiles/22_23/ICB_APR_2023_EN_BSC.shp")

#proj4string(stp_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

#stp_spdf@proj4string # check system

stp_spdf <- stp_spdf %>% st_transform(CRS("+init=epsg:4326")) # reproject to latlong system

stp_data = stp_spdf


#############################################
# Load in Region data
#############################################

region_spdf = read_sf('./Inputs/shapefiles/22_23/NHSER_JUL_2022_EN_BUC.shp')
#proj4string(region_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

#region_spdf@proj4string # check system

region_spdf <- region_spdf %>% st_transform(CRS("+init=epsg:4326")) # reproject to latlong system

region_full <- region_spdf



#############################################
# Create a points shapefile for Trusts
##########+
##########+###################################
trusts_data = read.csv("./data/DSPT Snapshots/22_23/dspt_trusts_snapshot_16_11_23.csv")
#trusts_data <- subset(trusts_data, !(ODS.Code %in% c('RBZ', 'RW6', 'RTV')))


trust_spdf_points <- sp::SpatialPointsDataFrame(
  coords = trusts_data %>% select(long,lat),
  data = trusts_data %>% select(-c(long,lat)),
  proj4string = CRS("+init=epsg:4326") # indicate it is is longitude latitude
)



#############################################
#Further prep
#############################################
# Join DSPT data and ONS code
lookupdata = read.csv('./data/aux/22_23/Sub_ICB_Locations_to_Integrated_Care_Boards_to_NHS_England_(Region)_(April_2023)_Lookup_in_England (1).csv')
lookup_icb = lookupdata[!duplicated(lookupdata$ICB23CD), ]


#jon the shapefile data with dspt data for ICBs
icb_data = read.csv('./data/DSPT Snapshots/22_23/ICB_dspt_snapshot_16_11_23.csv')
stp_spdf = left_join(stp_spdf, icb_data %>% select(-c(X, ICB23NM, ICB23CDH, Organisation.Name, Integrated.Care.Board..where.available..from.ODS.)))
stp_spdf = left_join(stp_spdf, lookup_icb %>% select(-c(ICB23NM, ICB23CDH)), by = c('ICB23CD'))
stp_spdf$Short.Status = factor(stp_spdf$Status,dsptlevels)




# Choose a palette
my_palette <- colorspace::divergingx_hcl(5)

catpal <- colorFactor(my_palette, dsptlevels,reverse=F,ordered=T) 

## Plotting
# Prepare the text for tooltips:
mytext <- paste(
  "<b>ICB 2023 code (ODS): </b>", stp_spdf$ICB23CD, "<br/>",
  "<b>ICB 2023 name: </b>", stp_spdf$ICB23NM,"<br/>",
  "<b>Region name: </b>", stp_spdf$NHSER23NM,"<br/>",
  "<b>DSPT Status: </b>", stp_spdf$Short.Status, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


mytext_region <- paste(
  "<b>Region 2023 code: </b>", region_spdf$NHSER22CD,"<br/>",
  "<b>Region 2023 name: </b>", region_spdf$NHSER22NM,"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#############################################
# Mapping - CCGS
#############################################


# Final Map
m<-leaflet(stp_spdf) %>% 
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons( 
    fillColor = ~catpal(Status), 
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
  addLegend( pal=catpal, values=~Status, opacity=0.9, title = "22/23 ICB Status", position = "bottomleft" )

m



#############################################
# Mapping - CCGS + ICS + trust layer
#############################################
# https://gis.stackexchange.com/questions/283658/add-layers-with-leaflet-from-different-spatial-data-frames-in-r
m02 <- leaflet() %>% 
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons(
    data=stp_spdf,
    group = "ICB",
    fillColor = ~catpal(Short.Status), 
    stroke=TRUE, 
    fillOpacity = 0.7, 
    color="black", 
    weight=0.7,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addPolygons(
    data=region_spdf,
    group="Region boundary",
    fillOpacity=0.1,
    color='black',
    weight=2,
    label=mytext_region
  ) %>%
  #addLegend( data=ccg_spdf,pal=catpal, values=~Short.Status, opacity=0.9, title = "21/22 DSPT Status (CCG)", position = "bottomleft" ) %>%
  leaflet::addLayersControl(
    overlayGroups = c("ICB","Region boundary"),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
  ) %>% 
  hideGroup(c("Region boundary"))  # turn these off by default



m02_l <- m02 %>% addLegend( data=stp_spdf,pal=catpal, values=~Short.Status, opacity=0.9, title = "22/23 ICB DSPT Status", position = "bottomleft" )


get_popup_content <- function(my_spdf) {
  paste0(
    "<b>Provider </b>",
    "<br><b>- Provider code</b>:", my_spdf$Code,
    "<br><b>- Provider name:</b> ", my_spdf$Organisation.Name,
    #"<br><b>- STP/ICS (HQ postcode-based):</b> ", my_spdf$STP20NM,
    #"<br><b>- Region:</b> ", my_spdf$`NHSER20NM`,
    "<br><b>- DSPT status:</b> ", my_spdf$Status,
    sep="" 
  )
}



m03 <- m02_l %>%
  addCircleMarkers(data=trust_spdf_points,
                   group="Trusts",       
                   label = ~ lapply(get_popup_content(trust_spdf_points), htmltools::HTML),
                   fillColor = ~catpal(Status),
                   color="black",
                   weight=2,
                   fillOpacity = 1,
                   stroke = T,
                   #clusterOptions = markerClusterOptions(),
                   radius= 6)


#adding the zoom toggle for trust level (trust layer appears between 9 and 20)
m03 <- m03 %>% 
 groupOptions("Trusts", zoomLevels = 9:20)

#adding legend and layering CCG trusts and ICG boundary together
m03 <- m03 %>% 
  addLegend( data=trust_spdf_points,pal=catpal, values=~Status, opacity=0.9, title = "22/23 Trusts DSPT Status", position = "bottomright" ) %>%
  leaflet::addLayersControl(
    overlayGroups = c("ICB","Trusts", 'Region boundary'),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
  ) %>% 
  hideGroup(c("Region boundary"))  # turn these off by default


m03


# save the widget in a html file if needed.
#library(htmlwidgets)
saveWidget(m03, "icb_trusts_map_16_11_23.html")




#############################################
# Summary metric
#############################################
#get the stp codes for trusts from etr
# etr = read.csv('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/etr.csv', header = FALSE)
# etr = etr[,c('V1', 'V4')]
# etr = select(etr, 'ODS.Code' = 'V1', 'STP21CDH' = 'V4')
# 
# #get the final 4 stp codes of the 4 care trusts from ect (modified to change column names and select relevant columns)
# ect = read.csv('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/ect_modified_21_22.csv')
# ect = select(ect, 'ODS.Code', 'STP21CDH')
# 
# etr_ect = rbind(etr, ect)


trusts_data = trusts_data %>% rename(ICB23CDH = High.Level.Health.Geography,
                                     ICB23NM = Integrated.Care.Board..where.available..from.ODS.,
                                     Sector = Primary.Sector)

trusts_data2 = trusts_data %>% left_join(lookup_icb %>% select(-c(ICB23NM)))


trusts_data2 <- trusts_data2 %>% mutate(Status.Score=case_when(Status=="22/23 Standards Exceeded"~3,
                                                             Status=="22/23 Standards Met"~1,
                                                             Status=="22/23 Approaching Standards"~-1,
                                                             Status=="22/23 Standards Not Met"~-3,
                                                             Status=="Not Published"~-3))
data_metric_Trusts <- trusts_data2 %>% group_by(ICB23CD) %>% summarise(Simple.n=n(), Simple.Score.Trust = mean(Status.Score, na.rm=TRUE))


#add a mean score for ICB data too
stp_spdf <- stp_spdf %>% mutate(Status.Score=case_when(Status=="22/23 Standards Exceeded"~3,
                                                               Status=="22/23 Standards Met"~1,
                                                               Status=="22/23 Approaching Standards"~-1,
                                                               Status=="22/23 Standards Not Met"~-3,
                                                               Status=="Not Published"~-3))


stp_spdf2 <- stp_spdf %>% left_join(data_metric_Trusts)

stp_spdf2$metric_ICB_Trusts <- (0.5*stp_spdf2$Simple.Score.Trust) + (0.5*stp_spdf$Status.Score)






# Create a continuous palette function
# Create a continuous palette function in our desired range (+3 - 3)

# Create a continuous palette function in our desired range (+3 - 3)
pal_metric <- colorNumeric(
  palette = "RdYlBu",
  domain = range(-3:3))

#create the label text for the first composite map
mytext_ics_score <- paste(
  "<b>STP code (ODS): </b>", stp_spdf2$ICB23CD,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf2$ICB23NM,"<br/>",
  "<b>ICB score (ICB + Trust Simple), range [-3,3]: </b>",round(stp_spdf2$metric_ICB_Trusts,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)




#create the map using map panes
#add a layer for the ICS colour coded polygons
#add a layer for empty ICS polygons with just the labels (this has the highest zindex - will be the top layer)
#add a layer for the region boundaries to be displayed in blue
m04 = leaflet() %>%
  addMapPane(name = "regionBorder", zIndex = 425) %>%
  addMapPane(name = "ICB polygons", zIndex = 400) %>%
  addMapPane(name = "ICB Labels", zIndex = 450) %>%
  addPolygons(
    data=region_spdf,
    group="Region boundary",
    fillOpacity=0,
    color='blue',
    weight=5,
    options = leafletOptions(pane = "regionBorder")
  ) %>%
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons(
    data=stp_spdf2,
    group="ICS",
    fillOpacity=1,
    fillColor=~pal_metric(metric_ICB_Trusts),
    color="black",
    weight=1,
    options = leafletOptions(pane = "ICB polygons")) %>%
  addPolygons(
    data=stp_spdf2,
    group="ICS",
    fillOpacity=0,
    fillColor=~pal_metric(metric_ICB_Trusts),
    color="black",
    weight= 0,
    options = leafletOptions(pane = "ICB Labels"),
    label = mytext_ics_score) %>%
  addLegend("bottomright",pal=pal_metric,values=-3:3,title="ICB score (ICB + Trust Simple)")

m04



# save the widget in a html file if needed.
#library(htmlwidgets)
saveWidget(m04, "icb_composite_map_16_11_23.html")
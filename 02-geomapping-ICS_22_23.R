
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
# CCG shapefile #CCGs will not be submitting for 22/23 
#############################################

## Load CCG shapefile

#ccg_spdf <- read_sf("/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/Inputs/shapefile/CCG_APR_2021_EN_BFC.shp")

#proj4string(ccg_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

#ccg_spdf@proj4string # check system

#ccg_spdf <- ccg_spdf %>% st_transform(CRS("+init=epsg:4326")) # reproject to latlong system

# Write to shapefile
#writeOGR(ccg_spdf, layer = 'myshp_simplified', 'C:/temp', driver="ESRI Shapefile")

#############################################
# Load in Region data
#############################################

region_spdf = read_sf('./Inputs/shapefiles/22_23/NHSER_JUL_2022_EN_BUC.shp')
#proj4string(region_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

#region_spdf@proj4string # check system

region_spdf <- region_spdf %>% st_transform(CRS("+init=epsg:4326")) # reproject to latlong system

region_full <- region_spdf

#region_s <- rgeos::gSimplify(region_full,tol=0.01, topologyPreserve=FALSE)

# Create a spatial polygon data frame (includes shp attributes)
#regions_spdf = SpatialPolygonsDataFrame(region_full, data.frame(region_full))



##Load correspondence from ODS to ONS codes
# https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2020-names-and-codes-in-england/explore

#lkp_ccg <- read.csv('./Inputs/Clinical_Commissioning_Groups_(April_2020)_Names_and_Codes_in_England.csv')


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
#data_merged = left_join(data, lookupdata %>% select(-c(ICB23CD, ICB23NM)), by = c("ICB23CDH" = "ICB23CDH"))
#data_merged = data_merged %>% select(-c(X))

#jon the shapefile data with dspt data for ICBs
icb_data = read.csv('./data/DSPT Snapshots/22_23/ICB_dspt_snapshot_16_11_23.csv')
stp_spdf = left_join(stp_spdf, icb_data %>% select(-c(X, ICB23NM, ICB23CDH, Organisation.Name, Integrated.Care.Board..where.available..from.ODS.)))
stp_spdf = left_join(stp_spdf, lookup_icb %>% select(-c(ICB23NM, ICB23CDH)), by = c('ICB23CD'))
stp_spdf$Short.Status = factor(stp_spdf$Status,dsptlevels)
# Make Status a categorical
#ccg_spdf@data$Short.Status = factor(ccg_spdf@data$Short.Status,dsptlevels)



# Choose a palette
#my_palette <- rev(colorspace::rainbow_hcl(5))
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
#m03 <- m03 %>% 
#  groupOptions("Trusts", zoomLevels = 9:20)

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
saveWidget(m04, "icb_composite_map_16_11_23.html")




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
                                                             Status=="22/23 Not Published"~-3))
data_metric_Trusts <- trusts_data2 %>% group_by(ICB23CD) %>% summarise(Simple.n=n(), Simple.Score.Trust = mean(Status.Score, na.rm=TRUE))


#add a mean score for ICB data too
stp_spdf <- stp_spdf %>% mutate(Status.Score=case_when(Status=="22/23 Standards Exceeded"~3,
                                                               Status=="22/23 Standards Met"~1,
                                                               Status=="22/23 Approaching Standards"~-1,
                                                               Status=="22/23 Standards Not Met"~-3,
                                                               Status=="22/23 Not Published"~-3))


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

##############################################################################################
# Mapping the composite ICS for CCG(population weighted) and Trusts (Weighted for EPRR risk)
##############################################################################################

# #create new map labels to contain the dspt metric and stp region info
# mytext_new <- paste(
#   "<b>STP code (ODS): </b>", stp_spdf$stp20cd,"<br/>",
#   "<b>STP name (ODS): </b>", stp_spdf$stp20nm,"<br/>",
#   "<b>Region: </b>", stp_spdf$NHSER20NM.y,"<br/>",
#   "<b>ICS score (CCG+Trust simple), range [-3,3]: </b>",round(stp_spdf$metric_CCGp_Trusts_EPRR.x,2),"<br/>",
#   sep="") %>%
#   lapply(htmltools::HTML)
# 
# 
# 
# m05 = leaflet() %>%
#   addMapPane(name = "regionBorder", zIndex = 425) %>%
#   addMapPane(name = "ICS polygons", zIndex = 400) %>%
#   addMapPane(name = "ICS Labels", zIndex = 450) %>%
#   addMapPane(name = "Minicharts", zIndex = 435) %>%
#   addPolygons(
#     data=region_spdf,
#     group="Region boundary",
#     fillOpacity=0,
#     color='blue',
#     weight=5,
#     options = leafletOptions(pane = "regionBorder")
#   ) %>%
#   addTiles()  %>% 
#   setView( lat=53, lng=-2 , zoom=6) %>%
#   addPolygons(
#     data=stp_spdf,
#     group="ICS",
#     fillOpacity=1,
#     fillColor=~pal_metric(metric_CCGp_Trusts_EPRR),
#     color="black",
#     weight=1,
#     options = leafletOptions(pane = "ICS polygons")) %>%
#   addPolygons(
#     data=stp_spdf,
#     group="ICS",
#     fillOpacity=0,
#     fillColor=~pal_metric(metric_CCGp_Trusts_EPRR),
#     color="black",
#     weight= 0,
#     options = leafletOptions(pane = "ICS Labels"),
#     label = mytext_new ) %>%
#   addLegend("bottomright",pal=pal_metric,values=-3:3,title="ICS score - CCG Population/Trust EPRR")
# 
# m05



###############################################################################################################################################
# Mapping the proportion of Trusts dspt scores using minicharts (piecharts) with the width of pie charts indicating patient population level
###############################################################################################################################################
#the statuses will be seperated out into individual columns and the values will be summed grouped by 
#the STP code so we get the proportion of trusts that met each status for each STP

#load in the data and filter for trusts changing the statuses to numerical values
data_trusts =  data_joint %>% filter(Sector %in% c("Trust"))
#data_trusts <- data_trusts[c("ODS.Code", "ODS.Org.Name", "STP21CD", "Status")]

#change all the instances of each dspt score to 1 so this can be summed as a numerical tally to make the charts
data_trusts<- data_trusts %>% mutate(Standards_Met = case_when(Status == "21/22 Standards Met"~1,
                                                               TRUE ~ 0))
data_trusts<- data_trusts %>% mutate(Standards_Exceeded = case_when(Status == "21/22 Standards Exceeded"~1,
                                                                    TRUE ~ 0))
data_trusts<- data_trusts %>% mutate(Standards_Not_Met = case_when(Status == "21/22 Standards Not Met"~1,
                                                                   TRUE ~ 0))
data_trusts<- data_trusts %>% mutate(Approaching_Standards = case_when(Status == "21/22 Approaching Standards"~1,
                                                                       TRUE ~ 0))
#select the relevant columns only
data_trusts = data_trusts[,c("STP21CD", "Standards_Met", "Standards_Exceeded", "Standards_Not_Met", "Approaching_Standards")]

#get the sum of each DSPT metric in separate columns grouped by each STP
#data_trusts_aggregate = data_trusts %>% group_by(STP21CD) %>% summarise_each(funs(sum))
data_trusts_aggregate = data_trusts %>% group_by(STP21CD) %>% summarise_at(vars(Standards_Met, Standards_Exceeded, Standards_Not_Met, Approaching_Standards), funs(sum))

#data_trusts_aggregate = data_trusts_aggregate %>% rename("STP21CD" = 1)

#wrangle teh GP population data to be used as the diameter for each of the mini piecharts
stp_filter_numpatients <- gppopdata %>% filter(SEX=="ALL",AGE=="ALL",ORG_TYPE=="ICB")

#stp_filter_numpatients <- stp_filter_numpatients %>% rename("STP21CD" = 5)
#stp_filter_numpatients <- stp_filter_numpatients[c("STP21CD", "NUMBER_OF_PATIENTS")]
#stp_filter_numpatients <- unique(stp_filter_numpatients)
#merge together the separated dspt data with the gp population data and stp spatial data frame for mapping
data_trust_spdf_pie = left_join(x = stp_spdf, y = data_trusts_aggregate, by = "STP21CD")
#data_trust_spdf_pie = merge(x = data_trust_spdf_pie, y = stp_filter_numpatients, by = "STP21CD")
data_trust_spdf_pie[41, 7] = 51.0
data_trust_spdf_pie[41, 6] = 0.0

#create the map with the ICS boundaries displayed in black

class(data_trust_spdf_pie) = 'data.frame'
m06 <- leaflet() %>%
  addTiles %>%
  addPolygons(
    data=stp_spdf,
    group="ICS Tiles",
    fillOpacity=0.8,
    color='gray', #grey out the ICS tiles so there is less unnecessary detail
    weight=0,
    label=mytext_new) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS boundary",
    fillOpacity=0,
    color='black',
    weight=5,
    label=mytext_new) %>%
  addMinicharts(lng = data_trust_spdf_pie$LONG, 
                lat = data_trust_spdf_pie$LAT, 
                type = "pie", 
                chartdata = data_trust_spdf_pie[, c("Standards_Exceeded", "Standards_Met", "Approaching_Standards", "Standards_Not_Met")], 
                colorPalette = c("#129F8C", '#9FD0BA', "#F5FFBF", "#FF4227"), 
                #width = 0.00001 * data_trust_spdf_pie$NUMBER_OF_PATIENTS , 
                transitionTime = 0)
m06

###############################################################################################################################################
# Mapping the proportion of Trusts dspt scores using minicharts (piecharts) with patient population level color coded in ICS polygons
###############################################################################################################################################

gppopdata_blue <- gppopdata %>% filter(SEX=="ALL",AGE=="ALL",ORG_TYPE=="ICB") %>% select(c("ONS_CODE","NUMBER_OF_PATIENTS"))
gppopdata_blue <- gppopdata_blue %>% rename("STP21CD" = 1)

stp_spdfdata <- stp_spdf

stp_spdfdata1 = merge(stp_spdfdata, gppopdata_blue, by = "STP21CD")

#stp_spdf$data <- stp_spdfdata1

num_patients_stp <- gppopdata_blue[c("NUMBER_OF_PATIENTS")]

minpatients <- min(num_patients_stp)

maxpatients <- max(num_patients_stp)


patients_stps <- stp_spdfdata$data.NUMBER_OF_PATIENTS
pal_metric2 <- colorNumeric(
  palette = "Blues",
  domain = range(minpatients:maxpatients))
m07 <- leaflet() %>%
  addTiles %>%
  addPolygons(
    data=stp_spdf,
    group="ICS Tiles",
    fillOpacity=1,
    fillColor=~pal_metric2(stp_spdfdata1$NUMBER_OF_PATIENTS), #grey out the ICS tiles so there is less unnecessary detail
    weight=0,
    label=mytext_ics) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS boundary",
    fillOpacity=0,
    color='black',
    weight=5,
    label=mytext_ics) %>%
  addMinicharts(lng = data_trust_spdf_pie$LONG, 
                lat = data_trust_spdf_pie$LAT, 
                type = "pie", 
                chartdata = data_trust_spdf_pie[, c("Standards_Exceeded", "Standards_Met", "Approaching_Standards", "Standards_Not_Met")], 
                colorPalette = c("#129F8C", '#9FD0BA', "#F5FFBF", "#FF4227"), 
                width = 25, 
                transitionTime = 0) %>%
  addLegend("topright",pal=pal_metric2, minpatients:maxpatients, title="ICS Patient Population Level")

m07

###############################################################
#load in the snapshot data to create summary tables and charts
###############################################################
#load in the snapshot data and filter for the 5 relevant CSUs
data_snapshot = read.csv('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/DSPT search results 09_09_2022 12_44_07.csv')
data_summary = data_snapshot %>% select('Organisation.Name', 'Status', 'Primary.Sector')
data_summary = data_summary %>% filter(Primary.Sector == "Commissioning Support Unit (CSU)")
data_csu = data_summary %>% filter(str_detect(Organisation.Name, 'CSU'))

#get the joint dataframe of CCGs and Trusts and merge with CSUs
data_joint$Primary.Sector = data_joint$Sector
data_joint$Organisation.Name = data_joint$ODS.Org.Name

data_s = rbind(select(data_joint, 'Organisation.Name', 'Status', 'Primary.Sector'), data_csu)

#data_s = data_summary %>% mutate(Short.Status = case_when(Status %in% c("20/21 Standards Met", "18/19 Standards Met", "19/20 Standards Met", "19/20 Approaching Standards", "19/20 Standards Exceeded", "20/21 Standards Exceeded", "20/21 Standards Not Met") ~ '21/22 Status Not Met',
#                                                    Status %in% c("22/23 Standards Met") ~ '21/22 Standards Met', TRUE ~ Status))


auxl <-data_s %>% group_by(Primary.Sector,Status) %>% summarise(n=n())
aux <- data_s %>% group_by(Primary.Sector,Status) %>% summarise(n=n()) %>% pivot_wider(names_from='Status',values_from='n')
aux <- select(aux, "21/22 Standards Exceeded", "21/22 Standards Met", "21/22 Approaching Standards", "21/22 Standards Not Met")
org_type <-aux$Primary.Sector

xform <- list(categoryorder = "array",
              categoryarray = c("21/22 Standards Exceeded", "21/22 Standards Met", "21/22 Approaching Standards", "21/22 Standards Not Met"))

fig_x <- auxl %>% plot_ly(x=~Primary.Sector,y= ~n,color=~Status,type='bar')%>% 
  layout(xaxis = xform)
fig_x

tbl_summary(select(data_s, 'Primary.Sector', 'Status'), by = (c('Status')))


ct_final = ctable(data_s$Primary.Sector, data_s$Status,
                  prop = "r", chisq = FALSE, headings = FALSE
)
ct_final %>% print(method="browser")
ct_final %>% print(file=paste0("/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/cross_table_summary_21_22",Sys.Date(),".html"))
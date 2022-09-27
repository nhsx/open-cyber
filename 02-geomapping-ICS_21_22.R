# MPBF
# 16/11/2021
# Adapted from au-cyberdeepdive repo
# Dependencies: main_jan21.R output
#########################################################################################

library(rgdal)
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

#############################################
# DSPT curated file
#############################################


## Load 'curated' DSPT file
data <- read.csv("/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/DSPT_CCG_21_22_snapshot09_09_22.csv")
data <- data[-107,]

dsptlevels=c("21/22 Standards Exceeded","21/22 Standards Met","21/22 Approaching Standards","21/22 Standards Not Met","21/22 Not Published")

# Make Status a categorical
data$Short.Status = factor(data$Status,dsptlevels)

#############################################
# STP shapefile
#############################################

## Load STP shapefile
# Source: https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en/explore?location=52.950000%2C-2.000000%2C7.02
stp_spdf <- readOGR("/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/Inputs/shapefile/STP_APR_2021_EN_BUC_V2.shp")

proj4string(stp_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

stp_spdf@proj4string # check system

stp_spdf <- stp_spdf %>% sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system


#############################################
# CCG shapefile
#############################################

## Load CCG shapefile

ccg_spdf <- readOGR("/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/Inputs/shapefile/CCG_APR_2021_EN_BFC.shp")

proj4string(ccg_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

ccg_spdf@proj4string # check system

ccg_spdf <- ccg_spdf %>% sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system

# Write to shapefile
#writeOGR(ccg_spdf, layer = 'myshp_simplified', 'C:/temp', driver="ESRI Shapefile")


##Load correspondence from ODS to ONS codes
# https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2020-names-and-codes-in-england/explore

#lkp_ccg <- read.csv('./Inputs/Clinical_Commissioning_Groups_(April_2020)_Names_and_Codes_in_England.csv')


#############################################
# Create a points shapefile for Trusts
##########+
##########+###################################
trusts_data = read.csv("/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/DSPT_trusts_21_22_snapshot09_09_22.csv")
trusts_data <- subset(trusts_data, !(ODS.Code %in% c('RBZ', 'RW6', 'RTV')))


trust_spdf_points <- sp::SpatialPointsDataFrame(
  coords = trusts_data %>% select(longitude,latitude),
  data = trusts_data %>% select(-c(longitude,latitude)),
  proj4string = CRS("+init=epsg:4326") # indicate it is is longitude latitude
)




#############################################
#Further prep
#############################################
# Join DSPT data and ONS code
lookupdata = read.csv('/Users/muhammad-faaiz.shanawas/Documents/Github/open-cyber/data/Clinical_Commissioning_Group_to_STPs_(April_2021)_Lookup_in_England.csv')
data_merged = left_join(data, lookupdata, by = c("ODS.Code" = "CCG21CDH"))
data_merged = data_merged[-107,]

# Join the reduced DSPT info with the CCG shapefile
ccg_spdf@data <- left_join(ccg_spdf@data,data_merged,by=c("CCG21CD"="CCG21CD"))
data_ccg_spdf <- ccg_spdf@data


# Make Status a categorical
ccg_spdf@data$Short.Status = factor(ccg_spdf@data$Short.Status,dsptlevels)



# Choose a palette
#my_palette <- rev(colorspace::rainbow_hcl(5))
my_palette <- colorspace::divergingx_hcl(5)

catpal <- colorFactor(my_palette, dsptlevels,reverse=F,ordered=T) 

## Plotting
# Prepare the text for tooltips:
mytext <- paste(
  "<b>ICB 2022 code (ODS): </b>", ccg_spdf@data$ICB22CD.x,"<br/>",
  "<b>ICB 2022 name: </b>", ccg_spdf@data$LOC22NM.y,"<br/>",
  "<b>2021 STP name: </b>", ccg_spdf@data$STP21NM.y,"<br/>",
  "<b>Region name: </b>", ccg_spdf@data$NHSER22NM.y,"<br/>",
  "<b>DSPT Status: </b>", ccg_spdf@data$Short.Status.y, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

mytext_ics <- paste(
  "<b>ICS/STP 2021 code: </b>", stp_spdf@data$STP21CD,"<br/>",
  "<b>ICS/STP 2021 name: </b>", stp_spdf@data$STP21NM,"<br/>",
  "<b>Region name: </b>", ccg_spdf@data$NHSER22NM.y,"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#############################################
# Mapping - CCGS
#############################################


# Final Map
m<-leaflet(ccg_spdf) %>% 
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons( 
    fillColor = ~catpal(Short.Status.y), 
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
  addLegend( pal=catpal, values=~Short.Status.y, opacity=0.9, title = "21/22 DSPT Status (CCG)", position = "bottomleft" )

m

#############################################
# Mapping - CCGS + ICS layer
#############################################

# https://gis.stackexchange.com/questions/283658/add-layers-with-leaflet-from-different-spatial-data-frames-in-r
m02 <- leaflet() %>% 
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons(
    data=ccg_spdf,
    group = "CCG",
    fillColor = ~catpal(Short.Status.y), 
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
    data=stp_spdf,
    group="ICS boundary",
    fillOpacity=0.1,
    color='black',
    weight=2,
    label=mytext_ics
  ) %>%
  #addLegend( data=stp_spdf,pal=catpal, values=~Short.Status, opacity=0.9, title = "21/22 DSPT Status (CCG)", position = "bottomleft" ) %>%
  leaflet::addLayersControl(
    overlayGroups = c("CCG","ICS boundary"),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
  ) %>% 
  hideGroup(c("ICS boundary"))  # turn these off by default



m02_l <- m02 %>% addLegend( data=ccg_spdf,pal=catpal, values=~Short.Status.y, opacity=0.9, title = "21/22 DSPT Status (CCG)", position = "bottomleft" )



#############################################
# Mapping - CCGS + ICS + trust layer
#############################################


get_popup_content <- function(my_spdf) {
  paste0(
    "<b>Provider </b>",
    "<br><b>- Provider code</b>:", my_spdf@data$ODS.Code,
    "<br><b>- Provider name:</b> ", my_spdf@data$ODS.Org.Name,
    #"<br><b>- STP/ICS (HQ postcode-based):</b> ", my_spdf@data$STP20NM,
    #"<br><b>- Region:</b> ", my_spdf@data$`NHSER20NM`,
    "<br><b>- DSPT status:</b> ", my_spdf@data$Status,
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
  
m03

#adding the zoom toggle for trust level (trust layer appears between 9 and 20)
#m03 <- m03 %>% 
#  groupOptions("Trusts", zoomLevels = 9:20)

#adding legend and layering CCG trusts and ICG boundary together
m03 <- m03 %>% 
  addLegend( data=trust_spdf_points,pal=catpal, values=~Status, opacity=0.9, title = "21/22 DSPT Status (trust)", position = "bottomright" ) %>%
  leaflet::addLayersControl(
  overlayGroups = c("ICS boundary","CCG","Trusts"),  # add these layers
  options = layersControlOptions(collapsed = FALSE)  # expand on hover?
) %>% 
  hideGroup(c("ICS boundary","Trusts"))  # turn these off by default
  

m03



# save the widget in a html file if needed.
#library(htmlwidgets)
saveWidget(m04, file=paste0( getwd(), "choropleth_DSPT_CCG_",Sys.Date(),".html"))




#############################################
# Summary metric
#############################################

data_metric <- data %>% filter(Sector %in% c("Trust","CCG"))


# GP practice population
# https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/march-2021
gppopdata <- read_csv("https://files.digital.nhs.uk/59/D3AD40/gp-reg-pat-prac-sing-age-regions.csv")

gppopdata_red <- gppopdata %>% filter(SEX=="ALL",AGE=="ALL",ORG_TYPE=="CCG") %>% select(c("ORG_CODE","NUMBER_OF_PATIENTS"))




# score mapping

data_metric <- data_metric %>% mutate(Status.Score=case_when(Short.Status=="Standards Exceeded"~3,
                                                             Short.Status=="Standards Met"~1,
                                                             Short.Status=="Approaching Standards"~-1,
                                                             Short.Status=="Standards Not Met"~-3,
                                                             Short.Status=="Not Published"~-3))


NUMBER_OF_PATIENTS = gppopdata['NUMBER_OF_PATIENTS']
data_metric <- data_metric %>% left_join(gppopdata_red,by=c("Code"="ORG_CODE"))                                                             #TRUE~NA_integer_))

data_metric_ICS <- data_metric %>% group_by(STP20CD,STP20NM,Sector,NHSER20NM) %>% summarise(Simple.Score=mean(Status.Score,na.rm=T),
                                                                                  Simple.n=n(),
                                                                                  Pop.Score=sum(NUMBER_OF_PATIENTS*Status.Score)/sum(NUMBER_OF_PATIENTS))

data_metric_ICS <- data_metric_ICS %>% pivot_wider(names_from=Sector,values_from=c("Simple.Score","Simple.n","Pop.Score"))

data_metric_ICS <- data_metric_ICS %>% mutate(metric_CCG_simple = Simple.Score_CCG,
                                              metric_CCG_pop = Pop.Score_CCG,
                                              metric_CCGTrust_simple = 0.5*Simple.Score_CCG+0.5*Simple.Score_Trust,
                                              metric_CCGp_Trusts =0.5*Pop.Score_CCG+0.5*Simple.Score_Trust)






stp_spdf@data <- stp_spdf@data %>% left_join(data_metric_ICS,by=c("stp20nm"="STP20NM","stp20cd"="STP20CD"))



# Create a continuous palette function
pal_metric <- colorNumeric(
  palette = "RdYlBu",
  domain = stp_spdf@data$metric_CCGTrust_simple)

mytext_ics_score <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$stp20cd,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$stp20nm,"<br/>",
  "<b>ICS score (CCG+Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGTrust_simple,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)



region_spdf = readOGR('./Inputs/shapefile/NHS_England_Regions_(April_2020)_Boundaries_EN_BUC.shp')
proj4string(region_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

region_spdf@proj4string # check system

region_spdf <- region_spdf %>% sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system

region_full <- region_spdf

region_s <- rgeos::gSimplify(region_full,tol=0.01, topologyPreserve=FALSE)

# Create a spatial polygon data frame (includes shp attributes)
regions_spdf = SpatialPolygonsDataFrame(region_s, data.frame(region_full))


data_regions = data[c("STP20NM", "NHSER20NM")]
names(data_regions)[names(data_regions) == "STP20NM"] <- "stp20nm"

#merge and assign to stp_spdf data
stp_spdfdata = stp_spdf@data
stp_spdfdata = merge(stp_spdfdata, data_regions, by = "stp20nm", all = TRUE)

mytext_new <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$stp20cd,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$stp20nm,"<br/>",
  "<b>Region: </b>", stp_spdf@data$NHSER20NM.x,"<br/>",
  "<b>ICS score (CCG+Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGTrust_simple,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)



m04 = leaflet() %>%
  addMapPane(name = "regionBorder", zIndex = 425) %>%
  addMapPane(name = "ICS polygons", zIndex = 400) %>%
  addMapPane(name = "ICS Labels", zIndex = 450) %>%
  addPolygons(
    data=regions_spdf,
    group="Region boundary",
    fillOpacity=0,
    color='blue',
    weight=5,
    options = leafletOptions(pane = "regionBorder")
  ) %>%
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS",
    fillOpacity=1,
    fillColor=~pal_metric(metric_CCGTrust_simple),
    color="black",
    weight=1,
    options = leafletOptions(pane = "ICS polygons")) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS",
    fillOpacity=0,
    fillColor=~pal_metric(metric_CCGTrust_simple),
    color="black",
    weight= 0,
    options = leafletOptions(pane = "ICS Labels"),
    label = mytext_new ) %>%
  addLegend("bottomright",pal=pal_metric,values=stp_spdf@data$metric_CCGTrust_simple,title="ICS score - CCG/Trust simple")

m04

#filter data to work out proprotion of DSPT for trusts in each CCG
data_trusts = data %>% filter(Sector=="Trust")
data_trusts2 = data_trusts %>% count(STP20CD, STP20NM, Short.Status, sort = TRUE)
data_trusts4 = unique(cbind.data.frame(c(data_trusts2$STP20CD)))
data_trusts4 <- data_trusts4 %>% rename("STP20CD" = 1)

data_trust_met = data_trusts2 %>% filter(Short.Status == "Standards Met")
data_trust_met = data_trust_met[c("STP20CD", "n")]
data_trust_met <- data_trust_met %>% rename("Standards Met" = "n")

data_trust_exceeded = data_trusts2 %>% filter(Short.Status == "Standards Exceeded")
data_trust_exceeded = data_trust_exceeded[c("STP20CD", "n")]
data_trust_exceeded <- data_trust_exceeded %>% rename("Standards Exceeded" = 2)

data_trust_approaching = data_trusts2 %>% filter(Short.Status == "Approaching Standards")
data_trust_approaching = data_trust_approaching[c("STP20CD", "n")]
data_trust_approaching <- data_trust_approaching %>% rename("Approaching Standards" = 2)

data_trust_notmet = data_trusts2 %>% filter(Short.Status == "Standards Not Met")
data_trust_notmet = data_trust_notmet[c("STP20CD", "n")]
data_trust_notmet <- data_trust_notmet %>% rename("Standards Not Met" = 2)

data_trusts5 <- left_join(x = data_trusts4, y = data_trust_met)
data_trusts6 <- left_join(x = data_trusts5, y = data_trust_exceeded)
data_trusts7 <- left_join(x = data_trusts6, y = data_trust_approaching)
data_trusts8 <- left_join(x = data_trusts7, y = data_trust_notmet)

data_trusts8[is.na(data_trusts8)] <- 0
data_trusts8 <- data_trusts8 %>% rename("stp20cd" = "STP20CD")
trust_spdf_pie <- stp_spdf
data_trust_spdf_pie <- left_join(x = data_trusts8, y = stp_spdf@data, by = "stp20cd")

trust_spdf_pie@data = data_trust_spdf_pie
stp_filter_numpatients <- gppopdata %>% filter(SEX=="ALL",AGE=="ALL",ORG_TYPE=="STP")
library(leaflet.minicharts)
m05 <- m02 %>%
  addMinicharts(lng = data_trust_spdf_pie$long, 
                lat = data_trust_spdf_pie$lat, 
                type = "pie", 
                chartdata = data_trust_spdf_pie[, c("Standards Met", "Standards Exceeded", "Approaching Standards", "Standards Not Met")], 
                colorPalette = c("#104E8B", "#FF00FF", "#3093e5", "#fcba50"), 
                width = 60 * sqrt(stp_filter_numpatients$NUMBER_OF_PATIENTS) / sqrt(max(stp_filter_numpatients$NUMBER_OF_PATIENTS)), 
                transitionTime = 0)

m05 <- m05 %>%
  #addLegend( data=trust_spdf_points,pal=catpal, values=~Short.Status, opacity=0.9, title = "20/21 DSPT Status (trust)", position = "bottomright" ) %>%
  leaflet::addLayersControl(
    overlayGroups = c("ICS boundary","CCG"),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
  ) %>% 
  hideGroup(c("ICS boundary"))  # turn these off by default
m05






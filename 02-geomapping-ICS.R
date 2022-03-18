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
library(leaflet.minicharts)

#############################################
# DSPT curated file
#############################################


## Load 'curated' DSPT file
data <- read_csv("./outputs/data_DSPTmetric_20220208.csv")


dsptlevels=c("Standards Exceeded","Standards Met","Approaching Standards","Standards Not Met","Not Published")

# Make Status a categorical
data$Short.Status = factor(data$Short.Status,dsptlevels)

#############################################
# STP shapefile
#############################################

## Load STP shapefile
# Source: https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en/explore?location=52.950000%2C-2.000000%2C7.02
stp_spdf <- readOGR("./Inputs/shapefile/Sustainability_and_Transformation_Partnerships_(April_2020)_Boundaries_EN_BUC.shp")

proj4string(stp_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

stp_spdf@proj4string # check system

stp_spdf <- stp_spdf %>% sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system


#############################################
# CCG shapefile
#############################################

## Load CCG shapefile
# Source: https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en/explore?location=52.950000%2C-2.000000%2C7.02
ccg_spdf <- readOGR("./Inputs/shapefile/Clinical_Commissioning_Groups_(April_2020)_EN_BFC_V2.shp")

proj4string(ccg_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

ccg_spdf@proj4string # check system

ccg_spdf <- ccg_spdf %>% sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system

ccg_spdf_full <- ccg_spdf

#### Simplifying
# https://stackoverflow.com/questions/34827043/gsimplify-not-simplifying-shapefile-in-r
# https://gis.stackexchange.com/questions/151924/writeogr-with-a-spatialpolygon-simplified-by-gsimplify?rq=1
#ccg_spdf <- rmapshaper::ms_simplify() # simplify the mapping (load time)

ccg_s_sp <- rgeos::gSimplify(ccg_spdf_full,tol=0.01, topologyPreserve=FALSE)

# Create a spatial polygon data frame (includes shp attributes)
ccg_spdf = SpatialPolygonsDataFrame(ccg_s_sp, data.frame(ccg_spdf_full))

# Write to shapefile
#writeOGR(ccg_spdf, layer = 'myshp_simplified', 'C:/temp', driver="ESRI Shapefile")


##Load correspondence from ODS to ONS codes
# https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2020-names-and-codes-in-england/explore

#lkp_ccg <- read.csv('./Inputs/Clinical_Commissioning_Groups_(April_2020)_Names_and_Codes_in_England.csv')


#############################################
# Create a points shapefile for Trusts
#############################################
trust_spdf_points <- sp::SpatialPointsDataFrame(
  coords = data %>% filter(Sector=="Trust") %>% select(longitude,latitude),
  data = data %>% filter(Sector=="Trust") %>% select(-c(longitude,latitude)),
  proj4string = CRS("+init=epsg:4326") # indicate it is is longitude latitude
)




#############################################
#Further prep
#############################################


# Join DSPT data and ONS code
#data <- data %>% left_join(lkp_ccg %>% select(c("CCG20CD","CCG20CDH")),by=c("Code"="CCG20CDH"))

# Select relevant columns of data
#data_red <- data %>% select(Final.Status,CCG20CD,Code,Successor.Code)

# Join the reduced DSPT info with the CCGshapefile
ccg_spdf@data <- left_join(ccg_spdf@data %>% select(-c("ccg20nm")),data,by=c("ccg20cd"="CCG20CD"))


#dsptlevels=c("Standards Exceeded","Standards Met","Approaching Standards","Standards Not Met","Not Published")

# Make Status a categorical
#ccg_spdf@data$Short.Status = factor(ccg_spdf@data$Short.Status,dsptlevels)



# Choose a palette
#my_palette <- rev(colorspace::rainbow_hcl(5))
my_palette <- colorspace::divergingx_hcl(5)

catpal <- colorFactor(my_palette, dsptlevels,reverse=F,ordered=T) 

## Plotting
# Prepare the text for tooltips:
mytext <- paste(
  "<b>CCG code (ODS): </b>", ccg_spdf@data$Code,"<br/>",
  "<b>CCG name: </b>", ccg_spdf@data$CCG20NM,"<br/>",
  "<b>CCG Successor code (ODS): </b>", ccg_spdf@data$Successor.Code,"<br/>",
  "<b>STP name: </b>", ccg_spdf@data$STP20NM,"<br/>",
  "<b>Region name: </b>", ccg_spdf@data$NHSER20NM,"<br/>",
  "<b>DSPT Status: </b>", ccg_spdf@data$Final.Status, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

mytext_ics <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$stp20cd,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$stp20nm,"<br/>",
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
    fillColor = ~catpal(Short.Status), 
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
  addLegend( pal=catpal, values=~Short.Status, opacity=0.9, title = "20/21 DSPT Status (CCG)", position = "bottomleft" )

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
    data=stp_spdf,
    group="ICS boundary",
    fillOpacity=0.1,
    color='black',
    weight=2,
    label=mytext_ics
  ) %>%
  #addLegend( data=stp_spdf,pal=catpal, values=~Short.Status, opacity=0.9, title = "20/21 DSPT Status (CCG)", position = "bottomleft" ) %>%
  leaflet::addLayersControl(
    overlayGroups = c("CCG","ICS boundary"),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
  ) %>% 
  hideGroup(c("ICS boundary"))  # turn these off by default



m02_l <- m02 %>% addLegend( data=ccg_spdf,pal=catpal, values=~Short.Status, opacity=0.9, title = "20/21 DSPT Status (CCG)", position = "bottomleft" )


#############################################
# Mapping - CCGS + ICS + trust layer
#############################################


get_popup_content <- function(my_spdf) {
  paste0(
    "<b>Provider </b>",
    #"<br><b>- Provider code</b>:", my_spdf@data$Trust_Code,
    "<br><b>- Provider name:</b> ", my_spdf@data$Name,
    "<br><b>- STP/ICS (HQ postcode-based):</b> ", my_spdf@data$STP20NM,
    "<br><b>- Region:</b> ", my_spdf@data$`NHSER20NM`,
    "<br><b>- DSPT status:</b> ", my_spdf@data$Final.Status,
    sep="" 
  )
}



m03 <- m02_l %>%
  addCircleMarkers(data=trust_spdf_points,
                   group="Trusts",       
                   label = ~ lapply(get_popup_content(trust_spdf_points), htmltools::HTML),
                   fillColor = ~catpal(Short.Status),
                   color="black",
                   weight=2,
                   fillOpacity = 1,
                   stroke = T,
                   radius= 6)
                   #clusterOptions = markerClusterOptions())
  
m03

#adding the zoom toggle for trust level (trust layer appears between 9 and 20)
#m03 <- m03 %>% 
#  groupOptions("Trusts", zoomLevels = 9:20)

#adding legend and layering CCG trusts and ICG boundary together
m03 <- m03 %>% 
  addLegend( data=trust_spdf_points,pal=catpal, values=~Short.Status, opacity=0.9, title = "20/21 DSPT Status (trust)", position = "bottomright" ) %>%
  leaflet::addLayersControl(
  overlayGroups = c("ICS boundary","CCG","Trusts"),  # add these layers
  options = layersControlOptions(collapsed = FALSEca)  # expand on hover?
) %>% 
  hideGroup(c("ICS boundary","Trusts"))  # turn these off by default
  

m03



# save the widget in a html file if needed.

library(htmlwidgets)
saveWidget(m05, file=paste('./outputs/',"chloropleth_DSPT_PieCharts",".html"))




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
  domain = range(-3:3))

mytext_ics_score <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$stp20cd,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$stp20nm,"<br/>",
  "<b>ICS score (CCG+Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGp_Trusts,2),"<br/>",
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
  "<b>Region: </b>", stp_spdf@data$NHSER20NM,"<br/>",
  "<b>ICS score (CCG population + Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGp_Trusts,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


#colorpal <- colorBin(my_palette, dsptlevels, bins = c(0.0, 1.0, 1.5, 2.0, 2.5), reverse=F) 

m04 = leaflet() %>%
  addMapPane(name = "regionBorder", zIndex = 425) %>%
  addMapPane(name = "ICS polygons", zIndex = 400) %>%
  addMapPane(name = "ICS Labels", zIndex = 450) %>%
  addMapPane(name = "Minicharts", zIndex = 435) %>%
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
    fillColor=~pal_metric(metric_CCGp_Trusts),
    color="black",
    weight=1,
    options = leafletOptions(pane = "ICS polygons")) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS",
    fillOpacity=0,
    fillColor=~pal_metric(metric_CCGp_Trusts),
    color="black",
    weight= 0,
    options = leafletOptions(pane = "ICS Labels"),
    label = mytext_new ) %>%
  addLegend("bottomright",pal=pal_metric,values=-3:3,title="ICS score - CCG population/Trust simple")

m04




#PLOTTING PIE CHART MAP

#load in the data and filter for trusts changing the statuses to numerical values
#the statuses will be seperated out into individual columns and the values will be summed grouped by 
#the STP code so we get the proportion of trusts that met each status for each STP

data_trusts = data %>% filter(Sector=="Trust")
data_trusts <- data_trusts[c("Code", "Name", "STP20CD", "Short.Status")]

data_trusts<- data_trusts %>% mutate(Standards_Met = case_when(Short.Status == "Standards Met"~1,
                                                              TRUE ~ 0))
data_trusts<- data_trusts %>% mutate(Standards_Exceeded = case_when(Short.Status == "Standards Exceeded"~1,
                                                               TRUE ~ 0))
data_trusts<- data_trusts %>% mutate(Standards_Not_Met = case_when(Short.Status == "Standards Not Met"~1,
                                                               TRUE ~ 0))
data_trusts<- data_trusts %>% mutate(Approaching_Standards = case_when(Short.Status == "Approaching Standards"~1,
                                                               TRUE ~ 0))

data_trusts = data_trusts[,c("STP20CD", "Standards_Met", "Standards_Exceeded", "Standards_Not_Met", "Approaching_Standards")]

data_trusts_aggregate = data_trusts %>% group_by(STP20CD) %>% summarise_each(funs(sum))
data_trusts_aggregate = data_trusts_aggregate %>% rename("stp20cd" = 1)

stp_filter_numpatients <- gppopdata %>% filter(SEX=="ALL",AGE=="ALL",ORG_TYPE=="STP")
stp_filter_numpatients <- stp_filter_numpatients %>% rename("stp20cd" = 5)
stp_filter_numpatients <- stp_filter_numpatients[c("stp20cd", "NUMBER_OF_PATIENTS")]
data_trust_spdf_pie = left_join(x = data_trusts_aggregate, y = stp_spdf@data, by = "stp20cd")
data_trust_spdf_pie = merge(x = data_trust_spdf_pie, y = stp_filter_numpatients, by = "stp20cd")


m05 <- leaflet() %>%
  addTiles %>%
  addPolygons(
    data=stp_spdf,
    group="ICS Tiles",
    fillOpacity=0.8,
    color='gray',
    weight=0,
    label=mytext_ics) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS boundary",
    fillOpacity=0,
    color='black',
    weight=5,
    label=mytext_ics) %>%
  addMinicharts(lng = data_trust_spdf_pie$long, 
                lat = data_trust_spdf_pie$lat, 
                type = "pie", 
                chartdata = data_trust_spdf_pie[, c("Standards_Exceeded", "Standards_Met", "Approaching_Standards", "Standards_Not_Met")], 
                colorPalette = c("#129F8C", '#9FD0BA', "#F5FFBF", "#FF4227"), 
                width = 0.00001 * data_trust_spdf_pie$NUMBER_OF_PATIENTS , 
                transitionTime = 0)
m05


#Plotting region map with summary metric weighted for EPRR

#read in the eprr data 
#csv has been edited to take out first row of headings in matrix sheet

eprr_data = read.xlsx('./Inputs/eprr_rankings_data.xlsx', sheet = 2)
eprr_data <- eprr_data[ ,1:5]
eprr_data <- eprr_data %>% rename("Name"= 3)
eprr_data$Name = toupper(eprr_data$Name)

eprr_data <- eprr_data %>% rename("Code" = 1)

eprr_data<- eprr_data %>% mutate(Tier_rank = case_when(Tier == "Tier 1"~"4",
                                                       Tier == "Tier 2"~"3",
                                                       Tier == "Tier 3"~"2",
                                                       Tier == "Tier 4"~"1",
                                                       TRUE ~ "Not Applicable"))

eprr_data <- transform(eprr_data, Tier_rank = as.numeric(Tier_rank))
eprr_data <- eprr_data[c("Code", "Tier_rank")]



#recode the data metrics to add in the prr metric


data_metric <- data %>% filter(Sector %in% c("Trust", "CCG"))


# GP practice population
# https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/march-2021
gppopdata <- read_csv("https://files.digital.nhs.uk/59/D3AD40/gp-reg-pat-prac-sing-age-regions.csv")

gppopdata_red <- gppopdata %>% filter(SEX=="ALL",AGE=="ALL",ORG_TYPE=="CCG") %>% select(c("ORG_CODE","NUMBER_OF_PATIENTS"))



# score mapping

data_metric <- left_join(x = data_metric, y = eprr_data, by = "Code")

data_metric <- data_metric %>% mutate(Status.Score=case_when(Short.Status=="Standards Exceeded"~3,
                                                             Short.Status=="Standards Met"~1,
                                                             Short.Status=="Approaching Standards"~-1,
                                                             Short.Status=="Standards Not Met"~-3,
                                                             Short.Status=="Not Published"~-3))
NUMBER_OF_PATIENTS = gppopdata['NUMBER_OF_PATIENTS']
data_metric <- data_metric %>% left_join(gppopdata_red,by=c("Code"="ORG_CODE"))                                                             #TRUE~NA_integer_))

data_metric_ICS <- data_metric %>% group_by(STP20CD,STP20NM,Sector,NHSER20NM) %>% summarise(Simple.Score=mean(Status.Score,na.rm=T),
                                                                                            Simple.n=n(),
                                                                                            Pop.Score=sum(NUMBER_OF_PATIENTS*Status.Score)/sum(NUMBER_OF_PATIENTS),
                                                                                            EPRR.Score = sum(Tier_rank*Status.Score)/sum(Tier_rank))

data_metric_ICS <- data_metric_ICS %>% pivot_wider(names_from=Sector,values_from=c("Simple.Score","Simple.n","Pop.Score", "EPRR.Score"))

data_metric_ICS <- data_metric_ICS %>% mutate(metric_CCG_simple = Simple.Score_CCG,
                                              metric_CCG_pop = Pop.Score_CCG,
                                              metric_CCGTrust_simple = 0.5*Simple.Score_CCG+0.5*Simple.Score_Trust,
                                              metric_CCGp_Trusts =0.5*Pop.Score_CCG+0.5*Simple.Score_Trust,
                                              metric_CCGp_Trusts_EPRR =0.5*Pop.Score_CCG+0.5*EPRR.Score_Trust)


stp_spdf@data <- stp_spdf@data %>% left_join(data_metric_ICS,by=c("stp20nm"="STP20NM","stp20cd"="STP20CD"))



# Create a continuous palette function
pal_metric <- colorNumeric(
  palette = "RdYlBu",
  domain = range(-3:3))

mytext_ics_score <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$stp20cd,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$stp20nm,"<br/>",
  "<b>ICS score (CCG Population+Trust EPRR), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGp_Trusts_EPRR,2),"<br/>",
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
  "<b>Region: </b>", stp_spdf@data$NHSER20NM.y,"<br/>",
  "<b>ICS score (CCG+Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGp_Trusts_EPRR,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

m06 = leaflet() %>%
  addMapPane(name = "regionBorder", zIndex = 425) %>%
  addMapPane(name = "ICS polygons", zIndex = 400) %>%
  addMapPane(name = "ICS Labels", zIndex = 450) %>%
  addMapPane(name = "Minicharts", zIndex = 435) %>%
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
    fillColor=~pal_metric(metric_CCGp_Trusts_EPRR),
    color="black",
    weight=1,
    options = leafletOptions(pane = "ICS polygons")) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS",
    fillOpacity=0,
    fillColor=~pal_metric(metric_CCGp_Trusts_EPRR),
    color="black",
    weight= 0,
    options = leafletOptions(pane = "ICS Labels"),
    label = mytext_new ) %>%
  addLegend("bottomright",pal=pal_metric,values=-3:3,title="ICS score - CCG Population/Trust EPRR")

m06


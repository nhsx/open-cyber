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
library(leaflet)

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

stp_data = stp_spdf@data
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

region_spdf = readOGR('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/Inputs/shapefile/NHS_England_Regions_(April_2020)_Boundaries_EN_BUC.shp')
proj4string(region_spdf) <- CRS("+init=epsg:27700")  # BNG projection system

region_spdf@proj4string # check system

region_spdf <- region_spdf %>% sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system

region_full <- region_spdf

region_s <- rgeos::gSimplify(region_full,tol=0.01, topologyPreserve=FALSE)

# Create a spatial polygon data frame (includes shp attributes)
regions_spdf = SpatialPolygonsDataFrame(region_s, data.frame(region_full))



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


#load in ccg-stp-region lookup data
regions_lookup = read.csv('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/Clinical_Commissioning_Group_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv')
data_regions = unique(select(regions_lookup, 'STP21CD', 'STP21NM', 'NHSER21NM'))

# Join the reduced DSPT info with the CCG shapefile
ccg_spdf@data <- left_join(ccg_spdf@data,data_merged,by=c("CCG21CD"="CCG21CD"))
data_ccg_spdf <- ccg_spdf@data


#merge and assign to stp_spdf data
stp_spdfdata = stp_spdf@data
stp_spdfdata = merge(stp_spdfdata, select(data_regions, 'STP21CD', 'NHSER21NM'), by = "STP21CD", all = TRUE)
#stp_spdfdata = na.omit(stp_spdfdata)
stp_spdf@data = stp_spdfdata



# Make Status a categorical
#ccg_spdf@data$Short.Status = factor(ccg_spdf@data$Short.Status,dsptlevels)



# Choose a palette
#my_palette <- rev(colorspace::rainbow_hcl(5))
my_palette <- colorspace::divergingx_hcl(5)

catpal <- colorFactor(my_palette, dsptlevels,reverse=F,ordered=T) 

## Plotting
# Prepare the text for tooltips:
mytext <- paste(
  "<b>SUB-ICB 2022 code (ODS): </b>", ccg_spdf@data$ICB22CD,"<br/>",
  "<b>SUB-ICB 2022 name: </b>", ccg_spdf@data$LOC22NM,"<br/>",
  "<b>2021 STP name: </b>", ccg_spdf@data$STP21NM,"<br/>",
  "<b>Region name: </b>", ccg_spdf@data$NHSER22NM,"<br/>",
  "<b>DSPT Status: </b>", ccg_spdf@data$Short.Status, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

mytext_ics <- paste(
  "<b>ICS/STP 2021 code: </b>", stp_spdf@data$STP21CD,"<br/>",
  "<b>ICS/STP 2021 name: </b>", stp_spdf@data$STP21NM,"<br/>",
  "<b>Region name: </b>", stp_spdf@data$NHSER21NM,"<br/>",
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
  addLegend( pal=catpal, values=~Status, opacity=0.9, title = "21/22 DSPT Status (CCG)", position = "bottomleft" )

#m

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
    fillColor = ~catpal(Status), 
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
  #addLegend( data=ccg_spdf,pal=catpal, values=~Short.Status, opacity=0.9, title = "21/22 DSPT Status (CCG)", position = "bottomleft" ) %>%
  leaflet::addLayersControl(
    overlayGroups = c("CCG","ICS boundary"),  # add these layers
    options = layersControlOptions(collapsed = FALSE)  # expand on hover?
  ) %>% 
  hideGroup(c("ICS boundary"))  # turn these off by default



m02_l <- m02 %>% addLegend( data=ccg_spdf,pal=catpal, values=~Short.Status, opacity=0.9, title = "21/22 DSPT Status (CCG)", position = "bottomleft" )



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
  
#m03

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
saveWidget(m04, "ICS_composite_map1_21_22.html")




#############################################
# Summary metric
#############################################
#get the stp codes for trusts from etr
etr = read.csv('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/etr.csv', header = FALSE)
etr = etr[,c('V1', 'V4')]
etr = select(etr, 'ODS.Code' = 'V1', 'STP21CDH' = 'V4')

#get the final 4 stp codes of the 4 care trusts from ect (modified to change column names and select relevant columns)
ect = read.csv('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/ect_modified_21_22.csv')
ect = select(ect, 'ODS.Code', 'STP21CDH')

etr_ect = rbind(etr, ect)


trusts_data2 = trusts_data
trusts_data2$Sector = 'Trust'

trusts_data_merged = left_join(trusts_data2, etr_ect)
#trusts_data_merged = na.omit(trusts_data_merged)
trusts_data_merged = select(trusts_data_merged, c('ODS.Code', 'ODS.Org.Name', 'Status', 'Sector', 'STP21CDH'))


data_ccg = data_merged
stp_codes = select(data_ccg, 'STP21CD' = 'STP21CD', 'STP21CDH' = 'STP21CDH', 'STP21NM' = 'STP21NM')
stp_codes = unique(stp_codes)
trusts_data_merged = left_join(trusts_data_merged, stp_codes)


#load in ccg-stp-region lookup data
regions_lookup = read.csv('/Users/muhammad-faaiz.shanawas/Documents/GitHub/open-cyber/data/Clinical_Commissioning_Group_to_STP_and_NHS_England_(Region)_(April_2021)_Lookup_in_England.csv')
data_regions = unique(select(regions_lookup, 'STP21CD', 'STP21NM', 'NHSER21NM'))

trusts_data_merged = left_join(trusts_data_merged, data_regions)


data_ccg$Sector = 'CCG'
data_ccg = select(data_ccg, 'ODS.Code' = 'ODS.Code', 'ODS.Org.Name' = 'LOC22NM', 'Status' = 'Status', 'Sector' = 'Sector', 'STP21CD' = 'STP21CD', 'STP21CDH' = 'STP21CDH', 'STP21NM' = 'STP21NM', 'NHSER21NM' = 'NHSER22NM')

data_joint <- rbind(data_ccg, trusts_data_merged)
data_metric <- data_joint %>% filter(Sector %in% c("Trust","CCG"))

# GP practice population
# https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/september-2022
gppopdata <- read_csv("https://files.digital.nhs.uk/62/79B56F/gp-reg-pat-prac-sing-age-regions.csv")

gppopdata_red <- gppopdata %>% filter(SEX=="ALL",AGE=="ALL",ORG_TYPE=="SUB_ICB_LOCATION") %>% select(c("ORG_CODE","NUMBER_OF_PATIENTS"))




# score mapping

data_metric <- data_metric %>% mutate(Status.Score=case_when(Status=="21/22 Standards Exceeded"~3,
                                                             Status=="21/22 Standards Met"~1,
                                                             Status=="21/22 Approaching Standards"~-1,
                                                             Status=="21/22 Standards Not Met"~-3,
                                                             Status=="21/22 Not Published"~-3))


NUMBER_OF_PATIENTS = gppopdata['NUMBER_OF_PATIENTS']
data_metric <- data_metric %>% left_join(gppopdata_red,by=c("ODS.Code"="ORG_CODE"))                                                             #TRUE~NA_integer_))

data_metric_ICS <- data_metric %>% group_by(STP21CDH, STP21CD, Sector) %>% summarise(Simple.Score=mean(Status.Score,na.rm=T),
                                                                                  Simple.n=n(),
                                                                                  Pop.Score=sum(NUMBER_OF_PATIENTS*Status.Score)/sum(NUMBER_OF_PATIENTS))

data_metric_ICS <- data_metric_ICS %>% pivot_wider(names_from=Sector,values_from=c("Simple.Score","Simple.n","Pop.Score"))

data_metric_ICS <- data_metric_ICS %>% mutate(metric_CCG_simple = Simple.Score_CCG,
                                              metric_CCG_pop = Pop.Score_CCG,
                                              metric_CCGTrust_simple = 0.5*Simple.Score_CCG+0.5*Simple.Score_Trust,
                                              metric_CCGp_Trusts =0.5*Pop.Score_CCG+0.5*Simple.Score_Trust)






stp_spdf@data <- stp_spdf@data %>% left_join(data_metric_ICS,by=c("STP21CD"="STP21CD"))
stp_data = stp_spdf@data


# Create a continuous palette function
# Create a continuous palette function in our desired range (+3 - 3)

# Create a continuous palette function in our desired range (+3 - 3)
pal_metric <- colorNumeric(
  palette = "RdYlBu",
  domain = range(-3:3))

#create the label text for the first composite map
mytext_ics_score <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$STP21CD,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$STP21NM,"<br/>",
  "<b>ICS score (CCG+Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGp_Trusts,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)



#create the label text to display the stp info for each polygon
mytext_new <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$STP21CD,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$STP21NM,"<br/>",
  "<b>Region: </b>", stp_spdf@data$NHSER21NM,"<br/>",
  "<b>ICS score (CCG population + Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGp_Trusts,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)

#create the map using map panes
#add a layer for the ICS colour coded polygons
#add a layer for empty ICS polygons with just the labels (this has the highest zindex - will be the top layer)
#add a layer for the region boundaries to be displayed in blue
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

##############################################################################################
# Mapping the composite ICS for CCG(population weighted) and Trusts (Weighted for EPRR risk)
##############################################################################################

#create new map labels to contain the dspt metric and stp region info
mytext_new <- paste(
  "<b>STP code (ODS): </b>", stp_spdf@data$stp20cd,"<br/>",
  "<b>STP name (ODS): </b>", stp_spdf@data$stp20nm,"<br/>",
  "<b>Region: </b>", stp_spdf@data$NHSER20NM.y,"<br/>",
  "<b>ICS score (CCG+Trust simple), range [-3,3]: </b>",round(stp_spdf@data$metric_CCGp_Trusts_EPRR,2),"<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


m05 = leaflet() %>%
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

m05



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
data_trust_spdf_pie = left_join(x = stp_spdf@data, y = data_trusts_aggregate, by = "STP21CD")
#data_trust_spdf_pie = merge(x = data_trust_spdf_pie, y = stp_filter_numpatients, by = "STP21CD")
data_trust_spdf_pie[41, 7] = 51.0
data_trust_spdf_pie[41, 6] = 0.0

#create the map with the ICS boundaries displayed in black

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

stp_spdfdata <- stp_spdf@data

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

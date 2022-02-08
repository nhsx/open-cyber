# MPBF
# 16/11/2021
# Adapted from au-cyberdeepdive repo
# Dependencies: main_jan21.R output
#########################################################################################

library(rgdal)
# load ggplot2
library(ggplot2)
library(tidyverse)
library(leaflet)


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
                   radius=6)


m03 <- m03 %>% 
  addLegend( data=trust_spdf_points,pal=catpal, values=~Short.Status, opacity=0.9, title = "20/21 DSPT Status (trust)", position = "bottomright" ) %>%
  leaflet::addLayersControl(
  overlayGroups = c("ICS boundary","CCG","Trusts"),  # add these layers
  options = layersControlOptions(collapsed = FALSE)  # expand on hover?
) %>% 
  hideGroup(c("ICS boundary","Trusts"))  # turn these off by default


m03

# save the widget in a html file if needed.
library(htmlwidgets)
saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choropleth_DSPT_CCG_",Sys.Date(),".html"))


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



data_metric <- data_metric %>% left_join(gppopdata_red,by=c("Code"="ORG_CODE"))                                                             #TRUE~NA_integer_))

data_metric_ICS <- data_metric %>% group_by(STP20CD,STP20NM,Sector) %>% summarise(Simple.Score=mean(Status.Score,na.rm=T),
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

m_icscomp <- leaflet() %>% 
  addTiles()  %>% 
  setView( lat=53, lng=-2 , zoom=6) %>%
  addPolygons(
    data=stp_spdf,
    group="ICS boundary",
    fillOpacity=0.7,
    fillColor=~pal_metric(metric_CCGTrust_simple),
    color="black",
    weight=2,
    label=mytext_ics_score
  ) %>%
  addLegend("bottomright",pal=pal_metric,values=stp_spdf@data$metric_CCGTrust_simple,title="ICS score - CCG/Trust simple")

m_icscomp


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

### Load list of 2020/21 CCGs
# https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-groups-april-2020-names-and-codes-in-england/about

data_ccg <- read.csv("./data/Clinical_Commissioning_Groups_(April_2020)_Names_and_Codes_in_England.csv")

data_ccg <- data_ccg[,c(3,4)]

colnames(data_ccg)=c("Code","Name")

data_ccg$Sector="CCG"


### Load list of CCGs for 2020/21 (cop) - approach 2 ###
#https://digital.nhs.uk/services/organisation-data-service/data-downloads/other-nhs-organisations
# Outcome: 135 CCGs (106 in 21/22, 38 merging codes to 9 new codes - net effect of 29)

temp <- tempfile()
download.file("https://files.digital.nhs.uk/assets/ods/current/eccg.zip",temp)
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name # the following line assuming the archive has only a single file
data_eccg <- read.csv(unz(temp,csv_file[1]),header=FALSE)
unlink(temp)
data_eccg %>% View()

data_eccg_cut <- data_eccg %>% select(c("V1","V2","V10","V11","V12","V14"))


colnames(data_eccg_cut)=c("Code","Name","Postcode","Start","Close","Standard_ind")

data_eccg_cut <- filter(data_eccg_cut,Standard_ind=="C")

data_eccg_cut$Start <- anytime::anydate(data_eccg_cut$Start)
data_eccg_cut$Close <- anytime::anydate(as.character(data_eccg_cut$Close),"%Y%m%d")

data_eccg_cut$Sector="CCG"

data_eccg_cut_2021 <- filter(data_eccg_cut,Start<"2021-04-01",(is.na(Close)|Close>="2021-03-31"))

#data_eccg_cut_2122hyp <- filter(data_eccg_cut,Start<"2022-04-01",(is.na(Close)|Close>="2022-03-31"))
#
#data_eccg_cut_2122newonly <- filter(data_eccg_cut,Start>="2021-04-01",(is.na(Close)|Close>="2022-03-31"))

### Load hierarchy from CCGs to STP in 2020/21 ### # SOP: change to 21/22 once edition closes
# https://geoportal.statistics.gov.uk/datasets/ons::clinical-commissioning-group-to-stps-april-2020-lookup-in-england/about
ccg_hie <- read.csv("./Inputs/Clinical_Commissioning_Group_to_STPs_(April_2020)_Lookup_in_England.csv")
### Load hierarchy from STP to Region in 2020/21 ### # SOP: change to 21/22 once edition closes
# https://geoportal.statistics.gov.uk/datasets/sustainability-transformation-partnerships-and-nhs-england-region-april-2020-lookup-in-england/explore
stp_hie <- read.csv("./Inputs/Sustainability_Transformation_Partnerships_and_NHS_England__Region___April_2020__Lookup_in_England.csv")

ccg_stp_hie <- ccg_hie %>% left_join(stp_hie %>% select(-c("FID")))

data_eccg_enrich_2021 <- data_eccg_cut_2021 %>% left_join(ccg_stp_hie %>% select(-c("FID")),by=c("Code"="CCG20CDH"))

#######
### List of trusts for 2020/21 ###
#https://digital.nhs.uk/services/organisation-data-service/data-downloads/other-nhs-organisations
# Outcome: 114 trusts (113 in 21/22, one merging with existing one, net effect of 1)

temp <- tempfile()
download.file("https://files.digital.nhs.uk/assets/ods/current/etr.zip",temp)
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name # the following line assuming the archive has only a single file
data_etr <- read.csv(unz(temp,csv_file[1]),header=FALSE)
unlink(temp)

data_etr_cut <- data_etr %>% select(c("V1","V2","V3","V4","V10","V11","V12"))

colnames(data_etr_cut)=c("Code","Name","Area","STPCDH","Postcode","Start","Close")


temp <- tempfile()
download.file("https://files.digital.nhs.uk/assets/ods/current/ect.zip",temp)
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name # the following line assuming the archive has only a single file
data_ect <- read.csv(unz(temp,csv_file[1]),header=FALSE)
unlink(temp)

data_ect_cut <- data_ect %>% select(c("V1","V2","V3","V4","V10","V11","V12"))

colnames(data_ect_cut)=c("Code","Name","Area","STPCDH","Postcode","Start","Close")

data_etr_cut <- bind_rows(data_ect_cut,data_etr_cut)

data_etr_cut <- data_etr_cut %>% filter(substr(Area,1,1)!="W") # exclude Wales

#data_etr_cut <- data_etr_cut %>% rename("STPCDH"="Area")
data_etr_cut <- data_etr_cut %>% select(-c("Area"))


data_etr_cut$Start <- anytime::anydate(data_etr_cut$Start)
data_etr_cut$Close <- anytime::anydate(as.character(data_etr_cut$Close),"%Y%m%d")

data_etr_cut$Sector="Trust"

#data_etr_cut <- data_etr_cut %>% mutate(Close_d=as.POSIXct(Close))
#as.POSIXct(data_etr_cut$Close)

data_etr_cut <- filter(data_etr_cut,Start<"2021-04-01",(is.na(Close)|Close>="2021-03-31"))


data_etr_enrich <- data_etr_cut %>% left_join(stp_hie %>% select(-c("FID")),by=c("STPCDH"="STP20CDH"))

library(PostcodesioR)

latfun <- function(geod){
  latitude <- geod$latitude
  return(latitude)
}

longfun <- function(geod){
  longitude <- geod$longitude
  return(longitude)
}


data_etr_enrich_ <- data_etr_enrich %>% mutate(geod = map(Postcode,postcode_lookup))

data_etr_enrich_ <- data_etr_enrich_ %>% mutate(latitude = unlist(map(geod,latfun)),
                                        longitude = unlist(map(geod,longfun)))

data_etr_enrich <- data_etr_enrich_ %>% select(-c("geod"))

######
### List of CSUs for 2020/21 (curated internal) ###
######
# Self-imposed outcome: 5 CSUs
# Alternative not used: eCSU from https://digital.nhs.uk/services/organisation-data-service/data-downloads/health-authorities-and-support-agencies
data_csu <- read.xlsx("./data/2021-curated-csus.xlsx")

View(data_csu)

data_csu <- data_csu[,c(2,3,4)]
colnames(data_csu)=c("Code","Name","Sector")


#### Bind rows for Trusts + CCGs + CSUs ####

data_main <- bind_rows(data_eccg_enrich_2021 %>% select(-"Standard_ind"),data_csu,data_etr_enrich)





#### Load CCG successors (https://digital.nhs.uk/services/organisation-data-service/change-summary-reconfiguration-2021-ccg-mergers)
#data_ccg_succ <- read.xlsx("april-2021-reconfiguration-mapping-file.xlsx")
#View(data_ccg_succ)
#data_ccg_succ <- data_ccg_succ[,c(3,6)]
#data_ccg_succ <- data_ccg_succ %>% rename(`Successor.Code`="New.CCG.Code")

### Join CCG successorts to main info
#data_main <- data_main %>% left_join(data_ccg_succ,by=c("Code"="Legacy.(Closing).CCG.Code"))


#### Load organisational successors (https://digital.nhs.uk/services/organisation-data-service/file-downloads/miscellaneous)
temp <- tempfile()
download.file("https://files.digital.nhs.uk/assets/ods/current/succ.zip",temp)
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name # the following line assuming the archive has only a single file
data_succ <- read.csv(unz(temp,csv_file[1]),header=FALSE)
unlink(temp)
data_succ <- data_succ[,c(1,2)] %>% rename(`Successor.Code`="V2")

### Join trust successorts to main info
data_main <- data_main %>% left_join(data_succ,by=c("Code"="V1"))

### Load DSPT data
#data_dspt <- read.csv("dspt_snapshot.csv")
data_dspt <- read.csv("./data/DSPT search results 08_02_2022 15_17_38.csv") # SOP: update
cutdate='20220208' ## SOP: update
data_dspt <- data_dspt %>% mutate(Code=toupper(Code))

### Join DSPT status to main org and to successor org
data_metric <- left_join(data_main,data_dspt,by=c("Code"))
data_metric <- left_join(data_metric,data_dspt %>% select("Code","Latest.Status") %>% rename("Successor.Status"="Latest.Status"),by=c("Successor.Code"="Code"))

### Enforce final status - its own, or if it doesn't have a 20/21 or 2`/11` status, allows for it to inherit its successors' 21/22 one

editions_allowed=c("20/21","21/22") # SOP: once 21/22 closes, change to "21/22","22/23"
data_metric <- data_metric %>% mutate(Final.Status = ifelse(str_sub(Latest.Status,1,5) %in% editions_allowed,
                                                            Latest.Status,
                                                            Successor.Status),
                                      Final.Status = ifelse(is.na(Final.Status),
                                                            Successor.Status,
                                                            Final.Status))


## simplify status based on editions allowed
data_metric <- data_metric %>% mutate(Short.Status = case_when(Final.Status %in% paste0(editions_allowed," Standards Met")~"Standards Met",
                                                               Final.Status %in% paste0(editions_allowed," Standards Exceeded")~"Standards Exceeded",
                                                               Final.Status %in% paste0(editions_allowed," Standards Not Met")~"Standards Not Met",
                                                               Final.Status %in% paste0(editions_allowed," Approaching Standards")~"Approaching Standards",
                                                               TRUE ~ "Not Published") )



order_levels = c("Not Published","Standards Not Met","Approaching Standards","Standards Met","Standards Exceeded")

data_metric$Short.Status <- factor(data_metric$Short.Status,levels=order_levels)


write.csv(data_metric,paste0("./outputs/data_DSPTmetric_",cutdate,".csv"),na="")

#### Cross table - successors
ct_final = ctable(data_metric$Sector, data_metric$Short.Status,
       prop = "r", chisq = FALSE, headings = FALSE
)

ct_final %>% print(method="browser")
ct_final %>% print(file=paste0("./outputs/data_DSPTmetric",Sys.Date(),".html"))




#### Plotly barchart
# https://plotly.com/r/bar-charts/

library(plotly)

auxl <-data_metric %>% group_by(Sector,Short.Status) %>% summarise(n=n())
aux <- data_metric %>% group_by(Sector,Short.Status) %>% summarise(n=n()) %>% pivot_wider(names_from='Short.Status',values_from='n')
org_type <-aux$Sector



fig_x <- auxl %>% plot_ly(x=~Sector,y= ~n,color=~Short.Status,type='bar')
fig_x

htmlwidgets::saveWidget(fig_x, paste0("./_includes/barchart_summary_FY2021_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")

htmlwidgets::saveWidget(as_widget(fig_x), paste0("./_includes/barchart_summary_FY2021_w_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")

htmltools::save_html(fig_x, paste0("./_includes/barchart_summary_FY2021-ii_",Sys.Date(),".html"))

htmlwidgets::saveWidget(as_widget(fig_x %>% layout(autosize = F, width = 500, height = 500)), paste0("./_includes/barchart_summary_FY2021_fw_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")


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

data_eccg_cut <- data_eccg %>% select(c("V1","V2","V11","V12","V14"))


colnames(data_eccg_cut)=c("Code","Name","Start","Close","Standard_ind")

data_eccg_cut <- filter(data_eccg_cut,Standard_ind=="C")

data_eccg_cut$Start <- anytime::anydate(data_eccg_cut$Start)
data_eccg_cut$Close <- anytime::anydate(as.character(data_eccg_cut$Close),"%Y%m%d")

data_eccg_cut$Sector="CCG"

data_eccg_cut_2021 <- filter(data_eccg_cut,Start<"2021-04-01",(is.na(Close)|Close>="2021-03-31"))

data_eccg_cut_2122hyp <- filter(data_eccg_cut,Start<"2022-04-01",(is.na(Close)|Close>="2022-03-31"))

data_eccg_cut_2122newonly <- filter(data_eccg_cut,Start>="2021-04-01",(is.na(Close)|Close>="2022-03-31"))


### List of trusts for 2020/21 ###
#https://digital.nhs.uk/services/organisation-data-service/data-downloads/other-nhs-organisations
# Outcome: 114 trusts (113 in 21/22, one merging with existing one, net effect of 1)

temp <- tempfile()
download.file("https://files.digital.nhs.uk/assets/ods/current/etr.zip",temp)
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name # the following line assuming the archive has only a single file
data_etr <- read.csv(unz(temp,csv_file[1]),header=FALSE)
unlink(temp)

data_etr_cut <- data_etr %>% select(c("V1","V2","V3","V11","V12"))

colnames(data_etr_cut)=c("Code","Name","Area","Start","Close")

data_etr_cut <- data_etr_cut %>% filter(substr(Area,1,1)!="W") # exclude Wales

data_etr_cut <- data_etr_cut %>% select(-c("Area"))

data_etr_cut$Start <- anytime::anydate(data_etr_cut$Start)
data_etr_cut$Close <- anytime::anydate(as.character(data_etr_cut$Close),"%Y%m%d")

data_etr_cut$Sector="Trust"

#data_etr_cut <- data_etr_cut %>% mutate(Close_d=as.POSIXct(Close))
#as.POSIXct(data_etr_cut$Close)

data_etr_cut <- filter(data_etr_cut,Start<"2021-04-01",(is.na(Close)|Close>="2021-03-31"))


### List of CSUs for 2020/21 (curated internal) ###
# Self-imposed outcome: 5 CSUs
# Alternative not used: eCSU from https://digital.nhs.uk/services/organisation-data-service/data-downloads/health-authorities-and-support-agencies
data_csu <- read.xlsx("./data/2021-curated-csus.xlsx")

View(data_csu)

data_csu <- data_csu[,c(2,3,4)]
colnames(data_csu)=c("Code","Name","Sector")


#### Bind rows for Trusts + CCGs + CSUs ####

data_main <- bind_rows(data_ccg,data_csu,data_etr_cut)


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
data_dspt <- read.csv("./data/DSPT search results 25_10_2021 17_21_46.csv")
data_dspt <- data_dspt %>% mutate(Code=toupper(Code))

### Join DSPT status to main org and to successor org
data_metric <- left_join(data_main,data_dspt,by=c("Code"))
data_metric <- left_join(data_metric,data_dspt %>% select("Code","Latest.Status") %>% rename("Successor.Status"="Latest.Status"),by=c("Successor.Code"="Code"))

### Enforce final status - its own, or if it doesn't have a 21/22 status, allows for it to inherit its successors' 21/22 one

data_metric <- data_metric %>% mutate(Final.Status = ifelse(str_sub(Latest.Status,1,5)=="20/21",
                                                            Latest.Status,
                                                            Successor.Status),
                                      Final.Status = ifelse(is.na(Final.Status),
                                                            Successor.Status,
                                                            Final.Status))

s_levels <- unique(data_metric$Final.Status)

mainlevels = c("20/21 Standards Exceeded","20/21 Standards Met","20/21 Approaching Standards","20/21 Standards Not Met","Not Published")

other <- s_levels[!(mainlevels %in% s_levels)]

order_levels = c(mainlevels,other)

data_metric$Final.Status <- factor(data_metric$Final.Status,levels=order_levels)

#### Cross table - no successors
ctable(data_metric$Sector, data_metric$Latest.Status,
       prop = "r", chisq = FALSE, headings = FALSE
) %>% print(method="browser")


#### Cross table - successors
ct_final = ctable(data_metric$Sector, data_metric$Final.Status,
       prop = "r", chisq = FALSE, headings = FALSE
)

ct_final %>% print(method="browser")
ct_final %>% print(file="./_includes/crosstab_summary_FY2021.html")



#https://digital.nhs.uk/services/organisation-data-service/change-summary-reconfiguration-2021-ccg-mergers

# missing: 06F, 06P and 04F that merged to form M1J4Y on 1 Apr 21 ('Not submitted') but would still be expected to have entries.
# missing: 36L (new in FY20/21) - submitted under 36l so use _toupper(). Its predecessor 07V submitted in DSPT19/20 to Standards Met
# missing: trust RXH that merged into RYR but would still be expected to be listed. If not added, override rating with RYR since new parent has submitted?

### Exploratory - what's happening to the nine new CCGs?

data_metric_new2122 <- data_eccg_cut_2122newonly %>% left_join(data_dspt)
# 1 of the new CCGs published to 21/22. 1 isn't registered. Others 'Not published'

write.csv(data_metric,"./outputs/data_DSPTmetric_20211021.csv")



#### Plotly barchart
# https://plotly.com/r/bar-charts/

library(plotly)

auxl <-data_metric %>% group_by(Sector,Final.Status) %>% summarise(n=n())
aux <- data_metric %>% group_by(Sector,Final.Status) %>% summarise(n=n()) %>% pivot_wider(names_from='Final.Status',values_from='n')
org_type <-aux$Sector

# x1 = aux$`Not Published`
# x2= aux$`20/21 Standards Not Met`
# x3 = aux$`20/21 Approaching Standards`
# x4=aux$`20/21 Standards Met`
# x5=aux$`20/21 Standards Exceeded`
# 
# fig <- plot_ly(data, x = ~org_type, y = ~x1, type = 'bar', name = 'Not Published')
# fig <- fig %>% add_trace(y = ~x2, name = 'Standards Not Met')
# fig <- fig %>% add_trace(y = ~x3, name = 'Approaching Standards')
# fig <- fig %>% add_trace(y = ~x4, name = 'Standards Met')
# fig <- fig %>% add_trace(y = ~x5, name = 'Standards Exceeded')
# fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
# 
# fig


fig_x <- auxl %>% plot_ly(x=~Sector,y= ~n,color=~Final.Status,type='bar')
fig_x

htmlwidgets::saveWidget(fig_x, "./_includes/barchart_summary_FY2021.html", selfcontained = T, libdir = "lib")

htmlwidgets::saveWidget(as_widget(fig_x), "./_includes/barchart_summary_FY2021_w.html", selfcontained = T, libdir = "lib")

htmltools::save_html(fig_x, "./_includes/barchart_summary_FY2021-ii.html")

htmlwidgets::saveWidget(as_widget(fig_x %>% layout(autosize = F, width = 500, height = 500)), "./_includes/barchart_summary_FY2021_fw.html", selfcontained = T, libdir = "lib")


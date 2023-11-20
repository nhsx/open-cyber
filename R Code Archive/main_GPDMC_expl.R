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


### Load list of GP practices - approach EPPRACUR ###
#https://digital.nhs.uk/services/organisation-data-service/data-downloads/other-nhs-organisations

temp <- tempfile()
download.file("https://files.digital.nhs.uk/assets/ods/current/epraccur.zip",temp)
zipdf <- unzip(temp, list = TRUE)
csv_file <- zipdf$Name # the following line assuming the archive has only a single file
data_eccg <- read.csv(unz(temp,csv_file[1]),header=FALSE)
unlink(temp)
data_eccg %>% View()

data_eccg_cut <- data_eccg %>% select(c("V1","V2","V11","V12","V13","V26"))

colnames(data_eccg_cut)=c("Code","Name","Start","Close","Prac_Status","Prescribing_Setting")

#data_eccg_cut <- filter(data_eccg_cut,Standard_ind=="C")

data_eccg_cut$Start <- anytime::anydate(data_eccg_cut$Start)
data_eccg_cut$Close <- anytime::anydate(as.character(data_eccg_cut$Close),"%Y%m%d")

data_eccg_cut <- data_eccg_cut %>%
  filter(substr(Code,1,1)!="W") %>%    # rule: remove Wales practices
  filter(Prescribing_Setting==4) # only keep '4 = GP Practice'

data_eprac_cut_20_21 <- filter(data_eccg_cut,Start<"2021-04-01",(is.na(Close)|Close>="2021-03-31")) %>%
  mutate(validedition="20/21",open_at="2021-03-31") # yields 6804 practices

data_eprac_cut_19_20 <- filter(data_eccg_cut,Start<"2020-04-01",(is.na(Close)|Close>="2020-03-31")) %>%
  mutate(validedition="19/20",open_at="2020-03-31") # yields 7024 practices

data_eprac_cut_18_19 <- filter(data_eccg_cut,Start<"2019-04-01",(is.na(Close)|Close>="2019-03-31")) %>%
  mutate(validedition="18/19",open_at="2019-03-31") # yields 7246 practices

# business rule not enforced: remove 'Dormant' ('D')?

# Though ETL wise EPPRACUR is better, 'analysis'/intelligence/curation list wise seems better in https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice better?

#### Bind rows for DSPT GPDMC denominators among editions ####

data_eprac_editions <- bind_rows(data_eprac_cut_18_19,data_eprac_cut_19_20,data_eprac_cut_20_21)


write.csv(data_eprac_editions,"data_practicelist_editions.csv")



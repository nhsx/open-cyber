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




trusts_dspt = read.csv('./data/DSPT Snapshots/22_23/dspt_trusts_snapshot_16_11_23.csv')
icb_dspt = read.csv('./data/DSPT Snapshots/22_23/ICB_dspt_snapshot_16_11_23.csv')
icb_dspt$Primary.Sector <- 'Integrated Care Board (ICB)' 
csu_dspt = read.csv('./data/DSPT Snapshots/22_23/CSU_dspt_snapshot_16_11_23.csv')

csu_dspt <- csu_dspt %>% select(c(Code, Status, Primary.Sector))
trusts_dspt <- trusts_dspt %>% select(c(Code, Status, Primary.Sector))
icb_dspt <- icb_dspt %>% select(c(Code, Status, Primary.Sector))

data_metric <- rbind(csu_dspt, trusts_dspt, icb_dspt)



#### Cross table - successors
ct_final = ctable(data_metric$Primary.Sector, data_metric$Status,
                  prop = "r", chisq = FALSE, headings = FALSE
)

ct_final %>% print(method="browser")
ct_final %>% print(file=paste0("./outputs/data_DSPTmetric",Sys.Date(),".html"))


#### Plotly barchart
# https://plotly.com/r/bar-charts/

library(plotly)

auxl <-data_metric %>% group_by(Primary.Sector,Status) %>% summarise(n=n())
aux <- data_metric %>% group_by(Primary.Sector,Status) %>% summarise(n=n()) %>% pivot_wider(names_from='Status',values_from='n')
org_type <-aux$Primary.Sector



fig_x <- auxl %>% plot_ly(x=~Primary.Sector,y= ~n,color=~Status,type='bar')
fig_x

htmlwidgets::saveWidget(fig_x, paste0("./_includes/barchart_summary_FY2023_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")

htmlwidgets::saveWidget(as_widget(fig_x), paste0("./_includes/barchart_summary_FY2023_w_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")

htmltools::save_html(fig_x, paste0("./_includes/barchart_summary_FY2023-ii_",Sys.Date(),".html"))


htmlwidgets::saveWidget(as_widget(fig_x , paste0("./_includes/barchart_summary_FY2023_fw_",Sys.Date(),".html")), selfcontained = T, libdir = "lib")


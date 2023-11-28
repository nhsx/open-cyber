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


#load in cuts of dspt data for cat 1 organisations
trusts_dspt = read.csv('./data/DSPT Snapshots/22_23/dspt_trusts_snapshot_16_11_23.csv')
icb_dspt = read.csv('./data/DSPT Snapshots/22_23/ICB_dspt_snapshot_16_11_23.csv')
icb_dspt$Primary.Sector <- 'Integrated Care Board (ICB)' 
csu_dspt = read.csv('./data/DSPT Snapshots/22_23/CSU_dspt_snapshot_16_11_23.csv')

#select relevant column names
csu_dspt <- csu_dspt %>% select(c(Code, Status, Primary.Sector))
trusts_dspt <- trusts_dspt %>% select(c(Code, Status, Primary.Sector))
icb_dspt <- icb_dspt %>% select(c(Code, Status, Primary.Sector))

#concatenate all 3 files
data_metric <- rbind(csu_dspt, trusts_dspt, icb_dspt)

## simplify status based on editions allowed
data_metric <- data_metric %>% mutate(Short.Status = case_when(Status %in% paste0("22/23 Standards Met")~"Standards Met",
                                                               Status %in% paste0("22/23 Standards Exceeded")~"Standards Exceeded",
                                                               Status %in% paste0("22/23 Standards Not Met")~"Standards Not Met",
                                                               Status %in% paste0("22/23 Approaching Standards")~"Approaching Standards",
                                                               TRUE ~ "Not Published") )



order_levels = c("Not Published","Standards Not Met","Approaching Standards","Standards Met","Standards Exceeded")

data_metric$Short.Status <- factor(data_metric$Short.Status,levels=order_levels)


#### Summary Cross Table
ct_final = ctable(data_metric$Primary.Sector, data_metric$Short.Status,
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


htmlwidgets::saveWidget(fig_x, paste0("./outputs/barchart_summary_FY2023_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")

htmlwidgets::saveWidget(as_widget(fig_x), paste0("./outputs/barchart_summary_FY2023_w_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")

htmltools::save_html(fig_x, paste0("./outputs/barchart_summary_FY2023-ii_",Sys.Date(),".html"))


htmlwidgets::saveWidget(as_widget(fig_x), paste0("./outputs/barchart_summary_FY2023_fw_",Sys.Date(),".html"), selfcontained = T, libdir = "lib")


# MPBF
# 12/11/2021
# Adapted from au-cyberdeepdive repo
# Dependencies: main_oct21.R output
#########################################################################################


library(ggplot2)
library(ggalluvial) # Sankey method 2
library(colorspace)
library(tidyverse)
library(readxl)
library(openxlsx)
library(summarytools)


### Trusts ####

# ### Load directory on trusts # previous approach. Here not needed as end-year (31Mar21) already fixed in main_oct21.R file
# trusts_raw <- read_xlsx("./Data sources/organisations_apr21_cut.xlsx",sheet="query_noheader",range="A1:S541")
# trusts_raw$Effective_From <- as.Date(trusts_raw$Effective_From)
# trusts_raw <- trusts_raw %>% mutate(Effective_To=ifelse(Effective_To=="NULL",NA,Effective_To),
#                                     Effective_To=as.numeric(Effective_To))
# trusts_raw <- trusts_raw %>% mutate(Effective_To=as.Date(Effective_To,origin = "1899-12-30"))
# trusts_1920 <- 
#   trusts_raw %>%
#   filter(Effective_From<"2020-04-01") %>%
#   filter(is.na(Effective_To) | Effective_To>="2020-03-31") # end19/20: 222 Trusts. Note: includes three welsh trusts. Includes acute, comm, MH


### Load auxiliary file of trusts involved in mergers in 2019/20 (whether child or parent)
# to simplify. For more end-to-end could also use reference 'successor file'
trusts_merger <- read.xlsx("./Data sources/trusts_merger_1920.xlsx")

### Load DSPT position shortly after 19/20 closure (closure end September 2020)
dspt_1920close <- read.csv("./data/DSPT search results 23_10_2020 15_39_05.csv")
dspt_1920close <- dspt_1920close %>% rename("Status19/20"="Latest.Status")

### Load DSPT position in 20/21 (after having been pre-processed in main_oct21.R to already reflect latest trusts)
dspt_2021assim <- read.csv("./outputs/data_DSPTmetric_20211021.csv")
dspt_2021assim <- dspt_2021assim %>% rename("Status20/21"="Latest.Status")

### Load DSPT position  after 18/19 (published june 2019)
dspt_1819 <- read_xlsx("./data/DSPT Progress as of 17_06_2019 15_25_54 publish.xlsx")
dspt_1819close <- dspt_1819 %>% rename("Status18/19"="PublicationStatus")

### Create the object with old and new position for 20/21 trusts (non-mergees).
# Only status of existing trusts is considered (maintained ODS code). Current pipeline does not map pre-merger trust statuses into their post-merger equivalent

dsptt_y2y3 <- left_join(dspt_2021assim %>% filter(Sector=="Trust") %>% select(Code,Name,`Status20/21`),
                        dspt_1920close %>% select(Code,`Status19/20`) )

dsptt_y2y3 <- left_join(dsptt_y2y3,
                        dspt_1819close %>% select(ODSCode,`Status18/19`),by=c("Code"="ODSCode") )

dsptt_y2y3 <- dsptt_y2y3 %>% mutate(`Status19/20`=ifelse(is.na(`Status19/20`),'19/20 None',`Status19/20`),
                                    `Status18/19`=ifelse(is.na(`Status18/19`),'18/19 None',`Status18/19`)) # if N/A, assign none (likely new trust, but could refine)


## Change factor levels
dsptt_y2y3$`Status19/20` <- factor(dsptt_y2y3$`Status19/20`,levels=c("19/20 Standards Exceeded","19/20 Standards Met","19/20 Standards Not Fully Met (Plan Agreed)","19/20 Standards Not Met","19/20 None"))
dsptt_y2y3$`Status20/21` <- factor(dsptt_y2y3$`Status20/21`,levels=c("20/21 Standards Exceeded","20/21 Standards Met","20/21 Approaching Standards","20/21 Standards Not Met","Not published"))
dsptt_y2y3$`Status18/19` <- factor(dsptt_y2y3$`Status18/19`,levels=c("Standards Exceeded","Standards Met","Standards not fully met (plan agreed)","Standards Not Met","18/19 None"))


# create static summary table of 19/20 > 20/21 changes between statuses.
table_forplotting <- dsptt_y2y3 %>% group_by(`Status19/20`,`Status20/21`) %>% summarise(freq=n())

colorspace::diverge_hsv(4)[1]


activdata_atb_lodes <- to_lodes_form(as.data.frame(table_forplotting),
                                     axes = 1:2,
                                     id = "Cohort")
activdata_atb_lodes$stratum <- factor(activdata_atb_lodes$stratum,levels=(unique(activdata_atb_lodes$stratum)))


is_lodes_form(activdata_atb_lodes, key = x, value = stratum, id = Cohort, silent = TRUE)

#https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
n=4
my_palette <- rev(rainbow_hcl(4))
cols <- c("19/20 Standards Exceeded" = my_palette[1],
          "19/20 Standards Met" =  my_palette[2],
          "19/20 Standards Not Fully Met (Plan Agreed)" =  my_palette[3],
          "19/20 Standards Not Met" =  my_palette[4],
          "20/21 Standards Exceeded" ="gray",
          "20/21 Standards Met"="gray",
          "20/21 Approaching Standards"="gray",
          "20/21 Standards Not Met" = "gray",
          "Not published"="gray")


ggplot(activdata_atb_lodes,
       aes(x = x, stratum = stratum, alluvium = Cohort,
           y = freq,
           fill = stratum, label = stratum)) +
  scale_fill_manual(values = cols)+
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = 0.3,fill="White") +
  geom_text(stat = "stratum", size = 3,angle=0) +
  labs(title = "DSPT Status",
       subtitle = "Status year to year - Trusts as of 31 March 2021",
       y = "Frequency") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave(paste0(here::here("outputs"),"/sankey_trusts_y2y3.svg"),width=30,height=20,dpi=300,units="cm")
ggsave(paste0(here::here("outputs"),"/sankey_trusts_y2y3.png"),width=30,height=20,dpi=300,units="cm")



# create static summary table of 18/19 > 19/20 > 20/21 changes between statuses.
table_forplotting <- dsptt_y2y3 %>% group_by(`Status18/19`,`Status19/20`,`Status20/21`) %>% summarise(freq=n())


activdata_atb_lodes <- to_lodes_form(as.data.frame(table_forplotting),
                                     axes = 1:3,
                                     id = "Cohort")
#activdata_atb_lodes$stratum <- factor(activdata_atb_lodes$stratum,levels=(unique(activdata_atb_lodes$stratum)))


is_lodes_form(activdata_atb_lodes, key = x, value = stratum, id = Cohort, silent = TRUE)

#https://www.nceas.ucsb.edu/sites/default/files/2020-04/colorPaletteCheatsheet.pdf
n=4
my_palette <- rev(rainbow_hcl(4))
cols <- c("Standards Exceeded"=my_palette[1],
          "Standards Met"=my_palette[2],
          "Standards not fully met (plan agreed)"=my_palette[3],
          "Standards Not Met"=my_palette[4],
          "18/19 None"="black",
          "19/20 Standards Exceeded" = my_palette[1],
          "19/20 Standards Met" =  my_palette[2],
          "19/20 Standards Not Fully Met (Plan Agreed)" =  my_palette[3],
          "19/20 Standards Not Met" =  my_palette[4],
          "19/20 None"="black",
          "20/21 Standards Exceeded" ="gray",
          "20/21 Standards Met"="gray",
          "20/21 Approaching Standards"="gray",
          "20/21 Standards Not Met" = "gray",
          "Not published"="gray")


ggplot(activdata_atb_lodes,
       aes(x = x, stratum = stratum, alluvium = Cohort,
           y = freq,
           fill = stratum, label = stratum)) +
  scale_fill_manual(values = cols)+
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = 0.3,fill="White") +
  geom_text(stat = "stratum", size = 3,angle=0) +
  labs(title = "DSPT Status",
       subtitle = "Status year to year - Trusts as of 31 March 2021",
       y = "Frequency") +
  theme_minimal()+
  theme(legend.position = "none")

ggsave(paste0(here::here("outputs"),"/sankey_trusts_y1y2y3.svg"),width=30,height=20,dpi=300,units="cm")
ggsave(paste0(here::here("outputs"),"/sankey_trusts_y1y2y3.png"),width=30,height=20,dpi=300,units="cm")


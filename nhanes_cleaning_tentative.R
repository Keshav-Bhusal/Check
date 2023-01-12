rm(list = ls())

# PACKAGES

# From CRAN

library(ggplot2)           # graphing
library(scales)            # graphing aids 
library(dplyr)             # data manipulation
library(tidyr)             # data manipulation
library(purrr)             # data manipulation
library(haven)
library(lubridate)         # better date formatting 
library(stringr)           # extracting information from character variable
library(RSQLite)           # Database stuff
library(XML)
library(RCurl)
library(reshape2)


# Courtemanche replication (ROUGH CODE - in progress)

nhanes_demo  <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.XPT")
nhanes_bmi   <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BMX_E.XPT")
nhanes_hist  <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/WHQ_E.XPT")

nhanes <- list(nhanes_demo, nhanes_bmi, nhanes_hist) %>%
  purrr::reduce(., full_join, by = "SEQN") %>%
  select(SEQN, RIAGENDR, RIDAGEYR, RIDRETH1, WHD010, WHD020, BMXWT, BMXHT, WTMEC2YR) %>%
  filter(RIDAGEYR > 18, RIDAGEYR < 65, WHD010 != 9999, WHD010 != 7777, WHD020 != 9999, WHD020 != 7777) %>%
  drop_na() %>%
  mutate(HEIGHTINCH = BMXHT / 2.54,
         WEIGHTPOUN = BMXWT * 2.205)


# con <- dbConnect(SQLite(), "C:/Users/Fabiana/Box/AREC MS Thesis Fabiana/data/ATUSraw.db") # Connection
# 
# EATRESPI  <- dbGetQuery(con, "SELECT TUCASEID, ERBMI, EUHGT, EUWGT FROM eatrespI" ) 
# ROST  <- dbGetQuery(con, "SELECT TUCASEID, TULINENO, TESEX, TEAGE from rost") %>%
#   filter(TULINENO == 1) %>%
#   select(-TULINENO)
# CPS  <- dbGetQuery(con, "SELECT TUCASEID, TULINENO, PTDTRACE from cps") %>%
#   filter(TULINENO == 1) %>%
#   select(-TULINENO)
# 
# atus <- list(EATRESPI, ROST, CPS) %>%
#   purrr::reduce(., left_join, by ="TUCASEID") %>%
#   filter(TUCASEID > 20069999999999) %>%
#   filter(TEAGE > 18, TEAGE < 65)

# write_dta(nhanes, "C:/Users/Fabiana/Desktop/nhanes.dta")


## Random checks on the most recent data available


# Download pre-pandemic (post pandemic not yet available)
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_BMX.htm 

nhanes_demo <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DEMO.XPT")
nhanes_diet1 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DR1IFF.XPT") # disaggregated (nutrients)
nhanes_diet2 <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_DR1TOT.XPT") # aggregated (nutrients)
nhanes_bmi  <- read_xpt("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/P_BMX.XPT")

# Missing BMI? 1163
nhanes_bmi_miss <- nhanes %>% 
  filter(is.na(BMXBMI))

# Merge demo and BMI data
nhanes <- list(nhanes_demo, nhanes_bmi) %>%
  purrr::reduce(., full_join, by = "SEQN") 

# BMI info
BMI <- ggplot(nhanes, aes(x = BMXBMI, fill = as.factor(RIAGENDR), color = as.factor(RIAGENDR))) +
  geom_histogram(position="identity", bins=30, alpha=0.5) +
  theme_bw(base_size=14)
BMI

mean_bmi <- nhanes %>%
  drop_na(BMXBMI) %>%  
  group_by(RIAGENDR) %>%
  summarize(mean = mean(BMXBMI, na.rm = T),
            count = n())
head(mean_bmi)


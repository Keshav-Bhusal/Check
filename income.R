#Setting the working environment
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(zoo)
library(scales)

#Loading the Data
brfss_data <- load("~/Desktop/AREC/RA Works/Raw Data/BRFSS_data.RData")

#Cleaning the Data
brfss_income <- brfss_data %>% 
  select(IYEAR, IMONTH, '_INCOMG', '_INCOMG1') %>% 
  rename(INCOME1 = '_INCOMG',
         INCOME2 = '_INCOMG1') %>% 
  mutate(income = case_when(INCOME1 == 1 ~ 1,
                            INCOME1 == 2 ~ 2,
                            INCOME1 == 3 ~ 3,
                            INCOME1 == 4 ~ 4,
                            INCOME1 == 5 ~ 5,
                            INCOME1 == 9 ~ 9,
                            INCOME2 == 1 ~ 1,
                            INCOME2 == 2 ~ 2,
                            INCOME2 == 3 ~ 3,
                            INCOME2 == 4 ~ 4,
                            INCOME2 == 5 ~ 5,
                            INCOME2 == 6 ~ 5,
                            INCOME2 == 7 ~ 5,
                            INCOME2 == 9 ~ 9))

#Grouping the data (Categorization)
brfss_income_gp <- brfss_income %>%   
  group_by(income) %>% 
  summarise(N = n()) 
  mutate(percentage= N/sum(N)*100)
  
write.csv(brfss_income_gp, "income_size.csv", row.names = F)

#Dropping the extra datas
brfss_income_gp <- brfss_income_gp[!(brfss_income_gp$IYEAR== 2022 | brfss_income_gp$income == 9),]


#Distribution of the Income
ggplot(brfss_income_gp, aes(x = IYEAR, y = N, color = income, group = income)) + 
  geom_point()+
  geom_line()+
  scale_linetype_binned()+
  scale_y_continuous(breaks=pretty_breaks(n=10))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1.0, size = 12))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  labs(title = "Income Distribution of the Household", subtitle = "(Categorical distribution)", x="Years", y="Numbers of individuals in the category")

ggsave("desktop/AREC/RA Works/income.png", width = 35, height = 15, units = "cm")

#monthly distribution of Income
mnth <- ggplot(brfss_income_gp, aes(x = IYEAR, y = N, color = income, 
                            group = income)) + 
  geom_point() +
  geom_line() +
  scale_linetype_binned()+
  scale_y_continuous(breaks=pretty_breaks(n=10))+
  scale_x_date(date_labels = "%m-%Y", breaks = "4 month", expand = c(0,0)) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust = 1.0))+
  labs(title = "Income distribution of the household", subtitle = "(Categorical distribution)", x="Years", y="Numbers of individuals in the category")






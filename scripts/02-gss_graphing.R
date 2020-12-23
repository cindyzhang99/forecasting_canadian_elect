#### Preamble ####
# Purpose: The purpose of this code is to select variables of interest from the 
# cleaned version of the 2017 GSS dataset. We will drop rows with missing data 
# and match factors to those in the CES dataset. You will end up with a CSV saved 
# at inputs/gss/gss_input.csv.
# Finally, we produce the graphs seen in the Data section of our paper.

# Author: Xinyi Zhang
# Contact: xinyicindy.zhang@mail.utoronto.ca
# Date: 14 December 2020
# License: MIT
# Pre-reqs: You need to have run gss_cleaning/gss_cleaning.R (written by Professors
# Rohan Alexander and Sam Caetano). You need to have saved a cleaned 
# version of the 2017 GSS dataset to inputs/gss/gss_2017_cleaned.csv. 

library(tidyverse)

# picking up where the 2017 GSS cleaning script left off
gss <- read_csv("inputs/gss/gss_2017_cleaned.csv")

# selecting variables of interest
gss_abridged <- gss %>% 
  select(age, 
         sex, 
         education, 
         province, 
         religion_importance)

# select only rows with values for every variable
gss_abridged <- gss_abridged[complete.cases(gss_abridged), ]

# match GSS data to CES data
# drop data for individuals younger than 18
gss_abridged <- gss_abridged[!(gss_abridged$age<18),]
# partition age into same bins as GSS
gss_abridged$age = cut(gss_abridged$age,c(17, 29, 41, 53, 65, 77, 100))
levels(gss_abridged$age) = c('18 to 29', '30 to 41', '42 to 53', '54 to 65', '66 to 77', '78 and above')

# match education data in GSS to education data in CES
# drop "NA" answers 
gss_abridged <- gss_abridged[!is.na(gss_abridged$education),]
# match values in name to those in CES
gss_abridged$education <- plyr::mapvalues(gss_abridged$education, 
                                          from=c("Less than high school diploma or its equivalent",
                                                 "High school diploma or a high school equivalency certificate",
                                                 "Trade certificate or diploma",
                                                 "College, CEGEP or other non-university certificate or di...",
                                                 "University certificate or diploma below the bachelor's level",
                                                 "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)",
                                                 "University certificate, diploma or degree above the bach..."), 
                                          to=c("High school or less",
                                               "High school or less", 
                                               "Some college or trade school", 
                                               "Some college or trade school",
                                               "Some college or trade school",
                                               "Undergraduate degree",
                                               "Some graduate school or more"))

# save cleaned data ready to be used in MRP
write_csv(gss_abridged, "inputs/gss/gss_input.csv")

################################################################################
# plot variables of interest
gss <- read_csv("inputs/gss/gss_input.csv")

# find percentage of each age range
age_distribution <- gss %>% dplyr::count(age) %>% mutate(Percent = n/nrow(gss))
# plot distribution as bar graph
ggplot(data=age_distribution, aes(x=age, y=Percent, fill=Percent)) +
  geom_bar(stat="identity") +
  labs(x="Age", y="Percent", title = "Distribution of Age in GSS") +
  geom_text(aes(label=paste0(round(Percent*100), "%")), position=position_dodge(width=0.9), vjust=-0.25)

# find percentage of each gender
gender_distribution <- gss %>% count(sex) %>% mutate(percent = n/nrow(gss))
# plot distribution as pie chart
ggplot(gender_distribution, aes(x="", y=percent, fill=sex)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#f7b6d2", "#9edae5")) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Gender in GSS") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

# find percentage of each province
province_distribution <- gss %>% dplyr::count(province) %>% mutate(Percent = n/nrow(gss))
# plot distribution as bar graph
ggplot(data=province_distribution, aes(x=province, y=Percent, fill=Percent)) +
  geom_bar(stat="identity") +
  labs(x="Province", y="Percent", title = "Distribution of Provinces in GSS") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_text(aes(label=paste0(round(Percent*100), "%")), position=position_dodge(width=0.9), vjust=-0.25)

# find percentage of each education level
education_distribution <- gss %>% dplyr::count(education) %>% mutate(percent = n/nrow(gss))
# reorder levels
education_distribution$education <- factor(education_distribution$education, 
                                           levels=c("High school or less",
                                                    "Some college or trade school",
                                                    "Undergraduate degree",
                                                    "Some graduate school or more"))
# plot distribution as pie chart
ggplot(education_distribution, aes(x="", y=percent, fill=education)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#b1d8b7", "#76b947", "#54902a", "#2f5233")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Education in GSS") + 
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

# find percentage of each religion level
religion_distribution <- gss %>% dplyr::count(religion_importance) %>% mutate(percent = n/nrow(gss))
# plot distribution as pie chart
ggplot(religion_distribution, aes(x="", y=percent, fill=religion_importance)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#b1d8b7", "#94c973", "#76b947", "#54902a", "#2f5233")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Religious Importance in GSS") + 
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))
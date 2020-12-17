#### Preamble ####
# Purpose: The purpose of this code is to clean up the 2019 online CES data 
# obtained through the hodgettsp/cesR git repository. We select variables of interest,
# drop rows with missing data and match factors to those in the GSS dataset. 
# You will end up with a CSV saved at inputs/ces/ces_input.csv.
# Finally, we produce the graphs seen in the Data section of our paper.

# Author: Xinyi Zhang
# Contact: xinyicindy.zhang@mail.utoronto.ca
# Date: 14 December 2020
# License: MIT
# Pre-reqs: You need to have installed the cesR library. 
# To do that uncomment and run the lines below:
# install.packages("devtools")
# devtools::install_github("hodgettsp/cesR")

library(cesR)
library(labelled)
library(tidyverse)
################################################################################
# get CES 2019 web data
get_ces("ces2019_web")
ces2019_web <- to_factor(ces2019_web)
# select variables of interest
ces_abridged <- ces2019_web %>% 
  select(cps19_yob, 
         cps19_gender, 
         cps19_education, 
         cps19_province, 
         cps19_rel_imp,
         cps19_votechoice,
         cps19_vote_unlikely,
         cps19_vote_lean,
         cps19_v_advance)

# combine vote choice columns
ces_abridged$cps19_vote <- coalesce(ces_abridged$cps19_votechoice, ces_abridged$cps19_vote_unlikely)
ces_abridged$cps19_vote <- coalesce(ces_abridged$cps19_vote, ces_abridged$cps19_vote_lean)
ces_abridged$cps19_vote <- coalesce(ces_abridged$cps19_vote, ces_abridged$cps19_v_advance)

# reselect variables of interest
ces_abridged <- ces_abridged %>% 
  select(cps19_yob, 
         cps19_gender, 
         cps19_education, 
         cps19_province, 
         cps19_rel_imp,
         cps19_vote)

# select only rows with values for every variable
ces_abridged <- ces_abridged[complete.cases(ces_abridged), ]

write_csv(ces_abridged, "inputs/ces/ces_abridged.csv")
################################################################################
# match CES data to GSS data

ces <- read_csv("inputs/ces/ces_abridged.csv")

# change year of birth to age
ces$age <- 2019 - ces$cps19_yob
# partition age into same bins as GSS
ces$age <- cut(ces$age,c(17, 29, 41, 53, 65, 77, 100))
levels(ces$age) = c('18 to 29', '30 to 41', '42 to 53', '54 to 65', '66 to 77', '78 and above')

# match gender data in CES to sex data available in GSS
# drop "Other" level
ces<-ces[!(ces$cps19_gender=="Other (e.g. Trans, non-binary, two-spirit, gender-queer)"),]
# match values to those in GSS
ces$cps19_gender <- plyr::mapvalues(ces$cps19_gender, 
                                    from=c("A man","A woman"), 
                                    to=c("Male","Female"))

# match province data in CES to province data available in GSS
# drop territories
ces<-ces[!(ces$cps19_province=="Northwest Territories" | ces$cps19_province=="Yukon" | ces$cps19_province=="Nunavut"),]

# match education data in CES to education data available in GSS
# drop "Don't know" answers
ces<-ces[!(ces$cps19_education=="Don't know/ Prefer not to answer"),]
# match values in name to those in GSS
ces$cps19_education <- plyr::mapvalues(ces$cps19_education, 
                                       from=c("No schooling",
                                              "Some elementary school",
                                              "Completed elementary school",
                                              "Some secondary/ high school",
                                              "Completed secondary/ high school",
                                              "Some technical, community college, CEGEP, College Classique",
                                              "Completed technical, community college, CEGEP, College Classique",
                                              "Some university",
                                              "Bachelor's degree",
                                              "Master's degree",
                                              "Professional degree or doctorate"), 
                                       to=c("High school or less",
                                            "High school or less", 
                                            "High school or less", 
                                            "High school or less", 
                                            "High school or less", 
                                            "Some college or trade school", 
                                            "Some college or trade school",
                                            "Some college or trade school",
                                            "Undergraduate degree",
                                            "Some graduate school or more",
                                            "Some graduate school or more"))

# match religious importance values to those in GSS
ces$cps19_rel_imp <- plyr::mapvalues(ces$cps19_rel_imp, 
                                     from=c("Don't know/ Prefer not to answer","Not important at all", "Not very important", "Somewhat important", "Very important"), 
                                     to=c("Don't know","Not at all important", "Not very important", "Somewhat important", "Very important"))

# clean voting data
# drop non-choices
ces<-ces[!(ces$cps19_vote=="Don't know/ Prefer not to answer" | ces$cps19_vote=="Another party (please specify)" | ces$cps19_vote=="I do not intend to vote"),]
# format levels
ces$cps19_vote <- plyr::mapvalues(ces$cps19_vote, 
                                  from=c("ndp",
                                         "Green Party",
                                         "Liberal Party",
                                         "Conservative Party",
                                         "People's Party",
                                         "Bloc Québécois"), 
                                  to=c("New Democratic Party",
                                       "Green Party",
                                       "Liberal Party",
                                       "Conservative Party",
                                       "People's Party",
                                       "Bloc Québécois"))

# reselect variables of interest
ces <- ces %>% 
  select(age, 
         cps19_gender, 
         cps19_province, 
         cps19_education, 
         cps19_rel_imp,
         cps19_vote)

# save cleaned data ready to be modeled
write_csv(ces, "inputs/ces/ces_input.csv")


################################################################################
# plot variables of interest

ces <- read_csv("inputs/ces/ces_input.csv")

# find percentage of each age range
age_distribution <- ces %>% dplyr::count(age) %>% mutate(Percent = n/nrow(ces))
# plot distribution as bar graph
ggplot(data=age_distribution, aes(x=age, y=Percent, fill=Percent)) +
  geom_bar(stat="identity") +
  labs(x="Age", y="Percent", title = "Distribution of Age in CES") +
  geom_text(aes(label=paste0(round(Percent*100), "%")), position=position_dodge(width=0.9), vjust=-0.25)

# find percentage of each gender
gender_distribution <- ces %>% dplyr::count(cps19_gender) %>% mutate(percent = n/nrow(ces))
# plot distribution as pie chart
ggplot(gender_distribution, aes(x="", y=percent, fill=cps19_gender)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#f7b6d2", "#9edae5")) + 
  labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Gender in CES") +
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

# find percentage of each province
province_distribution <- ces %>% dplyr::count(cps19_province) %>% mutate(Percent = n/nrow(ces))
# plot distribution as bar graph
ggplot(data=province_distribution, aes(x=cps19_province, y=Percent, fill=Percent)) +
  geom_bar(stat="identity") +
  labs(x="Province", y="Percent", title = "Distribution of Provinces in CES") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_text(aes(label=paste0(round(Percent*100), "%")), position=position_dodge(width=0.9), vjust=-0.25)

# find percentage of each education level
education_distribution <- ces %>% dplyr::count(cps19_education) %>% mutate(percent = n/nrow(ces))
# reorder levels
education_distribution$cps19_education <- factor(education_distribution$cps19_education, 
                                                 levels=c("High school or less",
                                                          "Some college or trade school",
                                                          "Undergraduate degree",
                                                          "Some graduate school or more"))
# plot distribution as pie chart
ggplot(education_distribution, aes(x="", y=percent, fill=cps19_education)) + geom_bar(stat="identity", width=1) + 
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#b1d8b7", "#76b947", "#54902a", "#2f5233")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Education in CES") + 
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

# find percentage of each religion level
religion_distribution <- ces %>% dplyr::count(cps19_rel_imp) %>% mutate(percent = n/nrow(ces))
# plot distribution as pie chart
ggplot(religion_distribution, aes(x="", y=percent, fill=cps19_rel_imp)) + geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + geom_text(aes(label = paste0(round(percent*100), "%")), position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values=c("#b1d8b7", "#94c973", "#76b947", "#54902a", "#2f5233")) +
  labs(x = NULL, y = NULL, fill = NULL, title = "Distribution of Religious Importance in CES") + 
  theme_classic() + theme(axis.line = element_blank(),
                          axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          plot.title = element_text(hjust = 0.5, color = "#666666"))

# find percentage of each vote
vote_distribution <- ces %>% dplyr::count(cps19_vote) %>% mutate(Percent = n/nrow(ces))
# reorder levels
vote_distribution$cps19_vote <- factor(vote_distribution$cps19_vote, 
                                       levels=c("New Democratic Party",
                                                "Green Party",
                                                "Liberal Party",
                                                "Conservative Party",
                                                "People's Party",
                                                "Bloc Québécois"))
# plot distribution as bar graph
ggplot(data=vote_distribution, aes(x=cps19_vote, y=Percent, fill=Percent)) +
  geom_bar(stat="identity") +
  labs(x="Province", y="Percent", title = "Distribution of Votes in CES") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_text(aes(label=paste0(round(Percent*100), "%")), position=position_dodge(width=0.9), vjust=-0.25)
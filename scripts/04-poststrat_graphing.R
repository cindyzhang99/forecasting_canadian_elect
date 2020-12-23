#### Preamble ####
# Purpose: The purpose of this code is to produce the graphs seen in the Results
# section of our paper. 

# Author: Xinyi Zhang
# Contact: xinyicindy.zhang@mail.utoronto.ca
# Date: 22 December 2020
# License: MIT

library(tidyverse)

# load original cleaned ces data
ces <- read_csv("inputs/ces/ces_input.csv")
# load post-stratification data
poststrat <- read_csv("outputs/post_strat.csv")

################################################################################
# find percentage of each vote in ces data
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

# find percentage of each vote in poststrat data
vote_count <- poststrat %>% count(vote, wt=freq) %>% mutate(Proportion=n/sum(n))

# plot distribution as bar graph
ggplot(data=vote_count, aes(x=vote, y=Proportion, fill=Proportion)) +
  geom_bar(stat="identity") +
  labs(x="Political Party", y="Proportion") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_text(aes(label=paste0(round(Proportion*100), "%")), position=position_dodge(width=0.9), vjust=-0.25)

################################################################################
# set up custom color scheme where blue indicates the more liberal parties and red indicates the more conservative parties
# match colors across ces and post-strat
colors <- c("Green Party" = "#2166AC", "New Democratic Party" = "#4393C3", "Liberal Party" = "#92C5DE",
            "Conservative Party" = "#F4A582", "People's Party" = "#D6604D", "Bloc Québécois" = "#B2182B")

# find percentage of each vote by age groups in ces
age_distribution_ces <- count(ces, age, cps19_vote) %>% group_by(age) %>% mutate(Percent = n/sum(n))
# reorder levels
age_distribution_ces$cps19_vote <- factor(age_distribution_ces$cps19_vote, 
                                       levels=c("Green Party",
                                                "New Democratic Party",
                                                "Liberal Party",
                                                "Conservative Party",
                                                "People's Party",
                                                "Bloc Québécois"))
# plot distribution with respect to age as line graph
ggplot(age_distribution_ces, aes(x=age, y=Percent, group=cps19_vote, color=cps19_vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Age", y="Vote Proportion", color="Political Party") + 
  scale_color_manual(values=colors)

# find percentage of each vote by age groups in poststrat
age_distribution_poststrat <- aggregate(freq ~ age + vote, data=poststrat, sum) %>% group_by(age) %>% mutate(Percent = freq/sum(freq))
# reorder levels
age_distribution_ces$cps19_vote <- factor(age_distribution_ces$cps19_vote, 
                                          levels=c("Green Party",
                                                   "New Democratic Party",
                                                   "Liberal Party",
                                                   "Conservative Party",
                                                   "People's Party",
                                                   "Bloc Québécois"))
# plot distribution with respect to age as line graph
ggplot(age_distribution_poststrat, aes(x=age, y=Percent, group=vote, color=vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Age", y="Vote Proportion", color="Political Party") +
  scale_color_manual(values=colors)

################################################################################
# find percentage of each vote by sex in ces
sex_distribution_ces <- count(ces, cps19_gender, cps19_vote) %>% group_by(cps19_gender) %>% mutate(Percent = n/sum(n))
# reorder levels
sex_distribution_ces$cps19_vote <- factor(sex_distribution_ces$cps19_vote, 
                                          levels=c("Green Party",
                                                   "New Democratic Party",
                                                   "Liberal Party",
                                                   "Conservative Party",
                                                   "People's Party",
                                                   "Bloc Québécois"))

# plot distribution with respect to sex as line graph
ggplot(sex_distribution_ces, aes(x=cps19_gender, y=Percent, group=cps19_vote, color=cps19_vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Sex", y="Vote Proportion", color="Political Party") + 
  scale_color_manual(values=colors)

# find percentage of each vote by sex groups in poststrat
sex_distribution_poststrat <- aggregate(freq ~ sex + vote, data=poststrat, sum) %>% group_by(sex) %>% mutate(Percent = freq/sum(freq))
# reorder levels
sex_distribution_poststrat$vote <- factor(sex_distribution_poststrat$vote, 
                                          levels=c("Green Party",
                                                   "New Democratic Party",
                                                   "Liberal Party",
                                                   "Conservative Party",
                                                   "People's Party",
                                                   "Bloc Québécois"))
# plot distribution with respect to sex as line graph
ggplot(sex_distribution_poststrat, aes(x=sex, y=Percent, group=vote, color=vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Sex", y="Vote Proportion", color="Political Party") +
  scale_color_manual(values=colors)

################################################################################
# find percentage of each vote by province in ces
province_distribution_ces <- count(ces, cps19_province, cps19_vote) %>% group_by(cps19_province) %>% mutate(Percent = n/sum(n))
# reorder levels
province_distribution_ces$cps19_vote <- factor(province_distribution_ces$cps19_vote, 
                                          levels=c("Green Party",
                                                   "New Democratic Party",
                                                   "Liberal Party",
                                                   "Conservative Party",
                                                   "People's Party",
                                                   "Bloc Québécois"))

# plot distribution with respect to province as line graph
ggplot(province_distribution_ces, aes(x=cps19_province, y=Percent, group=cps19_vote, color=cps19_vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Province", y="Vote Proportion", color="Political Party") + 
  theme(axis.text.x = element_text(angle = 25)) +
  scale_color_manual(values=colors)

# find percentage of each vote by province in poststrat
province_distribution_poststrat <- aggregate(freq ~ province + vote, data=poststrat, sum) %>% group_by(province) %>% mutate(Percent = freq/sum(freq))
# reorder levels
province_distribution_poststrat$vote <- factor(province_distribution_poststrat$vote, 
                                          levels=c("Green Party",
                                                   "New Democratic Party",
                                                   "Liberal Party",
                                                   "Conservative Party",
                                                   "People's Party",
                                                   "Bloc Québécois"))
# plot distribution with respect to province as line graph
ggplot(province_distribution_poststrat, aes(x=province, y=Percent, group=vote, color=vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Province", y="Vote Proportion", color="Political Party") +
  theme(axis.text.x = element_text(angle = 25)) +
  scale_color_manual(values=colors)

################################################################################
# find percentage of each vote by education in ces
education_distribution_ces <- count(ces, cps19_education, cps19_vote) %>% group_by(cps19_education) %>% mutate(Percent = n/sum(n))
# reorder levels
education_distribution_ces$cps19_vote <- factor(education_distribution_ces$cps19_vote, 
                                               levels=c("Green Party",
                                                        "New Democratic Party",
                                                        "Liberal Party",
                                                        "Conservative Party",
                                                        "People's Party",
                                                        "Bloc Québécois"))
# reorder education levels
education_distribution_ces$cps19_education <- factor(education_distribution_ces$cps19_education, 
                                                levels=c("High school or less",
                                                         "Some college or trade school",
                                                         "Undergraduate degree",
                                                         "Some graduate school or more"))
# plot distribution with respect to education as line graph
ggplot(education_distribution_ces, aes(x=cps19_education, y=Percent, group=cps19_vote, color=cps19_vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Education", y="Vote Proportion", color="Political Party") + 
  theme(axis.text.x = element_text(angle = 10)) +
  scale_color_manual(values=colors)

# find percentage of each vote by education in poststrat
education_distribution_poststrat <- aggregate(freq ~ education + vote, data=poststrat, sum) %>% group_by(education) %>% mutate(Percent = freq/sum(freq))
# reorder levels
education_distribution_poststrat$vote <- factor(education_distribution_poststrat$vote, 
                                               levels=c("Green Party",
                                                        "New Democratic Party",
                                                        "Liberal Party",
                                                        "Conservative Party",
                                                        "People's Party",
                                                        "Bloc Québécois"))
# reorder education levels
education_distribution_poststrat$education <- factor(education_distribution_poststrat$education, 
                                                     levels=c("High school or less",
                                                              "Some college or trade school",
                                                              "Undergraduate degree",
                                                              "Some graduate school or more"))
# plot distribution with respect to education as line graph
ggplot(education_distribution_poststrat, aes(x=education, y=Percent, group=vote, color=vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Education", y="Vote Proportion", color="Political Party") +
  theme(axis.text.x = element_text(angle = 10)) +
  scale_color_manual(values=colors)

################################################################################
# find percentage of each vote by religion importance in ces
religion_distribution_ces <- count(ces, cps19_rel_imp, cps19_vote) %>% group_by(cps19_rel_imp) %>% mutate(Percent = n/sum(n))
# reorder levels
religion_distribution_ces$cps19_vote <- factor(religion_distribution_ces$cps19_vote, 
                                                levels=c("Green Party",
                                                         "New Democratic Party",
                                                         "Liberal Party",
                                                         "Conservative Party",
                                                         "People's Party",
                                                         "Bloc Québécois"))
# plot distribution with respect to religion importance as line graph
ggplot(religion_distribution_ces, aes(x=cps19_rel_imp, y=Percent, group=cps19_vote, color=cps19_vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Religion Importance", y="Vote Proportion", color="Political Party") + 
  theme(axis.text.x = element_text(angle = 10)) +
  scale_color_manual(values=colors)

# find percentage of each vote by religion importance in poststrat
religion_distribution_poststrat <- aggregate(freq ~ religion_importance + vote, data=poststrat, sum) %>% group_by(religion_importance) %>% mutate(Percent = freq/sum(freq))
# reorder levels
religion_distribution_poststrat$vote <- factor(religion_distribution_poststrat$vote, 
                                                levels=c("Green Party",
                                                         "New Democratic Party",
                                                         "Liberal Party",
                                                         "Conservative Party",
                                                         "People's Party",
                                                         "Bloc Québécois"))
# plot distribution with respect to religion importance as line graph
ggplot(religion_distribution_poststrat, aes(x=religion_importance, y=Percent, group=vote, color=vote)) +
  geom_point() + 
  geom_line() + 
  labs(x="Religion Importance", y="Vote Proportion", color="Political Party") +
  theme(axis.text.x = element_text(angle = 10)) +
  scale_color_manual(values=colors)

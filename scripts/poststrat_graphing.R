library(tidyverse)

poststrat <- read("outputs/post_strat.csv")
counttb <- count(x=poststrat, vote, wt=freq)
total <- sum(counttb$n)
counttb$Proportion <- counttb$n/total

# plot distribution as bar graph
ggplot(data=counttb, aes(x=vote, y=Proportion, fill=Proportion)) +
  geom_bar(stat="identity") +
  labs(x="Political Party", y="Proportion", title = "Distribution of Votes in Post-Stratification") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_text(aes(label=paste0(round(Proportion*100), "%")), position=position_dodge(width=0.9), vjust=-0.25)

#Top Platforms:
library(ggplot2)
library(dplyr)
plat<-separate_rows(campus_df2,Platform,sep=";")
plat$Platform<-as.factor(plat$Platform)
count_plat<-table(plat$Platform)
sorted_count_plat<-head(sort(count_plat,decreasing = TRUE))
sorted_count_platform=data.frame(sorted_count_plat)

ggplot(sorted_count_platform,aes(Var1,Freq))+
  geom_bar(stat="identity",
           colour="brown1",
           fill="darkorchid")+
  labs(x="Platforms",
       y="Count",
       title = "Top Platforms across Eduvos")
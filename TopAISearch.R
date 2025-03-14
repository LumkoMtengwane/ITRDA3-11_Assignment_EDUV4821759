#Top AI Search:
library(ggplot2)
library(dplyr)
aisearch2<-separate_rows(campus_df2,AISearch,sep = ";")
aisearch2$AISearch<-as.factor(aisearch2$AISearch)
count_aisearch2<-table(aisearch2$AISearch)
sorted_count_ais<-head(sort(count_aisearch2,decreasing = TRUE))
sorted_count_aisearch<-data.frame(sorted_count_ais)

ggplot(sorted_count_aisearch,aes(Var1,Freq))+
  geom_bar(stat="identity",
           colour="blue",
           fill="coral")+
  labs(x="AISearch",
       y="Count",
       title = "Top AISearch engines across Eduvos")
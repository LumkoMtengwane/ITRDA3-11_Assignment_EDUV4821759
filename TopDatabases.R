#Top Databases:
library(ggplot2)
library(dplyr)
database2<-separate_rows(campus_df2,Databases,sep = ";")
database2$Databases<-as.factor(database2$Databases)
count_database2<-table(database2$Databases)
sorted_count_d<-head(sort(count_database2, decreasing = TRUE))
sorted_count_database<-data.frame(sorted_count_d)

ggplot(sorted_count_database,aes(Var1,Freq))+
  geom_bar(stat="identity",
           colour="black",
           fill="darkolivegreen3")+
  labs(x="Databases",
       y="Count",
       title = "Top Databases across Eduvos")
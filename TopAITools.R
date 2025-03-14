#Top AI Tools
library(ggplot2)
library(dplyr)
aitool2<-separate_rows(campus_df2,AITool,sep=";")
aitool2$AITool<-as.factor(aitool2$AITool)
count_aitool2<-table(aitool2$AITool)
sorted_count_ait<-head(sort(count_aitool2,decreasing = TRUE))
sorted_count_aitool<-data.frame(sorted_count_ait)

ggplot(sorted_count_aitool,aes(Var1,Freq))+
  geom_bar(stat="identity",
           colour="blue4",
           fill="cadetblue")+
  labs(x="AI Tools",
       y="Count",
       title = "Top AI tools across Eduvos")
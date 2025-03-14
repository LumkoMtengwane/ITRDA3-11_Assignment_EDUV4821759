#Top Web Frameworks:
library(ggplot2)
library(dplyr)
webf<-separate_rows(campus_df2,WebFramework,sep=";")
webf$WebFramework<-as.factor(webf$WebFramework)
count_webf<-table(webf$WebFramework)
sorted_count_webf<-head(sort(count_webf,decreasing = TRUE))
sorted_count_webframework=data.frame(sorted_count_webf)

ggplot(sorted_count_webframework,aes(Var1,Freq))+
  geom_bar(stat="identity",
           colour="bisque4",
           fill="bisque2")+
  labs(x="Web Frameworks",
       y="Count",
       title = "Top Web Frameworks across Eduvos")
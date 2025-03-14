###Question 2:
library(ggplot2)
library(dplyr)

#1)i.Top tools used:
#Top Programming languages:
proglang2<-separate_rows(campus_df2,ProgLang,sep = ";")
proglang2$ProgLang<-as.factor(proglang2$ProgLang)
count_proglang2<-table(proglang2$ProgLang)
sorted_count_pl<-head(sort(count_proglang2,decreasing =TRUE))
sorted_prog_lang<-data.frame(sorted_count_pl)

ggplot(sorted_prog_lang,aes(Var1,Freq))+
  geom_bar(stat="identity",
           color="magenta",
           fill="lightblue")+
  labs(x="Programming Languages",
       y="Count",
       title = "Top Programming Languages across Eduvos")

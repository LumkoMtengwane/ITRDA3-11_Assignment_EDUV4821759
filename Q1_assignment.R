######################################
######Question 1:

#Importing libraries:
library(dplyr)
library(tidyverse)

#Importing data frame:
campus_df<-read_csv("graduate_survey.csv")

#Creating new data frame with only necessary columns:
campus_df2<-campus_df %>% select("Campus",
                                 "StudyField",
                                 "Branch",
                                 "Role",
                                 "EduLevel",
                                 "ProgLang",
                                 "Databases",
                                 "Platform", "WebFramework","Industry","AISearch","AITool","Employment")


#Removing rows with NA values:
missing_values<-is.na(campus_df2)
sum(missing_values)
campus_df2<-na.omit(campus_df2)


#Standardizing categorical values in columns:
campus_df2<-campus_df2 %>% 
  mutate(Campus=recode(Campus,"Umhlanga Campus"="Durban Campus", 
                       "Mbombela"="Nelspruit",
                       "Nelson Mandela Bay Campus"="Port Elizabeth Campus",
                       "Mowbray Campus"="Tyger Valley Campus"))



#Sub-setting data to contain the 3-5 campuses with the most responses
count(campus_df2,Campus,sort=TRUE)
campus_df2<-subset(campus_df2,campus_df2$Campus %in% 
                     c("Port Elizabeth Campus","Durban Campus","Tyger Valley Campus","Pretoria Campus","Vanderbijlpark Campus"),
                   select=c("Campus","StudyField","Branch","Role","EduLevel","ProgLang","Databases","Platform","WebFramework",
                          "Industry","AISearch","AITool","Employment"))

#Removing duplicates:
campus_df2<-campus_df2 %>% distinct()

#Changing class types to factor class:
summary(campus_df2)
campus_df2$Campus<-as.factor(campus_df2$Campus)
campus_df2$StudyField<-as.factor(campus_df2$StudyField)
campus_df2$Branch<-as.factor(campus_df2$Branch)
campus_df2$Role<-as.factor(campus_df2$Role)
campus_df2$EduLevel<-as.factor(campus_df2$EduLevel)
campus_df2$ProgLang<-as.factor(campus_df2$ProgLang)
campus_df2$Databases<-as.factor(campus_df2$Databases)
campus_df2$Platform<-as.factor(campus_df2$Platform)
campus_df2$WebFramework<-as.factor(campus_df2$WebFramework)
campus_df2$Industry<-as.factor(campus_df2$Industry)
campus_df2$AISearch<-as.factor(campus_df2$AISearch)
campus_df2$AITool<-as.factor(campus_df2$AITool)
campus_df2$Employment<-as.factor(campus_df2$Employment)
summary(campus_df2)

##############################################
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

#Top Databases:
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

#Top AI Search
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

#Top Web Frameworks:
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

#Top Platforms:
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

#Top AI Tools
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

#ii)
#Most popular industry graduates go into based on study field:
industry3<-separate_rows(campus_df2,Industry,sep = ",")
count_industry3<- industry3 %>% group_by(StudyField,Industry) %>%
  count(Industry,sort = TRUE)


ggplot(count_industry3,aes(x=reorder(Industry,n),y=n, fill=StudyField))+
  geom_bar(stat="identity",
    position = "dodge")+
  labs(title="Industries graduates go into based on study field")+
  coord_flip()+
  facet_wrap(~StudyField)+
  theme_minimal()

#iii)Top job roles graduates do into from various study fields:
count_role<-campus_df2 %>% group_by(StudyField,Role) %>%
  count(Role,sort = TRUE)

ggplot(count_role,aes(x=reorder(Role,n),y=n, fill=StudyField))+
  geom_bar(stat="identity",
           position = "dodge")+
  labs(title="Top roles graduates go into based on study field",
       title_position="middle")+##############################
  coord_flip()+
  facet_wrap(~StudyField)+
  theme_minimal()


#iv)Employment rates of graduates from each study:
employment_rate<-campus_df2 %>% 
  mutate(Employment=case_when(
    Employment %in% c("Not employed, but looking for work", 
                      "Not employed, but looking for work;Employed, part-time",
                      "Not employed, but looking for work;Not employed, and not looking for work", 
                      "Not employed, but looking for work;Student, full-time", 
                      "Not employed, but looking for work;Student, full-time;Student, part-time",
                      "Not employed, but looking for work;Student, part-time") ~"Unemployed",
    TRUE~ Employment
  ) )%>% group_by(StudyField)

employment_rate<- employment_rate %>%
  mutate(Employment=ifelse(Employment=="Unemployed","Unemployed","Employed"))

employment_rate<-employment_rate %>% count(Employment)
grad_rate<-employment_rate %>% 
  summarise(total_grads=n())

sum_employees<-sum(grad_rate$total_grads)

employee_rate_final<-((sum_employees/grad_rate$total_grads)*100)
employee_rate_final





#########################
###Question 3:
#i)
install.packages('rsconnect')
rsconnect::setAccountInfo(name='lumkods',
                          token='50C1372AC6A42A8615DB9A09D83C57B1',
                          secret='RacX2BHqDUyvbep1UgCJotISw2K9jNtxYJZJq6KU')
library(rsconnect)
rsconnect::deployApp('C:\Users\Apostle')

  
  


































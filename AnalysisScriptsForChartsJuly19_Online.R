#Clear existing data and graphics
#rm(list=ls())
#graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('july.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$redcap_survey_identifier)="Survey Identifier"
label(data$data_day_to_day_evaluation_timestamp)="Survey Timestamp"
label(data$course_title)="Course Title"
label(data$course_title_other)="Please specify other course title"
label(data$course_date)="Course Date"
label(data$affiliation)="Affiliation"
label(data$other_affiliation)="If other affiliation, please specify"
label(data$dept_div)="Department/Division"
label(data$role)="Role"
label(data$other_role)="If other role, please specify"
label(data$research_focus)="What is the focus of your research?"
label(data$class_expectations)="What did you hope to get out of this class?"
label(data$use_material)="Will you use what you learned in this class in your work?"
label(data$level_material)="Was the level of material presented:"
label(data$recommend_class)="Would you recommend this class to others?"
label(data$use_material_inrole)="How will you use what you learned in todays training in your current role?"
label(data$adv_topics_yn)="Are you interested in advanced topics"
label(data$adv_topics_description)="What other topics would you be interested in learning?"
label(data$effectively_presented)="How effectively presented was this class?"
label(data$class_length)="Was the length of time allotted for this topic:"
label(data$other_comments)="Share any other comments about this class or the Day to Day Series"
label(data$datascience_comfort)="Do you feel more comfortable with data science concepts?"
label(data$datascience_collab_prep)="Do you feel more prepared to pursue a collaboration?"
label(data$repro_redcap_level)="Was the level of material presented for REDCap:"
label(data$repro_r_level)="Was the level of material presented for R/R Markdown:"
label(data$data_day_to_day_evaluation_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$course_title.factor = factor(data$course_title,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","17","16"))
data$affiliation.factor = factor(data$affiliation,levels=c("1","2","3","4","5","6"))
data$role.factor = factor(data$role,levels=c("1","2","3","4","5","6","7","8","9","10","11"))
data$use_material.factor = factor(data$use_material,levels=c("1","2","3","4"))
data$level_material.factor = factor(data$level_material,levels=c("1","2","3"))
data$recommend_class.factor = factor(data$recommend_class,levels=c("1","2","3","4"))
data$adv_topics_yn.factor = factor(data$adv_topics_yn,levels=c("1","0"))
data$effectively_presented.factor = factor(data$effectively_presented,levels=c("1","2","3","4"))
data$class_length.factor = factor(data$class_length,levels=c("1","2","3"))
data$datascience_comfort.factor = factor(data$datascience_comfort,levels=c("1","2","3","4"))
data$datascience_collab_prep.factor = factor(data$datascience_collab_prep,levels=c("1","2","3","4"))
data$repro_redcap_level.factor = factor(data$repro_redcap_level,levels=c("1","2","3"))
data$repro_r_level.factor = factor(data$repro_r_level,levels=c("1","2","3"))
data$data_day_to_day_evaluation_complete.factor = factor(data$data_day_to_day_evaluation_complete,levels=c("0","1","2"))

levels(data$course_title.factor)=c("Clinical Research Data Management","Data Science for Non-Data Scientists","Data Visualization Clinic","Data Visualization with Excel","Data Visualization with GraphPad Prism","Data Visualization Best Practices","Data Visualization with ggplot2","Designing Longitudinal Studies and Surveys in REDCap","Improving Data Collection Workflows in REDCap","Getting Started with REDCap","Advanced REDCap","Introduction to R","Introduction to Git and GitHub","Research Data Management Essentials","Reproducibility Workshop","Statistical Process Control for Quality Improvement","Other")
levels(data$affiliation.factor)=c("School of Medicine","College of Dentistry","College of Nursing","NYU Langone Health","Sackler","Other")
levels(data$role.factor)=c("Faculty","Postdoc","Medical Student","Graduate Student","Resident","Administrator","Intern","Project/Research Coordinator","Project/Program/Research Manager","Data Analyst","Other")
levels(data$use_material.factor)=c("Definitely will","Probably will","Probably wont","Definitely wont")
levels(data$level_material.factor)=c("Too low","Just right","Too advanced")
levels(data$recommend_class.factor)=c("Highly recommend","Recommend","Recommend with reservations","Not recommend")
levels(data$adv_topics_yn.factor)=c("Yes","No")
levels(data$effectively_presented.factor)=c("Highly effective","Mostly effective","Somewhat effective","Not effective")
levels(data$class_length.factor)=c("Too short","Just right","Too long")
levels(data$datascience_comfort.factor)=c("Much more comfortable","Somewhat more comfortable","A little bit more comfortable","The same level of comfort")
levels(data$datascience_collab_prep.factor)=c("Definitely do","Probably do","Probably dont","Definitely dont")
levels(data$repro_redcap_level.factor)=c("Too low","Just right","Too advanced")
levels(data$repro_r_level.factor)=c("Too low","Just right","Too advanced")
levels(data$data_day_to_day_evaluation_complete.factor)=c("Incomplete","Unverified","Complete")

library(dplyr)
IntroR <- filter(data, record_id == 120)
HypoR <- filter(data, record_id == 121)
QualMan <- filter(data, record_id == 119)
Python <- filter(data, record_id == 118)
Document <- filter(data, record_id == 117)
LongiRed <- filter(data, record_id == 116)
WorkflowRed <- filter(data, record_id == 115)
IntroRed <- filter(data, record_id == 114)


#Making a bar chart of recommendations
library(ggplot2)
#Note this is a fake path for upload to github, you will need to point to a real directory
setwd("/July2019EvalChart")

##Intro to REDCap

ggplot(IntroRed,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("introRedRec.png")

#bar chart of will use in work
ggplot(IntroRed,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("introRedUse.png")
#Level of Material
ggplot(IntroRed,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("introRedLevel.png")

##Document

ggplot(Document,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("DocumentRec.png")

#bar chart of will use in work
ggplot(Document,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("DocumentUse.png")

#Level of Material
ggplot(Document,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("DocumentLevel.png")

## Hypothesis Testing 

ggplot(HypoR,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("HypoRRec.png")

#bar chart of will use in work
ggplot(HypoR,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("HypoRUse.png")
#Level of Material
ggplot(HypoR,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("HypoRLevel.png")

##IntroR

ggplot(IntroR,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("introRRec.png")

#bar chart of will use in work
ggplot(IntroR,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("introRUse.png")
#Level of Material
ggplot(IntroR,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("introRLevel.png")

##LongiRed

ggplot(LongiRed,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("LongiRedRec.png")

#bar chart of will use in work
ggplot(LongiRed,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("LongiRedUse.png")

#Level of Material
ggplot(LongiRed,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("LongiRedLevel.png")

##Python

ggplot(Python,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("PythonRec.png")

#bar chart of will use in work
ggplot(Python,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("PythonUse.png")
#Level of Material
ggplot(Python,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("PythonLevel.png")

##Qualitative

ggplot(QualMan,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("QualManRec.png")

#bar chart of will use in work
ggplot(QualMan,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("QualManUse.png")
#Level of Material
ggplot(QualMan,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("QualManLevel.png")

#WorkflowRed

ggplot(WorkflowRed,aes(x = recommend_class))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Would You Recommend this class to Others?",labels = c("", "Highly Recommend", "Recommend", "Recommend With Reservations", "Do Not Recommend", ""), limits = c(5,0), trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("WorkflowRedRec.png")

#bar chart of will use in work
ggplot(WorkflowRed,aes(x = use_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Will use material",labels = c("", "Definitely Will", "Probably Will", "Probably Won't", "Definitely Won't", ""), limits = c(5,0),trans = "reverse")+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("WorkflowRedUse.png")
#Level of Material
ggplot(WorkflowRed,aes(x = level_material))+geom_bar(fill = "steelblue")+ 
  scale_x_continuous(name = "Level of material",labels = c("", "Too Low", "Just Right", "Too Advanced", ""), limits = c(0,4))+
  theme_bw()+coord_flip()+ 
  stat_count(geom="text", aes(label =..count..),color = "white",  hjust = 1.5)
ggsave("WorkflowRedLevel.png")

#Creating tables of our Free Text Fields

PythonFreeText <- Python[,c(25,29,31,34)]
write.csv(PythonFreeText, file = "PythonFreeText.csv")

DocumentFreeText <- Document[,c(25,29,31,34)]
write.csv(DocumentFreeText, file = "DocumentFreeText.csv")

HypoRFreeText <- HypoR[,c(25,29,31,34)]
write.csv(HypoRFreeText, file = "HypoRFreeText.csv")

IntroRFreeText <- IntroR[,c(25,29,31,34)]
write.csv(IntroRFreeText, file = "IntroRFreeText.csv")

IntroRedFreeText <- IntroRed[,c(25,29,31,34)]
write.csv(IntroRedFreeText, file = "IntroRedFreeText.csv")

LongiRedFreeText <- LongiRed[,c(25,29,31,34)]
write.csv(LongiRedFreeText, file = "LongiRedFreeText.csv")

QualManFreeText <- QualMan[,c(25,29,31,34)]
write.csv(QualManFreeText, file = "QualManFreeText.csv")

WorkflowRedFreeText <- WorkflowRed[,c(25,29,31,34)]
write.csv(WorkflowRedFreeText, file = "WorkflowredFreeText.csv")

#optional
#Word Cloud
install.packages("tm")
library(tm)
#save comments as a csv called test.csv
#test <- read.csv("test.csv", header = TRUE)
png("pythonCloud.png")
PythonCorpus <- Corpus(VectorSource(Python$other_comments))
#View(PythonCorpus)
PythonCorpus <- tm_map(PythonCorpus, content_transformer(tolower))
PythonCorpus <- tm_map(PythonCorpus, removeNumbers)
PythonCorpus <- tm_map(PythonCorpus, removeWords, stopwords("english"))
PythonCorpus <- tm_map(PythonCorpus, removePunctuation)
PythonCorpus <- tm_map(PythonCorpus, stripWhitespace)
tdm <- TermDocumentMatrix(PythonCorpus)
m <- as.matrix(tdm)

png("IntroREDCloud.png")
IntroRedCorpus <- Corpus(VectorSource(IntroRed$other_comments))
#View(PythonCorpus)
IntroRedCorpus <- tm_map(IntroRedCorpus, content_transformer(tolower))
IntroRedCorpus <- tm_map(IntroRedCorpus, removeNumbers)
IntroRedCorpus <- tm_map(IntroRedCorpus, removeWords, stopwords("english"))
IntroRedCorpus <- tm_map(IntroRedCorpus, removePunctuation)
IntroRedCorpus <- tm_map(IntroRedCorpus, stripWhitespace)
tdm <- TermDocumentMatrix(IntroRedCorpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing = TRUE)
d <- data.frame(word = names(v),freq=v)
wordcloud(d$word, d$freq, random.order = FALSE,scale = c(1,.5) , rot.per = .3, min.freq=1, colors = brewer.pal(8, "Dark2"))
dev.off()

#Info on Role as chart
ggplot(HypoR, aes(x = role.factor))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Role")+scale_y_continuous(breaks = (1:3))
ggsave("roleHypoR.png")
ggplot(HypoR, aes(x = role.factor))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Role")+scale_y_continuous(breaks = (1:3))
ggsave("roleHypoR.png")

#dept Div as chart
ggplot(HypoR, aes(x = dept_div))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Department or Division")+scale_y_continuous(breaks = (1:3))
ggsave("hypoRDepts.png")

#Info on Role as chart
ggplot(Document, aes(x = role.factor))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Role")+scale_y_continuous(breaks = (1:5))
ggsave("DocumentRole.png")


#dept Div as chart
ggplot(Document, aes(x = dept_div))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Department or Division")+scale_y_continuous(breaks = (1:5))
ggsave("DocumentDept.png")

#Info on Role as chart
ggplot(QualMan, aes(x = role.factor))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Role")+scale_y_continuous(breaks = (1:5))
ggsave("QualManRole.png")


#dept Div as chart
ggplot(QualMan, aes(x = dept_div))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Department or Division")+scale_y_continuous(breaks = (1:10))
ggsave("QualManDept.png")

#Info on Role as chart
ggplot(IntroR, aes(x = role.factor))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Role")+scale_y_continuous(breaks = (1:5))
ggsave("IntroRRole.png")


#dept Div as chart
ggplot(IntroR, aes(x = dept_div))+geom_bar(fill ="steelblue")+coord_flip()+theme_bw()+scale_x_discrete(name="Department or Division")+scale_y_continuous(breaks = (1:10))
ggsave("IntroRDept.png")
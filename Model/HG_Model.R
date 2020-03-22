#########################################
# OUJBIH ABDERRAHIM              ########
# 21/03/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
#########################################

# importation des packages-----
library(readxl)
library(ggplot2)
library(dplyr)
library(class)
library(hrbrthemes)
library(RColorBrewer)
library(plotly) #interactive graphe 
library(dygraphs)
library(randomForest)
library(rpart)


# importation des données -------


My_data <-read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "Main")
My_data_sortie <-read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "Sortie") %>% 
  filter(Ligne =="HG")

Janvier_DATA <- My_data %>% filter(DATE <"2020-02-01")

a <- My_data_sortie %>% 
  group_by(Poste,as.Date(Date)) %>% 
  summarize(min = min(format(Heure,'%H:%M')),
            Max = max(format(Heure,'%H:%M')),
            n = n())

a %>% ggplot(aes(`as.Date(Date)`,n,group=Poste,col=Poste,label=n))+
  geom_line()+
  geom_text()+ 
  facet_wrap(~Poste,ncol = 1)

a %>% group_by(Poste) %>% 
  summarize(mean = mean(n),
            sd = sd(n))






dim(My_data)
summary(My_data)
head(My_data)

nbatterie <- My_data %>% group_by(Batterie) %>% summarise()
date <- My_data %>% select(Date) %>% distinct 
qualité <- My_data %>% select(Qualité) %>% distinct 



# ------ nombre de mesure par jour -------






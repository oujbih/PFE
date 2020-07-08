#########################################
# OUJBIH ABDERRAHIM              ########
# 27/02/2020                     ########
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

My_data <-read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/PILOTAGE PROCESS 2020 .xlsx", sheet = "PSD", skip = 2)
My_dataHG= My_data %>% select(-`Débit brut t/h`,-`Mode de traitement`) %>% 
  filter( Ligne %in% "HG") 
My_dataHG$`%>250 µm (SDP)`=as.numeric(My_dataHG$`%>250 µm (SDP)`)
My_dataHG$`%<40 µm  (SDP)`=as.numeric(My_dataHG$`%<40 µm  (SDP)`)
My_dataHG$`D 80  (SDP)`=as.numeric(My_dataHG$`D 80  (SDP)`)
My_dataHG$`Densité mesuré  (SDP)`=as.numeric(My_dataHG$`Densité mesuré  (SDP)`)

My_dataLG= My_data %>%  select(-`Débit brut t/h`,-`Mode de traitement`) %>% 
  filter( Ligne %in% "LG")
My_dataLG$`%>250 µm (SDP)`=as.numeric(My_dataLG$`%>250 µm (SDP)`)
My_dataLG$`%<40 µm  (SDP)`=as.numeric(My_dataLG$`%<40 µm  (SDP)`)
My_dataLG$`D 80  (SDP)`=as.numeric(My_dataLG$`D 80  (SDP)`)
My_dataLG$`Densité mesuré  (SDP)`=as.numeric(My_dataLG$`Densité mesuré  (SDP)`)


#info

My_dataHG %>% 
  summarise_at(vars(-Date,-Poste,-Heure,-Qualité,-`Repère de stockage`,-Ligne), funs(mean(.,na.rm = T)))

My_dataLG %>% 
  summarise_at(vars(-Date,-Poste,-Heure,-Qualité,-`Repère de stockage`,-Ligne), funs(mean(.,na.rm = T)))


# qualité <- My_data %>% select(Qualité) %>% distinct 




# ------ nombre de mesure par jour -------
tmp <- My_data  %>% group_by(Date) %>%tally()

ggplot(tmp,aes(x=Date,y=n))+
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  theme_ipsum()
 
p <- ggplot(tmp,aes(x=Date,y=n))+
  geom_line(color="#69b3a2") +
  geom_point(color="#69b3a2", size=2)  +
  coord_cartesian(xlim =)+
  theme_ipsum()
ggplotly(p)

tmp$Date <- ymd(tmp$Date)
don <- xts(x = tmp$n, order.by = tmp$Date)
dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#69b3a2") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)




#par batterie ----
My_data %>% 
  group_by(Batterie,Qualité) %>% 
  tally() %>% 
  ggplot( aes(x=Qualité, y=n, group=Batterie, color=Batterie,label = Batterie)) +
  geom_text()+
  geom_line() +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("") +
  theme_ipsum()


#pie qualit-------

a <- My_dataLG %>% 
  group_by(Qualité) %>% 
  tally()

myPalette <- brewer.pal(9, "Set2") 
pie(a$n,labels =paste(a$Qualité,'\n',round(prop.table(a$n)*100,2), "%", sep = ""),col = myPalette)





#petite analyse de ##############

#distribution D80 ----

My_dataLG %>% 
  ggplot( aes(x=`%>250 µm (SDP)`)) +
  geom_density(fill="#084887", color="#e9ecef", alpha=0.8) +
  geom_vline(xintercept = 5,color="red") +
  geom_vline(xintercept = 6.4,color="#58355e") +
  ggtitle(">250 µm  SDP") +
  theme_ipsum()

#touts 
# My_data %>% 
#   select(Batterie,`D80 OF`) %>% 
#   ggplot( aes(x=`D80 OF`,group=Batterie,fill=Batterie)) +
#   geom_density(alpha=0.8) +
#   theme_ipsum()+
#   facet_wrap(~Batterie)
# tmp2 %>% select(Batterie,`D80 OF`)








#RandomForeste----------

#HG 
# graph -----


My_dataHG %>% 
  ggplot( aes(x=Date, y=`D 80  (SDP)`, group=Qualité, color=Qualité,label = Qualité)) +
  geom_line() +
  geom_hline(yintercept = 160,color="red") +
  theme(
    legend.position="none",
    plot.title = element_text(size=14)
  ) +
  ggtitle("HG D80") +
  theme_ipsum()


# LM -----

#HG

set.seed(1234)
index_train  <- sample(1:nrow(My_dataHGD80),nrow(My_dataHGD80)*0.75)
DATA.train <- My_dataHGD80[index_train,]
DATA.test <- My_dataHGD80[-index_train,]

#------

names(DATA.train) <- make.names(names(DATA.train))

rf=randomForest(`D80.OF`~ . , data = DATA.train)
summarise(rf)
plot(rf)
varImpPlot(rf)


par(xpd=T)
D80.rpart=rpart(`D80 OF`~ . , data = DATA.train)
plot(D80.rpart)
text(D80.rpart,use.n = T,pretty = 0)

#interprétation ?

# lm----

modelDHG80 <- lm(`D80 OF`~. , data=DATA.train)
summary(modelDHG80)

My_dataD80new <- My_dataD80 %>% filter(Qualité %in% "BTR BA") %>% select(-Qualité)
index_trainnew  <- sample(1:nrow(My_dataD80new),nrow(My_dataD80new)*0.75)
modelD80 <- lm(`D80 OF`~. , data=My_dataD80new[index_trainnew,])
summary(modelD80)


modelD80 <- lm(`D80 OF`~`Pression bar` , data=My_dataD80new[index_trainnew,])
summary(modelD80)


ggplot(My_dataD80new,aes(y=`D80 OF`,x=`Pression bar`))+
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  theme_ipsum()





#Intervalle de confiance
confint(modelD80)
#Probleme !!!!!

DATA.train %>% 
  ggplot( aes(x = factor(1:168), y=`D80 OF`))+
  geom_line(color="#69b3a2") +
  geom_point(color="#69b3a2", size=2) +
  # geom_point(aes(y=`Dilution HP m3/h`),color="red", size=2) +
  theme_ipsum()










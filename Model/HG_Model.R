#########################################
# OUJBIH ABDERRAHIM              ########
# 21/03/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
#########################################




# importation des packages----------------------------------------------------------
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
library(rpart.plot)
library(TTR)
# setwd("C:/Users/OUJBIH/Desktop/Stage PFE/R")

# importation des données -----------------------------------------------------------------
My_data <-read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "Main")
Janvier_DATA <- My_data %>% filter(DATE <"2020-02-01") %>% select(-`Energie Broyage HG`,-`Puissance Broyage HG`)
My_data_sortie <- read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "Sortie") %>%  filter(Ligne =="HG")
My_data_CPT <- read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "CPT")
My_data_CPT <- imputeTS::na_kalman(My_data_CPT)


# My_data_CPT$`CPT >2500` %>% forecast::ma(order=5)
# My_data_CPTtmp <- My_data_CPT %>%  tidyr::drop_na(`CPT >2500`)
# My <-imputeTS::na_kalman(My_data_CPT)
# My_data_CPT_MA=My_data_CPT












# founction ----------------------------------------------------------
tmp <- colooor
colooor =sample(col_vector, 2)

ftmp <- function(col){
  Model_data %>% 
    ggplot(aes(DATE,get(col))) +
    geom_line()+
    geom_smooth()+
    ylab(col)
  ggsave(paste(col,".png",sep = ""),path = "images",scale =3)
}

ftmp <- function(col){

My_data_CPT %>% 
  ggplot(aes(as.Date(DATE),get(col))) +
  geom_line(color=colooor[1])+
  geom_point(color=colooor[2], size=2)  +
  geom_smooth()+xlab("")+ylab(as.character(col))+
  scale_x_date(date_labels = "%d-%b",
               date_minor_breaks = "1 day",
               date_breaks = "2 day"
               #limit=c(as.Date("2020-01-01"),as.Date("2020-01-10"))
               )+
  # ylim(0,60)+
  theme(axis.text.x=element_text(angle=60, hjust=1))
ggsave(paste("CPT ",substr(col,6,10),".pdf",sep = ""),path = "images/input",scale =3)
}

# for(col in names(My_data_CPT)){
#   if(col != "DATE"){
#     print(col)
#     ftmp(col)
#   }
# }



Transform_sortie <- function(dataframe){
  dataframe %>% 
    filter(Date <"2020-02-01") %>% 
    mutate(sortie_date = lubridate::as_datetime(paste(as.Date(Date),format(Heure,'%H:%M:%S')))) %>% 
    select(sortie_date, -Date,-Heure ,Poste,Qualité,`%<40 µm  (SDP)`,`%>250 µm (SDP)`,`D 80  (SDP)`) %>% 
    transform("40µm" = as.numeric(`%<40 µm  (SDP)`),
              "250µm" = as.numeric(`%>250 µm (SDP)`) ,
              "D80" = as.numeric(`D 80  (SDP)`)) %>% 
    select(sortie_date,Poste,Qualité,"X40µm","X250µm","D80")
    
}



# observation par poste --------------------------------------------------------------
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
a <- My_data_sortie %>% 
  group_by(Qualité) %>% 
  tally()

myPalette <- RColorBrewer::brewer.pal(9, "Set2") 
pie(a$n,labels =paste(a$Qualité,'\n',round(prop.table(a$n)*100,2), "%", sep = ""),col = myPalette)

# clean data ---------------------------------------------------------------
My_data_sortie <- Transform_sortie(My_data_sortie)


#Merge paramètres et les entres----------------------------------------------------------
#merge CPT and Output

My_data_sortie_tmp <- My_data_sortie %>% mutate(DATE=lubridate::as_date(sortie_date))
My_data_CPT$DATE =as.Date(My_data_CPT$DATE)
Model_data_CPT <- merge(x = My_data_CPT, y = My_data_sortie_tmp, by.x = "DATE",by.y = "DATE", all.y = TRUE)
Model_data_CPT <- Model_data_CPT %>% select(-DATE,-Qualité.x) %>% select(sortie_date,everything())


Model_data <- merge(x = Janvier_DATA, y = Model_data_CPT, by.x = "DATE",by.y = "sortie_date", all.y = TRUE)
Model_data[Model_data==0] <- NA
Model_data$`Courant Broyeur ML004`= 159
Model_data$`Courant Broyeur ML002`[Model_data$`Courant Broyeur ML002`== "Arc Off-line"] <- NA
Model_data <- imputeTS::na_kalman(Model_data) #na_interpolation na_ma 




#Train and test ----------------------------------------------------------
set.seed(1234)
index_train  <- sample(1:nrow(Model_data),nrow(Model_data)*0.75)
DATA.train <- Model_data[index_train,]
DATA.test <- Model_data[-index_train,]

DATA.train <- DATA.train %>% mutate_if(is.character, as.factor)
DATA.traintmp <- DATA.train %>% 
  select(-"D80",-"X250µm",-"DATE") %>% 
  mutate(sortie=ifelse(as.numeric(X40µm)<25,"Bon","Mauvais"))  %>% 
  select(-"X40µm",-"Courant Broyeur ML002",-"Courant Broyeur ML004") %>% 
  mutate_if(is.character, as.factor)

DATA.traintmp_regression <- DATA.train %>% 
  select(-"D80",-"X250µm",-"DATE") %>% 
  select(-"Courant Broyeur ML002",-"Courant Broyeur ML004") %>% 
  mutate_if(is.character, as.factor)



DATA.testmp <- DATA.test %>% 
  select(-"D80",-"X250µm",-"DATE") %>% 
  mutate(sortie=ifelse(as.numeric(X40µm)<25,"Bon","Mauvais"))  %>% 
  select(-"X40µm",-"Courant Broyeur ML002",-"Courant Broyeur ML004") %>% 
  mutate_if(is.character, as.factor)

DATA.testmp_regression <- DATA.test %>% 
  select(-"D80",-"X250µm",-"DATE") %>% 
  select(-"Courant Broyeur ML002",-"Courant Broyeur ML004") %>% 
  mutate_if(is.character, as.factor)

modelDHG80 <- lm(X40µm~. , data=DATA.traintmp_regression)
summary(modelDHG80)


# DATA.traintmp <- DATA.traintmp %>% select(-"Courant Broyeur ML002",-"Courant Broyeur ML004")--------------
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))


#randomForest ----------------------------------------------------------
colnames(DATA.traintmp_regression)[13:18]=c("CPT2500","CPT400","CPT160","CPT125","CPT40","CPTinf40")
colnames(DATA.testmp_regression)[13:18]=c("CPT2500","CPT400","CPT160","CPT125","CPT40","CPTinf40")
rf=randomForest(sortie~ . , data = DATA.traintmp)
rf_rg=randomForest(X40µm~ . , data = DATA.traintmp_regression)

summary(rf_rg)
plot(rf_rg)
varImpPlot(rf_rg)
imp <- cbind.data.frame(Feature=rownames(rf_rg$importance),rf_rg$importance)
g <- ggplot(imp, aes(x=reorder(Feature, IncNodePurity), y=IncNodePurity))
g + geom_bar(stat = 'identity',fill=sample(col_vector, 20)) + xlab('les vaiaibles ')+ylab("L'importance")+coord_flip()


DATA.testmp$sortie <- factor(DATA.testmp$sortie, levels = levels(DATA.traintmp$sortie))
DATA.testmp_regression$Qualité.y <- factor(DATA.testmp_regression$Qualité.y, levels = levels(DATA.traintmp$Qualité.y))


pred.train=predict(rf_rg,DATA.testmp_regression)
rf.pred=rep("Bon",nrow(DATA.testmp))
rf.pred[pred.train=="Mauvais"]="Mauvais"
pred.train <- as.vector(pred.train)
cbind(rf.pred,DATA.testmp$sortie)
table(rf.pred,DATA.testmp$sortie)
mean(rf.pred==DATA.testmp$sortie)


#ggplot test and train
group <- rep(1,nrow(Model_data))
group <- ifelse(seq(nrow(Model_data)) %in% index_train,"Train","Test")
df <- data.frame(date=Model_data$DATE[-index_train],rain=DATA.testmp_regression$X40µm,pred.train)
df <- data.frame(date=Model_data$DATE,rain=Model_data$X40µm,group)

# ...and plot it
ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
  scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")

ggplot(df,aes(x = date)) + geom_point( aes(y= rain,colour ="réel")) +
  geom_point( aes(y= pred.train,colour ="prévu")) +
  # geom_smooth(aes(y= rain))+
  # geom_smooth(aes(y= pred.train,color=col_vector[1]))+
  scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")





#Arbre de decision----------------------------------------------------------

par(xpd=T)
D80.rpart=rpart(sortie~ . , data = DATA.traintmp)
rpart.plot(D80.rpart,
           box.palette = "GnBu",
           tweak =1,
           uniform =FALSE,
           under.cex = 1)
plot(D80.rpart)

text(D80.rpart,use.n = T)
rattle::fancyRpartPlot(D80.rpart,tweak=1.2)

#########################################
# OUJBIH ABDERRAHIM              ########
# 29/03/2020                     ########
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
Fevrier_DATA <- My_data %>% filter(DATE <="2020-02-24") %>% select(-`Energie Broyage HG`,-`Puissance Broyage HG`)
My_data_sortie <- read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "Sortie") %>%  filter(Ligne =="HG")
My_data_sortie <- Transform_sortie(My_data_sortie)
My_data_CPT <- read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HG_DATA_Model.xlsx",sheet = "Entrées ")
# My_data_CPT <- imputeTS::na_kalman(My_data_CPT)
My_data_CPT <- My_data_CPT %>%select(-chaine,-repere) %>%  na.omit()

#merge CPT and Output
My_data_sortie_tmp <- My_data_sortie %>% mutate(DATE=lubridate::as_date(sortie_date))
My_data_CPT$DATE =as.Date(My_data_CPT$DATE)
Model_data_CPT <- merge(x = My_data_CPT, y = My_data_sortie_tmp, by.x = "DATE",by.y = "DATE", all.y = TRUE)
Model_data_CPT <- Model_data_CPT %>% select(-DATE,-Qualité.x) %>% select(sortie_date,everything())
#Merge with parametres
Model_data <- merge(x = Fevrier_DATA, y = Model_data_CPT, by.x = "DATE",by.y = "sortie_date", all.y = TRUE)
Model_data[Model_data==0] <- NA
Model_data$`Courant Broyeur ML004`= 159
Model_data$`Courant Broyeur ML002`[Model_data$`Courant Broyeur ML002`== "Arc Off-line"] <- NA
Model_data <- imputeTS::na_kalman(Model_data) #na_interpolation na_ma 
Model_data <- Model_data %>% na.omit()

#Train and test ----------------------------------------------------------
set.seed(1234)
index_train  <- sample(1:nrow(Model_data),nrow(Model_data)*0.75)
DATA.train <- Model_data[index_train,]
DATA.test <- Model_data[-index_train,]


DATA.traintmp_regression <- DATA.train %>% 
  select(-"D80",-"X250µm",-"DATE") %>% 
  select(-"Courant Broyeur ML002",-"Courant Broyeur ML004") %>% 
  mutate_if(is.character, as.factor) %>% na.omit()

DATA.testmp_regression <- DATA.test %>% 
  select(-"D80",-"X250µm",-"DATE") %>% 
  select(-"Courant Broyeur ML002",-"Courant Broyeur ML004") %>% 
  mutate_if(is.character, as.factor) %>% na.omit()

apply(is.na(DATA.traintmp_regression),2,sum)
apply(is.na(DATA.testmp_regression),2,sum)

#randomforest-----------------------------------------------------------
rf_rg=randomForest(X40µm~ . , data = DATA.traintmp_regression)

#R-squared 
#Train 

pred.train=predict(rf_rg,DATA.traintmp_regression)


Moyenne <- mean(DATA.traintmp_regression$X40µm)
df_moyenne <- rep(Moyenne,nrow(DATA.traintmp_regression))
R <- 1 - (sum(DATA.traintmp_regression$X40µm-pred.train)^2)/(sum(DATA.traintmp_regression$X40µm-df_moyenne)^2)

df <- data.frame(Date=Model_data$DATE[index_train],Train=DATA.traintmp_regression$X40µm,Predict =pred.train)
df <- as_tibble(df)

dftm <- df %>% filter(Date<="2020-02-28" )
dftm2 <- arrange(dftm,Date)
dftm2 <- dftm2 %>% mutate(diff = Train-Predict,
                          diff2= Train-Moyenne,
                          a = diff^2,
                          b = diff2^2)
1-sum(dftm2$a)/sum(dftm2$b) #0.8495432

ggplot(dftm2,aes(Date,Train))+
  geom_line(color=col_vector[5])+
  geom_line(aes(y=Predict),color=col_vector[10])+
  geom_hline(yintercept=Moyenne)


#Test 
DATA.testmp_regression <- rbind(DATA.traintmp_regression[1, ] , DATA.testmp_regression)
DATA.testmp_regression <- DATA.testmp_regression[-1,]
DATA.testmp_regression$Qualité.y <- factor(DATA.testmp_regression$Qualité.y, levels = levels(DATA.traintmp_regression$Qualité.y))
pred.test=predict(rf_rg,DATA.testmp_regression)


Moyenne <- mean(DATA.testmp_regression$X40µm)


df <- data.frame(Date=Model_data$DATE[-index_train],Train=DATA.testmp_regression$X40µm,Predict =pred.test)
df <- as_tibble(df)

dftm <- df %>% filter(Date<="2020-02-28" )
dftm2 <- arrange(dftm,Date)
dftm2 <- dftm2 %>% mutate(diff = Train-Predict,
                          diff2= Train-Moyenne,
                          a = diff^2,
                          b = diff2^2) %>% na.omit()
1-sum(dftm2$a)/sum(dftm2$b) #0.1705243

ggplot(dftm2,aes(Date,Train))+
  geom_line(color=col_vector[5])+
  geom_line(aes(y=Predict),color=col_vector[10])+
  geom_hline(yintercept=Moyenne)


ggplot(data = df,aes(Train,y=Predict))+
  geom_point(color=sample(col_vector, 1))+
  geom_line(aes(Train,Train,color=sample(col_vector, 1)))+
  geom_hline(yintercept=Moyenne)

#plot
imp <- cbind.data.frame(Feature=rownames(rf_rg$importance),rf_rg$importance)
g <- ggplot(imp, aes(x=reorder(Feature, IncNodePurity), y=IncNodePurity))
g + geom_bar(stat = 'identity',fill=sample(col_vector, 20)) + xlab('les vaiaibles ')+ylab("L'importance")+coord_flip()
ggsave(paste("Importance Mois Fevrier",".png",sep = ""),path = "images",width =11,height = 6)


DATA.testmp_regression <- rbind(DATA.traintmp_regression[1, ] , DATA.testmp_regression)
DATA.testmp_regression <- DATA.testmp_regression[-1,]
pred.train=predict(rf_rg,DATA.testmp_regression)

#ggplot test and train
group <- rep(1,nrow(Model_data))
group <- ifelse(seq(nrow(Model_data)) %in% index_train,"Train","Test")
#
df <- data.frame(date=Model_data$DATE,rain=Model_data$X40µm,group)

# ...and plot it
ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
  theme(legend.position="top")+ylab("le pourcentage des fines")

df <- data.frame(date=Model_data$DATE[-index_train],rain=DATA.traintmp_regression$X40µm,pred.train)



ggplot(df,aes(x = date)) + geom_point( aes(y= rain,colour ="réel")) +
  geom_point( aes(y= pred.train,colour ="prévu")) +
  # geom_smooth(aes(y= rain))+
  # geom_smooth(aes(y= pred.train,color=col_vector[1]))+
  scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")

ggsave(paste("TrainFines",".png",sep = ""),path = "images",width =11,height = 6)



##box plot 


Anova_data <- Model_data %>% select(16:21,Qualité.y) %>% distinct()


for(col in colnames(Model_data)[16:21]){
  ggplot(Anova_data,aes(Qualité.y,get(col),color=Qualité.y))+
    geom_boxplot()
  ggsave(paste(col,".png",sep = ""),path = "images/Qualite/2",width =11,height = 6)
}

Qualite=as.factor(Qualité.y)
CPT160 =as.double(CPT160)
'%ni%' <- Negate('%in%')
Anova_data <- Anova_data %>% filter(Qualité.y %ni% c("BTNX","TBTBA"))
attach(Anova_data)

modele <- aov(Qualite~CPT160) 
coef(modele)
anova(modele)
summary(modele)
TukeyHSD(modele) 
plot(modele)








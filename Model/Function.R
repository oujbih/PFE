#########################################
# OUJBIH ABDERRAHIM              ########
# 31/03/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
#########################################

qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
rm(qual_col_pals)
colooor =sample(col_vector, 2)

ftmp <- function(col){
  Model_data %>% 
    ggplot(aes(DATE,get(col))) +
    geom_line()+
    geom_smooth()+
    ylab(col)
  ggsave(paste(col,".png",sep = ""),path = "images/Parametres/Fevrier",width =11,height = 6)
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
  ggsave(paste("CPT ",substr(col,6,10),".pdf",sep = ""),path = "images/input",width =11,height = 6)
}




Transform_sortie <- function(dataframe){
  dataframe %>% 
    filter(Date <="2020-02-24") %>% 
    mutate(sortie_date = lubridate::as_datetime(paste(as.Date(Date),format(Heure,'%H:%M:%S'))),
           sortie_date2 = lubridate::as_datetime(ifelse(Poste=="P3"&format(sortie_date,'%H:%M:%S')<"23:00:00"&format(sortie_date,'%H:%M:%S')!="00:00:00",lubridate::as_datetime(paste(as.Date(sortie_date)+as.difftime(1, units="days"),format(sortie_date,'%H:%M:%S'))),sortie_date))) %>%
    
    select(sortie_date2, -Date,-Heure ,Poste,Qualité,`%<40 µm  (SDP)`,`%>250 µm (SDP)`,`D 80  (SDP)`,"repere"=`Repère de stockage`) %>% 
    transform("40µm" = as.numeric(`%<40 µm  (SDP)`),
              "250µm" = as.numeric(`%>250 µm (SDP)`) ,
              "D80" = as.numeric(`D 80  (SDP)`)) %>% 
    select("sortie_date"=sortie_date2,Poste,Qualité,"X40µm","X250µm","D80",repere) %>% arrange(sortie_date)
  
}


#MakeTrainTest
MakeTrainTest <- function(dataframe,a){
  set.seed(1234)
  index_train  <- sample(1:nrow(dataframe),nrow(dataframe)*a)
  DATA.train <- dataframe[index_train,]
  DATA.test <- dataframe[-index_train,]
  return(list(DATA.train,DATA.test,index_train))
}


#Make Merge CPT Sortie HG
MakeMergesoriteHG <- function(){
  load("Data/m.Rda")
  for(date in My_data_CPT$DATE){
    date <- as.Date(lubridate::as_datetime(date))
    k_line <-  My_data_CPT %>% filter(DATE ==date) %>% select(-chaine)%>% mutate(tmpdate2=as.Date(DATE))
    k_repere <-k_line %>% select(repere) %>% pull()
    k <- My_data_sortie %>% 
      filter(as.Date(sortie_date)==date | as.Date(sortie_date)==date+as.difftime(1, units="days") ) %>% 
      filter(repere ==k_repere) %>% mutate(tmpdate=as.Date(sortie_date))
    if(nrow(k)!=0){
      tmp<- merge(x = k_line, y = k, by.x = "tmpdate2",by.y = "tmpdate", all.y = TRUE)
      tmp22 <- tmp %>% select(-tmpdate2,-repere.y,-DATE,-Qualité.x) %>% select(sortie_date,Poste,"Qualité"=Qualité.y,everything(),"repere"=repere.x)
      print(date)
      print("===========================================")
      m <- rbind(m, tmp22)
    }
  }
  
  
  My_data_CPT_HG <- zoo::na.locf(m)
}



#data plot
plot_data_evolution <- function(dataframe,date,col){
  
  dataframe %>% 
    ggplot(aes(get(date),get(col),color=Qualité,label=Qualité)) +
    geom_line(color=col_vector[3])+
    geom_point(color=col_vector[2], size=2)  +
    geom_text(nudge_y = 0.6,angle=90)+
    xlab("")+ylab(col)+
    scale_x_date(date_labels = "%d-%b",
                 date_minor_breaks = "1 day",
                 date_breaks = "2 day"
                 #limit=c(as.Date("2020-01-01"),as.Date("2020-01-10"))
    )+
    theme(axis.text.x=element_text(angle=60, hjust=1))
    # ylim(0,60)+geom_smooth()
  ggsave(paste(col,".png",sep = ""),path = "images/version0.3/CPT",width =11,height = 6)
}

# plot_data_evolution(tmp2,"sortie_date","CPT_2500")

plot_data_density <- function(dataframe,col){
  
  dataframe %>% 
    ggplot(aes(get(col))) +
    geom_density(fill=sample(col_vector, 1))+
    xlab("")+ylab(col)
  # ylim(0,60)+geom_smooth()
  # ggsave(paste(col,".png",sep = ""),path = "images/version0.3",width =11,height = 6)
}


for(col in names(CPT_HG)){
  if(col != "sortie_date"){
    print(col)

    plot_data_evolution(CPT_HG,"sortie_date",col)
  }
}


randomForestHG <- function(){
  
  pred.train=predict(rf_rg,DATA.traintmp_regression)
  
  
  Moyenne <- mean(DATA.traintmp_regression$X40µm)
  df <- data.frame(Date=Model_data$sortie_date[index_train],Train=DATA.traintmp_regression$X40µm,Predict =pred.train)
  df <- as_tibble(df)
  
  dftm <- df %>% filter(Date<="2020-02-28" )
  dftm2 <- arrange(dftm,Date)
  dftm2 <- dftm2 %>% mutate(diff = Train-Predict,
                            diff2= Train-Moyenne,
                            a = diff^2,
                            b = diff2^2)
  R_train <- 1-sum(dftm2$a)/sum(dftm2$b) #0.8592731 0.8268042
  R_train
  
  ggplot(dftm2,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  imp <- cbind.data.frame(Feature=rownames(rf_rg$importance),rf_rg$importance)
  g <- ggplot(imp, aes(x=reorder(Feature, IncNodePurity), y=IncNodePurity))
  g + geom_bar(stat = 'identity',fill=sample(col_vector, 4)) + xlab('les vaiaibles ')+ylab("L'importance")+coord_flip()
  
  
  
  
  
  
  #Test 
  levels(DATA.testmp_regression$Qualité) <- levels(DATA.traintmp_regression$Qualité)
  pred.test=predict(rf_rg,DATA.testmp_regression)
  
  
  Moyenne <- mean(DATA.testmp_regression$X40µm)
  
  
  df <- data.frame(Date=Model_data$sortie_date[-index_train],Train=DATA.testmp_regression$X40µm,Predict =pred.test)
  df <- as_tibble(df)
  
  dftm <- df %>% filter(Date<="2020-02-28" )
  dftm2 <- arrange(dftm,Date)
  dftm2 <- dftm2 %>% mutate(diff = Train-Predict,
                            diff2= Train-Moyenne,
                            a = diff^2,
                            b = diff2^2) %>% na.omit()
  R_test <- 1-sum(dftm2$a)/sum(dftm2$b) #0.1705243
  R_test
  ggplot(dftm2,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  
  print(ggplot(data = df,aes(Train,y=Predict))+
    geom_point(color=col_vector[10])+
    geom_point(aes(Train,Train),color=col_vector[1])+
    geom_line(aes(Train,Train),color=col_vector[5])+
    geom_hline(yintercept=Moyenne))
  
  print(R_train)
  R_test
  # smalltrain <- DATA.traintmp_regression
  # smalltest <- DATA.testmp_regression
  # # 
  # #ggplot test and train
  # group <- rep(1,nrow(Model_data))
  # group <- ifelse(seq(nrow(Model_data)) %in% index_train,"Train","Test")
  # # df <- data.frame(date=Model_data_max$sortie_date[-index_train],rain=DATA.testmp_regression$X40µm,pred.test)
  # df <- data.frame(date=Model_data$sortie_date,rain=Model_data_mean$X40µm,group)
  # 
  # # ...and plot it
  # ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
  #   scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")
  # 
}


Makelag <- function(DataFrame,col,a){
  
}










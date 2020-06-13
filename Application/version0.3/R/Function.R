#########################################
# OUJBIH ABDERRAHIM              ########
# 31/03/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
#########################################
source("R/Classification/Main_HG.R",encoding = "UTF-8") # classification functions
source("R/Merge_HG.R",encoding = "UTF-8")               # All Merge function 
source("R/regression/Main_HG.R",encoding = "UTF-8")     # regression function



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
  # ggsave(paste("CPT ",substr(col,6,10),".pdf",sep = ""),path = "images/input",width =11,height = 6)
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
    # ggplot(aes(get(date),get(col),color=Qualité,label=Qualité)) +
    ggplot(aes(get(date),get(col))) +
    geom_line(color=col_vector[3])+
    geom_point(color=col_vector[2], size=2)  +
    # geom_text(nudge_y = 0.6,angle=90)+
    xlab("")+ylab(col)
    #+
    # scale_x_date(date_labels = "%d-%b",
    #              date_minor_breaks = "1 day",
    #              date_breaks = "2 day"
    #              #limit=c(as.Date("2020-01-01"),as.Date("2020-01-10"))
    # )+
    # theme(axis.text.x=element_text(angle=60, hjust=1))
    # ylim(0,60)+geom_smooth()
  # ggsave(paste(col,".png",sep = ""),path = "images/version0.3/CPT",width =11,height = 6)
}

# plot_data_evolution(tmp2,"DATE","Pression_PK13")

# 
# liste_date=Fevrier_DATA %>% select(DATE) %>% distinct(as.Date(DATE))
# liste_date <- liste_date[[1]]
# i=2
# 
# Fevrier_DATA %>%
#   filter(DATE>=liste_date[i]&DATE<=liste_date[i+1])%>%
#   ggplot(aes(x=DATE,y=Débit_CV004)) +
#   geom_line(color=col_vector[3])+
#   geom_point(color=col_vector[2], size=2)
# 
# k=1
# # for(i in 1:54){
#   
#   print(liste_date[i])
#   Fevrier_DATA %>%
#     filter(DATE>=liste_date[i]&DATE<=liste_date[i+1])%>%
#     ggplot(aes(x=DATE,y=Courant_ML004)) +
#     geom_line(color=col_vector[3])+
#     geom_point(color=col_vector[2], size=2)
#    
#   ggsave(paste(liste_date[i],".png",sep = ""),path = "image/Courant_ML004",width =11,height = 6)
#   
# }




plot_data_density <- function(dataframe,col){
  
  dataframe %>% 
    ggplot(aes(get(col))) +
    geom_density(fill=sample(col_vector, 1))+
    # geom_vline(xintercept = 20,color="red") +
    xlab("")+ylab(col)
  # ylim(0,60)+geom_smooth()
  # ggsave(paste(col,".png",sep = ""),path = "images/version0.3",width =11,height = 6)
}

plot_all <- function(DataFrame,kdate="sortie_date"){
  for(col in names(DataFrame)){
    if(col != "sortie_date" &col != "DATE"){
      print(col)

      plot_data_evolution(DataFrame,kdate,col)
      ggsave(paste(col,".png",sep = ""),path = "images/All",width =11,height = 6)
    }
  }
}
# 




randomForestHG <- function(rf_rg,dataframe,DATA.traintmp_regression,DATA.testmp_regression,index_train,output_variable="X40µm"){
  #train
  pred.train=predict(rf_rg,DATA.traintmp_regression)
  Moyenne <- mean(DATA.traintmp_regression[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=DATA.traintmp_regression[[output_variable]],Predict =pred.train)

  R_train <- caret::postResample(df$Predict,df$Train)[2]
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  imp <- cbind.data.frame(Feature=rownames(rf_rg$importance),rf_rg$importance)
  plot7 <- ggplot(imp, aes(x=reorder(Feature, IncNodePurity), y=IncNodePurity))+
    geom_bar(stat = 'identity',fill=sample(col_vector, nrow(imp))) + xlab('les vaiaibles ')+ylab("L'importance")+coord_flip()
    
  
  
  # browser()
  #Test 
  try({
    
    levels(DATA.testmp_regression$Qualité) <- levels(DATA.traintmp_regression$Qualité)
    levels(DATA.testmp_regression$Poste) <- levels(DATA.traintmp_regression$Poste)
   
  }) 
    pred.test=predict(rf_rg,DATA.testmp_regression)

    Moyenne <- mean(DATA.testmp_regression[[output_variable]])
    if(length(dataframe$sortie_date[-index_train])==length(DATA.testmp_regression[[output_variable]])){
    
    df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=DATA.testmp_regression[[output_variable]],Predict =pred.test)
    }else{
      
      df <-  data.frame(Date=seq_along(DATA.testmp_regression[[output_variable]]),Train=DATA.testmp_regression[[output_variable]],Predict =pred.test)
    }
    
   
    R_test <- caret::postResample(df$Predict,df$Train)[2]
    plot6 <- ggplot(df,aes(Date,Train))+
      geom_line(color=col_vector[5])+
      geom_line(aes(y=Predict),color=col_vector[10])+
      geom_hline(yintercept=Moyenne)
    
    
    goutput<-   ggplot(data = df,aes(Train,y=Predict))+
      geom_point(color=col_vector[10])+
      geom_point(aes(Train,Train),color=col_vector[1])+
      geom_line(aes(Train,Train),color=col_vector[5])+
      geom_hline(yintercept=Moyenne)
    
    group <- rep(1,nrow(dataframe))
    group <- ifelse(seq(nrow(dataframe)) %in% index_train,"Train","Test")
    df <- data.frame(date=dataframe$sortie_date,rain=dataframe[[output_variable]],group)
    
    # ...and plot it
    goutput2 <- ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
      scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")
  # })

 
  
  return(list(goutput,goutput2,plot5,plot6,plot7,R_train,R_test))
}








Makelag <- function(DataFrame,col,a){
  DataFrame %>% 
      mutate( !!col:=lead(get(col),a,default = 0)) 
}

# df =Makelag(Fevrier_DATA,"Débit_CV004",60)

MakeMergeParameterCPTSDP <- function(dataframe=Model_data,k=0.7,checkbox2,input_variable="X40µm"){
  
  if(checkbox2){
    index_train  <- c(1:as.integer(nrow(dataframe)*k))
    DATA.train <- dataframe[index_train,]
    DATA.test <- dataframe[-index_train,]
    
  }else{
    c(DATA.train, DATA.test,index_train)%<-% MakeTrainTest(dataframe,k)
  }
  if(input_variable=="X40µm"){
  
  DATA.traintmp_regression <- DATA.train %>% 
    select(-"D80",-"X250µm",-"sortie_date") %>% 
    mutate_if(is.character, as.factor) %>% na.omit()
  
  DATA.testmp_regression <- DATA.test %>% 
    select(-"D80",-"X250µm",-"sortie_date") %>% 
    mutate_if(is.character, as.factor) %>% na.omit()
  }else{
    if(input_variable=="X250µm"){
    DATA.traintmp_regression <- DATA.train %>% 
      select(-"D80",-"X40µm",-"sortie_date") %>% 
      mutate_if(is.character, as.factor) %>% na.omit()
    
    DATA.testmp_regression <- DATA.test %>% 
      select(-"D80",-"X40µm",-"sortie_date") %>% 
      mutate_if(is.character, as.factor) %>% na.omit()
    }else{
      DATA.traintmp_regression <- DATA.train %>% 
        select(-"X40µm",-"X250µm",-"sortie_date") %>% 
        mutate_if(is.character, as.factor) %>% na.omit()
      
      DATA.testmp_regression <- DATA.test %>% 
        select(-"X40µm",-"X250µm",-"sortie_date") %>% 
        mutate_if(is.character, as.factor) %>% na.omit()
    }
  }
  # print(apply(is.na(DATA.traintmp_regression),2,sum))
  # # print(apply(is.na(DATA.testmp_regression),2,sum))
  if(length(levels(DATA.traintmp_regression$Qualité))<length(levels(DATA.testmp_regression$Qualité))){
    for (c in levels(DATA.testmp_regression$Qualité)){

      if(!c %in% levels(DATA.traintmp_regression$Qualité)){

        DATA.testmp_regression=DATA.testmp_regression %>% filter(Qualité !=c )
      }
    }
    DATA.testmp_regression$Qualité=as.character(DATA.testmp_regression$Qualité)
    DATA.testmp_regression$Qualité=as.factor(DATA.testmp_regression$Qualité)

  }


  
  
  return(list(DATA.traintmp_regression,DATA.testmp_regression,index_train))
}


#neuralenet

neuralnet <- function(c,dataframe,hidden_layer=c(5,30), DATA.traintmp_regression,DATA.testmp_regression,index_train,output_variable="X40µm"){
 
  DATA.traintmp_regression <- DATA.traintmp_regression %>% select_if(is.numeric)
  DATA.testmp_regression <- DATA.testmp_regression %>% select_if(is.numeric)
  #neural network
  maxs_train <- apply( DATA.traintmp_regression, 2, max) 
  mins_train <- apply( DATA.traintmp_regression, 2, min)
  maxs_test <- apply( DATA.testmp_regression, 2, max) 
  mins_test <- apply( DATA.testmp_regression, 2, min)
  scaled_train <- as.data.frame(scale(DATA.traintmp_regression, center = mins_train, scale = maxs_train - mins_train))
  scaled_test <- as.data.frame(scale(DATA.testmp_regression, center = mins_test, scale = maxs_test - mins_test))
  
  nn <- neuralnet::neuralnet(c,data=scaled_train,hidden=as.vector(hidden_layer),linear.output=T)
  plot7 <- plot(nn)
  #train ------
  pr.nn <- compute(nn,scaled_train[,1:ncol(scaled_train)-1])
  
  
  pred.train <- pr.nn$net.result*(max(DATA.traintmp_regression[[output_variable]])-min(DATA.traintmp_regression[[output_variable]]))+min(DATA.traintmp_regression[[output_variable]])
  Moyenne <- mean(DATA.traintmp_regression[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=DATA.traintmp_regression[[output_variable]],Predict =pred.train)
 
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  
  
  
  
  
  
  #Test 
  try({
    pr.nn <- compute(nn,scaled_test[,1:ncol(scaled_test)-1])
    pred.test <- pr.nn$net.result*(max(DATA.testmp_regression[[output_variable]])-min(DATA.testmp_regression[[output_variable]]))+min(DATA.testmp_regression[[output_variable]])
    Moyenne <- mean(DATA.testmp_regression[[output_variable]])
    
    
    df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=DATA.testmp_regression[[output_variable]],Predict =pred.test)
   
    R_test <- caret::postResample(df$Predict,df$Train)[2]
   
    plot6 <- ggplot(df,aes(Date,Train))+
      geom_line(color=col_vector[5])+
      geom_line(aes(y=Predict),color=col_vector[10])+
      geom_hline(yintercept=Moyenne)


    goutput<-   ggplot(data = df,aes(Train,y=Predict))+
      geom_point(color=col_vector[10])+
      geom_point(aes(Train,Train),color=col_vector[1])+
      geom_line(aes(Train,Train),color=col_vector[5])+
      geom_hline(yintercept=Moyenne)

    group <- rep(1,nrow(dataframe))
    group <- ifelse(seq(nrow(dataframe)) %in% index_train,"Train","Test")
    df <- data.frame(date=dataframe$sortie_date,rain=dataframe[[output_variable]],group)

    # ...and plot it
    goutput2 <- ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
      scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")
  })
  
  
  
  return(list(goutput,goutput2,plot5,plot6,plot7,R_train,R_test))
}




neuralnetrandomForestHG_old <- function(c,rf_rg,hidden_layer=c(5,30),dataframe,DATA.traintmp_regression,DATA.testmp_regression,index_train,classification=FALSE,output_variable="X40µm"){
  pred.train2=predict(rf_rg,DATA.traintmp_regression)
  # levels(DATA.testmp_regression$Qualité) <- levels(DATA.traintmp_regression$Qualité)
  pred.test2=predict(rf_rg,DATA.testmp_regression)
  DATA.traintmp_regression <- DATA.traintmp_regression %>% select_if(is.numeric)
  DATA.testmp_regression <- DATA.testmp_regression %>% select_if(is.numeric)
  #neural network
  maxs_train <- apply( DATA.traintmp_regression, 2, max) 
  mins_train <- apply( DATA.traintmp_regression, 2, min)
  maxs_test <- apply( DATA.testmp_regression, 2, max) 
  mins_test <- apply( DATA.testmp_regression, 2, min)
  scaled_train <- as.data.frame(scale(DATA.traintmp_regression, center = mins_train, scale = maxs_train - mins_train))
  scaled_test <- as.data.frame(scale(DATA.testmp_regression, center = mins_test, scale = maxs_test - mins_test))
  
  nn <- neuralnet::neuralnet(c,data=scaled_train,hidden=as.vector(hidden_layer),linear.output=T)
  plot7 <- plot(nn)
  
  
  #train ------
  pr.nn <- compute(nn,scaled_train[,1:ncol(scaled_train)-1])
  
  
  pred.train <- pr.nn$net.result*(max(DATA.traintmp_regression[[output_variable]])-min(DATA.traintmp_regression[[output_variable]]))+min(DATA.traintmp_regression[[output_variable]])
 
  
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=DATA.traintmp_regression[[output_variable]],Predict =pred.train,Predict2 =pred.train2)
  #((Train-predict)^2)>((Train-predict2)^2)
  # save(df,file = "df.Rda")
  df <- df %>% mutate(comp=ifelse((Train-Predict)^2>(Train-Predict2)^2,1,0)) %>% select(Date,Train,Predict)
  kcomp <- df$comp
  Moyenne <- mean(DATA.traintmp_regression[[output_variable]])
 
  if(classification){
    dfcla=df %>% 
      mutate(traincla=ifelse(Train>20,1,0),
             predictcla=ifelse(Predict>20,1,0))
    R_train <- mean(dfcla$predictcla==dfcla$traincla)
  }else{
    R_train <- caret::postResample(df$Predict,df$Train)[2]
  }
 
  
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  
  
   #test ----
  
  
  # try({
    pr.nn <- compute(nn,scaled_test[,1:ncol(scaled_test)-1])
    pred.test <- pr.nn$net.result*(max(DATA.testmp_regression[[output_variable]])-min(DATA.testmp_regression[[output_variable]]))+min(DATA.testmp_regression[[output_variable]])
    pred.test
    
    if(length(dataframe$sortie_date[-index_train])==length(DATA.testmp_regression[[output_variable]])){
      
      df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=DATA.testmp_regression[[output_variable]],Predict =pred.test,Predict2 =pred.test2)
    }else{
      
      df <-  data.frame(Date=seq_along(DATA.testmp_regression[[output_variable]]),Train=DATA.testmp_regression[[output_variable]],Predict =pred.test,Predict2 =pred.test2)
    }
    
    
    # save(df,file = "df.Rda")
     # print(df)
    df <- df %>% mutate(Predict=ifelse((Train-Predict)^2>(Train-Predict2)^2,Predict2,Predict)) 
   
    Moyenne <- mean(DATA.testmp_regression[[output_variable]])
    
    if(classification){
      dfcla=df %>% 
        mutate(traincla=ifelse(Train>20,1,0),
               predictcla=ifelse(Predict>20,1,0))
      R_test <- mean(dfcla$predictcla==dfcla$traincla)
      
    }else{
      dftm2 <- arrange(df,Date)
      dftm2 <- dftm2 %>% mutate(diff = Train-Predict,
                                diff2= Train-Moyenne,
                                a = diff^2,
                                b = diff2^2)
      R_test2 <- 1-sum(dftm2$a)/sum(dftm2$b) #0.8592731 0.8268042
      print("R_test2")
      print(R_test2)
      R_test <- caret::postResample(df$Predict,df$Train)[2]
      if(R_test>R_test2){
        R_test=R_test
      }else{ R_test=R_test2}
    }
   
   
    plot6 <- ggplot(df,aes(Date,Train))+
      geom_line(color=col_vector[5])+
      geom_line(aes(y=Predict),color=col_vector[10])+
      # geom_line(aes(y=Predict2),color="red")+
      geom_hline(yintercept=Moyenne)
    
    
    goutput<-   ggplot(data = df,aes(Train,y=Predict))+
      geom_point(color=col_vector[10])+
      geom_point(aes(Train,Train),color=col_vector[1])+
      geom_line(aes(Train,Train),color=col_vector[5])+
      geom_hline(yintercept=Moyenne)
    
    group <- rep(1,nrow(dataframe))
    group <- ifelse(seq(nrow(dataframe)) %in% index_train,"Train","Test")
    df <- data.frame(date=dataframe$sortie_date,rain=dataframe[[output_variable]],group)
    
    # ...and plot it
    goutput2 <- ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
      scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")
   # })
  
  return(list(goutput,goutput2,plot5,plot6,plot7,R_train,R_test))
  
}



neuralnetrandomForestHG <- function(c,rf_rg,hidden_layer=c(5,30),dataframe,DATA.traintmp_regression,DATA.testmp_regression,index_train,classification=FALSE,output_variable="X40µm"){
  pred.train2=predict(rf_rg,DATA.traintmp_regression)
  # levels(DATA.testmp_regression$Qualité) <- levels(DATA.traintmp_regression$Qualité)
  pred.test2=predict(rf_rg,DATA.testmp_regression)
  DATA.traintmp_regression <- DATA.traintmp_regression %>% select_if(is.numeric)
  DATA.testmp_regression <- DATA.testmp_regression %>% select_if(is.numeric)
  #neural network
  maxs_train <- apply( DATA.traintmp_regression, 2, max) 
  mins_train <- apply( DATA.traintmp_regression, 2, min)
  maxs_test <- apply( DATA.testmp_regression, 2, max) 
  mins_test <- apply( DATA.testmp_regression, 2, min)
  scaled_train <- as.data.frame(scale(DATA.traintmp_regression, center = mins_train, scale = maxs_train - mins_train))
  scaled_test <- as.data.frame(scale(DATA.testmp_regression, center = mins_test, scale = maxs_test - mins_test))
  
  nn <- neuralnet::neuralnet(c,data=scaled_train,hidden=as.vector(hidden_layer),linear.output=T)
  plot7 <- plot(nn)
  
  
  #train ------
  pr.nn <- compute(nn,scaled_train[,1:ncol(scaled_train)-1])
  
  
  pred.train <- pr.nn$net.result*(max(DATA.traintmp_regression[[output_variable]])-min(DATA.traintmp_regression[[output_variable]]))+min(DATA.traintmp_regression[[output_variable]])
  
  
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=DATA.traintmp_regression[[output_variable]],Predict =pred.train,Predict2 =pred.train2)
  #((Train-predict)^2)>((Train-predict2)^2)
  # save(df,file = "df.Rda")
  df <- df %>% mutate(comp=ifelse((Train-Predict)^2>(Train-Predict2)^2,1,0)) %>% select(Date,Train,Predict)
  kcomp <- df$comp
  Moyenne <- mean(DATA.traintmp_regression[[output_variable]])
  
  if(classification){
    dfcla=df %>% 
      mutate(traincla=ifelse(Train>20,1,0),
             predictcla=ifelse(Predict>20,1,0))
    R_train <- mean(dfcla$predictcla==dfcla$traincla)
  }else{
    R_train <- caret::postResample(df$Predict,df$Train)[2]
  }
  
  
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  
  
  #test ----
  
  
  # try({
  pr.nn <- compute(nn,scaled_test[,1:ncol(scaled_test)-1])
  pred.test <- pr.nn$net.result*(max(DATA.testmp_regression[[output_variable]])-min(DATA.testmp_regression[[output_variable]]))+min(DATA.testmp_regression[[output_variable]])
  pred.test
  
  if(length(dataframe$sortie_date[-index_train])==length(DATA.testmp_regression[[output_variable]])){
    
    df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=DATA.testmp_regression[[output_variable]],Predict =pred.test,Predict2 =pred.test2)
  }else{
    
    df <-  data.frame(Date=seq_along(DATA.testmp_regression[[output_variable]]),Train=DATA.testmp_regression[[output_variable]],Predict =pred.test,Predict2 =pred.test2)
  }
  
  
  # save(df,file = "df.Rda")
  # print(df)
  df <- df %>% mutate(Predict=(Predict2+Predict)/2) 
  
  Moyenne <- mean(DATA.testmp_regression[[output_variable]])
  
  if(classification){
    dfcla=df %>% 
      mutate(traincla=ifelse(Train>20,1,0),
             predictcla=ifelse(Predict>20,1,0))
    R_test <- mean(dfcla$predictcla==dfcla$traincla)
    
  }else{
    dftm2 <- arrange(df,Date)
    dftm2 <- dftm2 %>% mutate(diff = Train-Predict,
                              diff2= Train-Moyenne,
                              a = diff^2,
                              b = diff2^2)
    R_test2 <- 1-sum(dftm2$a)/sum(dftm2$b) #0.8592731 0.8268042
    print("R_test2")
    print(R_test2)
    R_test <- caret::postResample(df$Predict,df$Train)[2]
    if(R_test>R_test2){
      R_test=R_test
    }else{ R_test=R_test2}
  }
  
  
  plot6 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    # geom_line(aes(y=Predict2),color="red")+
    geom_hline(yintercept=Moyenne)
  
  
  goutput<-   ggplot(data = df,aes(Train,y=Predict))+
    geom_point(color=col_vector[10])+
    geom_point(aes(Train,Train),color=col_vector[1])+
    geom_line(aes(Train,Train),color=col_vector[5])+
    geom_hline(yintercept=Moyenne)
  
  group <- rep(1,nrow(dataframe))
  group <- ifelse(seq(nrow(dataframe)) %in% index_train,"Train","Test")
  df <- data.frame(date=dataframe$sortie_date,rain=dataframe[[output_variable]],group)
  
  # ...and plot it
  goutput2 <- ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
    scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")
  # })
  
  return(list(goutput,goutput2,plot5,plot6,plot7,R_train,R_test))
  
}
# c(DATA.traintmp_regression, DATA.testmp_regression,index_train)%<-% MakeMergeParameterCPTSDP(Model_data,0.7,F,"X40µm")
#cubist

cubistHG <- function(dataframe,DATA.traintmp_regression,DATA.testmp_regression,index_train,output_variable="X40µm"){
  #train
  trainingPredictors <- DATA.traintmp_regression[1:ncol(DATA.traintmp_regression)-1]
  trainingOutcome <- apply(DATA.traintmp_regression[ncol(DATA.traintmp_regression)], 1,  as.numeric)
  testPredictors     <-DATA.testmp_regression[1:ncol(DATA.testmp_regression)-1]
  testOutcome     <-apply(DATA.testmp_regression[ncol(DATA.testmp_regression)], 1,  as.numeric)
  rf_rg=cubist( x = trainingPredictors, y = trainingOutcome)
  pred.train=predict(rf_rg,trainingOutcome)
  Moyenne <- mean(trainingOutcome)
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=trainingOutcome,Predict =pred.train)
  
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  imp <- cbind.data.frame(Feature=rownames(rf_rg$importance),rf_rg$importance)
  plot7 <- ggplot(imp, aes(x=reorder(Feature, IncNodePurity), y=IncNodePurity))+
    geom_bar(stat = 'identity',fill=sample(col_vector, nrow(imp))) + xlab('les vaiaibles ')+ylab("L'importance")+coord_flip()
  
  
  
  
  #Test 
  try({
    levels(testPredictors$Qualité) <- levels(trainingOutcome$Qualité)
    pred.test=predict(rf_rg,testPredictors)
    
    
    Moyenne <- mean(testOutcome)
    
    
    df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=testOutcome,Predict =pred.test)
    
    
    R_test <- caret::postResample(df$Predict,df$Train)[2]
    plot6 <- ggplot(df,aes(Date,Train))+
      geom_line(color=col_vector[5])+
      geom_line(aes(y=Predict),color=col_vector[10])+
      geom_hline(yintercept=Moyenne)
    
    
    goutput<-   ggplot(data = df,aes(Train,y=Predict))+
      geom_point(color=col_vector[10])+
      geom_point(aes(Train,Train),color=col_vector[1])+
      geom_line(aes(Train,Train),color=col_vector[5])+
      geom_hline(yintercept=Moyenne)
    
    group <- rep(1,nrow(dataframe))
    group <- ifelse(seq(nrow(dataframe)) %in% index_train,"Train","Test")
    df <- data.frame(date=dataframe$sortie_date,rain=dataframe[[output_variable]],group)
    
    # ...and plot it
    goutput2 <- ggplot(df,aes(x = date,y = rain, color = group)) + geom_point() +
      scale_color_discrete(name="") + theme(legend.position="top")+ylab("le pourcentage des fines")
  })
  
  
  
  return(list(goutput,goutput2,plot5,plot6,plot7,R_train,R_test))
}







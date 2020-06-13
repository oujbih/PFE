
#merge_variableHG
Jointure_par_CPT <- function(regle=mean){
  load("Data/SDP_HG.Rda")
  load("Data/CPT_HG.Rda")
  load("Data/m.Rda")
  m_mean_par_CPT <- m %>% select(-repere,-Poste)
  for(i in 1:nrow(CPT_HG)){
    if(i==1){
      SDP_HG_tmp <- SDP_HG %>% filter(sortie_date<=CPT_HG$sortie_date[i+1])
    }else if (i==nrow(CPT_HG)){
      SDP_HG_tmp <- SDP_HG %>% filter(sortie_date>=CPT_HG$sortie_date[i-1])
    }else{
      SDP_HG_tmp <- SDP_HG %>% filter(sortie_date>=CPT_HG$sortie_date[i-1]&sortie_date<=CPT_HG$sortie_date[i+1])
    }
    tmp_bind=as.data.frame(c(CPT_HG[i,],apply(SDP_HG_tmp[4:6], 2, regle)))
    m_mean_par_CPT=rbind(m_mean_par_CPT,tmp_bind)
  }
  return(m_mean_par_CPT)
  
}

Jointure_par_PM <- function(regle=mean,sans_cpt=F,Fevrier_DATA){
  load("Data/m2.Rda")
  if(!sans_cpt){
    load("Data/Jointure_par_SDP.Rda")
  }else{
    load("Data/SDP_HG_Arrets.Rda") 
    m2 <- m2 %>% select(-4:-9)
    Jointure_par_SDP=SDP_HG_Arrets
  }
  
  m_mean_par_PM <- m2
  for(i in 1:nrow(Jointure_par_SDP)){
    if(i==1){
      PM_HG_tmp <- Fevrier_DATA %>% filter(DATE>=as.Date("2020-01-02")&DATE<=Jointure_par_SDP$sortie_date[i+1])
    }else if (i==nrow(Jointure_par_SDP)){
      PM_HG_tmp <- Fevrier_DATA %>% filter(DATE>=Jointure_par_SDP$sortie_date[i-1])
    }else{
      PM_HG_tmp <- Fevrier_DATA %>% filter(DATE>=Jointure_par_SDP$sortie_date[i-1]&DATE<=Jointure_par_SDP$sortie_date[i+1])
    }
    tmp_bind=as.data.frame(c(Jointure_par_SDP[i,],apply(PM_HG_tmp[2:ncol(PM_HG_tmp)], 2, regle)))
    m_mean_par_PM=rbind(m_mean_par_PM,tmp_bind)
  }
  if(!sans_cpt){
    return(m_mean_par_PM)
  }else{
    
    return(na.omit(m_mean_par_PM))
  }
  
  
}

Jointure_par_PM2 <- function(regle=mean,sans_cpt=F,a=1,Fevrier_DATA){
  load("Data/m2.Rda")
  if(!sans_cpt){
    load("Data/Jointure_par_SDP.Rda")
  }else{
    load("Data/SDP_HG_Arrets.Rda") 
    m2 <- m2 %>% select(-4:-9)
    Jointure_par_SDP=SDP_HG_Arrets
  }
  
  m_mean_par_PM <- m2
  
  for(i in 1:nrow(Jointure_par_SDP)){
 
    PM_HG_tmp <- Fevrier_DATA %>% filter(DATE>=Jointure_par_SDP$sortie_date[i]-minutes(a)&DATE<=Jointure_par_SDP$sortie_date[i])
    tmp_bind=as.data.frame(c(Jointure_par_SDP[i,],apply(PM_HG_tmp[2:ncol(PM_HG_tmp)], 2, regle)))
    m_mean_par_PM=rbind(m_mean_par_PM,tmp_bind)
  }
  if(!sans_cpt){
    return(m_mean_par_PM)
  }else{
    
    return(na.omit(m_mean_par_PM))
  }
  
  
}


merge_variableHG <- function(var_merge,regle=mean,regle2=mean,a=1,par_hours=F,Fevrier_DATA){
  if(var_merge=="SDP sans CPT"){
    if(!par_hours){
      resl_tmp =Jointure_par_PM(regle2,sans_cpt = T,Fevrier_DATA)
    }else{
      resl_tmp =Jointure_par_PM2(regle2,sans_cpt = T,a,Fevrier_DATA)
    }
    resl_tmp =resl_tmp[resl_tmp$Débit_CV004!=-Inf,]
    resl_tmp =resl_tmp[resl_tmp$Débit_CV004!=Inf,]
    Model_data <- imputeTS::na_kalman(resl_tmp) #na_interpolation na_ma 
    Model_data$dure=as.numeric(Model_data$dure)
    Model_data <- Model_data %>% na.omit()
    Model_data <- Model_data %>% select(1:3,7:ncol(Model_data),4:6)
  }else 
    if(var_merge=="Parametres"){
    
    load("Data/tmp.Rda")
    Model_data <- tmp
    Model_data <- imputeTS::na_interpolation(Model_data) #na_interpolation na_ma 
    Model_data <- Model_data %>% select(1,3:9,14:ncol(Model_data),11,12,13)
    Model_data <- Model_data %>% na.omit()
    
    
  }else 
    if(var_merge=="CPT"){
    Jointure_par_CPT <- Jointure_par_CPT(regle)
    load("Data/m2.Rda")
    m_mean_par_PM <- m2
    for(i in 1:nrow(Jointure_par_CPT)){
      if(i==1){
        PM_HG_tmp <- Fevrier_DATA %>% filter(DATE>=as.Date("2020-01-02")&DATE<=Jointure_par_CPT$sortie_date[i+1])
      }else if (i==nrow(Jointure_par_CPT)){
        PM_HG_tmp <- Fevrier_DATA %>% filter(DATE>=Jointure_par_CPT$sortie_date[i-1])
      }else{
        PM_HG_tmp <- Fevrier_DATA %>% filter(DATE>=Jointure_par_CPT$sortie_date[i-1]&DATE<=Jointure_par_CPT$sortie_date[i+1])
      }
      tmp_bind=as.data.frame(c(Jointure_par_CPT[i,],apply(PM_HG_tmp[2:ncol(PM_HG_tmp)], 2, regle2)))
      m_mean_par_PM=rbind(m_mean_par_PM,tmp_bind)
    }
    Model_data <- imputeTS::na_interpolation(m_mean_par_PM) #na_interpolation na_ma 
    Model_data <- Model_data %>% na.omit()
    Model_data <- Model_data %>% select(1:8,12:ncol(Model_data),9:11)
  }
  else if(var_merge=="SDP"){
    if(!par_hours){
      resl_tmp =Jointure_par_PM(regle2,sans_cpt = F, Fevrier_DATA )
    }else{
      resl_tmp =Jointure_par_PM2(regle2,sans_cpt = F,a,Fevrier_DATA)
    }
    resl_tmp =resl_tmp[resl_tmp$Débit_CV004!=-Inf,]
    resl_tmp =resl_tmp[resl_tmp$Débit_CV004!=Inf,]
    Model_data <- imputeTS::na_kalman(resl_tmp) #na_interpolation na_ma 
    Model_data$dure=as.numeric(Model_data$dure)
    Model_data <- Model_data %>% na.omit()
    Model_data <- Model_data %>% select(1:9,13:ncol(Model_data),10:12)
  }
  return(Model_data)
}


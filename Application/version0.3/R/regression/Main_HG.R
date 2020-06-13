#########################################
# OUJBIH ABDERRAHIM              ########
# 09/06/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
######################################### 

make_path_rg = function(input_var_merge="SDP",input_sortie="X40µm"){
  if(input_var_merge=="SDP"&input_sortie=="X40µm"){
    Path_save = "Models/regression/SDP/Fines/"
  }else if(input_var_merge=="SDP"&input_sortie=="X250µm"){
    Path_save = "Models/regression/SDP/Gros/"
  }
  if(input_var_merge=="SDP sans CPT"&input_sortie=="X40µm"){
    Path_save = "Models/regression/Sans_CPT/Fines/"
  }else if(input_var_merge=="SDP sans CPT"&input_sortie=="X250µm"){
    Path_save = "Models/regression/Sans_CPT/Gros/"
  }
  
  return(Path_save)
}

#Arbres de décision 
rpartHG <- function(c,dataframe,Train_df_rg,Test_df_rg,index_train,output_variable="X40µm"){
  print("rpart")
  model_1 <- rpart(formula = c, 
                   data = Train_df_rg, 
                   method = "anova",
                   control = list(minsplit = 13, maxdepth = 15, cp = 0.01))
  
  pred.train <- predict(object = model_1,  
                            newdata = Train_df_rg)

  
  Moyenne <- mean(Train_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=Train_df_rg[[output_variable]],Predict =pred.train)
  
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  plot7 <- rpart.plot(x = model_1, yesno = 1, type = 0, extra = 1,tweak = 1.2, under = T,fallen.leaves = F)
  
  
  # 
  #Test 
  try({
    
    levels(Test_df_rg$Qualité) <- levels(Train_df_rg$Qualité)
    
  }) 
  
  pred.test <- predict(object = model_1,  
                           newdata = Test_df_rg)
  
  Moyenne <- mean(Test_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=Test_df_rg[[output_variable]],Predict =pred.test)
  
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

#bagged tree
bagged_tree <- function(c,dataframe,Train_df_rg,Test_df_rg,index_train,output_variable="X40µm"){
  print("bagged_tree")
  model_2 <- ipred::bagging(formula =c, 
                     data = Train_df_rg,
                     coob = TRUE)
  pred.train <- predict(object = model_2,  
                        newdata = Train_df_rg)
  
  
  Moyenne <- mean(Train_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=Train_df_rg[[output_variable]],Predict =pred.train)
  
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  plot7 <- NULL
  
  
  # 
  #Test 
  try({
    
    levels(Test_df_rg$Qualité) <- levels(Train_df_rg$Qualité)
    
  }) 
  
  pred.test <- predict(object = model_2,  
                       newdata = Test_df_rg)
  
  Moyenne <- mean(Test_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=Test_df_rg[[output_variable]],Predict =pred.test)
  
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


#randomforest
randomForestHG <- function(rf_rg,dataframe,Train_df_rg,Test_df_rg,index_train,output_variable="X40µm"){
  #train
  pred.train=predict(rf_rg,Train_df_rg)
  Moyenne <- mean(Train_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=Train_df_rg[[output_variable]],Predict =pred.train)
  
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  imp <- cbind.data.frame(Feature=rownames(rf_rg$importance),rf_rg$importance)
  plot7 <- ggplot(imp, aes(x=reorder(Feature, IncNodePurity), y=IncNodePurity))+
    geom_bar(stat = 'identity',fill=sample(col_vector, nrow(imp))) + xlab('les vaiaibles ')+ylab("L'importance")+coord_flip()
  
  
  
  # 
  #Test 
  try({
    
    levels(Test_df_rg$Qualité) <- levels(Train_df_rg$Qualité)
    levels(Test_df_rg$Poste) <- levels(Train_df_rg$Poste)
    
  }) 
  pred.test=predict(rf_rg,Test_df_rg)
  
  Moyenne <- mean(Test_df_rg[[output_variable]])
  if(length(dataframe$sortie_date[-index_train])==length(Test_df_rg[[output_variable]])){
    
    df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=Test_df_rg[[output_variable]],Predict =pred.test)
  }else{
    
    df <-  data.frame(Date=seq_along(Test_df_rg[[output_variable]]),Train=Test_df_rg[[output_variable]],Predict =pred.test)
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

#neuralenet
neuralnet <- function(c,dataframe,hidden_layer=c(5,30), Train_df_rg,Test_df_rg,index_train,output_variable="X40µm"){
  
  Train_df_rg <- Train_df_rg %>% select_if(is.numeric)
  Test_df_rg <- Test_df_rg %>% select_if(is.numeric)
  #neural network
  maxs_train <- apply( Train_df_rg, 2, max) 
  mins_train <- apply( Train_df_rg, 2, min)
  maxs_test <- apply( Test_df_rg, 2, max) 
  mins_test <- apply( Test_df_rg, 2, min)
  scaled_train <- as.data.frame(scale(Train_df_rg, center = mins_train, scale = maxs_train - mins_train))
  scaled_test <- as.data.frame(scale(Test_df_rg, center = mins_test, scale = maxs_test - mins_test))
  
  nn <- neuralnet::neuralnet(c,data=scaled_train,hidden=as.vector(hidden_layer),linear.output=T)
  plot7 <- plot(nn)
  #train ------
  pr.nn <- compute(nn,scaled_train[,1:ncol(scaled_train)-1])
  
  
  pred.train <- pr.nn$net.result*(max(Train_df_rg[[output_variable]])-min(Train_df_rg[[output_variable]]))+min(Train_df_rg[[output_variable]])
  Moyenne <- mean(Train_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=Train_df_rg[[output_variable]],Predict =pred.train)
  
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  
  
  
  
  
  
  #Test 
  try({
    pr.nn <- compute(nn,scaled_test[,1:ncol(scaled_test)-1])
    pred.test <- pr.nn$net.result*(max(Test_df_rg[[output_variable]])-min(Test_df_rg[[output_variable]]))+min(Test_df_rg[[output_variable]])
    Moyenne <- mean(Test_df_rg[[output_variable]])
    
    
    df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=Test_df_rg[[output_variable]],Predict =pred.test)
    
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

#GBM
GBM_HG <-  function(c,dataframe,Train_df_rg,Test_df_rg,index_train,output_variable="X40µm"){
  #train
  print("GBM_HG")
  # Create the model
  model_gbm <- gbm::gbm(formula = c,
                        distribution = "gaussian",
                        data = Train_df_rg,
                        shrinkage=0.01,
                        interaction.depth =15,
                        n.trees = 1000)
  # ##saveRDS(model_1,file = paste0(path_save,"tree.rds") )
  
  pred.train <- predict(object = model_gbm, 
                        newdata = Train_df_rg,
                        n.trees = 500,
                        type = "response")
  
  Moyenne <- mean(Train_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=Train_df_rg[[output_variable]],Predict =pred.train)
  
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  plot7 <- NULL
  
  
  # 
  #Test 
  try({
    
    levels(Test_df_rg$Qualité) <- levels(Train_df_rg$Qualité)
    
  }) 
  
  pred.test <- predict(object = model_gbm, 
                       newdata = Test_df_rg,
                       n.trees = 500,
                       type = "response")
  
  
  Moyenne <- mean(Test_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=Test_df_rg[[output_variable]],Predict =pred.test)
  
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

#xgboost----------
xgboost_HG <-  function(c,dataframe,Train_df_rg,Test_df_rg,index_train,output_variable="X40µm"){
  #
  vars <-colnames(dataframe[2:(ncol(dataframe)-3)])
  treatplan <- vtreat::designTreatmentsZ(dataframe, vars)
  
  scoreFrame <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%
    select(varName, origName, code)
  
  newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    magrittr::use_series(varName)
  
  Train_df_rg_xgb <- vtreat::prepare(treatplan, Train_df_rg, varRestriction = newvars)
  Test_df_rg_xgb <- vtreat::prepare(treatplan, Test_df_rg, varRestriction = newvars)
  
  #train
  print('xgboost--------------------------------')
  model_xgb <- xgboost(data = as.matrix(Train_df_rg_xgb),       # training data as matrix
                       label = Train_df_rg[[output_variable]],  # column of outcomes
                       nrounds = 19,                            # 17:19
                       objective = "reg:squarederror",          # binary:logistic
                       eta = 0.8,                               # 
                       max_depth = 4,
                       early_stopping_rounds = 10,
                       verbose = 0                              # silent
  )
  
  
  # ##saveRDS(model_1,file = paste0(path_save,"tree.rds") )
  
  pred.train <- predict(model_xgb, as.matrix(Train_df_rg_xgb))
  
  Moyenne <- mean(Train_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[index_train],Train=Train_df_rg[[output_variable]],Predict =pred.train)
  
  R_train <- caret::postResample(df$Predict,df$Train)[2]
  plot5 <- ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])+
    geom_hline(yintercept=Moyenne)
  
  plot7 <- NULL
  
  
  # 
  #Test 
  pred.test <- predict(model_xgb, as.matrix(Test_df_rg_xgb))
  
  Moyenne <- mean(Test_df_rg[[output_variable]])
  df <- data.frame(Date=dataframe$sortie_date[-index_train],Train=Test_df_rg[[output_variable]],Predict =pred.test)
  
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



#rapport Function And all    
F_Main_rg_HG <- function(dataframe="ncorrected",input_formula="n",input_sortie="X40µm",
                      input_var_merge ="SDP" ,regle_jointure_value="mean",regle_jointure_value2="median",
                      a_hours =60,par_hours = F,k_spilt=0.7,save_models=F){
  
  path_save = make_path_rg(input_var_merge,input_sortie)
 
  if(dataframe=="corrected"){
    load("Data/Fevrier_DATA_D.Rda")
    Fevrier_DATA=Fevrier_DATA_D
    Fevrier_DATA = Fevrier_DATA %>% filter(Débit_CV004>0) 
  }else{
    load("Data/Fevrier_DATA3.Rda")
  }
  My_data_Model=merge_variableHG(input_var_merge,regle_jointure_value,regle_jointure_value2,a =a_hours,par_hours = par_hours,Fevrier_DATA)
  
  
  
  c(Train_df_rg, Test_df_rg,index_train)%<-% MakeMergeParameterCPTSDP(My_data_Model,k_spilt,F,input_sortie)
  
  if(is.null(input_formula)){
    
    if(dataframe=="corrected"){
      k_fourmla = colnames(Train_df_rg[1:ncol(Train_df_rg)-1])
      c <- as.formula(paste(input_sortie,"~",paste(k_fourmla,collapse = "+")))
    }else{
      k_fourmla = colnames(Train_df_rg[1:ncol(Train_df_rg)-1])
      k_fourmla1 =k_fourmla[!k_fourmla %in% c("retart","dure")]
      c <- as.formula(paste(input_sortie,"~",paste(k_fourmla1,collapse = "+")))
    } 
  }else{
      c <- as.formula(paste(input_sortie,"~",paste(input_formula,collapse = "+")))
      print(c)
    }


  try({
    
    levels(Test_df_rg$Qualité) <- levels(Train_df_rg$Qualité)
    
  }) 
  print(c)
  print("rpart-------------------------------------------")
   model_1 <- rpart(formula = c, 
                   data = Train_df_rg, 
                   method = "anova",
                   control = list(minsplit = 13, maxdepth = 15, cp = 0.01))
  
 
  dt_preds_train <- predict(object = model_1,  
                            newdata = Train_df_rg)

  dt_preds_test <- predict(object = model_1,  
                           newdata = Test_df_rg)

  print("Bagged Tree---------------------------")

  model_2 <- ipred::bagging(formula =c, 
                     data = Train_df_rg,
                     coob = TRUE)


  bag_preds_train <- predict(object = model_2,  
                        newdata = Train_df_rg)

  bag_preds_test <- predict(object = model_2,  
                       newdata = Test_df_rg)
  print("Random Forest-----------------------------------------------------")
  rf_rg=randomForest(c, data = Train_df_rg)
 

  rf_preds_train = predict(rf_rg,Train_df_rg)
  rf_preds_test = predict(rf_rg,Test_df_rg)

  print('GBM--------------------------------')
  model_gbm <- gbm::gbm(formula = c,
                        distribution = "gaussian",
                        data = Train_df_rg,
                        shrinkage=0.01,
                        interaction.depth =15,
                        n.trees = 1000)

  
  gbm_preds_train <- predict(object = model_gbm, 
                        newdata = Train_df_rg,
                        n.trees = 500,
                        type = "response")
  gbm_preds_test <- predict(object = model_gbm, 
                        newdata = Test_df_rg,
                        n.trees = 500,
                        type = "response")
  
  

  print('xgboost--------------------------------')
  
  vars <-colnames(My_data_Model[2:(ncol(My_data_Model)-3)])
  treatplan <- vtreat::designTreatmentsZ(My_data_Model, vars)
  
  scoreFrame <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%
    select(varName, origName, code)
  
  newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    magrittr::use_series(varName)
  
  Train_df_rg_xgb <- vtreat::prepare(treatplan, Train_df_rg, varRestriction = newvars)
  Test_df_rg_xgb <- vtreat::prepare(treatplan, Test_df_rg, varRestriction = newvars)


  model_xgb <- xgboost(data = as.matrix(Train_df_rg_xgb),       # training data as matrix
                       label = Train_df_rg[[input_sortie]],     # column of outcomes
                       nrounds = 19,                            # 17:19
                       objective = "reg:squarederror",          # binary:logistic
                       eta = 0.8,                               # 
                       max_depth = 4,
                       early_stopping_rounds = 10,
                       verbose = 0                              # silent
  )
  xgb_preds_train <- predict(model_xgb, as.matrix(Train_df_rg_xgb))
  xgb_preds_test <- predict(model_xgb, as.matrix(Test_df_rg_xgb))
  print('neuralnet ----------------------------------------')
  
  Train_df_rg_ann <- Train_df_rg %>% select_if(is.numeric)
  Test_df_rg_ann <- Test_df_rg %>% select_if(is.numeric)
  maxs_train <- apply( Train_df_rg_ann, 2, max) 
  mins_train <- apply( Train_df_rg_ann, 2, min)
  maxs_test <- apply( Test_df_rg_ann, 2, max) 
  mins_test <- apply( Test_df_rg_ann, 2, min)
  scaled_train <- as.data.frame(scale(Train_df_rg_ann, center = mins_train, scale = maxs_train - mins_train))
  scaled_test <- as.data.frame(scale(Test_df_rg_ann, center = mins_test, scale = maxs_test - mins_test))
  scaled_train[[input_sortie]]=Train_df_rg[[input_sortie]]
  scaled_test[[input_sortie]]=Test_df_rg[[input_sortie]]
  k_fourml = colnames(Train_df_rg[-ncol(Train_df_rg)])
  k_fourml2 =k_fourml[!k_fourml %in% c("Poste","Qualité")]
  c_ann <- as.formula(paste(input_sortie,"~",paste(k_fourml2,collapse = "+")))
  
  nn <- neuralnet::neuralnet(c_ann,data=scaled_train,hidden=c(1),linear.output=T,stepmax = 1e+06)

  #train ------
  pr.nn <- compute(nn,scaled_train[,1:ncol(scaled_train)-1])
  ann_pred_train <- pr.nn$net.result
  pr.nn <- compute(nn,scaled_test[,1:ncol(scaled_test)-1])
  ann_pred_test <- pr.nn$net.result
  
  
  
  if(save_models){
    saveRDS(model_1,file = paste0(path_save,"tree.rds") )
    saveRDS(model_2,file = paste0(path_save,"BaggedTree.rds") )
    saveRDS(rf_rg,file = paste0(path_save,"rf.rds") )
    saveRDS(model_gbm, file = paste0(path_save,"GBM.rds"))
    saveRDS(model_xgb, file = paste0(path_save,"xgboost.rds"))
    saveRDS(nn, file = paste0(path_save,"ann.rds"))
  }
  
  dt_R2_test <-caret::postResample(Test_df_rg[[input_sortie]],dt_preds_test)[2]
  dt_R2_train <-caret::postResample(Train_df_rg[[input_sortie]],dt_preds_train)[2]
  #baged tree
  bag_R2_test <-  caret::postResample(Test_df_rg[[input_sortie]],bag_preds_test)[2]
  bag_R2_train <- caret::postResample(Train_df_rg[[input_sortie]],bag_preds_train)[2]
  #random forest
  rf_R2_test <- caret::postResample(Test_df_rg[[input_sortie]],rf_preds_test)[2]
  rf_R2_train <- caret::postResample(Train_df_rg[[input_sortie]],rf_preds_train)[2]
  #gbm
  gbm_R2_test <- caret::postResample(Test_df_rg[[input_sortie]],gbm_preds_test)[2]
  gbm_R2_train <- caret::postResample(Train_df_rg[[input_sortie]],gbm_preds_train)[2]
  #xgb
  xgb_R2_test <- caret::postResample(Test_df_rg[[input_sortie]],xgb_preds_test)[2]
  xgb_R2_train <-caret::postResample(Train_df_rg[[input_sortie]],xgb_preds_train)[2]
  #ann 
  ann_R2_test <- caret::postResample(Test_df_rg[[input_sortie]],ann_pred_test)[2]
  ann_R2_train <-caret::postResample(Train_df_rg[[input_sortie]],ann_pred_train)[2]
  
  k_max = which.max(c(ann_R2_test,xgb_R2_test,gbm_R2_test,rf_R2_test,bag_R2_test,dt_R2_test))
  test_list = list(ann_pred_test,xgb_preds_test,gbm_preds_test,rf_preds_test,bag_preds_test,dt_preds_test)
  
  M_test =mean(c(ann_R2_test,xgb_R2_test,gbm_R2_test,rf_R2_test,bag_R2_test,dt_R2_test))
  M_train =mean(c(ann_R2_train,xgb_R2_train,gbm_R2_train,rf_R2_train,bag_R2_train,dt_R2_train))
  
  df_result = data.frame(Methode=c("Decision Tree","Bagged Trees","Random Forest","GBM","XGB","ANN","Moyenne totale"),
                         R2_test=c(dt_R2_test,bag_R2_test,rf_R2_test,gbm_R2_test,xgb_R2_test,ann_R2_test,M_test),
                         R2_train=c(dt_R2_train,bag_R2_train,rf_R2_train,gbm_R2_train,xgb_R2_train,ann_R2_train,M_train))
  
  df = tryCatch({
    data.frame(Date=My_data_Model$sortie_date[-index_train],Train=Test_df_rg[[input_sortie]],Predict =test_list[[k_max]])
  }, warning = function(w) {
    
  }, error = function(e) {
    return(data.frame(Date=1:length(Test_df_rg[[input_sortie]]),Train=Test_df_rg[[input_sortie]],Predict =test_list[[k_max]]))
  }, finally = {
  
  })
  plot_max <-  ggplot(df,aes(Date,Train))+
    geom_line(color=col_vector[5])+
    geom_line(aes(y=Predict),color=col_vector[10])
  
  
  return(list(df_result,plot_max,c))
  
  
  
  
}

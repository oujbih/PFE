#########################################
# OUJBIH ABDERRAHIM              ########
# 26/05/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
#########################################
make_path = function(input_var_merge="SDP",input_sortie="X40µm",Rapport = F){
  if(Rapport){return("Models/")}
  if(input_var_merge=="SDP"&input_sortie=="X40µm"){
    Path_save = "Models/Classification/SDP/Fines/"
  }else if(input_var_merge=="SDP"&input_sortie=="X250µm"){
    Path_save = "Models/Classification/SDP/Gros/"
  }
  if(input_var_merge=="SDP sans CPT"&input_sortie=="X40µm"){
    Path_save = "Models/Classification/Sans_CPT/Fines/"
  }else if(input_var_merge=="SDP sans CPT"&input_sortie=="X250µm"){
    Path_save = "Models/Classification/Sans_CPT/Gros/"
  }
  
  return(Path_save)
}





F_Main_HG <- function(dataframe="ok",input_formula="n",input_sortie="X40µm",
                      input_var_merge ="SDP" ,regle_jointure_value="mean",regle_jointure_value2="median",
                      a_hours =60,par_hours = F,k_spilt=0.7,Rapport=F){
  
  path_save = make_path(input_var_merge,input_sortie,Rapport=Rapport)
  
  if(dataframe=="corrected"){
    load("Data/Fevrier_DATA_D.Rda")
    Fevrier_DATA=Fevrier_DATA_D
    Fevrier_DATA = Fevrier_DATA %>% filter(Débit_CV004>0) 
  }else{
    load("Data/Fevrier_DATA3.Rda")
  }
  My_data_Model=merge_variableHG(input_var_merge,regle_jointure_value,regle_jointure_value2,a =a_hours,par_hours = par_hours,Fevrier_DATA)
  
  My_data_Model_classification = My_data_Model %>% 
      mutate(X40µm=ifelse(X40µm<20,"bad",ifelse(X40µm<=26,"good","bad")),
             X250µm=ifelse(X250µm<10,"good","bad"))
  
  c(Train_df_cl, Test_df_cl,index_train)%<-% MakeMergeParameterCPTSDP(My_data_Model_classification,k_spilt,F,input_sortie)
  
  if(is.null(input_formula)){
    
    if(dataframe=="corrected"){
      k_fourmla = colnames(Train_df_cl[1:ncol(Train_df_cl)-1])
      c <- as.formula(paste(input_sortie,"~",paste(k_fourmla,collapse = "+")))
    }else{
      k_fourmla = colnames(Train_df_cl[1:ncol(Train_df_cl)-1])
      k_fourmla1 =k_fourmla[!k_fourmla %in% c("retart","dure")]
      c <- as.formula(paste(input_sortie,"~",paste(k_fourmla1,collapse = "+")))
    }
  
    
  }else{
    c <- as.formula(paste(input_sortie,"~",paste(input_formula,collapse = "+")))
    print(c)
    
    }
      

    #xgboot data preparing 
    
    vars <-colnames(My_data_Model_classification[2:(ncol(My_data_Model_classification)-3)])
    treatplan <- designTreatmentsZ(My_data_Model_classification, vars)
  
    scoreFrame <- treatplan %>%
      magrittr::use_series(scoreFrame) %>%
      select(varName, origName, code)
    
    newvars <- scoreFrame %>%
      filter(code %in% c("clean", "lev")) %>%
      magrittr::use_series(varName)
    
    save(treatplan,file = paste0(dirname(path_save),"/treatplan.rds")) 
    save(newvars,file = paste0(dirname(path_save),"/newvars.rds")) 
    My_data_Model_classification_xgb <- prepare(treatplan, My_data_Model_classification, varRestriction = newvars)
    Train_df_cl_xgb <- prepare(treatplan, Train_df_cl, varRestriction = newvars)
    Test_df_cl_xgb <- prepare(treatplan, Test_df_cl, varRestriction = newvars)
    
    if(is.null(input_formula)){
      if(dataframe=="corrected"){
        k_fourmla = colnames(Train_df_cl_xgb)
        c_xbg <- as.formula(paste(input_sortie,"~",paste(k_fourmla,collapse = "+")))
      }else{
        k_fourmla11 = colnames(Train_df_cl_xgb)
        k_fourmla2 =k_fourmla11[!k_fourmla11 %in% c("retart","dure")]
        c_xbg <- as.formula(paste(input_sortie,"~",paste(k_fourmla2,collapse = "+")))
      }
      
    }else{
      k_fourmla11 = input_formula
      k_fourmla2 =k_fourmla11[!k_fourmla11 %in% c("Poste","Qualité")]
      c_xbg <- as.formula(paste(input_sortie,"~",paste(k_fourmla2,collapse = "+"),"+Poste_lev_x_P1+Poste_lev_x_P2+Poste_lev_x_P3+Qualité_lev_x_BTNBA+Qualité_lev_x_BTNSC+Qualité_lev_x_BTRBA+Qualité_lev_x_BTRSC+Qualité_lev_x_MTBA"))
      print(c_xbg)
      
      
    }
    
   
    actual_test <- ifelse(Test_df_cl[[input_sortie]] == "good", 1, 0)
    actual_train <- ifelse(Train_df_cl[[input_sortie]] == "good", 1, 0)
    Train_df_cl_gbm =Train_df_cl
    Train_df_cl_gbm[[input_sortie]] =ifelse(Train_df_cl[[input_sortie]] == "good", 1, 0)
    Test_df_cl_gbm=Test_df_cl
    Test_df_cl_gbm[[input_sortie]]=ifelse(Test_df_cl[[input_sortie]] == "good", 1, 0)
    Train_df_cl[[input_sortie]] <-as.factor(ifelse(Train_df_cl[[input_sortie]] == "good", 1, 0))
    Test_df_cl[[input_sortie]] <- as.factor(ifelse(Test_df_cl[[input_sortie]] == "good", 1, 0))
    
    
    #nueralnet 
    maxs_train <- apply( Train_df_cl_xgb, 2, max) 
    mins_train <- apply( Train_df_cl_xgb, 2, min)
    maxs_test <- apply( Test_df_cl_xgb, 2, max) 
    mins_test <- apply( Test_df_cl_xgb, 2, min)
    scaled_train <- as.data.frame(scale(Train_df_cl_xgb, center = mins_train, scale = maxs_train - mins_train))
    scaled_test <- as.data.frame(scale(Test_df_cl_xgb, center = mins_test, scale = maxs_test - mins_test))
    
    scaled_train=scaled_train %>% 
      mutate(v_sotrie=Train_df_cl[[input_sortie]],
             Qualité_lev_x_MTBA=Train_df_cl_xgb$Qualité_lev_x_MTBA,
             Qualité_lev_x_BTRSC=Train_df_cl_xgb$Qualité_lev_x_BTRSC,
             Qualité_lev_x_BTNBA=Train_df_cl_xgb$Qualité_lev_x_BTNBA,
             Qualité_lev_x_BTRBA=Train_df_cl_xgb$Qualité_lev_x_BTRBA,
             Qualité_lev_x_BTNSC=Train_df_cl_xgb$Qualité_lev_x_BTNSC
             
      )
    names(scaled_train)[names(scaled_train) == "v_sotrie"] <- input_sortie
    scaled_test=scaled_test %>% 
      mutate(v_sotrie=Test_df_cl[[input_sortie]],
             Qualité_lev_x_MTBA=Test_df_cl_xgb$Qualité_lev_x_MTBA,
             Qualité_lev_x_BTRSC=Test_df_cl_xgb$Qualité_lev_x_BTRSC,
             Qualité_lev_x_BTNBA=Test_df_cl_xgb$Qualité_lev_x_BTNBA,
             Qualité_lev_x_BTRBA=Test_df_cl_xgb$Qualité_lev_x_BTRBA,
             Qualité_lev_x_BTNSC=Test_df_cl_xgb$Qualité_lev_x_BTNSC) 
    names(scaled_test)[names(scaled_test) == "v_sotrie"] <- input_sortie  
    
  
    # browser()
    try({
      levels(Test_df_cl$Qualité) <- levels(Train_df_cl$Qualité)
      
    }) 
    
    
    #Build a classification tree---------------------------------------------------------------------------
    
    str(Train_df_cl)
    print("rpart-------------------------------------------")
    # Create the model
    model_1 <- rpart(formula = c, 
                     data = Train_df_cl, 
                     method = "class")
    saveRDS(model_1,file = paste0(path_save,"tree.rds") )
    
    class_prediction_1 <- predict(object = model_1,  
                                  newdata = Train_df_cl,  
                                  type = "prob")
    dt_preds_train=class_prediction_1[,'1']
    class_prediction_1 <- predict(object = model_1,  
                                  newdata = Test_df_cl,  
                                  type = "prob") 
    dt_preds_test=class_prediction_1[,'1']
    
    
    #Train a bagged tree model---------------------------------------------------------------------------
    library(ipred)
    # Train a bagged model
    print("Bagged Tree---------------------------")
    model_2 <- bagging(formula =c, 
                       data = Train_df_cl,
                       coob = TRUE)
    
    saveRDS(model_2,file = paste0(path_save,"BaggedTree.rds") )
    class_prediction_2 <- predict(object = model_2, 
                                  newdata = Test_df_cl,  
                                  type = "prob") 
    
    bag_preds_test=class_prediction_2[,'1']
    
    class_prediction_2 <- predict(object = model_2, 
                                  newdata = Train_df_cl,  
                                  type = "prob") 
    
    bag_preds_train=class_prediction_2[,'1']
    
    
    
    #random Foresct ---------------------------------------------------
    print("Random Forest-----------------------------------------------------")
    rf_rg=randomForest(c, data = Train_df_cl)
    #Test 
    saveRDS(rf_rg,file = paste0(path_save,"rf.rds") )
    pred <- predict(object = rf_rg,
                    newdata = Test_df_cl,
                    type = "prob")
    
    
    rf_preds_test=pred[,'1']
    pred <- predict(object = rf_rg,
                    newdata = Train_df_cl,
                    type = "prob")
    
    
    rf_preds_train=pred[,'1']
    
    
    
    #Train a GBM model ---------------------------------------
    print('GBM--------------------------------')
    
    model_gbm <- gbm::gbm(formula = c,
                          distribution = "bernoulli",
                          data = Train_df_cl_gbm,
                          shrinkage=0.01,
                          interaction.depth =15,
                          n.trees = 1000)
    saveRDS(model_gbm, file = paste0(path_save,"GBM.rds"))
    
    
    
    preds2 <- predict(object = model_gbm, 
                      newdata = Test_df_cl_gbm,
                      n.trees = 500,
                      type = "response")
    gbm_preds_test=preds2
    preds2 <- predict(object = model_gbm, 
                      newdata = Train_df_cl_gbm,
                      n.trees = 500,
                      type = "response")
    gbm_preds_train=preds2
    
    #xgboost -----
    print('xgboost--------------------------------')
    model_xgb <- xgboost(data = as.matrix(Train_df_cl_xgb), # training data as matrix
                         label = Train_df_cl_gbm[[input_sortie]],  # column of outcomes
                         nrounds = 19,       # 17:19
                         objective = "binary:logistic", # objective
                         eta = 0.8,
                         max_depth = 4,
                         early_stopping_rounds = 10,
                         verbose = 0  # silent
    )
    
    
    saveRDS(model_xgb, file = paste0(path_save,"xgboost.rds"))
    # Make predictions
    xgb_preds_train <- predict(model_xgb, as.matrix(Train_df_cl_xgb))
    xgb_preds_test <- predict(model_xgb, as.matrix(Test_df_cl_xgb))

    #neuralnet ANN
    print('neuralnet ----------------------------------------')
    nn <- tryCatch({
        nn <- neuralnet::neuralnet(c_xbg,data=scaled_train,hidden=c(12,4),linear.output=F)
        },
        warning = function(w) {
        print("ANN new hidden layers testing .... ")
        
        nn <- tryCatch({ 
          nn <- neuralnet::neuralnet(c_xbg,data=scaled_train,hidden=c(18,4),linear.output=F)
          },
          warning = function(w) {
          print("ANN new hidden layers testing 2 .... ")
            nn <- tryCatch({ 
                nn <- neuralnet::neuralnet(c_xbg,data=scaled_train,hidden=1,linear.output=F)
              },
              warning = function(w) {
                print("ANN new hidden layers testing 3 .... ")
                nn <- neuralnet::neuralnet(c_xbg,data=scaled_train,hidden=c(4,4),linear.output=F)
              return(nn)
            })
          return(nn)
          })
       
        return(nn)
      })
    saveRDS(nn, file = paste0(path_save,"ann.rds"))
    pr.nn <- compute(nn,scaled_train[,1:ncol(scaled_train)-1])
    ann_pred_train =pr.nn$net.result[,2]
    #test 
    pr.nn <- compute(nn,scaled_test[,1:ncol(scaled_test)-1])
    ann_pred_test =pr.nn$net.result[,2]
    #train ------
    
  
    #Knn 
    print("knn --------------------------------------")
    
    pred_knn_train<- knn(Train_df_cl_xgb,Train_df_cl_xgb, Train_df_cl[[input_sortie]], k =4 , prob=TRUE)
    pred_knn_test<- knn(Train_df_cl_xgb,Test_df_cl_xgb, Train_df_cl[[input_sortie]], k =4 , prob=TRUE)
    
    #glm
    print('glm ----------------------------------------')
    model_glm = glm(c, data  = Train_df_cl,family = "binomial" )
    pred_glm_test <- predict(model_glm, Test_df_cl, type = 'response')
    pred_glm_train <- predict(model_glm, Train_df_cl, type = 'response')
    saveRDS(model_glm, file = paste0(path_save,"glm.rds"))
    
    
    dt_auc_test <- Metrics::auc(actual = actual_test, predicted = dt_preds_test)
    dt_auc_train <- Metrics::auc(actual = actual_train, predicted = dt_preds_train)
    #baged tree
    bag_auc_test <- Metrics::auc(actual = actual_test, predicted = bag_preds_test)
    bag_auc_train <- Metrics::auc(actual = actual_train, predicted = bag_preds_train)
    #random forest
    rf_auc_test <- Metrics::auc(actual = actual_test, predicted = rf_preds_test)
    rf_auc_train <- Metrics::auc(actual = actual_train, predicted = rf_preds_train)
    #gbm
    gbm_auc_test <- Metrics::auc(actual = actual_test, predicted = gbm_preds_test)
    gbm_auc_train <- Metrics::auc(actual = actual_train, predicted = gbm_preds_train)
    #xgb
    xgb_auc_test <- Metrics::auc(actual = actual_test, predicted = xgb_preds_test)
    xgb_auc_train <- Metrics::auc(actual = actual_train, predicted = xgb_preds_train)
    #ann 
   
    
    
    ann_auc_test <- Metrics::auc(actual = actual_test, predicted = ann_pred_test)
    ann_auc_train <- Metrics::auc(actual = actual_train, predicted = ann_pred_train)
  
    #knn
    knn_auc_test <- Metrics::auc(actual = actual_test, predicted = pred_knn_test)
    knn_auc_train <- Metrics::auc(actual = actual_train, predicted = pred_knn_train)
    #glm
    glm_auc_test <- Metrics::auc(actual = actual_test, predicted = pred_glm_test)
    glm_auc_train <- Metrics::auc(actual = actual_train, predicted = pred_glm_train) 
    
    #accuracy------------------------------------------------
    #rpart
    dt_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted = ifelse( dt_preds_test>0.5,1,0))
    dt_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted = ifelse( dt_preds_train>0.5,1,0))
    #baged tree
    bag_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted   = ifelse(bag_preds_test>0.5,1,0))
    bag_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted = ifelse(bag_preds_train>0.5,1,0))
    #random forest
    rf_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted    = ifelse(rf_preds_test>0.5,1,0))
    rf_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted  = ifelse(rf_preds_train>0.5,1,0))
    #gbm
    gbm_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted   = ifelse(gbm_preds_test>0.5,1,0))
    gbm_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted = ifelse(gbm_preds_train>0.5,1,0))
    #xgb
    xgb_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted   = ifelse(xgb_preds_test>0.5,1,0))
    xgb_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted = ifelse(xgb_preds_train>0.5,1,0))
    #ann 
   
    
    ann_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted   = ifelse(ann_pred_test>0.5,1,0))
    ann_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted = ifelse(ann_pred_train>0.5,1,0))
    
  
    #knn
    knn_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted   = as.vector(pred_knn_test))
    knn_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted = as.vector(pred_knn_train))
    #glm
    glm_accuracy_test <- Metrics::accuracy(actual = actual_test, predicted   = ifelse(pred_glm_test>0.5,1,0))
    glm_accuracy_train <- Metrics::accuracy(actual = actual_train, predicted = ifelse(pred_glm_train>0.5,1,0)) 
   
    M_auc =mean(c(glm_auc_test,knn_auc_test,ann_auc_test,xgb_auc_test,gbm_auc_test,rf_auc_test,bag_auc_test,dt_auc_test))
    M_auc2 =mean(c(glm_auc_train,knn_auc_train,ann_auc_train,xgb_auc_train,gbm_auc_train,rf_auc_train,bag_auc_train,dt_auc_train))
    M_accuracy2 =mean(c(glm_accuracy_train,knn_accuracy_train,ann_accuracy_train,xgb_accuracy_train,gbm_accuracy_train,rf_accuracy_train,bag_accuracy_train,dt_accuracy_train))
    M_accuracy =mean(c(glm_accuracy_test,knn_accuracy_test,ann_accuracy_test,xgb_accuracy_test,gbm_accuracy_test,rf_accuracy_test,bag_accuracy_test,dt_accuracy_test))
    
    
 
   
    
    
    df_result = data.frame(Methode=c("Decision Tree","Bagged Trees","Random Forest","GBM","XGB","ANN","Knn","glm","Moyenne totale"),
                           AUC_test=c(dt_auc_test,bag_auc_test,rf_auc_test,gbm_auc_test,xgb_auc_test,ann_auc_test,knn_auc_test,glm_auc_test,M_auc),
                           AUC_train=c(dt_auc_train,bag_auc_train,rf_auc_train,gbm_auc_train,xgb_auc_train,ann_auc_train,knn_auc_train,glm_auc_train,M_auc2),
                           accuracy_test=c(dt_accuracy_test,bag_accuracy_test,rf_accuracy_test,gbm_accuracy_test,xgb_accuracy_test,ann_accuracy_test,knn_accuracy_test,glm_accuracy_test,M_accuracy),
                           accuracy_train=c(dt_accuracy_train,bag_accuracy_train,rf_accuracy_train,gbm_accuracy_train,xgb_accuracy_train,ann_accuracy_train,knn_accuracy_train,glm_accuracy_train,M_accuracy2))
    
    
    preds_list <- list(dt_preds_test,
                       bag_preds_test,
                       rf_preds_test,
                       gbm_preds_test,
                       xgb_preds_test,
                       ann_pred_test,
                       attributes(pred_knn_test)$prob,
                       pred_glm_test)
    library(ROCR)
    m <- length(preds_list)
    actuals_list <- rep(list(actual_test), m)
    
    # Plot the ROC curves
    pred <- ROCR::prediction(preds_list, actuals_list)
    rocs <- performance(pred, "tpr", "fpr")

    
    return(list(df_result,rocs,m,c))
}


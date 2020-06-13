#########################################
# OUJBIH ABDERRAHIM              ########
# 17/05/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
######################################### 
source("R/Packages.R")
#Function
source("R/Function.R",encoding = "UTF-8")

load("Data/Fevrier_DATA3.Rda")

load("Fevrier_DATA_D.Rda")
library(Metrics)
#mean sans retart 

Fevrier_DATA=Fevrier_DATA_D

# All possibility of merge 


input_var_merge <- c("SPD","CPT","SDP sans CPT")
regle_jointure_value <- c("mean","max","median")
df_grid <- expand.grid(input_var_merge = input_var_merge, regle_jointure_value = regle_jointure_value)

##-------------------------------------------------Main-------------------------------
input_sortie="X40µm"
input_var_merge ="SDP" #SDP #CPT  SDP sans CPT Parametres
regle_jointure_value="mean"
regle_jointure_value2="max"


#Merge -----------------------------------------------------------------------------
Fevrier_DATA = Fevrier_DATA %>% filter(Débit_CV004>0) 
My_data_Model=merge_variableHG(input_var_merge,regle_jointure_value,regle_jointure_value2,a =60,par_hours = F)

{
# Model_SDP_Mean = My_data_Model
# save(Model_SDP_Mean,file = "Data/Jointure_par_SDP/Model_SDP_Mean.Rda")

My_data_Model_classification = My_data_Model %>% 
  mutate(X40µm=ifelse(X40µm<20,"bad",ifelse(X40µm<=26,"good","bad")),
         X250µm=ifelse(X250µm<10,"good","bad"))

My_data_Model_classification=My_data_Model_classification %>% filter(Débit_CV004>0)
#formula --------------------------------------------------------------------------------
{c(Train_df_cl, Test_df_cl,index_train)%<-% MakeMergeParameterCPTSDP(My_data_Model_classification,0.7,F,input_sortie)
  k_fourmla1 = colnames(Train_df_cl[1:ncol(Train_df_cl)-1])
print(k_fourmla1)
k_fourmla=k_fourmla1
# k_fourmla =k_fourmla1[!k_fourmla1 %in% c("retart","dure")]#"Qualité","Poste"
print(k_fourmla)
c <- as.formula(paste(input_sortie,"~",paste(k_fourmla,collapse = "+")))
print(c)


actual_test <- ifelse(Test_df_cl[[input_sortie]] == "good", 1, 0)
actual_train <- ifelse(Train_df_cl[[input_sortie]] == "good", 1, 0)
Train_df_cl_gbm =Train_df_cl
Train_df_cl_gbm[[input_sortie]] =ifelse(Train_df_cl[[input_sortie]] == "good", 1, 0)
Test_df_cl_gbm=Test_df_cl
Test_df_cl_gbm[[input_sortie]]=ifelse(Test_df_cl[[input_sortie]] == "good", 1, 0)
Train_df_cl[[input_sortie]] <-as.factor(ifelse(Train_df_cl[[input_sortie]] == "good", 1, 0))
Test_df_cl[[input_sortie]] <- as.factor(ifelse(Test_df_cl[[input_sortie]] == "good", 1, 0))
#xgboost 
{vars <-colnames(My_data_Model_classification[2:(ncol(My_data_Model_classification)-3)])
  
  
  library(vtreat)
  library(xgboost)
  treatplan <- designTreatmentsZ(My_data_Model_classification, vars)
  scoreFrame <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%
    select(varName, origName, code)
  
  newvars <- scoreFrame %>%
    filter(code %in% c("clean", "lev")) %>%
    magrittr::use_series(varName)
  
  # Create the treated training data
  My_data_Model_classification_xgb <- prepare(treatplan, My_data_Model_classification, varRestriction = newvars)
  save(My_data_Model_classification_xgb,file = "Data/xgboost/My_data_Model_classification_xgb.Rda")
  Train_df_cl_xgb <- prepare(treatplan, Train_df_cl, varRestriction = newvars)
  Test_df_cl_xgb <- prepare(treatplan, Test_df_cl, varRestriction = newvars)
  k_fourmla11 = colnames(Train_df_cl_xgb)
  k_fourmla2=k_fourmla11
  # k_fourmla2 =k_fourmla11[!k_fourmla11 %in% c("retart","dure")]
  c_xbg <- as.formula(paste(input_sortie,"~",paste(k_fourmla2,collapse = "+")))
  print(c_xbg)
  
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
}
}


# Generate the test set AUCs using the two sets of predictions & compare
c(dt_preds_test,dt_preds_train, 
  bag_preds_test,bag_preds_train, 
  rf_preds_test,rf_preds_train, 
  gbm_preds_test,gbm_preds_train, #gbm
  xgb_preds_test,xgb_preds_train, #xgb
  ann_pred_test,ann_pred_train,
  pred_knn_test,pred_knn_train,
  pred_glm_test,pred_glm_train) %<-% classificationHG()

#AUC Accuracy
{
  #AUC---------------------------
#rpart
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
}
M_auc =mean(glm_auc_test,knn_auc_test,ann_auc_test,xgb_auc_test,gbm_auc_test,rf_auc_test,bag_auc_test,dt_auc_test)
M_accuracy =mean(glm_accuracy_test,knn_accuracy_test,ann_accuracy_test,xgb_accuracy_test,gbm_accuracy_test,rf_accuracy_test,bag_accuracy_test,dt_accuracy_test)
# Print results
# Print results
{ print(c)
  print("AUC----------------------------------------")
  print('                         test  train')
  print(sprintf("Decision Tree Test AUC: %.3f  %.3f", dt_auc_test,dt_auc_train))
  print(sprintf("Bagged Trees Test  AUC: %.3f  %.3f", bag_auc_test,bag_auc_train))
  print(sprintf("Random Forest Test AUC: %.3f  %.3f", rf_auc_test,rf_auc_train))
  print(sprintf("          GBM Test AUC: %.3f  %.3f", gbm_auc_test,gbm_auc_train))
  print(sprintf("          XGB Test AUC: %.3f  %.3f", xgb_auc_test,xgb_auc_train))
  print(sprintf("          ANN Test AUC: %.3f  %.3f", ann_auc_test,ann_auc_train))
  print(sprintf("          Knn Test AUC: %.3f  %.3f", knn_auc_test,knn_auc_train))
  print(sprintf("          glm Test AUC: %.3f  %.3f", glm_auc_test,glm_auc_train))
  print(sprintf("    Moyenne totale AUC: %.3f       ", M_auc))
  print("accuracy---------------------------------")
  print('                              test  train')
  print(sprintf("Decision Tree Test accuracy: %.3f  %.3f", dt_accuracy_test,dt_accuracy_train))
  print(sprintf("Bagged Trees Test  accuracy: %.3f  %.3f", bag_accuracy_test,bag_accuracy_train))
  print(sprintf("Random Forest Test accuracy: %.3f  %.3f", rf_accuracy_test,rf_accuracy_train))
  print(sprintf("          GBM Test accuracy: %.3f  %.3f", gbm_accuracy_test,gbm_accuracy_train))
  print(sprintf("          XGB Test accuracy: %.3f  %.3f", xgb_accuracy_test,xgb_accuracy_train))
  print(sprintf("          ANN Test accuracy: %.3f  %.3f", ann_accuracy_test,ann_accuracy_train))
  print(sprintf("          Knn Test accuracy: %.3f  %.3f", knn_accuracy_test,knn_accuracy_train))
  print(sprintf("          glm Test accuracy: %.3f  %.3f", glm_accuracy_test,glm_accuracy_train))
  print(sprintf("    Moyenne totale accuracy: %.3f       ", M_accuracy))
}

#









{
preds_list <- list(dt_preds_test,
                   bag_preds_test,
                   rf_preds_test,
                   gbm_preds_test,
                   xgb_preds_test,
                   ann_pred_test,
                   attributes(pred_knn_test)$prob,
                   pred_glm_test)
# preds_list_df <- data.frame(dt_preds, bag_preds, rf_preds, gbm_preds)
# ensemble =apply(preds_list_df, 1,mean)
# Metrics::auc(actual = actual, predicted = ensemble)
library(ROCR)
m <- length(preds_list)
actuals_list <- rep(list(actual_test), m)

# Plot the ROC curves
pred <- ROCR::prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM","XGB","ANN","KNN","glm"),
       fill = 1:m,
       cex = 0.5)

}

print(regle_jointure_value2)
} #fin 
  










#########################################
# OUJBIH ABDERRAHIM              ########
# 17/05/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
######################################### 
source("R/Packages.R")
#Function
source("R/Function.R",encoding = "UTF-8")
library(Metrics)
#mean sans retart 
load("Data/Classification/My_data_Model_classification_sans_retart.Rda")
input_sortie="X40µm"
input_var_merge ="SDP sans CPT" #SDP #CPT  SDP sans CPT Parametres
regle_jointure_value="mean"
regle_jointure_value2="mean"
My_data_Model=merge_variableHG(input_var_merge,regle_jointure_value,regle_jointure_value2)
My_data_Model_classification = My_data_Model %>% 
  mutate(X40µm=ifelse(X40µm<20,"bad",ifelse(X40µm<=26,"good","bad")),
         X250µm=ifelse(X250µm<10,"good","bad"))

c(DATA.traintmp_regression, DATA.testmp_regression,index_train)%<-% MakeMergeParameterCPTSDP(My_data_Model_classification,0.7,F,input_sortie)
# c(DATA.traintmp_regression1, DATA.testmp_regression1,index_train1)%<-% MakeMergeParameterCPTSDP(My_data_Model_classification,0.7,F,input_sortie)
k_fourmla1 = colnames(DATA.traintmp_regression[1:ncol(DATA.traintmp_regression)-1])
print(k_fourmla1)
k_fourmla =k_fourmla1[!k_fourmla1 %in% c("Courant_ML004","Courant_ML002")]#"Qualité","Poste"
# k_fourmla =k_fourmla1
print(k_fourmla)
c <- as.formula(paste(input_sortie,"~",paste(k_fourmla,collapse = "+")))
print(c)
actual <- ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0)

#gbm
classificationHG <- function(){
  DATA.traintmp_regression_gbm =DATA.traintmp_regression
  DATA.traintmp_regression_gbm[[input_sortie]] =ifelse(DATA.traintmp_regression[[input_sortie]] == "good", 1, 0)
  DATA.testmp_regression_gbm=DATA.testmp_regression
  DATA.testmp_regression_gbm[[input_sortie]]=ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0)
  
  DATA.traintmp_regression[[input_sortie]] <-as.factor(ifelse(DATA.traintmp_regression[[input_sortie]] == "good", 1, 0))
  DATA.testmp_regression[[input_sortie]] <- as.factor(ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0))
  # DATA.testmp_regression[[input_sortie]]=as.factor(DATA.testmp_regression[[input_sortie]])
  # DATA.traintmp_regression[[input_sortie]]=as.factor(DATA.traintmp_regression[[input_sortie]])
 
  #Cor 
  # library(Hmisc)
  # cor_df =as.matrix(Model_data[,4:ncol(Model_data)-3])
  # cor_df =cor_df[,4:ncol(cor_df)]
  # x_cor=rcorr(cor_df)
  # xx =x_cor$r
  # xx[upper.tri(xx)] <- 0
  # diag(xx) <- 0
  # data.new <- Model_data[,!apply(xx,2,function(x) any(abs(x) >= 0.7))]
  # colnames(data.new)
  # colnames(Model_data)
  # 
  
  
  
  
  
  #Build a classification tree---------------------------------------------------------------------------
  # Look at the data
  str(DATA.traintmp_regression)
  print("rpart-------------------------------------------")
  # Create the model
  model_1 <- rpart(formula = c, 
                          data = DATA.traintmp_regression, 
                          method = "class")
  
  # Display the results 
  # rpart.plot(x = model_1, yesno = 1, type = 0, extra = 1,tweak = 1.2, under = T,fallen.leaves = F)
  
  
  # Generate predicted classes using the model object
  class_prediction_1 <- predict(object = model_1,  
                                newdata = DATA.testmp_regression,  
                                type = "class") 
  dt_preds=class_prediction_1[,'1']
  # 
  # # Calculate the confusion matrix for the test set
  # caret::confusionMatrix(data = class_prediction,         
  #                 reference = DATA.testmp_regression[[input_sortie]]) 
  # 
  # #Compare models with a different splitting criterion 
  # 
  # # Train a gini-based model
  # model1 <- rpart(formula =  c, 
  #                        data = DATA.traintmp_regression, 
  #                        method = "class",
  #                        parms = list(split = "gini"))
  # 
  # # Train an information-based model
  # model2 <- rpart(formula = c, 
  #                        data = DATA.traintmp_regression, 
  #                        method = "class",
  #                        parms = list(split = "information"))
  # 
  # # Generate predictions on the validation set using the gini model
  # pred1 <- predict(object = model1,
  #                  newdata = DATA.testmp_regression,
  #                  type = "prob")    
  # 
  # # Generate predictions on the validation set using the information model
  # pred2 <- predict(object = model2, 
  #                  newdata = DATA.testmp_regression,
  #                  type = "prob")
  # 
  # # Compare classification error
  # ce(actual = DATA.testmp_regression[[input_sortie]], 
  #    predicted = pred1)
  # ce(actual = DATA.testmp_regression[[input_sortie]], 
  #    predicted = pred2)  
  # 
  # caret::confusionMatrix(data = pred1,         
  #                        reference = DATA.testmp_regression[[input_sortie]]) 
  # Metrics::auc(actual = ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0), 
  #              predicted = class_prediction_1[,"good"])    
  
  
  #Train a bagged tree model---------------------------------------------------------------------------
  library(ipred)
  # Train a bagged model
  print("Bagged Tree---------------------------")
  model_2 <- bagging(formula =c, 
                            data = DATA.traintmp_regression,
                            coob = TRUE)
  
  # Print the model
  # print(model_2)
  
  
  # Generate predicted classes using the model object
  class_prediction_2 <- predict(object = model_2, 
                                newdata = DATA.testmp_regression,  
                                type = "prob") 
  
  bag_preds=class_prediction_2[,'1']
  # return classification labels prob
  # Print the predicted classes
  # print(class_prediction_2)
  # DATA.testmp_regression[[input_sortie]]=ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0)
  # # Calculate the confusion matrix for the test set
  # caret::confusionMatrix(data = class_prediction_2,         
  #                 reference = DATA.testmp_regression[[input_sortie]])  
  # 
  # # Compute the AUC (`actual` must be a binary vector)
  # Metrics::auc(actual = ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0), 
  #     predicted = class_prediction_2[,"good"])                
  
  
  #Cross-validate a bagged tree model in caret--------------------------------------
  # # Specify the training configuration
  # library(caret)
  # ctrl <- trainControl(method = "cv",     # Cross-validation
  #                      number = 5,        # 5 folds
  #                      classProbs = TRUE,                  # For AUC
  #                      summaryFunction = twoClassSummary,
  #                      savePredictions = TRUE)  # For AUC
  # 
  # 
  # caret_model <- train(c, 
  #                             data = DATA.traintmp_regression, 
  #                             method = "treebag",
  #                             metric = "ROC",
  #                             trControl = ctrl)
  # 
  # # Look at the model object
  # print(caret_model)
  # 
  # # library(pROC)
  # # selectedIndices <- caret_model$pred$mtry =
  # # plot.roc(caret_model$pred
  # 
  # 
  # 
  # 
  # # Generate predictions on the test set
  # pred <- predict(object = caret_model, 
  #                 newdata = DATA.testmp_regression,
  #                 type = "prob")
  # 
  # caret::confusionMatrix(data = pred,         
  #                        reference = DATA.testmp_regression[[input_sortie]]) 
  # # Compute the AUC (`actual` must be a binary (or 1/0 numeric) vector)
  # Metrics::auc(actual = ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0), 
  #     predicted = pred[,"good"])
  # 
  # # Print ipred::bagging test set AUC estimate
  # print(ipred_model_test_auc)
  # 
  # # Print caret "treebag" test set AUC estimate
  # print(caret_model_test_auc)
  # 
  # # Compare to caret 5-fold cross-validated AUC
  # caret_model$results[, 2]
  # 
  
  
  #random Foresct ---------------------------------------------------
  print("Random Forest-----------------------------------------------------")
  rf_rg=randomForest(c, data = DATA.traintmp_regression)
  # rf_rg1=randomForest(c, data = DATA.traintmp_regression1)
  # plot( rf_rg)
  # plot( rf_rg1)
  # Add a legend since it doesn't have one by default
  # legend(x = "right", 
  #        legend = colnames(rf_rg$err.rate),
  #        fill = 1:4)
  #Test 
  try({
    
    levels(DATA.testmp_regression$Qualité) <- levels(DATA.traintmp_regression$Qualité)
    # levels(DATA.testmp_regression1$Qualité) <- levels(DATA.traintmp_regression1$Qualité)
    
  }) 
  # pred.test=predict(rf_rg,DATA.testmp_regression)
  # 
  # 
  # table(pred.test,DATA.testmp_regression[[input_sortie]])
  # 
  # cm <- caret::confusionMatrix(data = pred.test,          # predicted classes
  #                              reference = DATA.testmp_regression[[input_sortie]]) 
  # cm
  # # Compare test set accuracy to OOB accuracy
  # paste0("Test Accuracy: ", cm$overall[1])
  # paste0("OOB Accuracy: ", 1 - rf_rg$err[nrow(rf_rg$err), "OOB"])
  
  
  # Generate predictions on the test set
  pred <- predict(object = rf_rg,
                  newdata = DATA.testmp_regression,
                  type = "prob")
  
  
  rf_preds=pred[,'1']
  # rf_pred =ifelse(pred[,"good"]>0.5, "good", "bad")
  # table(rf_pred,DATA.testmp_regression1[[input_sortie]])
  
  # `pred` is a matrix ------
  # class(pred)
  # 
  # # Look at the pred format
  # head(pred)
  # 
  # # Compute the AUC (`actual` must be a binary 1/0 numeric vector)
  # 
  # Metrics::auc(actual = ifelse(DATA.testmp_regression[[input_sortie]] == "good", 1, 0), 
  #              predicted = pred[,"good"])      
  # 
  # 
  # # Execute the tuning process
  # set.seed(1234)              
  # res <- tuneRF(x = subset(DATA.traintmp_regression, select = -X40µm),
  #               y = DATA.traintmp_regression[[input_sortie]],
  #               ntreeTry = 500
  #               )
  # 
  # # Look at results
  # print(res)
  # 
  # # Find the mtry value that minimizes OOB Error
  # mtry_opt <- res[,"mtry"][which.min(res[,"OOBError"])]
  # print(mtry_opt)
  # 
  # 
  # #tunning 
  # # Establish a list of possible values for mtry, nodesize and sampsize
  # mtry <- seq(2, ncol(DATA.traintmp_regression) * 0.8, 2)
  # nodesize <- seq(3, 8, 2)
  # sampsize <- nrow(DATA.traintmp_regression) * c(0.7, 0.8)
  # 
  # # Create a data frame containing all combinations 
  # hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize)
  # 
  # # Create an empty vector to store OOB error values
  # oob_err <- c()
  # 
  # # Write a loop over the rows of hyper_grid to train the grid of models
  # for (i in 1:nrow(hyper_grid)) {
  #   
  #   # Train a Random Forest model
  #   model <- randomForest(formula = c, 
  #                         data = DATA.traintmp_regression,
  #                         mtry = hyper_grid$mtry[i],
  #                         nodesize = hyper_grid$nodesize[i],
  #                         sampsize = hyper_grid$sampsize[i])
  #   
  #   # Store OOB error for the model                      
  #   oob_err[i] <- model$err.rate[nrow(model$err.rate), "OOB"]
  # }
  # 
  # # Identify optimal set of hyperparmeters based on OOB error
  # opt_i <- which.min(oob_err)
  # print(hyper_grid[opt_i,])
  # 
  # 
  # model <- randomForest(formula = c, 
  #                       data = DATA.traintmp_regression,
  #                       mtry = hyper_grid[opt_i,1],
  #                       nodesize =  hyper_grid[opt_i,2],
  #                       sampsize =  hyper_grid[opt_i,3])
  # 
  # 
  # #Test 
  # try({
  #   
  #   levels(DATA.testmp_regression$Qualité) <- levels(DATA.traintmp_regression$Qualité)
  #   levels(DATA.testmp_regression$Poste) <- levels(DATA.traintmp_regression$Poste)
  #   
  # }) 
  # pred.test=predict(model,DATA.testmp_regression)
  # 
  # 
  # table(pred.test,DATA.testmp_regression[[input_sortie]])
  # 
  # cm <- caret::confusionMatrix(data = pred.test,          # predicted classes
  #                              reference = DATA.testmp_regression[[input_sortie]]) 
  # 
  # 
  # 
  # 
  # pred <- predict(object = model,
  #                 newdata = DATA.testmp_regression,
  #                 type = "prob")
  # 
  # Metrics::auc(actual = DATA.testmp_regression[[input_sortie]], 
  #              predicted = pred[,"good"])     
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  
  
  
  
  
  
  
  #Train a GBM model ---------------------------------------
  print('GBM--------------------------------')
  # # Convert "yes" to 1, "no" to 0
  
  # # Train a 10000-tree GBM model
  # # set.seed(1)
  # model <- gbm::gbm(formula = c,
  #                          distribution = "tdist",
  #                          shrinkage=0.02,
  #                          interaction.depth = 1,
  #                          data = DATA.traintmp_regression,
  #                          n.trees = 2000)
  # # 
  # plot(model$valid.error)
  # tmp <- gbm::gbm.perf(object = model, 
  #               method = "OOB")
  # tmp
  # 
  # # Print the model object                    
  # print(model) 
  # 
  # # summary() prints variable importance
  # summary(model)  
  
  
  #Prediction using a GBM model -------------------------------------------------------------------
  # 
  # preds2 <- predict(object = model,
  #                   newdata = DATA.testmp_regression,
  #                   n.trees = 5000,
  #                   type = "response")
  # 
  # 
  # glm_pred =ifelse(preds2>1.6, 1, 0)
  # table(glm_pred,as.factor(DATA.testmp_regression[[input_sortie]]))
  # caret::confusionMatrix(data = glm_pred,          # predicted classes
  #                       reference = as.factor(DATA.testmp_regression[[input_sortie]])) 
  # # Compare the range of the two sets of predictions
  # range(preds1)
  # range(preds2)
  # 
  # # Generate the test set AUCs using the two sets of preditions & compare
  # Metrics::auc(actual = DATA.testmp_regression[[input_sortie]], predicted = preds1)  #default
  # Metrics::auc(actual = DATA.testmp_regression[[input_sortie]], predicted = preds2)  #rescaled
  #Early stopping in GBMs----------------------------------------------------------------
  # Optimal ntree estimate based on OOB
  # ntree_opt_oob <- gbm::gbm.perf(object = model, 
  #                                 method = "OOB", 
  #                                 oobag.curve = TRUE)
  # ntree_opt_oob
  # # Train a CV GBM model
  # # # set.seed(1)
  model_cv <- gbm::gbm(formula = c,
                              distribution = "tdist",
                              data = DATA.traintmp_regression_gbm,
                              shrinkage=0.01,
                              interaction.depth =15,
                              n.trees = 500,
                              cv.folds = 2)
  # 
  # # plot(model_cv$cv.error)
  # # Optimal ntree estimate based on CV
  # ntree_opt_cv <- gbm::gbm.perf(object = model_cv,
  # 
  #                               method = "cv")

   ntree_opt_cv=178
  # Compare the estimates
  # print(paste0("Optimal n.trees (OOB Estimate): ", ntree_opt_oob))
  print(paste0("Optimal n.trees (CV Estimate): ", ntree_opt_cv))

  
  
  
  # Generate predictions on the test set using ntree_opt_oob number of trees
  # preds1 <- predict(object = model, 
  #                   newdata = DATA.testmp_regression,
  #                   n.trees = ntree_opt_oob)
  # 
  # Generate predictions on the test set using ntree_opt_cv number of trees
  preds2 <- predict(object = model_cv, 
                    newdata = DATA.testmp_regression_gbm,
                    n.trees = ntree_opt_cv,
                    type = "response")
  gbm_preds=preds2
  # Metrics::auc(actual = actual, predicted = gbm_preds)
  # glm_pred =ifelse(preds2>0.5, 1, 0)
  # table(glm_pred,as.factor(DATA.testmp_regression_gbm[[input_sortie]]))
  # # Generate the test set AUCs using the two sets of preditions & compare
  # auc1 <- Metrics::auc(actual =DATA.testmp_regression[[input_sortie]], predicted = preds1)  #OOB
  # auc2 <- Metrics::auc(actual =DATA.testmp_regression[[input_sortie]], predicted = glm_pred)  #CV 
  # 
  # # Compare AUC 
  # print(paste0("Test set AUC (OOB): ", auc1))                         
  # print(paste0("Test set AUC (CV): ", auc2))
  return(list(dt_preds, bag_preds, rf_preds, gbm_preds))
}

# Generate the test set AUCs using the two sets of predictions & compare
c(dt_preds, bag_preds, rf_preds, gbm_preds) %<-% classificationHG()
dt_auc <- Metrics::auc(actual = actual, predicted = dt_preds)
bag_auc <- Metrics::auc(actual = actual, predicted = bag_preds)
rf_auc <- Metrics::auc(actual = actual, predicted = rf_preds)
gbm_auc <- Metrics::auc(actual = actual, predicted = gbm_preds)

# Print results
sprintf("Decision Tree Test AUC: %.3f", dt_auc)
sprintf("Bagged Trees Test AUC: %.3f", bag_auc)
sprintf("Random Forest Test AUC: %.3f", rf_auc)
sprintf("GBM Test AUC: %.3f", gbm_auc)





preds_list <- list(dt_preds, bag_preds, rf_preds, gbm_preds)
preds_list_df <- data.frame(dt_preds, bag_preds, rf_preds, gbm_preds)
ensemble =apply(preds_list_df, 1,mean)
Metrics::auc(actual = actual, predicted = ensemble)
# List of actual values (same for all)
m <- length(preds_list)
actuals_list <- rep(list(actual), m)

# Plot the ROC curves
pred <- prediction(preds_list, actuals_list)
rocs <- performance(pred, "tpr", "fpr")
plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
legend(x = "bottomright", 
       legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM"),
       fill = 1:m)















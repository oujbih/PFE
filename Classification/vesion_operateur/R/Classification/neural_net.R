

#neural network
maxs_train <- apply( Train_df_cl_xgb, 2, max) 
mins_train <- apply( Train_df_cl_xgb, 2, min)
maxs_test <- apply( Test_df_cl_xgb, 2, max) 
mins_test <- apply( Test_df_cl_xgb, 2, min)
scaled_train <- as.data.frame(scale(Train_df_cl_xgb, center = mins_train, scale = maxs_train - mins_train))
scaled_test <- as.data.frame(scale(Test_df_cl_xgb, center = mins_test, scale = maxs_test - mins_test))

scaled_train[,24]=Train_df_cl$X40µm
scaled_test[,24]=Test_df_cl$X40µm

names(scaled_train)[names(scaled_train) == 'V24'] <- 'X40µm'
names(scaled_test)[names(scaled_test) == 'V24'] <- 'X40µm'
str(scaled_train);str(scaled_test)
scaled_test[is.na(scaled_test)]=0

k_fourmla1 = colnames(scaled_train[1:ncol(scaled_train)-1])
print(k_fourmla1)
k_fourmla =k_fourmla1[!k_fourmla1 %in% c("retart","dure")]#"Qualité","Poste"
print(k_fourmla)
c <- as.formula(paste(input_sortie,"~",paste(k_fourmla,collapse = "+")))
print(c)


nn <- neuralnet::neuralnet(c,data=scaled_train,hidden=c(24,4),linear.output=F)
plot7 <- plot(nn)
#train ------
pr.nn <- compute(nn,scaled_train[,1:ncol(scaled_train)-1])

nettrain_train =ifelse(pr.nn$net.result[,2]>0.5,1,0)
table(nettrain_train,Train_df_cl$X40µm)
#test 
pr.nn <- compute(nn,scaled_test[,1:ncol(scaled_test)-1])

nettrain_test =ifelse(pr.nn$net.result[,2]>0.5,1,0)
sum(diag(table(nettrain_test,Test_df_cl$X40µm)))/nrow(scaled_test)


Metrics::auc(actual = nettrain_test, predicted = Test_df_cl$X40µm)


#KNNN



knn(Train_df_cl_gbm[,1:16], Test_df_cl_gbm[,1:16], Train_df_cl$X40µm, k = 3, prob=TRUE)


sum(diag(table(pred_knn,Test_df_cl$X40µm)))/nrow(scaled_test)




pred_random = ifelse(rnorm(nrow(scaled_test),mean = 0.5,sd = 0.2)>0.5,1,0)
sum(diag(table(pred_random,Test_df_cl$X40µm)))/nrow(scaled_test)
Metrics::auc(actual = nettrain_test, predicted = Test_df_cl$X40µm)
sum(diag(table(nettrain_test,Test_df_cl$X40µm)))/nrow(scaled_test)


Metrics::auc(actual = nettrain_test, predicted = Test_df_cl$X40µm)


#SVM -not working yet 
dat = data.frame(x, y = as.factor(y))
library(e1071)
svmfit = svm(c, x = Train_df_cl_xgb[,1:16],y = , kernel = "sigmoid", cost = 10, scale = F,type = "nu-classification")
print(svmfit)


#glm 


model_glm = glm(c, data  = Train_df_cl,family = "binomial" )
pred_glm <- predict(model_glm, Test_df_cl, type = 'response')

pred_glm_class = ifelse(pred_glm>0.5,1,0)
sum(diag(table(pred_glm_class,Test_df_cl$X40µm)))/nrow(Test_df_cl)

#Rnn 
library(rnn)
c(dim(scaled_train),4)
x_train =array(as.matrix(scaled_train[1:14]),c(dim(scaled_train[1:14]),1))
model_rnn <- trainr(Y =t(scaled_train$X40µm) ,X = as.matrix(scaled_train[1:14]),
                    learningrate = 0.05,
                    hidden_dim = c(14,10),
                    numepochs = 100,
                    sigmoid = "logistic")



# Create 3d array: dim 1: samples; dim 2: time; dim 3: variables.
X <- array(scaled_train[1:14], dim=c(dim(scaled_train[1:14]),14) )
Y <- array( t(scaled_train$X40µm), dim=c(dim(t(scaled_train$X40µm)),1) ) 

# train the model
model <- trainr(Y=Y[,dim(Y)[2]:1,,drop=F],
                X=X[,dim(X)[2]:1,,drop=F],
                learningrate   =  0.1,
                hidden_dim     =  c(10,10),
                numepochs      =  5,
                batch_size     = 100,
                momentum       =0,
                use_bias       = F,
                learningrate_decay = 1)


















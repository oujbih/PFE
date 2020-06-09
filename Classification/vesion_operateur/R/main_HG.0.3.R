#########################################
# OUJBIH ABDERRAHIM              ########
# 05/04/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
#########################################

#Package
source("Packages.R")
#Function 
source("Function.R",encoding = "UTF-8")
#Load Data
source("Data_HR.R",encoding = "UTF-8")



# plot_data_evolution(Model_data,"sortie_date","Débit_CV004")
# 
# plot_data_density(Model_data,"Débit_CV004")
# ggsave("ev0.png",path = "images/version0.3",width =11,height = 6)


#Train and test ----------------------------------------------------------
tmp <- Makelag(Fevrier_DATA,"Débit_CV004",60)
tmp2 <- tmp[tmp$Débit_CV004 !=0,]

Model_data <- merge(x = My_data_CPT_HG, y = Fevrier_DATA, by.x = "sortie_date",by.y = "DATE", all.x = TRUE)
# plot_all(Model_data)
c(DATA.traintmp_regression, DATA.testmp_regression,index_train)%<-% MakeMergeParameterCPTSDP(0.8)

#-Dilution_HP18-Dilution_HP15-Dilution_HP14-Dilution_HP19-Arrosage_Crible_SC003-Dilution_SB002-CPT40-CPT_2500-CPT160-CPT125-CPT400-CPT_40
rf_rg=randomForest(X40µm~Pression_PK16+Débit_CV004+Pression_PK13+Pression_PK12+Pression_PK16+ CPT400+ CPT125 , data = DATA.traintmp_regression)
# rf_rg=randomForest(X40µm~Débit_CV004 , data = DATA.traintmp_regression)
# rf_rg=randomForest(X40µm~. , data = DATA.traintmp_regression)
randomForestHG()



#TODO
library(cloudml)
gcloud_install()

cloudml_train("Data_HR.R")


























#########################################
# OUJBIH ABDERRAHIM              ########
# 31/03/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
#########################################



# My_data <-read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "Main")
# Fevrier_DATA <- My_data %>% filter(DATE <="2020-02-24") %>% select(-`Energie Broyage HG`,-`Puissance Broyage HG`)
# My_data_sortie <- read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HGDataExtraction.xlsx",sheet = "Sortie") %>%  filter(Ligne =="HG")
# My_data_sortie <- Transform_sortie(My_data_sortie)
# My_data_CPT <- read_excel("C:/Users/OUJBIH/Desktop/Stage PFE/Excel/DATA/HG_DATA_Model.xlsx",sheet = "Entrées ")
# Fevrier_DATA=Fevrier_DATA %>% select(-Courant_ML002,-Courant_ML004)
# save(Fevrier_DATA,file="Data/Fevrier_DATA3.Rda")

# load("Data/My_data_CPT_Fevrier.Rda")
load("Data/Jointure_par_SDP/Model_SDP_Mean.Rda")

load("Data/Fevrier_DATA3.Rda")
load("Data/Mean_PM_HG.Rda")
# load("Data/My_data_sortie_LG.Rda")

# apply(is.na(Model_data),2,sum)
# 
# 
# Model_data <- merge(x = My_data_CPT_HG, y = Fevrier_DATA, by.x = "sortie_date",by.y = "DATE", all.x = TRUE)
# Model_data <- imputeTS::na_kalman(Model_data) #na_interpolation na_ma 
# Model_data <- Model_data %>% na.omit()
Model_data <- Model_SDP_Mean
# 

# #retart de parametre de machre 
# df_retartpm <- data.frame(Fevrier_DATA[0,])
# df_retartpm = df_retartpm[2:15]
# df_retartpm = rbind(df_retartpm,rep(0,14))
# colnames(df_retartpm) = colnames(Fevrier_DATA[2:15])
# 


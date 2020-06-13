#########################################
# OUJBIH ABDERRAHIM              ########
# 14/04/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
######################################### 
rm(list=ls())
source("R/Packages.R")
#Function
source("R/Function.R",encoding = "UTF-8")
source("R/Merge_HG.R",encoding = "UTF-8")
#Load Data
source("R/Data_HR.R",encoding = "UTF-8")
source("R/deps.R")
source("R/cards.R")

#body -----
body <- dashboardBody(
    
  chooseSliderSkin("Nice"),    
  tabItems(
  # Home --------------------
  tabItem("Home",
          fluidRow(
            column( class="side_bar",
                    width = 3,
                    
                    selectInput("Inputs", "Inputs",choices =c(colnames(Model_data)),selected ="Débit_CV004", width = "90%"),
                    selectInput("Inputs2", "Inputs2",choices =c(colnames(Model_data)),selected ="Débit_CV004", width = "90%"),
                    checkboxInput("checkbox", label = "Fonction densité", value = FALSE),
                    sliderInput(
                      "gaugeVal",
                      "Gauge Value:",
                      min = 0,
                      max = 100,
                      value = 50
                    )
            ),
            column(class="main_bar",
                   width = 9,
                   
                   tablerCard(
                     title = "Evolutions",
                     
                     plotOutput("plot1",width = "100%"),
                     plotOutput("plot11",width = "100%"),
                     plotOutput("plot2",width = "100%"),
                     width = 12,
                     overflow = F
                   )
                   
            )
          ) #fin row 
  ),#fin model
  
  # parametres de marche 
  tabItem("pmarche",
          fluidRow(
            column( class="side_bar",
                    width = 3,
                    selectInput("Inputs3", "Inputs",choices =c(colnames(Fevrier_DATA)),selected ="Débit_CV004", width = "90%"),
                    fluidRow(column(width = 5,
                                    actionButton("Train2", label = "Train",style = "width: 90%;background-color: white;")
                    ),
                    column(width = 5,
                           actionButton("Test2", label = "Test",style = "width: 90%;background-color: white;"))),
                    br(),
                    sliderInput("part1","filter valuer",min = 0,max = 1000,value = 50)
            ),
            column(class="main_bar",
                   width = 9,
                   
                   tablerCard(
                     title = "Evolutions",
                     
                     plotOutput("plotp1",width = "100%"),
                     
                     width = 12,
                     overflow = F
                   )
                   
            )
          ) #fin  
  ),#fin parametres de marche 
  
  # Development ------
  # D rg -----------------
  tabItem("D_Regression",
    useShinyjs(), 
    fluidRow(
      column( class="side_bar", 
              
              width = 3,
              tags$strong("HG"),
              br(),
              br(),
              selectInput("sortie_rg", "Sortie",choices =c("X40µm","X250µm","D80"),selected ="X40µm", width = "90%"),
              selectInput("var_merge_rg", "Jointure par",choices =c("CPT","Parametres","SDP","SDP sans CPT"),selected ="SDP", width = "90%"),
              uiOutput("regle_jointure_rg"),
              uiOutput("regle_jointure2_rg"),
              selectInput("Models_rg", "Models",choices =c("Arbre de décision","bagged trees","random forest","GBM","Xgboost","neuralnet","all"),selected ="random forest", width = "90%"),
              uiOutput("hidden_layer_rg"),
              pickerInput(width = "95%",
                inputId = "myPicker_rg", 
                label = "Inputs", 
                choices = colnames(Model_data[!colnames(Model_data) %in% c("X250µm","X40µm","sortie_date","D80")]), 
                options = list(`actions-box` = TRUE,style = "background: white;",size = 10,`selected-text-format` = "count > 3"),
                choicesOpt = list(style = rep(("color: black; font-weight: bold;background-color: white;"),26)),
                multiple = TRUE
              ),
              sliderInput("split_rg","Split Value:",min = 0,max = 1,value = 0.7,sep = 0.05),
              fluidRow(column(width = 5,
                              actionButton("btn_train_rg", label = "Train",style = "width: 90%;background-color: white;"),
                              checkboxInput("checkbox2_rg", label = "par heurs", value = FALSE)
              ),
              column(width = 5,
                     actionButton("btn_rapport_rg", label = "Rapport",style = "width: 90%;background-color: white;"),
                     checkboxInput("cbox_cor_rg", label = "Correction", value = FALSE))),
              
              
              textInput("par_heurs_rg",label = "En minutes", value = 60),
              checkboxInput("save_models_rg", label = "Enregistrer les modèles", value = FALSE)

      ),
      column(class="main_bar", 
             width = 9,
             align="center",

             tablerCard(
               title = "Evolutions",
               options = tagList(
                 
               ),
               fluidRow(
                  column(width = 6,echarts4rOutput("train", height = "300px")),
                  column(width = 6,echarts4rOutput("test", height = "300px"))),
               tableOutput('table_rg'),
               plotOutput("plot3",width = "100%"),
               plotOutput("plot4",width = "100%"),
               plotOutput("plot5",width = "100%"),
               plotOutput("plot6",width = "100%"),
               plotOutput("plot7",width = "100%"),
               width = 12,
               overflow = F,
               collapsed=  F
             )
             
      )
    ) #fin row 
          
  ),#fin D_rg
  
  
  # D_Classification ------
  tabItem("D_Classification",
          
          fluidRow(
            column( class="side_bar", 
                    
                    width = 3,
                    tags$strong("HG"),
                    br(),
                    br(),
                    selectInput("sortie_cl", "Sortie",choices =c("X40µm","X250µm","Les deux"),selected ="X40µm", width = "90%"),
                    selectInput("var_merge_cl", "Jointure par",choices =c("CPT","Parametres","SDP","SDP sans CPT"),selected ="SDP", width = "90%"),
                    uiOutput("regle_jointure_cl"),
                    uiOutput("regle_jointure2_cl"),
                    
                    uiOutput("hidden_layer_cl"),
                    pickerInput(width = "95%",
                                inputId = "myPicker_cl", 
                                label = "Inputs", 
                                choices = colnames(Model_data[!colnames(Model_data) %in% c("X250µm","X40µm","sortie_date","D80")]), 
                                options = list(`actions-box` = TRUE,style = "background: white;",size = 10,`selected-text-format` = "count > 3"),
                                choicesOpt = list(style = rep(("color: black; font-weight: bold;background-color: white;"),26)),
                                multiple = TRUE
                    ),
                    sliderInput("split_cl","Split Value:",min = 0,max = 1,value = 0.7,sep = 0.05),
                    fluidRow(column(width = 5,
                                    actionButton("btn_train_cl", label = "Train",style = "width: 90%;background-color: white;"),
                                    checkboxInput("checkbox2_cl", label = "par heurs", value = FALSE)
                    ),
                    column(width = 5,
                           actionButton("btn_rapport_cl", label = "Rapport",style = "width: 90%;background-color: white;"),
                           checkboxInput("cbox_cor_cl", label = "Correction", value = FALSE))),
                    
                    
                    textInput("par_heurs_cl",label = "En minutes", value = 60),
                    checkboxInput("save_models_cl", label = "Enregistrer les modèles", value = FALSE)
                    
            ),
            column(class="main_bar", 
                   width = 9,
                   align="center",
                   tablerCard(
                     title = "Résultats",
                     tableOutput('table_cl'),
                     plotOutput("plot3_cl",width = "400px"),
                     
                     width = 12,
                     overflow = F,
                     collapsed=  F
                   )
            )
          ) #fin row 
          
  ),#fin HG
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Moldels -------
  tabItem("M_Regression", 
          fluidRow(
            column( class="side_bar",
                    width =3,
                    selectInput("sortie_rg_M", "Sortie",choices =c("X40µm","X250µm","Les deux"),selected ="X40µm", width = "90%"),
                    selectInput("Poste_rg_M", "Poste",choices =c("P1","P2","P3"),selected ="P1", width = "90%"),
                    selectInput("Qualité_rg_M", "Qualité",choices =c("BTRSC","BTRBA","MTBA","BTNBA","BTNSC"),selected ="BTRBA", width = "90%"),
                    fluidRow(column(width = 5,
                                    checkboxInput("checkbox_cpt_rg_M", label = "Ajouter la CPT", value = TRUE)
                    ),
                    column(width = 5,
                           checkboxInput("checkbox_retart_rg_M", label = "retart", value = TRUE))),
                    uiOutput("CPT_rg_M")
                    
                    
                    
            ),
            column( class="side_bar",
                    width =3,
                    # 
                    lapply(c("Débit_CV004","Dilution_SB002","Arrosage_Crible_SC003","Dilution_HP14","Dilution_HP15","Dilution_HP18"),function(c){

                      textInput(c,c,value =  Mean_PM_HG[c])

                    }),
                    uiOutput("retart")
            ),
            column( class="side_bar",
                    width =3,
                    
                    lapply(colnames(Fevrier_DATA[8:ncol(Fevrier_DATA)]),function(c){
                      
                      textInput(c,c,value = Mean_PM_HG[c])
                      
                    }),
                    uiOutput("dure"),
                    fluidRow(column(width = 5,
                                    actionButton("btn_Calculer_rg_M", label = "Calculer",style = "width: 90%;background-color:white;")
                    ),
                    column(width = 5,
                           actionButton("btn_Rappor_rg_M", label = "Rapport",style = "width: 90%;background-color: white;")))
                    
                    
                    
            ),
            column(class="main_bar",
                   width = 3,
                   tablerCard(
                     title =NULL,
                     width = 12,
                     overflow = T,
                     collapsed=  F,
                     tags$h3("résultats :Regression",style = "width: 90%;color:red;"),
                     textOutput("resultats"),
                     tags$head(tags$style("#resultats{color: #0d6060;
                                 font-size: 71px;
                                font-family: Lato;
                                 margin-top: 50px;
                                 }")),
                     plotOutput("plot1988",width = "100%")
                   )
            )
          ) #fin  
  ),#Fin model
  #M classification
  tabItem("M_Classification", 
          fluidRow(
            column( class="side_bar",
                    width =3,
                   
                    selectInput("sortie_cl_M", "Sortie",choices =c("X40µm","X250µm","Les deux"),selected ="X40µm", width = "90%"),
                    selectInput("Poste_cl_M", "Poste",choices =c("P1","P2","P3"),selected ="P1", width = "90%"),
                    selectInput("Qualité_cl_M", "Qualité",choices =c("BTRSC","BTRBA","MTBA","BTNBA","BTNSC"),selected ="BTRBA", width = "90%"),
                    fluidRow(column(width = 5,
                                    checkboxInput("checkbox_cpt_cl_M", label = "Ajouter la CPT", value = TRUE)
                    ),
                    column(width = 5,
                           checkboxInput("checkbox_retart_cl_M", label = "retart", value = TRUE))),
                    uiOutput("CPT_cl_M")
                    
                    
                    
            ),
            column( class="side_bar",
                    width =3,
                    # 
                    lapply(c("Débit_CV004","Dilution_SB002","Arrosage_Crible_SC003","Dilution_HP14","Dilution_HP15","Dilution_HP18"),function(c){
                      
                      textInput(paste0(c,'_cl'),c,value =  Mean_PM_HG[c])
                      
                    }),
                    uiOutput("retart_cl")
            ),
            column( class="side_bar",
                    width =3,
                    
                    lapply(colnames(Fevrier_DATA[8:ncol(Fevrier_DATA)]),function(c){
                      
                      textInput(paste0(c,'_cl'),c,value = Mean_PM_HG[c])
                      
                    }),
                    uiOutput("dure_cl"),
                    fluidRow(column(width = 5,
                                    actionButton("btn_Calculer_cl_M", label = "Calculer",style = "width: 90%;background-color:white;")
                    ),
                    column(width = 5,
                           actionButton("btn_Rappor_cl_M", label = "Rapport",style = "width: 90%;background-color: white;")))
                    
                    
                    
            ),
            column(class="main_bar",
                   width = 3,
                   tablerCard(
                     title =NULL,
                     width = 12,
                     overflow = T,
                     collapsed=  F,
                     tags$h3("résultats :Classification",style = "width: 90%;color:red;"),
                     textOutput("resultats_cl"),
                     tags$head(tags$style("#resultats_cl{color: #0d6060;
                                 font-size: 71px;
                                font-family: Lato;
                                 margin-top: 50px;
                                 }")),
                     plotOutput("plot_cl",width = "100%")
                   )
            )
          ) #fin  
  )#Fin model
))

  
#ui ------
ui <- function(req) {
 
  dashboardPagePlus(
    title = "OCP Beni Amir",
    enable_preloader = F,
    loading_duration = 0.5,
    header = header,
    sidebar = sidebar,
    body = addDeps( body),
    skin = "green-light" # -light
  )
  
}
# Define server ----


server <- function(input, output,session) {
  #Load data -----
  My_data <- reactiveValues(
    Model=Model_data,
    cbox_cor_rg="ncorrected",
    cbox_cor_cl="ncorrected"
  )
  
  #update selectinput----
  observe({
    updateSelectInput(session, "Inputs",
                      label = "Inputs",
                      choices = colnames(My_data$Model),
                      selected = "Débit_CV004")
    updateSelectInput(session, "Inputs2",
                      label = "Inputs2",
                      choices = colnames(My_data$Model),
                      selected = "Débit_CV004")
    updatePickerInput(session,
                      inputId = "myPicker_rg", 
                      label = "Inputs", 
                      choices = colnames(My_data$Model[!colnames(My_data$Model) %in% c("X250µm","X40µm","sortie_date","D80")]),
                      choicesOpt = list(style = rep(("color: black; font-weight: bold;background-color: white;"),26)))
    updatePickerInput(session,
                      inputId = "myPicker_cl", 
                      label = "Inputs", 
                      choices = colnames(My_data$Model[!colnames(My_data$Model) %in% c("X250µm","X40µm","sortie_date","D80")]),
                      choicesOpt = list(style = rep(("color: black; font-weight: bold;background-color: white;"),26)))
                    
  })
  
  
  #hiddien UI -------------------
  observeEvent(input$Models_rg,{
    if (input$Models_rg=="neuralnet"){
      output$hidden_layer_rg <- renderUI({ textInput("hidden_layer_value_rg","hidden layer",width = "90%",value = "4,5")})}
    else{
      output$hidden_layer_rg <- renderUI({ })
    }
  })
  observeEvent(input$var_merge_rg,{
    if (input$var_merge_rg=="CPT"){
      output$regle_jointure_rg <- renderUI({ selectInput("regle_jointure_value_rg", "Régle de Joinutre SDP",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      output$regle_jointure2_rg <- renderUI({ selectInput("regle_jointure_value2_rg", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      
    }else{
      if (input$var_merge_rg=="SDP" | input$var_merge_rg=="SDP sans CPT"){
        output$regle_jointure_rg <- renderUI({ selectInput("regle_jointure_value2_rg", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
        output$regle_jointure2_rg <- renderUI({ })
      }else{
        
        output$regle_jointure_rg <- renderUI({ })
        output$regle_jointure2_rg <- renderUI({ })
      }
    }
  })
  observeEvent(input$var_merge_cl,{
    if (input$var_merge_cl=="CPT"){
      output$regle_jointure_cl <- renderUI({ selectInput("regle_jointure_value_cl", "Régle de Joinutre SDP",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      output$regle_jointure2_cl <- renderUI({ selectInput("regle_jointure_value2_cl", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      
    }else{
      if (input$var_merge_cl=="SDP" | input$var_merge_cl=="SDP sans CPT"){
        output$regle_jointure_cl <- renderUI({ selectInput("regle_jointure_value2_cl", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
        output$regle_jointure2_cl <- renderUI({ })
      }else{
        
        output$regle_jointure_cl <- renderUI({ })
        output$regle_jointure2_cl <- renderUI({ })
      }
    }
  })
  observeEvent(input$cbox_cor_rg,{
    if(input$cbox_cor_rg){
      My_data$cbox_cor_rg ="corrected"
    }else{
      My_data$cbox_cor_rg ="ncorrected" 
    }
    
  })
  observeEvent(input$cbox_cor_cl,{
    if(input$cbox_cor_cl){
      My_data$cbox_cor_cl ="corrected"
    }else{
      My_data$cbox_cor_cl ="ncorrected" 
    }
    
  })
  #Homme -----
  output$data <- renderDataTable(
    My_data$Model)
  output$plot1 <-  renderPlot({
    plot_data_evolution(My_data$Model,"sortie_date",input$Inputs)
  })
  output$plot11 <-  renderPlot({
    plot_data_evolution(My_data$Model,"sortie_date",input$Inputs2)
  })
  output$plot2 <- renderPlot({
    if (input$checkbox == TRUE){
      plot_data_density(My_data$Model,input$Inputs)
   
    }
  })
  
  #Parametres de marche ----
  output$plotp1 <- renderPlot(
    plot_data_evolution(My_data$Model,"sortie_date",input$Inputs3)
  )
  
  observeEvent(input$Train2,{
    My_data$Model <- My_data$Model %>% filter(Débit_CV004 > as.integer(input$part1))
      output$plotp1 <- renderPlot(
        plot_data_evolution(My_data$Model,"sortie_date",input$Inputs3)
      )
      
  })
  
  

  

  
 
  # ###############################################
  # Development ###################################
  # Regression  ------------------------------#####
  # Rapprot     ###################################
  observeEvent(input$btn_rapport_rg,{
    
    
  })
  # Train btn HG -------
  observeEvent(input$btn_train_rg,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      My_data$Model <- merge_variableHG(input$var_merge_rg,input$regle_jointure_value_rg,input$regle_jointure_value2_rg,a  =input$par_heurs_rg,par_hours = input$checkbox2_rg,Fevrier_DATA)
      print("Merge done")
      c(Train_df_rg, Test_df_rg,index_train)%<-% MakeMergeParameterCPTSDP(My_data$Model,input$split_rg,F,input$sortie_rg)
      print("Spliting done")
      if(is.null(input$myPicker_rg)){
        k_fourmla = colnames(Train_df_rg[1:ncol(Train_df_rg)-1])
        c <- as.formula(paste(input$sortie_rg,"~",paste(k_fourmla,collapse = "+")))
      }else{
        c <- as.formula(paste(input$sortie_rg,"~",paste(input$myPicker_rg,collapse = "+")))
      }
      print("Model----------------------")
      if(input$Models_rg=="Arbre de décision"){
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% rpartHG(c,My_data$Model,Train_df_rg,Test_df_rg,index_train,input$sortie_rg)
        
      }else if(input$Models_rg=="bagged trees"){
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% bagged_tree(c,My_data$Model,Train_df_rg,Test_df_rg,index_train,input$sortie_rg)
      }else if(input$Models_rg=="random forest"){
        rf_rg=randomForest(c, data = Train_df_rg)
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% randomForestHG(rf_rg,My_data$Model,Train_df_rg,Test_df_rg,index_train,input$sortie_rg)
      }else if(input$Models_rg=="GBM"){
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% GBM_HG(c,My_data$Model,Train_df_rg,Test_df_rg,index_train,input$sortie_rg)
      }else if(input$Models_rg=="Xgboost"){
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% xgboost_HG(c,My_data$Model,Train_df_rg,Test_df_rg,index_train,input$sortie_rg)
      }else if (input$Models_rg=="neuralnet"){
        hidden_layer_value_rg <- strsplit(input$hidden_layer_value_rg,",")[[1]]
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% neuralnet(c,My_data$Model,as.numeric(hidden_layer_value_rg),Train_df_rg,Test_df_rg,index_train,input$sortie_rg)
      }else if (input$Models_rg=="all"){
        c(df_result,plot_max,c) %<-% F_Main_rg_HG(dataframe =My_data$cbox_cor_rg,input_formula = input$myPicker_rg,input_sortie=input$sortie_rg,
                                                  input_var_merge =input$var_merge_rg ,regle_jointure_value=input$regle_jointure_value_rg,regle_jointure_value2=input$regle_jointure_value2_rg,
                                                  a_hours =input$par_heurs_rg,par_hours = input$checkbox2_rg,k_spilt=input$split_rg,save_models=input$save_models_rg )
        hide("train")
        hide("test");hide("plot4");hide("plot5");hide("plot6");hide("plot7")
        show("table_rg")
        output$table_rg <- renderTable(df_result)
        output$plot3 <-  renderPlot({plot_max})}
      if(input$Models_rg!="all"){
        show("train")
        show("test");show("plot4");show("plot5");show("plot6");show("plot7")
        hide("table_rg")
        output$plot3 <-  renderPlot({g})           #scatter plot 
        output$plot4 <-  renderPlot({goutput2})    #test train spliting plot
        output$plot5 <-  renderPlot({plot5})       #train prediction
        output$plot6 <-  renderPlot({plot6})       #test prediction
        output$plot7 <-  renderPlot({plot7})       #variable importantes
       
        output$train <- renderEcharts4r({
          e_charts() %>%
            e_gauge(round(as.numeric(R_train),3)*100, "Train")
        })
        output$test <- renderEcharts4r({
          e_charts() %>%
            e_gauge(round(as.numeric(R_test),3)*100, "Test")
    })}
  })})
  # classification 
  observeEvent(input$btn_train_cl,{ 
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      
      
      c(df_result,rocs,m,c) %<-% F_Main_HG(dataframe =My_data$cbox_cor_cl,input_formula = input$myPicker_cl,input_sortie=input$sortie_cl,
                                           input_var_merge =input$var_merge_cl ,regle_jointure_value=input$regle_jointure_value_cl,regle_jointure_value2=input$regle_jointure_value2_cl,
                                           a_hours =input$par_heurs_cl,par_hours = input$checkbox2_cl,k_spilt=input$split_cl,save_models=input$save_models_cl)
      
      
      
      output$table_cl <- renderTable(df_result)
      
      
      
      output$plot3_cl <-  renderPlot({
        plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
        legend(x = "bottomright", 
               legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM","XGB","ANN","KNN","glm"),
               fill = 1:m,
               cex = 1)
        
        
      })
      
      
      
    })
    
  })
  
  # ###############################################
  # Model      ###################################
  # Regression  ------------------------------#####
  # Rapprot     ###################################
  #rg -------------------
  
  observeEvent(input$checkbox_retart_rg_M,{
    if (input$checkbox_retart_rg_M){
      output$retart <- renderUI({textInput("retart","retart",value = 0)})
      output$dure <- renderUI({textInput("dure","durée",value = 0)})
    }else{
      output$dure <- renderUI({})
      output$retart <- renderUI({})
      
    }})
  observeEvent(input$checkbox_cpt_rg_M,{
    if (input$checkbox_cpt_rg_M){
      load("R/Classification/input_test.Rda")
      output$CPT_rg_M <- renderUI({ 
        lapply(c("CPT_2500","CPT400","CPT160","CPT125","CPT40","CPT_40"),function(c){
          
          textInput(c,c,value = input_test[[c]])
         
        })
        }) 
      
    }else{
      output$CPT_rg_M <- renderUI({})
      load("R/Classification/input_test.Rda")
      input_test=input_test %>% select(-3:-8)
    }})
  observeEvent(input$checkbox_retart_cl_M,{
    if (input$checkbox_retart_cl_M){
      output$retart_cl <- renderUI({textInput("retart_cl","retart",value = 0)})
      output$dure_cl <- renderUI({textInput("dure_cl","durée",value = 0)})
    }else{
      output$retart_cl <- renderUI({})
      output$dure_cl <- renderUI({})
      
    }})
  observeEvent(input$checkbox_cpt_cl_M,{
    if (input$checkbox_cpt_cl_M){
      load("R/Classification/input_test.Rda")
      output$CPT_cl_M <- renderUI({ 
        lapply(c("CPT_2500","CPT400","CPT160","CPT125","CPT40","CPT_40"),function(c){
          
          textInput(paste0(c,'_cl'),c,value = input_test[[c]])
          
        })
      }) 
      
    }else{
      output$CPT_cl_M <- renderUI({})
      load("R/Classification/input_test.Rda")
      input_test=input_test %>% select(-3:-8)
    }})
  
  
  
  #btn-calculer-regression
  observeEvent(input$btn_Calculer_rg_M,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {
        
      if (input$checkbox_cpt_rg_M){
        load("Data/test/input_test.Rda") 
        load_path =make_path_rg("SDP",input$sortie_rg_M)
      }else{
        
        load("Data/test/input_test.Rda")
        input_test=input_test %>% select(-3:-8)
        load_path =make_path_rg("SDP sans CPT",input$sortie_rg_M)
      }
     
      output$resultats=renderText("")
      input_test$Poste[1]=input$Poste_rg_M
      input_test$Qualité[1]=input$Qualité_rg_M
      for(c in colnames(input_test[3:ncol(input_test)])){
        input_test[[c]][1]=as.numeric(input[[c]])
      }
      
      load(paste0(dirname(load_path),"/treatplan.rds"))
      load(paste0(dirname(load_path),"/newvars.rds"))
      model_1<- readRDS(file = paste0(load_path,"tree.rds"))
      model_2<- readRDS(file = paste0(load_path,"BaggedTree.rds"))
      model_rf<- readRDS(file = paste0(load_path,"rf.rds"))
      model_gbm<- readRDS(file = paste0(load_path,"GBM.rds"))
      model_xgb<- readRDS(file = paste0(load_path,"xgboost.rds"))
      model_ann<- readRDS(file = paste0(load_path,"ann.rds"))
      
      
      #levels correction 
      input_test$Qualité =droplevels(input_test$Qualité)
      levels(input_test$Qualité) = model_rf$forest$xlevels$Qualité
      
      model_1_pred <- predict(object = model_1,newdata = input_test) 
      model_2_pred <- predict(object = model_2,newdata = input_test) 
      model_rf_pred <- predict(object = model_rf,newdata = input_test) 
      model_gbm_pred <- predict(object = model_gbm,newdata = input_test,n.trees = 500,type = "response") 
      input_test_xgb= vtreat::prepare(treatplan, input_test, varRestriction = newvars)
      model_xgb_pred <- predict(object = model_xgb,newdata = as.matrix(input_test_xgb))
      resutl = mean(c(model_1_pred,model_2_pred,model_rf_pred,model_gbm_pred,model_xgb_pred))
      resutl = round(resutl,2)
      print(resutl)
      
      output$resultats=renderText(resutl)
      
        
    })})
  #btn-calculer-classification  
  observeEvent(input$btn_Calculer_cl_M,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      
      if (input$checkbox_cpt_cl_M){
        load("Data/test/input_test.Rda") 
        load_path =make_path("SDP",input$sortie_rg_M)
      }else{
        
        load("Data/test/input_test.Rda")
        input_test=input_test %>% select(-3:-8)
        load_path =make_path("SDP sans CPT",input$sortie_rg_M)
      }
      output$resultats=renderText("")
      input_test$Poste[1]=input$Poste_cl_M
      input_test$Qualité[1]=input$Qualité_cl_M
      for(c in colnames(input_test[3:ncol(input_test)])){
        input_test[[c]][1]=as.numeric(input[[paste0(c,'_cl')]])
      }
      
      load(paste0(dirname(load_path),"/treatplan.rds"))
      load(paste0(dirname(load_path),"/newvars.rds"))
      model_1<- readRDS(file = paste0(load_path,"tree.rds"))
      model_2<- readRDS(file = paste0(load_path,"BaggedTree.rds"))
      model_rf<- readRDS(file = paste0(load_path,"rf.rds"))
      model_gbm<- readRDS(file = paste0(load_path,"GBM.rds"))
      model_xgb<- readRDS(file = paste0(load_path,"xgboost.rds"))
      model_ann<- readRDS(file = paste0(load_path,"ann.rds"))
      model_glm<- readRDS(file = paste0(load_path,"glm.rds"))
      
      #levels correction 
      input_test$Qualité =droplevels(input_test$Qualité)
      levels(input_test$Qualité) = model_rf$forest$xlevels$Qualité
      
      model_1_pred <- predict(object = model_1,newdata = input_test,type = "prob") 
      model_2_pred <- predict(object = model_2,newdata = input_test,type = "prob") 
      model_rf_pred <- predict(object = model_rf,newdata = input_test,type = "prob") 
      model_gbm_pred <- predict(object = model_gbm,newdata = input_test,n.trees = 500,type = "response") 
      input_test_xgb= vtreat::prepare(treatplan, input_test, varRestriction = newvars)
      model_xgb_pred <- predict(object = model_xgb,newdata = as.matrix(input_test_xgb))
      # model_ann_pred <- compute(object = model_ann,  input_test_xgb) 
      model_glm_pred <- predict(object = model_glm,newdata = input_test,type = "response") 
      resutl = mean(c(model_1_pred[1,2],model_2_pred[1,2],model_rf_pred[1,2],model_gbm_pred,model_xgb_pred,model_glm_pred[1]))
      print(resutl)
      output$resultats_cl=renderText(ifelse(resutl>0.5,"Good","Bad"))
      
      
      
    })})
  
  
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server,onStart = function() {

  onStop(function() {
    
    rm(list=ls(all=TRUE))
    rm(list = ls())
  })})



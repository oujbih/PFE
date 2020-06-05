#########################################
# OUJBIH ABDERRAHIM              ########
# 14/04/2020                     ########
# OCP SA Beni Amir Khouribga PFE ########
######################################### 
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

  # HG ------
  tabItem("Regression",

    fluidRow(
      column( class="side_bar", #-------
              
              width = 3,
              tags$strong("HG"),
              br(),
              br(),
              selectInput("sortie", "Sortie",choices =c("X40µm","X250µm","Les deux"),selected ="X40µm", width = "90%"),
              selectInput("var_merge", "Jointure par",choices =c("CPT","Parametres","SDP","SDP sans CPT"),selected ="SDP", width = "90%"),
              uiOutput("regle_jointure"),
              uiOutput("regle_jointure2"),
              
              uiOutput("hidden_layer"),
              pickerInput(width = "95%",
                inputId = "myPicker", 
                label = "Inputs", 
                choices = colnames(Model_data[!colnames(Model_data) %in% c("X250µm","X40µm","sortie_date","D80")]), 
                options = list(`actions-box` = TRUE,style = "background: white;",size = 10,`selected-text-format` = "count > 3"),
                choicesOpt = list(style = rep(("color: black; font-weight: bold;background-color: white;"),26)),
                multiple = TRUE
              ),
              sliderInput("split","Split Value:",min = 0,max = 1,value = 0.7,sep = 0.05),
              fluidRow(column(width = 5,
                              actionButton("Train", label = "Train",style = "width: 90%;background-color: white;"),
                              checkboxInput("checkbox2", label = "par heurs", value = FALSE)
                              ),
                       column(width = 5,
                              actionButton("btn_rapport", label = "Rapport",style = "width: 90%;background-color: white;"),
                              checkboxInput("cbox_cor", label = "Correction", value = FALSE))),
              
              
              textInput("par_heurs",label = "En minutes", value = 60)

      ),
      column(class="main_bar", #-----
             width = 9,
             align="center",
             tablerCard(
               title = "Résultats",
               tableOutput('table'),
               plotOutput("plot3",width = "400px"),
        
               width = 12,
               overflow = F,
               collapsed=  F
             )
      )
    ) #fin row 
          
  ),#fin HG
  #Model classification -------
  tabItem("Classification", 
          fluidRow(
            column( class="side_bar",
                    width =3,
                    
                    selectInput("sortie_cl", "Sortie",choices =c("X40µm","X250µm","Les deux"),selected ="X40µm", width = "90%"),
                    selectInput("Poste", "Poste",choices =c("P1","P2","P3"),selected ="P1", width = "90%"),
                    selectInput("Qualité", "Qualité",choices =c("BTRSC","BTRBA","MTBA","BTNBA","BTNSC"),selected ="BTRBA", width = "90%"),
                    fluidRow(column(width = 5,
                                    checkboxInput("checkbox_cpt", label = "Ajouter la CPT", value = TRUE)
                    ),
                    column(width = 5,
                           checkboxInput("checkbox_retart", label = "retart", value = TRUE))),
                   
                    uiOutput("CPT"),
                    
                    
                    
                    
                    
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
                                    actionButton("btn_Calculer", label = "Calculer",style = "width: 90%;background-color:white;")
                    ),
                    column(width = 5,
                           actionButton("btn_Rapport2", label = "Rapport",style = "width: 90%;background-color: white;")))
                    
                    
            ),
            column(class="main_bar",
                   width = 3,
                   tablerCard(
                     title =NULL,
                     width = 12,
                     overflow = T,
                     collapsed=  F,
                     tags$h3("résultats:",style = "width: 90%;color:red;"),
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
    cbox_cor="ncorrected"
  )
  
  #update selectinput----
  observe({
    updatePickerInput(session,
                      inputId = "myPicker", 
                      label = "Inputs", 
                      choices = colnames(My_data$Model[!colnames(My_data$Model) %in% c("X250µm","X40µm","sortie_date","D80")]),
                      choicesOpt = list(style = rep(("color: black; font-weight: bold;background-color: white;"),26)))
                    
  })

  
  

  
  
  #CLassification Model------------
 
  observeEvent(input$var_merge,{
    if (input$var_merge=="CPT"){
      output$regle_jointure <- renderUI({ selectInput("regle_jointure_value", "Régle de Joinutre SDP",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      output$regle_jointure2 <- renderUI({ selectInput("regle_jointure_value2", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      
    }else{
      if (input$var_merge=="SDP" | input$var_merge=="SDP sans CPT"){
        output$regle_jointure <- renderUI({ selectInput("regle_jointure_value2", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
        output$regle_jointure2 <- renderUI({ })
      }else{
        
        output$regle_jointure <- renderUI({ })
        output$regle_jointure2 <- renderUI({ })
      }
    }
    
      
    
    
  })
  observeEvent(input$cbox_cor,{
    if(input$cbox_cor){
    My_data$cbox_cor ="corrected"
    }else{
      My_data$cbox_cor ="ncorrected" 
    }

  })
 
  
  # Train btn HG -------
  observeEvent(input$Train,{ 
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      

      c(df_result,rocs,m,c) %<-% F_Main_HG(dataframe =My_data$cbox_cor,input_formula = input$myPicker,input_sortie=input$sortie,
                                                     input_var_merge =input$var_merge ,regle_jointure_value=input$regle_jointure_value,regle_jointure_value2=input$regle_jointure_value2,
                                                     a_hours =input$par_heurs,par_hours = input$checkbox2,k_spilt=input$split )
      
      
      
      output$table <- renderTable(df_result)
      
      
      
      output$plot3 <-  renderPlot({
        plot(rocs, col = as.list(1:m), main = "Test Set ROC Curves")
        legend(x = "bottomright", 
               legend = c("Decision Tree", "Bagged Trees", "Random Forest", "GBM","XGB","ANN","KNN","glm"),
               fill = 1:m,
               cex = 1)
        
        
      })
      
     
      
    })
  
  })
  
  
  # Train btn Rapport -------
  observeEvent(input$btn_rapport,{ 
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      
      
      knitr::opts_chunk$set(error = TRUE)
      
      
      knit2pdf("Latex/rapport_HG.Rnw", compiler = 'xelatex')
      knit2pdf("Latex/rapport_HG_Sans_CPT.Rnw", compiler = 'xelatex')
      
      
    })
  })
  
  
  
  
  
  
  
  
  
  #Classification -------------------
  #checkbox ----
  observeEvent(input$checkbox_cpt,{
    if (input$checkbox_cpt){
      output$CPT <- renderUI({ 
        lapply(c("CPT_2500","CPT400","CPT160","CPT125","CPT40","CPT_40"),function(c){
          load("Data/Classification/input_test.Rda") 
          textInput(c,c,value = input_test[[c]])
          
        })
        }) 
      
    }else{
      output$CPT <- renderUI({})
      load("Data/Classification/input_test.Rda")
      input_test=input_test %>% select(-3:-8)
    }})
  observeEvent(input$checkbox_retart,{
    if (input$checkbox_retart){
      output$retart <- renderUI({textInput("retart","retart",value = 0)})
      output$dure <- renderUI({textInput("dure","durée",value = 0)})
    }else{
      output$dure <- renderUI({})
      output$retart <- renderUI({})
      
    }})
  


  #btn-calculer 
  observeEvent(input$btn_Calculer,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      print(getwd())
      if (input$checkbox_cpt){
        load("Data/Classification/input_test.Rda") 
        load_path =make_path("SDP",input$sortie)
      }else{
        
        load("Data/Classification/input_test.Rda")
        input_test=input_test %>% select(-3:-8)
        load_path =make_path("SDP sans CPT",input$sortie)
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
      
      
      output$resultats=renderText("")
      input_test$Poste[1]=input$Poste
      input_test$Qualité[1]=input$Qualité
      for(c in colnames(input_test[3:ncol(input_test)])){
        input_test[[c]][1]=as.numeric(input[[c]])
      }
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
      output$resultats=renderText(ifelse(resutl>0.5,"Good","Bad"))
      
        
    })})
  
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server,onStart = function() {

  onStop(function() {
    
    rm(list=ls(all=TRUE))
    rm(list = ls())
  })})



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
  
  # parametres de marche --------------------
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
  # retart PM --------------------
  tabItem("retartpm",
          fluidRow(
            column( class="side_bar",
                    width =3,
                    lapply(colnames(Fevrier_DATA[2:6]),function(c){
                      
                    sliderInput(c,c,min = 0,max = 60,value = 0)
                      
                    }),
                    
                    fluidRow(column(width = 5,
                                    actionButton("Train_retartpm", label = "retart",style = "width: 90%;background-color: white;")
                    ),
                    column(width = 5,
                           actionButton("Test2", label = "Test",style = "width: 90%;background-color: white;")))
                   
            ),
            column(class="main_bar",
                   width = 3,
                   lapply(colnames(Fevrier_DATA[6:10]),function(c){
                     
                     sliderInput(c,c,min = 0,max = 60,value = 0)
                     
                   })
                   
            ),
            column(class="main_bar",
                   width = 3,
                   lapply(colnames(Fevrier_DATA[10:13]),function(c){
                     
                     sliderInput(c,c,min = 0,max = 60,value = 0)
                     
                   })
                   
            )
          ) #fin  
  ),#fin retartpm
  # Merge PM --------------------
  tabItem("Merge",
          fluidRow(
            column( class="side_bar",
                    width =3,
                  
                  
                    br(),
                    selectInput("var_merge", "Jointure par",choices =c("CPT","Parametres","SDP","SDP sans CPT"),selected ="SDP", width = "90%"),
                    uiOutput("regle_jointure"),
                    uiOutput("regle_jointure2"),
                   
                    fluidRow(column(width = 5,
                                    actionButton("Merge", label = "Merge",style = "width: 90%;background-color: white;")
                    ),
                    column(width = 5,
                           actionButton("TestMerge", label = "Test",style = "width: 90%;background-color: white;")))
                    
            ),
            column(class="main_bar",
                   width = 9,
                   tablerCard(
                     title ="Base de données",
                     width = 12,
                     overflow = T,
                     collapsed=  F,
                     dataTableOutput("data_merge")
                   )
            )
          ) #fin  
  ),#fin Merge
  
  
  
  #HG ------
  tabItem("HG",

    fluidRow(
      column( class="side_bar", #-------
              
              width = 3,
              tags$strong("HG"),
              br(),
              br(),
              selectInput("sortie", "Sortie",choices =c("X40µm","X250µm","D80"),selected ="X40µm", width = "90%"),
              selectInput("Models", "Models",choices =c("random forest","neuralnet","neuralnet rf","test","caret"),selected ="random forest", width = "90%"),
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
                              actionButton("Train", label = "Train",style = "width: 90%;background-color: white;")
                              ),
                       column(width = 5,
                              actionButton("Test", label = "Test",style = "width: 90%;background-color: white;"))),
              checkboxInput("checkbox2", label = "not random", value = FALSE),
              checkboxInput("checkbox3", label = "classification", value = FALSE)

      ),
      column(class="main_bar", #-----
             width = 9,

             tablerCard(
               title = "Evolutions",
               options = tagList(
                 
               ),
               fluidRow(
                  column(width = 6,echarts4rOutput("train", height = "300px")),
                  column(width = 6,echarts4rOutput("test", height = "300px"))),
               # verbatimTextOutput("console"),
               plotOutput("plot3",width = "100%"),
               plotOutput("plot4",width = "100%"),
               plotOutput("plot5",width = "100%"),
               plotOutput("plot6",width = "100%"),
               plotOutput("plot7",width = "100%"),
               width = 12,
               overflow = F,
               collapsed=  F
             ),
             tablerCard(
               title ="Base de données",
               width = 12,
               overflow = T,
               collapsed=  F,
               dataTableOutput("data")
             )
             
      )
    ) #fin row 
          
  ),#fin HG
  #Model classifaction -------
  tabItem("Model", 
          fluidRow(
            column( class="side_bar",
                    width =3,
                    
                    selectInput("sortie_cl", "Sortie",choices =c("X40µm","X250µm","Les deux"),selected ="X40µm", width = "90%"),
                    selectInput("Poste", "Poste",choices =c("P1","P2","P3"),selected ="P1", width = "90%"),
                    selectInput("Qualité", "Qualité",choices =c("BTRSC","BTRBA","MTBA","BTNBA","BTNSC"),selected ="BTRBA", width = "90%"),
                    checkboxInput("checkbox_cpt", label = "Ajouter la CPT", value = FALSE),
                    uiOutput("CPT"),
                    br(),
                    fluidRow(column(width = 5,
                                    actionButton("btn_Calculer", label = "Calculer",style = "width: 90%;background-color:white;")
                    ),
                    column(width = 5,
                           actionButton("btn_Rapport", label = "Rapport",style = "width: 90%;background-color: white;")))
                    
                    
                    
            ),
            column( class="side_bar",
                    width =3,
                    # 
                    # lapply(c("Débit_CV004","Dilution_SB002","Arrosage_Crible_SC003","Dilution_HP14","Dilution_HP15","Dilution_HP18"),function(c){
                    #   
                    #   textInput(c,c,value = 0)
                    #   
                    # })
                    # 
                    textInput("Débit_CV004","Débit_CV004",value = 0)
            ),
            column( class="side_bar",
                    width =3,
                    
                    lapply(colnames(Fevrier_DATA[8:ncol(Fevrier_DATA)]),function(c){
                      
                      textInput(c,c,value = Mean_PM_HG[c])
                      
                    })
                    
                    
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
    df.retartpm=0
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
                      inputId = "myPicker", 
                      label = "Inputs", 
                      choices = colnames(My_data$Model[!colnames(My_data$Model) %in% c("X250µm","X40µm","sortie_date","D80")]),
                      choicesOpt = list(style = rep(("color: black; font-weight: bold;background-color: white;"),26)))
                    
  })
  # vals$df1<-as.data.frame(df_products_upload)
  
  
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
  
  
  #retart de marche -----
  observeEvent(input$Train_retartpm,{
    # print(input[["Débit_CV004"]])
    withProgress(message = 'Calculs en cours....  merci de patienter', {
    for(c in colnames(Fevrier_DATA[2:15])){
      if(input[[c]]!=0){
        
        My_data$df.retartpm[[c]]=input[[c]]
        Fevrier_DATA=Makelag(Fevrier_DATA,c,input[[c]])
      }
      
     
    }
    print(head(Fevrier_DATA))
    Model_data <- merge(x = My_data_CPT_HG, y = Fevrier_DATA, by.x = "sortie_date",by.y = "DATE", all.x = TRUE)
    Model_data <- imputeTS::na_kalman(Model_data) #na_interpolation na_ma 
    Model_data <- Model_data %>% na.omit()
    My_data$Model <- Model_data %>% select(1,3:9,14:ncol(Model_data),11,12,13)
    
    
  })})
  
  #merge -----
  observeEvent(input$Merge,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {

    My_data$Model <- merge_variableHG(input$var_merge,input$regle_jointure_value,input$regle_jointure_value2)
    
    
    output$data_merge <- renderDataTable(
      My_data$Model)
  })})
  observeEvent(input$TestMerge,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      
      My_data$Model <- merge_variableHG2(input$checkboxCPT,input$checkboxCPTPM,input$checkboxSDPPM,input$regle_jointure_value)
      
      
      output$data_merge <- renderDataTable(
        My_data$Model)
    })})
  
  
  
  #train ------------
  #HG ------
  #seuil
  observeEvent(input$Models,{
    if (input$Models=="neuralnet"|input$Models=="neuralnet rf"|input$Models=="test"){
      output$hidden_layer <- renderUI({ textInput("hidden_layer_value","hidden layer",width = "90%",value = "4,5")})}
    else{
      output$hidden_layer <- renderUI({ })
    }
  })
  observeEvent(input$var_merge,{
    if (input$var_merge=="CPT"){
      output$regle_jointure <- renderUI({ selectInput("regle_jointure_value", "Régle de Joinutre SDP",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      output$regle_jointure2 <- renderUI({ selectInput("regle_jointure_value2", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      
    }else{
      if (input$var_merge=="SDP" | input$var_merge=="SDP sans CPT"){
        output$regle_jointure <- renderUI({ selectInput("regle_jointure_value2", "Régle de Joinutre PM",choices =c("mean","max","median","min","sd"),selected ="mean", width = "90%")}) 
      }else{
        
        output$regle_jointure <- renderUI({ })
        output$regle_jointure2 <- renderUI({ })
      }
    }
    
      
    
    
  })
  
  observeEvent(input$Test,{
    source_python('C:\\Users\\OUJBIH\\Desktop\\Stage PFE\\R\\draft\\Candlestick.py')
 
  })
 
  
  
  # Train btn HG -------
  observeEvent(input$Train,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      
      # My_data$Model <- My_data$Model%>% filter(as.Date(sortie_date)==as.Date("2020-01-26"))   
      c(DATA.traintmp_regression, DATA.testmp_regression,index_train)%<-% MakeMergeParameterCPTSDP(My_data$Model,input$split,input$checkbox2,input$sortie)

      c <- as.formula(paste(input$sortie,"~",paste(input$myPicker,collapse = "+")))
      print(c)
      # print("Model----------------------")
      # print(apply(My_data$Model, 2, mode))
      if(input$Models=="random forest"){
      rf_rg=randomForest(c, data = DATA.traintmp_regression)
      c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% randomForestHG(rf_rg,My_data$Model,DATA.traintmp_regression,DATA.testmp_regression,index_train,input$sortie)
      }else if (input$Models=="neuralnet"){
        hidden_layer_value <- strsplit(input$hidden_layer_value,",")[[1]]
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% neuralnet(c,My_data$Model,as.numeric(hidden_layer_value),DATA.traintmp_regression,DATA.testmp_regression,index_train,input$sortie)
      }else if (input$Models=="neuralnet rf"){
        rf_rg=randomForest(c, data = DATA.traintmp_regression)
        hidden_layer_value <- strsplit(input$hidden_layer_value,",")[[1]]
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% neuralnetrandomForestHG(c,rf_rg,as.numeric(hidden_layer_value),My_data$Model,DATA.traintmp_regression,DATA.testmp_regression,index_train,input$checkbox3,input$sortie)
      }else if (input$Models == "test"){
        rf_rg=randomForest(c, data = DATA.traintmp_regression)
        hidden_layer_value <- strsplit(input$hidden_layer_value,",")[[1]]
        c(g,goutput2,plot5,plot6,plot7,R_train,R_test)%<-% neuralnetrandomForestHG_old(c,rf_rg,as.numeric(hidden_layer_value),My_data$Model,DATA.traintmp_regression,DATA.testmp_regression,index_train,input$checkbox3,input$sortie)
      }else if(input$Models == "caret"){
        
      }
     
      output$plot3 <-  renderPlot({
        g
        
      })
     
      output$plot4 <-  renderPlot({
        goutput2
      })
      
      output$plot5 <-  renderPlot({
        plot5
       
      })
     
      output$plot6 <-  renderPlot({
        plot6
      
      })
      
      output$plot7 <-  renderPlot({
        plot7
        
      })
      output$data <- renderDataTable(
        My_data$Model)
      
    })
    output$train <- renderEcharts4r({
      e_charts() %>%
        e_gauge(round(as.numeric(R_train),3)*100, "Train")
    })
    output$test <- renderEcharts4r({
      e_charts() %>%
        e_gauge(round(as.numeric(R_test),3)*100, "Test")
    })
    # Sys.sleep(7)
  })
  
  
  
  
  
  
  #Classifaction -------------------
  observeEvent(input$checkbox_cpt,{
    if (input$checkbox_cpt){
      output$CPT <- renderUI({ 
        lapply(c("CPT_2500","CPT400","CPT160","CPT125","CPT40","CPT_40"),function(c){
          
          textInput(c,c,value = 0)
          
        })
        }) 
      
    }else{
      output$CPT <- renderUI({})
    }})
  #btn-calculer 
  observeEvent(input$btn_Calculer,{
    withProgress(message = 'Calculs en cours....  merci de patienter', {
      load("R/Classification/input_test.Rda")
      for(c in colnames(input_test)){
        
        print(input[["Débit_CV004"]])
      }
      
      
        
    })})
  
    
}

# Create Shiny app ----
shinyApp(ui = ui, server = server,onStart = function() {

  onStop(function() {
    
    rm(list=ls(all=TRUE))
    rm(list = ls())
  })})



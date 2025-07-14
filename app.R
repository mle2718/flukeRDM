# Required packages - everything else uses package:: found in r/required_packages.R
library(shiny)
library(shinyjs)
library(dplyr)

#### Start UI ####
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Recreational Fisheries Decision Support Tool"),
  #### Regulation Selection ####
  tabsetPanel(
    #### Summary Page ####
    tabPanel("Sumary Page",
             shinyWidgets::awesomeCheckboxGroup( # Select which state(s) to run
               inputId = "stateID", 
               label = "State", 
               choices = c("MA", "RI", "CT", "NY", "NJ", "DE",  "MD", "VA", "NC"),
               inline = TRUE,
               status = "danger"),
             uiOutput("outputMAfig"),
             uiOutput("outputMAtab"),
             uiOutput("outputRI"),
             uiOutput("outputCT"), 
             uiOutput("outputNY"),
             uiOutput("outputNJ"), 
             uiOutput("outputDE"),
             uiOutput("outputMD"),
             uiOutput("outputVA"), 
             uiOutput("outputNC")),
    
    tabPanel( "Regulation Selection",
              strong(div("REMINDER: (1) select state(s)  (2) Make selections below (3) click run me and then the `Results` tab to run model", style = "color:blue")), # Warning for users
              shinyWidgets::awesomeCheckboxGroup( # Select which state(s) to run
                inputId = "state", 
                label = "State", 
                choices = c("MA", "RI", "CT", "NY", "NJ", "DE",  "MD", "VA", "NC"),
                inline = TRUE,
                status = "danger"),
              
              #Run Button
              actionButton("runmeplease", "Run Me"), 
              textInput("Run_Name", "Please name this using your initials and the number of the run (ex. AB1)."),
              textOutput("message"),
              # Add UI code for each state
              uiOutput("addMA"),
              uiOutput("addRI"),
              uiOutput("addCT"), 
              uiOutput("addNY"),
              uiOutput("addNJ"), 
              uiOutput("addDE"),
              uiOutput("addMD"),
              uiOutput("addVA"), 
              uiOutput("addNC")),
    #### Results ####
    # tabPanel("Results", 
    #          
    #          conditionalPanel(condition="$('html').hasClass('shiny-busy')",
    #                           tags$div("Calculating...This will take ~15-20 min per state selected.",id="loadmessage")), #Warning for users
    #          ## KB - ADD PROGRESS BUTTON HERE
    #          
    #          downloadButton(outputId = "downloadData", "Download"),
    #          # Add table outputs
    #          ## KB - Make tables DTs - should fix RMD documentation issue
    #          tableOutput(outputId = "regtableout"),
    #          tableOutput(outputId = "welfare_tableout"),
    #          tableOutput(outputId = "keep_tableout"),
    #          tableOutput(outputId = "releaseout"),
    #          tableOutput(outputId = "ntrips_tableout"), 
    #          plotOutput(outputId = "fig")), 
    
  ))

####### Start Server ###################
server <- function(input, output, session) {
  
  library(magrittr) 
  
  ### Percent Change Approach
  sf_percent_change <- .72
  #bsb_percent_change <- 0
  scup_percent_change <- .9
  
  sf_rhl <- function(){
    sf_rhl = 99
    return(sf_rhl)
  }
  
  bsb_rhl <- function(){
    bsb_rhl = 99
    return(bsb_rhl)
  }
  
  scup_rhl <- function(){
    scup_rhl = 99
    return(scup_rhl)
  }
  
  Run_Name <- function(){
    if(stringr::str_detect(input$Run_Name, "_")){
      Run_Name <-  gsub("_", "-", input$Run_Name)
    }else {
      Run_Name <- input$Run_Name
    }
    print(Run_Name)
    return(Run_Name)
  }
  
  #### Toggle extra seasons on UI ####
  # Allows for extra seasons to show and hide based on click
  shinyjs::onclick("SFMAaddSeason",
                   shinyjs::toggle(id = "SFmaSeason2", anim = TRUE))
  shinyjs::onclick("BSBMAaddSeason",
                   shinyjs::toggle(id = "BSBmaSeason2", anim = TRUE))
  shinyjs::onclick("SCUPMAaddSeason",
                   shinyjs::toggle(id = "SCUPmaSeason2", anim = TRUE))
  
  shinyjs::onclick("SFRIaddSeason",
                   shinyjs::toggle(id = "SFriSeason2", anim = TRUE))
  shinyjs::onclick("BSBRIaddSeason",
                   shinyjs::toggle(id = "BSBriSeason3", anim = TRUE))
  shinyjs::onclick("SCUPRIaddSeason",
                   shinyjs::toggle(id = "SCUPriSeason2", anim = TRUE))
  
  shinyjs::onclick("SFCTaddSeason",
                   shinyjs::toggle(id = "SFctSeason3", anim = TRUE))
  shinyjs::onclick("BSBCTaddSeason",
                   shinyjs::toggle(id = "BSBctSeason2", anim = TRUE))
  shinyjs::onclick("SCUPCTaddSeason",
                   shinyjs::toggle(id = "SCUPctSeason2", anim = TRUE))
  
  shinyjs::onclick("SFNYaddSeason",
                   shinyjs::toggle(id = "SFnySeason3", anim = TRUE))
  shinyjs::onclick("BSBNYaddSeason",
                   shinyjs::toggle(id = "BSBnySeason3", anim = TRUE))
  shinyjs::onclick("SCUPNYaddSeason",
                   shinyjs::toggle(id = "SCUPnySeason2", anim = TRUE))
  
  shinyjs::onclick("SFNJaddSeason",
                   shinyjs::toggle(id = "SFnjSeason2", anim = TRUE))
  shinyjs::onclick("BSBNJaddSeason",
                   shinyjs::toggle(id = "BSBnjSeason5", anim = TRUE))
  shinyjs::onclick("SCUPNJaddSeason",
                   shinyjs::toggle(id = "SCUPnjSeason2", anim = TRUE))
  
  shinyjs::onclick("SFDEaddSeason",
                   shinyjs::toggle(id = "SFdeSeason3", anim = TRUE))
  shinyjs::onclick("BSBDEaddSeason",
                   shinyjs::toggle(id = "BSBdeSeason3", anim = TRUE))
  shinyjs::onclick("SCUPDEaddSeason",
                   shinyjs::toggle(id = "SCUPdeSeason2", anim = TRUE))
  
  shinyjs::onclick("SFMDaddSeason",
                   shinyjs::toggle(id = "SFmdSeason3", anim = TRUE))
  shinyjs::onclick("BSBMDaddSeason",
                   shinyjs::toggle(id = "BSBmdSeason3", anim = TRUE))
  shinyjs::onclick("SCUPMDaddSeason",
                   shinyjs::toggle(id = "SCUPmdSeason2", anim = TRUE))
  
  shinyjs::onclick("SFVAaddSeason",
                   shinyjs::toggle(id = "SFvaSeason3", anim = TRUE))
  shinyjs::onclick("BSBVAaddSeason",
                   shinyjs::toggle(id = "BSBvaSeason3", anim = TRUE))
  shinyjs::onclick("SCUPVAaddSeason",
                   shinyjs::toggle(id = "SCUPvaSeason2", anim = TRUE))
  
  shinyjs::onclick("SFNCaddSeason",
                   shinyjs::toggle(id = "SFncSeason2", anim = TRUE))
  shinyjs::onclick("BSBNCaddSeason",
                   shinyjs::toggle(id = "BSBncSeason3", anim = TRUE))
  shinyjs::onclick("SCUPNCaddSeason",
                   shinyjs::toggle(id = "SCUPncSeason2", anim = TRUE))
  
  #### Output$addSTATE ####
  ############## MASSACHUSETTS ###########################################################
  output$addMA <- renderUI({
    if(any("MA" == input$state)){
      fluidRow( 
        style = "background-color: #FBB4AE;",
        column(4,
               titlePanel("Summer Flounder - MA"),
               sliderInput(inputId = "SFmaFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        sliderInput(inputId = "SFmaFH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 17.5, step = .5))),
               sliderInput(inputId = "SFmaPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        sliderInput(inputId = "SFmaPR_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 17.5, step = .5))),
               sliderInput(inputId = "SFmaSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-24","%m-%d"),as.Date("09-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SFmaSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 5)),
                 column(5, 
                        sliderInput(inputId = "SFmaSH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 16.5, step = .5))),
               
               actionButton("SFMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFmaSeason2",
                                    sliderInput(inputId = "SFmaFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SFmaFH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SFmaPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SFmaPR_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SFmaSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SFmaSH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MA"),
               
               selectInput("BSB_MA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBmaMode"),
               
               
               actionButton("BSBMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmaSeason2",
                                    sliderInput(inputId = "BSBmaFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmaFH_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBmaPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmaPR_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBmaSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmaSH_2_len", label ="Min Length",
                                                         min = 5, max = 25, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - MA"),
               sliderInput(inputId = "SCUPmaFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("06-30","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaFH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPmaFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("07-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaFH_2_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))), 
               
               
               sliderInput(inputId = "SCUPmaPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaPR_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 11, step = .5))),
               sliderInput(inputId = "SCUPmaSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPmaSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPmaSH_1_len", label = "Min Length",
                                    min = 5, max = 25, value = 9.5, step = .5))),
               
               actionButton("SCUPMAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmaSeason2",
                                    sliderInput(inputId = "SCUPmaFH_seas3", label ="For Hire Open Season 3", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmaFH_3_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPmaPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmaPR_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPmaSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmaSH_2_len", label ="Min Length",
                                                         min = 3, max = 25, value = 10, step = .5)))))))
    }})
  
  
  ############# MA Breakout by mode ######################################
  output$SFmaMode <- renderUI({
    if (is.null(input$SF_MA_input_type))
      return()
    
    switch(input$SF_MA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFma_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFma_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 5)),
                                        column(6,
                                               sliderInput(inputId = "SFma_1_len", label ="Min Length",
                                                           min = 5, max = 25, value = 16.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFmaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              sliderInput(inputId = "SFmaFH_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "SFmaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              sliderInput(inputId = "SFmaPR_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "SFmaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-21","%m-%d"),as.Date("09-29","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 5)),
                                       column(6,
                                              sliderInput(inputId = "SFmaSH_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 16.5, step = .5)))))
  })
  
  
  output$BSBmaMode <- renderUI({
    if (is.null(input$BSB_MA_input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component. i.e. when all modes combined is selected only one
    switch(input$BSB_MA_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "BSBma_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBma_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 4)), 
                                        column(6,
                                               sliderInput(inputId = "BSBma_1_len", label ="Min Length",
                                                           min = 3, max = 25, value = 16.5, step = .5)))),
           
           
           "Seperated By Mode" = div(sliderInput(inputId = "BSBmaFH_seas1", label =" For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              sliderInput(inputId = "BSBmaFH_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5))),
                                     sliderInput(inputId = "BSBmaPR_seas1", label ="Private/Rental Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              sliderInput(inputId = "BSBmaPR_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5))),
                                     sliderInput(inputId = "BSBmaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-18","%m-%d"),as.Date("09-03","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 4)), 
                                       column(6,
                                              sliderInput(inputId = "BSBmaSH_1_len", label ="Min Length",
                                                          min = 3, max = 25, value = 16.5, step = .5)))))
    
  })

  
  ############## RHODE ISLAND ###########################################################
  output$addRI <- renderUI({
    if(any("RI" == input$state)){
      fluidRow( 
        style = "background-color: #B3CDE3;",
        column(4,
               titlePanel("Summer Flounder - RI"),
               
               selectInput("SF_RI_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFriMode"),
               
               actionButton("SFRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFriSeason2",
                                    sliderInput(inputId = "SFriFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFriFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))), 
                                    sliderInput(inputId = "SFriPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFriPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))), 
                                    sliderInput(inputId = "SFriSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFriSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFriSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - RI"),
               
               sliderInput(inputId = "BSBriFH_seas1", label ="For Hire Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("06-18","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriFH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        sliderInput(inputId = "BSBriFH_1_len", label ="Min Length",
                                    min = 11, max =18, value = 16, step = .5))),
               sliderInput(inputId = "BSBriFH_seas2", label ="For Hire Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriFH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 6)),
                 column(6,
                        sliderInput(inputId = "BSBriFH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBriPR_seas1", label ="Private Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriPR_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        sliderInput(inputId = "BSBriPR_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               sliderInput(inputId = "BSBriPR_seas2", label ="Private Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriPR_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 3)),
                 column(6,
                        sliderInput(inputId = "BSBriPR_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               sliderInput(inputId = "BSBriSH_seas1", label ="Shore Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-22","%m-%d"),as.Date("08-26","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriSH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 2)),
                 column(6,
                        sliderInput(inputId = "BSBriSH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               sliderInput(inputId = "BSBriSH_seas2", label ="Shore Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("08-27","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBriSH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 3)),
                 column(6,
                        sliderInput(inputId = "BSBriSH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16.5, step = .5))),
               
               actionButton("BSBRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBriSeason2",
                                    sliderInput(inputId = "BSBriFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBriFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    sliderInput(inputId = "BSBriPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBriPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBriSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBriSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBriSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - RI"),
               sliderInput(inputId = "SCUPriFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPriFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPriFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               sliderInput(inputId = "SCUPriFH_seas3", label ="For Hire Open Season 3", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               sliderInput(inputId = "SCUPriPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               sliderInput(inputId = "SCUPriSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPriSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPriSH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPRIaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPriSeason2",
                                    sliderInput(inputId = "SCUPriFH_seas4", label ="For Hire Open Season 4", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPriFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPriPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPriPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPriSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPriSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPriSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  
  
  ############# RI Breakout by mode ######################################
  output$SFriMode <- renderUI({
    if (is.null(input$SF_RI_input_type))
      return()
    
    switch(input$SF_RI_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFri_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFri_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 6)),
                                        column(6,
                                               sliderInput(inputId = "SFri_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFriFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "SFriFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFriPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "SFriPR_1_len", label ="Min Length",
                                                          min = 5, max = 25, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFriSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("04-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFriSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "SFriSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5)))))
  })
  
  ############## CONNECTICUT ###########################################################
  output$addCT <- renderUI({
    if(any("CT" == input$state)){
      fluidRow( 
        style = "background-color: #CCEBC5;",
        column(4,
               titlePanel("Summer Flounder - CT"),
               
               selectInput("SF_CT_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFctMode"),
               
               actionButton("SFCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFctSeason3",
                                    sliderInput(inputId = "SFctFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFctFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFctPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFctPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFctSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFctSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - CT"),
               
               sliderInput(inputId = "BSBctFH_seas1", label ="For Hire Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-18","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctFH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               sliderInput(inputId = "BSBctFH_seas2", label ="For Hire Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctFH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 7)),
                 column(6,
                        sliderInput(inputId = "BSBctFH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctPR_seas1", label ="Private Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-18","%m-%d"),as.Date("06-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctPR_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctPR_seas2", label ="Private Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("07-08","%m-%d"),as.Date("11-28","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctPR_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctPR_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctSH_seas1", label ="Shore Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("05-18","%m-%d"),as.Date("06-23","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_1_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctSH_1_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               sliderInput(inputId = "BSBctSH_seas2", label ="Shore Open Season 2",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value=c(as.Date("07-08","%m-%d"),as.Date("11-28","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "BSBctSH_2_bag", label ="Bag Limit",
                                     min = 0, max = 20, value = 5)),
                 column(6,
                        sliderInput(inputId = "BSBctSH_2_len", label ="Min Length",
                                    min = 11, max = 18, value = 16, step = .5))),
               
               actionButton("BSBCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBctSeason2",
                                    sliderInput(inputId = "BSBctFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBctFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    sliderInput(inputId = "BSBctPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBctPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5))),
                                    sliderInput(inputId = "BSBctSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBctSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBctSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - CT"),
               sliderInput(inputId = "SCUPctFH_seas1", label ="For Hire Open Season 1",
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPctFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPctFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               sliderInput(inputId = "SCUPctFH_seas3", label ="For Hire Open Season 3", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               sliderInput(inputId = "SCUPctPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               sliderInput(inputId = "SCUPctSH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPctSH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPctSH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPCTaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPctSeason2",
                                    sliderInput(inputId = "SCUPctFH_seas4", label ="For Hire Open Season 4", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPctFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPctPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPctPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPctSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPctSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPctSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  
  
  
  ############# CT Breakout by mode ######################################
  output$SFctMode <- renderUI({
    if (is.null(input$SF_CT_input_type))
      return()
    
    switch(input$SF_CT_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFct_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFct_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      sliderInput(inputId = "SFct_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFct_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFct_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFctFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFctFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFctPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFctPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFctSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFctSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     sliderInput(inputId = "SFctFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFctFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFctPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFctPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFctSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFctSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 2)),
                                       column(6,
                                              sliderInput(inputId = "SFctSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5)))))
          
  })
  
  
  
  ############# NEW YORK #######################
  output$addNY <- renderUI({
    if(any("NY" == input$state)){
      fluidRow( 
        style = "background-color: #DECBE4;",
        column(4,
               titlePanel("Summer Flounder - NY"),
               
               selectInput("SF_NY_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFnyMode"),
               
               actionButton("SFNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFnySeason3",
                                    sliderInput(inputId = "SFnyFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnyFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFnyPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnyPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5))), 
                                    sliderInput(inputId = "SFnySH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnySH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18.5, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NY"),
               
               selectInput("BSB_NY_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBnyMode"),
               
               
               actionButton("BSBNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBnySeason3",
                                    sliderInput(inputId = "BSBnyFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnyFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBnyPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnyPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnyPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5))),
                                    sliderInput(inputId = "BSBnySH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnySH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBnySH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 16.5, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NY"),
               sliderInput(inputId = "SCUPnyFH_seas1", label ="For Hire Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("08-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyFH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               
               sliderInput(inputId = "SCUPnyFH_seas2", label ="For Hire Open Season 2", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("09-01","%m-%d"),as.Date("10-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_2_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 40)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyFH_2_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               sliderInput(inputId = "SCUPnyFH_seas3", label ="For Hire Open Season 3", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyFH_3_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyFH_3_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))), 
               
               sliderInput(inputId = "SCUPnyPR_seas1", label ="Private Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnyPR_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnyPR_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 11, step = .5))),
               sliderInput(inputId = "SCUPnySH_seas1", label ="Shore Open Season 1", 
                           min = as.Date("01-01","%m-%d"),
                           max = as.Date("12-31","%m-%d"),
                           value =c(as.Date("05-01","%m-%d"),as.Date("12-31","%m-%d")), 
                           timeFormat = "%m-%d", ticks = FALSE),
               fluidRow(
                 column(4,
                        numericInput(inputId = "SCUPnySH_1_bag", label = "Bag Limit",
                                     min = 0, max = 100, value = 30)),
                 column(5, 
                        sliderInput(inputId = "SCUPnySH_1_len", label = "Min Length",
                                    min = 8, max = 12, value = 9.5, step = .5))),
               
               actionButton("SCUPNYaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnySeason2",
                                    sliderInput(inputId = "SCUPnyFH_seas4", label ="For Hire Open Season 4", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyFH_4_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPnyFH_4_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnyPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnyPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPnyPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnySH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnySH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPnySH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }})
  
  ############# NY Breakout by mode ######################################
  output$SFnyMode <- renderUI({
    if (is.null(input$SF_NY_input_type))
      return()
    
    switch(input$SF_NY_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFny_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFny_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19, step = .5))), 
                                      sliderInput(inputId = "SFny_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFny_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 19.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFnyFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFnyPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))) ,
                                     sliderInput(inputId = "SFnySH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("08-01","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnySH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19, step = .5))), 
                                     sliderInput(inputId = "SFnyFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFnyPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnyPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5))) ,
                                     sliderInput(inputId = "SFnySH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-02","%m-%d"),as.Date("10-15","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnySH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 19.5, step = .5)))))
  })
  
  
  output$BSBnyMode <- renderUI({
    if (is.null(input$BSB_NY_input_type))
      return()
    
    switch(input$BSB_NY_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBny_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-23","%m-%d"),as.Date("8-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "BSBny_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBny_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBny_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 6)),
                                        column(6,
                                               sliderInput(inputId = "BSBny_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 16.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBnyFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnyPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnySH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-23","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "BSBnySH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBnyFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnyPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnyPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "BSBnyPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5))) ,
                                     sliderInput(inputId = "BSBnySH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("09-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnySH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 6)),
                                       column(6,
                                              sliderInput(inputId = "BSBnySH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 16.5, step = .5)))))
  })
  
  ############## NEW JERSEY ############################################################
  output$addNJ <- renderUI({
    if(any("NJ" == input$state)){
      fluidRow( 
        style = "background-color: #FED9A6;",
        column(4,
               titlePanel("Summer Flounder - NJ"),
               
               selectInput("SF_NJ_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFnjMode"),
               
               
               actionButton("SFNJaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFnjSeason2",
                                    sliderInput(inputId = "SFnjFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),#)),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 7, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 18, step = .5))),
                                    sliderInput(inputId = "SFnjPR_seas2", label ="Private/Rental Open Season 2",  
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5))),
                                    sliderInput(inputId = "SFnjSH_seas2", label ="Shore Open Season 2",  
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFnjSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFnjSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value =  18, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NJ"),
               
               selectInput("BSB_NJ_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBnjMode"),
               
               actionButton("BSBNJaddSeason", "Add Season"), 
               #Season 5
               shinyjs::hidden( div(ID = "BSBnjSeason5",
                                    sliderInput(inputId = "BSBnjFH_seas5", label =" For Hire Open Season 5", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjFH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjFH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjPR_seas5", label ="Private/Rental Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjPR_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjPR_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5))),
                                    sliderInput(inputId = "BSBnjSH_seas5", label ="Shore Open Season 5",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBnjSH_5_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "BSBnjSH_5_len", label ="Min Length",
                                                         min = 11, max = 18, value = 12.5, step = .5)))))),
        
        
        
        
        column(4, 
               titlePanel("Scup - NJ"),
               
               selectInput("SCUP_NJ_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPnjMode"),
               
               actionButton("SCUPNJaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPnjSeason2",
                                    sliderInput(inputId = "SCUPnjFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnjPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5))), 
                                    sliderInput(inputId = "SCUPnjSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPnjSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SCUPnjSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 10, step = .5)))))))
    }
    
  })
  
  ############# NJ Breakout by mode ######################################
  output$SFnjMode <- renderUI({
    if (is.null(input$SF_NJ_input_type))
      return()
    
    switch(input$SF_NJ_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFnj_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4, 
                                               numericInput(inputId = "SFnj_1_bag", label ="Bag Limit", 
                                                            min = 0, max = 100, value = 3)),
                                        column(6,
                                               sliderInput(inputId = "SFnj_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 18, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFnjFH_seas1", label ="For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value =c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjFH_1_bag", label ="Bag Limit", 
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnjFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5)), 
                                     sliderInput(inputId = "SFnjPR_seas1", label ="Private/Rental Open Season 1",  
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)),
                                       column(6,
                                              sliderInput(inputId = "SFnjPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5))),
                                     sliderInput(inputId = "SFnjSH_seas1", label ="Shore Open Season 1",  
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-04","%m-%d"),as.Date("09-25","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4, 
                                              numericInput(inputId = "SFnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 3)), 
                                       column(6,
                                              sliderInput(inputId = "SFnjSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 18, step = .5))))))
  })
  
  
  output$BSBnjMode <- renderUI({
    if (is.null(input$BSB_NJ_input_type))
      return()
    
    switch(input$BSB_NJ_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "BSBnj_seas1", label ="Open Season 1", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_1_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 2
                                      sliderInput(inputId = "BSBnj_seas2", label ="Open Season 2", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_2_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 1)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 3
                                      sliderInput(inputId = "BSBnj_seas3", label ="Open Season 3", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-01","%m-%d"),as.Date("10-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_3_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 10)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_3_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5))),
                                      
                                      #Season 4
                                      sliderInput(inputId = "BSBnj_seas4", label ="Open Season 4", 
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnj_4_bag", label ="Bag Limit",
                                                            min = 0, max = 20, value = 15)), 
                                        column(6,
                                               sliderInput(inputId = "BSBnj_4_len", label ="Min Length",
                                                           min = 11, max = 18, value = 12.5, step = .5)))),
           
           "Seperated By Mode" = div(sliderInput(inputId = "BSBnjFH_seas1", label =" For Hire Open Season 1", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas1", label ="Private/Rental Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-17","%m-%d"),as.Date("06-19","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 2
                                     sliderInput(inputId = "BSBnjFH_seas2", label =" For Hire Open Season 2", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas2", label ="Private/Rental Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("07-01","%m-%d"),as.Date("08-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 3
                                     sliderInput(inputId = "BSBnjFH_seas3", label =" For Hire Open Season 3", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas3", label ="Private/Rental Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas3", label ="Shore Open Season 3",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-07","%m-%d"),as.Date("10-26","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_3_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_3_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     #Season 4
                                     sliderInput(inputId = "BSBnjFH_seas4", label =" For Hire Open Season 4", 
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjFH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjFH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjPR_seas4", label ="Private/Rental Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjPR_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjPR_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5))),
                                     sliderInput(inputId = "BSBnjSH_seas4", label ="Shore Open Season 4",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("11-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBnjSH_4_bag", label ="Bag Limit",
                                                           min = 0, max = 20, value = 10)), 
                                       column(6,
                                              sliderInput(inputId = "BSBnjSH_4_len", label ="Min Length",
                                                          min = 11, max = 18, value = 12.5, step = .5)))))
  })
  
  output$SCUPnjMode <- renderUI({
    if (is.null(input$SCUP_NJ_input_type))
      return()

    switch(input$SCUP_NJ_input_type,
           
           "All Modes Combined" = div(sliderInput(inputId = "SCUPnj_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnj_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPnj_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 10, step = .5)))),
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPnjFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5))), 
                                     sliderInput(inputId = "SCUPnjSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPnjSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPnjSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 10, step = .5)))))
  })
  
 
  output$SF_NJ_input_type_text <- renderText({
    input$SF_NJ_input_type
  })
  output$BSB_NJ_input_type_text <- renderText({
    input$BSB_NJ_input_type
  })
  output$SCUP_NJ_input_type_text <- renderText({
    input$SCUP_NJ_input_type
  })
  
  output$dynamic_value <- renderPrint({
    str(input$dynamic)
  })
  
  
  
  
  
  
  
  ############## DELEWARE ###########################################################
  output$addDE <- renderUI({
    if(any("DE" == input$state)){
      fluidRow( 
        style = "background-color: #FFFFCC;",
        column(4,
               titlePanel("Summer Flounder - DE"),
               
               selectInput("SF_DE_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFdeMode"),
               
               actionButton("SFDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFdeSeason3",
                                    sliderInput(inputId = "SFdeFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFdeFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFdePR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFdePR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFdeSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFdeSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - DE"),
               
               selectInput("BSB_DE_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBdeMode"),
               
               
               actionButton("BSBDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBdeSeason3",
                                    sliderInput(inputId = "BSBdeFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBdeFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBdePR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdePR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBdePR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBdeSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBdeSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBdeSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - DE"),
               
               selectInput("SCUP_DE_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPdeMode"),
               
               actionButton("SCUPDEaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPdeSeason2",
                                    sliderInput(inputId = "SCUPdeFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPdeFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPdePR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdePR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPdePR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPdeSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPdeSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPdeSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## DE breakout by mode ############################
  
  output$SFdeMode <- renderUI({
    if (is.null(input$SF_DE_input_type))
      return()
    
    switch(input$SF_DE_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFde_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFde_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      sliderInput(inputId = "SFde_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFde_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFdeFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFdePR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdePR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFdeSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     
                                     sliderInput(inputId = "SFdeFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFdePR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdePR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFdeSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFdeSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBdeMode <- renderUI({
    if (is.null(input$BSB_DE_input_type))
      return()
    
    switch(input$BSB_DE_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBde_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBde_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBde_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBde_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBde_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBdeFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBdePR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdePR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBdeSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBdeFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBdePR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdePR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdePR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBdeSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBdeSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBdeSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPdeMode <- renderUI({
    if (is.null(input$SCUP_DE_input_type))
      return()
    
    switch(input$SCUP_DE_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPde_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPde_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPde_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPdeFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPdeFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPdePR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdePR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPdePR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPdeSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPdeSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPdeSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  ############## MARYLAND ###########################################################
  output$addMD <- renderUI({
    if(any("MD" == input$state)){
      fluidRow( 
        style = "background-color: #E5D8BD;",
        column(4,
               titlePanel("Summer Flounder - MD"),
               
               selectInput("SF_MD_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFmdMode"),
               
               actionButton("SFMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFmdSeason3",
                                    sliderInput(inputId = "SFmdFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFmdFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFmdPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFmdPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFmdSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFmdSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - MD"),
               
               selectInput("BSB_MD_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBmdMode"),
               
               
               actionButton("BSBMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBmdSeason3",
                                    sliderInput(inputId = "BSBmdFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmdFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBmdPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmdPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBmdSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBmdSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBmdSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - MD"),
               
               selectInput("SCUP_MD_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPmdMode"),
               
               actionButton("SCUPMDaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPmdSeason2",
                                    sliderInput(inputId = "SCUPmdFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmdFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPmdPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmdPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPmdSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPmdSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPmdSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## MD breakout by mode ############################
  
  output$SFmdMode <- renderUI({
    if (is.null(input$SF_MD_input_type))
      return()
    
    switch(input$SF_MD_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFmd_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFmd_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      sliderInput(inputId = "SFmd_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFmd_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFmdFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFmdPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFmdSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))), 
                                     sliderInput(inputId = "SFmdFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFmdPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFmdSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFmdSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))
                                     ))
  })
  
  
  output$BSBmdMode <- renderUI({
    if (is.null(input$BSB_MD_input_type))
      return()
    
    switch(input$BSB_MD_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBmd_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBmd_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBmd_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBmd_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBmd_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBmdFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBmdPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBmdSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBmdFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBmdPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBmdSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBmdSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBmdSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPmdMode <- renderUI({
    if (is.null(input$SCUP_MD_input_type))
      return()
    
    switch(input$SCUP_MD_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPmd_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPmd_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPmd_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPmdFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPmdFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPmdPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPmdPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPmdSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPmdSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPmdSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  ############## VIRGINIA ###########################################################
  output$addVA <- renderUI({
    if(any("VA" == input$state)){
      fluidRow( 
        style = "background-color: #FDDAEC;",
        column(4,
               titlePanel("Summer Flounder - VA"),
               
               selectInput("SF_VA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFvaMode"),
               
               actionButton("SFVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFvaSeason3",
                                    sliderInput(inputId = "SFvaFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFvaFH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFvaPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFvaPR_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5))), 
                                    sliderInput(inputId = "SFvaSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFvaSH_3_len", label ="Min Length",
                                                         min = 14, max = 21, value = 16, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - VA"),
               
               selectInput("BSB_VA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBvaMode"),
               
               
               actionButton("BSBVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBvaSeason3",
                                    sliderInput(inputId = "BSBvaFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBvaFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBvaPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBvaPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBvaSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBvaSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBvaSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - VA"),
               
               selectInput("SCUP_VA_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPvaMode"),
               
               actionButton("SCUPVAaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPvaSeason2",
                                    sliderInput(inputId = "SCUPvaFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPvaFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPvaPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPvaPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPvaSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPvaSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPvaSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## VA breakout by mode ############################
  
  output$SFvaMode <- renderUI({
    if (is.null(input$SF_VA_input_type))
      return()
    
    switch(input$SF_VA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFva_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFva_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 16, step = .5))), 
                                      sliderInput(inputId = "SFva_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 4)),
                                        column(6,
                                               sliderInput(inputId = "SFva_2_len", label ="Min Length",
                                                           min = 14, max = 21, value = 17.5, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFvaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFvaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))) ,
                                     sliderInput(inputId = "SFvaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("05-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 16, step = .5))),
                                     sliderInput(inputId = "SFvaFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaFH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFvaPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaPR_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5))) ,
                                     sliderInput(inputId = "SFvaSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("06-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 4)),
                                       column(6,
                                              sliderInput(inputId = "SFvaSH_2_len", label ="Min Length",
                                                          min = 14, max = 21, value = 17.5, step = .5)))))
  })
  
  
  output$BSBvaMode <- renderUI({
    if (is.null(input$BSB_VA_input_type))
      return()
    
    switch(input$BSB_VA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBva_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBva_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBva_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBva_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBva_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBvaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBvaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBvaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("07-06","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBvaFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBvaPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBvaSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-09","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBvaSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBvaSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPvaMode <- renderUI({
    if (is.null(input$SCUP_VA_input_type))
      return()
    
    switch(input$SCUP_VA_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPva_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPva_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPva_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPvaFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPvaFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPvaPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPvaPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPvaSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPvaSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPvaSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  ############## NORTH CAROLINA ###########################################################
  output$addNC <- renderUI({
    if(any("NC" == input$state)){
      fluidRow( 
        style = "background-color: #F2F2F2;",
        column(4,
               titlePanel("Summer Flounder - NC"),
               
               selectInput("SF_NC_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SFncMode"),
               
               actionButton("SFNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SFncSeason2",
                                    sliderInput(inputId = "SFncFH_seas2", label ="For Hire Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFncFH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    sliderInput(inputId = "SFncPR_seas2", label ="Private Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFncPR_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5))), 
                                    sliderInput(inputId = "SFncSH_seas2", label ="Shore Open Season 2",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SFncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "SFncSH_2_len", label ="Min Length",
                                                         min = 14, max = 21, value = 15, step = .5)))))),
        
        column(4, 
               titlePanel("Black Sea Bass - NC"),
               
               selectInput("BSB_NC_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("BSBncMode"),
               
               
               actionButton("BSBNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "BSBncSeason3",
                                    sliderInput(inputId = "BSBncFH_seas3", label ="For Hire Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncFH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBncFH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBncPR_seas3", label ="Private Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncPR_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBncPR_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5))),
                                    sliderInput(inputId = "BSBncSH_seas3", label ="Shore Open Season 3",
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "BSBncSH_3_bag", label ="Bag Limit",
                                                          min = 0, max = 100, value = 0)),
                                      column(6,
                                             sliderInput(inputId = "BSBncSH_3_len", label ="Min Length",
                                                         min = 11, max = 18, value = 13, step = .5)))))),
        
        
        
        
        column(4, #### SCUP 
               titlePanel("Scup - NC"),
               
               selectInput("SCUP_NC_input_type", "Regulations combined or seperated by mode?",
                           c("All Modes Combined", "Seperated By Mode")),
               uiOutput("SCUPncMode"),
               
               actionButton("SCUPNCaddSeason", "Add Season"), 
               shinyjs::hidden( div(ID = "SCUPncSeason2",
                                    sliderInput(inputId = "SCUPncFH_seas2", label ="For Hire Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncFH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPncFH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPncPR_seas2", label ="Private Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncPR_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPncPR_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5))), 
                                    sliderInput(inputId = "SCUPncSH_seas2", label ="Shore Open Season 2", 
                                                min = as.Date("01-01","%m-%d"),
                                                max = as.Date("12-31","%m-%d"),
                                                value=c(as.Date("12-31","%m-%d"),as.Date("12-31","%m-%d")), 
                                                timeFormat = "%m-%d", ticks = FALSE),
                                    fluidRow(
                                      column(4,
                                             numericInput(inputId = "SCUPncSH_2_bag", label ="Bag Limit",
                                                          min = 0, max = 20, value = 0)), 
                                      column(6,
                                             sliderInput(inputId = "SCUPncSH_2_len", label ="Min Length",
                                                         min = 8, max = 12, value = 9, step = .5)))))))
    }})
  
  
  
  ############## NC breakout by mode ############################
  
  output$SFncMode <- renderUI({
    if (is.null(input$SF_NC_input_type))
      return()
    
    switch(input$SF_NC_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SFnc_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SFnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 1)),
                                        column(6,
                                               sliderInput(inputId = "SFnc_1_len", label ="Min Length",
                                                           min = 14, max = 21, value = 15, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SFncFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId = "SFncFH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     sliderInput(inputId = "SFncPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId = "SFncPR_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5))) ,
                                     sliderInput(inputId = "SFncSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("08-16","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SFncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 1)),
                                       column(6,
                                              sliderInput(inputId = "SFncSH_1_len", label ="Min Length",
                                                          min = 14, max = 21, value = 15, step = .5)))))
  })
  
  
  output$BSBncMode <- renderUI({
    if (is.null(input$BSB_NC_input_type))
      return()
    
    switch(input$BSB_NC_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "BSBnc_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBnc_1_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5))), 
                                      
                                      sliderInput(inputId = "BSBnc_seas2", label ="Open Season 2",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "BSBnc_2_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 15)),
                                        column(6,
                                               sliderInput(inputId = "BSBnc_2_len", label ="Min Length",
                                                           min = 11, max = 18, value = 13, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "BSBncFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncFH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBncPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncPR_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 15, step = .5))) ,
                                     sliderInput(inputId = "BSBncSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("05-15","%m-%d"),as.Date("09-30","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncSH_1_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))), 
                                     
                                     
                                     sliderInput(inputId = "BSBncFH_seas2", label ="For Hire Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncFH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncFH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBncPR_seas2", label ="Private Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncPR_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncPR_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5))) ,
                                     sliderInput(inputId = "BSBncSH_seas2", label ="Shore Open Season 2",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("10-10","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "BSBncSH_2_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 15)),
                                       column(6,
                                              sliderInput(inputId = "BSBncSH_2_len", label ="Min Length",
                                                          min = 11, max = 18, value = 13, step = .5)))))
  })
  
  output$SCUPncMode <- renderUI({
    if (is.null(input$SCUP_NC_input_type))
      return()
    
    switch(input$SCUP_NC_input_type, 
           "All Modes Combined" = div(sliderInput(inputId = "SCUPnc_seas1", label ="Open Season 1",
                                                  min = as.Date("01-01","%m-%d"),
                                                  max = as.Date("12-31","%m-%d"),
                                                  value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                  timeFormat = "%m-%d", ticks = FALSE),
                                      fluidRow(
                                        column(4,
                                               numericInput(inputId = "SCUPnc_1_bag", label ="Bag Limit",
                                                            min = 0, max = 100, value = 30)),
                                        column(6,
                                               sliderInput(inputId = "SCUPnc_1_len", label ="Min Length",
                                                           min = 8, max = 12, value = 9, step = .5)))), 
           "Seperated By Mode" = div(sliderInput(inputId = "SCUPncFH_seas1", label ="For Hire Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncFH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPncFH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPncPR_seas1", label ="Private Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncPR_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPncPR_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5))) ,
                                     sliderInput(inputId = "SCUPncSH_seas1", label ="Shore Open Season 1",
                                                 min = as.Date("01-01","%m-%d"),
                                                 max = as.Date("12-31","%m-%d"),
                                                 value=c(as.Date("01-01","%m-%d"),as.Date("12-31","%m-%d")), 
                                                 timeFormat = "%m-%d", ticks = FALSE),
                                     fluidRow(
                                       column(4,
                                              numericInput(inputId = "SCUPncSH_1_bag", label ="Bag Limit",
                                                           min = 0, max = 100, value = 30)),
                                       column(6,
                                              sliderInput(inputId = "SCUPncSH_1_len", label ="Min Length",
                                                          min = 8, max = 12, value = 9, step = .5)))))
  })
  
  
  
  
  
  ################ Summary page outputs #################
  ############ MA
  
  output$DToutMA <- DT::renderDT({
    
    Regs_out<- outputs()
    # df3<- outputs() %>% dplyr::filter(!Category %in% c("CV", "ntrips", "nchoiceoccasions","cod" , "had")) %>%
    #   dplyr::select(Category, Value, run_number) %>%
    #   dplyr::left_join(SQ_regulations, by = c("Category"))
    # 
    # seas<- df3 %>% dplyr::filter(stringr::str_detect(Category, "Season")) %>%
    #   tidyr::separate(Value, into = c("Value1", "Value2"), sep = " - ") %>%
    #   tidyr::separate(SQ, into = c("SQ1", "SQ2"), sep = " - ") %>%
    #   dplyr::mutate(Value = as.integer(lubridate::ymd(Value2)-lubridate::ymd(Value1)),
    #                 SQ = as.integer(lubridate::ymd(SQ2)-lubridate::ymd(SQ1))) %>%
    #   dplyr::mutate(Diff_from_SQ = dplyr::case_when(Value < SQ ~ "Shorter Season", TRUE ~ ""),
    #                 Diff_from_SQ = dplyr::case_when(Value > SQ ~ "Longer Season", TRUE ~ Diff_from_SQ),
    #                 Value = paste0(Value1, " - ", Value2)) %>%
    #   dplyr::select(Category, Diff_from_SQ, run_number)
    # 
    # bag<- df3 %>% dplyr::filter(stringr::str_detect(Category, "bag")) %>%
    #   dplyr::mutate(Diff_from_SQ = dplyr::case_when(as.numeric(Value) < as.numeric(SQ) ~ "Smaller Bag", TRUE ~ ""),
    #                 Diff_from_SQ = dplyr::case_when(as.numeric(Value) > as.numeric(SQ) ~ "Larger Bag", TRUE ~ Diff_from_SQ)) %>%
    #   dplyr::select(Category, Diff_from_SQ, run_number)
    # 
    # size<- df3 %>% dplyr::filter(stringr::str_detect(Category, "size")) %>%
    #   dplyr::mutate(Diff_from_SQ = dplyr::case_when(as.numeric(Value) < as.numeric(SQ) ~ "Smaller Min Length", TRUE ~ ""),
    #                 Diff_from_SQ = dplyr::case_when(as.numeric(Value) > as.numeric(SQ) ~ "Larger Min Length", TRUE ~ Diff_from_SQ)) %>%
    #   dplyr::select(Category, Diff_from_SQ, run_number)
    # 
    # df4<- rbind(seas, bag, size) %>%
    #   dplyr::ungroup()
    # 
    # Regs_out <- df3 %>%
    #   dplyr::left_join(df4, by = c("Category", "run_number")) %>%
    #   dplyr::select(!SQ) %>%
    #   dplyr::select(!Opt) %>%
    #   tidyr::separate(Category, into =c("Species", "mode", "Var"), sep = "_") %>%
    #   dplyr::ungroup() %>%
    #   tidyr::pivot_wider(names_from = Var, values_from = c(Value, Diff_from_SQ)) %>%
    #   dplyr::mutate(Value_Season = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Value_Season),
    #                 Value_size = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Value_size),
    #                 Diff_from_SQ_bag = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_bag),
    #                 Diff_from_SQ_size = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_size),
    #                 Diff_from_SQ_Season = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Diff_from_SQ_Season),
    #                 Value_bag = dplyr::case_when(Value_bag == 0 ~"NA", TRUE ~ Value_bag)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(Diff_from_SQ = paste0(Diff_from_SQ_bag,Diff_from_SQ_size,Diff_from_SQ_Season)) %>%
    #   dplyr::select(!c(Diff_from_SQ_bag,Diff_from_SQ_size,Diff_from_SQ_Season)) %>%
    #   tidyr::pivot_wider(names_from = Species, values_from = c(Diff_from_SQ, Value_bag, Value_size, Value_Season)) %>%
    #   dplyr::ungroup() %>%
    #   dplyr::mutate(cod_bag = paste0(Value_bag_Cod1, " , ", Value_bag_Cod2),
    #                 cod_size = paste0(Value_size_Cod1, " , ", Value_size_Cod2),
    #                 cod_season = paste0(Value_Season_Cod1, " , ", Value_Season_Cod2),
    #                 had_bag = paste0(Value_bag_Had1, " , ", Value_bag_Had2, " , ", Value_bag_Had3),
    #                 had_size = paste0(Value_size_Had1, " , ", Value_size_Had2, " , ", Value_size_Had3),
    #                 had_season = paste0(Value_Season_Had1, " , ", Value_Season_Had2, " , ", Value_Season_Had3),
    #                 cod_bag = stringr::str_remove(cod_bag, " , NA"),
    #                 cod_size = stringr::str_remove(cod_size, " , NA"),
    #                 cod_season = stringr::str_remove(cod_season, " , NA"),
    #                 had_bag = stringr::str_remove(had_bag, " , NA"),
    #                 had_size = stringr::str_remove(had_size, " , NA"),
    #                 had_season = stringr::str_remove(had_season, " , NA"),
    #                 cod_bag = stringr::str_remove(cod_bag, "NA ,"),
    #                 cod_size = stringr::str_remove(cod_size, "NA ,"),
    #                 cod_season = stringr::str_remove(cod_season, "NA ,"),
    #                 had_bag = stringr::str_remove(had_bag, "NA ,"),
    #                 had_size = stringr::str_remove(had_size, "NA ,"),
    #                 had_season = stringr::str_remove(had_season, "NA ,"),
    #                 Diff_from_SQ_cod = paste0(Diff_from_SQ_Cod1, " , ", Diff_from_SQ_Cod2),
    #                 Diff_from_SQ_had = paste0(Diff_from_SQ_Had1, " , ", Diff_from_SQ_Had2, " , ", Diff_from_SQ_Had3),
    #                 Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, " , NA"),
    #                 Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, "NANA"),
    #                 Diff_from_SQ_cod = stringr::str_remove(Diff_from_SQ_cod, "NA ,"),
    #                 Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, "NA ,"),
    #                 Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, " , NA"),
    #                 Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, " , NANA"),
    #                 Diff_from_SQ_had = stringr::str_remove(Diff_from_SQ_had, "NANA")) %>%
    #   dplyr::select(mode, run_number, Diff_from_SQ_cod, Diff_from_SQ_had, cod_bag, cod_size, cod_season, had_bag, had_size, had_season) %>%
    #   dplyr::mutate(cod_season = stringr::str_remove_all(cod_season, "202.-"),
    #                 had_season = stringr::str_remove_all(had_season, "202.-")) %>%
    #   dplyr::mutate(mode = dplyr::recode(mode, "FH" = "For Hire",
    #                                      "PR" = "Private")) %>%
    #   dplyr::select(run_number, mode, cod_bag, cod_size, cod_season, Diff_from_SQ_cod,
    #                 had_bag, had_size, had_season, Diff_from_SQ_had) %>%
    #   dplyr::rename(Mode = mode,
    #                 `Run Identifier` = run_number,
    #                 `Cod Bag Limit` = cod_bag,
    #                 `Cod Minimum Size (in)` = cod_size,
    #                 `Cod Season(s)` = cod_season,
    #                 `Haddock Bag Limit` = had_bag,
    #                 `Haddock Minimum Size (in)` = had_size,
    #                 `Haddock Season(s)` = had_season,
    #                 `Difference from Cod SQ` = Diff_from_SQ_cod,
    #                 `Difference from haddock SQ` = Diff_from_SQ_had)
    # 
    # 
     DT::datatable(Regs_out)
  })
  
  output$figoutMA <- plotly::renderPlotly({
    
    harvest_dat<- outputs()  %>% 
      #dat %>%
      dplyr::filter(keep_release == "keep", 
                    number_weight == "weight", 
                    mode == "all modes", 
                    state == "MA") %>%
      dplyr::group_by(state, category, keep_release, number_weight, draw, run_number) %>%
      dplyr::summarise(Value = sum(as.numeric(value))) %>% 
      dplyr::group_by(state, draw, category) %>% 
      dplyr::mutate(SQ_value = Value[stringr::str_detect( run_number, "SQ")], 
                    diff_perc = ((Value - SQ_value)/SQ_value) *100) %>% 
      dplyr::group_by(run_number, state, category) %>% 
      dplyr::summarise(Value = median(Value), 
                       perc_under = sum(dplyr::case_when(category == "bsb" ~ diff_perc > 1, 
                                                        category == "sf" ~ diff_perc > 0.5, 
                                                        category == "scup" ~ diff_perc > 0.25)), 
                       diff_perc = median(diff_perc)) %>%
      tidyr::pivot_wider(names_from = category, values_from = c(Value, diff_perc, perc_under) )
    
    p<- harvest_dat %>% 
      #dplyr::mutate(under_acl_cod = as.numeric(under_acl_cod)) %>%
      ggplot2::ggplot(ggplot2::aes(x = diff_perc_sf, y = diff_perc_bsb, color = as.character(perc_under_sf), shape = as.character(perc_under_bsb), label = run_number)) +
      ggplot2::geom_point() +
      ggplot2::geom_text(vjust = -1) + 
      ggplot2::geom_vline( xintercept =1, linetype="dashed")+
      ggplot2::geom_hline( yintercept =1, color="grey45")+
      ggplot2::annotate(geom="text", x=1.2, label="SF RHL reduction", y=1) +
      ggplot2::annotate(geom="text", y=1.3, label="BSB RHL reduction", x=1.5) +
      ggplot2::guides(size = "none")+
      ggplot2::ggtitle("Fluke and BSB percent differenct in Harvest from SQ")+
      ggplot2::ylab("Median Recreational BSB % diff")+
      ggplot2::xlab("Median Recreational SF % diff")
    
    fig<- plotly::ggplotly(p) %>% #,
      #tooltip = c("x", "y", "colour")) %>%
      plotly::style(textposition = "top center")
    fig
  })
  output$outputMAtab <- renderUI({
    if(any("MA" == input$stateID)){
      DT::DTOutput(outputId = "DToutMA")
    }
  })
  output$outputMAfig <- renderUI({
    if(any("MA" == input$stateID)){
      plotly::plotlyOutput(outputId = "figoutMA")
    }
  })
   

  ####  Storing Inputs for decoupled model ####
  
  regulations <- observeEvent(input$runmeplease,{
    
    regulations <- NULL
    print("where am i")
    #if(any( )) will run all selected check boxes on UI-regulations selection tab
    if(any("MA" == input$state)){
      print("start MA")
      
      if(input$BSB_MA_input_type == "All Modes Combined"){
        bsbMAregs <- data.frame(run_name = c(Run_Name()), 
                                input =  c("BSBma_seas1_op", "BSBma_seas1_cl", "BSBma_1_bag", "BSBma_1_len", 
                                           "BSBmaFH_seas2_op", "BSBmaFH_seas2_cl", "BSBmaFH_2_bag", "BSBmaFH_2_len", 
                                           "BSBmaPR_seas2_op", "BSBmaPR_seas2_cl", "BSBmaPR_2_bag", "BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op", "BSBmaSH_seas2_cl", "BSBmaSH_2_bag", "BSBmaSH_2_len"),
                                value =  c(as.character(input$BSBma_seas1[1]), as.character(input$BSBma_seas1[2]), as.character(input$BSBma_1_bag), as.character(input$BSBma_1_len), 
                                           as.character(input$BSBmaFH_seas2[1]), as.character(input$BSBmaFH_seas2[2]), as.character(input$BSBmaFH_2_bag), as.character(input$BSBmaFH_2_len), 
                                           as.character(input$BSBmaPR_seas2[1]), as.character(input$BSBmaPR_seas2[2]), as.character(input$BSBmaPR_2_bag), as.character(input$BSBmaPR_2_len),
                                           as.character(input$BSBmaSH_seas2[1]), as.character(input$BSBmaSH_seas2[2]), as.character(input$BSBmaSH_2_bag), as.character(input$BSBmaSH_2_len)))
      }else{
        bsbMAregs <- data.frame(run_name = c(Run_Name()), 
                                input =  c("BSBmaFH_seas1_op", "BSBmaFH_seas1_cl", "BSBmaFH_1_bag", "BSBmaFH_1_len", 
                                           "BSBmaPR_seas1_op", "BSBmaPR_seas1_cl", "BSBmaPR_1_bag", "BSBmaPR_1_len",
                                           "BSBmaSH_seas1_op", "BSBmaSH_seas1_cl", "BSBmaSH_1_bag", "BSBmaSH_1_len",
                                           "BSBmaFH_seas2_op", "BSBmaFH_seas2_cl", "BSBmaFH_2_bag", "BSBmaFH_2_len", 
                                           "BSBmaPR_seas2_op", "BSBmaPR_seas2_cl", "BSBmaPR_2_bag", "BSBmaPR_2_len",
                                           "BSBmaSH_seas2_op", "BSBmaSH_seas2_cl", "BSBmaSH_2_bag", "BSBmaSH_2_len"),
                                value =  c(as.character(input$BSBmaFH_seas1[1]), as.character(input$BSBmaFH_seas1[2]), as.character(input$BSBmaFH_1_bag), as.character(input$BSBmaFH_1_len),
                                           as.character(input$BSBmaPR_seas1[1]), as.character(input$BSBmaPR_seas1[2]), as.character(input$BSBmaPR_1_bag), as.character(input$BSBmaPR_1_len), 
                                           as.character(input$BSBmaSH_seas1[1]), as.character(input$BSBmaSH_seas1[2]), as.character(input$BSBmaSH_1_bag), as.character(input$BSBmaSH_1_len), 
                                           as.character(input$BSBmaFH_seas2[1]), as.character(input$BSBmaFH_seas2[2]), as.character(input$BSBmaFH_2_bag), as.character(input$BSBmaFH_2_len), 
                                           as.character(input$BSBmaPR_seas2[1]), as.character(input$BSBmaPR_seas2[2]), as.character(input$BSBmaPR_2_bag), as.character(input$BSBmaPR_2_len),
                                           as.character(input$BSBmaSH_seas2[1]), as.character(input$BSBmaSH_seas2[2]), as.character(input$BSBmaSH_2_bag), as.character(input$BSBmaSH_2_len)))
      }
      print("made in out of bsb")
      
      MA_regs <- data.frame(run_name = c(Run_Name()), 
                            input =  c("SFmaFH_seas1_op", "SFmaFH_seas1_cl", "SFmaFH_1_bag", "SFmaFH_1_len", 
                                       "SFmaPR_seas1_op", "SFmaPR_seas1_cl", "SFmaPR_1_bag", "SFmaPR_1_len",
                                       "SFmaSH_seas1_op", "SFmaSH_seas1_cl", "SFmaSH_1_bag", "SFmaSH_1_len",
                                       "SFmaFH_seas2_op", "SFmaFH_seas2_cl", "SFmaFH_2_bag", "SFmaFH_2_len", 
                                       "SFmaPR_seas2_op", "SFmaPR_seas2_cl", "SFmaPR_2_bag", "SFmaPR_2_len",
                                       "SFmaSH_seas2_op", "SFmaSH_seas2_cl", "SFmaSH_2_bag", "SFmaSH_2_len",
                                       
                                       "SCUPmaFH_seas1_op", "SCUPmaFH_seas1_cl", "SCUPmaFH_1_bag", "SCUPmaFH_1_len", 
                                       "SCUPmaPR_seas1_op", "SCUPmaPR_seas1_cl", "SCUPmaPR_1_bag", "SCUPmaPR_1_len",
                                       "SCUPmaSH_seas1_op", "SCUPmaSH_seas1_cl", "SCUPmaSH_1_bag", "SCUPmaSH_1_len",
                                       "SCUPmaFH_seas2_op", "SCUPmaFH_seas2_cl", "SCUPmaFH_2_bag", "SCUPmaFH_2_len", 
                                       "SCUPmaPR_seas2_op", "SCUPmaPR_seas2_cl", "SCUPmaPR_2_bag", "SCUPmaPR_2_len",
                                       "SCUPmaSH_seas2_op", "SCUPmaSH_seas2_cl", "SCUPmaSH_2_bag", "SCUPmaSH_2_len", 
                                       "SCUPmaFH_seas3_op", "SCUPmaFH_seas3_cl", "SCUPmaFH_3_bag", "SCUPmaFH_3_len"),
                            value =  c(as.character(input$SFmaFH_seas1[1]), as.character(input$SFmaFH_seas1[2]), as.character(input$SFmaFH_1_bag), as.character(input$SFmaFH_1_len), 
                                       as.character(input$SFmaPR_seas1[1]), as.character(input$SFmaPR_seas1[2]), as.character(input$SFmaPR_1_bag), as.character(input$SFmaPR_1_len),
                                       as.character(input$SFmaSH_seas1[1]), as.character(input$SFmaSH_seas1[2]), as.character(input$SFmaSH_1_bag), as.character(input$SFmaSH_1_len),
                                       as.character(input$SFmaFH_seas2[1]), as.character(input$SFmaFH_seas2[2]), as.character(input$SFmaFH_2_bag), as.character(input$SFmaFH_2_len), 
                                       as.character(input$SFmaPR_seas2[1]), as.character(input$SFmaPR_seas2[2]), as.character(input$SFmaPR_2_bag), as.character(input$SFmaPR_2_len),
                                       as.character(input$SFmaSH_seas2[1]), as.character(input$SFmaSH_seas2[2]), as.character(input$SFmaSH_2_bag), as.character(input$SFmaSH_2_len),
                                       
                                       as.character(input$SCUPmaFH_seas1[1]), as.character(input$SCUPmaFH_seas1[2]), as.character(input$SCUPmaFH_1_bag), as.character(input$SCUPmaFH_1_len), 
                                       as.character(input$SCUPmaPR_seas1[1]), as.character(input$SCUPmaPR_seas1[2]), as.character(input$SCUPmaPR_1_bag), as.character(input$SCUPmaPR_1_len),
                                       as.character(input$SCUPmaSH_seas1[1]), as.character(input$SCUPmaSH_seas1[2]), as.character(input$SCUPmaSH_1_bag), as.character(input$SCUPmaSH_1_len),
                                       as.character(input$SCUPmaFH_seas2[1]), as.character(input$SCUPmaFH_seas2[2]), as.character(input$SCUPmaFH_2_bag), as.character(input$SCUPmaFH_2_len), 
                                       as.character(input$SCUPmaPR_seas2[1]), as.character(input$SCUPmaPR_seas2[2]), as.character(input$SCUPmaPR_2_bag), as.character(input$SCUPmaPR_2_len),
                                       as.character(input$SCUPmaSH_seas2[1]), as.character(input$SCUPmaSH_seas2[2]), as.character(input$SCUPmaSH_2_bag), as.character(input$SCUPmaSH_2_len),
                                       as.character(input$SCUPmaFH_seas3[1]), as.character(input$SCUPmaFH_seas3[2]), as.character(input$SCUPmaFH_3_bag), as.character(input$SCUPmaFH_3_len)))
      
      print("out SF and Scup")
      
      
      regulations <- regulations %>% rbind(MA_regs, bsbMAregs)
      print("made regulations MA")
    }
    
    if(any("RI" == input$state)){
      if(input$SF_RI_input_type == "All Modes Combined"){
        sfRIregs <- data.frame(run_name = c(Run_Name()), 
                                input =  c("SFri_seas1_op", "SFri_seas1_cl", "SFri_1_bag", "SFri_1_len", 
                                           "SFriFH_seas2_op", "SFriFH_seas2_cl", "SFriFH_2_bag", "SFriFH_2_len", 
                                            "SFriPR_seas2_op", "SFriPR_seas2_cl", "SFriPR_2_bag", "SFriPR_2_len",
                                            "SFriSH_seas2_op", "SFriSH_seas2_cl", "SFriSH_2_bag", "SFriSH_2_len"),
                                value =  c(as.character(input$SFri_seas1[1]), as.character(input$SFri_seas1[2]), as.character(input$SFri_1_bag), as.character(input$SFri_1_len), 
                                           as.character(input$SFriFH_seas2[1]), as.character(input$SFriFH_seas2[2]), as.character(input$SFriFH_2_bag), as.character(input$SFriFH_2_len), 
                                           as.character(input$SFriPR_seas2[1]), as.character(input$SFriPR_seas2[2]), as.character(input$SFriPR_2_bag), as.character(input$SFriPR_2_len),
                                           as.character(input$SFriSH_seas2[1]), as.character(input$SFriSH_seas2[2]), as.character(input$SFriSH_2_bag), as.character(input$SFriSH_2_len)))
      }else{
        sfRIregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c("SFriFH_seas1_op", "SFriFH_seas1_cl", "SFriFH_1_bag", "SFriFH_1_len", 
                                            "SFriPR_seas1_op", "SFriPR_seas1_cl", "SFriPR_1_bag", "SFriPR_1_len",
                                            "SFriSH_seas1_op", "SFriSH_seas1_cl", "SFriSH_1_bag", "SFriSH_1_len",
                                            "SFriFH_seas2_op", "SFriFH_seas2_cl", "SFriFH_2_bag", "SFriFH_2_len", 
                                            "SFriPR_seas2_op", "SFriPR_seas2_cl", "SFriPR_2_bag", "SFriPR_2_len",
                                            "SFriSH_seas2_op", "SFriSH_seas2_cl", "SFriSH_2_bag", "SFriSH_2_len"),
                                value = c( as.character(input$SFriFH_seas1[1]), as.character(input$SFriFH_seas1[2]), as.character(input$SFriFH_1_bag), as.character(input$SFriFH_1_len), 
                                           as.character(input$SFriPR_seas1[1]), as.character(input$SFriPR_seas1[2]), as.character(input$SFriPR_1_bag), as.character(input$SFriPR_1_len),
                                           as.character(input$SFriSH_seas1[1]), as.character(input$SFriSH_seas1[2]), as.character(input$SFriSH_1_bag), as.character(input$SFriSH_1_len),
                                           as.character(input$SFriFH_seas2[1]), as.character(input$SFriFH_seas2[2]), as.character(input$SFriFH_2_bag), as.character(input$SFriFH_2_len), 
                                           as.character(input$SFriPR_seas2[1]), as.character(input$SFriPR_seas2[2]), as.character(input$SFriPR_2_bag), as.character(input$SFriPR_2_len),
                                           as.character(input$SFriSH_seas2[1]), as.character(input$SFriSH_seas2[2]), as.character(input$SFriSH_2_bag), as.character(input$SFriSH_2_len)))
      }
      
      
      RI_regs <- data.frame(run_name = c(Run_Name()), 
                            input =  c( "BSBriFH_seas1_op", "BSBriFH_seas1_cl", "BSBriFH_1_bag", "BSBriFH_1_len",
                                        "BSBriPR_seas1_op", "BSBriPR_seas1_cl", "BSBriPR_1_bag", "BSBriPR_1_len",
                                        "BSBriSH_seas1_op", "BSBriSH_seas1_cl", "BSBriSH_1_bag", "BSBriSH_1_len",
                                        "BSBriFH_seas2_op", "BSBriFH_seas2_cl", "BSBriFH_2_bag", "BSBriFH_2_len", 
                                        "BSBriPR_seas2_op", "BSBriPR_seas2_cl", "BSBriPR_2_bag", "BSBriPR_2_len",
                                        "BSBriSH_seas2_op", "BSBriSH_seas2_cl", "BSBriSH_2_bag", "BSBriSH_2_len",
                                        "BSBriFH_seas3_op", "BSBriFH_seas3_cl", "BSBriFH_3_bag", "BSBriFH_3_len", 
                                        "BSBriPR_seas3_op", "BSBriPR_seas3_cl", "BSBriPR_3_bag", "BSBriPR_3_len",
                                        "BSBriSH_seas3_op", "BSBriSH_seas3_cl", "BSBriSH_3_bag", "BSBriSH_3_len",
                                        
                                       "SCUPriFH_seas1_op", "SCUPriFH_seas1_cl", "SCUPriFH_1_bag", "SCUPriFH_1_len", 
                                       "SCUPriPR_seas1_op", "SCUPriPR_seas1_cl", "SCUPriPR_1_bag", "SCUPriPR_1_len",
                                       "SCUPriSH_seas1_op", "SCUPriSH_seas1_cl", "SCUPriSH_1_bag", "SCUPriSH_1_len",
                                       "SCUPriFH_seas2_op", "SCUPriFH_seas2_cl", "SCUPriFH_2_bag", "SCUPriFH_2_len", 
                                       "SCUPriPR_seas2_op", "SCUPriPR_seas2_cl", "SCUPriPR_2_bag", "SCUPriPR_2_len",
                                       "SCUPriSH_seas2_op", "SCUPriSH_seas2_cl", "SCUPriSH_2_bag", "SCUPriSH_2_len", 
                                       "SCUPriFH_seas3_op", "SCUPriFH_seas3_cl", "SCUPriFH_3_bag", "SCUPriFH_3_len", 
                                       "SCUPriFH_seas4_op", "SCUPriFH_seas4_cl", "SCUPriFH_4_bag", "SCUPriFH_4_len"),
                            value =  c(as.character(input$BSBriFH_seas1[1]), as.character(input$BSBriFH_seas1[2]), as.character(input$BSBriFH_1_bag), as.character(input$BSBriFH_1_len),
                                       as.character(input$BSBriPR_seas1[1]), as.character(input$BSBriPR_seas1[2]), as.character(input$BSBriPR_1_bag), as.character(input$BSBriPR_1_len), 
                                       as.character(input$BSBriSH_seas1[1]), as.character(input$BSBriSH_seas1[2]), as.character(input$BSBriSH_1_bag), as.character(input$BSBriSH_1_len), 
                                       as.character(input$BSBriFH_seas2[1]), as.character(input$BSBriFH_seas2[2]), as.character(input$BSBriFH_2_bag), as.character(input$BSBriFH_2_len), 
                                       as.character(input$BSBriPR_seas2[1]), as.character(input$BSBriPR_seas2[2]), as.character(input$BSBriPR_2_bag), as.character(input$BSBriPR_2_len),
                                       as.character(input$BSBriSH_seas2[1]), as.character(input$BSBriSH_seas2[2]), as.character(input$BSBriSH_2_bag), as.character(input$BSBriSH_2_len), 
                                       as.character(input$BSBriFH_seas3[1]), as.character(input$BSBriFH_seas3[2]), as.character(input$BSBriFH_3_bag), as.character(input$BSBriFH_3_len), 
                                       as.character(input$BSBriPR_seas3[1]), as.character(input$BSBriPR_seas3[2]), as.character(input$BSBriPR_3_bag), as.character(input$BSBriPR_3_len),
                                       as.character(input$BSBriSH_seas3[1]), as.character(input$BSBriSH_seas3[2]), as.character(input$BSBriSH_3_bag), as.character(input$BSBriSH_3_len),
                                       
                                       as.character(input$SCUPriFH_seas1[1]), as.character(input$SCUPriFH_seas1[2]), as.character(input$SCUPriFH_1_bag), as.character(input$SCUPriFH_1_len), 
                                       as.character(input$SCUPriPR_seas1[1]), as.character(input$SCUPriPR_seas1[2]), as.character(input$SCUPriPR_1_bag), as.character(input$SCUPriPR_1_len),
                                       as.character(input$SCUPriSH_seas1[1]), as.character(input$SCUPriSH_seas1[2]), as.character(input$SCUPriSH_1_bag), as.character(input$SCUPriSH_1_len),
                                       as.character(input$SCUPriFH_seas2[1]), as.character(input$SCUPriFH_seas2[2]), as.character(input$SCUPriFH_2_bag), as.character(input$SCUPriFH_2_len), 
                                       as.character(input$SCUPriPR_seas2[1]), as.character(input$SCUPriPR_seas2[2]), as.character(input$SCUPriPR_2_bag), as.character(input$SCUPriPR_2_len),
                                       as.character(input$SCUPriSH_seas2[1]), as.character(input$SCUPriSH_seas2[2]), as.character(input$SCUPriSH_2_bag), as.character(input$SCUPriSH_2_len),
                                       as.character(input$SCUPriFH_seas3[1]), as.character(input$SCUPriFH_seas3[2]), as.character(input$SCUPriFH_3_bag), as.character(input$SCUPriFH_3_len), 
                                       as.character(input$SCUPriFH_seas4[1]), as.character(input$SCUPriFH_seas4[2]), as.character(input$SCUPriFH_4_bag), as.character(input$SCUPriFH_4_len)))
      regulations <- regulations %>% rbind(RI_regs, sfRIregs)
    }

    if(any("CT" == input$state)){
            if(input$SF_CT_input_type == "All Modes Combined"){
              sfCTregs <- data.frame(run_name = c(Run_Name()), 
                                     input =  c("SFct_seas1_op", "SFct_seas1_cl", "SFct_1_bag", "SFct_1_len", 
                                                "SFct_seas2_op", "SFct_seas2_cl", "SFct_2_bag", "SFct_2_len", 
                                                "SFctFH_seas3_op", "SFctFH_seas3_cl", "SFctFH_3_bag", "SFctFH_3_len", 
                                                "SFctPR_seas3_op", "SFctPR_seas3_cl", "SFctPR_3_bag", "SFctPR_3_len",
                                                "SFctSH_seas3_op", "SFctSH_seas3_cl", "SFctSH_3_bag", "SFctSH_3_len"),
                                     value =  c(as.character(input$SFct_seas1[1]), as.character(input$SFct_seas1[2]), as.character(input$SFct_1_bag), as.character(input$SFct_1_len), 
                                                as.character(input$SFct_seas2[1]), as.character(input$SFct_seas2[2]), as.character(input$SFct_2_bag), as.character(input$SFct_2_len), 
                                                as.character(input$SFctFH_seas3[1]), as.character(input$SFctFH_seas3[2]), as.character(input$SFctFH_3_bag), as.character(input$SFctFH_3_len), 
                                                as.character(input$SFctPR_seas3[1]), as.character(input$SFctPR_seas3[2]), as.character(input$SFctPR_3_bag), as.character(input$SFctPR_3_len),
                                                as.character(input$SFctSH_seas3[1]), as.character(input$SFctSH_seas3[2]), as.character(input$SFctSH_3_bag), as.character(input$SFctSH_3_len)))
            }else{
              sfCTregs <-  data.frame(run_name = c(Run_Name()), 
                                      input =  c("SFctFH_seas1_op", "SFctFH_seas1_cl", "SFctFH_1_bag", "SFctFH_1_len", 
                                                 "SFctPR_seas1_op", "SFctPR_seas1_cl", "SFctPR_1_bag", "SFctPR_1_len",
                                                 "SFctSH_seas1_op", "SFctSH_seas1_cl", "SFctSH_1_bag", "SFctSH_1_len",
                                                 "SFctFH_seas2_op", "SFctFH_seas2_cl", "SFctFH_2_bag", "SFctFH_2_len", 
                                                 "SFctPR_seas2_op", "SFctPR_seas2_cl", "SFctPR_2_bag", "SFctPR_2_len",
                                                 "SFctSH_seas2_op", "SFctSH_seas2_cl", "SFctSH_2_bag", "SFctSH_2_len", 
                                                 "SFctFH_seas3_op", "SFctFH_seas3_cl", "SFctFH_3_bag", "SFctFH_3_len", 
                                                 "SFctPR_seas3_op", "SFctPR_seas3_cl", "SFctPR_3_bag", "SFctPR_3_len",
                                                 "SFctSH_seas3_op", "SFctSH_seas3_cl", "SFctSH_3_bag", "SFctSH_3_len"),
                                      value = c( as.character(input$SFctFH_seas1[1]), as.character(input$SFctFH_seas1[2]), as.character(input$SFctFH_1_bag), as.character(input$SFctFH_1_len), 
                                                 as.character(input$SFctPR_seas1[1]), as.character(input$SFctPR_seas1[2]), as.character(input$SFctPR_1_bag), as.character(input$SFctPR_1_len),
                                                 as.character(input$SFctSH_seas1[1]), as.character(input$SFctSH_seas1[2]), as.character(input$SFctSH_1_bag), as.character(input$SFctSH_1_len),
                                                 as.character(input$SFctFH_seas2[1]), as.character(input$SFctFH_seas2[2]), as.character(input$SFctFH_2_bag), as.character(input$SFctFH_2_len), 
                                                 as.character(input$SFctPR_seas2[1]), as.character(input$SFctPR_seas2[2]), as.character(input$SFctPR_2_bag), as.character(input$SFctPR_2_len),
                                                 as.character(input$SFctSH_seas2[1]), as.character(input$SFctSH_seas2[2]), as.character(input$SFctSH_2_bag), as.character(input$SFctSH_2_len), 
                                                 as.character(input$SFctFH_seas3[1]), as.character(input$SFctFH_seas3[2]), as.character(input$SFctFH_3_bag), as.character(input$SFctFH_3_len), 
                                                 as.character(input$SFctPR_seas3[1]), as.character(input$SFctPR_seas3[2]), as.character(input$SFctPR_3_bag), as.character(input$SFctPR_3_len),
                                                 as.character(input$SFctSH_seas3[1]), as.character(input$SFctSH_seas3[2]), as.character(input$SFctSH_3_bag), as.character(input$SFctSH_3_len)))
    }
    
    
    CT_regs <- data.frame(run_name = c(Run_Name()), 
                          input =  c( "BSBctFH_seas1_op", "BSBctFH_seas1_cl", "BSBctFH_1_bag", "BSBctFH_1_len",
                                      "BSBctPR_seas1_op", "BSBctPR_seas1_cl", "BSBctPR_1_bag", "BSBctPR_1_len",
                                      "BSBctSH_seas1_op", "BSBctSH_seas1_cl", "BSBctSH_1_bag", "BSBctSH_1_len",
                                      "BSBctFH_seas2_op", "BSBctFH_seas2_cl", "BSBctFH_2_bag", "BSBctFH_2_len", 
                                      "BSBctPR_seas2_op", "BSBctPR_seas2_cl", "BSBctPR_2_bag", "BSBctPR_2_len",
                                      "BSBctSH_seas2_op", "BSBctSH_seas2_cl", "BSBctSH_2_bag", "BSBctSH_2_len",
                                      "BSBctFH_seas3_op", "BSBctFH_seas3_cl", "BSBctFH_3_bag", "BSBctFH_3_len", 
                                      "BSBctPR_seas3_op", "BSBctPR_seas3_cl", "BSBctPR_3_bag", "BSBctPR_3_len",
                                      "BSBctSH_seas3_op", "BSBctSH_seas3_cl", "BSBctSH_3_bag", "BSBctSH_3_len",
                                      
                                      "SCUPctFH_seas1_op", "SCUPctFH_seas1_cl", "SCUPctFH_1_bag", "SCUPctFH_1_len", 
                                      "SCUPctPR_seas1_op", "SCUPctPR_seas1_cl", "SCUPctPR_1_bag", "SCUPctPR_1_len",
                                      "SCUPctSH_seas1_op", "SCUPctSH_seas1_cl", "SCUPctSH_1_bag", "SCUPctSH_1_len",
                                      "SCUPctFH_seas2_op", "SCUPctFH_seas2_cl", "SCUPctFH_2_bag", "SCUPctFH_2_len", 
                                      "SCUPctPR_seas2_op", "SCUPctPR_seas2_cl", "SCUPctPR_2_bag", "SCUPctPR_2_len",
                                      "SCUPctSH_seas2_op", "SCUPctSH_seas2_cl", "SCUPctSH_2_bag", "SCUPctSH_2_len", 
                                      "SCUPctFH_seas3_op", "SCUPctFH_seas3_cl", "SCUPctFH_3_bag", "SCUPctFH_3_len", 
                                      "SCUPctFH_seas4_op", "SCUPctFH_seas4_cl", "SCUPctFH_4_bag", "SCUPctFH_4_len"),
                          value =  c(as.character(input$BSBctFH_seas1[1]), as.character(input$BSBctFH_seas1[2]), as.character(input$BSBctFH_1_bag), as.character(input$BSBctFH_1_len),
                                     as.character(input$BSBctPR_seas1[1]), as.character(input$BSBctPR_seas1[2]), as.character(input$BSBctPR_1_bag), as.character(input$BSBctPR_1_len), 
                                     as.character(input$BSBctSH_seas1[1]), as.character(input$BSBctSH_seas1[2]), as.character(input$BSBctSH_1_bag), as.character(input$BSBctSH_1_len), 
                                     as.character(input$BSBctFH_seas2[1]), as.character(input$BSBctFH_seas2[2]), as.character(input$BSBctFH_2_bag), as.character(input$BSBctFH_2_len), 
                                     as.character(input$BSBctPR_seas2[1]), as.character(input$BSBctPR_seas2[2]), as.character(input$BSBctPR_2_bag), as.character(input$BSBctPR_2_len),
                                     as.character(input$BSBctSH_seas2[1]), as.character(input$BSBctSH_seas2[2]), as.character(input$BSBctSH_2_bag), as.character(input$BSBctSH_2_len), 
                                     as.character(input$BSBctFH_seas3[1]), as.character(input$BSBctFH_seas3[2]), as.character(input$BSBctFH_3_bag), as.character(input$BSBctFH_3_len), 
                                     as.character(input$BSBctPR_seas3[1]), as.character(input$BSBctPR_seas3[2]), as.character(input$BSBctPR_3_bag), as.character(input$BSBctPR_3_len),
                                     as.character(input$BSBctSH_seas3[1]), as.character(input$BSBctSH_seas3[2]), as.character(input$BSBctSH_3_bag), as.character(input$BSBctSH_3_len),
                                     
                                     as.character(input$SCUPctFH_seas1[1]), as.character(input$SCUPctFH_seas1[2]), as.character(input$SCUPctFH_1_bag), as.character(input$SCUPctFH_1_len), 
                                     as.character(input$SCUPctPR_seas1[1]), as.character(input$SCUPctPR_seas1[2]), as.character(input$SCUPctPR_1_bag), as.character(input$SCUPctPR_1_len),
                                     as.character(input$SCUPctSH_seas1[1]), as.character(input$SCUPctSH_seas1[2]), as.character(input$SCUPctSH_1_bag), as.character(input$SCUPctSH_1_len),
                                     as.character(input$SCUPctFH_seas2[1]), as.character(input$SCUPctFH_seas2[2]), as.character(input$SCUPctFH_2_bag), as.character(input$SCUPctFH_2_len), 
                                     as.character(input$SCUPctPR_seas2[1]), as.character(input$SCUPctPR_seas2[2]), as.character(input$SCUPctPR_2_bag), as.character(input$SCUPctPR_2_len),
                                     as.character(input$SCUPctSH_seas2[1]), as.character(input$SCUPctSH_seas2[2]), as.character(input$SCUPctSH_2_bag), as.character(input$SCUPctSH_2_len),
                                     as.character(input$SCUPctFH_seas3[1]), as.character(input$SCUPctFH_seas3[2]), as.character(input$SCUPctFH_3_bag), as.character(input$SCUPctFH_3_len), 
                                     as.character(input$SCUPctFH_seas4[1]), as.character(input$SCUPctFH_seas4[2]), as.character(input$SCUPctFH_4_bag), as.character(input$SCUPctFH_4_len)))
    regulations <- regulations %>% rbind(CT_regs, sfCTregs)
    
    }

    if(any("NY" == input$state)){
      if(input$SF_NY_input_type == "All Modes Combined"){
        sfNYregs <- data.frame(run_name = c(Run_Name()), 
                               input =  c("SFny_seas1_op", "SFny_seas1_cl", "SFny_1_bag", "SFny_1_len", 
                                          "SFny_seas2_op", "SFny_seas2_cl", "SFny_2_bag", "SFny_2_len", 
                                          "SFnyFH_seas3_op", "SFnyFH_seas3_cl", "SFnyFH_3_bag", "SFnyFH_3_len", 
                                          "SFnyPR_seas3_op", "SFnyPR_seas3_cl", "SFnyPR_3_bag", "SFnyPR_3_len",
                                          "SFnySH_seas3_op", "SFnySH_seas3_cl", "SFnySH_3_bag", "SFnySH_3_len"),
                               value =  c(as.character(input$SFny_seas1[1]), as.character(input$SFny_seas1[2]), as.character(input$SFny_1_bag), as.character(input$SFny_1_len), 
                                          as.character(input$SFny_seas2[1]), as.character(input$SFny_seas2[2]), as.character(input$SFny_2_bag), as.character(input$SFny_2_len), 
                                          as.character(input$SFnyFH_seas3[1]), as.character(input$SFnyFH_seas3[2]), as.character(input$SFnyFH_3_bag), as.character(input$SFnyFH_3_len), 
                                          as.character(input$SFnyPR_seas3[1]), as.character(input$SFnyPR_seas3[2]), as.character(input$SFnyPR_3_bag), as.character(input$SFnyPR_3_len),
                                          as.character(input$SFnySH_seas3[1]), as.character(input$SFnySH_seas3[2]), as.character(input$SFnySH_3_bag), as.character(input$SFnySH_3_len)))
      }else{
        sfNYregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c("SFnyFH_seas1_op", "SFnyFH_seas1_cl", "SFnyFH_1_bag", "SFnyFH_1_len", 
                                           "SFnyPR_seas1_op", "SFnyPR_seas1_cl", "SFnyPR_1_bag", "SFnyPR_1_len",
                                           "SFnySH_seas1_op", "SFnySH_seas1_cl", "SFnySH_1_bag", "SFnySH_1_len",
                                           "SFnyFH_seas2_op", "SFnyFH_seas2_cl", "SFnyFH_2_bag", "SFnyFH_2_len", 
                                           "SFnyPR_seas2_op", "SFnyPR_seas2_cl", "SFnyPR_2_bag", "SFnyPR_2_len",
                                           "SFnySH_seas2_op", "SFnySH_seas2_cl", "SFnySH_2_bag", "SFnySH_2_len", 
                                           "SFnyFH_seas3_op", "SFnyFH_seas3_cl", "SFnyFH_3_bag", "SFnyFH_3_len", 
                                           "SFnyPR_seas3_op", "SFnyPR_seas3_cl", "SFnyPR_3_bag", "SFnyPR_3_len",
                                           "SFnySH_seas3_op", "SFnySH_seas3_cl", "SFnySH_3_bag", "SFnySH_3_len"),
                                value = c( as.character(input$SFnyFH_seas1[1]), as.character(input$SFnyFH_seas1[2]), as.character(input$SFnyFH_1_bag), as.character(input$SFnyFH_1_len), 
                                           as.character(input$SFnyPR_seas1[1]), as.character(input$SFnyPR_seas1[2]), as.character(input$SFnyPR_1_bag), as.character(input$SFnyPR_1_len),
                                           as.character(input$SFnySH_seas1[1]), as.character(input$SFnySH_seas1[2]), as.character(input$SFnySH_1_bag), as.character(input$SFnySH_1_len),
                                           as.character(input$SFnyFH_seas2[1]), as.character(input$SFnyFH_seas2[2]), as.character(input$SFnyFH_2_bag), as.character(input$SFnyFH_2_len), 
                                           as.character(input$SFnyPR_seas2[1]), as.character(input$SFnyPR_seas2[2]), as.character(input$SFnyPR_2_bag), as.character(input$SFnyPR_2_len),
                                           as.character(input$SFnySH_seas2[1]), as.character(input$SFnySH_seas2[2]), as.character(input$SFnySH_2_bag), as.character(input$SFnySH_2_len), 
                                           as.character(input$SFnyFH_seas3[1]), as.character(input$SFnyFH_seas3[2]), as.character(input$SFnyFH_3_bag), as.character(input$SFnyFH_3_len), 
                                           as.character(input$SFnyPR_seas3[1]), as.character(input$SFnyPR_seas3[2]), as.character(input$SFnyPR_3_bag), as.character(input$SFnyPR_3_len),
                                           as.character(input$SFnySH_seas3[1]), as.character(input$SFnySH_seas3[2]), as.character(input$SFnySH_3_bag), as.character(input$SFnySH_3_len)))
      }
      
      
      if(input$BSB_NY_input_type == "All Modes Combined"){
        bsbNYregs <- data.frame(run_name = c(Run_Name()), 
                               input =  c("BSBny_seas1_op", "BSBny_seas1_cl", "BSBny_1_bag", "BSBny_1_len", 
                                          "BSBny_seas2_op", "BSBny_seas2_cl", "BSBny_2_bag", "BSBny_2_len", 
                                          "BSBnyFH_seas3_op", "BSBnyFH_seas3_cl", "BSBnyFH_3_bag", "BSBnyFH_3_len", 
                                          "BSBnyPR_seas3_op", "BSBnyPR_seas3_cl", "BSBnyPR_3_bag", "BSBnyPR_3_len",
                                          "BSBnySH_seas3_op", "BSBnySH_seas3_cl", "BSBnySH_3_bag", "BSBnySH_3_len"),
                               value =  c(as.character(input$BSBny_seas1[1]), as.character(input$BSBny_seas1[2]), as.character(input$BSBny_1_bag), as.character(input$BSBny_1_len), 
                                          as.character(input$BSBny_seas2[1]), as.character(input$BSBny_seas2[2]), as.character(input$BSBny_2_bag), as.character(input$BSBny_2_len), 
                                          as.character(input$BSBnyFH_seas3[1]), as.character(input$BSBnyFH_seas3[2]), as.character(input$BSBnyFH_3_bag), as.character(input$BSBnyFH_3_len), 
                                          as.character(input$BSBnyPR_seas3[1]), as.character(input$BSBnyPR_seas3[2]), as.character(input$BSBnyPR_3_bag), as.character(input$BSBnyPR_3_len),
                                          as.character(input$BSBnySH_seas3[1]), as.character(input$BSBnySH_seas3[2]), as.character(input$BSBnySH_3_bag), as.character(input$BSBnySH_3_len)))
      }else{
        bsbNYregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c( "BSBnyFH_seas1_op", "BSBnyFH_seas1_cl", "BSBnyFH_1_bag", "BSBnyFH_1_len",
                                            "BSBnyPR_seas1_op", "BSBnyPR_seas1_cl", "BSBnyPR_1_bag", "BSBnyPR_1_len",
                                            "BSBnySH_seas1_op", "BSBnySH_seas1_cl", "BSBnySH_1_bag", "BSBnySH_1_len",
                                            "BSBnyFH_seas2_op", "BSBnyFH_seas2_cl", "BSBnyFH_2_bag", "BSBnyFH_2_len", 
                                            "BSBnyPR_seas2_op", "BSBnyPR_seas2_cl", "BSBnyPR_2_bag", "BSBnyPR_2_len",
                                            "BSBnySH_seas2_op", "BSBnySH_seas2_cl", "BSBnySH_2_bag", "BSBnySH_2_len",
                                            "BSBnyFH_seas3_op", "BSBnyFH_seas3_cl", "BSBnyFH_3_bag", "BSBnyFH_3_len", 
                                            "BSBnyPR_seas3_op", "BSBnyPR_seas3_cl", "BSBnyPR_3_bag", "BSBnyPR_3_len",
                                            "BSBnySH_seas3_op", "BSBnySH_seas3_cl", "BSBnySH_3_bag", "BSBnySH_3_len"),
                                value = c( as.character(input$BSBnyFH_seas1[1]), as.character(input$BSBnyFH_seas1[2]), as.character(input$BSBnyFH_1_bag), as.character(input$BSBnyFH_1_len),
                                           as.character(input$BSBnyPR_seas1[1]), as.character(input$BSBnyPR_seas1[2]), as.character(input$BSBnyPR_1_bag), as.character(input$BSBnyPR_1_len), 
                                           as.character(input$BSBnySH_seas1[1]), as.character(input$BSBnySH_seas1[2]), as.character(input$BSBnySH_1_bag), as.character(input$BSBnySH_1_len), 
                                           as.character(input$BSBnyFH_seas2[1]), as.character(input$BSBnyFH_seas2[2]), as.character(input$BSBnyFH_2_bag), as.character(input$BSBnyFH_2_len), 
                                           as.character(input$BSBnyPR_seas2[1]), as.character(input$BSBnyPR_seas2[2]), as.character(input$BSBnyPR_2_bag), as.character(input$BSBnyPR_2_len),
                                           as.character(input$BSBnySH_seas2[1]), as.character(input$BSBnySH_seas2[2]), as.character(input$BSBnySH_2_bag), as.character(input$BSBnySH_2_len), 
                                           as.character(input$BSBnyFH_seas3[1]), as.character(input$BSBnyFH_seas3[2]), as.character(input$BSBnyFH_3_bag), as.character(input$BSBnyFH_3_len), 
                                           as.character(input$BSBnyPR_seas3[1]), as.character(input$BSBnyPR_seas3[2]), as.character(input$BSBnyPR_3_bag), as.character(input$BSBnyPR_3_len),
                                           as.character(input$BSBnySH_seas3[1]), as.character(input$BSBnySH_seas3[2]), as.character(input$BSBnySH_3_bag), as.character(input$BSBnySH_3_len)))
      }
      
      
      
      NY_regs <- data.frame(run_name = c(Run_Name()), 
                            input =  c( "SCUPnyFH_seas1_op", "SCUPnyFH_seas1_cl", "SCUPnyFH_1_bag", "SCUPnyFH_1_len", 
                                        "SCUPnyPR_seas1_op", "SCUPnyPR_seas1_cl", "SCUPnyPR_1_bag", "SCUPnyPR_1_len",
                                        "SCUPnySH_seas1_op", "SCUPnySH_seas1_cl", "SCUPnySH_1_bag", "SCUPnySH_1_len",
                                        "SCUPnyFH_seas2_op", "SCUPnyFH_seas2_cl", "SCUPnyFH_2_bag", "SCUPnyFH_2_len", 
                                        "SCUPnyPR_seas2_op", "SCUPnyPR_seas2_cl", "SCUPnyPR_2_bag", "SCUPnyPR_2_len",
                                        "SCUPnySH_seas2_op", "SCUPnySH_seas2_cl", "SCUPnySH_2_bag", "SCUPnySH_2_len", 
                                        "SCUPnyFH_seas3_op", "SCUPnyFH_seas3_cl", "SCUPnyFH_3_bag", "SCUPnyFH_3_len", 
                                        "SCUPnyFH_seas4_op", "SCUPnyFH_seas4_cl", "SCUPnyFH_4_bag", "SCUPnyFH_4_len"),
                            value =  c(as.character(input$SCUPnyFH_seas1[1]), as.character(input$SCUPnyFH_seas1[2]), as.character(input$SCUPnyFH_1_bag), as.character(input$SCUPnyFH_1_len), 
                                       as.character(input$SCUPnyPR_seas1[1]), as.character(input$SCUPnyPR_seas1[2]), as.character(input$SCUPnyPR_1_bag), as.character(input$SCUPnyPR_1_len),
                                       as.character(input$SCUPnySH_seas1[1]), as.character(input$SCUPnySH_seas1[2]), as.character(input$SCUPnySH_1_bag), as.character(input$SCUPnySH_1_len),
                                       as.character(input$SCUPnyFH_seas2[1]), as.character(input$SCUPnyFH_seas2[2]), as.character(input$SCUPnyFH_2_bag), as.character(input$SCUPnyFH_2_len), 
                                       as.character(input$SCUPnyPR_seas2[1]), as.character(input$SCUPnyPR_seas2[2]), as.character(input$SCUPnyPR_2_bag), as.character(input$SCUPnyPR_2_len),
                                       as.character(input$SCUPnySH_seas2[1]), as.character(input$SCUPnySH_seas2[2]), as.character(input$SCUPnySH_2_bag), as.character(input$SCUPnySH_2_len),
                                       as.character(input$SCUPnyFH_seas3[1]), as.character(input$SCUPnyFH_seas3[2]), as.character(input$SCUPnyFH_3_bag), as.character(input$SCUPnyFH_3_len), 
                                       as.character(input$SCUPnyFH_seas4[1]), as.character(input$SCUPnyFH_seas4[2]), as.character(input$SCUPnyFH_4_bag), as.character(input$SCUPnyFH_4_len)))
      regulations <- regulations %>% rbind(NY_regs, sfNYregs, bsbNYregs)
      
    }

    if(any("NJ" == input$state)){
      if(input$SF_NJ_input_type == "All Modes Combined"){
        sfNJregs <- data.frame(run_name = c(Run_Name()), 
                               input =  c("SFnj_seas1_op", "SFnj_seas1_cl", "SFnj_1_bag", "SFnj_1_len", 
                                          "SFnjFH_seas2_op", "SFnjFH_seas2_cl", "SFnjFH_2_bag", "SFnjFH_2_len", 
                                          "SFnjPR_seas2_op", "SFnjPR_seas2_cl", "SFnjPR_2_bag", "SFnjPR_2_len",
                                          "SFnjSH_seas2_op", "SFnjSH_seas2_cl", "SFnjSH_2_bag", "SFnjSH_2_len"),
                               value =  c(as.character(input$SFnj_seas1[1]), as.character(input$SFnj_seas1[2]), as.character(input$SFnj_1_bag), as.character(input$SFnj_1_len), 
                                          as.character(input$SFnjFH_seas2[1]), as.character(input$SFnjFH_seas2[2]), as.character(input$SFnjFH_2_bag), as.character(input$SFnjFH_2_len), 
                                          as.character(input$SFnjPR_seas2[1]), as.character(input$SFnjPR_seas2[2]), as.character(input$SFnjPR_2_bag), as.character(input$SFnjPR_2_len),
                                          as.character(input$SFnjSH_seas2[1]), as.character(input$SFnjSH_seas2[2]), as.character(input$SFnjSH_2_bag), as.character(input$SFnjSH_2_len)))
      }else{
        sfNJregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c("SFnjFH_seas1_op", "SFnjFH_seas1_cl", "SFnjFH_1_bag", "SFnjFH_1_len", 
                                           "SFnjPR_seas1_op", "SFnjPR_seas1_cl", "SFnjPR_1_bag", "SFnjPR_1_len",
                                           "SFnjSH_seas1_op", "SFnjSH_seas1_cl", "SFnjSH_1_bag", "SFnjSH_1_len",
                                           "SFnjFH_seas2_op", "SFnjFH_seas2_cl", "SFnjFH_2_bag", "SFnjFH_2_len", 
                                           "SFnjPR_seas2_op", "SFnjPR_seas2_cl", "SFnjPR_2_bag", "SFnjPR_2_len",
                                           "SFnjSH_seas2_op", "SFnjSH_seas2_cl", "SFnjSH_2_bag", "SFnjSH_2_len"),
                                value = c( as.character(input$SFnjFH_seas1[1]), as.character(input$SFnjFH_seas1[2]), as.character(input$SFnjFH_1_bag), as.character(input$SFnjFH_1_len), 
                                           as.character(input$SFnjPR_seas1[1]), as.character(input$SFnjPR_seas1[2]), as.character(input$SFnjPR_1_bag), as.character(input$SFnjPR_1_len),
                                           as.character(input$SFnjSH_seas1[1]), as.character(input$SFnjSH_seas1[2]), as.character(input$SFnjSH_1_bag), as.character(input$SFnjSH_1_len),
                                           as.character(input$SFnjFH_seas2[1]), as.character(input$SFnjFH_seas2[2]), as.character(input$SFnjFH_2_bag), as.character(input$SFnjFH_2_len), 
                                           as.character(input$SFnjPR_seas2[1]), as.character(input$SFnjPR_seas2[2]), as.character(input$SFnjPR_2_bag), as.character(input$SFnjPR_2_len),
                                           as.character(input$SFnjSH_seas2[1]), as.character(input$SFnjSH_seas2[2]), as.character(input$SFnjSH_2_bag), as.character(input$SFnjSH_2_len)))
      }
      
      
      if(input$BSB_NJ_input_type == "All Modes Combined"){
        bsbNJregs <- data.frame(run_name = c(Run_Name()), 
                                input =  c("BSBnj_seas1_op", "BSBnj_seas1_cl", "BSBnj_1_bag", "BSBnj_1_len", 
                                           "BSBnj_seas2_op", "BSBnj_seas2_cl", "BSBnj_2_bag", "BSBnj_2_len", 
                                           "BSBnj_seas3_op", "BSBnj_seas3_cl", "BSBnj_3_bag", "BSBnj_3_len", 
                                           "BSBnj_seas4_op", "BSBnj_seas4_cl", "BSBnj_4_bag", "BSBnj_4_len", 
                                           "BSBnjFH_seas5_op", "BSBnjFH_seas5_cl", "BSBnjFH_5_bag", "BSBnjFH_5_len", 
                                           "BSBnjPR_seas5_op", "BSBnjPR_seas5_cl", "BSBnjPR_5_bag", "BSBnjPR_5_len",
                                           "BSBnjSH_seas5_op", "BSBnjSH_seas5_cl", "BSBnjSH_5_bag", "BSBnjSH_5_len"),
                                value =  c(as.character(input$BSBnj_seas1[1]), as.character(input$BSBnj_seas1[2]), as.character(input$BSBnj_1_bag), as.character(input$BSBnj_1_len), 
                                           as.character(input$BSBnj_seas2[1]), as.character(input$BSBnj_seas2[2]), as.character(input$BSBnj_2_bag), as.character(input$BSBnj_2_len), 
                                           as.character(input$BSBnj_seas3[1]), as.character(input$BSBnj_seas3[2]), as.character(input$BSBnj_3_bag), as.character(input$BSBnj_3_len), 
                                           as.character(input$BSBnj_seas4[1]), as.character(input$BSBnj_seas4[2]), as.character(input$BSBnj_4_bag), as.character(input$BSBnj_4_len),
                                           as.character(input$BSBnjFH_seas5[1]), as.character(input$BSBnjFH_seas5[2]), as.character(input$BSBnjFH_5_bag), as.character(input$BSBnjFH_5_len), 
                                           as.character(input$BSBnjPR_seas5[1]), as.character(input$BSBnjPR_seas5[2]), as.character(input$BSBnjPR_5_bag), as.character(input$BSBnjPR_5_len),
                                           as.character(input$BSBnjSH_seas5[1]), as.character(input$BSBnjSH_seas5[2]), as.character(input$BSBnjSH_5_bag), as.character(input$BSBnjSH_5_len)))
      }else{
        bsbNJregs <-  data.frame(run_name = c(Run_Name()), 
                                 input =  c( "BSBnjFH_seas1_op", "BSBnjFH_seas1_cl", "BSBnjFH_1_bag", "BSBnjFH_1_len",
                                             "BSBnjPR_seas1_op", "BSBnjPR_seas1_cl", "BSBnjPR_1_bag", "BSBnjPR_1_len",
                                             "BSBnjSH_seas1_op", "BSBnjSH_seas1_cl", "BSBnjSH_1_bag", "BSBnjSH_1_len",
                                             "BSBnjFH_seas2_op", "BSBnjFH_seas2_cl", "BSBnjFH_2_bag", "BSBnjFH_2_len", 
                                             "BSBnjPR_seas2_op", "BSBnjPR_seas2_cl", "BSBnjPR_2_bag", "BSBnjPR_2_len",
                                             "BSBnjSH_seas2_op", "BSBnjSH_seas2_cl", "BSBnjSH_2_bag", "BSBnjSH_2_len",
                                             "BSBnjFH_seas3_op", "BSBnjFH_seas3_cl", "BSBnjFH_3_bag", "BSBnjFH_3_len", 
                                             "BSBnjPR_seas3_op", "BSBnjPR_seas3_cl", "BSBnjPR_3_bag", "BSBnjPR_3_len",
                                             "BSBnjSH_seas3_op", "BSBnjSH_seas3_cl", "BSBnjSH_3_bag", "BSBnjSH_3_len",
                                             "BSBnjFH_seas4_op", "BSBnjFH_seas4_cl", "BSBnjFH_4_bag", "BSBnjFH_4_len",
                                             "BSBnjPR_seas4_op", "BSBnjPR_seas4_cl", "BSBnjPR_4_bag", "BSBnjPR_4_len",
                                             "BSBnjSH_seas4_op", "BSBnjSH_seas4_cl", "BSBnjSH_4_bag", "BSBnjSH_4_len",
                                             "BSBnjFH_seas5_op", "BSBnjFH_seas5_cl", "BSBnjFH_5_bag", "BSBnjFH_5_len", 
                                             "BSBnjPR_seas5_op", "BSBnjPR_seas5_cl", "BSBnjPR_5_bag", "BSBnjPR_5_len",
                                             "BSBnjSH_seas5_op", "BSBnjSH_seas5_cl", "BSBnjSH_5_bag", "BSBnjSH_5_len"),
                                 value = c( as.character(input$BSBnjFH_seas1[1]), as.character(input$BSBnjFH_seas1[2]), as.character(input$BSBnjFH_1_bag), as.character(input$BSBnjFH_1_len),
                                            as.character(input$BSBnjPR_seas1[1]), as.character(input$BSBnjPR_seas1[2]), as.character(input$BSBnjPR_1_bag), as.character(input$BSBnjPR_1_len), 
                                            as.character(input$BSBnjSH_seas1[1]), as.character(input$BSBnjSH_seas1[2]), as.character(input$BSBnjSH_1_bag), as.character(input$BSBnjSH_1_len), 
                                            as.character(input$BSBnjFH_seas2[1]), as.character(input$BSBnjFH_seas2[2]), as.character(input$BSBnjFH_2_bag), as.character(input$BSBnjFH_2_len), 
                                            as.character(input$BSBnjPR_seas2[1]), as.character(input$BSBnjPR_seas2[2]), as.character(input$BSBnjPR_2_bag), as.character(input$BSBnjPR_2_len),
                                            as.character(input$BSBnjSH_seas2[1]), as.character(input$BSBnjSH_seas2[2]), as.character(input$BSBnjSH_2_bag), as.character(input$BSBnjSH_2_len), 
                                            as.character(input$BSBnjFH_seas3[1]), as.character(input$BSBnjFH_seas3[2]), as.character(input$BSBnjFH_3_bag), as.character(input$BSBnjFH_3_len), 
                                            as.character(input$BSBnjPR_seas3[1]), as.character(input$BSBnjPR_seas3[2]), as.character(input$BSBnjPR_3_bag), as.character(input$BSBnjPR_3_len),
                                            as.character(input$BSBnjSH_seas3[1]), as.character(input$BSBnjSH_seas3[2]), as.character(input$BSBnjSH_3_bag), as.character(input$BSBnjSH_3_len), 
                                            as.character(input$BSBnjFH_seas4[1]), as.character(input$BSBnjFH_seas4[2]), as.character(input$BSBnjFH_4_bag), as.character(input$BSBnjFH_4_len),
                                            as.character(input$BSBnjPR_seas4[1]), as.character(input$BSBnjPR_seas4[2]), as.character(input$BSBnjPR_4_bag), as.character(input$BSBnjPR_4_len), 
                                            as.character(input$BSBnjSH_seas4[1]), as.character(input$BSBnjSH_seas4[2]), as.character(input$BSBnjSH_4_bag), as.character(input$BSBnjSH_4_len), 
                                            as.character(input$BSBnjFH_seas5[1]), as.character(input$BSBnjFH_seas5[2]), as.character(input$BSBnjFH_5_bag), as.character(input$BSBnjFH_5_len), 
                                            as.character(input$BSBnjPR_seas5[1]), as.character(input$BSBnjPR_seas5[2]), as.character(input$BSBnjPR_5_bag), as.character(input$BSBnjPR_5_len),
                                            as.character(input$BSBnjSH_seas5[1]), as.character(input$BSBnjSH_seas5[2]), as.character(input$BSBnjSH_5_bag), as.character(input$BSBnjSH_5_len)))
      }
      
      
      if(input$SCUP_NJ_input_type == "All Modes Combined"){
        scupNJregs <- data.frame(run_name = c(Run_Name()), 
                                input =  c("SCUPnj_seas1_op", "SCUPnj_seas1_cl", "SCUPnj_1_bag", "SCUPnj_1_len", 
                                           "SCUPnj_seas2_op", "SCUPnj_seas2_cl", "SCUPnj_2_bag", "SCUPnj_2_len",
                                           "SCUPnjFH_seas3_op", "SCUPnjFH_seas3_cl", "SCUPnjFH_3_bag", "SCUPnjFH_3_len", 
                                           "SCUPnjPR_seas3_op", "SCUPnjPR_seas3_cl", "SCUPnjPR_3_bag", "SCUPnjPR_3_len",
                                           "SCUPnjSH_seas3_op", "SCUPnjSH_seas3_cl", "SCUPnjSH_3_bag", "SCUPnjSH_3_len"),
                                value =  c(as.character(input$SCUPnj_seas1[1]), as.character(input$SCUPnj_seas1[2]), as.character(input$SCUPnj_1_bag), as.character(input$SCUPnj_1_len),
                                           as.character(input$SCUPnj_seas2[1]), as.character(input$SCUPnj_seas2[2]), as.character(input$SCUPnj_2_bag), as.character(input$SCUPnj_2_len), 
                                           as.character(input$SCUPnjFH_seas3[1]), as.character(input$SCUPnjFH_seas3[2]), as.character(input$SCUPnjFH_3_bag), as.character(input$SCUPnjFH_3_len), 
                                           as.character(input$SCUPnjPR_seas3[1]), as.character(input$SCUPnjPR_seas3[2]), as.character(input$SCUPnjPR_3_bag), as.character(input$SCUPnjPR_3_len),
                                           as.character(input$SCUPnjSH_seas3[1]), as.character(input$SCUPnjSH_seas3[2]), as.character(input$SCUPnjSH_3_bag), as.character(input$SCUPnjSH_3_len)))
      }else{
        scupbNJregs <-  data.frame(run_name = c(Run_Name()), 
                                 input =  c( "SCUPnjFH_seas1_op", "SCUPnjFH_seas1_cl", "SCUPnjFH_1_bag", "SCUPnjFH_1_len", 
                                             "SCUPnjPR_seas1_op", "SCUPnjPR_seas1_cl", "SCUPnjPR_1_bag", "SCUPnjPR_1_len",
                                             "SCUPnjSH_seas1_op", "SCUPnjSH_seas1_cl", "SCUPnjSH_1_bag", "SCUPnjSH_1_len",
                                             "SCUPnjFH_seas2_op", "SCUPnjFH_seas2_cl", "SCUPnjFH_2_bag", "SCUPnjFH_2_len", 
                                             "SCUPnjPR_seas2_op", "SCUPnjPR_seas2_cl", "SCUPnjPR_2_bag", "SCUPnjPR_2_len",
                                             "SCUPnjSH_seas2_op", "SCUPnjSH_seas2_cl", "SCUPnjSH_2_bag", "SCUPnjSH_2_len", 
                                             "SCUPnjFH_seas3_op", "SCUPnjFH_seas3_cl", "SCUPnjFH_3_bag", "SCUPnjFH_3_len", 
                                             "SCUPnjPR_seas3_op", "SCUPnjPR_seas3_cl", "SCUPnjPR_3_bag", "SCUPnjPR_3_len",
                                             "SCUPnjSH_seas3_op", "SCUPnjSH_seas3_cl", "SCUPnjSH_3_bag", "SCUPnjSH_3_len"),
                                 value =  c(as.character(input$SCUPnjFH_seas1[1]), as.character(input$SCUPnjFH_seas1[2]), as.character(input$SCUPnjFH_1_bag), as.character(input$SCUPnjFH_1_len), 
                                            as.character(input$SCUPnjPR_seas1[1]), as.character(input$SCUPnjPR_seas1[2]), as.character(input$SCUPnjPR_1_bag), as.character(input$SCUPnjPR_1_len),
                                            as.character(input$SCUPnjSH_seas1[1]), as.character(input$SCUPnjSH_seas1[2]), as.character(input$SCUPnjSH_1_bag), as.character(input$SCUPnjSH_1_len),
                                            as.character(input$SCUPnjFH_seas2[1]), as.character(input$SCUPnjFH_seas2[2]), as.character(input$SCUPnjFH_2_bag), as.character(input$SCUPnjFH_2_len), 
                                            as.character(input$SCUPnjPR_seas2[1]), as.character(input$SCUPnjPR_seas2[2]), as.character(input$SCUPnjPR_2_bag), as.character(input$SCUPnjPR_2_len),
                                            as.character(input$SCUPnjSH_seas2[1]), as.character(input$SCUPnjSH_seas2[2]), as.character(input$SCUPnjSH_2_bag), as.character(input$SCUPnjSH_2_len),
                                            as.character(input$SCUPnjFH_seas3[1]), as.character(input$SCUPnjFH_seas3[2]), as.character(input$SCUPnjFH_3_bag), as.character(input$SCUPnjFH_3_len), 
                                            as.character(input$SCUPnjPR_seas3[1]), as.character(input$SCUPnjPR_seas3[2]), as.character(input$SCUPnjPR_3_bag), as.character(input$SCUPnjPR_3_len),
                                            as.character(input$SCUPnjSH_seas3[1]), as.character(input$SCUPnjSH_seas3[2]), as.character(input$SCUPnjSH_3_bag), as.character(input$SCUPnjSH_3_len)))
      }
      regulations <- regulations %>% rbind(sfNJregs, bsbNJregs, scupNJregs)
      
    }

    
    
    
    if(any("DE" == input$state)){
      if(input$SF_DE_input_type == "All Modes Combined"){
        sfDEregs <- data.frame(run_name = c(Run_Name()), 
                               input =  c("SFde_seas1_op", "SFde_seas1_cl", "SFde_1_bag", "SFde_1_len", 
                                          "SFde_seas2_op", "SFde_seas2_cl", "SFde_2_bag", "SFde_2_len", 
                                          "SFdeFH_seas3_op", "SFdeFH_seas3_cl", "SFdeFH_3_bag", "SFdeFH_3_len", 
                                          "SFdePR_seas3_op", "SFdePR_seas3_cl", "SFdePR_3_bag", "SFdePR_3_len",
                                          "SFdeSH_seas3_op", "SFdeSH_seas3_cl", "SFdeSH_3_bag", "SFdeSH_3_len"),
                               value =  c(as.character(input$SFde_seas1[1]), as.character(input$SFde_seas1[2]), as.character(input$SFde_1_bag), as.character(input$SFde_1_len), 
                                          as.character(input$SFde_seas2[1]), as.character(input$SFde_seas2[2]), as.character(input$SFde_2_bag), as.character(input$SFde_2_len),
                                          as.character(input$SFdeFH_seas3[1]), as.character(input$SFdeFH_seas3[2]), as.character(input$SFdeFH_3_bag), as.character(input$SFdeFH_3_len), 
                                          as.character(input$SFdePR_seas3[1]), as.character(input$SFdePR_seas3[2]), as.character(input$SFdePR_3_bag), as.character(input$SFdePR_3_len),
                                          as.character(input$SFdeSH_seas3[1]), as.character(input$SFdeSH_seas3[2]), as.character(input$SFdeSH_3_bag), as.character(input$SFdeSH_3_len)))
      }else{
        sfDEregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c("SFdeFH_seas1_op", "SFdeFH_seas1_cl", "SFdeFH_1_bag", "SFdeFH_1_len", 
                                           "SFdePR_seas1_op", "SFdePR_seas1_cl", "SFdePR_1_bag", "SFdePR_1_len",
                                           "SFdeSH_seas1_op", "SFdeSH_seas1_cl", "SFdeSH_1_bag", "SFdeSH_1_len",
                                           "SFdeFH_seas2_op", "SFdeFH_seas2_cl", "SFdeFH_2_bag", "SFdeFH_2_len", 
                                           "SFdePR_seas2_op", "SFdePR_seas2_cl", "SFdePR_2_bag", "SFdePR_2_len",
                                           "SFdeSH_seas2_op", "SFdeSH_seas2_cl", "SFdeSH_2_bag", "SFdeSH_2_len", 
                                           "SFdeFH_seas3_op", "SFdeFH_seas3_cl", "SFdeFH_3_bag", "SFdeFH_3_len", 
                                           "SFdePR_seas3_op", "SFdePR_seas3_cl", "SFdePR_3_bag", "SFdePR_3_len",
                                           "SFdeSH_seas3_op", "SFdeSH_seas3_cl", "SFdeSH_3_bag", "SFdeSH_3_len"),
                                value = c( as.character(input$SFdeFH_seas1[1]), as.character(input$SFdeFH_seas1[2]), as.character(input$SFdeFH_1_bag), as.character(input$SFdeFH_1_len), 
                                           as.character(input$SFdePR_seas1[1]), as.character(input$SFdePR_seas1[2]), as.character(input$SFdePR_1_bag), as.character(input$SFdePR_1_len),
                                           as.character(input$SFdeSH_seas1[1]), as.character(input$SFdeSH_seas1[2]), as.character(input$SFdeSH_1_bag), as.character(input$SFdeSH_1_len),
                                           as.character(input$SFdeFH_seas2[1]), as.character(input$SFdeFH_seas2[2]), as.character(input$SFdeFH_2_bag), as.character(input$SFdeFH_2_len), 
                                           as.character(input$SFdePR_seas2[1]), as.character(input$SFdePR_seas2[2]), as.character(input$SFdePR_2_bag), as.character(input$SFdePR_2_len),
                                           as.character(input$SFdeSH_seas2[1]), as.character(input$SFdeSH_seas2[2]), as.character(input$SFdeSH_2_bag), as.character(input$SFdeSH_2_len),
                                           as.character(input$SFdeFH_seas3[1]), as.character(input$SFdeFH_seas3[2]), as.character(input$SFdeFH_3_bag), as.character(input$SFdeFH_3_len), 
                                           as.character(input$SFdePR_seas3[1]), as.character(input$SFdePR_seas3[2]), as.character(input$SFdePR_3_bag), as.character(input$SFdePR_3_len),
                                           as.character(input$SFdeSH_seas3[1]), as.character(input$SFdeSH_seas3[2]), as.character(input$SFdeSH_3_bag), as.character(input$SFdeSH_3_len)))
      }
      
      
      if(input$BSB_DE_input_type == "All Modes Combined"){
        bsbDEregs <- data.frame(run_name = c(Run_Name()), 
                              input =  c("BSBde_seas1_op", "BSBde_seas1_cl", "BSBde_1_bag", "BSBde_1_len", 
                                         "BSBde_seas2_op", "BSBde_seas2_cl", "BSBde_2_bag", "BSBde_2_len", 
                                         "BSBdeFH_seas3_op", "BSBdeFH_seas3_cl", "BSBdeFH_3_bag", "BSBdeFH_3_len", 
                                         "BSBdePR_seas3_op", "BSBdePR_seas3_cl", "BSBdePR_3_bag", "BSBdePR_3_len",
                                         "BSBdeSH_seas3_op", "BSBdeSH_seas3_cl", "BSBdeSH_3_bag", "BSBdeSH_3_len"),
                              value =  c(as.character(input$BSBde_seas1[1]), as.character(input$BSBde_seas1[2]), as.character(input$BSBde_1_bag), as.character(input$BSBde_1_len), 
                                         as.character(input$BSBde_seas2[1]), as.character(input$BSBde_seas2[2]), as.character(input$BSBde_2_bag), as.character(input$BSBde_2_len), 
                                         as.character(input$BSBdeFH_seas3[1]), as.character(input$BSBdeFH_seas3[2]), as.character(input$BSBdeFH_3_bag), as.character(input$BSBdeFH_3_len), 
                                         as.character(input$BSBdePR_seas3[1]), as.character(input$BSBdePR_seas3[2]), as.character(input$BSBdePR_3_bag), as.character(input$BSBdePR_3_len),
                                         as.character(input$BSBdeSH_seas3[1]), as.character(input$BSBdeSH_seas3[2]), as.character(input$BSBdeSH_3_bag), as.character(input$BSBdeSH_3_len)))
      }else{
        bsbDEregs <-  data.frame(run_name = c(Run_Name()), 
                                 input =  c( "BSBdeFH_seas1_op", "BSBdeFH_seas1_cl", "BSBdeFH_1_bag", "BSBdeFH_1_len",
                                             "BSBdePR_seas1_op", "BSBdePR_seas1_cl", "BSBdePR_1_bag", "BSBdePR_1_len",
                                             "BSBdeSH_seas1_op", "BSBdeSH_seas1_cl", "BSBdeSH_1_bag", "BSBdeSH_1_len",
                                             "BSBdeFH_seas2_op", "BSBdeFH_seas2_cl", "BSBdeFH_2_bag", "BSBdeFH_2_len", 
                                             "BSBdePR_seas2_op", "BSBdePR_seas2_cl", "BSBdePR_2_bag", "BSBdePR_2_len",
                                             "BSBdeSH_seas2_op", "BSBdeSH_seas2_cl", "BSBdeSH_2_bag", "BSBdeSH_2_len",
                                             "BSBdeFH_seas3_op", "BSBdeFH_seas3_cl", "BSBdeFH_3_bag", "BSBdeFH_3_len", 
                                             "BSBdePR_seas3_op", "BSBdePR_seas3_cl", "BSBdePR_3_bag", "BSBdePR_3_len",
                                             "BSBdeSH_seas3_op", "BSBdeSH_seas3_cl", "BSBdeSH_3_bag", "BSBdeSH_3_len"),
                                 value = c( as.character(input$BSBdeFH_seas1[1]), as.character(input$BSBdeFH_seas1[2]), as.character(input$BSBdeFH_1_bag), as.character(input$BSBdeFH_1_len),
                                            as.character(input$BSBdePR_seas1[1]), as.character(input$BSBdePR_seas1[2]), as.character(input$BSBdePR_1_bag), as.character(input$BSBdePR_1_len), 
                                            as.character(input$BSBdeSH_seas1[1]), as.character(input$BSBdeSH_seas1[2]), as.character(input$BSBdeSH_1_bag), as.character(input$BSBdeSH_1_len), 
                                            as.character(input$BSBdeFH_seas2[1]), as.character(input$BSBdeFH_seas2[2]), as.character(input$BSBdeFH_2_bag), as.character(input$BSBdeFH_2_len), 
                                            as.character(input$BSBdePR_seas2[1]), as.character(input$BSBdePR_seas2[2]), as.character(input$BSBdePR_2_bag), as.character(input$BSBdePR_2_len),
                                            as.character(input$BSBdeSH_seas2[1]), as.character(input$BSBdeSH_seas2[2]), as.character(input$BSBdeSH_2_bag), as.character(input$BSBdeSH_2_len), 
                                            as.character(input$BSBdeFH_seas3[1]), as.character(input$BSBdeFH_seas3[2]), as.character(input$BSBdeFH_3_bag), as.character(input$BSBdeFH_3_len), 
                                            as.character(input$BSBdePR_seas3[1]), as.character(input$BSBdePR_seas3[2]), as.character(input$BSBdePR_3_bag), as.character(input$BSBdePR_3_len),
                                            as.character(input$BSBdeSH_seas3[1]), as.character(input$BSBdeSH_seas3[2]), as.character(input$BSBdeSH_3_bag), as.character(input$BSBdeSH_3_len)))
      }
      
      
      if(input$SCUP_DE_input_type == "All Modes Combined"){
        scupDEregs <- data.frame(run_name = c(Run_Name()), 
                                 input =  c("SCUPde_seas1_op", "SCUPde_seas1_cl", "SCUPde_1_bag", "SCUPde_1_len", 
                                            "SCUPdeFH_seas2_op", "SCUPdeFH_seas2_cl", "SCUPdeFH_2_bag", "SCUPdeFH_2_len", 
                                            "SCUPdePR_seas2_op", "SCUPdePR_seas2_cl", "SCUPdePR_2_bag", "SCUPdePR_2_len",
                                            "SCUPdeSH_seas2_op", "SCUPdeSH_seas2_cl", "SCUPdeSH_2_bag", "SCUPdeSH_2_len"),
                                 value =  c(as.character(input$SCUPde_seas1[1]), as.character(input$SCUPde_seas1[2]), as.character(input$SCUPde_1_bag), as.character(input$SCUPde_1_len), 
                                            as.character(input$SCUPdeFH_seas2[1]), as.character(input$SCUPdeFH_seas2[2]), as.character(input$SCUPdeFH_2_bag), as.character(input$SCUPdeFH_2_len), 
                                            as.character(input$SCUPdePR_seas2[1]), as.character(input$SCUPdePR_seas2[2]), as.character(input$SCUPdePR_2_bag), as.character(input$SCUPdePR_2_len),
                                            as.character(input$SCUPdeSH_seas2[1]), as.character(input$SCUPdeSH_seas2[2]), as.character(input$SCUPdeSH_2_bag), as.character(input$SCUPdeSH_2_len)))
      }else{
        scupDEregs <-  data.frame(run_name = c(Run_Name()), 
                                  input =  c( "SCUPdeFH_seas1_op", "SCUPdeFH_seas1_cl", "SCUPdeFH_1_bag", "SCUPdeFH_1_len", 
                                              "SCUPdePR_seas1_op", "SCUPdePR_seas1_cl", "SCUPdePR_1_bag", "SCUPdePR_1_len",
                                              "SCUPdeSH_seas1_op", "SCUPdeSH_seas1_cl", "SCUPdeSH_1_bag", "SCUPdeSH_1_len",
                                              "SCUPdeFH_seas2_op", "SCUPdeFH_seas2_cl", "SCUPdeFH_2_bag", "SCUPdeFH_2_len", 
                                              "SCUPdePR_seas2_op", "SCUPdePR_seas2_cl", "SCUPdePR_2_bag", "SCUPdePR_2_len",
                                              "SCUPdeSH_seas2_op", "SCUPdeSH_seas2_cl", "SCUPdeSH_2_bag", "SCUPdeSH_2_len"),
                                  value =  c(as.character(input$SCUPdeFH_seas1[1]), as.character(input$SCUPdeFH_seas1[2]), as.character(input$SCUPdeFH_1_bag), as.character(input$SCUPdeFH_1_len), 
                                             as.character(input$SCUPdePR_seas1[1]), as.character(input$SCUPdePR_seas1[2]), as.character(input$SCUPdePR_1_bag), as.character(input$SCUPdePR_1_len),
                                             as.character(input$SCUPdeSH_seas1[1]), as.character(input$SCUPdeSH_seas1[2]), as.character(input$SCUPdeSH_1_bag), as.character(input$SCUPdeSH_1_len),
                                             as.character(input$SCUPdeFH_seas2[1]), as.character(input$SCUPdeFH_seas2[2]), as.character(input$SCUPdeFH_2_bag), as.character(input$SCUPdeFH_2_len), 
                                             as.character(input$SCUPdePR_seas2[1]), as.character(input$SCUPdePR_seas2[2]), as.character(input$SCUPdePR_2_bag), as.character(input$SCUPdePR_2_len),
                                             as.character(input$SCUPdeSH_seas2[1]), as.character(input$SCUPdeSH_seas2[2]), as.character(input$SCUPdeSH_2_bag), as.character(input$SCUPdeSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfDEregs, bsbDEregs, scupDEregs)
      
    }
    
    
    if(any("MD" == input$state)){
      if(input$SF_MD_input_type == "All Momds Combined"){
        sfMDregs <- data.frame(run_name = c(Run_Name()), 
                               input =  c("SFmd_seas1_op", "SFmd_seas1_cl", "SFmd_1_bag", "SFmd_1_len", 
                                          "SFmd_seas2_op", "SFmd_seas2_cl", "SFmd_2_bag", "SFmd_2_len", 
                                          "SFmdFH_seas3_op", "SFmdFH_seas3_cl", "SFmdFH_3_bag", "SFmdFH_3_len", 
                                          "SFmdPR_seas3_op", "SFmdPR_seas3_cl", "SFmdPR_3_bag", "SFmdPR_3_len",
                                          "SFmdSH_seas3_op", "SFmdSH_seas3_cl", "SFmdSH_3_bag", "SFmdSH_3_len"),
                               value =  c(as.character(input$SFmd_seas1[1]), as.character(input$SFmd_seas1[2]), as.character(input$SFmd_1_bag), as.character(input$SFmd_1_len), 
                                          as.character(input$SFmd_seas2[1]), as.character(input$SFmd_seas2[2]), as.character(input$SFmd_2_bag), as.character(input$SFmd_2_len),
                                          as.character(input$SFmdFH_seas3[1]), as.character(input$SFmdFH_seas3[2]), as.character(input$SFmdFH_3_bag), as.character(input$SFmdFH_3_len), 
                                          as.character(input$SFmdPR_seas3[1]), as.character(input$SFmdPR_seas3[2]), as.character(input$SFmdPR_3_bag), as.character(input$SFmdPR_3_len),
                                          as.character(input$SFmdSH_seas3[1]), as.character(input$SFmdSH_seas3[2]), as.character(input$SFmdSH_3_bag), as.character(input$SFmdSH_3_len)))
      }else{
        sfMDregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c("SFmdFH_seas1_op", "SFmdFH_seas1_cl", "SFmdFH_1_bag", "SFmdFH_1_len", 
                                           "SFmdPR_seas1_op", "SFmdPR_seas1_cl", "SFmdPR_1_bag", "SFmdPR_1_len",
                                           "SFmdSH_seas1_op", "SFmdSH_seas1_cl", "SFmdSH_1_bag", "SFmdSH_1_len",
                                           "SFmdFH_seas2_op", "SFmdFH_seas2_cl", "SFmdFH_2_bag", "SFmdFH_2_len", 
                                           "SFmdPR_seas2_op", "SFmdPR_seas2_cl", "SFmdPR_2_bag", "SFmdPR_2_len",
                                           "SFmdSH_seas2_op", "SFmdSH_seas2_cl", "SFmdSH_2_bag", "SFmdSH_2_len", 
                                           "SFmdFH_seas3_op", "SFmdFH_seas3_cl", "SFmdFH_3_bag", "SFmdFH_3_len", 
                                           "SFmdPR_seas3_op", "SFmdPR_seas3_cl", "SFmdPR_3_bag", "SFmdPR_3_len",
                                           "SFmdSH_seas3_op", "SFmdSH_seas3_cl", "SFmdSH_3_bag", "SFmdSH_3_len"),
                                value = c( as.character(input$SFmdFH_seas1[1]), as.character(input$SFmdFH_seas1[2]), as.character(input$SFmdFH_1_bag), as.character(input$SFmdFH_1_len), 
                                           as.character(input$SFmdPR_seas1[1]), as.character(input$SFmdPR_seas1[2]), as.character(input$SFmdPR_1_bag), as.character(input$SFmdPR_1_len),
                                           as.character(input$SFmdSH_seas1[1]), as.character(input$SFmdSH_seas1[2]), as.character(input$SFmdSH_1_bag), as.character(input$SFmdSH_1_len),
                                           as.character(input$SFmdFH_seas2[1]), as.character(input$SFmdFH_seas2[2]), as.character(input$SFmdFH_2_bag), as.character(input$SFmdFH_2_len), 
                                           as.character(input$SFmdPR_seas2[1]), as.character(input$SFmdPR_seas2[2]), as.character(input$SFmdPR_2_bag), as.character(input$SFmdPR_2_len),
                                           as.character(input$SFmdSH_seas2[1]), as.character(input$SFmdSH_seas2[2]), as.character(input$SFmdSH_2_bag), as.character(input$SFmdSH_2_len),
                                           as.character(input$SFmdFH_seas3[1]), as.character(input$SFmdFH_seas3[2]), as.character(input$SFmdFH_3_bag), as.character(input$SFmdFH_3_len), 
                                           as.character(input$SFmdPR_seas3[1]), as.character(input$SFmdPR_seas3[2]), as.character(input$SFmdPR_3_bag), as.character(input$SFmdPR_3_len),
                                           as.character(input$SFmdSH_seas3[1]), as.character(input$SFmdSH_seas3[2]), as.character(input$SFmdSH_3_bag), as.character(input$SFmdSH_3_len)))
      }
      
      
      if(input$BSB_MD_input_type == "All Momds Combined"){
        bsbMDregs <- data.frame(run_name = c(Run_Name()), 
                              input =  c("BSBmd_seas1_op", "BSBmd_seas1_cl", "BSBmd_1_bag", "BSBmd_1_len", 
                                         "BSBmd_seas2_op", "BSBmd_seas2_cl", "BSBmd_2_bag", "BSBmd_2_len", 
                                         "BSBmdFH_seas3_op", "BSBmdFH_seas3_cl", "BSBmdFH_3_bag", "BSBmdFH_3_len", 
                                         "BSBmdPR_seas3_op", "BSBmdPR_seas3_cl", "BSBmdPR_3_bag", "BSBmdPR_3_len",
                                         "BSBmdSH_seas3_op", "BSBmdSH_seas3_cl", "BSBmdSH_3_bag", "BSBmdSH_3_len"),
                              value =  c(as.character(input$BSBmd_seas1[1]), as.character(input$BSBmd_seas1[2]), as.character(input$BSBmd_1_bag), as.character(input$BSBmd_1_len), 
                                         as.character(input$BSBmd_seas2[1]), as.character(input$BSBmd_seas2[2]), as.character(input$BSBmd_2_bag), as.character(input$BSBmd_2_len), 
                                         as.character(input$BSBmdFH_seas3[1]), as.character(input$BSBmdFH_seas3[2]), as.character(input$BSBmdFH_3_bag), as.character(input$BSBmdFH_3_len), 
                                         as.character(input$BSBmdPR_seas3[1]), as.character(input$BSBmdPR_seas3[2]), as.character(input$BSBmdPR_3_bag), as.character(input$BSBmdPR_3_len),
                                         as.character(input$BSBmdSH_seas3[1]), as.character(input$BSBmdSH_seas3[2]), as.character(input$BSBmdSH_3_bag), as.character(input$BSBmdSH_3_len)))
      }else{
        bsbMDregs <-  data.frame(run_name = c(Run_Name()), 
                                 input =  c( "BSBmdFH_seas1_op", "BSBmdFH_seas1_cl", "BSBmdFH_1_bag", "BSBmdFH_1_len",
                                             "BSBmdPR_seas1_op", "BSBmdPR_seas1_cl", "BSBmdPR_1_bag", "BSBmdPR_1_len",
                                             "BSBmdSH_seas1_op", "BSBmdSH_seas1_cl", "BSBmdSH_1_bag", "BSBmdSH_1_len",
                                             "BSBmdFH_seas2_op", "BSBmdFH_seas2_cl", "BSBmdFH_2_bag", "BSBmdFH_2_len", 
                                             "BSBmdPR_seas2_op", "BSBmdPR_seas2_cl", "BSBmdPR_2_bag", "BSBmdPR_2_len",
                                             "BSBmdSH_seas2_op", "BSBmdSH_seas2_cl", "BSBmdSH_2_bag", "BSBmdSH_2_len",
                                             "BSBmdFH_seas3_op", "BSBmdFH_seas3_cl", "BSBmdFH_3_bag", "BSBmdFH_3_len", 
                                             "BSBmdPR_seas3_op", "BSBmdPR_seas3_cl", "BSBmdPR_3_bag", "BSBmdPR_3_len",
                                             "BSBmdSH_seas3_op", "BSBmdSH_seas3_cl", "BSBmdSH_3_bag", "BSBmdSH_3_len"),
                                 value = c( as.character(input$BSBmdFH_seas1[1]), as.character(input$BSBmdFH_seas1[2]), as.character(input$BSBmdFH_1_bag), as.character(input$BSBmdFH_1_len),
                                            as.character(input$BSBmdPR_seas1[1]), as.character(input$BSBmdPR_seas1[2]), as.character(input$BSBmdPR_1_bag), as.character(input$BSBmdPR_1_len), 
                                            as.character(input$BSBmdSH_seas1[1]), as.character(input$BSBmdSH_seas1[2]), as.character(input$BSBmdSH_1_bag), as.character(input$BSBmdSH_1_len), 
                                            as.character(input$BSBmdFH_seas2[1]), as.character(input$BSBmdFH_seas2[2]), as.character(input$BSBmdFH_2_bag), as.character(input$BSBmdFH_2_len), 
                                            as.character(input$BSBmdPR_seas2[1]), as.character(input$BSBmdPR_seas2[2]), as.character(input$BSBmdPR_2_bag), as.character(input$BSBmdPR_2_len),
                                            as.character(input$BSBmdSH_seas2[1]), as.character(input$BSBmdSH_seas2[2]), as.character(input$BSBmdSH_2_bag), as.character(input$BSBmdSH_2_len), 
                                            as.character(input$BSBmdFH_seas3[1]), as.character(input$BSBmdFH_seas3[2]), as.character(input$BSBmdFH_3_bag), as.character(input$BSBmdFH_3_len), 
                                            as.character(input$BSBmdPR_seas3[1]), as.character(input$BSBmdPR_seas3[2]), as.character(input$BSBmdPR_3_bag), as.character(input$BSBmdPR_3_len),
                                            as.character(input$BSBmdSH_seas3[1]), as.character(input$BSBmdSH_seas3[2]), as.character(input$BSBmdSH_3_bag), as.character(input$BSBmdSH_3_len)))
      }
      
      
      if(input$SCUP_MD_input_type == "All Momds Combined"){
        scupMDregs <- data.frame(run_name = c(Run_Name()), 
                                 input =  c("SCUPmd_seas1_op", "SCUPmd_seas1_cl", "SCUPmd_1_bag", "SCUPmd_1_len", 
                                            "SCUPmdFH_seas2_op", "SCUPmdFH_seas2_cl", "SCUPmdFH_2_bag", "SCUPmdFH_2_len", 
                                            "SCUPmdPR_seas2_op", "SCUPmdPR_seas2_cl", "SCUPmdPR_2_bag", "SCUPmdPR_2_len",
                                            "SCUPmdSH_seas2_op", "SCUPmdSH_seas2_cl", "SCUPmdSH_2_bag", "SCUPmdSH_2_len"),
                                 value =  c(as.character(input$SCUPmd_seas1[1]), as.character(input$SCUPmd_seas1[2]), as.character(input$SCUPmd_1_bag), as.character(input$SCUPmd_1_len), 
                                            as.character(input$SCUPmdFH_seas2[1]), as.character(input$SCUPmdFH_seas2[2]), as.character(input$SCUPmdFH_2_bag), as.character(input$SCUPmdFH_2_len), 
                                            as.character(input$SCUPmdPR_seas2[1]), as.character(input$SCUPmdPR_seas2[2]), as.character(input$SCUPmdPR_2_bag), as.character(input$SCUPmdPR_2_len),
                                            as.character(input$SCUPmdSH_seas2[1]), as.character(input$SCUPmdSH_seas2[2]), as.character(input$SCUPmdSH_2_bag), as.character(input$SCUPmdSH_2_len)))
      }else{
        scupMDregs <-  data.frame(run_name = c(Run_Name()), 
                                  input =  c( "SCUPmdFH_seas1_op", "SCUPmdFH_seas1_cl", "SCUPmdFH_1_bag", "SCUPmdFH_1_len", 
                                              "SCUPmdPR_seas1_op", "SCUPmdPR_seas1_cl", "SCUPmdPR_1_bag", "SCUPmdPR_1_len",
                                              "SCUPmdSH_seas1_op", "SCUPmdSH_seas1_cl", "SCUPmdSH_1_bag", "SCUPmdSH_1_len",
                                              "SCUPmdFH_seas2_op", "SCUPmdFH_seas2_cl", "SCUPmdFH_2_bag", "SCUPmdFH_2_len", 
                                              "SCUPmdPR_seas2_op", "SCUPmdPR_seas2_cl", "SCUPmdPR_2_bag", "SCUPmdPR_2_len",
                                              "SCUPmdSH_seas2_op", "SCUPmdSH_seas2_cl", "SCUPmdSH_2_bag", "SCUPmdSH_2_len"),
                                  value =  c(as.character(input$SCUPmdFH_seas1[1]), as.character(input$SCUPmdFH_seas1[2]), as.character(input$SCUPmdFH_1_bag), as.character(input$SCUPmdFH_1_len), 
                                             as.character(input$SCUPmdPR_seas1[1]), as.character(input$SCUPmdPR_seas1[2]), as.character(input$SCUPmdPR_1_bag), as.character(input$SCUPmdPR_1_len),
                                             as.character(input$SCUPmdSH_seas1[1]), as.character(input$SCUPmdSH_seas1[2]), as.character(input$SCUPmdSH_1_bag), as.character(input$SCUPmdSH_1_len),
                                             as.character(input$SCUPmdFH_seas2[1]), as.character(input$SCUPmdFH_seas2[2]), as.character(input$SCUPmdFH_2_bag), as.character(input$SCUPmdFH_2_len), 
                                             as.character(input$SCUPmdPR_seas2[1]), as.character(input$SCUPmdPR_seas2[2]), as.character(input$SCUPmdPR_2_bag), as.character(input$SCUPmdPR_2_len),
                                             as.character(input$SCUPmdSH_seas2[1]), as.character(input$SCUPmdSH_seas2[2]), as.character(input$SCUPmdSH_2_bag), as.character(input$SCUPmdSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfMDregs, bsbMDregs, scupMDregs)
    }
    
    
    if(any("VA" == input$state)){
      if(input$SF_VA_input_type == "All Movas Combined"){
        sfVAregs <- data.frame(run_name = c(Run_Name()), 
                               input =  c("SFva_seas1_op", "SFva_seas1_cl", "SFva_1_bag", "SFva_1_len", 
                                          "SFva_seas2_op", "SFva_seas2_cl", "SFva_2_bag", "SFva_2_len", 
                                          "SFvaFH_seas3_op", "SFvaFH_seas3_cl", "SFvaFH_3_bag", "SFvaFH_3_len", 
                                          "SFvaPR_seas3_op", "SFvaPR_seas3_cl", "SFvaPR_3_bag", "SFvaPR_3_len",
                                          "SFvaSH_seas3_op", "SFvaSH_seas3_cl", "SFvaSH_3_bag", "SFvaSH_3_len"),
                               value =  c(as.character(input$SFva_seas1[1]), as.character(input$SFva_seas1[2]), as.character(input$SFva_1_bag), as.character(input$SFva_1_len), 
                                          as.character(input$SFva_seas2[1]), as.character(input$SFva_seas2[2]), as.character(input$SFva_2_bag), as.character(input$SFva_2_len),
                                          as.character(input$SFvaFH_seas3[1]), as.character(input$SFvaFH_seas3[2]), as.character(input$SFvaFH_3_bag), as.character(input$SFvaFH_3_len), 
                                          as.character(input$SFvaPR_seas3[1]), as.character(input$SFvaPR_seas3[2]), as.character(input$SFvaPR_3_bag), as.character(input$SFvaPR_3_len),
                                          as.character(input$SFvaSH_seas3[1]), as.character(input$SFvaSH_seas3[2]), as.character(input$SFvaSH_3_bag), as.character(input$SFvaSH_3_len)))
      }else{
        sfVAregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c("SFvaFH_seas1_op", "SFvaFH_seas1_cl", "SFvaFH_1_bag", "SFvaFH_1_len", 
                                           "SFvaPR_seas1_op", "SFvaPR_seas1_cl", "SFvaPR_1_bag", "SFvaPR_1_len",
                                           "SFvaSH_seas1_op", "SFvaSH_seas1_cl", "SFvaSH_1_bag", "SFvaSH_1_len",
                                           "SFvaFH_seas2_op", "SFvaFH_seas2_cl", "SFvaFH_2_bag", "SFvaFH_2_len", 
                                           "SFvaPR_seas2_op", "SFvaPR_seas2_cl", "SFvaPR_2_bag", "SFvaPR_2_len",
                                           "SFvaSH_seas2_op", "SFvaSH_seas2_cl", "SFvaSH_2_bag", "SFvaSH_2_len", 
                                           "SFvaFH_seas3_op", "SFvaFH_seas3_cl", "SFvaFH_3_bag", "SFvaFH_3_len", 
                                           "SFvaPR_seas3_op", "SFvaPR_seas3_cl", "SFvaPR_3_bag", "SFvaPR_3_len",
                                           "SFvaSH_seas3_op", "SFvaSH_seas3_cl", "SFvaSH_3_bag", "SFvaSH_3_len"),
                                value = c( as.character(input$SFvaFH_seas1[1]), as.character(input$SFvaFH_seas1[2]), as.character(input$SFvaFH_1_bag), as.character(input$SFvaFH_1_len), 
                                           as.character(input$SFvaPR_seas1[1]), as.character(input$SFvaPR_seas1[2]), as.character(input$SFvaPR_1_bag), as.character(input$SFvaPR_1_len),
                                           as.character(input$SFvaSH_seas1[1]), as.character(input$SFvaSH_seas1[2]), as.character(input$SFvaSH_1_bag), as.character(input$SFvaSH_1_len),
                                           as.character(input$SFvaFH_seas2[1]), as.character(input$SFvaFH_seas2[2]), as.character(input$SFvaFH_2_bag), as.character(input$SFvaFH_2_len), 
                                           as.character(input$SFvaPR_seas2[1]), as.character(input$SFvaPR_seas2[2]), as.character(input$SFvaPR_2_bag), as.character(input$SFvaPR_2_len),
                                           as.character(input$SFvaSH_seas2[1]), as.character(input$SFvaSH_seas2[2]), as.character(input$SFvaSH_2_bag), as.character(input$SFvaSH_2_len),
                                           as.character(input$SFvaFH_seas3[1]), as.character(input$SFvaFH_seas3[2]), as.character(input$SFvaFH_3_bag), as.character(input$SFvaFH_3_len), 
                                           as.character(input$SFvaPR_seas3[1]), as.character(input$SFvaPR_seas3[2]), as.character(input$SFvaPR_3_bag), as.character(input$SFvaPR_3_len),
                                           as.character(input$SFvaSH_seas3[1]), as.character(input$SFvaSH_seas3[2]), as.character(input$SFvaSH_3_bag), as.character(input$SFvaSH_3_len)))
      }
      
      
      if(input$BSB_VA_input_type == "All Movas Combined"){
        bsbVAregs <- data.frame(run_name = c(Run_Name()), 
                              input =  c("BSBva_seas1_op", "BSBva_seas1_cl", "BSBva_1_bag", "BSBva_1_len", 
                                         "BSBva_seas2_op", "BSBva_seas2_cl", "BSBva_2_bag", "BSBva_2_len", 
                                         "BSBvaFH_seas3_op", "BSBvaFH_seas3_cl", "BSBvaFH_3_bag", "BSBvaFH_3_len", 
                                         "BSBvaPR_seas3_op", "BSBvaPR_seas3_cl", "BSBvaPR_3_bag", "BSBvaPR_3_len",
                                         "BSBvaSH_seas3_op", "BSBvaSH_seas3_cl", "BSBvaSH_3_bag", "BSBvaSH_3_len"),
                              value =  c(as.character(input$BSBva_seas1[1]), as.character(input$BSBva_seas1[2]), as.character(input$BSBva_1_bag), as.character(input$BSBva_1_len), 
                                         as.character(input$BSBva_seas2[1]), as.character(input$BSBva_seas2[2]), as.character(input$BSBva_2_bag), as.character(input$BSBva_2_len), 
                                         as.character(input$BSBvaFH_seas3[1]), as.character(input$BSBvaFH_seas3[2]), as.character(input$BSBvaFH_3_bag), as.character(input$BSBvaFH_3_len), 
                                         as.character(input$BSBvaPR_seas3[1]), as.character(input$BSBvaPR_seas3[2]), as.character(input$BSBvaPR_3_bag), as.character(input$BSBvaPR_3_len),
                                         as.character(input$BSBvaSH_seas3[1]), as.character(input$BSBvaSH_seas3[2]), as.character(input$BSBvaSH_3_bag), as.character(input$BSBvaSH_3_len)))
      }else{
        bsbVAregs <-  data.frame(run_name = c(Run_Name()), 
                                 input =  c( "BSBvaFH_seas1_op", "BSBvaFH_seas1_cl", "BSBvaFH_1_bag", "BSBvaFH_1_len",
                                             "BSBvaPR_seas1_op", "BSBvaPR_seas1_cl", "BSBvaPR_1_bag", "BSBvaPR_1_len",
                                             "BSBvaSH_seas1_op", "BSBvaSH_seas1_cl", "BSBvaSH_1_bag", "BSBvaSH_1_len",
                                             "BSBvaFH_seas2_op", "BSBvaFH_seas2_cl", "BSBvaFH_2_bag", "BSBvaFH_2_len", 
                                             "BSBvaPR_seas2_op", "BSBvaPR_seas2_cl", "BSBvaPR_2_bag", "BSBvaPR_2_len",
                                             "BSBvaSH_seas2_op", "BSBvaSH_seas2_cl", "BSBvaSH_2_bag", "BSBvaSH_2_len",
                                             "BSBvaFH_seas3_op", "BSBvaFH_seas3_cl", "BSBvaFH_3_bag", "BSBvaFH_3_len", 
                                             "BSBvaPR_seas3_op", "BSBvaPR_seas3_cl", "BSBvaPR_3_bag", "BSBvaPR_3_len",
                                             "BSBvaSH_seas3_op", "BSBvaSH_seas3_cl", "BSBvaSH_3_bag", "BSBvaSH_3_len"),
                                 value = c( as.character(input$BSBvaFH_seas1[1]), as.character(input$BSBvaFH_seas1[2]), as.character(input$BSBvaFH_1_bag), as.character(input$BSBvaFH_1_len),
                                            as.character(input$BSBvaPR_seas1[1]), as.character(input$BSBvaPR_seas1[2]), as.character(input$BSBvaPR_1_bag), as.character(input$BSBvaPR_1_len), 
                                            as.character(input$BSBvaSH_seas1[1]), as.character(input$BSBvaSH_seas1[2]), as.character(input$BSBvaSH_1_bag), as.character(input$BSBvaSH_1_len), 
                                            as.character(input$BSBvaFH_seas2[1]), as.character(input$BSBvaFH_seas2[2]), as.character(input$BSBvaFH_2_bag), as.character(input$BSBvaFH_2_len), 
                                            as.character(input$BSBvaPR_seas2[1]), as.character(input$BSBvaPR_seas2[2]), as.character(input$BSBvaPR_2_bag), as.character(input$BSBvaPR_2_len),
                                            as.character(input$BSBvaSH_seas2[1]), as.character(input$BSBvaSH_seas2[2]), as.character(input$BSBvaSH_2_bag), as.character(input$BSBvaSH_2_len), 
                                            as.character(input$BSBvaFH_seas3[1]), as.character(input$BSBvaFH_seas3[2]), as.character(input$BSBvaFH_3_bag), as.character(input$BSBvaFH_3_len), 
                                            as.character(input$BSBvaPR_seas3[1]), as.character(input$BSBvaPR_seas3[2]), as.character(input$BSBvaPR_3_bag), as.character(input$BSBvaPR_3_len),
                                            as.character(input$BSBvaSH_seas3[1]), as.character(input$BSBvaSH_seas3[2]), as.character(input$BSBvaSH_3_bag), as.character(input$BSBvaSH_3_len)))
      }
      
      
      if(input$SCUP_VA_input_type == "All Movas Combined"){
        scupVAregs <- data.frame(run_name = c(Run_Name()), 
                                 input =  c("SCUPva_seas1_op", "SCUPva_seas1_cl", "SCUPva_1_bag", "SCUPva_1_len", 
                                            "SCUPvaFH_seas2_op", "SCUPvaFH_seas2_cl", "SCUPvaFH_2_bag", "SCUPvaFH_2_len", 
                                            "SCUPvaPR_seas2_op", "SCUPvaPR_seas2_cl", "SCUPvaPR_2_bag", "SCUPvaPR_2_len",
                                            "SCUPvaSH_seas2_op", "SCUPvaSH_seas2_cl", "SCUPvaSH_2_bag", "SCUPvaSH_2_len"),
                                 value =  c(as.character(input$SCUPva_seas1[1]), as.character(input$SCUPva_seas1[2]), as.character(input$SCUPva_1_bag), as.character(input$SCUPva_1_len), 
                                            as.character(input$SCUPvaFH_seas2[1]), as.character(input$SCUPvaFH_seas2[2]), as.character(input$SCUPvaFH_2_bag), as.character(input$SCUPvaFH_2_len), 
                                            as.character(input$SCUPvaPR_seas2[1]), as.character(input$SCUPvaPR_seas2[2]), as.character(input$SCUPvaPR_2_bag), as.character(input$SCUPvaPR_2_len),
                                            as.character(input$SCUPvaSH_seas2[1]), as.character(input$SCUPvaSH_seas2[2]), as.character(input$SCUPvaSH_2_bag), as.character(input$SCUPvaSH_2_len)))
      }else{
        scupVAregs <-  data.frame(run_name = c(Run_Name()), 
                                  input =  c( "SCUPvaFH_seas1_op", "SCUPvaFH_seas1_cl", "SCUPvaFH_1_bag", "SCUPvaFH_1_len", 
                                              "SCUPvaPR_seas1_op", "SCUPvaPR_seas1_cl", "SCUPvaPR_1_bag", "SCUPvaPR_1_len",
                                              "SCUPvaSH_seas1_op", "SCUPvaSH_seas1_cl", "SCUPvaSH_1_bag", "SCUPvaSH_1_len",
                                              "SCUPvaFH_seas2_op", "SCUPvaFH_seas2_cl", "SCUPvaFH_2_bag", "SCUPvaFH_2_len", 
                                              "SCUPvaPR_seas2_op", "SCUPvaPR_seas2_cl", "SCUPvaPR_2_bag", "SCUPvaPR_2_len",
                                              "SCUPvaSH_seas2_op", "SCUPvaSH_seas2_cl", "SCUPvaSH_2_bag", "SCUPvaSH_2_len"),
                                  value =  c(as.character(input$SCUPvaFH_seas1[1]), as.character(input$SCUPvaFH_seas1[2]), as.character(input$SCUPvaFH_1_bag), as.character(input$SCUPvaFH_1_len), 
                                             as.character(input$SCUPvaPR_seas1[1]), as.character(input$SCUPvaPR_seas1[2]), as.character(input$SCUPvaPR_1_bag), as.character(input$SCUPvaPR_1_len),
                                             as.character(input$SCUPvaSH_seas1[1]), as.character(input$SCUPvaSH_seas1[2]), as.character(input$SCUPvaSH_1_bag), as.character(input$SCUPvaSH_1_len),
                                             as.character(input$SCUPvaFH_seas2[1]), as.character(input$SCUPvaFH_seas2[2]), as.character(input$SCUPvaFH_2_bag), as.character(input$SCUPvaFH_2_len), 
                                             as.character(input$SCUPvaPR_seas2[1]), as.character(input$SCUPvaPR_seas2[2]), as.character(input$SCUPvaPR_2_bag), as.character(input$SCUPvaPR_2_len),
                                             as.character(input$SCUPvaSH_seas2[1]), as.character(input$SCUPvaSH_seas2[2]), as.character(input$SCUPvaSH_2_bag), as.character(input$SCUPvaSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfVAregs, bsbVAregs, scupVAregs)
    }
    
    
    if(any("NC" == input$state)){
      if(input$SF_NC_input_type == "All Moncs Combined"){
        sfNCregs <- data.frame(run_name = c(Run_Name()), 
                               input =  c("SFnc_seas1_op", "SFnc_seas1_cl", "SFnc_1_bag", "SFnc_1_len",  
                                          "SFncFH_seas2_op", "SFncFH_seas2_cl", "SFncFH_2_bag", "SFncFH_2_len", 
                                          "SFncPR_seas2_op", "SFncPR_seas2_cl", "SFncPR_2_bag", "SFncPR_2_len",
                                          "SFncSH_seas2_op", "SFncSH_seas2_cl", "SFncSH_2_bag", "SFncSH_2_len"),
                               value =  c(as.character(input$SFnc_seas1[1]), as.character(input$SFnc_seas1[2]), as.character(input$SFnc_1_bag), as.character(input$SFnc_1_len), 
                                          as.character(input$SFncFH_seas2[1]), as.character(input$SFncFH_seas2[2]), as.character(input$SFncFH_2_bag), as.character(input$SFncFH_2_len), 
                                          as.character(input$SFncPR_seas2[1]), as.character(input$SFncPR_seas2[2]), as.character(input$SFncPR_2_bag), as.character(input$SFncPR_2_len),
                                          as.character(input$SFncSH_seas2[1]), as.character(input$SFncSH_seas2[2]), as.character(input$SFncSH_2_bag), as.character(input$SFncSH_2_len)))
      }else{
        sfNCregs <-  data.frame(run_name = c(Run_Name()), 
                                input =  c("SFncFH_seas1_op", "SFncFH_seas1_cl", "SFncFH_1_bag", "SFncFH_1_len", 
                                           "SFncPR_seas1_op", "SFncPR_seas1_cl", "SFncPR_1_bag", "SFncPR_1_len",
                                           "SFncSH_seas1_op", "SFncSH_seas1_cl", "SFncSH_1_bag", "SFncSH_1_len",
                                           "SFncFH_seas2_op", "SFncFH_seas2_cl", "SFncFH_2_bag", "SFncFH_2_len", 
                                           "SFncPR_seas2_op", "SFncPR_seas2_cl", "SFncPR_2_bag", "SFncPR_2_len",
                                           "SFncSH_seas2_op", "SFncSH_seas2_cl", "SFncSH_2_bag", "SFncSH_2_len"),
                                value = c( as.character(input$SFncFH_seas1[1]), as.character(input$SFncFH_seas1[2]), as.character(input$SFncFH_1_bag), as.character(input$SFncFH_1_len), 
                                           as.character(input$SFncPR_seas1[1]), as.character(input$SFncPR_seas1[2]), as.character(input$SFncPR_1_bag), as.character(input$SFncPR_1_len),
                                           as.character(input$SFncSH_seas1[1]), as.character(input$SFncSH_seas1[2]), as.character(input$SFncSH_1_bag), as.character(input$SFncSH_1_len),
                                           as.character(input$SFncFH_seas2[1]), as.character(input$SFncFH_seas2[2]), as.character(input$SFncFH_2_bag), as.character(input$SFncFH_2_len), 
                                           as.character(input$SFncPR_seas2[1]), as.character(input$SFncPR_seas2[2]), as.character(input$SFncPR_2_bag), as.character(input$SFncPR_2_len),
                                           as.character(input$SFncSH_seas2[1]), as.character(input$SFncSH_seas2[2]), as.character(input$SFncSH_2_bag), as.character(input$SFncSH_2_len)))
      }
      
      
      if(input$BSB_NC_input_type == "All Moncs Combined"){
        bsbNCgs <- data.frame(run_name = c(Run_Name()), 
                              input =  c("BSBnc_seas1_op", "BSBnc_seas1_cl", "BSBnc_1_bag", "BSBnc_1_len", 
                                         "BSBnc_seas2_op", "BSBnc_seas2_cl", "BSBnc_2_bag", "BSBnc_2_len", 
                                         "BSBncFH_seas3_op", "BSBncFH_seas3_cl", "BSBncFH_3_bag", "BSBncFH_3_len", 
                                         "BSBncPR_seas3_op", "BSBncPR_seas3_cl", "BSBncPR_3_bag", "BSBncPR_3_len",
                                         "BSBncSH_seas3_op", "BSBncSH_seas3_cl", "BSBncSH_3_bag", "BSBncSH_3_len"),
                              value =  c(as.character(input$BSBnc_seas1[1]), as.character(input$BSBnc_seas1[2]), as.character(input$BSBnc_1_bag), as.character(input$BSBnc_1_len), 
                                         as.character(input$BSBnc_seas2[1]), as.character(input$BSBnc_seas2[2]), as.character(input$BSBnc_2_bag), as.character(input$BSBnc_2_len), 
                                         as.character(input$BSBncFH_seas3[1]), as.character(input$BSBncFH_seas3[2]), as.character(input$BSBncFH_3_bag), as.character(input$BSBncFH_3_len), 
                                         as.character(input$BSBncPR_seas3[1]), as.character(input$BSBncPR_seas3[2]), as.character(input$BSBncPR_3_bag), as.character(input$BSBncPR_3_len),
                                         as.character(input$BSBncSH_seas3[1]), as.character(input$BSBncSH_seas3[2]), as.character(input$BSBncSH_3_bag), as.character(input$BSBncSH_3_len)))
      }else{
        bsbNCregs <-  data.frame(run_name = c(Run_Name()), 
                                 input =  c( "BSBncFH_seas1_op", "BSBncFH_seas1_cl", "BSBncFH_1_bag", "BSBncFH_1_len",
                                             "BSBncPR_seas1_op", "BSBncPR_seas1_cl", "BSBncPR_1_bag", "BSBncPR_1_len",
                                             "BSBncSH_seas1_op", "BSBncSH_seas1_cl", "BSBncSH_1_bag", "BSBncSH_1_len",
                                             "BSBncFH_seas2_op", "BSBncFH_seas2_cl", "BSBncFH_2_bag", "BSBncFH_2_len", 
                                             "BSBncPR_seas2_op", "BSBncPR_seas2_cl", "BSBncPR_2_bag", "BSBncPR_2_len",
                                             "BSBncSH_seas2_op", "BSBncSH_seas2_cl", "BSBncSH_2_bag", "BSBncSH_2_len",
                                             "BSBncFH_seas3_op", "BSBncFH_seas3_cl", "BSBncFH_3_bag", "BSBncFH_3_len", 
                                             "BSBncPR_seas3_op", "BSBncPR_seas3_cl", "BSBncPR_3_bag", "BSBncPR_3_len",
                                             "BSBncSH_seas3_op", "BSBncSH_seas3_cl", "BSBncSH_3_bag", "BSBncSH_3_len"),
                                 value = c( as.character(input$BSBncFH_seas1[1]), as.character(input$BSBncFH_seas1[2]), as.character(input$BSBncFH_1_bag), as.character(input$BSBncFH_1_len),
                                            as.character(input$BSBncPR_seas1[1]), as.character(input$BSBncPR_seas1[2]), as.character(input$BSBncPR_1_bag), as.character(input$BSBncPR_1_len), 
                                            as.character(input$BSBncSH_seas1[1]), as.character(input$BSBncSH_seas1[2]), as.character(input$BSBncSH_1_bag), as.character(input$BSBncSH_1_len), 
                                            as.character(input$BSBncFH_seas2[1]), as.character(input$BSBncFH_seas2[2]), as.character(input$BSBncFH_2_bag), as.character(input$BSBncFH_2_len), 
                                            as.character(input$BSBncPR_seas2[1]), as.character(input$BSBncPR_seas2[2]), as.character(input$BSBncPR_2_bag), as.character(input$BSBncPR_2_len),
                                            as.character(input$BSBncSH_seas2[1]), as.character(input$BSBncSH_seas2[2]), as.character(input$BSBncSH_2_bag), as.character(input$BSBncSH_2_len), 
                                            as.character(input$BSBncFH_seas3[1]), as.character(input$BSBncFH_seas3[2]), as.character(input$BSBncFH_3_bag), as.character(input$BSBncFH_3_len), 
                                            as.character(input$BSBncPR_seas3[1]), as.character(input$BSBncPR_seas3[2]), as.character(input$BSBncPR_3_bag), as.character(input$BSBncPR_3_len),
                                            as.character(input$BSBncSH_seas3[1]), as.character(input$BSBncSH_seas3[2]), as.character(input$BSBncSH_3_bag), as.character(input$BSBncSH_3_len)))
      }
      
      
      if(input$SCUP_NC_input_type == "All Moncs Combined"){
        scupNCregs <- data.frame(run_name = c(Run_Name()), 
                                 input =  c("SCUPnc_seas1_op", "SCUPnc_seas1_cl", "SCUPnc_1_bag", "SCUPnc_1_len", 
                                            "SCUPncFH_seas2_op", "SCUPncFH_seas2_cl", "SCUPncFH_2_bag", "SCUPncFH_2_len", 
                                            "SCUPncPR_seas2_op", "SCUPncPR_seas2_cl", "SCUPncPR_2_bag", "SCUPncPR_2_len",
                                            "SCUPncSH_seas2_op", "SCUPncSH_seas2_cl", "SCUPncSH_2_bag", "SCUPncSH_2_len"),
                                 value =  c(as.character(input$SCUPnc_seas1[1]), as.character(input$SCUPnc_seas1[2]), as.character(input$SCUPnc_1_bag), as.character(input$SCUPnc_1_len), 
                                            as.character(input$SCUPncFH_seas2[1]), as.character(input$SCUPncFH_seas2[2]), as.character(input$SCUPncFH_2_bag), as.character(input$SCUPncFH_2_len), 
                                            as.character(input$SCUPncPR_seas2[1]), as.character(input$SCUPncPR_seas2[2]), as.character(input$SCUPncPR_2_bag), as.character(input$SCUPncPR_2_len),
                                            as.character(input$SCUPncSH_seas2[1]), as.character(input$SCUPncSH_seas2[2]), as.character(input$SCUPncSH_2_bag), as.character(input$SCUPncSH_2_len)))
      }else{
        scupNCregs <-  data.frame(run_name = c(Run_Name()), 
                                  input =  c( "SCUPncFH_seas1_op", "SCUPncFH_seas1_cl", "SCUPncFH_1_bag", "SCUPncFH_1_len", 
                                              "SCUPncPR_seas1_op", "SCUPncPR_seas1_cl", "SCUPncPR_1_bag", "SCUPncPR_1_len",
                                              "SCUPncSH_seas1_op", "SCUPncSH_seas1_cl", "SCUPncSH_1_bag", "SCUPncSH_1_len",
                                              "SCUPncFH_seas2_op", "SCUPncFH_seas2_cl", "SCUPncFH_2_bag", "SCUPncFH_2_len", 
                                              "SCUPncPR_seas2_op", "SCUPncPR_seas2_cl", "SCUPncPR_2_bag", "SCUPncPR_2_len",
                                              "SCUPncSH_seas2_op", "SCUPncSH_seas2_cl", "SCUPncSH_2_bag", "SCUPncSH_2_len"),
                                  value =  c(as.character(input$SCUPncFH_seas1[1]), as.character(input$SCUPncFH_seas1[2]), as.character(input$SCUPncFH_1_bag), as.character(input$SCUPncFH_1_len), 
                                             as.character(input$SCUPncPR_seas1[1]), as.character(input$SCUPncPR_seas1[2]), as.character(input$SCUPncPR_1_bag), as.character(input$SCUPncPR_1_len),
                                             as.character(input$SCUPncSH_seas1[1]), as.character(input$SCUPncSH_seas1[2]), as.character(input$SCUPncSH_1_bag), as.character(input$SCUPncSH_1_len),
                                             as.character(input$SCUPncFH_seas2[1]), as.character(input$SCUPncFH_seas2[2]), as.character(input$SCUPncFH_2_bag), as.character(input$SCUPncFH_2_len), 
                                             as.character(input$SCUPncPR_seas2[1]), as.character(input$SCUPncPR_seas2[2]), as.character(input$SCUPncPR_2_bag), as.character(input$SCUPncPR_2_len),
                                             as.character(input$SCUPncSH_seas2[1]), as.character(input$SCUPncSH_seas2[2]), as.character(input$SCUPncSH_2_bag), as.character(input$SCUPncSH_2_len)))
      }
      regulations <- regulations %>% rbind(sfNCregs, bsbNCregs, scupNCregs)
      
    }
      

    readr::write_csv(regulations, file = here::here(paste0("saved_regs/regs_", input$Run_Name, ".csv")))
    print("saved_inputs")
    
    return(regulations)
    
  })
  
  observeEvent(input$runmeplease, {
    output$message <- renderText("Regulations saved - we will run these soon be sure to change run name before clicking again.")
  })
  
 
  
  
  ##### Start of output processing #####
  #### PREDICTIONS_1 ##############################
  # predictions_1 <- eventReactive(input$runmeplease,{
  #   
  #   predictions_1 <- NULL
  #   
  #   #if(any( )) will run all selected check boxes on UI-regulations selection tab
  #   if(any("MA" == input$state)){
  #     source(here::here(paste0("recDST/model_run_MA.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("RI" == input$state)){
  #     source(here::here(paste0("recDST/model_run_RI.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("CT" == input$state)){
  #     source(here::here(paste0("recDST/model_run_CT.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("NY" == input$state)){
  #     source(here::here(paste0("recDST/model_run_NY.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("NJ" == input$state)){
  #     source(here::here(paste0("recDST/model_run_NJ.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("DE" == input$state)){
  #     source(here::here(paste0("recDST/model_run_DE.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("MD" == input$state)){
  #     source(here::here(paste0("recDST/model_run_MD.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("VA" == input$state)){
  #     source(here::here(paste0("recDST/model_run_VA.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   if(any("NC" == input$state)){
  #     source(here::here(paste0("recDST/model_run_NC.R")), local = TRUE)
  #     predictions_1 <- predictions_1 %>% rbind(predictions_out10)
  #   }
  #   
  #   predictions_1 <- predictions_1 %>% dplyr::mutate(number_weight = dplyr::recode(number_weight, "numbers" = "number"))
  #   return(predictions_1)
  # 
  # })
  
  
  
  ### Recreational Harvest Limit Values

  
  #### keep ####
  # keep <- reactive({
  #   keep_output<- predictions_1 %>% 
  #     dplyr::mutate(keep_release = paste(keep_release, param)) %>% 
  #     dplyr::filter(keep_release %in% c("keep total")) %>%
  #         dplyr::group_by(model, category, draw, state) %>%
  #         dplyr::summarise(Value = sum(value)) %>%
  #         dplyr::mutate(under_rhl = dplyr::case_when(category == "sf" & Value <= sf_rhl() ~ 1, TRUE ~ 0),
  #                       under_rhl = dplyr::case_when(category == "bsb" & Value <= bsb_rhl() ~ 1, TRUE ~ under_rhl), 
  #                       under_rhl = dplyr::case_when(category == "scup" & Value <= scup_rhl() ~ 1, TRUE ~ under_rhl)) %>%
  #         dplyr::group_by(model, category, state) %>%
  #         dplyr::summarise(under_rhl = sum(under_rhl),
  #                          Value = median(Value)) %>%
  #         tidyr::pivot_wider(names_from = c(model), values_from = c(Value, under_rhl)) %>%
  #         dplyr::mutate(Category = dplyr::recode(category, "sf" = "Summer Flounder",
  #                                                "bsb" = "BSB", 
  #                                                "scup" = "Scup")) %>%
  #         dplyr::select(state, category, Value_SQ, under_rhl_SQ, Value_Alt, under_rhl_Alt) %>%
  #         dplyr::rename(Species = category, `SQ Total Harvest (mt)` = Value_SQ, `SQ % Under RHL (Out of 100 runs)` = under_rhl_SQ,
  #                       `Alternative Total Harvest (mt)` = Value_Alt, `Atlernative % Under RHL (Out of 100 runs)` = under_rhl_Alt)
  #       
  #       return(keep_output)
  #     })
  # 
  
  # #### keep release discards ####
  # which_keep_out<- reactiveVal(TRUE)
  # 
  # keep_by_mode <- reactive({
  #   keep_by_mode<- predictions_1() %>%
  #     dplyr::mutate(keep_release = paste(keep_release, param)) %>% 
  #     dplyr::filter(keep_release %in% c("keep total", "release total", "release discmort")) %>%
  #     dplyr::group_by(state, model, category, keep_release, number_weight, draw, mode) %>%
  #     dplyr::summarise(Value = sum(as.numeric(value))) %>%
  #     dplyr::group_by(state, model, category, keep_release, number_weight, mode) %>%
  #     dplyr::summarise(Value = median(Value)) %>%
  #     tidyr::pivot_wider(names_from = c(model, number_weight), values_from = Value) %>%
  #     dplyr::mutate(perc_diff_num = ((Alt_number-SQ_number)/SQ_number) * 100,
  #                   perc_diff_wt = ((Alt_weight-SQ_weight)/SQ_weight) * 100) %>%
  #     dplyr::group_by(state, category, keep_release, mode) %>%
  #     dplyr::filter(!perc_diff_num == "NA",
  #                   !perc_diff_wt == "NA") %>%
  #     dplyr::summarise(SQ_Number = median(SQ_number), SQ_Weight = median(SQ_weight),
  #                      alt_Number = median(Alt_number), alt_Weight = median(Alt_weight),
  #                      perc_diff_num = median(perc_diff_num), perc_diff_wt = median(perc_diff_wt)) %>%
  #     dplyr::select(!c(SQ_Number, SQ_Weight)) %>%
  #     dplyr::mutate(category = dplyr::recode(category, "sf" = "Summer Flounder",
  #                                            "bsb" = "BSB", 
  #                                            "scup" = "Scup"),
  #                   keep_release = dplyr::recode(keep_release, "keep total" = "Harvest",
  #                                                     "release discmort" = "Dead Discards", 
  #                                                "release total" = "Discards"),
  #                   mode = dplyr::recode(mode, "fh" = "For Hire",
  #                                        "pr" = "Private", 
  #                                        "sh" = "Shore")) %>%
  #     dplyr::select(state, category, keep_release, mode, alt_Number, perc_diff_num, alt_Weight, perc_diff_wt) %>%
  #     dplyr::rename(Species = category, Variable = keep_release,
  #                   `Total Number of fish` = alt_Number, `% difference in number of fish` = perc_diff_num,
  #                   `Total Weight (mt)` = alt_Weight, `% difference in weight of fish` = perc_diff_wt, `Mode` = mode)
  #   return(keep_by_mode)
  # })
  # #####################
  # 
  # # #### release ####
  # # release<- reactive({
  # #   release_output<- predictions_1() %>% 
  # #     dplyr::filter(draw %in% c("Summary", "All selected"),
  # #                   Statistic %in% c("release pounds", "dead release pounds", 
  # #                                    "release numbers", "dead release numbers")) %>% 
  # #     dplyr::select(! "% under harvest target (out of 100 simulations)") %>% 
  # #     dplyr::arrange(factor(Statistic, levels = c("release pounds", "release numbers", "dead release pounds", "dead release numbers"))) %>% 
  # #     dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", scientific = FALSE),
  # #                    `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
  # #                    `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
  # #   return(release_output)
  # # })
  # 
  # #### angler welfare ####
  # welfare_by_mode<- reactive({
  #   
  #   welfare_by_mode <- predictions_1() %>%
  #     #predictions_out %>%
  #     dplyr::filter(category == "CV") %>%
  #     dplyr::group_by(state, draw, model, mode) %>%
  #     dplyr::summarise(Value = sum(as.numeric(value))) %>%
  #     tidyr::pivot_wider(names_from = model, values_from = Value) %>%
  #     dplyr::mutate(Value_diff = SQ - Alt) %>%
  #     dplyr::filter(!Value_diff == "NA") %>%
  #     dplyr::group_by(state, mode) %>%
  #     dplyr::summarise(median_cv = median(Value_diff)) %>%
  #     dplyr::rename(`Relative change in Angler Satisfaction ($)` = median_cv) %>%
  #     dplyr::mutate(mode = dplyr::recode(mode, "fh" = "For Hire",
  #                                        "pr" = "Private", 
  #                                        "sh" = "Shore")) %>% 
  #     dplyr::ungroup()
  #   
  #   return(welfare_by_mode)
  # })
  # 
  # #### estimate trips ####
  # trips_by_mode<- reactive({
  #   
  #   trips_by_mode<- predictions_1() %>%
  #     #predictions_out %>%
  #     dplyr::filter(category =="predicted trips" & model == "Alt") %>%
  #     dplyr::group_by(state, draw, mode) %>%
  #     dplyr::summarise(Value = sum(as.numeric(value))) %>%
  #     dplyr::group_by(state, mode) %>%
  #     dplyr::summarise(Value = median(Value)) %>%
  #     #dplyr::select(Value) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::rename(`Total number of Angler Trips` = Value) %>%
  #     dplyr::select(state, mode, `Total number of Angler Trips`) %>% 
  #     dplyr::mutate(mode = dplyr::recode(mode, "fh" = "For Hire",
  #                                        "pr" = "Private", 
  #                                        "sh" = "Shore"))
  #   return(trips_by_mode)
  # })
  # 
  # ## ALL DRAWS
  # #### keep ####
  # # keep_draws <- reactive({
  # #   keep_draws_output<- predictions_1() %>% 
  # #     dplyr::filter(draw != "Summary", 
  # #                   Statistic %in% c("harvest pounds", "harvest numbers")) %>% 
  # #     dplyr::arrange(factor(Statistic, levels = c("harvest pounds", "harvest numbers"))) %>% 
  # #     dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE),
  # #                    `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
  # #                    `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
  # #   return(keep_draws_output)
  # # })
  # 
  # # #### release ####
  # # release_draws<- reactive({
  # #   release_draws_output<- predictions_1() %>% 
  # #     dplyr::filter(draw != "Summary",
  # #                   Statistic %in% c("release pounds", "dead release pounds", 
  # #                                    "release numbers", "dead release numbers")) %>% 
  # #     dplyr::select(! "% under harvest target (out of 100 simulations)") %>% 
  # #     dplyr::arrange(factor(Statistic, levels = c("release pounds", "release numbers", "dead release pounds", "dead release numbers"))) %>% 
  # #     dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE),
  # #                    `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
  # #                    `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE))
  # #   return(release_draws_output)
  # # })
  # # 
  # # #### angler welfare ####
  # # welfare_draws<- reactive({
  # #   welfare_draws_output<- predictions_1() %>% 
  # #     dplyr::filter(draw != "Summary",
  # #                   Statistic %in% c("CV")) %>% 
  # #     dplyr::mutate(Statistic = dplyr::recode(Statistic, "CV" = "Change in angler satisfaction ($)")) %>% 
  # #     dplyr::rename( "Difference relative to status-quo 2024 (median)" = "% difference from status-quo outcome (median)" ) %>%
  # #     dplyr::mutate( `% difference from status-quo outcome (median)` = prettyNum(`% difference from status-quo outcome (median)`, big.mark = ",", format = "f", digits = 2, scientific = FALSE),
  # #                    `Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", scientific = FALSE), 
  # #                    `Status-quo value (median)` = prettyNum(`Status-quo value (median)`, big.mark = ",", scientific = FALSE)) %>% 
  # #     dplyr::select(! c("% under harvest target (out of 100 simulations)","Status-quo value (median)","Alternative option value" ))
  # #   return(welfare_draws_output)
  # # })
  # # 
  # # #### estimate trips ####
  # # ntrips_draws<- reactive({
  # #   ntrips_draws_output<- predictions %>% 
  # #     dplyr::filter(draw != "Summary",
  # #                   Statistic %in% c( "ntrips")) %>% 
  # #     dplyr::mutate(Statistic = dplyr::recode(Statistic, "ntrips" = "Total estimate trips")) %>% 
  # #     dplyr::mutate(`Alternative option value` = prettyNum(`Alternative option value`, big.mark = ",", format = "f", digits = 2, scientific = FALSE)) %>% 
  # #     dplyr::select(! c("% under harvest target (out of 100 simulations)","Status-quo value (median)","% difference from status-quo outcome (median)"))
  # #   return(ntrips_draws_output)
  # # })
  # #  
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # #### Regulations ####
  # regulations <- eventReactive(input$runmeplease,{
  #   
  #   dat <- NULL
  #   #### MA regs ####
  #   if(any("MA" == input$state)){  
  #     SFmaFHseason1 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFmaFH_seas1[1], "-", input$SFmaFH_seas1[2]),
  #                                   BagLimit = paste(input$SFmaFH_1_bag),
  #                                   Length = paste(input$SFmaFH_1_len))
  #     SFmaPRseason1 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFmaPR_seas1[1], "-", input$SFmaPR_seas1[2]),
  #                                   BagLimit = paste(input$SFmaPR_1_bag),
  #                                   Length = paste(input$SFmaPR_1_len))
  #     SFmaSHseason1 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFmaSH_seas1[1], "-", input$SFmaSH_seas1[2]),
  #                                   BagLimit = paste(input$SFmaSH_1_bag),
  #                                   Length = paste(input$SFmaSH_1_len))
  #     
  #     SFmaFHseason2 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFmaFH_seas2[1], "-", input$SFmaFH_seas2[2]),
  #                                   BagLimit = paste(input$SFmaFH_2_bag),
  #                                   Length = paste(input$SFmaFH_2_len))
  #     SFmaPRseason2 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFmaPR_seas2[1], "-", input$SFmaPR_seas2[2]),
  #                                   BagLimit = paste(input$SFmaPR_2_bag),
  #                                   Length = paste(input$SFmaPR_2_len))
  #     SFmaSHseason2 <- data.frame(State = c("MA"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFmaSH_seas2[1], "-", input$SFmaSH_seas2[2]),
  #                                   BagLimit = paste(input$SFmaSH_2_bag),
  #                                   Length = paste(input$SFmaSH_2_len))
  # 
  #     
  #     SFma <- rbind(SFmaFHseason1, SFmaFHseason2, SFmaPRseason1,  SFmaPRseason2, SFmaSHseason1,  SFmaSHseason2)
  #     
  #     if(input$BSB_MA_input_type == "All Modes Combined"){
  #       BSBmaseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBma_seas1[1], "-", input$BSBma_seas1[2]),
  #                                  BagLimit = paste(input$BSBma_1_bag),
  #                                  Length = paste(input$BSBma_1_len))
  #       BSBmaFHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBmaFH_seas2[1], "-", input$BSBmaFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBmaFH_2_bag),
  #                                    Length = paste(input$BSBmaFH_2_len))
  #       BSBmaPRseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBmaPR_seas2[1], "-", input$BSBmaPR_seas2[2]),
  #                                    BagLimit = paste(input$BSBmaPR_2_bag),
  #                                    Length = paste(input$BSBmaPR_2_len))
  #       BSBmaSHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBmaSH_seas2[1], "-", input$BSBmaSH_seas2[2]),
  #                                    BagLimit = paste(input$BSBmaSH_2_bag),
  #                                    Length = paste(input$BSBmaSH_2_len))
  #       
  #       BSBma<- rbind(BSBmaseason1, BSBmaFHseason2, BSBmaPRseason2, BSBmaSHseason2)
  #     }else{
  #       
  #       BSBmaFHseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBmaFH_seas1[1], "-", input$BSBmaFH_seas1[2]),
  #                                    BagLimit = paste(input$BSBmaFH_1_bag),
  #                                    Length = paste(input$BSBmaFH_1_len))
  #       BSBmaFHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBmaFH_seas2[1], "-", input$BSBmaFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBmaFH_2_bag),
  #                                    Length = paste(input$BSBmaFH_2_len))
  #       
  #       
  #       BSBmaPRseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBmaPR_seas1[1], "-", input$BSBmaPR_seas1[2]),
  #                                    BagLimit = paste(input$BSBmaPR_1_bag),
  #                                    Length = paste(input$BSBmaPR_1_len))
  #       BSBmaPRseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBmaPR_seas2[1], "-", input$BSBmaPR_seas2[2]),
  #                                    BagLimit = paste(input$BSBmaPR_2_bag),
  #                                    Length = paste(input$BSBmaPR_2_len))
  #       
  #       
  #       BSBmaSHseason1 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBmaSH_seas1[1], "-", input$BSBmaSH_seas1[2]),
  #                                    BagLimit = paste(input$BSBmaSH_1_bag),
  #                                    Length = paste(input$BSBmaSH_1_len))
  #       BSBmaSHseason2 <- data.frame(State = c("MA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBmaSH_seas2[1], "-", input$BSBmaSH_seas2[2]),
  #                                    BagLimit = paste(input$BSBmaSH_2_bag),
  #                                    Length = paste(input$BSBmaSH_2_len))
  #       
  #       BSBma<- rbind(BSBmaFHseason1, BSBmaFHseason2, BSBmaPRseason1, BSBmaPRseason2,BSBmaSHseason1, BSBmaSHseason2)
  #     }
  #     
  #     
  #     
  #     
  #     SCUPmaFHseason1 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPmaFH_seas1[1], "-", input$SCUPmaFH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPmaFH_1_bag),
  #                                   Length = paste(input$SCUPmaFH_1_len))
  #     SCUPmaPRseason1 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPmaPR_seas1[1], "-", input$SCUPmaPR_seas1[2]),
  #                                   BagLimit = paste(input$SCUPmaPR_1_bag),
  #                                   Length = paste(input$SCUPmaPR_1_len))
  #     SCUPmaSHseason1 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPmaSH_seas1[1], "-", input$SCUPmaSH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPmaSH_1_bag),
  #                                   Length = paste(input$SCUPmaSH_1_len))
  #     
  #     SCUPmaFHseason2 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPmaFH_seas2[1], "-", input$SCUPmaFH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPmaFH_2_bag),
  #                                   Length = paste(input$SCUPmaFH_2_len))
  #     SCUPmaPRseason2 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPmaPR_seas2[1], "-", input$SCUPmaPR_seas2[2]),
  #                                   BagLimit = paste(input$SCUPmaPR_2_bag),
  #                                   Length = paste(input$SCUPmaPR_2_len))
  #     SCUPmaSHseason2 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPmaSH_seas2[1], "-", input$SCUPmaSH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPmaSH_2_bag),
  #                                   Length = paste(input$SCUPmaSH_2_len))
  #     
  #     SCUPmaFHseason3 <- data.frame(State = c("MA"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPmaFH_seas3[1], "-", input$SCUPmaFH_seas3[2]),
  #                                   BagLimit = paste(input$SCUPmaFH_3_bag),
  #                                   Length = paste(input$SCUPmaFH_3_len))
  #     
  #     SCUPma <- rbind(SCUPmaFHseason1, SCUPmaFHseason2, SCUPmaFHseason3 ,SCUPmaPRseason1,  SCUPmaPRseason2,SCUPmaSHseason1,  SCUPmaSHseason2)
  #     
  #     dat <- dat %>% rbind(SFma, BSBma, SCUPma)
  #     
  #   }
  #   
  #   #### RI regs ####
  #   if(any("RI" == input$state)){  
  #     if(input$SF_RI_input_type == "All Modes Combined"){
  #       
  #       SFriseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFri_seas1[1], "-", input$SFri_seas1[2]),
  #                                 BagLimit = paste(input$SFri_1_bag),
  #                                 Length = paste(input$SFri_1_len))
  #       SFriFHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFriFH_seas2[1], "-", input$SFriFH_seas2[2]),
  #                                   BagLimit = paste(input$SFriFH_2_bag),
  #                                   Length = paste(input$SFriFH_2_len))
  #       SFriPRseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFriPR_seas2[1], "-", input$SFriPR_seas2[2]),
  #                                   BagLimit = paste(input$SFriPR_2_bag),
  #                                   Length = paste(input$SFriPR_2_len))
  #       SFriSHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFriSH_seas2[1], "-", input$SFriSH_seas2[2]),
  #                                   BagLimit = paste(input$SFriSH_2_bag),
  #                                   Length = paste(input$SFriSH_2_len))
  #       SFri <- rbind(SFriseason1, SFriFHseason2, SFriPRseason2, SFriSHseason2)
  #     } else {
  #       SFriFHseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFriFH_seas1[1], "-", input$SFriFH_seas1[2]),
  #                                   BagLimit = paste(input$SFriFH_1_bag),
  #                                   Length = paste(input$SFriFH_1_len))
  #       SFriPRseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFriPR_seas1[1], "-", input$SFriPR_seas1[2]),
  #                                   BagLimit = paste(input$SFriPR_1_bag),
  #                                   Length = paste(input$SFriPR_1_len))
  #       SFriSHseason1 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFriSH_seas1[1], "-", input$SFriSH_seas1[2]),
  #                                   BagLimit = paste(input$SFriSH_1_bag),
  #                                   Length = paste(input$SFriSH_1_len))
  #       SFriFHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFriFH_seas2[1], "-", input$SFriFH_seas2[2]),
  #                                   BagLimit = paste(input$SFriFH_2_bag),
  #                                   Length = paste(input$SFriFH_2_len))
  #       SFriPRseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFriPR_seas2[1], "-", input$SFriPR_seas2[2]),
  #                                   BagLimit = paste(input$SFriPR_2_bag),
  #                                   Length = paste(input$SFriPR_2_len))
  #       SFriSHseason2 <- data.frame(State = c("RI"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFriSH_seas2[1], "-", input$SFriSH_seas2[2]),
  #                                   BagLimit = paste(input$SFriSH_2_bag),
  #                                   Length = paste(input$SFriSH_2_len))
  #       
  #       SFri <- rbind(SFriFHseason1,SFriFHseason2, SFriPRseason1,SFriPRseason2, SFriSHseason1,SFriSHseason2)
  #     }
  #     
  #     
  #     BSBriFHseason1 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                  Season = paste(input$BSBriFH_seas1[1], "-", input$BSBriFH_seas1[2]),
  #                                  BagLimit = paste(input$BSBriFH_1_bag),
  #                                  Length = paste(input$BSBriFH_1_len))
  #     BSBriFHseason2 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                  Season = paste(input$BSBriFH_seas2[1], "-", input$BSBriFH_seas2[2]),
  #                                  BagLimit = paste(input$BSBriFH_2_bag),
  #                                  Length = paste(input$BSBriFH_2_len))
  #     BSBriFHseason3 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                  Season = paste(input$BSBriFH_seas3[1], "-", input$BSBriFH_seas3[2]),
  #                                  BagLimit = paste(input$BSBriFH_3_bag),
  #                                  Length = paste(input$BSBriFH_3_len))
  #     
  #     BSBriPRseason1 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                  Season = paste(input$BSBriPR_seas1[1], "-", input$BSBriPR_seas1[2]),
  #                                  BagLimit = paste(input$BSBriPR_1_bag),
  #                                  Length = paste(input$BSBriPR_1_len))
  #     BSBriPRseason2 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                  Season = paste(input$BSBriPR_seas2[1], "-", input$BSBriPR_seas2[2]),
  #                                  BagLimit = paste(input$BSBriPR_2_bag),
  #                                  Length = paste(input$BSBriPR_2_len))
  #     BSBriPRseason3 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                  Season = paste(input$BSBriPR_seas3[1], "-", input$BSBriPR_seas3[2]),
  #                                  BagLimit = paste(input$BSBriPR_3_bag),
  #                                  Length = paste(input$BSBriPR_3_len))
  #     
  #     BSBriSHseason1 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                  Season = paste(input$BSBriSH_seas1[1], "-", input$BSBriSH_seas1[2]),
  #                                  BagLimit = paste(input$BSBriSH_1_bag),
  #                                  Length = paste(input$BSBriSH_1_len))
  #     BSBriSHseason2 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                  Season = paste(input$BSBriSH_seas2[1], "-", input$BSBriSH_seas2[2]),
  #                                  BagLimit = paste(input$BSBriSH_2_bag),
  #                                  Length = paste(input$BSBriSH_2_len))
  #     BSBriSHseason3 <- data.frame(State = c("RI"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                  Season = paste(input$BSBriSH_seas3[1], "-", input$BSBriSH_seas3[2]),
  #                                  BagLimit = paste(input$BSBriSH_3_bag),
  #                                  Length = paste(input$BSBriSH_3_len))
  #     
  #     BSBri<- rbind(BSBriFHseason1, BSBriFHseason2, BSBriFHseason3,BSBriPRseason1, BSBriPRseason2,BSBriPRseason3,BSBriSHseason1, BSBriSHseason2, BSBriSHseason3)
  #     
  #     
  #     
  #     
  #     
  #     SCUPriFHseason1 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPriFH_seas1[1], "-", input$SCUPriFH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPriFH_1_bag),
  #                                   Length = paste(input$SCUPriFH_1_len))
  #     SCUPriPRseason1 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPriPR_seas1[1], "-", input$SCUPriPR_seas1[2]),
  #                                   BagLimit = paste(input$SCUPriPR_1_bag),
  #                                   Length = paste(input$SCUPriPR_1_len))
  #     SCUPriSHseason1 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPriSH_seas1[1], "-", input$SCUPriSH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPriSH_1_bag),
  #                                   Length = paste(input$SCUPriSH_1_len))
  #     SCUPriFHseason2 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPriFH_seas2[1], "-", input$SCUPriFH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPriFH_2_bag),
  #                                   Length = paste(input$SCUPriFH_2_len))
  #     SCUPriPRseason2 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPriPR_seas2[1], "-", input$SCUPriPR_seas2[2]),
  #                                   BagLimit = paste(input$SCUPriPR_2_bag),
  #                                   Length = paste(input$SCUPriPR_2_len))
  #     SCUPriSHseason2 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPriSH_seas2[1], "-", input$SCUPriSH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPriSH_2_bag),
  #                                   Length = paste(input$SCUPriSH_2_len))
  #     SCUPriFHseason3 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPriFH_seas3[1], "-", input$SCUPriFH_seas3[2]),
  #                                   BagLimit = paste(input$SCUPriFH_3_bag),
  #                                   Length = paste(input$SCUPriFH_3_len))
  #     SCUPriFHseason4 <- data.frame(State = c("RI"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPriFH_seas4[1], "-", input$SCUPriFH_seas4[2]),
  #                                   BagLimit = paste(input$SCUPriFH_4_bag),
  #                                   Length = paste(input$SCUPriFH_4_len))
  #     
  #     SCUPri <- rbind(SCUPriFHseason1, SCUPriFHseason2, SCUPriFHseason3, SCUPriFHseason4,  SCUPriPRseason1,  SCUPriPRseason2,SCUPriSHseason1,  SCUPriSHseason2)
  #     
  #     dat <- dat %>% rbind(SFri, BSBri, SCUPri)
  #     
  #   }
  #   
  #   #### CT regs ####
  #   if(any("CT" == input$state)){  
  #     if(input$SF_CT_input_type == "All Modes Combined"){
  #       
  #       SFctseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFct_seas1[1], "-", input$SFct_seas1[2]),
  #                                 BagLimit = paste(input$SFct_1_bag),
  #                                 Length = paste(input$SFct_1_len))
  #       SFctseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFct_seas2[1], "-", input$SFct_seas2[2]),
  #                                 BagLimit = paste(input$SFct_2_bag),
  #                                 Length = paste(input$SFct_2_len))
  #       SFctFHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFctFH_seas3[1], "-", input$SFctFH_seas3[2]),
  #                                   BagLimit = paste(input$SFctFH_3_bag),
  #                                   Length = paste(input$SFctFH_3_len))
  #       SFctPRseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFctPR_seas3[1], "-", input$SFctPR_seas3[2]),
  #                                   BagLimit = paste(input$SFctPR_3_bag),
  #                                   Length = paste(input$SFctPR_3_len))
  #       SFctSHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFctSH_seas3[1], "-", input$SFctSH_seas3[2]),
  #                                   BagLimit = paste(input$SFctSH_3_bag),
  #                                   Length = paste(input$SFctSH_3_len))
  #       SFct <- rbind(SFctseason1, SFctseason2, SFctFHseason3, SFctPRseason3, SFctSHseason3)
  #     } else {
  #       SFctFHseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFctFH_seas1[1], "-", input$SFctFH_seas1[2]),
  #                                   BagLimit = paste(input$SFctFH_1_bag),
  #                                   Length = paste(input$SFctFH_1_len))
  #       SFctPRseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFctPR_seas1[1], "-", input$SFctPR_seas1[2]),
  #                                   BagLimit = paste(input$SFctPR_1_bag),
  #                                   Length = paste(input$SFctPR_1_len))
  #       SFctSHseason1 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFctSH_seas1[1], "-", input$SFctSH_seas1[2]),
  #                                   BagLimit = paste(input$SFctSH_1_bag),
  #                                   Length = paste(input$SFctSH_1_len))
  #       SFctFHseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFctFH_seas2[1], "-", input$SFctFH_seas2[2]),
  #                                   BagLimit = paste(input$SFctFH_2_bag),
  #                                   Length = paste(input$SFctFH_2_len))
  #       SFctPRseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFctPR_seas2[1], "-", input$SFctPR_seas2[2]),
  #                                   BagLimit = paste(input$SFctPR_2_bag),
  #                                   Length = paste(input$SFctPR_2_len))
  #       SFctSHseason2 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFctSH_seas2[1], "-", input$SFctSH_seas2[2]),
  #                                   BagLimit = paste(input$SFctSH_2_bag),
  #                                   Length = paste(input$SFctSH_2_len))
  #       SFctFHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFctFH_seas3[1], "-", input$SFctFH_seas3[2]),
  #                                   BagLimit = paste(input$SFctFH_3_bag),
  #                                   Length = paste(input$SFctFH_3_len))
  #       SFctPRseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFctPR_seas3[1], "-", input$SFctPR_seas3[2]),
  #                                   BagLimit = paste(input$SFctPR_3_bag),
  #                                   Length = paste(input$SFctPR_3_len))
  #       SFctSHseason3 <- data.frame(State = c("CT"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFctSH_seas3[1], "-", input$SFctSH_seas3[2]),
  #                                   BagLimit = paste(input$SFctSH_3_bag),
  #                                   Length = paste(input$SFctSH_3_len))
  #       
  #       SFct <- rbind(SFctFHseason1,SFctFHseason2,SFctFHseason3, SFctPRseason1,SFctPRseason2,SFctPRseason3, SFctSHseason1,SFctSHseason2, SFctSHseason3)
  #     }
  #     
  #     
  #     BSBctFHseason1 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                  Season = paste(input$BSBctFH_seas1[1], "-", input$BSBctFH_seas1[2]),
  #                                  BagLimit = paste(input$BSBctFH_1_bag),
  #                                  Length = paste(input$BSBctFH_1_len))
  #     BSBctFHseason2 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                  Season = paste(input$BSBctFH_seas2[1], "-", input$BSBctFH_seas2[2]),
  #                                  BagLimit = paste(input$BSBctFH_2_bag),
  #                                  Length = paste(input$BSBctFH_2_len))
  #     BSBctFHseason3 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                  Season = paste(input$BSBctFH_seas3[1], "-", input$BSBctFH_seas3[2]),
  #                                  BagLimit = paste(input$BSBctFH_3_bag),
  #                                  Length = paste(input$BSBctFH_3_len))
  #     
  #     BSBctPRseason1 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                  Season = paste(input$BSBctPR_seas1[1], "-", input$BSBctPR_seas1[2]),
  #                                  BagLimit = paste(input$BSBctPR_1_bag),
  #                                  Length = paste(input$BSBctPR_1_len))
  #     BSBctPRseason2 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                  Season = paste(input$BSBctPR_seas2[1], "-", input$BSBctPR_seas2[2]),
  #                                  BagLimit = paste(input$BSBctPR_2_bag),
  #                                  Length = paste(input$BSBctPR_2_len))
  #     BSBctPRseason3 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                  Season = paste(input$BSBctPR_seas3[1], "-", input$BSBctPR_seas3[2]),
  #                                  BagLimit = paste(input$BSBctPR_3_bag),
  #                                  Length = paste(input$BSBctPR_3_len))
  #     
  #     BSBctSHseason1 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                  Season = paste(input$BSBctSH_seas1[1], "-", input$BSBctSH_seas1[2]),
  #                                  BagLimit = paste(input$BSBctSH_1_bag),
  #                                  Length = paste(input$BSBctSH_1_len))
  #     BSBctSHseason2 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                  Season = paste(input$BSBctSH_seas2[1], "-", input$BSBctSH_seas2[2]),
  #                                  BagLimit = paste(input$BSBctSH_2_bag),
  #                                  Length = paste(input$BSBctSH_2_len))
  #     BSBctSHseason3 <- data.frame(State = c("CT"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                  Season = paste(input$BSBctSH_seas3[1], "-", input$BSBctSH_seas3[2]),
  #                                  BagLimit = paste(input$BSBctSH_3_bag),
  #                                  Length = paste(input$BSBctSH_3_len))
  #     
  #     BSBct<- rbind(BSBctFHseason1, BSBctFHseason2, BSBctFHseason3,BSBctPRseason1, BSBctPRseason2,BSBctPRseason3,BSBctSHseason1, BSBctSHseason2, BSBctSHseason3)
  #     
  #     
  #     
  #     
  #     
  #     SCUPctFHseason1 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPctFH_seas1[1], "-", input$SCUPctFH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPctFH_1_bag),
  #                                   Length = paste(input$SCUPctFH_1_len))
  #     SCUPctPRseason1 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPctPR_seas1[1], "-", input$SCUPctPR_seas1[2]),
  #                                   BagLimit = paste(input$SCUPctPR_1_bag),
  #                                   Length = paste(input$SCUPctPR_1_len))
  #     SCUPctSHseason1 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPctSH_seas1[1], "-", input$SCUPctSH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPctSH_1_bag),
  #                                   Length = paste(input$SCUPctSH_1_len))
  #     SCUPctFHseason2 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPctFH_seas2[1], "-", input$SCUPctFH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPctFH_2_bag),
  #                                   Length = paste(input$SCUPctFH_2_len))
  #     SCUPctPRseason2 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPctPR_seas2[1], "-", input$SCUPctPR_seas2[2]),
  #                                   BagLimit = paste(input$SCUPctPR_2_bag),
  #                                   Length = paste(input$SCUPctPR_2_len))
  #     SCUPctSHseason2 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPctSH_seas2[1], "-", input$SCUPctSH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPctSH_2_bag),
  #                                   Length = paste(input$SCUPctSH_2_len))
  #     SCUPctFHseason3 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPctFH_seas3[1], "-", input$SCUPctFH_seas3[2]),
  #                                   BagLimit = paste(input$SCUPctFH_3_bag),
  #                                   Length = paste(input$SCUPctFH_3_len))
  #     SCUPctFHseason4 <- data.frame(State = c("CT"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPctFH_seas4[1], "-", input$SCUPctFH_seas4[2]),
  #                                   BagLimit = paste(input$SCUPctFH_4_bag),
  #                                   Length = paste(input$SCUPctFH_4_len))
  #     
  #     SCUPct <- rbind(SCUPctFHseason1, SCUPctFHseason2, SCUPctFHseason3, SCUPctFHseason4,  SCUPctPRseason1,  SCUPctPRseason2,SCUPctSHseason1,  SCUPctSHseason2)
  #     
  #     dat <- dat %>% rbind(SFct, BSBct, SCUPct)
  #     
  #   }
  #   
  #   #### NY Regs ####
  #   if(any("NY" == input$state)){  
  #     if(input$SF_NY_input_type == "All Modes Combined"){
  #       
  #       SFnyseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFny_seas1[1], "-", input$SFny_seas1[2]),
  #                                 BagLimit = paste(input$SFny_1_bag),
  #                                 Length = paste(input$SFny_1_len))
  #       SFnyseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFny_seas2[1], "-", input$SFny_seas2[2]),
  #                                 BagLimit = paste(input$SFny_2_bag),
  #                                 Length = paste(input$SFny_2_len))
  #       SFnyFHseason3 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFnyFH_seas3[1], "-", input$SFnyFH_seas3[2]),
  #                                   BagLimit = paste(input$SFnyFH_3_bag),
  #                                   Length = paste(input$SFnyFH_3_len))
  #       SFnyPRseason3 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFnyPR_seas3[1], "-", input$SFnyPR_seas3[2]),
  #                                   BagLimit = paste(input$SFnyPR_3_bag),
  #                                   Length = paste(input$SFnyPR_3_len))
  #       SFnySHseason3 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFnySH_seas3[1], "-", input$SFnySH_seas3[2]),
  #                                   BagLimit = paste(input$SFnySH_3_bag),
  #                                   Length = paste(input$SFnySH_3_len))
  #       SFny <- rbind(SFnyseason1, SFnyseason2, SFnyFHseason3, SFnyPRseason3, SFnySHseason3)
  #     } else {
  #       SFnyFHseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFnyFH_seas1[1], "-", input$SFnyFH_seas1[2]),
  #                                   BagLimit = paste(input$SFnyFH_1_bag),
  #                                   Length = paste(input$SFnyFH_1_len))
  #       SFnyPRseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFnyPR_seas1[1], "-", input$SFnyPR_seas1[2]),
  #                                   BagLimit = paste(input$SFnyPR_1_bag),
  #                                   Length = paste(input$SFnyPR_1_len))
  #       SFnySHseason1 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFnySH_seas1[1], "-", input$SFnySH_seas1[2]),
  #                                   BagLimit = paste(input$SFnySH_1_bag),
  #                                   Length = paste(input$SFnySH_1_len))
  #       SFnyFHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFnyFH_seas2[1], "-", input$SFnyFH_seas2[2]),
  #                                   BagLimit = paste(input$SFnyFH_2_bag),
  #                                   Length = paste(input$SFnyFH_2_len))
  #       SFnyPRseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFnyPR_seas2[1], "-", input$SFnyPR_seas2[2]),
  #                                   BagLimit = paste(input$SFnyPR_2_bag),
  #                                   Length = paste(input$SFnyPR_2_len))
  #       SFnySHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFnySH_seas2[1], "-", input$SFnySH_seas2[2]),
  #                                   BagLimit = paste(input$SFnySH_2_bag),
  #                                   Length = paste(input$SFnySH_2_len))
  #       
  #       SFnyFHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFnyFH_seas3[1], "-", input$SFnyFH_seas3[2]),
  #                                   BagLimit = paste(input$SFnyFH_3_bag),
  #                                   Length = paste(input$SFnyFH_3_len))
  #       SFnyPRseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFnyPR_seas3[1], "-", input$SFnyPR_seas3[2]),
  #                                   BagLimit = paste(input$SFnyPR_3_bag),
  #                                   Length = paste(input$SFnyPR_3_len))
  #       SFnySHseason2 <- data.frame(State = c("NY"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFnySH_seas3[1], "-", input$SFnySH_seas3[2]),
  #                                   BagLimit = paste(input$SFnySH_3_bag),
  #                                   Length = paste(input$SFnySH_3_len))
  #       
  #       SFny <- rbind(SFnyFHseason1,SFnyFHseason2, SFnyFHseason3,SFnyPRseason1,SFnyPRseason2, SFnyPRseason3,SFnySHseason1,SFnySHseason2,SFnySHseason3)
  #     }
  #     
  #     
  #     if(input$BSB_NY_input_type == "All Modes Combined"){
  #       BSBnyseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBny_seas1[1], "-", input$BSBny_seas1[2]),
  #                                  BagLimit = paste(input$BSBny_1_bag),
  #                                  Length = paste(input$BSBny_1_len))
  #       BSBnyseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBny_seas2[1], "-", input$BSBny_seas2[2]),
  #                                  BagLimit = paste(input$BSBny_2_bag),
  #                                  Length = paste(input$BSBny_2_len))
  #       BSBnyFHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnyFH_seas3[1], "-", input$BSBnyFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBnyFH_3_bag),
  #                                    Length = paste(input$BSBnyFH_3_len))
  #       BSBnyPRseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnyPR_seas3[1], "-", input$BSBnyPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBnyPR_3_bag),
  #                                    Length = paste(input$BSBnyPR_3_len))
  #       BSBnySHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnySH_seas3[1], "-", input$BSBnySH_seas3[2]),
  #                                    BagLimit = paste(input$BSBnySH_3_bag),
  #                                    Length = paste(input$BSBnySH_3_len))
  #       BSBny<- rbind(BSBnyseason1, BSBnyseason2,BSBnyFHseason3,BSBnyPRseason3,BSBnySHseason3)
  #     } else {
  #       BSBnyFHseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnyFH_seas1[1], "-", input$BSBnyFH_seas1[2]),
  #                                    BagLimit = paste(input$BSBnyFH_1_bag),
  #                                    Length = paste(input$BSBnyFH_1_len))
  #       BSBnyFHseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnyFH_seas2[1], "-", input$BSBnyFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBnyFH_2_bag),
  #                                    Length = paste(input$BSBnyFH_2_len))
  #       BSBnyFHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnyFH_seas3[1], "-", input$BSBnyFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBnyFH_3_bag),
  #                                    Length = paste(input$BSBnyFH_3_len))
  #       
  #       BSBnyPRseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnyPR_seas1[1], "-", input$BSBnyPR_seas1[2]),
  #                                    BagLimit = paste(input$BSBnyPR_1_bag),
  #                                    Length = paste(input$BSBnyPR_1_len))
  #       BSBnyPRseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnyPR_seas2[1], "-", input$BSBnyPR_seas2[2]),
  #                                    BagLimit = paste(input$BSBnyPR_2_bag),
  #                                    Length = paste(input$BSBnyPR_2_len))
  #       BSBnyPRseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnyPR_seas3[1], "-", input$BSBnyPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBnyPR_3_bag),
  #                                    Length = paste(input$BSBnyPR_3_len))
  #       
  #       BSBnySHseason1 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnySH_seas1[1], "-", input$BSBnySH_seas1[2]),
  #                                    BagLimit = paste(input$BSBnySH_1_bag),
  #                                    Length = paste(input$BSBnySH_1_len))
  #       BSBnySHseason2 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnySH_seas2[1], "-", input$BSBnySH_seas2[2]),
  #                                    BagLimit = paste(input$BSBnySH_2_bag),
  #                                    Length = paste(input$BSBnySH_2_len))
  #       BSBnySHseason3 <- data.frame(State = c("NY"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnySH_seas3[1], "-", input$BSBnySH_seas3[2]),
  #                                    BagLimit = paste(input$BSBnySH_3_bag),
  #                                    Length = paste(input$BSBnySH_3_len))
  #       
  #       BSBny<- rbind(BSBnyFHseason1, BSBnyFHseason2, BSBnyFHseason3,BSBnyPRseason1, BSBnyPRseason2,BSBnyPRseason3,BSBnySHseason1, BSBnySHseason2, BSBnySHseason3)
  #     }
  #     
  #     
  #     
  #     SCUPnyFHseason1 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPnyFH_seas1[1], "-", input$SCUPnyFH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPnyFH_1_bag),
  #                                   Length = paste(input$SCUPnyFH_1_len))
  #     SCUPnyPRseason1 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPnyPR_seas1[1], "-", input$SCUPnyPR_seas1[2]),
  #                                   BagLimit = paste(input$SCUPnyPR_1_bag),
  #                                   Length = paste(input$SCUPnyPR_1_len))
  #     SCUPnySHseason1 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPnySH_seas1[1], "-", input$SCUPnySH_seas1[2]),
  #                                   BagLimit = paste(input$SCUPnySH_1_bag),
  #                                   Length = paste(input$SCUPnySH_1_len))
  #     SCUPnyFHseason2 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPnyFH_seas2[1], "-", input$SCUPnyFH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPnyFH_2_bag),
  #                                   Length = paste(input$SCUPnyFH_2_len))
  #     SCUPnyPRseason2 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Private"),
  #                                   Season = paste(input$SCUPnyPR_seas2[1], "-", input$SCUPnyPR_seas2[2]),
  #                                   BagLimit = paste(input$SCUPnyPR_2_bag),
  #                                   Length = paste(input$SCUPnyPR_2_len))
  #     SCUPnySHseason2 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("Shore"),
  #                                   Season = paste(input$SCUPnySH_seas2[1], "-", input$SCUPnySH_seas2[2]),
  #                                   BagLimit = paste(input$SCUPnySH_2_bag),
  #                                   Length = paste(input$SCUPnySH_2_len))
  #     SCUPnyFHseason3 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPnyFH_seas3[1], "-", input$SCUPnyFH_seas3[2]),
  #                                   BagLimit = paste(input$SCUPnyFH_3_bag),
  #                                   Length = paste(input$SCUPnyFH_3_len))
  #     SCUPnyFHseason4 <- data.frame(State = c("NY"), Species = c("Scup"), Mode = c("For Hire"),
  #                                   Season = paste(input$SCUPnyFH_seas4[1], "-", input$SCUPnyFH_seas4[2]),
  #                                   BagLimit = paste(input$SCUPnyFH_4_bag),
  #                                   Length = paste(input$SCUPnyFH_4_len))
  #     
  #     SCUPny <- rbind(SCUPnyFHseason1, SCUPnyFHseason2, SCUPnyFHseason3, SCUPnyFHseason4,  SCUPnyPRseason1,  SCUPnyPRseason2,SCUPnySHseason1,  SCUPnySHseason2)
  #     
  #     dat <- dat %>% rbind(SFny, BSBny, SCUPny)
  #     
  #   }
  #    
  #   #### NJ Regs ####
  #   if(any("NJ" == input$state)){  
  #     if(input$SF_NJ_input_type == "All Modes Combined"){
  #       
  #       SFnjseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFnj_seas1[1], "-", input$SFnj_seas1[2]),
  #                                 BagLimit = paste(input$SFnj_1_bag),
  #                                 Length = paste(input$SFnj_1_len))
  #       SFnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFnjFH_seas2[1], "-", input$SFnjFH_seas2[2]),
  #                                   BagLimit = paste(input$SFnjFH_2_bag),
  #                                   Length = paste(input$SFnjFH_2_len))
  #       SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFnjPR_seas2[1], "-", input$SFnjPR_seas2[2]),
  #                                   BagLimit = paste(input$SFnjPR_2_bag),
  #                                   Length = paste(input$SFnjPR_2_len))
  #       SFnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFnjSH_seas2[1], "-", input$SFnjSH_seas2[2]),
  #                                   BagLimit = paste(input$SFnjSH_2_bag),
  #                                   Length = paste(input$SFnjSH_2_len))
  #       SFnj <- rbind(SFnjseason1, SFnjFHseason2, SFnjPRseason2, SFnjSHseason2)
  #     } else {
  #       SFnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFnjFH_seas1[1], "-", input$SFnjFH_seas1[2]),
  #                                   BagLimit = paste(input$SFnjFH_1_bag),
  #                                   Length = paste(input$SFnjFH_1_len))
  #       SFnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFnjFH_seas2[1], "-", input$SFnjFH_seas2[2]),
  #                                   BagLimit = paste(input$SFnjFH_2_bag),
  #                                   Length = paste(input$SFnjFH_2_len))
  #       SFnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFnjPR_seas1[1], "-", input$SFnjPR_seas1[2]),
  #                                   BagLimit = paste(input$SFnjPR_1_bag),
  #                                   Length = paste(input$SFnjPR_1_len))
  #       SFnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFnjPR_seas2[1], "-", input$SFnjPR_seas2[2]),
  #                                   BagLimit = paste(input$SFnjPR_2_bag),
  #                                   Length = paste(input$SFnjPR_2_len))
  #       SFnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFnjSH_seas1[1], "-", input$SFnjSH_seas1[2]),
  #                                   BagLimit = paste(input$SFnjSH_1_bag),
  #                                   Length = paste(input$SFnjSH_1_len))
  #       SFnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFnjSH_seas2[1], "-", input$SFnjSH_seas2[2]),
  #                                   BagLimit = paste(input$SFnjSH_2_bag),
  #                                   Length = paste(input$SFnjSH_2_len))
  #       
  #       SFnj <- rbind(SFnjFHseason1,SFnjFHseason2, SFnjPRseason1,SFnjPRseason2, SFnjSHseason1,SFnjSHseason2)
  #     }
  #     
  #     if(input$BSB_NJ_input_type == "All Modes Combined"){
  #       BSBnjseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBnj_seas1[1], "-", input$BSBnj_seas1[2]),
  #                                  BagLimit = paste(input$BSBnj_1_bag),
  #                                  Length = paste(input$BSBnj_1_len))
  #       BSBnjseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBnj_seas2[1], "-", input$BSBnj_seas2[2]),
  #                                  BagLimit = paste(input$BSBnj_2_bag),
  #                                  Length = paste(input$BSBnj_2_len))
  #       BSBnjseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBnj_seas3[1], "-", input$BSBnj_seas3[2]),
  #                                  BagLimit = paste(input$BSBnj_3_bag),
  #                                  Length = paste(input$BSBnj_3_len))
  #       BSBnjseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBnj_seas4[1], "-", input$BSBnj_seas4[2]),
  #                                  BagLimit = paste(input$BSBnj_4_bag),
  #                                  Length = paste(input$BSBnj_4_len))
  #       
  #       BSBnjFHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnjFH_seas5[1], "-", input$BSBnjFH_seas5[2]),
  #                                    BagLimit = paste(input$BSBnjFH_5_bag),
  #                                    Length = paste(input$BSBnjFH_5_len))
  #       BSBnjPRseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnjPR_seas5[1], "-", input$BSBnjPR_seas5[2]),
  #                                    BagLimit = paste(input$BSBnjPR_5_bag),
  #                                    Length = paste(input$BSBnjPR_5_len))
  #       BSBnjSHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnjSH_seas5[1], "-", input$BSBnjSH_seas5[2]),
  #                                    BagLimit = paste(input$BSBnjSH_5_bag),
  #                                    Length = paste(input$BSBnjSH_5_len))
  #       
  #       BSBnj<- rbind(BSBnjseason1, BSBnjseason2, BSBnjseason3, BSBnjseason4, BSBnjFHseason5, BSBnjPRseason5, BSBnjSHseason5)
  #     }else{
  #       
  #       BSBnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnjFH_seas1[1], "-", input$BSBnjFH_seas1[2]),
  #                                    BagLimit = paste(input$BSBnjFH_1_bag),
  #                                    Length = paste(input$BSBnjFH_1_len))
  #       BSBnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnjFH_seas2[1], "-", input$BSBnjFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBnjFH_2_bag),
  #                                    Length = paste(input$BSBnjFH_2_len))
  #       BSBnjFHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnjFH_seas3[1], "-", input$BSBnjFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBnjFH_3_bag),
  #                                    Length = paste(input$BSBnjFH_3_len))
  #       BSBnjFHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnjFH_seas4[1], "-", input$BSBnjFH_seas4[2]),
  #                                    BagLimit = paste(input$BSBnjFH_4_bag),
  #                                    Length = paste(input$BSBnjFH_4_len))
  #       BSBnjFHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBnjFH_seas5[1], "-", input$BSBnjFH_seas5[2]),
  #                                    BagLimit = paste(input$BSBnjFH_5_bag),
  #                                    Length = paste(input$BSBnjFH_5_len))
  #       
  #       BSBnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnjPR_seas1[1], "-", input$BSBnjPR_seas1[2]),
  #                                    BagLimit = paste(input$BSBnjPR_1_bag),
  #                                    Length = paste(input$BSBnjPR_1_len))
  #       BSBnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnjPR_seas2[1], "-", input$BSBnjPR_seas2[2]),
  #                                    BagLimit = paste(input$BSBnjPR_2_bag),
  #                                    Length = paste(input$BSBnjPR_2_len))
  #       BSBnjPRseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnjPR_seas3[1], "-", input$BSBnjPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBnjPR_3_bag),
  #                                    Length = paste(input$BSBnjPR_3_len))
  #       BSBnjPRseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnjPR_seas4[1], "-", input$BSBnjPR_seas4[2]),
  #                                    BagLimit = paste(input$BSBnjPR_4_bag),
  #                                    Length = paste(input$BSBnjPR_4_len))
  #       BSBnjPRseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBnjPR_seas5[1], "-", input$BSBnjPR_seas5[2]),
  #                                    BagLimit = paste(input$BSBnjPR_5_bag),
  #                                    Length = paste(input$BSBnjPR_5_len))
  #       
  #       BSBnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnjSH_seas1[1], "-", input$BSBnjSH_seas1[2]),
  #                                    BagLimit = paste(input$BSBnjSH_1_bag),
  #                                    Length = paste(input$BSBnjSH_1_len))
  #       BSBnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnjSH_seas2[1], "-", input$BSBnjSH_seas2[2]),
  #                                    BagLimit = paste(input$BSBnjSH_2_bag),
  #                                    Length = paste(input$BSBnjSH_2_len))
  #       BSBnjSHseason3 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnjSH_seas3[1], "-", input$BSBnjSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBnjSH_3_bag),
  #                                    Length = paste(input$BSBnjSH_3_len))
  #       BSBnjSHseason4 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnjSH_seas4[1], "-", input$BSBnjSH_seas4[2]),
  #                                    BagLimit = paste(input$BSBnjSH_4_bag),
  #                                    Length = paste(input$BSBnjSH_4_len))
  #       BSBnjSHseason5 <- data.frame(State = c("NJ"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBnjSH_seas5[1], "-", input$BSBnjSH_seas5[2]),
  #                                    BagLimit = paste(input$BSBnjSH_5_bag),
  #                                    Length = paste(input$BSBnjSH_5_len))
  #       
  #       
  #       BSBnj<- rbind(BSBnjFHseason1, BSBnjFHseason2, BSBnjFHseason3, BSBnjFHseason4, BSBnjFHseason5,
  #                     BSBnjPRseason1, BSBnjPRseason2, BSBnjPRseason3, BSBnjPRseason4, BSBnjPRseason5,
  #                     BSBnjSHseason1, BSBnjSHseason2, BSBnjSHseason3, BSBnjSHseason4, BSBnjSHseason5)
  #     }
  #     
  #     
  #     
  #     if(input$SCUP_NJ_input_type == "All Modes Combined"){
  #       SCUPnjseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("All"),
  #                                   Season = paste(input$SCUPnj_seas1[1], "-", input$SCUPnj_seas1[2]),
  #                                   BagLimit = paste(input$SCUPnj_1_bag),
  #                                   Length = paste(input$SCUPnj_1_len))
  #       
  #       SCUPnjFHseason2<- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
  #                                    Season = paste(input$SCUPnjFH_seas2[1], "-", input$SCUPnjFH_seas2[2]),
  #                                    BagLimit = paste(input$SCUPnjFH_2_bag),
  #                                    Length = paste(input$SCUPnjFH_2_len))
  #       SCUPnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPnjPR_seas2[1], "-", input$SCUPnjPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPnjPR_2_bag),
  #                                     Length = paste(input$SCUPnjPR_2_len))
  #       SCUPnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPnjSH_seas2[1], "-", input$SCUPnjSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPnjSH_2_bag),
  #                                     Length = paste(input$SCUPnjSH_2_len))
  #       
  #       SCUPnj <- rbind(SCUPnjseason1, SCUPnjFHseason2, SCUPnjPRseason2, SCUPnjSHseason2)
  #     } else {
  #       SCUPnjFHseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPnjFH_seas1[1], "-", input$SCUPnjFH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPnjFH_1_bag),
  #                                     Length = paste(input$SCUPnjFH_1_len))
  #       SCUPnjPRseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPnjPR_seas1[1], "-", input$SCUPnjPR_seas1[2]),
  #                                     BagLimit = paste(input$SCUPnjPR_1_bag),
  #                                     Length = paste(input$SCUPnjPR_1_len))
  #       SCUPnjSHseason1 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPnjSH_seas1[1], "-", input$SCUPnjSH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPnjSH_1_bag),
  #                                     Length = paste(input$SCUPnjSH_1_len))
  #       
  #       SCUPnjFHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPnjFH_seas2[1], "-", input$SCUPnjFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPnjFH_2_bag),
  #                                     Length = paste(input$SCUPnjFH_2_len))
  #       SCUPnjPRseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPnjPR_seas2[1], "-", input$SCUPnjPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPnjPR_2_bag),
  #                                     Length = paste(input$SCUPnjPR_2_len))
  #       SCUPnjSHseason2 <- data.frame(State = c("NJ"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPnjSH_seas2[1], "-", input$SCUPnjSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPnjSH_2_bag),
  #                                     Length = paste(input$SCUPnjSH_2_len))
  #       
  #       SCUPnj <- rbind(SCUPnjFHseason1, SCUPnjPRseason1, SCUPnjSHseason1, SCUPnjFHseason2, SCUPnjPRseason2, SCUPnjSHseason2)
  #     }
  #     dat <- dat %>% rbind(SFnj, BSBnj, SCUPnj)
  #   }
  #   
  #   #### DE Regs ####
  #   if(any("DE" == input$state)){  
  #     if(input$SF_DE_input_type == "All Modes Combined"){
  #       
  #       SFdeseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFde_seas1[1], "-", input$SFde_seas1[2]),
  #                                 BagLimit = paste(input$SFde_1_bag),
  #                                 Length = paste(input$SFde_1_len))
  #       SFdeseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFde_seas2[1], "-", input$SFde_seas2[2]),
  #                                 BagLimit = paste(input$SFde_2_bag),
  #                                 Length = paste(input$SFde_2_len))
  #       SFdeFHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFdeFH_seas3[1], "-", input$SFdeFH_seas3[2]),
  #                                   BagLimit = paste(input$SFdeFH_3_bag),
  #                                   Length = paste(input$SFdeFH_3_len))
  #       SFdePRseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFdePR_seas3[1], "-", input$SFdePR_seas3[2]),
  #                                   BagLimit = paste(input$SFdePR_3_bag),
  #                                   Length = paste(input$SFdePR_3_len))
  #       SFdeSHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFdeSH_seas3[1], "-", input$SFdeSH_seas3[2]),
  #                                   BagLimit = paste(input$SFdeSH_3_bag),
  #                                   Length = paste(input$SFdeSH_3_len))
  #       SFde <- rbind(SFdeseason1,SFdeseason2, SFdeFHseason3, SFdePRseason3, SFdeSHseason3)
  #     } else {
  #       SFdeFHseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFdeFH_seas1[1], "-", input$SFdeFH_seas1[2]),
  #                                   BagLimit = paste(input$SFdeFH_1_bag),
  #                                   Length = paste(input$SFdeFH_1_len))
  #       SFdePRseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFdePR_seas1[1], "-", input$SFdePR_seas1[2]),
  #                                   BagLimit = paste(input$SFdePR_1_bag),
  #                                   Length = paste(input$SFdePR_1_len))
  #       SFdeSHseason1 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFdeSH_seas1[1], "-", input$SFdeSH_seas1[2]),
  #                                   BagLimit = paste(input$SFdeSH_1_bag),
  #                                   Length = paste(input$SFdeSH_1_len))
  #       SFdeFHseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFdeFH_seas2[1], "-", input$SFdeFH_seas2[2]),
  #                                   BagLimit = paste(input$SFdeFH_2_bag),
  #                                   Length = paste(input$SFdeFH_2_len))
  #       SFdePRseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFdePR_seas2[1], "-", input$SFdePR_seas2[2]),
  #                                   BagLimit = paste(input$SFdePR_2_bag),
  #                                   Length = paste(input$SFdePR_2_len))
  #       SFdeSHseason2 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFdeSH_seas2[1], "-", input$SFdeSH_seas2[2]),
  #                                   BagLimit = paste(input$SFdeSH_2_bag),
  #                                   Length = paste(input$SFdeSH_2_len))
  #       SFdeFHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFdeFH_seas3[1], "-", input$SFdeFH_seas3[2]),
  #                                   BagLimit = paste(input$SFdeFH_3_bag),
  #                                   Length = paste(input$SFdeFH_3_len))
  #       SFdePRseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFdePR_seas3[1], "-", input$SFdePR_seas3[2]),
  #                                   BagLimit = paste(input$SFdePR_3_bag),
  #                                   Length = paste(input$SFdePR_3_len))
  #       SFdeSHseason3 <- data.frame(State = c("DE"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFdeSH_seas3[1], "-", input$SFdeSH_seas3[2]),
  #                                   BagLimit = paste(input$SFdeSH_3_bag),
  #                                   Length = paste(input$SFdeSH_3_len))
  #       
  #       SFde <- rbind(SFdeFHseason1,SFdeFHseason2,SFdeFHseason3, SFdePRseason1,SFdePRseason2, SFdePRseason3, SFdeSHseason1,SFdeSHseason2, SFdeSHseason3)
  #     }
  #     
  #     
  #     if(input$BSB_DE_input_type == "All Modes Combined"){
  #       BSBdeseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBde_seas1[1], "-", input$BSBde_seas1[2]),
  #                                  BagLimit = paste(input$BSBde_1_bag),
  #                                  Length = paste(input$BSBde_1_len))
  #       BSBdeseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBde_seas2[1], "-", input$BSBde_seas2[2]),
  #                                  BagLimit = paste(input$BSBde_2_bag),
  #                                  Length = paste(input$BSBde_2_len))
  #       BSBdeFHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBdeFH_seas3[1], "-", input$BSBdeFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBdeFH_3_bag),
  #                                    Length = paste(input$BSBdeFH_3_len))
  #       BSBdePRseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBdePR_seas3[1], "-", input$BSBdePR_seas3[2]),
  #                                    BagLimit = paste(input$BSBdePR_3_bag),
  #                                    Length = paste(input$BSBdePR_3_len))
  #       BSBdeSHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBdeSH_seas3[1], "-", input$BSBdeSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBdeSH_3_bag),
  #                                    Length = paste(input$BSBdeSH_3_len))
  #       BSBde<- rbind(BSBdeseason1, BSBdeseason2,BSBdeFHseason3,BSBdePRseason3,BSBdeSHseason3)
  #     } else {
  #       
  #       BSBdeFHseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBdeFH_seas1[1], "-", input$BSBdeFH_seas1[2]),
  #                                    BagLimit = paste(input$BSBdeFH_1_bag),
  #                                    Length = paste(input$BSBdeFH_1_len))
  #       BSBdeFHseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBdeFH_seas2[1], "-", input$BSBdeFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBdeFH_2_bag),
  #                                    Length = paste(input$BSBdeFH_2_len))
  #       BSBdeFHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBdeFH_seas3[1], "-", input$BSBdeFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBdeFH_3_bag),
  #                                    Length = paste(input$BSBdeFH_3_len))
  #       
  #       BSBdePRseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBdePR_seas1[1], "-", input$BSBdePR_seas1[2]),
  #                                    BagLimit = paste(input$BSBdePR_1_bag),
  #                                    Length = paste(input$BSBdePR_1_len))
  #       BSBdePRseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBdePR_seas2[1], "-", input$BSBdePR_seas2[2]),
  #                                    BagLimit = paste(input$BSBdePR_2_bag),
  #                                    Length = paste(input$BSBdePR_2_len))
  #       BSBdePRseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBdePR_seas3[1], "-", input$BSBdePR_seas3[2]),
  #                                    BagLimit = paste(input$BSBdePR_3_bag),
  #                                    Length = paste(input$BSBdePR_3_len))
  #       
  #       BSBdeSHseason1 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBdeSH_seas1[1], "-", input$BSBdeSH_seas1[2]),
  #                                    BagLimit = paste(input$BSBdeSH_1_bag),
  #                                    Length = paste(input$BSBdeSH_1_len))
  #       BSBdeSHseason2 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBdeSH_seas2[1], "-", input$BSBdeSH_seas2[2]),
  #                                    BagLimit = paste(input$BSBdeSH_2_bag),
  #                                    Length = paste(input$BSBdeSH_2_len))
  #       BSBdeSHseason3 <- data.frame(State = c("DE"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBdeSH_seas3[1], "-", input$BSBdeSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBdeSH_3_bag),
  #                                    Length = paste(input$BSBdeSH_3_len))
  #       
  #       BSBde<- rbind(BSBdeFHseason1, BSBdeFHseason2, BSBdeFHseason3,
  #                     BSBdePRseason1, BSBdePRseason2, BSBdePRseason3,
  #                     BSBdeSHseason1, BSBdeSHseason2, BSBdeSHseason3)
  #       
  #     }
  #     
  #     
  #     
  #     if(input$SCUP_DE_input_type == "All Modes Combined"){
  #       SCUPdeseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("All"),
  #                                   Season = paste(input$SCUPde_seas1[1], "-", input$SCUPde_seas1[2]),
  #                                   BagLimit = paste(input$SCUPde_1_bag),
  #                                   Length = paste(input$SCUPde_1_len))
  #       SCUPdeFHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPdeFH_seas2[1], "-", input$SCUPdeFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPdeFH_2_bag),
  #                                     Length = paste(input$SCUPdeFH_2_len))
  #       SCUPdePRseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPdePR_seas2[1], "-", input$SCUPdePR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPdePR_2_bag),
  #                                     Length = paste(input$SCUPdePR_2_len))
  #       SCUPdeSHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPdeSH_seas2[1], "-", input$SCUPdeSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPdeSH_2_bag),
  #                                     Length = paste(input$SCUPdeSH_2_len))
  #       SCUPde <- rbind(SCUPdeseason1, SCUPdeFHseason2, SCUPdePRseason2, SCUPdeSHseason2)
  #     } else {
  #       SCUPdeFHseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPdeFH_seas1[1], "-", input$SCUPdeFH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPdeFH_1_bag),
  #                                     Length = paste(input$SCUPdeFH_1_len))
  #       SCUPdePRseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPdePR_seas1[1], "-", input$SCUPdePR_seas1[2]),
  #                                     BagLimit = paste(input$SCUPdePR_1_bag),
  #                                     Length = paste(input$SCUPdePR_1_len))
  #       SCUPdeSHseason1 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPdeSH_seas1[1], "-", input$SCUPdeSH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPdeSH_1_bag),
  #                                     Length = paste(input$SCUPdeSH_1_len))
  #       SCUPdeFHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPdeFH_seas2[1], "-", input$SCUPdeFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPdeFH_2_bag),
  #                                     Length = paste(input$SCUPdeFH_2_len))
  #       SCUPdePRseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPdePR_seas2[1], "-", input$SCUPdePR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPdePR_2_bag),
  #                                     Length = paste(input$SCUPdePR_2_len))
  #       SCUPdeSHseason2 <- data.frame(State = c("DE"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPdeSH_seas2[1], "-", input$SCUPdeSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPdeSH_2_bag),
  #                                     Length = paste(input$SCUPdeSH_2_len))
  #       
  #       SCUPde <- rbind(SCUPdeFHseason1, SCUPdeFHseason2, SCUPdePRseason1,  SCUPdePRseason2,SCUPdeSHseason1,  SCUPdeSHseason2)
  #       
  #     }
  #     dat <- dat %>% rbind(SFde, BSBde, SCUPde)
  #     
  #   }
  #   
  #   #### MD Regs ####
  #   if(any("MD" == input$state)){  
  #     if(input$SF_MD_input_type == "All Modes Combined"){
  #       
  #       SFmdseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFmd_seas1[1], "-", input$SFmd_seas1[2]),
  #                                 BagLimit = paste(input$SFmd_1_bag),
  #                                 Length = paste(input$SFmd_1_len))
  #       SFmdseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFmd_seas2[1], "-", input$SFmd_seas2[2]),
  #                                 BagLimit = paste(input$SFmd_2_bag),
  #                                 Length = paste(input$SFmd_2_len))
  #       
  #       SFmdFHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFmdFH_seas3[1], "-", input$SFmdFH_seas3[2]),
  #                                   BagLimit = paste(input$SFmdFH_3_bag),
  #                                   Length = paste(input$SFmdFH_3_len))
  #       SFmdPRseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFmdPR_seas3[1], "-", input$SFmdPR_seas3[2]),
  #                                   BagLimit = paste(input$SFmdPR_3_bag),
  #                                   Length = paste(input$SFmdPR_3_len))
  #       SFmdSHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFmdSH_seas3[1], "-", input$SFmdSH_seas3[2]),
  #                                   BagLimit = paste(input$SFmdSH_3_bag),
  #                                   Length = paste(input$SFmdSH_3_len))
  #       SFmd <- rbind(SFmdseason1,SFmdseason2, SFmdFHseason3, SFmdPRseason3, SFmdSHseason3)
  #     } else {
  #       SFmdFHseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFmdFH_seas1[1], "-", input$SFmdFH_seas1[2]),
  #                                   BagLimit = paste(input$SFmdFH_1_bag),
  #                                   Length = paste(input$SFmdFH_1_len))
  #       SFmdPRseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFmdPR_seas1[1], "-", input$SFmdPR_seas1[2]),
  #                                   BagLimit = paste(input$SFmdPR_1_bag),
  #                                   Length = paste(input$SFmdPR_1_len))
  #       SFmdSHseason1 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFmdSH_seas1[1], "-", input$SFmdSH_seas1[2]),
  #                                   BagLimit = paste(input$SFmdSH_1_bag),
  #                                   Length = paste(input$SFmdSH_1_len))
  #       SFmdFHseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFmdFH_seas2[1], "-", input$SFmdFH_seas2[2]),
  #                                   BagLimit = paste(input$SFmdFH_2_bag),
  #                                   Length = paste(input$SFmdFH_2_len))
  #       SFmdPRseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFmdPR_seas2[1], "-", input$SFmdPR_seas2[2]),
  #                                   BagLimit = paste(input$SFmdPR_2_bag),
  #                                   Length = paste(input$SFmdPR_2_len))
  #       SFmdSHseason2 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFmdSH_seas2[1], "-", input$SFmdSH_seas2[2]),
  #                                   BagLimit = paste(input$SFmdSH_2_bag),
  #                                   Length = paste(input$SFmdSH_2_len))
  #       SFmdFHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFmdFH_seas3[1], "-", input$SFmdFH_seas3[2]),
  #                                   BagLimit = paste(input$SFmdFH_3_bag),
  #                                   Length = paste(input$SFmdFH_3_len))
  #       SFmdPRseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFmdPR_seas3[1], "-", input$SFmdPR_seas3[2]),
  #                                   BagLimit = paste(input$SFmdPR_3_bag),
  #                                   Length = paste(input$SFmdPR_3_len))
  #       SFmdSHseason3 <- data.frame(State = c("MD"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFmdSH_seas3[1], "-", input$SFmdSH_seas3[2]),
  #                                   BagLimit = paste(input$SFmdSH_3_bag),
  #                                   Length = paste(input$SFmdSH_3_len))
  #       
  #       SFmd <- rbind(SFmdFHseason1,SFmdFHseason2,SFmdFHseason3, SFmdPRseason1,SFmdPRseason2, SFmdPRseason3, SFmdSHseason1,SFmdSHseason2,SFmdSHseason3)
  #     }
  #     
  #     if(input$BSB_MD_input_type == "All Modes Combined"){
  #       BSBmdseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBmd_seas1[1], "-", input$BSBmd_seas1[2]),
  #                                  BagLimit = paste(input$BSBmd_1_bag),
  #                                  Length = paste(input$BSBmd_1_len))
  #       BSBmdseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBmd_seas2[1], "-", input$BSBmd_seas2[2]),
  #                                  BagLimit = paste(input$BSBmd_2_bag),
  #                                  Length = paste(input$BSBmd_2_len))
  #       BSBmdFHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBmdFH_seas3[1], "-", input$BSBmdFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBmdFH_3_bag),
  #                                    Length = paste(input$BSBmdFH_3_len))
  #       BSBmdPRseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBmdPR_seas3[1], "-", input$BSBmdPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBmdPR_3_bag),
  #                                    Length = paste(input$BSBmdPR_3_len))
  #       BSBmdSHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBmdSH_seas3[1], "-", input$BSBmdSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBmdSH_3_bag),
  #                                    Length = paste(input$BSBmdSH_3_len))
  #       BSBmd<- rbind(BSBmdseason1, BSBmdseason2,BSBmdFHseason3,BSBmdPRseason3,BSBmdSHseason3)
  #     } else {
  #       BSBmdFHseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBmdFH_seas1[1], "-", input$BSBmdFH_seas1[2]),
  #                                    BagLimit = paste(input$BSBmdFH_1_bag),
  #                                    Length = paste(input$BSBmdFH_1_len))
  #       BSBmdFHseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBmdFH_seas2[1], "-", input$BSBmdFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBmdFH_2_bag),
  #                                    Length = paste(input$BSBmdFH_2_len))
  #       BSBmdFHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBmdFH_seas3[1], "-", input$BSBmdFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBmdFH_3_bag),
  #                                    Length = paste(input$BSBmdFH_3_len))
  #       
  #       BSBmdPRseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBmdPR_seas1[1], "-", input$BSBmdPR_seas1[2]),
  #                                    BagLimit = paste(input$BSBmdPR_1_bag),
  #                                    Length = paste(input$BSBmdPR_1_len))
  #       BSBmdPRseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBmdPR_seas2[1], "-", input$BSBmdPR_seas2[2]),
  #                                    BagLimit = paste(input$BSBmdPR_2_bag),
  #                                    Length = paste(input$BSBmdPR_2_len))
  #       BSBmdPRseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBmdPR_seas3[1], "-", input$BSBmdPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBmdPR_3_bag),
  #                                    Length = paste(input$BSBmdPR_3_len))
  #       
  #       BSBmdSHseason1 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBmdSH_seas1[1], "-", input$BSBmdSH_seas1[2]),
  #                                    BagLimit = paste(input$BSBmdSH_1_bag),
  #                                    Length = paste(input$BSBmdSH_1_len))
  #       BSBmdSHseason2 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBmdSH_seas2[1], "-", input$BSBmdSH_seas2[2]),
  #                                    BagLimit = paste(input$BSBmdSH_2_bag),
  #                                    Length = paste(input$BSBmdSH_2_len))
  #       BSBmdSHseason3 <- data.frame(State = c("MD"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBmdSH_seas3[1], "-", input$BSBmdSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBmdSH_3_bag),
  #                                    Length = paste(input$BSBmdSH_3_len))
  #       
  #       BSBmd<- rbind(BSBmdFHseason1, BSBmdFHseason2, BSBmdFHseason3,BSBmdPRseason1, BSBmdPRseason2,BSBmdPRseason3,BSBmdSHseason1, BSBmdSHseason2, BSBmdSHseason3)
  #     }
  #     
  #     
  #     
  #     if(input$SCUP_MD_input_type == "All Modes Combined"){
  #       SCUPmdseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("All"),
  #                                   Season = paste(input$SCUPmd_seas1[1], "-", input$SCUPmd_seas1[2]),
  #                                   BagLimit = paste(input$SCUPmd_1_bag),
  #                                   Length = paste(input$SCUPmd_1_len))
  #       SCUPmdFHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPmdFH_seas2[1], "-", input$SCUPmdFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPmdFH_2_bag),
  #                                     Length = paste(input$SCUPmdFH_2_len))
  #       SCUPmdPRseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPmdPR_seas2[1], "-", input$SCUPmdPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPmdPR_2_bag),
  #                                     Length = paste(input$SCUPmdPR_2_len))
  #       SCUPmdSHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPmdSH_seas2[1], "-", input$SCUPmdSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPmdSH_2_bag),
  #                                     Length = paste(input$SCUPmdSH_2_len))
  #       SCUPmd <- rbind(SCUPmdseason1, SCUPmdFHseason2, SCUPmdPRseason2, SCUPmdSHseason2)
  #     } else {
  #       SCUPmdFHseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPmdFH_seas1[1], "-", input$SCUPmdFH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPmdFH_1_bag),
  #                                     Length = paste(input$SCUPmdFH_1_len))
  #       SCUPmdPRseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPmdPR_seas1[1], "-", input$SCUPmdPR_seas1[2]),
  #                                     BagLimit = paste(input$SCUPmdPR_1_bag),
  #                                     Length = paste(input$SCUPmdPR_1_len))
  #       SCUPmdSHseason1 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPmdSH_seas1[1], "-", input$SCUPmdSH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPmdSH_1_bag),
  #                                     Length = paste(input$SCUPmdSH_1_len))
  #       SCUPmdFHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPmdFH_seas2[1], "-", input$SCUPmdFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPmdFH_2_bag),
  #                                     Length = paste(input$SCUPmdFH_2_len))
  #       SCUPmdPRseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPmdPR_seas2[1], "-", input$SCUPmdPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPmdPR_2_bag),
  #                                     Length = paste(input$SCUPmdPR_2_len))
  #       SCUPmdSHseason2 <- data.frame(State = c("MD"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPmdSH_seas2[1], "-", input$SCUPmdSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPmdSH_2_bag),
  #                                     Length = paste(input$SCUPmdSH_2_len))
  #       
  #       SCUPmd <- rbind(SCUPmdFHseason1, SCUPmdFHseason2, SCUPmdPRseason1,  SCUPmdPRseason2,SCUPmdSHseason1,  SCUPmdSHseason2)
  #     }
  #     dat <- dat %>% rbind(SFmd, BSBmd, SCUPmd)
  #     
  #   }
  #   
  #   #### VA Regs ####
  #   if(any("VA" == input$state)){  
  #     if(input$SF_VA_input_type == "All Modes Combined"){
  #       
  #       SFvaseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFva_seas1[1], "-", input$SFva_seas1[2]),
  #                                 BagLimit = paste(input$SFva_1_bag),
  #                                 Length = paste(input$SFva_1_len))
  #       SFvaseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFva_seas2[1], "-", input$SFva_seas2[2]),
  #                                 BagLimit = paste(input$SFva_2_bag),
  #                                 Length = paste(input$SFva_2_len))
  #       SFvaFHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFvaFH_seas3[1], "-", input$SFvaFH_seas3[2]),
  #                                   BagLimit = paste(input$SFvaFH_3_bag),
  #                                   Length = paste(input$SFvaFH_3_len))
  #       SFvaPRseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFvaPR_seas3[1], "-", input$SFvaPR_seas3[2]),
  #                                   BagLimit = paste(input$SFvaPR_3_bag),
  #                                   Length = paste(input$SFvaPR_3_len))
  #       SFvaSHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFvaSH_seas3[1], "-", input$SFvaSH_seas3[2]),
  #                                   BagLimit = paste(input$SFvaSH_3_bag),
  #                                   Length = paste(input$SFvaSH_3_len))
  #       SFva <- rbind(SFvaseason1,SFvaseason2, SFvaFHseason3, SFvaPRseason3, SFvaSHseason3)
  #     } else {
  #       SFvaFHseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFvaFH_seas1[1], "-", input$SFvaFH_seas1[2]),
  #                                   BagLimit = paste(input$SFvaFH_1_bag),
  #                                   Length = paste(input$SFvaFH_1_len))
  #       SFvaPRseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFvaPR_seas1[1], "-", input$SFvaPR_seas1[2]),
  #                                   BagLimit = paste(input$SFvaPR_1_bag),
  #                                   Length = paste(input$SFvaPR_1_len))
  #       SFvaSHseason1 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFvaSH_seas1[1], "-", input$SFvaSH_seas1[2]),
  #                                   BagLimit = paste(input$SFvaSH_1_bag),
  #                                   Length = paste(input$SFvaSH_1_len))
  #       SFvaFHseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFvaFH_seas2[1], "-", input$SFvaFH_seas2[2]),
  #                                   BagLimit = paste(input$SFvaFH_2_bag),
  #                                   Length = paste(input$SFvaFH_2_len))
  #       SFvaPRseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFvaPR_seas2[1], "-", input$SFvaPR_seas2[2]),
  #                                   BagLimit = paste(input$SFvaPR_2_bag),
  #                                   Length = paste(input$SFvaPR_2_len))
  #       SFvaSHseason2 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFvaSH_seas2[1], "-", input$SFvaSH_seas2[2]),
  #                                   BagLimit = paste(input$SFvaSH_2_bag),
  #                                   Length = paste(input$SFvaSH_2_len))
  #       SFvaFHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFvaFH_seas3[1], "-", input$SFvaFH_seas3[2]),
  #                                   BagLimit = paste(input$SFvaFH_3_bag),
  #                                   Length = paste(input$SFvaFH_3_len))
  #       SFvaPRseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFvaPR_seas3[1], "-", input$SFvaPR_seas3[2]),
  #                                   BagLimit = paste(input$SFvaPR_3_bag),
  #                                   Length = paste(input$SFvaPR_3_len))
  #       SFvaSHseason3 <- data.frame(State = c("VA"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFvaSH_seas3[1], "-", input$SFvaSH_seas3[2]),
  #                                   BagLimit = paste(input$SFvaSH_3_bag),
  #                                   Length = paste(input$SFvaSH_3_len))
  #       
  #       SFva <- rbind(SFvaFHseason1,SFvaFHseason2,SFvaFHseason3, SFvaPRseason1,SFvaPRseason2, SFvaPRseason3, SFvaSHseason1,SFvaSHseason2, SFvaSHseason3)
  #     }
  #     
  #     if(input$BSB_VA_input_type == "All Modes Combined"){
  #       BSBvaseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBva_seas1[1], "-", input$BSBva_seas1[2]),
  #                                  BagLimit = paste(input$BSBva_1_bag),
  #                                  Length = paste(input$BSBva_1_len))
  #       BSBvaseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBva_seas2[1], "-", input$BSBva_seas2[2]),
  #                                  BagLimit = paste(input$BSBva_2_bag),
  #                                  Length = paste(input$BSBva_2_len))
  #       BSBvaFHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBvaFH_seas3[1], "-", input$BSBvaFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBvaFH_3_bag),
  #                                    Length = paste(input$BSBvaFH_3_len))
  #       BSBvaPRseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBvaPR_seas3[1], "-", input$BSBvaPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBvaPR_3_bag),
  #                                    Length = paste(input$BSBvaPR_3_len))
  #       BSBvaSHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBvaSH_seas3[1], "-", input$BSBvaSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBvaSH_3_bag),
  #                                    Length = paste(input$BSBvaSH_3_len))
  #       BSBva<- rbind(BSBvaseason1, BSBvaseason2,BSBvaFHseason3,BSBvaPRseason3,BSBvaSHseason3)
  #     } else {
  #       BSBvaFHseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBvaFH_seas1[1], "-", input$BSBvaFH_seas1[2]),
  #                                    BagLimit = paste(input$BSBvaFH_1_bag),
  #                                    Length = paste(input$BSBvaFH_1_len))
  #       BSBvaFHseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBvaFH_seas2[1], "-", input$BSBvaFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBvaFH_2_bag),
  #                                    Length = paste(input$BSBvaFH_2_len))
  #       BSBvaFHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBvaFH_seas3[1], "-", input$BSBvaFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBvaFH_3_bag),
  #                                    Length = paste(input$BSBvaFH_3_len))
  #       
  #       BSBvaPRseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBvaPR_seas1[1], "-", input$BSBvaPR_seas1[2]),
  #                                    BagLimit = paste(input$BSBvaPR_1_bag),
  #                                    Length = paste(input$BSBvaPR_1_len))
  #       BSBvaPRseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBvaPR_seas2[1], "-", input$BSBvaPR_seas2[2]),
  #                                    BagLimit = paste(input$BSBvaPR_2_bag),
  #                                    Length = paste(input$BSBvaPR_2_len))
  #       BSBvaPRseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBvaPR_seas3[1], "-", input$BSBvaPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBvaPR_3_bag),
  #                                    Length = paste(input$BSBvaPR_3_len))
  #       
  #       BSBvaSHseason1 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBvaSH_seas1[1], "-", input$BSBvaSH_seas1[2]),
  #                                    BagLimit = paste(input$BSBvaSH_1_bag),
  #                                    Length = paste(input$BSBvaSH_1_len))
  #       BSBvaSHseason2 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBvaSH_seas2[1], "-", input$BSBvaSH_seas2[2]),
  #                                    BagLimit = paste(input$BSBvaSH_2_bag),
  #                                    Length = paste(input$BSBvaSH_2_len))
  #       BSBvaSHseason3 <- data.frame(State = c("VA"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBvaSH_seas3[1], "-", input$BSBvaSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBvaSH_3_bag),
  #                                    Length = paste(input$BSBvaSH_3_len))
  #       
  #       BSBva<- rbind(BSBvaFHseason1, BSBvaFHseason2, BSBvaFHseason3,BSBvaPRseason1, BSBvaPRseason2,
  #                     BSBvaPRseason3,BSBvaSHseason1, BSBvaSHseason2, BSBvaSHseason3)
  #     }
  #     
  #     
  #     
  #     if(input$SCUP_VA_input_type == "All Modes Combined"){
  #       SCUPvaseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("All"),
  #                                   Season = paste(input$SCUPva_seas1[1], "-", input$SCUPva_seas1[2]),
  #                                   BagLimit = paste(input$SCUPva_1_bag),
  #                                   Length = paste(input$SCUPva_1_len))
  #       SCUPvaFHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPvaFH_seas2[1], "-", input$SCUPvaFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPvaFH_2_bag),
  #                                     Length = paste(input$SCUPvaFH_2_len))
  #       SCUPvaPRseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPvaPR_seas2[1], "-", input$SCUPvaPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPvaPR_2_bag),
  #                                     Length = paste(input$SCUPvaPR_2_len))
  #       SCUPvaSHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPvaSH_seas2[1], "-", input$SCUPvaSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPvaSH_2_bag),
  #                                     Length = paste(input$SCUPvaSH_2_len))
  #       SCUPva <- rbind(SCUPvaseason1, SCUPvaFHseason2, SCUPvaPRseason2, SCUPvaSHseason2)
  #     } else {
  #       SCUPvaFHseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPvaFH_seas1[1], "-", input$SCUPvaFH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPvaFH_1_bag),
  #                                     Length = paste(input$SCUPvaFH_1_len))
  #       SCUPvaPRseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPvaPR_seas1[1], "-", input$SCUPvaPR_seas1[2]),
  #                                     BagLimit = paste(input$SCUPvaPR_1_bag),
  #                                     Length = paste(input$SCUPvaPR_1_len))
  #       SCUPvaSHseason1 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPvaSH_seas1[1], "-", input$SCUPvaSH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPvaSH_1_bag),
  #                                     Length = paste(input$SCUPvaSH_1_len))
  #       SCUPvaFHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPvaFH_seas2[1], "-", input$SCUPvaFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPvaFH_2_bag),
  #                                     Length = paste(input$SCUPvaFH_2_len))
  #       SCUPvaPRseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPvaPR_seas2[1], "-", input$SCUPvaPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPvaPR_2_bag),
  #                                     Length = paste(input$SCUPvaPR_2_len))
  #       SCUPvaSHseason2 <- data.frame(State = c("VA"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPvaSH_seas2[1], "-", input$SCUPvaSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPvaSH_2_bag),
  #                                     Length = paste(input$SCUPvaSH_2_len))
  #       
  #       SCUPva <- rbind(SCUPvaFHseason1, SCUPvaFHseason2, SCUPvaPRseason1,  SCUPvaPRseason2,SCUPvaSHseason1,  SCUPvaSHseason2)
  #     }
  #     dat <- dat %>% rbind(SFva, BSBva, SCUPva)
  #     
  #   }
  #   
  #   #### NC Regs ####
  #   if(any("NC" == input$state)){  
  #     if(input$SF_NC_input_type == "All Modes Combined"){
  #       
  #       SFncseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("All"),
  #                                 Season = paste(input$SFnc_seas1[1], "-", input$SFnc_seas1[2]),
  #                                 BagLimit = paste(input$SFnc_1_bag),
  #                                 Length = paste(input$SFnc_1_len))
  #       SFncFHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFncFH_seas2[1], "-", input$SFncFH_seas2[2]),
  #                                   BagLimit = paste(input$SFncFH_2_bag),
  #                                   Length = paste(input$SFncFH_2_len))
  #       SFncPRseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFncPR_seas2[1], "-", input$SFncPR_seas2[2]),
  #                                   BagLimit = paste(input$SFncPR_2_bag),
  #                                   Length = paste(input$SFncPR_2_len))
  #       SFncSHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFncSH_seas2[1], "-", input$SFncSH_seas2[2]),
  #                                   BagLimit = paste(input$SFncSH_2_bag),
  #                                   Length = paste(input$SFncSH_2_len))
  #       SFnc <- rbind(SFncseason1, SFncFHseason2, SFncPRseason2, SFncSHseason2)
  #     } else {
  #       SFncFHseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFncFH_seas1[1], "-", input$SFncFH_seas1[2]),
  #                                   BagLimit = paste(input$SFncFH_1_bag),
  #                                   Length = paste(input$SFncFH_1_len))
  #       SFncPRseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFncPR_seas1[1], "-", input$SFncPR_seas1[2]),
  #                                   BagLimit = paste(input$SFncPR_1_bag),
  #                                   Length = paste(input$SFncPR_1_len))
  #       SFncSHseason1 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFncSH_seas1[1], "-", input$SFncSH_seas1[2]),
  #                                   BagLimit = paste(input$SFncSH_1_bag),
  #                                   Length = paste(input$SFncSH_1_len))
  #       SFncFHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("For Hire"),
  #                                   Season = paste(input$SFncFH_seas2[1], "-", input$SFncFH_seas2[2]),
  #                                   BagLimit = paste(input$SFncFH_2_bag),
  #                                   Length = paste(input$SFncFH_2_len))
  #       SFncPRseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Private"),
  #                                   Season = paste(input$SFncPR_seas2[1], "-", input$SFncPR_seas2[2]),
  #                                   BagLimit = paste(input$SFncPR_2_bag),
  #                                   Length = paste(input$SFncPR_2_len))
  #       SFncSHseason2 <- data.frame(State = c("NC"), Species = c("Summer Flounder"), Mode = c("Shore"),
  #                                   Season = paste(input$SFncSH_seas2[1], "-", input$SFncSH_seas2[2]),
  #                                   BagLimit = paste(input$SFncSH_2_bag),
  #                                   Length = paste(input$SFncSH_2_len))
  #       
  #       SFnc <- rbind(SFncFHseason1,SFncFHseason2, SFncPRseason1,SFncPRseason2, SFncSHseason1,SFncSHseason2)
  #     }
  #     
  #     if(input$BSB_NC_input_type == "All Modes Combined"){
  #       BSBncseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBnc_seas1[1], "-", input$BSBnc_seas1[2]),
  #                                  BagLimit = paste(input$BSBnc_1_bag),
  #                                  Length = paste(input$BSBnc_1_len))
  #       BSBncseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("All"),
  #                                  Season = paste(input$BSBnc_seas2[1], "-", input$BSBnc_seas2[2]),
  #                                  BagLimit = paste(input$BSBnc_2_bag),
  #                                  Length = paste(input$BSBnc_2_len))
  #       BSBncFHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBncFH_seas3[1], "-", input$BSBncFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBncFH_3_bag),
  #                                    Length = paste(input$BSBncFH_3_len))
  #       BSBncPRseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBncPR_seas3[1], "-", input$BSBncPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBncPR_3_bag),
  #                                    Length = paste(input$BSBncPR_3_len))
  #       BSBncSHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBncSH_seas3[1], "-", input$BSBncSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBncSH_3_bag),
  #                                    Length = paste(input$BSBncSH_3_len))
  #       BSBnc<- rbind(BSBncseason1, BSBncseason2,BSBncFHseason3,BSBncPRseason3,BSBncSHseason3)
  #     } else {
  #       BSBncFHseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBncFH_seas1[1], "-", input$BSBncFH_seas1[2]),
  #                                    BagLimit = paste(input$BSBncFH_1_bag),
  #                                    Length = paste(input$BSBncFH_1_len))
  #       BSBncFHseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBncFH_seas2[1], "-", input$BSBncFH_seas2[2]),
  #                                    BagLimit = paste(input$BSBncFH_2_bag),
  #                                    Length = paste(input$BSBncFH_2_len))
  #       BSBncFHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("For Hire"),
  #                                    Season = paste(input$BSBncFH_seas3[1], "-", input$BSBncFH_seas3[2]),
  #                                    BagLimit = paste(input$BSBncFH_3_bag),
  #                                    Length = paste(input$BSBncFH_3_len))
  #       
  #       BSBncPRseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBncPR_seas1[1], "-", input$BSBncPR_seas1[2]),
  #                                    BagLimit = paste(input$BSBncPR_1_bag),
  #                                    Length = paste(input$BSBncPR_1_len))
  #       BSBncPRseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBncPR_seas2[1], "-", input$BSBncPR_seas2[2]),
  #                                    BagLimit = paste(input$BSBncPR_2_bag),
  #                                    Length = paste(input$BSBncPR_2_len))
  #       BSBncPRseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Private"),
  #                                    Season = paste(input$BSBncPR_seas3[1], "-", input$BSBncPR_seas3[2]),
  #                                    BagLimit = paste(input$BSBncPR_3_bag),
  #                                    Length = paste(input$BSBncPR_3_len))
  #       
  #       BSBncSHseason1 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBncSH_seas1[1], "-", input$BSBncSH_seas1[2]),
  #                                    BagLimit = paste(input$BSBncSH_1_bag),
  #                                    Length = paste(input$BSBncSH_1_len))
  #       BSBncSHseason2 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBncSH_seas2[1], "-", input$BSBncSH_seas2[2]),
  #                                    BagLimit = paste(input$BSBncSH_2_bag),
  #                                    Length = paste(input$BSBncSH_2_len))
  #       BSBncSHseason3 <- data.frame(State = c("NC"), Species = c("Black Sea Bass"), Mode = c("Shore"),
  #                                    Season = paste(input$BSBncSH_seas3[1], "-", input$BSBncSH_seas3[2]),
  #                                    BagLimit = paste(input$BSBncSH_3_bag),
  #                                    Length = paste(input$BSBncSH_3_len))
  #       
  #       BSBnc<- rbind(BSBncFHseason1, BSBncFHseason2, BSBncFHseason3,BSBncPRseason1, BSBncPRseason2,
  #                     BSBncPRseason3,BSBncSHseason1, BSBncSHseason2, BSBncSHseason3)
  #     }
  #     
  #     
  #     
  #     if(input$SCUP_NC_input_type == "All Modes Combined"){
  #       SCUPncseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("All"),
  #                                   Season = paste(input$SCUPnc_seas1[1], "-", input$SCUPnc_seas1[2]),
  #                                   BagLimit = paste(input$SCUPnc_1_bag),
  #                                   Length = paste(input$SCUPnc_1_len))
  #       SCUPncFHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPncFH_seas2[1], "-", input$SCUPncFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPncFH_2_bag),
  #                                     Length = paste(input$SCUPncFH_2_len))
  #       SCUPncPRseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPncPR_seas2[1], "-", input$SCUPncPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPncPR_2_bag),
  #                                     Length = paste(input$SCUPncPR_2_len))
  #       SCUPncSHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPncSH_seas2[1], "-", input$SCUPncSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPncSH_2_bag),
  #                                     Length = paste(input$SCUPncSH_2_len))
  #       SCUPnc <- rbind(SCUPncseason1, SCUPncFHseason2, SCUPncPRseason2, SCUPncSHseason2)
  #     } else {
  #       SCUPncFHseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPncFH_seas1[1], "-", input$SCUPncFH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPvFH_1_bag),
  #                                     Length = paste(input$SCUPncFH_1_len))
  #       SCUPncPRseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPncPR_seas1[1], "-", input$SCUPncPR_seas1[2]),
  #                                     BagLimit = paste(input$SCUPncPR_1_bag),
  #                                     Length = paste(input$SCUPncPR_1_len))
  #       SCUPncSHseason1 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPncSH_seas1[1], "-", input$SCUPncSH_seas1[2]),
  #                                     BagLimit = paste(input$SCUPncSH_1_bag),
  #                                     Length = paste(input$SCUPncSH_1_len))
  #       SCUPncFHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("For Hire"),
  #                                     Season = paste(input$SCUPncFH_seas2[1], "-", input$SCUPncFH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPncFH_2_bag),
  #                                     Length = paste(input$SCUPncFH_2_len))
  #       SCUPncPRseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Private"),
  #                                     Season = paste(input$SCUPncPR_seas2[1], "-", input$SCUPncPR_seas2[2]),
  #                                     BagLimit = paste(input$SCUPncPR_2_bag),
  #                                     Length = paste(input$SCUPncPR_2_len))
  #       SCUPncSHseason2 <- data.frame(State = c("NC"), Species = c("Scup"), Mode = c("Shore"),
  #                                     Season = paste(input$SCUPncSH_seas2[1], "-", input$SCUPncSH_seas2[2]),
  #                                     BagLimit = paste(input$SCUPncSH_2_bag),
  #                                     Length = paste(input$SCUPncSH_2_len))
  #       
  #       SCUPnc <- rbind(SCUPncFHseason1, SCUPncFHseason2, SCUPncPRseason1,  SCUPncPRseason2,SCUPncSHseason1,  SCUPncSHseason2)
  #     }
  #     dat <- dat %>% rbind(SFnc, BSBnc, SCUPnc)
  #     
  #   }
  #   
  #   
  #   regs_output<- dat %>% 
  #     dplyr::filter(!BagLimit == "0",
  #                   !BagLimit == "0 , 0") %>%
  #     dplyr::mutate(Season = stringr::str_remove(Season, pattern = "2024-"),
  #                   Season = stringr::str_remove(Season, pattern = "2024-"))
  #   return(regs_output)
  # })
  # 
  # 
  # #### Output Tables #### 
  # output$keep_tableout<- renderTable({
  #   keep_by_mode()
  # })
  # 
  # output$welfare_tableout<- renderTable({
  #   welfare_by_mode()
  # })
  # 
  # output$ntrips_tableout<- renderTable({
  #   trips_by_mode()
  # })
  # 
  # output$regtableout <- renderTable({
  #   regulations()
  # })
  # 
  # outputs <- function(){
  #   fnames <- list.files(path=here::here("output/"),pattern = "*.csv",full.names = T)
  #   
  #   fnames2<- as.data.frame(fnames) %>%
  #     tidyr::separate(fnames, into = c("a", "b"), sep = "_") %>%
  #     dplyr::mutate(b = ifelse(stringr::str_detect(b, "202501"),  "NA", b),
  #                   c=c(1:nrow(.)),
  #                   run_name = dplyr::case_when(b != "NA" ~ b, TRUE ~ as.character(c))) %>%
  #     dplyr::select(run_name)
  #   
  #   # fnames2<- as.data.frame(fnames) %>%
  #   #   tidyr::separate(fnames, into = c("a", "b", "c"), sep = "_") %>%
  #   #   dplyr::mutate(c = ifelse(stringr::str_detect(c, "202501"),  "NA", c),
  #   #                 d=c(1:nrow(.)),
  #   #                 run_name = dplyr::case_when(c != "NA" ~ c, TRUE ~ as.character(c))) %>%
  #   #   dplyr::select(run_name)
  #   
  #   df <- fnames %>%
  #     purrr::map_df(~data.table::fread(.,stringsAsFactors=F,check.names=T,strip.white=T))
  #   
  #   
  #   df2<- df %>% dplyr::mutate(run_number = as.character(rep(fnames2$run_name, each = 80)))
  #   return(df2)
  # }
  # 
  # 
  # #### Save Raw Data
  # observeEvent(input$runmeplease, {
  #   dat<- predictions_1()
  #   
  #   dat_out<- dat
  #   Run_Name = Run_Name()
  #   readr::write_csv(dat_out, file = here::here(paste0("output/output_", Run_Name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"),  ".csv")))
  #   
  # })
  # 
  # 
  # 
  # 
  # #### Download Button ####
  # output$downloadData <- downloadHandler(
  #   filename = function(){"RecDSToutput.xlsx"},
  #   content = function(filename) {
  #     
  #     df_list <- list(Regulations=regulations(), Harvest=keep_by_mode(), 
  #                     Change_Angler_Satisfaction = welfare_by_mode(), Estimated_Trips = trips_by_mode()) #, # Harvest_Draws = keep_draws(), 
  #                     #Releases_Draws = release_draws(), Change_Angler_Satisfaction_Draw = welfare_draws(), Estimated_Trips_Draws = ntrips_draws())
  #     openxlsx::write.xlsx(x = df_list , file = filename, row.names = FALSE)
  #   })
  

  
}

shiny::shinyApp(ui = ui, server = server)
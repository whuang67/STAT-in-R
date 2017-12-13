#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(DT)
library(shiny)
library(ggplot2)
library(reshape2)
library(randomForest)

Logged = FALSE
my_username <- c("group5")
my_password <- c("group5")



ui1 <- fluidPage(
  wellPanel(
    h3("Authorized Users Only (Password Required)"),
    # p("Maintainer: Wenke Huang (whuang67@illinois.edu)"),
    textInput("userName", "Username", value = "group5"),
    passwordInput("passwd", "Password"),
    # br(),
    actionButton("Login", "Log in")
    
  ))



# Define UI for application that draws a histogram
ui2 <- shinyUI(
  pageWithSidebar(
  
  headerPanel("PM 2.5 Project"),
  
  sidebarPanel(
    conditionalPanel(
      condition = "input.conditionalPanels == 1",
      selectInput("variable1", label = "Variable:",
                  choices = c("season", "month", "day", "weekday", "hour", "cbwd",
                              
                              "DEWP", "HUMI", "PRES", "TEMP", "Iws", "precipitation", "Iprec"),
                  selected = "season"),
      
      # radioButtons("subset", label = "Dataset:",
      #              choices = c("Fullset", "After 2011", "After 2012"),
      #              selected = "After 2011"),
      
      sliderInput("ylimit",
                  label = "PM 2.5 Limitation",
                  min = 100,
                  max = 750,
                  value = 750)
    ),
    
    conditionalPanel(
      condition = "input.conditionalPanels == 2",
      fileInput("file", label = "Uploade here!",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      radioButtons("sep", "Delimiter",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t",
                               Space = " "),
                   selected = ","),
      radioButtons("station", "Station", 
                   choices = c("US_Post", "Jingan", "Xuhui", "All"),
                   selected = "All")
      # downloadButton("download", label = "Predictions here!")
      ),
    
    conditionalPanel(
      condition = "input.conditionalPanels == 3",
      fluidRow(
        column(6, 
               numericInput("yesterdayPM",
                            label = "Yesterday PM 2.5",
                            value = 40, min=0, max=750),
               numericInput("DEWP", 
                            label = "Dew Point",
                            value = 12, min=-25, max=30),
               numericInput("HUMI", 
                            label = "Humidity",
                            value = 72, min=10, max=100),
               numericInput("PRES", 
                            label = "Pressure",
                            value = 1016, min=950, max=1120),
               numericInput("TEMP", 
                            label = "Temperature",
                            value = 18, min=-10, max=45),
               numericInput("Iws",
                            label = "Cumulated Wind Speed",
                            value = 20, min=0, max=1200)),
        column(6,
               selectInput("cbwd",
                           label = "Combined Wind Direction",
                           choices = c("cv", "SW", "SE", "NW", "NE")),
               selectInput("month",
                           label = "Month",
                           choices = c("January", "February", "March", "April", "May", "June",
                                       "July", "August", "September", "October", 
                                       "November", "December")),
               selectInput("weekday",
                           label = "Weekday",
                           choices = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                                       "Thursday", "Friday", "Saturday")),
               selectInput("hour",
                           label = "Hour",
                           choices = as.factor(seq(0, 23)))))
      
    )
  ),
  
  mainPanel(
    tabsetPanel(tabPanel("Data Visualization",
                         plotOutput("plot2"),
                         value = 1),
                tabPanel("Prediction",
                         tableOutput("predict2"),
                         value = 2),
                tabPanel("Calculator",
                         dataTableOutput("predict"),
                         value = 3),
                type = "pills",
                id = "conditionalPanels"))
  
  )
)

ui <- htmlOutput("page")

dat <- read.csv("ShanghaiPM20100101_20151231.csv")
# dat <- dat[complete.cases(dat), ]
dat$Date <- as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$weekday <- factor(weekdays(dat$Date), levels = c("Sunday", "Monday", "Tuesday",
                                                     "Wednesday", "Thursday", "Friday",
                                                     "Saturday", "Sunday"))
# load("Model.RData", envir=.GlobalEnv)
model_rf_Jingan <- readRDS("model_rf_Jingan.rds")
model_rf_Xuhui <- readRDS("model_rf_Xuhui.rds")
model_rf_US <- readRDS("model_rf_US.rds")

# Define server logic required to draw a histogram
server <- function(input, output){
  
  USER <- reactiveValues(Logged = Logged)
  observe({
    if(USER$Logged == FALSE){
      if(!is.null(input$Login)){
        if(input$Login > 0){
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(my_username == Username)
          Id.password <- which(my_password == Password)
          
          if(length(Id.username) > 0 & length(Id.password) >0){
            if(Id.username == Id.password){
              USER$Logged <- TRUE
            }
          }
        }
      }
    }
  })
  
  
  
  observe({
    if(USER$Logged == FALSE){
      
      output$page <- renderUI({
        # div(class="outer",do.call(bootstrapPage,c("",ui1())))
        ui1
      })
      
    }
    
    if(USER$Logged == TRUE){
      
      output$page <- renderUI({ui2})
      
      
      output$plot2 <- renderPlot({
        # if(input$subset == "After 2011"){
        #   dat1 = dat[dat$year >= 2012,]
        # } else if(input$subset == "Fullset"){
        #   dat1 = dat
        # } else{
        #   dat1 = dat[dat$year >= 2013,]
        # }
        dat1 <- dat
        dat1_ <- melt(dat1[, names(dat1) %in% c(input$variable1, "PM_Jingan", "PM_Xuhui", "PM_US.Post")],
                      id.vars = input$variable1)
        if(input$variable1 %in% c("season", "month", "day", "hour", "cbwd", "weekday")){
          
          ggplot(data = dat1_) +
            geom_boxplot(mapping = aes_string(x = paste0("as.factor(", input$variable1, ")"),
                                              y = "value")) +
            facet_grid(variable ~.) +
            ggtitle(paste0("PM values vs ", input$variable1)) +
            xlab(input$variable1) +
            ylim(0, input$ylimit)
        } else{
          
          ggplot(data = dat1_) +
            geom_point(mapping = aes_string(x = paste0(input$variable1),
                                            y = "value"),
                       size = 1,
                       alpha = .15) +
            facet_grid(variable ~.) +
            ggtitle(paste0("PM value vs ", input$variable1)) +
            xlab(input$variable1) +
            ylim(0, input$ylimit)
        }
        
      },
      height = 700, width = 700)
      
      output$predict2 <- renderTable({
        
        if(is.null(input$file)){
          return(NULL)
        }
        dat_predict <- read.csv(input$file$datapath,
                                header = T,
                                sep = input$sep)
        dat_predict$month <- as.factor(dat_predict$month)
        dat_predict$hour <- as.factor(dat_predict$hour)
        # dat_predict$Date <- as.Date(dat_predict$Date)
        dat_predict$weekday <- factor(dat_predict$weekday,
                                      levels = c("Sunday", "Monday", "Tuesday",
                                                 "Wednesday", "Thursday", "Friday",
                                                 "Saturday"))
        dat_predict$month <- factor(dat_predict$month,
                                    levels = c("1", "2", "3", "4", "5", "6",
                                               "7", "8", "9", "10", "11", "12"))
        dat_predict$hour <- factor(dat_predict$hour,
                                   levels = c("1", "2", "3", "4", "5", "6",
                                              "7", "8", "9", "10", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "0"))
        dat_predict$cbwd <- factor(dat_predict$cbwd,
                                   levels = c("cv", "NE", "NW", "SE", "SW"))
        
        if(input$station == "All"){
          output = data.frame(Time = seq(0, 23),
                              US_Post = round(predict(model_rf_US, dat_predict)),
                              Xuhui = round(predict(model_rf_Xuhui, dat_predict)),
                              Jingan = round(predict(model_rf_Jingan, dat_predict)))
          mean <- apply(output[, c(2,3,4)], 1, mean, 0, TRUE)
          output$Level <- ifelse(mean<75, "Low",
                                 ifelse(mean<150, "Medium",
                                        ifelse(is.na(mean)==TRUE, "Missing", "High")))
        } else if(input$station == "US_Post"){
          output = data.frame(Time=seq(0, 23),
                              US_Post = round(predict(model_rf_US, dat_predict)))
          output$Level <- ifelse(output$US_Post<75, "Low",
                                 ifelse(output$US_Post<150, "Medium",
                                        ifelse(is.na(output$US_Post) == TRUE, "Missing", "High")))
        } else if(input$station == "Xuhui"){
          output = data.frame(Time=seq(0, 23),
                              Xuhui = round(predict(model_rf_Xuhui, dat_predict)))
          output$Level <- ifelse(output$Xuhui<75, "Low",
                                 ifelse(output$Xuhui<150, "Medium",
                                        ifelse(is.na(output$Xuhui)==TRUE, "Missing", "High")))
        } else if(input$station == "Jingan"){
          output = data.frame(Time=seq(0, 23),
                              Jingan = round(predict(model_rf_Jingan, dat_predict)))
          output$Level <- ifelse(output$Jingan<75, "Low",
                                 ifelse(output$Jingan<150, "Medium",
                                        ifelse(is.na(output$Jingan)==TRUE, "Missing", "High")))
        }
        output
      })
      
      output$predict <- renderDataTable({
        
        month_ <- ifelse(input$month=="January", "1",
                         ifelse(input$month=="February", "2",
                                ifelse(input$month=="March", "3",
                                       ifelse(input$month=="April", "4",
                         ifelse(input$month=="May", "5",
                                ifelse(input$month=="June", "6",
                                       ifelse(input$month=="July", "7",
                         ifelse(input$month=="August", "8",
                                ifelse(input$month=="September", "9",
                                       ifelse(input$month=="October", "10",
                         ifelse(input$month=="November", "11", "12")))))))))))
        dat_pred <- data.frame(PM_US.Post = input$yesterdayPM,
                               PM_Jingan = input$yesterdayPM,
                               PM_Xuhui = input$yesterdayPM,
                               DEWP = input$DEWP,
                               HUMI = input$HUMI,
                               PRES = input$PRES,
                               TEMP = input$TEMP,
                               Iws = input$Iws,
                               cbwd = input$cbwd,
                               month = month_,
                               weekday = input$weekday,
                               hour = input$hour)
        dat_pred$weekday <- factor(dat_pred$weekday,
                                      levels = c("Sunday", "Monday", "Tuesday",
                                                 "Wednesday", "Thursday", "Friday",
                                                 "Saturday"))
        dat_pred$month <- factor(dat_pred$month,
                                    levels = c("1", "2", "3", "4", "5", "6",
                                               "7", "8", "9", "10", "11", "12"))
        dat_pred$hour <- factor(dat_pred$hour,
                                   levels = c("1", "2", "3", "4", "5", "6",
                                              "7", "8", "9", "10", "11", "12",
                                              "13", "14", "15", "16", "17", "18",
                                              "19", "20", "21", "22", "23", "0"))
        dat_pred$cbwd <- factor(dat_pred$cbwd,
                                   levels = c("cv", "NE", "NW", "SE", "SW"))
        
        # print(str(dat_pred))
        result <- data.frame(
          Location = c("US Post", "Jingan", "Xuhui"),
          PM_2.5 = c(round(predict(model_rf_US, newdata=dat_pred)),
                     round(predict(model_rf_Jingan, newdata=dat_pred)),
                     round(predict(model_rf_Xuhui, newdata=dat_pred)))
        )
        result$Level <- ifelse(result$PM_2.5<75, "Low",
                               ifelse(result$PM_2.5<150, "Medium", "High"))
        
        datatable(result, rownames=FALSE) %>% formatStyle("Level", target="row",
                                                          backgroundColor = styleEqual(
                                                            c("Low", "Medium", "High"),
                                                            c("green", "yellow", "red")))
      })
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

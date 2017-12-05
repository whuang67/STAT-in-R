#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)

Logged = FALSE
my_username <- c("group5", "whuang67")
my_password <- c("group5", "whuang67")


ui1 <- fluidPage(
  
  wellPanel(
    h3("Authorized Users Only (Password Required)"),
    p("Maintainer: Wenke Huang (whuang67@illinois.edu)"),
    textInput("userName", "Username", value = "group5"),
    passwordInput("passwd", "Password"),
    # br(),
    actionButton("Login", "Log in")
    
  ))



# Define UI for application that draws a histogram
ui2 <- shinyUI(pageWithSidebar(
  headerPanel("PM 2.5 Project"),
  
  sidebarPanel(
    # conditionalPanel(
      condition = "input.conditionalPanels == 1",
      selectInput("variable1", label = "Variable:",
                  choices = c("season", "month", "day", "weekday", "hour", "cbwd",
                              
                              "DEWP", "HUMI", "PRES", "TEMP", "Iws", "precipitation", "Iprec"),
                  selected = "season"),
      
      radioButtons("subset", label = "Dataset:",
                   choices = c("Fullset", "After 2011", "After 2012"),
                   selected = "After 2011"),
      
      sliderInput("ylimit",
                  label = "PM 2.5 Limitation",
                  min = 100,
                  max = 750,
                  value = 750)
  #   )
  ),
  
  mainPanel(tabPanel("Data Visualization",
                     plotOutput("plot2"),
                     value = 1),
            type = "pills",
            id = "conditionalPanels")
  )
)

ui <- htmlOutput("page")

dat <- read.csv("ShanghaiPM20100101_20151231.csv")
# dat <- dat[complete.cases(dat), ]
dat$Date <- as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$weekday <- factor(weekdays(dat$Date), levels = c("Sunday", "Monday", "Tuesday",
                                                     "Wednesday", "Thursday", "Friday",
                                                     "Saturday", "Sunday"))



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
        if(input$subset == "After 2011"){
          dat1 = dat[dat$year >= 2012,]
        } else if(input$subset == "Fullset"){
          dat1 = dat
        } else{
          dat1 = dat[dat$year >= 2013,]
        }
        
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
    }
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


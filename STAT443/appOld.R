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

Logged = FALSE
my_username <- c("group5", "whuang67")
my_password <- c("group5", "whuang67")


ui1 <- fluidPage(
  
  wellPanel(
    h3("Authorized Users Only"),
    textInput("userName", "Username", value = "group5"),
    passwordInput("passwd", "Password"),
    # br(),
    actionButton("Login", "Log in")
  
))



# Define UI for application that draws a histogram
ui2 <- shinyUI(pageWithSidebar(
  headerPanel("PM 2.5 Project"),
  
  sidebarPanel(
    conditionalPanel(
      condition = "input.conditionalPanels == 2",
      selectInput("variable1", label = "Variable:",
                  choices = c("season", "month", "day", "hour", "cbwd",
                              
                              "DEWP", "HUMI", "PRES", "TEMP", "Iws", "precipitation", "Iprec"),
                  selected = "season"),
      
      selectInput("variable2", label = "PM2.5 Station:",
                  choices = c("U.S. Consulate" = "PM_US.Post",
                              "Jing An" = "PM_Jingan",
                              "Xu Hui" = "PM_Xuhui"),
                  selected = "PM_US.Post"),
    
      radioButtons("subset", label = "Dataset:",
                   choices = c("Fullset", "After 2011", "After 2012"),
                   selected = "After 2011"),
      
      sliderInput("ylimit",
                label = "PM 2.5 Limitation",
                min = 300,
                max = 750,
                value = 750)
  ),
  
  conditionalPanel(
    condition = "input.conditionalPanels == 1",
    selectInput("variable3", label = "Variable:",
                choices = c("season", "month", "day", "hour", "cbwd",
                            
                            "DEWP", "HUMI", "PRES", "TEMP", "Iws", "precipitation", "Iprec",
                            
                            "U.S. Consulate" = "PM_US.Post",
                            "Jing An" = "PM_Jingan",
                            "Xu Hui" = "PM_Xuhui"),
                selected = "PM_US.Post"),
    radioButtons("subset2", label = "Dataset:",
                 choices = c("Fullset", "Since U.S. started recording",
                             "Since China started recording"),
                 selected = "Since U.S. started recording")
    
    
  )),
  
  mainPanel(
    tabsetPanel(tabPanel("Univariate",
                         plotOutput("plot1"),
                         value = 1),
                tabPanel("Multivariate",
                         plotOutput("plot2"),
                         value = 2),
    type = "pills",
    id = "conditionalPanels")
  )
)
)

ui <- htmlOutput("page")

dat <- read.csv("ShanghaiPM20100101_20151231.csv")



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
      output$plot1 <- renderPlot({
        
        if(input$subset2 == "Since U.S. started recording"){
          dat2 = dat[dat$No >= 17443,]
        } else if(input$subset2 == "Since China started recording"){
          dat2 = dat[dat$No >= 26305,]
        } else{
          dat2 = dat
        }
        
        
        if(input$variable3 %in% c("season", "month", "day", "hour", "cbwd")){
          
          ggplot(data = dat2) +
            geom_bar(mapping = aes_string(x = paste0("as.factor(", input$variable3, ")"))) +
            ggtitle(paste0("Barplot of ", input$variable3)) +
            xlab(input$variable3)
          
        } else if(input$variable3 %in% c("DEWP", "HUMI", "PRES", "TEMP",
                                         "Iws", "precipitation", "Iprec")){
          
          ggplot(data = dat2) +
            geom_histogram(mapping = aes_string(x = input$variable3),
                           color = "white",
                           bins = 50) +
            ggtitle(paste0("Histogram of ", input$variable3))
        } else{
          
          ggplot(data = dat2,
                 mapping = aes(x = No)) +
            geom_line(mapping = aes_string(y = input$variable3)) +
            ggtitle(paste0("Historical records of ", input$variable3))
        }
      },
      height = 500, width = 900)
      
      
      output$plot2 <- renderPlot({
        if(input$subset == "After 2011"){
          dat1 = dat[dat$year >= 2012,]
        } else if(input$subset == "Fullset"){
          dat1 = dat
        } else{
          dat1 = dat[dat$year >= 2013,]
        }
        
        if(input$variable1 %in% c("season", "month", "day", "hour", "cbwd")){
          
          ggplot(data = dat1) +
            geom_boxplot(mapping = aes_string(x = paste0("as.factor(", input$variable1, ")"),
                                              y = input$variable2)) +
            facet_grid(as.factor(year) ~.) +
            ggtitle(paste0(input$variable2, " vs ", input$variable1)) +
            xlab(input$variable1) +
            ylim(0, input$ylimit)
        } else{
          
          ggplot(data = dat1) +
            geom_point(mapping = aes_string(x = paste0(input$variable1),
                                            y = input$variable2),
                       size = 1,
                       alpha = .15) +
            facet_grid(as.factor(year) ~.) +
            ggtitle(paste0(input$variable2, " vs ", input$variable1)) +
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


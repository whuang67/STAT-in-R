#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

dat <- read.csv("C:/users/whuang67/downloads/ShanghaiPM20100101_20151231.csv")
dat$Date <- as.Date(paste(dat$year, dat$month, dat$day, sep="-"))
dat$weekday <- as.factor(weekdays(dat$Date))
dat$y <- c(rep(NA, 24), dat$PM_US.Post)[1:52584]
dat1 <- dat[(is.na(dat$PM_US.Post) == FALSE) &
              is.na(dat$y) == FALSE, ]
dat1$DEWP[is.na(dat1$DEWP)] <- median(dat1$DEWP, na.rm=TRUE)
dat1$HUMI[is.na(dat1$HUMI)] <- median(dat1$HUMI, na.rm=TRUE)
dat1$PRES[is.na(dat1$PRES)] <- median(dat1$PRES, na.rm=TRUE)
dat1$TEMP[is.na(dat1$TEMP)] <- median(dat1$TEMP, na.rm=TRUE)
dat1$cbwd <- as.character(dat1$cbwd)
dat1$cbwd[is.na(dat1$cbwd)] <- "NE"
dat1$cbwd <- as.factor(dat1$cbwd)
dat1$Iws[is.na(dat1$Iws)] <- median(dat1$Iws, na.rm=TRUE)
dat1$precipitation[is.na(dat1$precipitation)] <- median(dat1$precipitation, na.rm=TRUE)
dat1$Iprec[is.na(dat1$Iprec)] <- median(dat1$Iprec, na.rm=TRUE)
dat1$month <- as.factor(dat1$month)
dat1$hour <- as.factor(dat1$hour)

set.seed(1)
idx <- sample(1:nrow(dat1), nrow(dat1)*0.8)
train_dat <- dat1[idx, ]; test_dat <- dat1[-idx, ]
model_lm <- lm(log(y+1) ~ PM_US.Post + DEWP + HUMI + PRES + TEMP + Iws + as.factor(cbwd) +
                 precipitation + Iprec + as.factor(month) + as.factor(hour) +
                 weekday, data = train_dat)


ui <- shinyUI(pageWithSidebar(
  headerPanel("PM 2.5 Project"),
  
  sidebarPanel(
    conditionalPanel(
      condition = "input.conditionalPanels == 1",
      textInput("PM_US.Post", label = "Yesterday PM2.5:", value = 50),
      textInput("month", label = "month:", value = 1),
      textInput("weekday", label = "weekday", value = "Monday"),
      textInput("hour", label = "hour:", value = 1),
      textInput("season", label = "season:", value = 1),
      textInput("DEWP", label = "DEWP:", value = 2),
      textInput("HUMI", label = "HUMI:", value = 1),
      textInput("PRES", label = "PRES:", value = 1),
      textInput("TEMP", label = "TEMP:", value = 1),
      textInput("cbwd", label = "cbwd:", value = "cv"),
      textInput("Iws", label = "Iws:", value = 1),
      textInput("precipitation", label = "precipitation:", value = 1),
      textInput("Iprec", label = "Iprec:", value = 1)
    ),
    conditionalPanel(
      condition = "input.conditionalPanels == 2",
      sidebarPanel(
        fileInput("file", label = "Uploade here!",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
      ),
      downloadButton("download", label = "Predictions here!")
    )),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Predict 1",
               htmlOutput("predict"),
               value = 1),
      
      tabPanel("Predict 2",
               tableOutput("predict2"),
               value = 2),
      type = "pills",
      id = "conditionalPanels"
    )
    
  )
))

server <- function(input, output){
  dat_test <- reactiveValues()
  observe(dat_test$data <- data.frame(PM_US.Post = as.numeric(input$PM_US.Post),
                                      month = input$month,
                                      weekday = input$weekday,
                                      hour = input$hour,
                                      season = input$season,
                                      DEWP = as.numeric(input$DEWP),
                                      HUMI = as.numeric(input$HUMI),
                                      PRES = as.numeric(input$PRES),
                                      TEMP = as.numeric(input$TEMP),
                                      cbwd = input$cbwd,
                                      Iws = as.numeric(input$Iws),
                                      precipitation = as.numeric(input$precipitation),
                                      Iprec = as.numeric(input$Iprec)))

  
  output$predict <- renderText({
    paste0("The prediction is ", predict(model_lm, dat_test$data))
  })
  
  output$predict2 <- renderTable({
   
   inFile <- input$file
   if(is.null(inFile)){
     return(NULL)
   }
   dat_predict <- read.csv(inFile$datapath, header = TRUE)
   dat_predict
  })
  

}

shinyApp(ui = ui, server = server)


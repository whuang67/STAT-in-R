
library(dplyr)
library(ggplot2)
library(shiny)
library(lazyeval)
library(pROC)
library(gridExtra)


ui3 <- shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel("Krannert Center (Wenke Huang's sample work)"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    conditionalPanel(
      condition = "input.conditionalPanels <= 2",
      selectInput("variable1", label = "Variable 1:",
                  choices = c("PriceType", "Theatre", "Disposition",
                              "PurchasedOnline", "Producer", "Category",
                              "Price","Capacity",
                              "TicketSold", "DaysBetween", "QuantityPurchased",
                              "QuantityPurchasedTotal",
                              "EventPurchasedTotal"), selected = NULL),
      selectInput("variable2", label = "Variable 2:",
                  choices = c("PriceType", "Theatre", "Disposition",
                              "PurchasedOnline", "Producer", "Category",
                              "Redeemed (Response Variable)" = "Redeemed",
                              "Price","Capacity",
                              "TicketSold", "DaysBetween", "QuantityPurchased",
                              "QuantityPurchasedTotal",
                              "EventPurchasedTotal"), selected = "Redeemed"),
      radioButtons("Conf_Level", label = "Confidence Level:",
                   choices = c("0.90" = 0.9,
                               "0.95" = 0.95,
                               "0.99" = 0.99),
                   selected = 0.95)
    ),
    
    conditionalPanel(
      condition = "input.conditionalPanels >= 3",
      checkboxGroupInput("Predictor",
                         label = "Potential Predictors:",
                         choices = c("PriceType", "Theatre", "Disposition",
                                     "PurchasedOnline", "Producer", "Category",
                                     "Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal"),
                         selected = c("PriceType","QuantityPurchased", "Price",
                                      "QuantityPurchasedTotal",
                                      "TicketSold", "EventPurchasedTotal")),
      radioButtons("size", "Training Set Size:",
                   choices = c("50%" = 25918,
                               "60%" = 31102,
                               "70%" = 36285,
                               "80%" = 41469,
                               "90%" = 46652),
                   selected = 36285)
    ),
    br(),
    width = 3),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Visualization",
               plotOutput("plot2"),
               h5(strong("Remarks:")),
               p("* For Categorical variable vs Categorical variable, ",
                 strong("Pearson Chi-Square test"),
                 " is applied."),
               p("* For Categorical variable vs Numerical variable, ",
                 strong("ANOVA"),
                 " and ",
                 strong("Tukey HSD test"),
                 " is applied."),
               p("* For Numerical variable vs Numerical variable, ",
                 strong("Pearson Correlation"),
                 " is applied"),
               value = 1),
      tabPanel("Statistical Test",
               tableOutput("table"),
               h5(strong("Remark:")),
               p("* For Categorical variable vs Categorical variable, ",
                 strong("Contingency Table"),
                 " is applied."),
               p("* For Categorical variable vs Numerical variable, ",
                 strong("ANOVA"),
                 " and ",
                 strong("Tukey HSD test"),
                 " is applied."),
               p("* For Numerical variable vs Numerical variable, ",
                 strong("Pearson Correlation"),
                 " and ",
                 strong("Covariance"),
                 " is applied"),
               value = 2),
      tabPanel("Summary of Fitted Model",
               p("* The summary of ",
                 strong("Logistic Regression"),
                 " model is shown below."),
               p("* Potential input ",
                 strong("Predictors"),
                 " and ",
                 strong("Size of Training Dataset"),
                 " can be chosen from the left side"),
               verbatimTextOutput("summary"),
               value = 3),
      tabPanel("Prediction",
               p("* ",
                 strong("ROC Curve"),
                 " and its corresponding ",
                 strong("AUC"),
                 " are shown below."),
               p("* The left plot is prediction from the ",
                 strong("Training Dataset"),
                 " while the right one is from the ",
                 strong("Testing Dataset")),
               p("* Please wait for around 5 seconds for the plots. ",
                 "Thank you for your patience!"),
               plotOutput("plot1"),
               value = 4),
      type = "pills",
      id = "conditionalPanels"
    )
  )
))



ui1 <- shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel("Krannert Center (Wenke Huang's sample work)"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    selectInput("variable1", label = "Variable 1:",
                choices = c("PriceType", "Theatre", "Disposition",
                            "PurchasedOnline", "Producer", "Category",
                            "Price","Capacity",
                            "TicketSold", "DaysBetween", "QuantityPurchased",
                            "QuantityPurchasedTotal",
                            "EventPurchasedTotal"), selected = NULL),
    selectInput("variable2", label = "Variable 2:",
                choices = c("PriceType", "Theatre", "Disposition",
                            "PurchasedOnline", "Producer", "Category",
                            "Redeemed (Response Variable)" = "Redeemed",
                            "Price","Capacity",
                            "TicketSold", "DaysBetween", "QuantityPurchased",
                            "QuantityPurchasedTotal",
                            "EventPurchasedTotal"), selected = "Redeemed"),
    radioButtons("Conf_Level", label = "Confidence Level:",
                 choices = c("0.90" = 0.9,
                             "0.95" = 0.95,
                             "0.99" = 0.99),
                 selected = 0.95),
    checkboxGroupInput("Predictor",
                       label = "Potential Predictors:",
                       choices = c("PriceType", "Theatre", "Disposition",
                                   "PurchasedOnline", "Producer", "Category",
                                   "Price","Capacity",
                                   "TicketSold", "DaysBetween", "QuantityPurchased",
                                   "QuantityPurchasedTotal",
                                   "EventPurchasedTotal"),
                       selected = c("PriceType", "Theatre", "Producer", "DaysBetween",
                                    "QuantityPurchased", "Price", "QuantityPurchasedTotal",
                                    "TicketSold", "EventPurchasedTotal")),
    radioButtons("size", "Training Set Size:",
                 choices = c("50%" = 25918,
                             "60%" = 31102,
                             "70%" = 36285,
                             "80%" = 41469,
                             "90%" = 46652),
                 selected = 36285),
    br(),
    width = 3),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Sample Work (Visualization)",
               plotOutput("plot2"),
               h5(strong("Remarks:")),
               p("* For Categorical variable vs Categorical variable, ",
                 strong("Pearson Chi-Square test"),
                 " is applied."),
               p("* For Categorical variable vs Numerical variable, ",
                 strong("ANOVA"),
                 " and ",
                 strong("Tukey HSD test"),
                 " is applied."),
               p("* For Numerical variable vs Numerical variable, ",
                 strong("Pearson Correlation"),
                 " is applied")),
      tabPanel("Sample Work (Statistical Test)",
               tableOutput("table"),
               h5(strong("Remark:")),
               p("* For Categorical variable vs Categorical variable, ",
                 strong("Contingency Table"),
                 " is applied."),
               p("* For Categorical variable vs Numerical variable, ",
                 strong("ANOVA"),
                 " and ",
                 strong("Tukey HSD test"),
                 " is applied."),
               p("* For Numerical variable vs Numerical variable, ",
                 strong("Pearson Correlation"),
                 " and ",
                 strong("Covariance"),
                 " is applied")),
      tabPanel("Summary of Fitted Model",
               verbatimTextOutput("summary")),
      type = "pills"
    )
  )
))





kran <- read.csv("Cleaned_Ticket_Redemption_Data.csv", header = TRUE)
kran$Redeemed_Numeric <- kran$Redeemed
kran$Redeemed <- as.factor(kran$Redeemed)
kran$DaysBetween <- as.numeric(kran$DaysBetween)
kran$Disposition <- ifelse(is.na(kran$Disposition) == TRUE,
                           "N/A",
                           as.character(kran$Disposition))

kran$PriceType <- factor(kran$PriceType, levels = c("YT", "SA", "SC", "SU",
                                                    "UI", "SP", "C", "P"))
kran$Theatre <- factor(kran$Theatre, levels = c("Tryon Festival Theatre", "Colwell Playhouse",
                                                "FGH Stage, Salon Style", "Foellinger Great Hall",
                                                "Studio Theatre", "Krannert Room"))
kran$Disposition <- factor(kran$Disposition, levels = c("M (Mailed)", "PAH (Print at Home)",
                                                        "C (Counter)", "MD1 (Mobile Delivery - pdf only)",
                                                        "MD2 (Mobile Delivery - pdf + passbook)",
                                                        "W (Will Call)", "N/A"))
kran$Producer <- factor(kran$Producer, levels = c("Lyric Theatre at Illinois",
                                                  "Dance at Illinois",
                                                  "Marquee", "Champaign Urbana Symphony Orchestra",
                                                  "Illinois Theatre",
                                                  "Sinfonia da Camera",
                                                  "School of Music"))
kran$Category <- factor(kran$Category, levels = c("Dance", "Family", "Opera",
                                                  "Orchestra/Soloist", "Chamber Group/Soloist",
                                                  "Performance Art", "Theatre",
                                                  "Music-Contemporary", "Music-Instrumental",
                                                  "Music-Choral",
                                                  "Dessert and Conversation"))


server2 <- function(input, output){
  dat <- reactiveValues()
  
    observe(dat$test <- kran %>% 
              group_by_(input$variable1, input$variable2) %>% 
              summarize(Total = n()))
    observe(dat$test1 <- dat$test %>%
              group_by_(input$variable1) %>%
              summarize(Total1 = sum(Total)))
    observe(dat$test2 <- dat$test %>%
              group_by_(input$variable1) %>%
              mutate(Total2 = sum(Total), Percentage = round(Total/sum(Total), 4)))
    observe(dat$test3 <- kran %>% 
              group_by_(input$variable1) %>%
              summarize_(Mean = interp(~round(mean(x, na.rm = TRUE), 2), x = as.name(input$variable2))) %>%
              mutate(Adjust = max(Mean)/5))
    observe(dat$test4 <- kran %>% 
              group_by_(input$variable2) %>%
              summarize_(Mean = interp(~round(mean(x, na.rm = TRUE), 2), x = as.name(input$variable1))) %>%
              mutate(Adjust = max(Mean)/5))
    observe(dat$anova_set <- kran %>%
              select_(a = input$variable1,
                      b = input$variable2) %>%
              mutate_(Conf_Level = input$Conf_Level))
    observe(dat$Train <- kran[sample(1:51863, input$size),
                             names(kran) %in% c(input$Predictor, "Redeemed_Numeric")])
    observe(dat$Test <- kran[-sample(1:51863, input$size),
                             names(kran) %in% c(input$Predictor, "Redeemed_Numeric")])

  
  
  
  output$plot2 <- renderPlot({
    if(input$variable1 != input$variable2 &
       input$variable2 %in% c("PriceType", "Theatre", "Disposition",
                              "PurchasedOnline", "Producer", "Category",
                              "Redeemed") &
       input$variable1 %in% c("PriceType", "Theatre", "Disposition",
                              "PurchasedOnline", "Producer", "Category")){
      
      PearsonChiSq <- chisq.test(table(dat[["anova_set"]]$a, dat[["anova_set"]]$b))
      ChiSq <- PearsonChiSq$statistic
      pvalue <- PearsonChiSq$p.value
      
      ggplot(mapping = aes_string(x = input$variable1,
                                  y = "Percentage",
                                  fill = input$variable2),
             data = dat[["test2"]]) +
        geom_bar(stat = "identity",
                 color = "white") +
        geom_text(mapping = aes(label = paste0(Percentage*100, "%")),
                  position = position_stack(vjust = 0.5)) +
        scale_fill_discrete(name = input$variable2) +
        theme(axis.text.x = element_text(angle = -15)) +
        ggtitle(paste(input$variable1,
                      " vs ",
                      input$variable2,
                      ifelse(input$variable2 == "Redeemed",
                             " (Response Variable),", ","),
                      " Chi-Sq Stat: ",
                      round(ChiSq, 2),
                      ", p-value: ",
                      round(pvalue, 4),
                      ifelse(pvalue >= (1 - dat[["anova_set"]]$Conf_Level[1]),
                             " (Insignificant)",
                             " (Significant)"),
                      sep = ""))
      
      
    } else if(input$variable1 == input$variable2){
      ggplot() +
        geom_bar(mapping = aes_string(x = input$variable1,
                                      y = "Total"),
                 data = dat[["test"]],
                 stat = "identity",
                 color = "white") +
        geom_text(aes_string(x= input$variable1,
                             y = "Total1 + 500",
                             label = "Total1"),
                  data = dat[["test1"]],
                  position = position_stack(vjust = 1)) +
        scale_fill_discrete(name = input$variable1) +
        theme(axis.text.x = element_text(angle = -15)) +
        ggtitle(paste("Histogram of ", input$variable1, sep = ""))
      
    } else if(input$variable2 %in% c("Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal") &
              input$variable1 %in% c("PriceType", "Theatre", "Disposition",
                                     "PurchasedOnline", "Producer", "Category")){
      
      ANOVA <- anova(lm(b ~ a, data = dat[["anova_set"]]))
      FStatistics <- ANOVA[[4]][1]
      pvalue <- ANOVA[[5]][1]
      ggplot() +
        geom_boxplot(mapping = aes_string(x = input$variable1,
                                          y = input$variable2,
                                          group = input$variable1),
                     data = kran) +
        geom_text(mapping = aes_string(x = input$variable1,
                                       y = "Mean + Adjust",
                                       label = "Mean"),
                  data = dat[["test3"]]) +
        geom_point(mapping = aes_string(x = input$variable1,
                                        y = "Mean",
                                        color = "'Mean'"),
                   data = dat[["test3"]],
                   shape = 23,
                   fill = "red",
                   size = 3) +
        scale_fill_discrete(name = input$variable1) +
        theme(axis.text.x = element_text(angle = -15)) +
        ggtitle(paste(input$variable1,
                      " vs ",
                      input$variable2,
                      ", F Statistics: ",
                      round(FStatistics, 2),
                      ", p-value: ",
                      round(pvalue, 4),
                      ifelse(pvalue >= 0.05,
                             " (Insignificant)",
                             " (Significant)"),
                      sep = ""))
    } else if(input$variable1 != input$variable2 &
              input$variable2 %in% c("Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal")){
      ggplot() +
        geom_point(mapping = aes(x = a, y = b),
                   data = dat[["anova_set"]]) +
        ggtitle(paste(input$variable1,
                      " vs ",
                      input$variable2,
                      ", Pearson Correlation: ",
                      round(cor(dat[["anova_set"]]$a, dat[["anova_set"]]$b), 4),
                      sep = ""))
      
    } else if(input$variable1 %in% c("Price","Capacity",
                                      "TicketSold", "DaysBetween", "QuantityPurchased",
                                      "QuantityPurchasedTotal",
                                      "EventPurchasedTotal") &
               input$variable2 %in% c("PriceType", "Theatre", "Disposition",
                                      "PurchasedOnline", "Producer", "Category",
                                      "Redeemed")){
      
      ANOVA <- anova(lm(a ~ b, data = dat[["anova_set"]]))
      FStatistics <- ANOVA[[4]][1]
      pvalue <- ANOVA[[5]][1]
      ggplot() +
        geom_boxplot(mapping = aes_string(x = input$variable2,
                                          y = input$variable1,
                                          group = input$variable2),
                     data = kran) +
        geom_text(mapping = aes_string(x = input$variable2,
                                       y = "Mean + Adjust",
                                       label = "Mean"),
                  data = dat[["test4"]]) +
        geom_point(mapping = aes_string(x = input$variable2,
                                        y = "Mean",
                                        color = "'Mean'"),
                   data = dat[["test4"]],
                   shape = 23,
                   fill = "red",
                   size = 3) +
        scale_fill_discrete(name = input$variable2) +
        theme(axis.text.x = element_text(angle = -15)) +
        ggtitle(paste(input$variable2,
                      ifelse(input$variable2 == "Redeemed",
                             " (Response Variable)", ""),
                      " vs ",
                      input$variable1,
                      ", F Statistics: ",
                      round(FStatistics, 2),
                      ", p-value: ",
                      round(pvalue, 4),
                      ifelse(pvalue >= 0.05,
                             " (Insignificant)",
                             " (Significant)"),
                      sep = ""))
    } 
    
  },
  height = 400,
  width = 700)
  
  
  
  
  output$table <- renderTable({
    if(input$variable1 != input$variable2 &
       input$variable2 %in% c("PriceType", "Theatre", "Disposition",
                              "PurchasedOnline", "Producer", "Category",
                              "Redeemed") &
       input$variable1 %in% c("PriceType", "Theatre", "Disposition",
                              "PurchasedOnline", "Producer", "Category")){
      
        ContingencyTable <- as.data.frame.matrix(table(dat[["anova_set"]]$a, dat[["anova_set"]]$b))
        ContingencyTable
        
    } else if(input$variable1 == input$variable2 &
              input$variable1 %in% c("PriceType", "Theatre", "Disposition",
                                     "PurchasedOnline", "Producer", "Category")){
      
      Table <- kran %>%
        group_by_(input$variable1) %>%
        summarize(Freq = n())
      Table
      
    } else if(input$variable1 == input$variable2 &
              input$variable1 %in% c("Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal")){
      Table <- dat[["anova_set"]] %>%
        summarize(Mean = mean(a, na.rm = TRUE),
                  Variance = var(a, na.rm = TRUE),
                  Max = max(a, na.rm = TRUE),
                  Q3 = quantile(a, probs = 0.75, na.rm = TRUE),
                  Median = median(a, na.rm = TRUE),
                  Q1 = quantile(a, probs = 0.25, na.rm = TRUE),
                  Min = min(a, na.rm = TRUE))
      rownames(Table) <- "Basic Statistics"
      Table
      
    } else if(input$variable1 != input$variable2 &
              input$variable1 %in% c("Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal") &
              input$variable2 %in% c("Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal")){
      Table <- dat[["anova_set"]] %>%
        summarize(Covariance = cov(a, b, use = "pairwise.complete.obs"),
                  Correlation = cor(a, b, use = "pairwise.complete.obs"),
                  Relationship = ifelse(cor(a, b, use = "pairwise.complete.obs") > 0,
                                        "Positive Correlated",
                                        "Negative Correlated"))
      Table
      
    } else if(input$variable2 %in% c("Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal") &
              input$variable1 %in% c("PriceType", "Theatre", "Disposition",
                                     "PurchasedOnline", "Producer", "Category")){
      Table <- TukeyHSD(aov(lm(b ~ a, data = dat[["anova_set"]])),
                        conf.level = dat[["anova_set"]]$Conf_Level[1])[[1]]
      Table <- data.frame(Table)
      names(Table) <- c("Difference", "Lower Bound", "Upper Bound", "Adj p-value")
      Table
    } else if(input$variable1 %in% c("Price","Capacity",
                                     "TicketSold", "DaysBetween", "QuantityPurchased",
                                     "QuantityPurchasedTotal",
                                     "EventPurchasedTotal") &
              input$variable2 %in% c("PriceType", "Theatre", "Disposition",
                                     "PurchasedOnline", "Producer", "Category",
                                     "Redeemed")){
      Table <- TukeyHSD(aov(lm(a ~ b, data = dat[["anova_set"]])),
                        conf.level = dat[["anova_set"]]$Conf_Level[1])[[1]]
      Table <- data.frame(Table)
      names(Table) <- c("Difference", "Lower Bound", "Upper Bound", "Adj p-value")
      Table

    }
  },
  #caption = "Statistical Test",
  #caption.placement = getOption("xtable.caption.placement", "top"),
  rownames = TRUE,
  digit = 3)
  
  output$summary <- renderPrint({
    Logistic <- glm(Redeemed_Numeric ~.,
                    data = dat[["Train"]],
                    family = binomial())
    summary(Logistic)
  })
  
  
  output$plot1 <- renderPlot({
    Logistic <- glm(Redeemed_Numeric ~.,
                    data = dat[["Train"]],
                    family = binomial())
    
    
    prediction_response <- predict(Logistic, dat[["Train"]], type = "response")
    a <- roc(dat[["Train"]]$Redeemed, prediction_response)
    ROCdata <- data.frame(Sensitivity = a$sensitivities,
                          Specificity = a$specificities)
    
    
    test_response <- predict(Logistic, dat[["Test"]], type = "response")
    b <- roc(dat[["Test"]]$Redeemed, test_response)
    ROCdata_test <- data.frame(Sensitivity = b$sensitivities,
                               Specificity = b$specificities)
    
    PlotA <- ggplot(data = ROCdata,
                    mapping = aes(x = 1-Specificity,
                                  y = Sensitivity)) +
      geom_line() +
      ggtitle("ROC Curve of Training Set") +
      geom_text(mapping = aes(x = 0.75,
                              y = 0.3,
                              label = paste("AUC = ",
                                            a$auc,
                                            sep = ""))) +
      ylab("True Positive Rate") +
      xlab("False Positive Rate")
  
    PlotB <- ggplot(data = ROCdata_test,
                    mapping = aes(x = 1-Specificity,
                                  y = Sensitivity)) +
      geom_line() +
      ggtitle("ROC Curve of Testing Set") +
      geom_text(mapping = aes(x = 0.75,
                              y = 0.3,
                              label = paste("AUC = ",
                                            b$auc,
                                            sep = ""))) +
      ylab("True Positive Rate") +
      xlab("False Positive Rate")
    
    grid.arrange(PlotA,
                 PlotB,
                 ncol = 2, nrow =1, widths = c(1/2, 1/2))
  },
  height = 400,
  width = 800)
}  
  
  

shinyApp(ui3, server2)




# Content: STAT 427: Statistical Consultanting
## Project: Ticket Redemption Rate Analysis

### Install

This project requires **R** and the following R libraries installed:

- [ggplot2](https://ggplot2.org/)
- [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html)
- [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
- [shiny](https://shiny.rstudio.com/)
- [pROC](https://cran.r-project.org/web/packages/pROC/index.html)

I highly recommend you choose [RStudio](https://www.rstudio.com/) which makes R easier to use and includes a code editor, debugging & visualization tools.

### Run

After opening file `Shiny_app.R`, you will find `Run App` buttom on the top-right side. Theoretical this will turn on the Shiny Application which I created especially target for the Data Visualization here. However, the dataset is not uploaded since it contains detailed information of customers of [Krannert Center](https://krannertcenter.com/).

I also created a [simpler version](https://wenkehuang.shinyapps.io/SampleWork/) of this application which does not contain any sensitive or personal information here!

## Data and Background

Redemption rates are typically only 80-85% for any show, even if all tickets have been distributed.

[Krannert Center for the Performing Arts](https://krannertcenter.com/) does not resell or oversell, so unredeemed tickets are wasted seats to the event.

The dataset has 20 variables and more than 50,000 observations.  

After employing ticket scanners in 2016, KCPA increased and improved its data collection. That is why our data source includes only the ticket information from July, 2016 to Feburary, 2017. 

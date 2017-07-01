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

**Features**
1) `Theatre`: where the show was held in (Nominal);
2) `Price Type`:  {Standard Admision - SA, Senior Citizen - SC, Non-UI - SU, U of IL - UI, YT - Youth, C - Special 1, P - Special 2, SP - Special 3} (Nominal);
3) `Disposition`: where the tickets were purchased (Nominal);
4) `Online`: online purchased or not (Nominal);
5) `Producer`: producer of the event (Nominal);
6) `Category`: category of the event (Nominal);
7) `Ticket Sold`: how many tickets were sold (Continuous);
8) `Capacity`: the capacity of the event (Continuous, ranging from 0 to 1);
9) `Price`: ticket price (Continuous);
10) `Days in Between`: days between purchase and event date (Continuous);
11) `Quantity Purchase`: how many tickets each customer purchased for one event (Continuous);
12) `Quantity Purchase Total`: summarized how many tickets each customer purchased (Continuous);
12) `Event Purchase Total`: for each customer, how many different events did they purchase (Continuous).

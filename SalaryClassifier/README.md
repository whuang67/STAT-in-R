# Content: Machine Learning
## Projects: Annual Salary Classifier

### Install

This project requires **R** and the following R libraries installed:

- [ggplot2](https://ggplot2.org/)
- [gridExtra](https://cran.r-project.org/web/packages/gridExtra/index.html)
- [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
- [shiny](https://shiny.rstudio.com/)
- [pROC](https://cran.r-project.org/web/packages/pROC/index.html)
- [randomForest](https://cran.r-project.org/web/packages/randomForest/index.html)
- [sqldf](https://cran.r-project.org/web/packages/sqldf/index.html)
- [e1071](https://cran.r-project.org/web/packages/e1071/index.html)
- [rpart](https://cran.r-project.org/web/packages/rpart/index.html)
- [rpart.plot](https://cran.r-project.org/web/packages/rpart.plot/index.html)

I highly recommend you choose [RStudio](https://www.rstudio.com/) which makes R easier to use and includes a code editor, debugging & visualization tools.

### Run

After opening the file `AnnulIncome_Project_Code.R`, you will find the `run` botton on the top-right side.

### Data

The modified census dataset consists of approximately 32,000 data points, with each datapoint having 13 features. This dataset is a modified version of the dataset published in the paper *"Scaling Up the Accuracy of Naive-Bayes Classifiers: a Decision-Tree Hybrid",* by Ron Kohavi. You may find this paper [online](https://www.aaai.org/Papers/KDD/1996/KDD96-033.pdf), with the original dataset hosted on [UCI](https://archive.ics.uci.edu/ml/datasets/Census+Income).

**Features**
- `age`: Age
- `workclass`: Working Class (Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, Without-pay, Never-worked)
- `education_level`: Level of Education (Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool)
- `education-num`: Number of educational years completed
- `marital-status`: Marital status (Married-civ-spouse, Divorced, Never-married, Separated, Widowed, Married-spouse-absent, Married-AF-spouse)
- `occupation`: Work Occupation (Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces)
- `relationship`: Relationship Status (Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried)
- `race`: Race (White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black)
- `sex`: Sex (Female, Male)
- `capital-gain`: Monetary Capital Gains
- `capital-loss`: Monetary Capital Losses
- `hours-per-week`: Average Hours Per Week Worked
- `native-country`: Native Country (United-States, Cambodia, England, Puerto-Rico, Canada, Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, Trinadad&Tobago, Peru, Hong, Holand-Netherlands)

**Target Variable**
- `income`: Income Class (<=50K, >50K)


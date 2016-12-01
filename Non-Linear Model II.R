## Markov-switching model
#Example 1- simulated

#install.packages('MSwM')
library(MSwM)
## example data is a simulated data set to show how msmFit can detect 
## the prescence of two different regime
data(example)

str(example)

# plot1: Simulated data. The y variable is the response variable 
# and there are two periods in which this depends on the x covariate
par(mfrow = c(5, 4))
plot(ts(example))

## A linear model is fitted to study how the covariate x explains 
## the variable response y
mod = lm(y~x, example)
summary(mod)
## The covariate is really significant but the data behaviour 
## is very bad explained by the model. 

# The normality of the residual
par(mfrow = c(1, 1))
qqnorm(resid(mod))
qqline(resid(mod))

# Check the ACF of the residual
par(mfrow = c(1, 1))
acf(resid(mod))



## Next, a Autoregressive Markov Switching Model (MSM-AR) is 
## fitted to the data
mod.mswm=msmFit(mod,k=2,p=1,sw=c(T,T,T,T),control=list(parallel=F))
summary(mod.mswm)

## The model mod.mswm has a regime where the covariant x is very 
## significant and in the other regime the autocorrelation variable
## is very significant too. In both, the R-squared have high values. 
## Finally, the transition probabilities matrix has high values which 
## indicate that is difficult to change from on regime to the other. 

# Check the residual 
par(mfrow = c(1, 1))
plot(mod.mswm)

## Generate dianostic plots for the residuals
## overall residual
plotDiag(mod.mswm, which = 2)
plotDiag(mod.mswm, which = 3)
## residual for regime 1
plotDiag(mod.mswm, regime = 1,  which = 2)
plotDiag(mod.mswm, regime = 1,  which = 3)
## residual for regime 2
plotDiag(mod.mswm, regime = 2,  which = 2)
plotDiag(mod.mswm, regime = 2,  which = 3)
## The model detect perfectly the periods of each state. 
## The residuals look like to be white noise and they fit to the 
## Normal Distribution. Moreover, the autocorrelation has disappeared.

plotProb(mod.mswm,which=2)

plotReg(mod.mswm,expl="x")




#Example 2 - text book(AR = 4)
# The growth rate, in percentage, of U.S. quarterly real gross national 
## product 
# (GNP) from the second quarter of 1947 to the ﬁrst quarter of 1991


example2 = 
  structure(c(2.59316410021381, 2.20217123302681, 0.458275619103479, 
              0.968743815568942, -0.241307564718414, 0.896474791426144, 2.05393216767198, 
              1.73353647046698, 0.938712869506845, -0.464778333117193, -0.809834082445603, 
              -1.39763692441103, -0.398860927649558, 1.1918415768741, 1.4562004729396, 
              2.1180822079447, 1.08957867423914, 1.32390272784813, 0.87296368144358, 
              -0.197732729861307, 0.45420214345009, 0.0722187603196887, 1.10303634435563, 
              0.820974907499614, -0.0579579499110212, 0.584477722838197, -1.56192668045796, 
              -2.05041027007508, 0.536371845140342, 2.3367684244086, 2.34014568267516, 
              1.23392627573662, 1.88696478737248, -0.459207909351867, 0.84940472194713, 
              1.70139850766727, -0.287563102546191, 0.095946277449187, -0.860802907461483, 
              1.03447124467041, 1.23685943797014, 1.42004498680119, 2.22410642769683, 
              1.3021017302965, 1.0351769691057, 0.925342521818, -0.165599507925585, 
              1.3444381723048, 1.37500136316918, 1.73222186043569, 0.716056342342333, 
              2.21032138350616, 0.853330335823775, 1.00238777849592, 0.427254413549543, 
              2.14368353713136, 1.4378918561536, 1.5795993028646, 2.27469837381376, 
              1.95962653201067, 0.2599239932111, 1.01946919515563, 0.490163994319276, 
              0.563633789161385, 0.595954621290765, 1.43082852218349, 0.562301244017229, 
              1.15388388887095, 1.68722847001462, 0.774382052478202, -0.0964704476805431, 
              1.39600141863966, 0.136467982223878, 0.552237133917267, -0.399448716111952, 
              -0.61671104590512, -0.0872256083215416, 1.21018349098461, -0.907297546921259, 
              2.64916154469762, -0.00806939681695959, 0.511118931407946, -0.00401437145032572, 
              2.1682142321342, 1.92586729194597, 1.03504719187207, 1.85897218652101, 
              2.32004929969819, 0.255707901889092, -0.0985527428151145, 0.890736834018326, 
              -0.55896483237131, 0.283502534230679, -1.31155410054958, -0.882787789285689, 
              -1.97454945511993, 1.01275266533046, 1.68264718400186, 1.38271278970291, 
              1.86073641586006, 0.444737715592073, 0.414490009766608, 0.992022769383933, 
              1.36283572253682, 1.59970527327726, 1.98845814838348, -0.256842316681229, 
              0.877869502339381, 3.10956544706826, 0.853244770655281, 1.23337321374495, 
              0.0031430232743432, -0.0943336967005583, 0.898833191548979, -0.190366278407953, 
              0.997723787687709, -2.39120056095144, 0.0664967330277127, 1.26136016443398, 
              1.91637832265846, -0.334802886728505, 0.44207108280265, -1.40664914211265, 
              -1.52129894225829, 0.299198686266393, -0.801974492802505, 0.152047924379708, 
              0.985850281223592, 2.1303461510993, 1.34397927090998, 1.61550521216825, 
              2.70930096486278, 1.24461416484445, 0.508354657516633, 0.148021660957899
  ), .Tsp = c(1951.25, 1984.75, 4), class = "ts")

str(example2)

# plot1: Simulated data. 
par(mfrow = c(1, 1))
plot(example2)
abline(h = 0, col = 'red')
# Fit a linear model (AR p = 4). Because msmFit() does not take arima model.
fit = lm(example2[5:135] ~ 1 + example2[4:134] + example2[3:133] 
         + example2[2:132] + example2[1:131])
summary(fit)



# The normality of the residual
par(mfrow = c(1, 1))
plot(resid(fit))
qqnorm(resid(fit))
qqline(resid(fit), col = 'red')

# Check the ACF of the residual
par(mfrow = c(1, 2))
acf(resid(fit))
pacf(resid(fit))



## Next, a Autoregressive Markov Switching Model (MSM-AR) is fitted to the data
mod.mswm=msmFit(fit, k = 2, p = 0, sw = c(T,F,T,T,T,T))
summary(mod.mswm)

mod.mswm2=msmFit(fit, k = 2, p = 0, sw = c(T,T,T,T,T,T))
summary(mod.mswm2)
## The model mod.mswm has a regime where the covariant x is very significant and in the other regime the autocorrelation variable is very significant too. In both, the R-squared have high values. Finally, the transition probabilities matrix has high values which indicate that is difficult to change from on regime to the other. 

# Check the residual 
par(mfrow = c(1, 1))
plot(mod.mswm)
resid(mod.mswm)

## Generate dianostic plots for the residuals
## overall residual
plotDiag(mod.mswm, which = 2)
plotDiag(mod.mswm, which = 3)
## residual for regime 1
plotDiag(mod.mswm, regime = 1,  which = 2)
plotDiag(mod.mswm, regime = 1,  which = 3)
plotDiag(mod.mswm, regime = 2,  which = 2)
plotDiag(mod.mswm, regime = 2,  which = 3)
## The model detect perfectly the periods of each state. The residuals look like to be white noise and they fit to the Normal Distribution. Moreover, the autocorrelation has disappeared.

plotProb(mod.mswm,which=1)
plotProb(mod.mswm,which=2)





# Example 3-traffic (Poisson)

# Daily traffic caualties by car accidents in Spain
# 
# The traffic data contains the daily number of deaths in traffic accidents
# in Spain during the year 2010, the average daily temperature and the daily sum
# of precipitations. The interest of this data is to study the relation between
# the number of deaths with the climate conditions.

data(traffic)
class(traffic)
str(traffic)

# check the overall data distribution
plot(ts(traffic[, 2:4]))
# fit a poisson regression on NDead using Temp and Prec as predictors
pois.fit = glm(NDead ~ Temp + Prec, data = traffic, family = 'poisson')
summary(pois.fit)

# The normality of the residual
par(mfrow = c(1, 1))
qqnorm(resid(pois.fit))
qqline(resid(pois.fit), col = 'red')

# Check the ACF of the residual
par(mfrow = c(1, 2))
acf(resid(pois.fit))
pacf(resid(pois.fit))

# fit a markov-switching model
m1 = msmFit(pois.fit, k = 2, sw = c(T, T, T), family = 'poisson', control = list(parallel = F))
summary(m1)


# derive the intervals for coefficients
intervals(m1)

# Check the Pearson residual 
par(mfrow = c(1, 1))
plot(m1)

## Generate dianostic plots for the residuals
## overall residual
plotDiag(m1, which = 2) # departure from normality. it's ok. GLM does not need to meet the normality assumption
par(mfrow = c(105, 4))
plotDiag(m1, which = 3) # No more patterns left in the residual

## check residuals for the two regimes

## residual for regime 1
plotDiag(m1, regime = 1,  which = 2)
plotDiag(m1, regime = 1,  which = 3)
## residual for regime 2
plotDiag(m1, regime = 2,  which = 2)
plotDiag(m1, regime = 2,  which = 3)

plotProb(m1,which=2)
plotProb(m1,which=3)




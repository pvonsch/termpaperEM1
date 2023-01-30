################################################################################ 
                            ## Script Description ## 
################################################################################
## Name: Amanda DeWitt, Michael Littunen, Philip von Scheele
## Email: amanda.dewitt@hotmail.com, micke.littunen@gmail.com, 
## philip.vonsch@hotmail.com
## Date: 14-01-2022
## Title: Empirical Methods 1, Term Paper
## Data package: Ecdat, Extramarital Affairs by Fair
## Other packages used:  cowplot, ggplot2, stargazer, lmtest, sandwich
## The version of the software and packages you use: cowplot_1.1.1, 
## ggplot2_3.3.5, stargazer 5.2.2, lmtest 0.9-39, and sandwich 3.0-1
################################################################################

################################################################################
                                ## PREAMBLE ## 
################################################################################
## The script manipulates the data by creating relevant dummy-variables and uses 
## these in regressions for the purpose of answering our research question. 
## The script is structures as such: installs necessary packages, attaches the 
## data, and then goes on to manipulate and use the data. Firstly it creates the 
## dummys and some descriptive ## statistics to then proceed to run regression 
## models.
################################################################################

################################################################################
                                ## Purpose ## 
################################################################################ 
## RQ:Is there a correlation between marital happiness and extramarital affairs?
################################################################################

################################################################################
                                  ## DATA ## 
################################################################################ 
## Source: Cross-sectional data.
## Data collection: Survey data collected by Psychology today in 1969 and then 
## manipulated and changed by R. Fair. Fair had to remove several observations 
## from the original survey due to incomplete questions and other factors.
## Sample selection. After Fair created this data set by sorting through the 
## survey data he ##ended up with 601 viable observations 
## (Psychology today originally had ~20'000).
################################################################################

################################################################################
                    ## Variables used in your application ## 
################################################################################ 
## sex (male, female), 
## age, 
## ym (numbers of years married), 
## child (yes, no), 
## religious (1 = not religious at all, 5 = very religious), 
## education (num. years), 
## occupation (reverse hollingshead classification), 
## rate (happiness rating of marriage, 1 = very unhappy, 5 = very happy)
## nbaffairs (number of affairs in the past year).
################################################################################ 


################################################################################
## Start by setting you working directory
setwd("~/Skola/R grejer")
################################################################################

################################################################################
                        ##Intsalling all packages##
################################################################################

##installs data packages needed for this term paper
install.packages("Ecdat")

##installs the necessary packages used for plotting graphs.
install.packages("cowplot")
install.packages("ggplot2")
install.packages("margins")
install.packages("stargazer")

## installs packages necessary for doing the robust SE
install.packages("lmtest")
install.packages("sandwich")

################################################################################

##The data package used in this term paper
data(Fair, package = "Ecdat")

##Attaches data package for easier access
attach(Fair)

##Shows working directory
getwd()

##Lists files in the working directory 
list.files()
############################################################

################################################################################ 
                            ## Data management ## 
################################################################################ 
## logit_plot takes a generalised linear model and plots the fitted logit
## function together with the fitted values from the generalised linear model. 
## Each data point is ranked (sorted) using the logit model and shown in the 
## plot such that coordinate (x,y)=(rank,probability) where 
## Source: "Logistic Regression in R, Clearly Explained!!!!", 
## StatQuest with Josh Starmer, https://www.youtube.com/watch?v=C4N3_XJJ-jU


## We rewrite the nbaffairs data to a dummy-variable where "0" is no affair in 
## the past year and "1" means that at least one affair occurred in the past
## year.
affairs = (nbaffairs > 0)*1

## Choosing the columns where nbaffairs>0. 
## nbaffairs are ordered by size and therefore those who cheated "0" times were 
## listed first in the data set. This allows us to 
## select those who have participated in extramarital affairs by excluding the 
## first 451 rows.
cheaters = Fair[452:601,]

################################################################################
                          ## Descriptive statistics ## 
################################################################################ 
## creating a table over the new variable "affairs" in order to count the total 
## number of people who engaged in extramarital affairs.
affair_table = table(affairs>0, rate)

## Sum of observations where nbaffairs > 0
sum(affair_table [2,])
## [150]

## x-axis: marial happiness rate, 
## y-axis: Number of people with marital happiness x who have cheated
## divide by number of people with marital happiness x who have not cheated.
plot(affair_table[2,]/affair_table[1,], xlab = "happiness rate", ylab = 
       "Num. cheaters/num non-cheaters")

################################################################################
            ## A few graphs and a summary of the models used ##
################################################################################
hist(religious)
hist(ym)
hist(rate)
## These plots above show that those parameters are not normally distributed. 
## For example, the mean of rate is 3.93, and one third of all people surveyed 
## had been married for 15 years or longer. 
## These could indicate that the data has been surveyed in specific groups, 
## but it is also possible ##that this is true for the whole population.

################################################################################
                                ## Estimations ## 
################################################################################

################################################################################
                                  ## Model 1 ##
################################################################################

## Simple linear regression to establish correlation between extramarital 
## affairs and their marital happiness rate
mod1=lm(affairs ~rate)

## Calls the regression and allows us to inspect the coefficients
mod1

## Shows us a summary of the model and the p-value for our chosen variable
summary(mod1)

## Shows a summary regarding F-value for mod1
anova(mod1)

################################################################################

################################################################################
                    ## Model 1: Robust standard errors ##
################################################################################

## Simple linear regression to establish correlation between extramarital 
## affairs and their marital happiness rate
library(lmtest)
library(sandwich)
Robust.mod1 <- (lm (affairs ~ rate))

coeftest(Robust.mod1)
coeftest(Robust.mod1, vcov=(vcovHC(Robust.mod1,"HC1")))

## Calls the regression and allows us to inspect the coefficients
Robust.mod1

## Shows us a summary of the model and the p-value for our chosen variable
summary(Robust.mod1)

## Showsa  summary regarding F-value for Robust.mod1
anova(Robust.mod1)

## Null hypothesis:
## f?1 = 0, where f?1 is the coefficient for the marital rate

## since f?1 =/= 0 we can reject the null hypothesis. These results mean that 
## marital happiness rate does affect the probability of extramarital affairs.

################################################################################

################################################################################
                                ## Model 2 ##
################################################################################

## multivariable linear regression to establish correlation between extramarital 
## affairs, years married, self-rated religiousness, and marital happiness
mod2=lm(Fair$nbaffairs ~  Fair$ym+  Fair$religious+ Fair$rate)

## Shows us a summary of the model and the p-value for our chosen variables
summary(mod2)

################################################################################

################################################################################
                    ## Model 2: Robust standard errors ##
################################################################################

## multivariable linear regression to establish correlation between extramarital 
## affairs, years married, self-rated religiousness, and marital happiness
Robust.mod2 <- (lm(nbaffairs ~ ym + religious + rate))

coeftest(Robust.mod2)
coeftest(Robust.mod2, vcov=(vcovHC(Robust.mod2,"HC1")))

## Calls the regression and allows us to inspect the coefficients
Robust.mod2

## Shows us a summary of the model and the p-value for our chosen variables
summary(Robust.mod2)

## Shows a summary regarding F-valuea for Robust.mod2
anova(Robust.mod2)

## Null hypothesis:
## H0: β1 = β2 = β3 = 0, where 
## β1 = the coefficiant for ym
## β2 = the coefficiant for religious
## β3 = the coefficiant for rate

## Since the F-values > 0 and f?1 =/= f?2 =/= f?3, we can reject the null
## hypothesis.
## All of these independent variables affect the number of affairs had in 
## the last year.

################################################################################

################################################################################
                                ## Model 3 ##
################################################################################

## multivariable logit regression to establish correlation between extramarital 
## affairs, years married, self-rated religiousness, and marital happiness
## Uses the binomial distribution since the independent variable maps the 
## dependent variable to 0 or 1. 
mod3 = glm(affairs ~ ym+ religious + rate, family = "binomial") 

library(margins)
margins(mod3)
summary(margins(mod3))

## Shows us a summary of the model and the p-values for our chosen variables
summary(mod3)

################################################################################

################################################################################
                               ## Ignored model ##
################################################################################

## This model includes all of the variables in the daata set, as well as factors
## occupation into different groups. This model is "ignored" by us due to the
## added variables not adding anything of value to the model.
## The exception is age which is statistically significant but less so than the 
## variables in model 2, and it is closely related to ym, meaning they might
## overlap.
ignored_model = lm(nbaffairs ~ sex + age + ym +child +religious+ education+ 
                     factor(occupation) + rate)

## Shows us a summary of the model and the p-values for our chosen variables
summary(ignored_model)

################################################################################

################################################################################
                          ## More graphs and visuals ##
################################################################################

## Loads the ggplot2 and cowplot libraries


## creates a graph visualizing the connection between the independent variables
## and the probability of extramarital affairs, as well as if they actually have 
## reported to have engaged in such an affair
library(ggplot2)
library(cowplot)
logit_plot <- function(regression){
  ## List containing the probability of individual i according to logit model
  affair_probability= regression$fitted.values
  ## Data frame (basically a higher dimensional list) containing 
  ## affair_probability together with their corresponding 0 or 1 from the 
  ## affairs variable
  regressed_values = data.frame(affair_probability, affairs)
  ## Ranks the regressed_values data frame such that the person with lowest 
  ## probability of cheating is first
  ordered_regressed_values = regressed_values[order(regressed_values$affair_probability, decreasing = FALSE),]
  ## Gives each person its rank
  ordered_regressed_values$rank = 1:nrow(ordered_regressed_values)
  ## Draws the plot, as explained before. aes takes the x- and y-coordinates.
  ggplot(data = ordered_regressed_values, aes(x=rank, y = affair_probability)) + geom_point(aes(color = factor(affairs)), alpha = 0.9, shape = 0, stroke = 1)+labs(x = "Rank", y = "Probability of cheating", colour = "Cheating")}

logit_plot(mod3)

## A table to summarise the results from our model
library(stargazer)
stargazer(mod1,mod2,mod3,type="text",
          title="Regression results from the three models")

################################################################################
                    ## Interpretation of your results ## 
################################################################################ 
## Interpretation of the results you obtained from your estimations. 

## Model 1
## In our first model, we use a simple linear regression to test the effect of 
## marital happiness on the prevalence of extramarital affairs. 
## The resulting b = -0.09965 tells us that marital happiness is negatively 
## correlated with extramarital affairs. This lines up with our expectations 
## that the happier someone is in their marriage, the less likely they are to be 
## unfaithful. It could also be interpreted as a marriage becoming less happy in 
## light of an affair. Correlation =/= causation.
## The residuals are however not perfectly normally distributed but our p-values 
## suggest that the result is statistically significant. Our results for R2 
## equals  0.06442 and 0.06286 for our adjusted R2, this is then used to compare 
## model 1 with model 2 and 3 to see if we get an improvement in our fit.

##Model 2
## In model 2 we expand on model 1 by adding multiple variables. These include 
## number of years married (b1), degree of religiousness (b2) and marital 
## happiness (b3).  We find that a = 8.09626 , b1 = 0.21789, b2 = -0.69994, 
## b3 = -0.67969. Our results suggest that our findings from model 1 hold and 
## that marital happiness still has a large effect on the probability of 
## extramarital affairs, or the reverse. Aside from that we also find that a 
## higher degree of religiousness is negatively correlated with extramarital 
## affairs and that years married has a positive correlation with the prevalence
## of extramarital affairs. We find statistically significant p-values for all 
## of the variables and also get an improved adjusted R2 of 0.1268, suggesting 
## that this model is better at explaining the interactions between our 
## variables and risk of extramarital affairs.

##Model 3
## Model 3 is a logit regression instead of a linear one. Through this we 
## map the dependent variable to the interval (0,1), thus allowing us to 
## interpret it as a probability. 
## Using the same variables as in model 2, we find that a= 1.13820, b1= 0.05545, 
## b2= -0.33065 ##and b3= -0.45332. This result shows that the variables affect 
## a in the same direction as in ##model 2, although the largest effect now 
## comes from marital happiness. 
## The resulting log(odds) then essintally allows us to determine the risk of an 
## extramarital affair having occured given  ##information about these variables 
## for an individual. As in the previous models we get statistically
## significant p-values.

################################################################################

################################################################################
                                ## References ## 
################################################################################

## authors: Atkins, D. C., Baucom, D. H., & Jacobson, N. S.
## year: 2001 
## title: Understanding infidelity: Correlates in a national random sample
## Journal of Family Psychology, Vol. 15, No. 4, pp. 735- 749

## author: Fair, R. C
## year: 1978 
## title: A Theory of Extramarital Affairs
## Yale University,  Journal of political economy, Vol. 86, No. 1, pp. 45-61.

## authors: Infidelity recovery institute
## year: 2020 
## title:  U.S.A. Laws on Infidelity and Adultery
## https://infidelityrecoveryinstitute.com/u-s-a-laws-on-infidelity-and-adultery/, 
## [accessed Nov 22nd 2021].

## authors: Shackelford, T. K., Besser, A., & Goetz A. T.
## year: 2008 
## title: Personality, Marital Satisfaction, and Probability of Marital Infidelity
## Individual Differences Research, Vol. 6, No. 1, ISSN: 1541-745X, pp. 13-25.

################################################################################

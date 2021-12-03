#Assignment PART 2 - Roberta Lopes Pereira


library(psych) # for describe	
library(lm.beta) # for lm.beta	
library(gridExtra) # for grid.arrange
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy format


# custom functions to get bootstrapped confidence intervals.	

# function to obtain regression coefficients	
# source: https://www.statmethods.net/advstats/bootstrapping.html	
bs_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(coef(fit)) 	
}	

# function to obtain adjusted R^2	
# source: https://www.statmethods.net/advstats/bootstrapping.html (partially modified)	
adjR2_to_boot <- function(model, data, indices) {	
  d <- data[indices,] # allows boot to select sample 	
  fit <- lm(formula(model), data=d)	
  return(summary(fit)$adj.r.squared)	
}	


# Computing the booststrap BCa (bias-corrected and accelerated) bootstrap confidence intervals by Elfron (1987)	
# This is useful if there is bias or skew in the residuals.	

confint.boot <- function(model, data = NULL, R = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  boot.ci_output_table = as.data.frame(matrix(NA, nrow = length(coef(model)), ncol = 2))	
  row.names(boot.ci_output_table) = names(coef(model))	
  names(boot.ci_output_table) = c("boot 2.5 %", "boot 97.5 %")	
  results.boot = results <- boot(data=data, statistic=bs_to_boot, 	
                                 R=1000, model = model)	
  
  for(i in 1:length(coef(model))){	
    boot.ci_output_table[i,] = unlist(unlist(boot.ci(results.boot, type="bca", index=i))[c("bca4", "bca5")])	
  }	
  
  return(boot.ci_output_table)	
}	

# Computing the booststrapped confidence interval for a linear model using wild bottstrapping as descibed by Wu (1986) <doi:10.1214/aos/1176350142>	
# requires the lmboot pakcage	

wild.boot.confint <- function(model, data = NULL, B = 1000){	
  if(is.null(data)){	
    data = eval(parse(text = as.character(model$call[3])))	
  }	
  
  wild_boot_estimates = wild.boot(formula(model), data = data, B = B)	
  
  result = t(apply(wild_boot_estimates[[1]], 2, function(x) quantile(x,probs=c(.025,.975))))	
  
  return(result)	
  
}	
#
# This is a custom function that I wrote which helps in creating the final table for the regression coefficients.	


coef_table = function(model){	
  require(lm.beta)	
  mod_sum = summary(model)	
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))		
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))		
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"		
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model), confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])), 2)), mod_sum_p_values)		
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")		
  mod_sum_table["(Intercept)","Std.Beta"] = "0"		
  return(mod_sum_table)	
}	

#

data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")

#excluding incorrect data and outliers

data_sample_1_correct <- data_sample_1 %>% 
  slice(-c(88, 34))

data_sample_1_correct = data_sample_1_correct %>%
  slice(-c(64, 84, 102))

data_sample_1_correct = data_sample_1_correct %>% 
  mutate(sex = factor(sex))

data_sample_1_correct %>% 
  summary()

#Backward model

model1 = lm(pain ~  age + sex + STAI_trait + pain_cat + mindfulness + 
              cortisol_serum + weight + IQ + household_income, data = data_sample_1_correct)

summary(model1)

#Model diagnostics

#weight
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = weight, y = pain, label = rownum) +
  geom_label()

#IQ
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = IQ, y = pain, label = rownum) +
  geom_label()

#household-income
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = household_income, y = pain, label = rownum) +
  geom_label()

#Cook's distance
model1 %>%
  plot(which = 5)

model1 %>%
  plot(which = 4)

#normality
model1 %>%
  plot(which = 2)

residuals_model1 = enframe(residuals(model1))
residuals_model1 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model1))

#Linearity
model1 %>%
  residualPlots()

#Homoscedasticty
model1 %>%
  plot(which = 3)

model1 %>%
  ncvTest()

model1 %>%
  bptest()

#No multicollinearity
model1 %>%
  vif()

#Diagnostics ok - assumptions not violated

backward_reg_model = step(model1, direction = "backward")

summary(backward_reg_model)

backward_model = lm(pain ~ age + pain_cat + mindfulness + cortisol_serum, 
                    data = data_sample_1_correct)
summary(backward_model)  

theory_based_model = lm(pain ~  age + sex + STAI_trait + pain_cat + mindfulness + 
              cortisol_serum, data = data_sample_1_correct)
summary(theory_based_model)  

AIC(backward_model)
AIC(theory_based_model)
AIC(model1)

summary(model1)

anova(backward_model, theory_based_model)
anova(backward_model)

coef_table(backward_model)

#new data sample
data_sample_2 = read.csv("https://tinyurl.com/87v6emky")

#Calculation of predicted values
pred_test_theory <- predict(theory_based_model, data_sample_2)
pred_test_backward <- predict(backward_model, data_sample_2)

pred_test_theory

pred_test_backward

#Calculation of the sum of squared residuals
RSS_test_theory = sum((data_sample_2[, "pain"] - pred_test_theory)^2)
RSS_test_backward = sum((data_sample_2[, "pain"] - pred_test_backward)^2)

RSS_test_theory
RSS_test_backward

anova(backward_model, model1)


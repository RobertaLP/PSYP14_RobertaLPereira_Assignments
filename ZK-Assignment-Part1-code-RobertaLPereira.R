#Assignment PART 1 - Roberta Lopes Pereira


library(psych) # for describe	
library(lm.beta) # for lm.beta	
library(gridExtra) # for grid.arrange
library(car) # for residualPlots, vif, pairs.panels, ncvTest
library(lmtest) # bptest
library(sandwich) # for coeftest vcovHC estimator
library(boot) # for bootstrapping
library(lmboot) # for wild bootsrapping
library(tidyverse) # for tidy format
library(apaTables)


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

#Data exploration
data_sample_1 %>% 
  summary()

data_sample_1 %>%	
  ggplot() +	
  aes(x = sex) +	
  geom_bar()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain) +	
  geom_histogram()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait) +	
  geom_histogram()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = age) +	
  geom_histogram()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat) +	
  geom_histogram()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum) +	
  geom_histogram()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva) +	
  geom_histogram()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness) +	
  geom_histogram()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = age, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = sex, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point()

data_sample_1 %>% 	
  ggplot() +	
  aes(x = cortisol_saliva, y = pain) +	
  geom_point()

#excluding incorrect data
data_sample_1_correct <- data_sample_1 %>% 
  slice(-c(88, 34))

data_sample_1_correct = data_sample_1_correct %>% 
  mutate(sex = factor(sex))
  
data_sample_1_correct %>% 
  summary()

data_sample_1_correct %>% 	
  ggplot() +	
  aes(x = sex, y = pain) +	
  geom_point()


data_sample_1_correct %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain) +	
  geom_point() +
  geom_smooth(method = "lm")

#Models

model1 = lm(pain ~  age + sex, data = data_sample_1_correct)
summary(model1)


model2 = lm(pain ~  age + sex + STAI_trait + pain_cat + mindfulness + 
              cortisol_serum + cortisol_saliva, data = data_sample_1_correct)
summary(model2)

AIC(model1)

AIC(model2)

confint(model2)

lm.beta(model2)

#Model diagnostics

#STAI_trait
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) +
  geom_point() + geom_text()

data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = rownum) +
  geom_label()

#cortisol_saliva
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = cortisol_saliva, y = pain, label = rownum) +
  geom_label()

#cortisol_serum
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = cortisol_serum, y = pain, label = rownum) +
  geom_label()

#pain_cat
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = pain_cat, y = pain, label = rownum) +
  geom_label()

#mindfulness
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = mindfulness, y = pain, label = rownum) +
  geom_label()

#age
data_sample_1_correct %>%
  mutate(rownum = row.names(data_sample_1_correct)) %>%
  ggplot() + aes(x = age, y = pain, label = rownum) +
  geom_label()

#residual leverage plot
data_sample_1_correct %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() + 
  geom_smooth(method = "lm")

#Cook's distance
model2 %>%
  plot(which = 5)

model2 %>%
  plot(which = 4)

data_sample_1_correct %>%
  slice(c(46, 73, 85))

#normality
model2 %>%
  plot(which = 2)

residuals_model2 = enframe(residuals(model2))
residuals_model2 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2))

#Excluding outliers
data_sample_1_correct_nooutliers = data_sample_1_correct %>%
  slice(-c(64, 84, 102))

model3 = lm(pain ~  age + sex + STAI_trait + pain_cat + mindfulness + 
              cortisol_serum + cortisol_saliva, data = data_sample_1_correct_nooutliers)

summary(model2)
summary(model3)

model3 %>%
  plot(which = 2)

residuals_model3 = enframe(residuals(model3))
residuals_model3 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model3))

#residual leverage plot
data_sample_1_correct_nooutliers %>%
  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() + 
  geom_smooth(method = "lm")

#Cook's distance
model3 %>%
  plot(which = 5)

model3 %>%
  plot(which = 4)

summary(model3)

AIC(model3)

######
#outliers test 2
#data_sample_1_correct_nooutlierstest2 = data_sample_1_correct %>%
# slice(-c(64, 73, 84, 102))

#modeltest2 = lm(pain ~  age + sex + STAI_trait + pain_cat + mindfulness + 
#              cortisol_serum, data = data_sample_1_correctnooulierstest2)

#modeltest2 = lm(pain ~  age + sex, data = data_sample_1_correct_nooutlierstest2)
                  
#summary(modeltest2)
#summary(model4)

#modeltest2 %>%
#  plot(which = 2)

#residuals_modeltest2 = enframe(residuals(modeltest2))
#residuals_modeltest2 %>%
#  ggplot() + aes(x = value) + geom_histogram()

#describe(residuals(modeltest2))

#residual leverage plot
#data_sample_1_correct_nooutlierstest2 %>%
#  ggplot() + aes(x = STAI_trait, y = pain) + geom_point() + 
#  geom_smooth(method = "lm")

#Cook's distance
#modeltest2 %>%
#  plot(which = 5)

#modeltest2 %>%
#  plot(which = 4)
#######


#Linearity
model3 %>%
  residualPlots()

#Homoscedasticty
model3 %>%
  plot(which = 3)

model3 %>%
  ncvTest()

model3 %>%
  bptest()

#No multicollinearity
model3 %>%
  vif()

#New model without multicollinearity
model4 = lm(pain ~  age + sex + STAI_trait + pain_cat + mindfulness + 
              cortisol_serum, data = data_sample_1_correct_nooutliers)

summary(model4)

describe(residuals(model4))

model1_nooutliersdataset = lm(pain ~  age + sex, data = data_sample_1_correct_nooutliers)

summary(model1_nooutliersdataset)

describe(residuals(model1_nooutliersdataset))

AIC(model1_nooutliersdataset)
AIC(model4)

confint(model4)

lm.beta(model4)

#Re-run Diagnostic for model 4
#normality
model4 %>%
  plot(which = 2)

residuals_model4 = enframe(residuals(model4))
residuals_model4 %>%
  ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model4))

model4 %>%
  plot(which = 5)

model4 %>%
  plot(which = 4)

#Linearity
model4 %>%
  residualPlots()

#Homoscedasticty
model4 %>%
  plot(which = 3)

model4 %>%
  ncvTest()

model4 %>%
  bptest()

#No multicollinearity
model4 %>%
  vif()


#Model comparison
summary(model1_nooutliersdataset)
summary(model4)

describe(residuals(model1_nooutliersdataset))
describe(residuals(model4))

anova(model1_nooutliersdataset, model4)

AIC(model1_nooutliersdataset)
AIC(model4)

confint(model4)

lm.beta(model4)

coef_table(model1_nooutliersdataset)
coef_table(model4)

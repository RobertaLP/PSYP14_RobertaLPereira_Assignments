#Assignment PART 3 - Roberta Lopes Pereira

library(psych) # for describe\t
library(tidyverse) # for tidy code and ggplot\t
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM


# ## Custom function	
stdCoef.merMod <- function(object) {	
  sdy <- sd(getME(object,"y"))	
  sdx <- apply(getME(object,"X"), 2, sd)	
  sc <- fixef(object)*sdx/sdy	
  se.fixef <- coef(summary(object))[,"Std. Error"]	
  se <- se.fixef*sdx/sdy	
  return(data.frame(stdcoef=sc, stdse=se))	
}	


data_file_3 = read_csv("https://tinyurl.com/b385chpu")

data_file_4 = read_csv("https://tinyurl.com/4f8thztv")

data_file_3 = data_file_3 %>% 	
  mutate(hospital = factor(hospital))	

data_file_3 = data_file_3 %>% 	
  mutate(sex = factor(sex))	

data_file_4 = data_file_4 %>% 	
  mutate(hospital = factor(hospital))	

data_file_4 = data_file_4 %>% 	
  mutate(sex = factor(sex))	

data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum) +		
  geom_point(aes(color = hospital), size = 4) +		
  geom_smooth(method = "lm", se = F)		

int_plot = data_file_3 %>% 		
  ggplot() +		
  aes(y = pain, x = cortisol_serum, color = hospital) +		
  geom_point(size = 2) +		
  geom_smooth(method = "lm", se = F, fullrange=TRUE)		

int_plot	

#Randon intercept model

model_rnd_int = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + 
                     cortisol_serum + (1|hospital), data = data_file_3)		

summary(model_rnd_int)

confint(model_rnd_int)

stdCoef.merMod(model_rnd_int)

# marginal R squared with confidence intervals	
r2beta(model_rnd_int, method = "nsj", data = data_file_3)	

# marginal and conditional R squared values	
r.squaredGLMM(model_rnd_int)	


#Test on data file 4
pred_test_rnd_intercept <- predict(model_rnd_int, data_file_4, allow.new.levels = TRUE)

pred_test_rnd_intercept


#Calculation of the sum of squared residuals
#RSS
RSS_test_rnd_intercept = sum((data_file_4[, "pain"] - pred_test_rnd_intercept)^2)

RSS_test_rnd_intercept

#TSS
mod_mean <- lm(pain ~ 1, data = data_file_4)

TSS = sum((data_file_4[, "pain"] - predict(mod_mean))^2)
TSS

#R2
R2_test_datafile4 = 1 - (RSS_test_rnd_intercept/TSS)
R2_test_datafile4


#Randon slope model

mod_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum|hospital),
                     data = data_file_3)
summary(mod_rnd_slope)

#Regression lines of the Random slope model
data_file_3 = data_file_3 %>%
  mutate(pred_slope = predict(mod_rnd_slope))

data_file_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 3) + 
  geom_line(color = "red", aes(y = pred_slope, x = cortisol_serum)) + 
  facet_wrap(~hospital, ncol = 2)



#other explorations
#data_file_3 = data_file_3 %>%
 # mutate(pred_int2 = predict(model_rnd_int))

#data_file_3 %>%
 # ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  #geom_point(aes(color = hospital), size = 3) + 
  #geom_line(color = "red", aes(y = pred_int2, x = cortisol_serum)) + 
  #facet_wrap(~hospital, ncol = 2)

#comparison
#sum(residuals(model_rnd_int)^2)
#sum(residuals(mod_rnd_slope)^2)

#cAIC(model_rnd_int)$caic
#cAIC(mod_rnd_slope)$caic
#anova(mod_rnd_slope, model_rnd_int)
# marginal and conditional R squared values	
#r.squaredGLMM(model_rnd_int)
#r.squaredGLMM(mod_rnd_slope)	


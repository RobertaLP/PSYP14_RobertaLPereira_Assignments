#Assignment GP Part1 - Roberta Lopes Pereira

library(tidyverse)

#Data exploration and organization of the data table - variables
#Original Data set: PAQ_Roberta

id <- PAQ_Roberta[c(1:300), 1]
sex <- PAQ_Roberta[c(1:300), 3]
age <- PAQ_Roberta[c(301:600), 3]
Q1_cry <- PAQ_Roberta[c(601:900), 3]
Q2_help <- PAQ_Roberta[c(901:1200), 3]
Q3_breathe <- PAQ_Roberta[c(1201:1500), 3]
Q4_freeze <- PAQ_Roberta[c(1501:1800), 3]
Q5_alien <- PAQ_Roberta[c(1801:2100), 3]
Q6_inferior <- PAQ_Roberta[c(2101:2400), 3]
Q7_weep <- PAQ_Roberta[c(2401:2700), 3]
Q8_support <- PAQ_Roberta[c(2701:3000), 3]
Q9_nerd <- PAQ_Roberta[c(3001:3300), 3]

PAQdata <- data.frame(Q1_cry, Q2_help, Q3_breathe, Q4_freeze, 
                      Q5_alien, Q6_inferior, Q7_weep, Q8_support, Q9_nerd)

summary(PAQdata)

PAQdata <- PAQdata %>% 	
  filter(!is.na(Q5_alien))

PAQdata <- PAQdata %>% 	
  filter(!is.na(Q9_nerd))


#Means and standard deviations
summary(PAQdata)

colMeans(PAQdata)

SD_Qs = data.frame(sd(PAQdata$Q1_cry), sd(PAQdata$Q2_help),
                   sd(PAQdata$Q3_breathe),
                   sd(PAQdata$Q4_freeze),
                   sd(PAQdata$Q5_alien),
                   sd(PAQdata$Q6_inferior),
                   sd(PAQdata$Q7_weep),
                   sd(PAQdata$Q8_support),
                   sd(PAQdata$Q9_nerd))
SD_Qs

sum(SD_Qs)/9


#barplot(meanQ) #do a plot for mean and sd?
barplot(colMeans(PAQdata)) 

#Correlation matrix
data_pcacor = princomp(PAQdata,cor=TRUE) 

summary(data_pcacor, loadings=TRUE)

#data_pcacov = princomp(PAQdata,cor=FALSE)
#summary(data_pcacov, loadings=TRUE)


#Plots
plot(data_pcacor, main = 'Variances Explained by Components')

barplot(data_pcacor$scores[,1])

plot(data_pcacor$scores[,1])


#Scree
plot(data_pcacor$sdev^2, xlab = "Component number", 
     ylab = "Component variance", type = "l", main = "Scree diagram")

#Log eigenvalue diagram
plot(log(data_pcacor$sdev^2), xlab = "Component number", ylab = "log(Component variance)", type="l",
      main = "Log(eigenvalue) diagram")


#Biplot

biplot(data_pcacor, col = c("gray", "black"))

biplot(data_pcacor)


#Plots and Biplot from factoextra package
library(factoextra)

# Percent of variance explained:
fviz_eig(data_pcacor) 

fviz_contrib(data_pcacor, choice = "var", axes = 1) 

fviz_contrib(data_pcacor, choice = "var", axes = 2)

fviz_contrib(data_pcacor, choice = "var", axes = 3)

fviz_pca_var(data_pcacor, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                                                        "#FC4E07"), repel = TRUE)

fviz_pca_biplot(data_pcacor, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", 
                           
                                                                                                             "#FC4E07"), repel = TRUE)
fviz_pca_biplot(data_pcacor)
fviz_pca(data_pcacor)


plot(data_pcacor$scores[,1],data_pcacor$scores[,2])



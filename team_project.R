# Main Team Project Script
# Authors: Youssef Emam, Hasan Abdo, Angela Bakaj
# Date: Aug. 12, 2024
# to run: set wd to main repo page (Solivana_DS1_Final folder)

eda <- function(sleep_data){
  #summarize mean age by gender
  sleep_data.age <- sleep_data %>% 
    group_by(Gender) %>%
    summarise(mean_age = mean(Age, na.rm = T))
  
  ggplot(data = sleep_data.age, mapping = aes(x = Gender, y= mean_age))+
    geom_col()
  
  #summarize BMI by gender
  sleep_data.BMI <- sleep_data %>% 
    group_by(Gender) %>%
    summarise(mean_BMI = mean(BMI, na.rm = T))
  
  ggplot(data = sleep_data.BMI, mapping = aes(x = Gender, y= mean_BMI))+
    geom_col()
}

#initialize libaries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(mice)

#import the data
sleep_data <- read.csv("datasets/project_data.csv", header = T)

names(sleep_data)

#clean the data

#select relevant columns
sleep_data <- sleep_data %>% 
  select(Subject,
         Gender,
         Age,
         BMI,
         Time.from.transplant,
         Liver.Diagnosis,
         Recurrence.of.disease,
         Rejection.graft.dysfunction,
         Any.fibrosis,
         Renal.Failure,
         Depression,
         Corticoid,
         Epworth.Sleepiness.Scale,
         Pittsburgh.Sleep.Quality.Index.Score,
         Athens.Insomnia.Scale,
         Berlin.Sleepiness.Scale,
         SF36.PCS,
         SF36.MCS)

#Quick glimplse of the data
summary(sleep_data)

#convert categorical data to factors
cols <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression",  "Corticoid", "Berlin.Sleepiness.Scale")

#convert the specified columns to factors
sleep_data[cols] <- lapply(sleep_data[cols], as.factor)

#look at the unique values in each categorical field
# make sure there is nothing there that shouldnt be there
apply(sleep_data[cols], 2, unique)

#identify missingness in the data
tab <- apply(ifelse(is.na(sleep_data), "Missing", "Not Missing"), 2, table)

#proportion tables for missingness (True = not missing, false = missing)
lapply(tab, prop.table)

# PSS has 30% missingness, may make sense to exclude from the analysis
sleep_data <- sleep_data %>% 
  select(!Pittsburgh.Sleep.Quality.Index.Score)

#See if we can identify a mechanism of missingness
#convert NAs to true and non-NAs to false
NA_matrix <- is.na(sleep_data)

#calculate correlation matrix and p-value matrix
cor_matrix <- Hmisc::rcorr(NA_matrix)

diag(cor_matrix$r) <- 0
diag(cor_matrix$P) <- 0

cor_matrix$r[NA] <- 0
cor_matrix$P[NA] <- 0


#plot a heatmap
# Note alot of NA values in rows where SD = 0, i.e all values are complete
corrplot(cor_matrix$r, method = "square", p.mat = cor_matrix$P, insig = "blank", type = "lower", sig.level = 0.05, na.label = " ", tl.cex = 0.5)

# it seems that alot of people who did not fill out one sleep quality/QOL score, also tended not to fill out the rest


#Description of relevant data
eda(sleep_data)

#===============Imputation===================#
# we need to do something to deal with the missing data

  
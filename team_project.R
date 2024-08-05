# Main Team Project Script
# Authors: Youssef Emam, Hasan Abdo, Angela Bakaj
# Date: Aug. 12, 2024
# to run: set wd to main repo page (Solivana_DS1_Final folder)

#initialize libaries
library(tidyverse)
library(ggplot2)

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


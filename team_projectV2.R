# Main Team Project Script
# Authors: Youssef Emam, Hasan Abdo, Angela Bakaj
# Date: Aug. 12, 2024
# to run: set wd to main repo page (Solivana_DS1_Final folder)
# This file requires functions.R to be run before in order to run properly
# The functions.R file is sourced below, but make sure this file and the functions file are both together in the working directory
# The tables.R file just creates some of the tables used in the report
# To run tables.R ensure that this file and functions.R have been run completely


#initialize libaries
library(tidyverse)
library(ggplot2)
library(corrplot)
library(Hmisc)
library(misty)
library(mice)
library(glue)
library(gtsummary)
library(gt)
library(car)
library(patchwork)

#load functions
source("functions.R")

#import the data
sleep_data <- read.csv("datasets/project_data.csv", header = T)

#### Data Cleaning ####

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

#Quick glimpse of the data
summary(sleep_data)

#convert categorical data to factors
cols <- c("Gender", "Liver.Diagnosis", "Recurrence.of.disease", "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression",  "Corticoid", "Berlin.Sleepiness.Scale")

#convert the specified columns to factors
sleep_data[cols] <- lapply(sleep_data[cols], as.factor)

#look at the unique values in each categorical field
# make sure there is nothing there that shouldnt be there
apply(sleep_data[cols], 2, unique)

#Scale goes from 0-24, max should not be 26
summary(sleep_data$Epworth.Sleepiness.Scale)
#remove any values greater than 24
sleep_data$Epworth.Sleepiness.Scale[sleep_data$Epworth.Sleepiness.Scale > 24]<- NA

#identify missingness in the data
tab <- apply(ifelse(is.na(sleep_data), "Missing", "Not Missing"), 2, table)

#proportion tables for missingness (True = not missing, false = missing)
Missingness <- lapply(tab, prop.table)
Missingness_df <- sapply(Missingness, cbind)

# PSQI has 30% missingness, may make sense to exclude from the analysis
sleep_data <- sleep_data %>% 
  select(!Pittsburgh.Sleep.Quality.Index.Score)

#### Summary Dataframe Modifications ####
# Creating modified version of the data frame to create EDA summaries

#remove the subject field
summary_data <- sleep_data %>%
  select(-Subject)

#Rename the factors so they show up nicer in the tables
summary_data$Gender <-summary_data$Gender %>% 
  fct_recode(Male = "1", Female = "2")

summary_data$Liver.Diagnosis <-summary_data$Liver.Diagnosis %>% 
  fct_recode(HepC = "1", HepB = "2", PSC.PBC.AHA = "3", Alcohol = "4", Other = "5")

#convert binaries to yes and no
#Select cols that need to be converted
cols <- c("Recurrence.of.disease", "Rejection.graft.dysfunction", "Any.fibrosis", "Renal.Failure", "Depression", "Corticoid")

#iterate through the columns and recode the factors
for (col in cols){
  summary_data[[col]] <- summary_data[[col]] %>% 
    fct_recode(Yes = "1", No = "0")
}

#recode the names of the factors
summary_data$Berlin.Sleepiness.Scale <-summary_data$Berlin.Sleepiness.Scale %>% 
  fct_recode(Sleep.Apnea = "1", Normal = "0")


#Description of relevant data
EDAs<- eda(sleep_data)
for (plots in EDAs) {
  print(plots)
}

#### Missingness Analysis ####

#See if we can identify a correlation between missing data
# i.e if some data is missing are we likely to be missing other data?

#convert NAs to true and non-NAs to false
NA_matrix <- is.na(sleep_data)

#Create a correlation plot of missingness
# see if missingness in one category is correlated with missingness in another category
# Remove insignificant correlatons by calcualting p value for each correlation

#calculate correlation matrix and p-value matrix
cor_matrix <- Hmisc::rcorr(NA_matrix, type = "spearman" )

#set diagonals (self correlations = 0)
diag(cor_matrix$r) <- 0
diag(cor_matrix$P) <- 0

# Note alot of NA values in rows where SD = 0, i.e all values are complete
#replace any NAs with 0 so they dont show up as question marks
cor_matrix$r[NA] <- 0
cor_matrix$P[NA] <- 0


#plot a heatmap
corrplot(cor_matrix$r, method = "square", p.mat = cor_matrix$P, insig = "blank", type = "lower", sig.level = 0.05, na.label = " ", tl.cex = 0.5)

# it seems that alot of people who did not fill out one sleep quality/QOL score, also tended not to fill out the rest

#missing data pattern, doesnt seem to have monotone pattern
md.pattern(sleep_data, rotate.names = T)
fluxplot(sleep_data, labels = F)


#### Prevalence of sleep disturbance ####
# to estimate the prevalence, identify cutoffs in the data
# identify percentage of patients that have sleep disturbance according to each metric

#create a binary metric and add to the table based on cutoff values
prevalence <- sleep_data %>%
  na.omit(sleep_data) %>% 
  mutate(
  ESS = case_when(
    Epworth.Sleepiness.Scale > 10 ~ 1,
    Epworth.Sleepiness.Scale <= 10 ~ 0
  ),
  AthensSS = case_when(
    Athens.Insomnia.Scale > 5.5 ~ 1,
    Athens.Insomnia.Scale <= 5.5 ~ 0
  ),
  #convert to character first to remove factor levels, and then convert to integer
  BSS = as.integer(as.character(Berlin.Sleepiness.Scale))
) %>% 
  select(ESS, BSS, AthensSS)


#determine the prevalence

#create empty data frame
prevalences <- data.frame()
rownames(prevalences) <- NULL

#loop through the metrics, summarize them and append to empty df
for (name in names(prevalence)) {
  
  #Select relevant columns
  prevalence_temp <- prevalence %>%
    select(all_of(name))
  
  #summarize and calculate how many patients have sleep issues and how many dont
  prevalence_temp <- prevalence_temp %>%
    summarise(sleep_trouble = sum(prevalence_temp[[all_of(name)]]),
              total = n(),
              percent = sleep_trouble/total*100)
  
  #Add the data to a df
  prevalences <- base::rbind(prevalences, prevalence_temp)
  
}

#set the row names of the df
row.names(prevalences) <- c("ESS", "BSS", "AIS")

#transpose
prevalences <- as.data.frame(t(prevalences))

#format for table presentation
prevalences <- data.frame(Scale = names((prevalences)),
                          Percent = t(prevalences[3,]))



#### Imputation ####
# we need to do something to deal with the missing data
# single imputation

#we cant do it all at once, since some data are binary, some are unordered and some are cont. 
#impute continuous data with linear regression + stochastic, logistic regression + stochasticity for binary
imputed_sleep_data <- mice(sleep_data,
                           defaultMethod = c("norm.nob", "logreg", "polyreg", "polyr"),
                           m = 1,
                           print = FALSE,
                           maxit = 1,
                           seed = 10)


#check one of the imputed datasets to ensure there are no missing values
imputed_sleep_data <- complete(imputed_sleep_data, action = 1)

#NO NAs
summary(imputed_sleep_data)

#remove negatives
imputed_sleep_data$Epworth.Sleepiness.Scale[imputed_sleep_data$Epworth.Sleepiness.Scale < 0] <- 0

summary(imputed_sleep_data)

#===============Creating Models===================#

#Create models that predict sleep disturbance based on predictor variables
# Research which variables are effective predictors of each model
# Research shows that Age, Gender, BMI, Depression, and Time from Transplant are strong predictors of sleep quality

##### Linear regression model for ESS######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:

#calculate the maximum df for the model
max_predictors_ESS <- as.integer(length(sleep_data$Epworth.Sleepiness.Scale)/15)

# These are the predictors to be used based on the literature search
ESS_model <-lm(Epworth.Sleepiness.Scale ~ Gender + Age + BMI + Depression + Time.from.transplant,
               data = imputed_sleep_data)

#Try adding liver diagnosis
ESS_model2 <- lm(Epworth.Sleepiness.Scale ~ Gender + Age + BMI + Depression + Time.from.transplant + Liver.Diagnosis,
                 data = imputed_sleep_data)


#Simpler model is better
anova(ESS_model,ESS_model2)
AIC(ESS_model)
AIC(ESS_model2)


#Try adding renal failure
ESS_model3 <- lm(Epworth.Sleepiness.Scale ~ Gender + Age + BMI + Depression + Time.from.transplant + Renal.Failure, 
                 data = imputed_sleep_data)

#Simpler model is better
anova(ESS_model,ESS_model3)
AIC(ESS_model)
AIC(ESS_model3)

#Check colinearity
vif(ESS_model)

#check for heterosedasticity
ESS_fits <- fitted(ESS_model)
ESS_resid <- resid(ESS_model)
heteroscedasticity_ESS <- cbind(ESS_fits, ESS_resid)

#plot
ggplot(data = heteroscedasticity_ESS, mapping = aes(x = ESS_resid, ESS_fits))+
  geom_point()

#QQplot
ggplot(data = heteroscedasticity_ESS, mapping = aes(sample = ESS_resid))+
  geom_qq()+
  geom_qq_line()+
  labs(title = "QQ Plot for ESS model")+
  theme(plot.title = element_text(hjust = 0.5))

#summarize the model
summary(ESS_model)

##### Logistic regression model for BSS ######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = the smallest class

#find the smallest class, 1s (sleep apnea)
smallest_class <- table(na.omit(sleep_data$Berlin.Sleepiness.Scale))

#calculate:
max_predictors_BSS <- sleep_data %>% 
  select(Berlin.Sleepiness.Scale) %>% 
  filter(Berlin.Sleepiness.Scale == 1) %>% 
  nrow()/15

#convert to integer
max_predictors_BSS <- as.integer(max_predictors_BSS)

#initial model with all predictors with strong clinical evidence
BSS_model <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant +  Depression,
                 family = "binomial",
                 data = imputed_sleep_data)

#Try adding models with weaker clinical evidence
#could not add liver diagnosis, it would exceed maximum amount of degrees of freedom allowed for this model, only adding Renal Failure
BSS_model2 <- glm(Berlin.Sleepiness.Scale ~ Gender + Age + BMI + Time.from.transplant +  Depression + Renal.Failure,
                  family = "binomial",
                  data = imputed_sleep_data)

#simpler model is better
anova(BSS_model,BSS_model2)

#summary
summary(BSS_model)


#Check colinearity
vif(BSS_model)

#Summary
summary(BSS_model)


##### Linear regression model for AthensSS ######

#calculate p; number of predictors
# for logistic regression: p<m/15 where m = number of events
#calculate:
max_predictors_AthensSS <- as.integer(length(sleep_data$Athens.Insomnia.Scale)/15)

#initial model with clinically relevant predictors found in literature
AthensSS_model <-lm(Athens.Insomnia.Scale ~ Gender + Age + BMI + Time.from.transplant + Depression,
               data = imputed_sleep_data)

#Try adding liver diagnosis
AthensSS_model2 <-lm(Athens.Insomnia.Scale ~ Gender + Age + BMI + Time.from.transplant + Depression + Liver.Diagnosis,
                    data = imputed_sleep_data)


#Simpler model is better
anova(AthensSS_model,AthensSS_model2)

AIC(AthensSS_model)
AIC(AthensSS_model2)

#Try adding renal failure
AthensSS_model3 <- lm(Athens.Insomnia.Scale ~ Gender + Age + BMI + Time.from.transplant + Depression + Renal.Failure,
                      data = imputed_sleep_data)

#Simpler model is better
anova(AthensSS_model,AthensSS_model3)
AIC(AthensSS_model)
AIC(AthensSS_model3)

#Check colinearity
vif(ESS_model)

#check for non-linearity/heterosedasticity
AIS_fits <- fitted(AthensSS_model)
AIS_resid <- resid(AthensSS_model)
heteroscedasticity_AIS <- cbind(AIS_fits, AIS_resid)

#plot
ggplot(data = heteroscedasticity_AIS, mapping = aes(x = AIS_resid, AIS_fits))+
  geom_point()+
  labs(title = "Fitted vs Residual plot for the AIS model")+
  theme(plot.title = element_text(hjust = 0.5))

#QQplot
ggplot(data = heteroscedasticity_AIS, mapping = aes(sample = AIS_resid))+
  geom_qq()+
  geom_qq_line()+
  labs(title = "QQ Plot for AIS model")+
  theme(plot.title = element_text(hjust = 0.5))

summary(AthensSS_model)

#### PCS Model ####

# use lm of AIS, BSS, ESS to predict PCS/MCS
PCS_model <- lm(SF36.PCS ~ Epworth.Sleepiness.Scale + Berlin.Sleepiness.Scale + Athens.Insomnia.Scale,
                data = imputed_sleep_data)

#Check colinearity
vif(PCS_model)

#check for heterosedasticity
PCS_fits <- fitted(PCS_model)
PCS_resid <- resid(PCS_model)
heteroscedasticity_PCS <- cbind(PCS_fits, PCS_resid)

#plot
ggplot(data = heteroscedasticity_PCS, mapping = aes(x = PCS_resid, PCS_fits))+
  geom_point()+
  labs(title = "Fitted vs Residual plot for the PCS model")+
  theme(plot.title = element_text(hjust = 0.5))


#QQplot
ggplot(data = heteroscedasticity_PCS, mapping = aes(sample = PCS_resid))+
  geom_qq()+
  geom_qq_line()+
  labs(title = "QQ Plot for AIS model")+
  theme(plot.title = element_text(hjust = 0.5))

summary(PCS_model)

#### MCS Model ####

#create model for MCS
MCS_model <- lm(SF36.MCS ~ Epworth.Sleepiness.Scale + Berlin.Sleepiness.Scale + Athens.Insomnia.Scale,
                data = imputed_sleep_data)

#Check colinearity
vif(MCS_model)

#check for non-linearity/heterosedasticity
MCS_fits <- fitted(MCS_model)
MCS_resid <- resid(MCS_model)
heteroscedasticity_MCS <- cbind(AIS_fits, AIS_resid)

#plot
ggplot(data = heteroscedasticity_AIS, mapping = aes(x = AIS_resid, AIS_fits))+
  geom_point()+
  labs(title = "Fitted vs Residual plot for the AIS model")+
  theme(plot.title = element_text(hjust = 0.5))

#QQplot
ggplot(data = heteroscedasticity_AIS, mapping = aes(sample = AIS_resid))+
  geom_qq()+
  geom_qq_line()+
  labs(title = "QQ Plot for AIS model")+
  theme(plot.title = element_text(hjust = 0.5))

summary(MCS_model)


#### Analysis: plot predicted values compared to each predictor
# Create new data: for each predictor, set all other predictors to the mean/reference

#### Analysis for ESS_model ####

#Use the generate_data function from functions.R
# This will take a model, a response variable, and a predictor and create a new dataset
# All variables except the response and predictor will be held constant, either at the mean or the refernce value
# This newdata will be used to predict the response variable using the model given to the funciton
# The predicted values are appended to the data and returned from the function
Depression_data <- generate_data(model = ESS_model, response = "Epworth.Sleepiness.Scale", predictor = "Depression")
Gender_data <-generate_data(model = ESS_model, response = "Epworth.Sleepiness.Scale", predictor = "Gender")
BMI_data <- generate_data(model = ESS_model, response = "Epworth.Sleepiness.Scale", predictor = "BMI")
Age_data <- generate_data(model = ESS_model, response = "Epworth.Sleepiness.Scale", predictor = "Age")
Transplant_data <- generate_data(model = ESS_model, response = "Epworth.Sleepiness.Scale", predictor = "Time.from.transplant")


#create a new dataframe with combined data that we will plot (to plot with facets)
#categorical
combined_data <- rbind(
  
  #One predictor row, one row with the data, and one row with the fitted values
  data.frame(Predictor = "Gender",
             Variable = Gender_data$Gender %>%
               fct_recode(Male = "1", Female = "2"),
             fit = Gender_data$predicted_values
             ),
  
  #bind a data frame with the same structure but with depression data
  data.frame(Predictor = "Depression",
             Variable = Depression_data$Depression %>% 
               fct_recode(Yes = "1", No = "0"),
             fit = Depression_data$predicted_values
             )
)

#Take a look
combined_data

#Same for continuous data
combined_data_cont <- rbind(
  data.frame(Predictor = "Age",
             Variable = Age_data$Age,
             fit = Age_data$predicted_values
  ),
  data.frame(Predictor = "BMI",
            Variable = BMI_data$BMI,
             fit = BMI_data$predicted_values
  ),
  data.frame(Predictor = "Time.from.transplant",
             Variable = Transplant_data$Time.from.transplant,
             fit = Transplant_data$predicted_values
  )
  
)

# Plot categorical variables and their impact on predicted values
cat_ess <- ggplot(data = combined_data, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_jitter(height = 0.05) +
  facet_wrap(~Predictor, scales = "free_x") +
  labs(y = "Predicted ESS", x = "", title = "Impact of Categorical Predictors on ESS") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  geom_hline(yintercept = 9.145496)+
  geom_hline(yintercept = 8.053150, , linetype = 2)+
  geom_hline(yintercept = 7.801526, linetype = 1)

#plot continuous variables and their impact on pred. values
cont_ess <- ggplot(data = combined_data_cont, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_smooth(method = lm, se = F) +
  facet_wrap(~Predictor)+
  labs(y = "Predicted ESS", title = "Impact of Continuous Predictors on ESS", x = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  guides()

#Place plots side by side, this is commented out
# cat_ess | cont_ess

#### Analysis for BSS_model ####
# The process above will be repeated for all models

Depression_data <- generate_data(model = BSS_model, response = "Berlin.Sleepiness.Scale", predictor = "Depression")
Gender_data <-generate_data(model = BSS_model, response = "Berlin.Sleepiness.Scale", predictor = "Gender")
BMI_data <- generate_data(model = BSS_model, response = "Berlin.Sleepiness.Scale", predictor = "BMI")
Age_data <- generate_data(model = BSS_model, response = "Berlin.Sleepiness.Scale", predictor = "Age")
Transplant_data <- generate_data(model = BSS_model, response = "Berlin.Sleepiness.Scale", predictor = "Time.from.transplant")



#create a new dataframe with combined data (to plot with facets)
combined_data <- rbind(
  data.frame(Predictor = "Gender",
             Variable = Gender_data$Gender %>%
               fct_recode(Male = "1", Female = "2"),
             fit = Gender_data$predicted_values
  ),
  data.frame(Predictor = "Depression",
             Variable = Depression_data$Depression %>% 
               fct_recode(Yes = "1", No = "0"),
             fit = Depression_data$predicted_values
  )
)

combined_data_cont <- rbind(
  data.frame(Predictor = "Age",
             Variable = Age_data$Age,
             fit = Age_data$predicted_values
  ),
  data.frame(Predictor = "BMI",
             Variable = BMI_data$BMI,
             fit = BMI_data$predicted_values
  ),
  data.frame(Predictor = "Time.from.transplant",
             Variable = Transplant_data$Time.from.transplant,
             fit = Transplant_data$predicted_values
  )
  
)

# Plot both categorical variables and their impact on predicted values
cat_bss <- ggplot(data = combined_data, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_jitter(height = 0.0001) +
  facet_wrap(~Predictor, scales = "free_x") +
  labs(y = "Probability of Sleep Disturbance", x = "", title = "Impact of Categorical Predictors on BSS") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  geom_hline(yintercept = 0.3683575, linetype = 1)+
  geom_hline(yintercept = 0.3916458, linetype = 1)+
  geom_hline(yintercept = 0.3736843, linetype = 2)

#plot continuous variables and their impact on pred. values
cont_bss <- ggplot(data = combined_data_cont, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_line(method = lm, se = F) +
  facet_wrap(~Predictor)+
  labs(y = "Predicted Probability of Sleep Disturbance", title = "Impact of Continuous Predictors on BSS", x = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))

#### Analysis for AIS model ####
Depression_data <- generate_data(model = AthensSS_model, response = "Athens.Insomnia.Scale", predictor = "Depression")
Gender_data <-generate_data(model = AthensSS_model, response = "Athens.Insomnia.Scale", predictor = "Gender")
BMI_data <- generate_data(model = AthensSS_model, response = "Athens.Insomnia.Scale", predictor = "BMI")
Age_data <- generate_data(model = AthensSS_model, response = "Athens.Insomnia.Scale", predictor = "Age")
Transplant_data <- generate_data(model = AthensSS_model, response = "Athens.Insomnia.Scale", predictor = "Time.from.transplant")


#create a new dataframe with combined data (to plot with facets)
combined_data <- rbind(
  data.frame(Predictor = "Gender",
             Variable = Gender_data$Gender %>%
               fct_recode(Male = "1", Female = "2"),
             fit = Gender_data$predicted_values
  ),
  data.frame(Predictor = "Depression",
             Variable = Depression_data$Depression %>% 
               fct_recode(Yes = "1", No = "0"),
             fit = Depression_data$predicted_values
  )
)

combined_data_cont <- rbind(
  data.frame(Predictor = "Age",
             Variable = Age_data$Age,
             fit = Age_data$predicted_values
  ),
  data.frame(Predictor = "BMI",
             Variable = BMI_data$BMI,
             fit = BMI_data$predicted_values
  ),
  data.frame(Predictor = "Time.from.transplant",
             Variable = Transplant_data$Time.from.transplant,
             fit = Transplant_data$predicted_values
  )
  
)

# Plot both categorical variables and their impact on predicted values
cat_AIS <- ggplot(data = combined_data, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_jitter(height = 0.05) +
  facet_wrap(~Predictor, scales = "free_x") +
  labs(y = "Predicted AIS", x = "", title = "Impact of Categorical Predictors on AIS") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  geom_hline(yintercept = 6.92)+
  geom_hline(yintercept = 7.95, linetype = 2)+
  geom_hline(yintercept = 8.62)
  guides()

#plot continuous variables and their impact on pred. values
cont_AIS <- ggplot(data = combined_data_cont, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_smooth(method = lm, se = F)+
  facet_wrap(~Predictor)+
  labs(y = "Predicted AIS", title = "Impact of Continuous Predictors on AIS", x = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  guides()


#### Analysis for PCS model ####
ESS_data <- generate_data(model = PCS_model, response = "SF36.PCS", predictor = "Epworth.Sleepiness.Scale")
BSS_data <-generate_data(model = PCS_model, response = "SF36.PCS", predictor = "Berlin.Sleepiness.Scale")
AIS_data <- generate_data(model = PCS_model, response = "SF36.PCS", predictor = "Athens.Insomnia.Scale")


#create a new dataframe with combined data (to plot with facets)
combined_data <- data.frame(Predictor = "Berlin Sleepiness Scale",
                            Variable = BSS_data$Berlin.Sleepiness.Scale %>%
                              fct_recode(Sleep_Obstuction = "1", Normal_Sleep = "0"),
                            fit = BSS_data$predicted_values
                            )

combined_data_cont <- rbind(
  data.frame(Predictor = "ESS",
             Variable = ESS_data$Epworth.Sleepiness.Scale,
             fit = ESS_data$predicted_values
  ),
  data.frame(Predictor = "AIS",
             Variable = AIS_data$Athens.Insomnia.Scale,
             fit = AIS_data$predicted_values
  )
)

# Plot both categorical variables and their impact on predicted values
cat_PCS <- ggplot(data = combined_data, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_jitter(height = 0.05) +
  facet_wrap(~Predictor, scales = "free_x") +
  labs(y = "Predicted PCS", x = "", title = "Impact of Categorical Predictors on PCS") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  geom_hline(yintercept = 44.27301)+
  geom_hline(yintercept = 41.5997)

#plot continuous variables and their impact on pred. values
cont_PCS <- ggplot(data = combined_data_cont, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_smooth(method = lm, se = F)+
  facet_wrap(~Predictor)+
  labs(y = "Predicted PCS", title = "Impact of Continuous Predictors on PCS", x = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  guides()


#### Analysis for MCS model ####
ESS_data <- generate_data(model = MCS_model, response = "SF36.MCS", predictor = "Epworth.Sleepiness.Scale")
BSS_data <-generate_data(model = MCS_model, response = "SF36.MCS", predictor = "Berlin.Sleepiness.Scale")
AIS_data <- generate_data(model = MCS_model, response = "SF36.MCS", predictor = "Athens.Insomnia.Scale")


#create a new dataframe with combined data (to plot with facets)
combined_data <- data.frame(Predictor = "Berlin Sleepiness Scale",
                            Variable = BSS_data$Berlin.Sleepiness.Scale %>%
                              fct_recode(Sleep_Obstuction = "1", Normal_Sleep = "0"),
                            fit = BSS_data$predicted_values
)

combined_data_cont <- rbind(
  data.frame(Predictor = "ESS",
             Variable = ESS_data$Epworth.Sleepiness.Scale,
             fit = ESS_data$predicted_values
  ),
  data.frame(Predictor = "AIS",
             Variable = AIS_data$Athens.Insomnia.Scale,
             fit = AIS_data$predicted_values
  )
)

# Plot both categorical variables and their impact on predicted values
cat_MCS <- ggplot(data = combined_data, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_jitter(height = 0.001) +
  facet_wrap(~Predictor, scales = "free_x") +
  labs(y = "Predicted MCS", x = "", title = "Impact of Categorical Predictors on MCS") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  geom_hline(yintercept = 46.19053)+
  geom_hline(yintercept = 46.22998)

#plot continuous variables and their impact on pred. values
cont_MCS <- ggplot(data = combined_data_cont, mapping = aes(x = Variable, y = fit, color = Predictor)) +
  geom_smooth(method = lm, se = F)+
  facet_wrap(~Predictor)+
  labs(y = "Predicted MCS", title = "Impact of Continuous Predictors on MCS", x = "") +
  theme(plot.title = element_text(hjust = 0.5, size = 10))+
  guides()

#plot all the graphs
cat_ess
cont_ess

cat_bss
cont_bss

cat_AIS
cont_AIS

cat_PCS
cont_PCS

cat_MCS
cont_MCS

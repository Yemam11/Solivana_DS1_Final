#### General Summary Tables ####

#Run the main file to ensure you have all the data to create the tables
source("team_projectV2.R")

#remove the subject field
summary_data <- sleep_data %>%
  select(-Subject) %>% 
  na.omit()

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

summary_data$Berlin.Sleepiness.Scale <-summary_data$Berlin.Sleepiness.Scale %>% 
  fct_recode(Sleep.Apnea = "1", Normal = "0")

#create a summary table
summary_table <- tbl_summary(summary_data,
            by = Gender,
            statistic = list(
              all_continuous() ~ "{median} ({sd})",
              all_categorical() ~"{n} ({p}%)"
              ),
            
            label = list(
              Time.from.transplant  = "Time from transplant",
              Liver.Diagnosis = "Liver Diagnosis",
              Recurrence.of.disease = "Recurrence of disease",
              Renal.Failure = "Renal Failure",
              Rejection.graft.dysfunction	 = "Evidence of rejection or graft dysfunction",
              Any.fibrosis = "Fibrosis",
              Epworth.Sleepiness.Scale = "Epworth Sleepiness Scale",
              Athens.Insomnia.Scale = "Athens Insomnia Scale",
              Berlin.Sleepiness.Scale = "Berlin Sleepiness Scale",
              SF36.PCS = "SF36 Physical Component Survey",
              SF36.MCS = "SF36 Mental Component Survey"),
            type = cols~"categorical",
            ) %>% 
  add_stat_label(location = "column")
summary_table

#### Model Summary Tables ####
#ESS
ESS_table <- tbl_regression(ESS_model,
                            label = list(Renal.Failure = "Renal Failure"),
                            intercept = T) %>%
  bold_p() %>%
  as_gt() %>% 
  tab_header(
    title = "Epworth Sleepiness Scale",
    subtitle = "Linear Regression Model Summary"
  )
ESS_table


#BSS
BSS_table <- tbl_regression(BSS_model,
                            label = list(Renal.Failure = "Renal Failure"),
                            intercept = T) %>% 
  bold_p() %>%
  as_gt() %>% 
  tab_header(
    title = "Berlin Sleepiness Scale",
    subtitle = "Logistic Regression Model Summary"
  )
BSS_table

#AthensSS
AthensSS_table <- tbl_regression(AthensSS_model,
                            label = list(Renal.Failure = "Renal Failure"),
                            intercept = T) %>%
  bold_p() %>%
  as_gt() %>% 
  tab_header(
    title = "Athens Insomnia Scale",
    subtitle = "Linear Regression Model Summary"
  )
AthensSS_table


#PCS
PCS_table <- tbl_regression(PCS_model,
                            label = list(Epworth.Sleepiness.Scale = "ESS",
                                         Berlin.Sleepiness.Scale = "BSS",
                                         Athens.Insomnia.Scale = "AIS"),
                            intercept = T) %>%
  bold_p() %>%
  as_gt() %>% 
  tab_header(
    title = "SF36 Physical Component Summary",
    subtitle = "Linear Regression Model Summary"
  )
PCS_table

#MCS
MCS_table <- tbl_regression(PCS_model,
                            label = list(Epworth.Sleepiness.Scale = "ESS",
                                         Berlin.Sleepiness.Scale = "BSS",
                                         Athens.Insomnia.Scale = "AIS"),
                            intercept = T) %>%
  bold_p() %>%
  as_gt() %>% 
  tab_header(
    title = "SF36 Mental Component Summary",
    subtitle = "Linear Regression Model Summary"
  )
MCS_table


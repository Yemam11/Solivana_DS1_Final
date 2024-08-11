#### General Summary Tables ####
#Run the main file to ensure you have all the data to create the tables
source("team_projectV2.R")

#create a summary table
summary_table <- tbl_summary(summary_data,
            by = Gender,
            statistic = list(
              all_continuous() ~ "{median} ({sd})",
              all_categorical() ~"{n} ({p}%)"
              ),
            
            label = list(
              Time.from.transplant  = "Time from transplant (Years)",
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
            missing = "no") %>% 
  add_stat_label(location = "column") %>%
  add_n(missing = TRUE, last = FALSE, statistic = "{N_miss} ({p_miss})%", col_label = "**Missing**") %>%
  as_gt() %>% 
  tab_header(
    title = "Data Summary",
    subtitle = "Physical and Clinical Observations of Patients in Sleep Disturbance Study"
  )
summary_table

gtsave(summary_table, filename = "summary_table.png", vwidth = 750, vheight = 2500, expand = 10)

#### Model Summary Tables ####
#ESS
ESS_table <- tbl_regression(ESS_model,
                            label = list(Time.from.transplant = "Time from Transplant (Years)"),
                            intercept = T) %>%
  bold_p() %>%
  add_vif() %>% 
  add_glance_table(include = c(adj.r.squared, AIC)) %>% 
  as_gt() %>% 
  tab_header(
    title = "Epworth Sleepiness Scale",
    subtitle = "Linear Regression Model Summary"
  )
ESS_table


#BSS
BSS_table <- tbl_regression(BSS_model,
                            label = list(Time.from.transplant = "Time from Transplant (Years)"),
                            intercept = T,
                            ) %>% 
  bold_p() %>%
  add_vif() %>%
  add_glance_table(include = c(AIC)) %>%
  as_gt() %>% 
  tab_header(
    title = "Berlin Sleepiness Scale",
    subtitle = "Logistic Regression Model Summary"
  )
BSS_table

BSS_exp_table <- tbl_regression(BSS_model,
                            label = list(Time.from.transplant = "Time from Transplant (Years)"),
                            intercept = T,
                            conf.int = F,
                            exponentiate = T
                            
) %>% 
  bold_p() %>%
  as_gt() %>%
  tab_header(
    title = "Berlin Sleepiness Scale",
    subtitle = "Odds Ratio Summary"
  )
BSS_exp_table

#AthensSS
AthensSS_table <- tbl_regression(AthensSS_model,
                            label = list(Time.from.transplant = "Time from Transplant (Years)"),
                            intercept = T) %>%
  bold_p() %>%
  add_vif() %>%
  add_glance_table(include = c(adj.r.squared, AIC)) %>%
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
  add_vif() %>% 
  add_glance_table(include = c(adj.r.squared, AIC)) %>%
  as_gt() %>% 
  tab_header(
    title = "SF36 Physical Component Summary",
    subtitle = "Linear Regression Model Summary"
  )
PCS_table

#MCS
MCS_table <- tbl_regression(MCS_model,
                            label = list(Epworth.Sleepiness.Scale = "ESS",
                                         Berlin.Sleepiness.Scale = "BSS",
                                         Athens.Insomnia.Scale = "AIS"),
                            intercept = T) %>%
  bold_p() %>%
  add_vif() %>%
  add_glance_table(include = c(adj.r.squared, AIC)) %>%
  as_gt() %>% 
  tab_header(
    title = "SF36 Mental Component Summary",
    subtitle = "Linear Regression Model Summary"
  )
MCS_table


#prevalences
prevalences_table <- prevalences %>%
  gt() %>% 
  fmt_number(decimals = 1) %>% 
  cols_label(percent = "Prevalence of Sleep Disturbance (%)") %>% 
  tab_header(
    title = "Prevalence of Sleep Disturbance",
    subtitle = "In post liver transplant patients measured by different scales"
  )
prevalences_table



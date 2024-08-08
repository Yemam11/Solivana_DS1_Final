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



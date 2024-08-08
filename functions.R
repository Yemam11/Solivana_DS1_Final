# Defines functions for team_project
# Authors: Youssef Emam, Hasan Abdo, Angela Bakaj


#### Data generation function ####
generate_data <- function(model = "", response = "", predictor ="", data = imputed_sleep_data){
  #alter all other columns to be the mean or a constant level value
  
  #select all columns except the predictor and response
  temp_data <- data %>% 
    select(!c({{response}}, {{predictor}}))
  
  #create a dataframe with just the predictor and response
  data <- data %>% 
    select({{predictor}}, {{response}})
  
  #create a vector with all the names of the cols to be altered
  cols <- names(temp_data)
  
  #iterate through each column and change all the variables to the mean/reference
  for(column in cols){
    
    #if it is numeric convert to mean
    if (class(temp_data[[column]]) == "numeric"){
      temp_data[[column]] <- mean(temp_data[[column]], na.rm = T)
      
      #if it is a factor convert to 1
      # 1 and not 0 because some data is 1-5 scale
    } else{
      temp_data[[column]] <- 1
      temp_data[[column]] <- as.factor(temp_data[[column]])
    }
  }
  
  new_data <- cbind(data,temp_data)
  
  #use the model to predict fitted values
  predicted_values <- predict(model, newdata = new_data, int = "c")
  
  #add the predicted values to the df
  new_data <- cbind(new_data, predicted_values)
  
  return(new_data)
}

#### EDA Function ####
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
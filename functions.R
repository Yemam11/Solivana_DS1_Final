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
  
  sleep_data$Gender <- fct_recode(sleep_data$Gender, Male = "1", Female = "2")
  
  #Plot age distribution
  ggplot(data = sleep_data, mapping = aes(x = Age, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "Age Distribution", y = "Count")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  
  #plot BMI distribution
  ggplot(data = summary_data, mapping = aes(x = BMI, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "BMI Distribution", y = "Count")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  
  #plot incidence of depression
  ggplot(data = summary_data, mapping = aes(x = Depression, fill = Gender))+
    geom_bar(col = "black", width = 0.3)+
    theme_minimal()+
    labs(title = "BMI Distribution", y = "Count")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  
  #plot ESS score distribution
  ggplot(data = sleep_data, mapping = aes(x = Epworth.Sleepiness.Scale, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "Epworth Sleepiness Scale Score Distribution", y = "Count", x = "ESS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)+
    geom_vline(xintercept = 10, linetype = 2, size = 1)+
    annotate(x=10,y=Inf,label="High Risk",vjust=1, hjust = 0.5, geom="label")
  
  #Plot BSS scores
  ggplot(data = summary_data, mapping = aes(x = Berlin.Sleepiness.Scale, fill = Gender))+
    geom_bar(col = "black", width = 0.3)+
    theme_minimal()+
    labs(title = "Berlin Sleepiness Scale Result Distribtion", y = "Count")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  
  #plot Athens insominia score distribution
  ggplot(data = sleep_data, mapping = aes(x = Athens.Insomnia.Scale, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "Athens Insomnia Scale Score Distribution", y = "Count", x = "AIS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)+
    geom_vline(xintercept = 5.5, linetype = 2, size = 1)+
    annotate(x=5.5,y=Inf,label="Insomina",vjust=1, hjust = -0.1, geom="label")
  
  #plot PCS
  ggplot(data = sleep_data, mapping = aes(x = SF36.PCS, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "SF36 Physical Component Survery Score Distribution", y = "Count", x = "SF36 PCS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  
  #plot PCS
  ggplot(data = sleep_data, mapping = aes(x = SF36.MCS, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "SF36 Mental Component Survery Score Distribution", y = "Count", x = "SF36 MCS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
}
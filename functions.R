# Defines functions for team_project
# Authors: Youssef Emam, Hasan Abdo, Angela Bakaj
# Run the main file to ensure you have all the necessary data to generate the plots

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
  
  if(class(new_data[[response]]) == "factor"){
    #if it's glm predict the probability
    predicted_values <- predict(model, newdata = new_data, type = "response")
  } else{
    #if its a lm predict the actual values
    predicted_values <- predict(model, newdata = new_data)
  }
  
  
  #add the predicted values to the df
  new_data <- cbind(new_data, predicted_values)
  
  return(new_data)
}

#### EDA Function ####
eda <- function(sleep_data){
  
  sleep_data$Gender <- fct_recode(sleep_data$Gender, Male = "1", Female = "2")
  
  #Plot age distribution
  age_dist<- ggplot(data = sleep_data, mapping = aes(x = Age, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "Age Distribution", y = "Count", x = "Age (Years)")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  age_dist
  
  #plot BMI distribution
  BMI_dist <- ggplot(data = summary_data, mapping = aes(x = BMI, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "BMI Distribution", y = "Count")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  BMI_dist
  
  #plot incidence of depression
  Depression_dist <-ggplot(data = summary_data, mapping = aes(x = Depression, fill = Gender))+
    geom_bar(col = "black", width = 0.3)+
    theme_minimal()+
    labs(title = "Depression Distribution", y = "Count")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  Depression_dist
  
  Transplant_dist <- ggplot(data = summary_data, mapping = aes(x = Time.from.transplant, fill = Gender))+
    geom_histogram(col = "black", width = 0.3)+
    theme_minimal()+
    labs(title = "Time from Transplant Distribution", y = "Count", x = "Time from Transplant (Years)")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  Transplant_dist
  
  
  #plot ESS score distribution
  ESS_dist <- ggplot(data = summary_data, mapping = aes(x = Epworth.Sleepiness.Scale, fill = Gender))+
    geom_rect(mapping = aes(xmin=10,xmax=Inf,ymin=0,ymax=Inf),
              fill="lightgrey", alpha=1)+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "Epworth Sleepiness Scale Score Distribution", y = "Count", x = "ESS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)+
    geom_vline(xintercept = 10, linetype = 2, size = 1)+
    annotate(x=10,y=Inf,label="High Risk",vjust=2, hjust = -1.5, geom="label")
  ESS_dist
  
  #Plot BSS scores
  BSS_dist <- ggplot(data = summary_data, mapping = aes(x = Berlin.Sleepiness.Scale, fill = Gender))+
    geom_bar(col = "black", width = 0.3)+
    theme_minimal()+
    labs(title = "Berlin Sleepiness Scale Result Distribtion", y = "Count", x = "Berlin Sleepiness Scale Result")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  BSS_dist
  
  #plot Athens insominia score distribution
  AIS_dist <- ggplot(data = summary_data, mapping = aes(x = Athens.Insomnia.Scale, fill = Gender))+
    geom_rect(mapping = aes(xmin=5.5,xmax=Inf,ymin=0,ymax=Inf),
              fill="lightgrey", alpha=1)+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "Athens Insomnia Scale Score Distribution", y = "Count", x = "AIS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)+
    geom_vline(xintercept = 5.5, linetype = 2, size = 1)+
    annotate(x=5.5,y=Inf,label="Insomina",vjust=2, hjust = -2, geom="label")
  AIS_dist
  
  #plot PCS
  PCS_dist <- ggplot(data = summary_data, mapping = aes(x = SF36.PCS, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "SF36 Physical Component Survery Score Distribution", y = "Count", x = "SF36 PCS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  PCS_dist
  
  #plot MCS
  MCS_dist<- ggplot(data = sleep_data, mapping = aes(x = SF36.MCS, fill = Gender))+
    geom_histogram(col = "black")+
    theme_minimal()+
    labs(title = "SF36 Mental Component Survery Score Distribution", y = "Count", x = "SF36 MCS Score")+
    theme(plot.title = element_text(hjust =0.5))+
    scale_fill_discrete(direction = -1)
  MCS_dist
  
  return(list(age_dist, BMI_dist, Depression_dist, Transplant_dist, ESS_dist, BSS_dist, AIS_dist, PCS_dist, MCS_dist))
}

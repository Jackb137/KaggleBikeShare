# Bikeshare

# LIBRARIES=====================================================================
  library(tidyverse)
  library(vroom)
  library(tidymodels)
  library(lubridate)

# READ IN THE DATA==============================================================

  test <- vroom("test.csv")
  train <- vroom("train.csv")

# ENGINEER DATA WITH DPLYR======================================================
  
  train <- train %>% mutate(datetime = with_tz(datetime, "America/New_York"),
                            Free_Time = as.factor(ifelse(hour(datetime) >= 16 & hour(datetime) <= 22,1, 0)),
                            weather = ifelse(weather == 4, 3, weather)) %>% 
                            select(-registered,-atemp,-casual)
  
  test <- test %>% mutate(weather = ifelse(weather == 4, 3, weather),
                          datetime = with_tz(datetime, "America/New_York"),
                          Free_Time = as.factor(ifelse(hour(datetime) >= 16 & hour(datetime) <= 22,1, 0)))

# ENGINEER DATA WITH RECIPE=====================================================
  
  Analysis_recipe <- recipe(count ~ ., data = train) %>% # Set model formula and dataset
    step_mutate(season = factor(season, levels=c(1,2,3,4), labels=c("Spring","Summer","Fall","Winter")), #Make something a factor
        weather = factor(weather, levels=c(1,2,3), labels=c("Clear","Few Clouds","Partly Cloudy")),
        workingday = factor(workingday, levels=c(1,2), labels=c("Workday","Weekend/Holiday")),
        holiday = factor(holiday, levels=c(1,2), labels=c("Holiday","Non Holiday")),
        datetime = with_tz(datetime, "America/New_York")) %>%
    
    step_zv(all_predictors()) #removes zero-variance predictors

# BAKE==========================================================================
  
  Ready_recipe <- prep(Analysis_recipe) # Sets up the preprocessing using myDataSet
  bake(Ready_recipe, new_data=test)

# LINEAR REGRESSION WITH TRANSFORMATION=========================================
  
  Linear_Bike_Model <- linear_reg() %>%
    set_engine("lm") # Engine = What R function to use
  
  bike_workflow <- workflow() %>%
    add_recipe(Analysis_recipe) %>%
    add_model(Linear_Bike_Model) %>%
    fit(data = train) # Fit the workflow
  
  test_dates <- vroom("test.csv")
  
  bike_predictions <- predict(bike_workflow, new_data = test) %>%
    mutate(datetime = test_dates$datetime) %>%
    mutate(count = pmax(.pred, 0)) %>%
    select(-.pred)

  bike_predictions$datetime <- as.character(format(bike_predictions$datetime))




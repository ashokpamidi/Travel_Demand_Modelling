library(shiny)
library(shinyBS)
library(tidyverse)
library(shinyvalidate)
library(readxl)
library(writexl)

#excel df to mat ----
df_to_mat <- function(file_path, sheet_name){
  df <- read_xlsx(path = file_path, sheet = sheet_name)
  as_mat <- as.matrix(df[-1])
  return(as_mat)
}

#reading data from excel and extracting a vector from it ----
extractingAVector <- function(file_path, sheet_name, para){
  df <- read_xlsx(path = file_path, sheet = sheet_name) %>% 
    pull(para)
  return(df)
}

#calculating friction factor ----
#Deterrence functions - Exponential - 1, Power - 2, Combined - 3
calculate_friction_fator <- function(b = 0.1, n = 2, deter_func_code, resist_para_mat){
  if(deter_func_code == 1){
    ff <- exp(resist_para_mat*(-b))
  } else if(deter_func_code == 2){
    ff <- resist_para_mat^(-n)
  } else{
    ff <- resist_para_mat^(n)*exp(resist_para_mat*(-b))
  }
  return(ff)
}


#trip distribution ----
tripDistribution <- function(attractions, productions, friction_factor){
  
  distributed_trips <- friction_factor
  distributed_trips[is.numeric(distributed_trips)] <- 0
  
  for (i in 1:nrow(friction_factor)) {
    for (j in 1:ncol(friction_factor)) {
      p <- productions[i]*attractions[j]*friction_factor[i,j]
      y <- attractions*friction_factor[i,]
      q <- Reduce("+", y)
    
      distributed_trips[i,j] <- p/q
    }
  }
  return(distributed_trips)
}


#adjusting attractions and productions ----
adjusting_trips <- function(no_iter, attractions, productions, trips_matrix){
  
  codes <- rep(c("attractions", "productions"), no_iter)
  codes <- codes[1:no_iter]
  
  for(code in codes){
    
    if(code == "attractions"){
      
      updated_attractions <- colSums(trips_matrix)
      modification_factors <- attractions/updated_attractions
      
      for(h in 1:nrow(trips_matrix)){
        trips_matrix[h,] <- trips_matrix[h,]*modification_factors
      }
    } else {
      
      updated_productions <- rowSums(trips_matrix)
      modification_factors <- productions/updated_productions
      
      for(h in 1:ncol(trips_matrix)){
        trips_matrix[,h] <- trips_matrix[,h]*modification_factors
      }
    }
  }
  return(trips_matrix)
}


#trip length frequency distribution plot ----
tlfd <- function(distance_mat, demand_mat, intervals){
  
  m = round(max(distance_mat), digits = -1)
  levels <- seq(from = 0, to = m+10,
                by = m/intervals)
  
    tlfd_df <- data.frame(
      "distance" = as.vector(distance_mat),
      "trips" = as.vector(demand_mat)) %>%
      mutate(distance_ranges = cut(distance, breaks = levels, right = FALSE)) %>%
      group_by(distance_ranges) %>%
      summarise(trips = sum(trips)) %>%
      mutate(trips = round(trips, digits = 0)) %>% 
      mutate(pct_trips = trips/sum(trips))
    
    plt <- ggplot(data = tlfd_df, aes(x = distance_ranges, y = pct_trips))+
      geom_col()+
      theme_minimal()+
      labs(x = "Distance Intervals",
           y = "% of Trips",
           title = "Trip Length Frequency Distribution")
  
  
    output <- list(tlfd_df, plt)
    return(output)
}


#comparing TLFDs -----
compare_tlfds <- function(distance_mat, demand_mat1, demand_mat2, intervals){
  
  m = round(max(distance_mat), digits = -1)
  levels <- seq(from = 0, to = m+10,
                by = m/intervals)
  
  tlfd_df1 <- data.frame(
    "distance" = as.vector(distance_mat),
    "trips1" = as.vector(demand_mat1)) %>%
    mutate(distance_ranges = cut(distance, breaks = levels, right = FALSE)) %>%
    group_by(distance_ranges) %>%
    summarise(trips1 = sum(trips1)) %>%
    mutate(trips1 = round(trips1, digits = 0)) %>% 
    mutate(pct_trips1 = trips1/sum(trips1))
  
  tlfd_df2 <- data.frame(
    "distance" = as.vector(distance_mat),
    "trips2" = as.vector(demand_mat2)) %>%
    mutate(distance_ranges = cut(distance, breaks = levels, right = FALSE)) %>%
    group_by(distance_ranges) %>%
    summarise(trips2 = sum(trips2)) %>%
    mutate(trips2 = round(trips2, digits = 0)) %>% 
    mutate(pct_trips2 = trips2/sum(trips2))
  
  tlfd_df <- left_join(tlfd_df1, tlfd_df2, by = c("distance_ranges" = "distance_ranges"))
  
  plt <- tlfd_df %>% 
    select(distance_ranges, pct_trips1, pct_trips2)%>% 
    pivot_longer(cols = c(pct_trips1, pct_trips2), values_to = "pct_trips") %>% 
    ggplot(aes(x = distance_ranges, y = pct_trips, fill = name))+
    geom_col(position = "dodge")+
    theme_minimal()+
    labs(x = "Distance Intervals",
         y = "% of Trips",
         title = "Trip Length Frequency Distribution")+
    theme(legend.title = element_blank())
  
  
  output <- list(tlfd_df, plt)
  return(output)
}
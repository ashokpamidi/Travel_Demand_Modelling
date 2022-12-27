library(tidyverse)
library(readxl)
library(writexl)
library(igraph)


#Trip Distribution ----
##excel df to mat ----
df_to_mat <- function(file_path, sheet_name){
  df <- read_xlsx(path = file_path, sheet = sheet_name)
  as_mat <- as.matrix(df[-1])
  return(as_mat)
}

##reading data from excel and extracting a vector from it ----
extractingAVector <- function(file_path, sheet_name, para){
  df <- read_xlsx(path = file_path, sheet = sheet_name) %>% 
    pull(para)
  return(df)
}

##calculating friction factor ----
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


##trip distribution alg ----
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


##adjusting attractions and productions ----
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


##trip length frequency distribution plot ----
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


##comparing TLFDs -----
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

#Trip Assignment ----
##reading base data ----
read_data <- function(input_file_path, sheet_name) {
  
  df <- read_xlsx(path = input_file_path,
                  sheet = sheet_name,
                  col_names = TRUE) %>% 
    suppressWarnings()
  return(df)
}

##creating network graph ----
creating_graph <- function(df_links, a, b){
  
  netwrok_edges <- c()
  for(i in 1:nrow(df_links)){
    netwrok_edges[length(netwrok_edges)+1] <- df_links$from[i]
    netwrok_edges[length(netwrok_edges)+1] <- df_links$to[i]
  }
  
  g <- graph(edges = netwrok_edges, directed = T) %>% 
    set_edge_attr(name = "capacity", value = df_links$capacity) %>%
    set_edge_attr(name = "free_flow_tt", value = df_links$free_flow_tt) %>%
    set_edge_attr(name = "flow", value = 0) %>% 
    set_edge_attr(name = "travel_time", value = 0) %>%
    set_edge_attr(name = "alpha", value = a) %>% 
    set_edge_attr(name = "beta", value = b) %>% 
    set_edge_attr(name = "target_flow", value = 0)
  
  return(g)
  
}

##updating travel time values ----
updateTravelTimes <- function(g){
  
  E(g)$travel_time <- E(g)$free_flow_tt*(1+E(g)$alpha*((E(g)$flow/E(g)$capacity)^E(g)$beta))
  return(g)
}

##initialization - first step of the Method of Successive Averages ----
initialization <- function(g,
                           df_flows){
  
  for(i in 1:nrow(df_flows)){
    p <- shortest_paths(g,
                        from = df_flows$from[i],
                        to = df_flows$to[i],
                        weights = E(g)$travel_time)$vpath[[1]]
    E(g, path = p)$target_flow <- df_flows$flow[i]
  }
  
  E(g)$flow <- E(g)$target_flow
  g <- updateTravelTimes(g)
  
  return(g)
  
}


##All or Nothing assignment ----
AoN <- function(g,
                df_flows){
  shortest_times <- c()
  for(i in 1:nrow(df_flows)){
    p <- shortest_paths(g,
                        from = df_flows$from[i],
                        to = df_flows$to[i],
                        weights = E(g)$travel_time)$vpath[[1]]
    E(g, path = p)$target_flow <- df_flows$flow[i]+E(g, path = p)$target_flow
    shortest_times[length(shortest_times)+1] <- sum(E(g, path = p)$travel_time)
  }
  output <- list(g, shortest_times)
  return(output)
}

##calculating step size based on algorithm type ----
calculateStepSize <- function(g,
                              alg,
                              bisection_interval,
                              iteration_num){
  
  if(alg == "fw"){
    x <- E(g)$flow
    x2 <- E(g)$target_flow
    t0 <- E(g)$free_flow_tt
    alpha <- E(g)$alpha
    beta <- E(g)$beta
    capacity <- E(g)$capacity
    
    upper <- 1
    lower <- 0
    for(l in 1:bisection_interval){
      a <- (lower+upper)/2
      a1 <- x2-x
      a2 <- alpha*(((a*x2+(1-a)*x)/capacity))^beta
      
      k <- sum(a1*(t0*(1+a2)))
      
      if(k>0){
        upper <- a
      } else{
        lower <- a
      }
    }
    
    ftr <- (lower+upper)/2
  } else{
    ftr <- 1/(iteration_num+1)
    k <- 0
  }
  output <- list(ftr,k)
  return(output)
}

##update flows based on step size ----
updateFlows <- function(g, ftr){
  E(g)$flow <- ftr*E(g)$target_flow + ((1-ftr)*E(g)$flow)
  return(g)
}

##generalized trip assignment algorithm ----
trip_assignment <- function(g, 
                            df_flows, 
                            iterations = 100, 
                            bisection_iterations = 10,
                            gap = 0.001, 
                            alg = "fw",
                            AoN_algorithm){
  
  iter_num <- c()
  aec_values <- c()
  
  for(j in 1:iterations){
    
    E(g)$target_flow <- 0
    g <- AoN_algorithm(g, df_flows)[[1]]
    shortest_times <- AoN_algorithm(g, df_flows)[[2]]
    
    aec <- (sum(E(g)$flow*E(g)$travel_time)- sum(shortest_times*df_flows$flow))/sum(df_flows$flow)
    
    iter_num[length(iter_num)+1] <- j
    aec_values[length(aec_values)+1] <- aec
    
    if(aec>gap){
      step_size <- calculateStepSize(g,alg,bisection_iterations,j)
      
      g <- updateFlows(g, step_size[[1]])
      g <- updateTravelTimes(g)
      
    }else{
      break
    }
  }
  output <- list(g, iter_num, aec_values)
  return(output)
}


##to extract attributes for a path
pathAttributes <- function(g, nodes_vect, path_parameter){
  a <- c()
  a[length(a)+1] <- sum(E(g, path = nodes_vect)$path_parameters)
  return(a)
}
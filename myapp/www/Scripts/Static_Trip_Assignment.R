library(tidyverse)
library(readxl)
library(igraph)

#reading base data ----
read_data <- function(input_file_path, sheet_name) {
  
  df <- read_xlsx(path = input_file_path,
                        sheet = sheet_name,
                        col_names = TRUE) %>% 
    suppressWarnings()
  return(df)
}

#creating network graph ----
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

#updating travel time values ----
updateTravelTimes <- function(g){
  
  E(g)$travel_time <- E(g)$free_flow_tt*(1+E(g)$alpha*((E(g)$flow/E(g)$capacity)^E(g)$beta))
  return(g)
}

#initialization - first step of the Method of Successive Averages ----
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


#All or Nothing assignment ----
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

#calculating step size based on algorithm type ----
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

#update flows based on step size ----
updateFlows <- function(g, ftr){
  E(g)$flow <- ftr*E(g)$target_flow + ((1-ftr)*E(g)$flow)
  return(g)
}

#generalized trip assignment algorithm ----
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
  # print(paste("lat iteration num: ", j))
  output <- list(g, iter_num, aec_values)
  return(output)
}


#to extract attributes for a path
pathAttributes <- function(g, nodes_vect, path_parameter){
  a <- c()
  a[length(a)+1] <- sum(E(g, path = nodes_vect)$path_parameters)
  return(a)
}
  




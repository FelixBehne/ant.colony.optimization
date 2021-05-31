library(dplyr)
#' ACO Algorithm applied to the Traveling Salesman Problem
#'
#' @param x X-Values from the Cities
#' @param y y-Values from the Cities
#' @param alpha The strength of the pheromone. The higher the stronger.
#' @param beta Relevan of the distance. The higher the more decreases the relevance.
#' @param evaporation Strength with which the pheromones evaporate.
#' @param randomness_factor Factor for random INterations.
#' @param nOfAnts Number of ants.
#' @param iterations Number of iterations.
#'
#' @return All the routes with the minimal distance, possible that this are more than one.
#' @author The code is adopted from: https://github.com/ciessielski/ACOTSP
#' @noRd
aco_tsp <- function(x,y, alpha, beta, evaporation, randomness_factor, nOfAnts, iterations){
  
  
  nOfCities <- 10 #number of cities
  #Create df from the x,y Values and the number of cities
  cities <- data.frame(cid = c(1:nOfCities),x = x, y = y, visited = FALSE, pheromone = 0)
  routes <- data.frame(distance = 0)
  for (i in 1:nOfCities) { routes[,paste0("stop_",i)] = 0 }
  
  trips <- 0
  
  
  
  for (i in 1:iterations)#The number of Iterations determine the numbr of runs
  {
    for (a in 1:nOfAnts) #For every ant
    {
      trips <- trips + 1
      
      current_city_id <- NULL
      next_city_id <- NULL
      routes[trips, "distance"] <- 0
      
      stops <- nOfCities+1
      
      for (j in 1:stops) 
      {
        not_visited <- filter(cities, cities$visited == FALSE)
        if (is.null(current_city_id)) { current_city_id <- 1 } #start all trips from the same place
        
        not_visited$dist_from_current <- ((not_visited$x - cities$x["cid" = current_city_id])^2 + (not_visited$y - cities$x["cid" = current_city_id])^2)^(1/2)
        
        #Calculate the distance with the ACO Formel for the not visited cities
        not_visited$rank <- not_visited$dist_from_current / beta + not_visited$pheromone * alpha
        
        cities$visited[current_city_id] <- TRUE #if visited becomes True
        cities$pheromone[current_city_id] <- cities$pheromone[current_city_id] + 1#Chooses next city and the pheromons become updated
        
        not_visited <- not_visited[-grep(current_city_id,not_visited$cid),]#not vistited cities become updated
        routes[trips,paste0("stop_",j)] <- current_city_id
        
        #randomise:
        if (i <= randomness_factor && j < nOfCities)#chooses randomly next city
        {
          if(nrow(not_visited) == 1)
          {
            next_city_id <- not_visited$cid[1] #Chooses next city
          }
          else
          {
            next_city_id <- sample(not_visited$cid, 1)#Chooses next city
          }
        }
        else #no random city selection
        {
          next_city_id <- not_visited[not_visited$rank==min(not_visited$rank),]$cid #Chooses next city
        }
        
        next_city_row <- filter(not_visited, not_visited$cid == next_city_id)
        distance_to_next <- next_city_row$dist_from_current
        
        if (j < nOfCities) 
        { 
          routes[trips, "distance"] <- routes[trips, "distance"] + distance_to_next #upgrades the distance
          cities$pheromone[current_city_id] <- max(cities$pheromone[current_city_id] - distance_to_next*evaporation, 0)
        }
        current_city_id <- next_city_id
        if (j == nOfCities) { current_city_id <- 1 }
        
      }
      cities$visited <- FALSE
    }
  }
  
  #returns all the routes with the minimal distance, possible that this are more than one
  return(routes[routes$distance==min(routes$distance),])
  
}

#Determine the x-Values
getXValues <- function(){
  x =c(1,5,3,4,8,10,6,9,5,3)
  return(x)
}
#Determine the y-Values
getYValues <- function(){
  y =c(9,12,6,4,8,12,1,6,8,7)
  return(y)
}

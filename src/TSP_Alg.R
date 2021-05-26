
rm(list=ls())
library(dplyr)

# This is the code for the ACO Algorithm
#The code is from the Git Repository: https://github.com/ciessielski/ACOTSP

acoAlg <- function(xVal,yVal, a, b , eva, rf, nAnts, iter){
  #Here we assign the params for the function
  alpha <- a # The strength of the pheromone. As higher -> as stronger
  beta <- b  # Relevanz of the distance. As higher -> as less relevant
  evaporation <- eva # How string the pheromone evaporate
  randomness_factor <- rf #Factor for random Iterations
  nOfCities <- 10 #Number of the cities
  nOfAnts <- nAnts #Number of Ants
  iterations <- iter #ANumber of Iterations
  
  cities <- data.frame(cid = c(1:nOfCities),x = xVal , y = xVal , visited = FALSE, pheromone = 0)#Data Frame from the x and y Values from the cities
  routes <- data.frame(distance = 0)
  
  for (i in 1:nOfCities) { routes[,paste0("stop_",i)] = 0 }
  
  trips <- 0
  
  for (i in 1:iterations)#The number if Iterations determine the number of runs 
  {
    for (a in 1:nOfAnts)#For every ant
    {
      trips <- trips + 1
      
      current_city_id <- NULL
      next_city_id <- NULL
      routes[trips, "distance"] <- 0
      
      stops <- nOfCities+1
      
      for (j in 1:stops) 
      {
        not_visited <- filter(cities, cities$visited == FALSE)
        if (is.null(current_city_id)) { current_city_id <- 1 } #All Ants starts from the selfe point
        
        not_visited$dist_from_current <- ((not_visited$x - cities$x["cid" = current_city_id])^2 + (not_visited$y - cities$x["cid" = current_city_id])^2)^(1/2)
        #calculate the dinstance with the ACO Formel for the not visited cities
        not_visited$rank <- not_visited$dist_from_current / beta + not_visited$pheromone * alpha #calculate the rank with the ACO Formel for the not visited cities
        
        cities$visited[current_city_id] <- TRUE #if visited becomes True
        cities$pheromone[current_city_id] <- cities$pheromone[current_city_id] + 1 # Chooses next city and the pheromons become updated
        
        not_visited <- not_visited[-grep(current_city_id,not_visited$cid),]#not visited cities become updated
        routes[trips,paste0("stop_",j)] <- current_city_id
        
        #randomise:
        if (i <= randomness_factor && j < nOfCities) #Chooses randomly next city
        {
          if(nrow(not_visited) == 1)
          {
            next_city_id <- not_visited$cid[1]#Chooses next city
          }
          else
          {
            next_city_id <- sample(not_visited$cid, 1)#Chooses next city
          }
        }
        else # no random city selectioin 
        {
          next_city_id <- not_visited[not_visited$rank==min(not_visited$rank),]$cid # chooses the next city
        }
        
        next_city_row <- filter(not_visited, not_visited$cid == next_city_id)
        distance_to_next <- next_city_row$dist_from_current
        
        if (j < nOfCities) 
        { 
          routes[trips, "distance"] <- routes[trips, "distance"] + distance_to_next #upgedates the distance
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
  x = c(1,5,6,7,8,1,12,3,15,16)
  return(x)
}
#Determine the y-Values
getYValues <- function(){
  y = c(5,1,9,4,5,9,7,6,5,7) 
  return(y)
}

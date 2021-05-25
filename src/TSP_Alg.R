
rm(list=ls())
library(dplyr)

#Hier ist der Algorihtmus für das TSP Problem.
#Der Code ist von dem Git Repository: https://github.com/ciessielski/ACOTSP

acoAlg <- function(xVal,yVal, a, b , eva, rf, nAnts, iter){
  #Hier werden die Parameter festgelegt
  alpha <- a #Stärke der Pheromone wird hier festgelegt, umo höher -> umso stärker
  beta <- b  #Die Relevanz der Distanz wird hier festgelegt, umso höher -> destro weniger relevant
  evaporation <- eva
  randomness_factor <- rf #Anzahl von zufälligen Interationen -> kann man auch auf 0 Seitzen um keine zu haben
  nOfCities <- 10 #Anzahl der Städte -> wird bei uns immer bei 10 bleiben 
  nOfAnts <- nAnts #Anzahl der Ameisen
  iterations <- iter #Anzahl der Iterationen
  
  cities <- data.frame(cid = c(1:nOfCities),x = xVal , y = xVal , visited = FALSE, pheromone = 0)#Data Frame aus den x,y Werten der Städte
  routes <- data.frame(distance = 0)
  
  for (i in 1:nOfCities) { routes[,paste0("stop_",i)] = 0 }
  
  trips <- 0
  
  for (i in 1:iterations)#So hoch die Anzahl der Iterationen, so oft wird es ausgeführt
  {
    for (a in 1:nOfAnts)#Für jede Ameise 
    {
      trips <- trips + 1
      
      current_city_id <- NULL
      next_city_id <- NULL
      routes[trips, "distance"] <- 0
      
      stops <- nOfCities+1
      
      for (j in 1:stops) 
      {
        not_visited <- filter(cities, cities$visited == FALSE)
        if (is.null(current_city_id)) { current_city_id <- 1 } #Hier wird festgelegt, dass alle Ameisen von der selben Stelle (Stadt starten)
        
        not_visited$dist_from_current <- ((not_visited$x - cities$x["cid" = current_city_id])^2 + (not_visited$y - cities$x["cid" = current_city_id])^2)^(1/2)
        #Distanz zu allen noch nicht besichtigten Städte wird hier berechnet
        not_visited$rank <- not_visited$dist_from_current / beta + not_visited$pheromone * alpha #Rang der Noch nicht besichitgten Stadt wird über die ACO Formel berechnet
        
        cities$visited[current_city_id] <- TRUE #Wenn besichtigt wird auf True gesetzt
        cities$pheromone[current_city_id] <- cities$pheromone[current_city_id] + 1 #Naächste Stadt wird ausgewählt und die Pheromonspur upgedatet
        
        not_visited <- not_visited[-grep(current_city_id,not_visited$cid),]#Nicht besichtigte Städte werden upgedatet
        routes[trips,paste0("stop_",j)] <- current_city_id
        
        #randomise:
        if (i <= randomness_factor && j < nOfCities) #Zufällige auswahl der nächste Stadt
        {
          if(nrow(not_visited) == 1)
          {
            next_city_id <- not_visited$cid[1]#nächste Stadt wird ausgewählt 
          }
          else
          {
            next_city_id <- sample(not_visited$cid, 1)#nächste Stadt wird ausgewählt 
          }
        }
        else #Keine zufällige Auswahl der Stadt
        {
          next_city_id <- not_visited[not_visited$rank==min(not_visited$rank),]$cid # nächste Stadt wird ausgewählt 
        }
        
        next_city_row <- filter(not_visited, not_visited$cid == next_city_id)
        distance_to_next <- next_city_row$dist_from_current
        
        if (j < nOfCities) 
        { 
          routes[trips, "distance"] <- routes[trips, "distance"] + distance_to_next #Distanz wird upgedatet
          cities$pheromone[current_city_id] <- max(cities$pheromone[current_city_id] - distance_to_next*evaporation, 0)
        }
        current_city_id <- next_city_id
        if (j == nOfCities) { current_city_id <- 1 }
        
      }
      cities$visited <- FALSE
    }
  }
  #Ausgabe -> Alle Routen mit dem Min Ergebniss, können auch mehrere sein!
  return(routes[routes$distance==min(routes$distance),])
}

#Hier werden die X-Werte festgelegt
getXValues <- function(){
  x = c(1,5,6,7,8,1,12,3,15,16)
  return(x)
}
#Hier werden die Y-Werte festgelegt
getYValues <- function(){
  y = c(5,1,9,4,5,9,7,6,5,7) 
  return(y)
}

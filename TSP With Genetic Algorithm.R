#Travelling salesman problem: finding the shortest order to visit random set of cities.
#coded by Olavi, 3.11.2017. Tallinn

#Finding solution with genetic algorithm

####Problem setup####

#generate random coordinates for cities
ncities = 6
iterations = 300
popsize = 50

set.seed(6) #to reproduce same coordinates 6
cities = matrix(c(runif(ncities*2)*100), ,ncol = 2)
colnames(cities) = c("X", "Y")
rownames(cities) = c("Belawan","Tanjung Priok","Tanjung Perak","Banjarmasin","Makassar","Sorong")

plot(cities, asp = 1)
text(cities-2,,rownames(cities))

startp = cities[1,] #set first citiy as fixed starting point

#Funciton to calculate distances btw cities
dist = function(order){
  distance = 0

  for(i in 1:ncities-1){
    distance[i] = sqrt((order[i+1,1]-order[i,1])^2 + (order[i+1,2]-order[i,2])^2)
  }

  return(sum(distance))
}

#Function to calculate distance for whole population
distP = function(pop){
  for (i in 1:popsize){
    distance[i] = dist(pop[, ,i])
  }
  return(distance)
}

#Function to calculate fitness, = reversed normalized distance
fitness = function(dista){ #fitn = rep(1/popsize, popsize) for equal fitn
  fitn = 0
  for (i in 1:popsize){
    fitn[i] = 1/dista[i]
  }
  tot = sum(fitn)
  for (i in 1:popsize){
    fitn[i] = fitn[i]/tot
  }
  return(fitn)
}

#Create initial population subsample solution from cities
set.seed(Sys.time())
pop = array(,c(ncities, 2, popsize))

distance=0
for (i in 1:popsize){
  pop[2:ncities, ,i] = cities[sample(nrow(cities)-1)+1,]
  pop[1, ,i] = startp
  distance[i] = dist(pop[, ,i])
}
best = which.min(distance)
worst = which.max(distance)
bestD0 = min(distance)
worstD0 = max(distance)
bestD = bestD0
worstD = worstD0

lines(pop[, ,best] )

####Done with setup, start algo####

#set.seed(Sys.time())
n=0
npop = pop #new population
meanD = 0
sdD = 0

while(n<iterations){


  #Choose parents based on fitness, better fitness increases probability
  fitn = fitness(distance)



  #Produce whole new generation once
  j=1
  while (j < popsize){

    parent = sample(popsize, 2, prob = fitn, replace = TRUE)
    parent1 = parent[1]
    parent2 = parent[2]

    crossp = round(runif(1,min = 2, max = ncities-1)) # random crossoverpoint

    child1 = pop[,,parent1]
    child1[crossp:ncities,]=pop[,,parent2][crossp:ncities,]

    child2 = pop[,,parent2]
    child2[crossp:ncities,]=pop[,,parent1][crossp:ncities,]


    #Replace duplicates of cities/genes in chromosome
    i=1
    while(anyDuplicated(child1)>0){
      child1[anyDuplicated(child1),] = pop[,,parent1][i,]
      i = i + 1
    }

    i=1
    while(anyDuplicated(child2)>0){
      child2[anyDuplicated(child2),] = pop[,,parent2][i,]
      i = i + 1
    }

    #Mutate with probability, by switching genes in genome
    probM = 0.5

    if(rbinom(1,1,probM)){
      pos = round(runif(2, min = 2, max = ncities))
      temp = child1[pos[1],]
      child1[pos[1],] = child1[pos[2],]
      child1[pos[2],] = temp
    }
    if(rbinom(1,1,probM)){
      pos = round(runif(2, min = 2, max = ncities))
      temp = child2[pos[1],]
      child2[pos[1],] = child2[pos[2],]
      child2[pos[2],] = temp
    }


    npop[, ,j] = child1
    npop[, ,j+1] = child2
    j = j + 2

  }
  #Insert part of the best of new pop into worst of old pop
  pop = pop[ , ,order(distance, decreasing=TRUE)] #order pop with shortest distance lastt

  distanceN = distP(npop)
  npop = npop[, , order(distanceN, decreasing=FALSE)] #order npop with shortest first

  cutPoint = 0.7 #how much to renew from old population
  cut = round(popsize*cutPoint) #cut off point
  pop[, ,1: cut] = npop[, ,1: cut] #set old population equal to new generation at cutoff point

  #Recalculate all distances in pop
  distance = distP(pop)
  meanD[n] = mean(distance)
  sdD[n] = sd(distance)

  best_prev = best
  worst = which.max(distance)
  best = which.min(distance)
  worstD = max(distance)
  bestD = min(distance)
  print(c(n, bestD, worstD), digits = 2)

  plot(cities, asp=1)
  text(cities-2,,rownames(cities))
  lines(child1)
  if (best != best_prev){
    lines(pop[, ,best], col= "red" )
  }


  Sys.sleep(0.01)

  n = n + 1

}

####Show best result####
plot(cities, asp=1)
text(cities-2,,rownames(cities))
lines(pop[, ,best], col= "red")

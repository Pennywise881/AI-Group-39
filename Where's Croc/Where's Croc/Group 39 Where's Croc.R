library(WheresCroc)

getTransitionMatrix <- function(edges, numOfWaterHoles)
{
  transitionMatrix <- matrix(0, nrow = numOfWaterHoles, ncol = numOfWaterHoles, byrow = T)
  
  for (waterhole in 1 : numOfWaterHoles)
  {
    options <- c(edges[which(edges[, 1] == waterhole), 2], edges[which(edges[, 2] == waterhole),1], waterhole)

    prob <- 1/length(options)
    
    for (option in options)
    {
      transitionMatrix[[option, waterhole]] <- prob
    }
  }
  return(transitionMatrix)
}

getStateVector <- function(positions, numOfWaterHoles)
{
  if(positions[1] == positions[2])
  {
    stateVector <- rep(1 / (numOfWaterHoles - 1), 40)
    stateVector[positions[1]] <- 0
    return(stateVector)
  }
  else
  {
    stateVector <- rep(1 / (numOfWaterHoles - 2), 40)
    stateVector[positions[1]] <- 0
    stateVector[positions[2]] <- 0
    return(stateVector)
  }
}

getObservationsMatrix <- function(readings, probs, numOfWaterHoles)
{
  matrix <- matrix(0, nrow = numOfWaterHoles, ncol = numOfWaterHoles, byrow = T)
  
  for(waterHole in 1 : numOfWaterHoles)
  {
    saltDNorm <- dnorm(readings[1], probs$salinity[waterHole, 1], probs$salinity[waterHole, 2])
    phosphateDNorm <- dnorm(readings[2], probs$phosphate[waterHole, 1], probs$phosphate[waterHole, 2])
    nitrogenDNorm <- dnorm(readings[3], probs$nitrogen[waterHole, 1], probs$nitrogen[waterHole, 2])
    
    matrix[waterHole, waterHole] = saltDNorm * phosphateDNorm * nitrogenDNorm
  }
  
  return(matrix)
}

findShortestPath <- function(startNode, goalNode, edges)
{
  queue <- list(list(node = startNode, parent = startNode))

  counter <- 1
  foundGoal <- F
  
  while(!foundGoal)
  {
    currentNode <- queue[[counter]]
    
    connectedNodes <- c(edges[which(edges[, 1] == currentNode$node), 2], edges[which(edges[, 2] == currentNode$node), 1])
    
    for(i in 1: length(connectedNodes))
    {
      newNode <- list(node = connectedNodes[i], parent = currentNode$node)
      
      if(newNode$node == goalNode)
      {
        foundGoal <- T
        queue[[length(queue) + 1]] <- newNode 
        break
      }
      
      if(!any(sapply(queue, function(x) x$node == newNode$node)))
      {
        queue[[length(queue) + 1]] <- newNode 
      }
    }
    
    counter <- counter + 1
  }
  
  path <- tracePath(startNode, goalNode, queue, path <- c())
  path <- rev(path)

  return(path)
}

tracePath <- function(startNode, node, queue, path)
{ 
  path[length(path) + 1] <- node
  
  if(node == startNode)
  {
    return(path)
  }
  
  for(i in 1 : length(queue))
  {
    if(queue[[i]]$node == node)
    {
      currentNode <- queue[[i]]
      break
    }
  }
  
  path <- tracePath(startNode, currentNode$parent, queue, path)
  return(path)
}

findCroc <- function(moveInfo, readings, positions, edges, probs)
{ 
  transitionMatrix <- getTransitionMatrix(edges, 40)
  
  if(length(moveInfo$mem$status) == 1)
  {
    prevStateVector <- getStateVector(positions, 40)
  }
  else 
  {
    prevStateVector <- moveInfo$mem$status
    if(!is.na(positions[1]))
    {
      if(positions[1] < 0)prevStateVector[-positions[1]] <- 1
      else prevStateVector[positions[1]] <- 0
    }
    if(!is.na(positions[2]))
    {
      if(positions[2] < 0)prevStateVector[-positions[2]] <- 1
      else prevStateVector[positions[2]] <- 0
    }
  }
  
  observationsMatrix <- getObservationsMatrix(readings, probs, 40)
  
  if(!is.na(positions[1]) && positions[1] < 0)
  {
    currentStateVector <- rep.int(0, 40)
    currentStateVector[-positions[1]] <- 1
  }
  if(!is.na(positions[2]) && positions[2] < 0)
  {
    currentStateVector <- rep.int(0, 40)
    currentStateVector[-positions[2]] <- 1
  }
  else
  {
    currentStateVector <- prevStateVector %*% transitionMatrix %*% observationsMatrix 
  }
  
  moveInfo$mem$status <- currentStateVector
  
  goalNode <- which.max(currentStateVector)
  pathToGoal <- findShortestPath(positions[3], goalNode, edges)
  
  if(length(pathToGoal) == 1)
  {
    moveInfo$moves <- c(pathToGoal[1], 0)
  }
  else if(length(pathToGoal) == 2)
  {
    moveInfo$moves <- c(pathToGoal[2], 0)
  }
  else
  {
    moveInfo$moves <- c(pathToGoal[2], pathToGoal[3])
  }
  
  return(moveInfo)
}

# runWheresCroc(findCroc, doPlot = T, showCroc = T, pause = 1, verbose = T, returnMem = F, mem = NA)
testWC(findCroc, verbose = 2, returnVec = T, n = 500, seed = 21, timeLimit = 300)
library(DeliveryMan)

dim <- 10
nodeMatrix <- matrix(list(), nrow = dim, ncol = dim, byrow = T)
nearestPackage <- -1

initializeNodes <- function(dim)
{
  for (i in 1 : dim)
  {
    for(j in 1 : dim)
    {
      fCost <- 1000
      gCost <- 0
      parentPos <- list(x = 1, y = 1)
      pos <- list(x = i, y = j)
      visited <- F
      
      .GlobalEnv$nodeMatrix[i, j] <- list(list(fCost = fCost, gCost = gCost, pos = pos, parentPos = parentPos, visited = visited))
    }
  } 
}

tracePath <- function(car, node)
{ 
  if(node$parentPos$x == car$x & node$parentPos$y == car$y)
  {
    return(node)
  }
  
  tracePath(car, .GlobalEnv$nodeMatrix[[node$parentPos$x, node$parentPos$y]])
}

getPathToGoal <- function(currentNode, goalNode, car, roads)
{
  while(T)
  {
    # check the node on the right
    if(currentNode$pos$x + 1 <= dim)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x + 1, currentNode$pos$y]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 6, roads, goalNode)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x + 1, currentNode$pos$y]] <- node
      }
    }
    
    #check the node on the left
    if(currentNode$pos$x - 1 >= 1)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x - 1, currentNode$pos$y]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 4, roads, goalNode)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x - 1, currentNode$pos$y]] <- node
      }
    }
    
    # check the node on top
    if(currentNode$pos$y + 1 <= dim)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 8, roads, goalNode)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]] <- node
      }
    }
    
    # check the node on the bottom
    if(currentNode$pos$y - 1 >= 1)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 2, roads, goalNode)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]] <- node
      }
    }
    
    for(i in 1:length(.GlobalEnv$openList))
    {
      if(.GlobalEnv$openList[[i]]$pos$x == goalNode[[1]]$pos$x & .GlobalEnv$openList[[i]]$pos$y == goalNode[[1]]$pos$y)
      {
        nextMove <- tracePath(car, .GlobalEnv$openList[[i]])
        
        if(nextMove$pos$x > car$x)
        {
          car$nextMove = 6
        }
        else if(nextMove$pos$x < car$x)
        {
          car$nextMove = 4
        }
        else if(nextMove$pos$y > car$y)
        {
          car$nextMove = 8
        }
        else if(nextMove$pos$y < car$y)
        {
          car$nextMove = 2
        }
        
        return(car)
      }
    }
    
    .GlobalEnv$openList <- .GlobalEnv$openList[order(sapply(.GlobalEnv$openList, `[[`, i = "fCost"))]
    currentNode <- .GlobalEnv$openList[[1]]
    currentNode$visited = T
    .GlobalEnv$openList <- .GlobalEnv$openList[-1]
    .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y]]$visited <- T
    .GlobalEnv$closedList[[length(.GlobalEnv$closedList) + 1]] <- currentNode
  }
}

getNearestPackage <- function(car, packages)
{
  availablePackages <- which(packages[, 5] == 0)
  minDist <- 10000
  nearestPackage <- -1
  
  for(i in 1 : length(availablePackages))
  {
    nextPackage <- list(x = packages[availablePackages[i], 1], y = packages[availablePackages[i], 2])
    
    dist <- abs(car$x - nextPackage$x) + abs(car$y - nextPackage$y)
    
    if(dist < minDist)
    {
      minDist <- dist
      nearestPackage <- availablePackages[i]
    }
  }

  return(nearestPackage)
}

doAStar <- function(roads, car, packages)
{ 
  initializeNodes(.GlobalEnv$dim)
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$parentPos$x <- car$x
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$parentPos$y <- car$y
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$visited <- TRUE
  
  .GlobalEnv$closedList <- list()
  .GlobalEnv$closedList[[length(.GlobalEnv$closedList) + 1]] <- .GlobalEnv$nodeMatrix[[car$x, car$y]]
  
  if(car$load > 0 & .GlobalEnv$nearestPackage != car$load)
  { 
    .GlobalEnv$nearestPackage <- car$load
  }
  
  if(car$load == 0)
  {
    .GlobalEnv$nearestPackage <- getNearestPackage(car, packages)
    goalNode <- nodeMatrix[packages[.GlobalEnv$nearestPackage, 1], packages[.GlobalEnv$nearestPackage, 2]]
  }else
  {
    goalNode <- nodeMatrix[packages[.GlobalEnv$nearestPackage, 3], packages[.GlobalEnv$nearestPackage, 4]]
  }
  
  currentNode <- .GlobalEnv$closedList[[1]]
  .GlobalEnv$openList <- list()
  
  if(car$x == goalNode[[1]]$pos$x & car$y == goalNode[[1]]$pos$y)
  {
    car$nextMove = 5
    return(car)
  }
  
  car = getPathToGoal(currentNode, goalNode, car, roads)
  
  return(car)
}

checkNode <- function(currentNode, node, dir, roads, goalNode)
{ 
  hCost <- abs(node$pos$x - goalNode[[1]]$pos$x) + abs(node$pos$y - goalNode[[1]]$pos$y)
  
  if(dir == 4)
  {
    gCost <- roads$hroads[currentNode$pos$x - 1, currentNode$pos$y] + currentNode$gCost
  }
  else if(dir == 6)
  {
    gCost <- roads$hroads[currentNode$pos$x, currentNode$pos$y] + currentNode$gCost
  }
  else if(dir == 8)
  {
    gCost <- roads$vroads[currentNode$pos$x, currentNode$pos$y] + currentNode$gCost
  }
  else if(dir == 2)
  {
    gCost <- roads$vroads[currentNode$pos$x, currentNode$pos$y-1] + currentNode$gCost
  }
  
  fCost <- gCost + hCost
  
  if(fCost < node$fCost)
  {
    tempList <- .GlobalEnv$openList
    if(length(tempList) > 0)
    {
      for(i in 1:length(tempList))
      {
        if(identical(node, tempList[[i]]))
        {
          .GlobalEnv$openList[[i]] <-NULL
          break
        }
      }
    }
    
    node$fCost <- fCost
    node$gCost <- gCost
    node$parentPos$x <- currentNode$pos$x
    node$parentPos$y <- currentNode$pos$y
    .GlobalEnv$openList[[length(.GlobalEnv$openList) + 1]] <- node
  }
  
  return(node)
}

# runDeliveryMan(carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T)
testDM(myFunction = doAStar, verbose = 2, returnVec = T, n = 500, seed = 21, timeLimit = 250)

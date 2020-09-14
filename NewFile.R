library(DeliveryMan)

dim <- 10
nodeMatrix <- matrix(list(), nrow = dim, ncol = dim, byrow = T)
nearestPackage <- -1
deliveryMatrix <- matrix(NA, nrow = 5, ncol = 6, byrow = T)
listOfCircuits <- list()
actualPackage  <- -1

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

checkNode <- function(currentNode, node, dir, roads, goalNode)
{
  hCost <- abs(node$pos$x - goalNode[[1]]$pos$x) + abs(node$pos$y - goalNode[[1]]$pos$y)
  gCost <- currentNode$gCost + 10
  
  if(dir == 4)
  {
    tCost <- roads$hroads[currentNode$pos$x - 1, currentNode$pos$y]
  }
  else if(dir == 6)
  {
    tCost <- roads$hroads[currentNode$pos$x, currentNode$pos$y]
  }
  else if(dir == 8)
  {
    tCost <- roads$vroads[currentNode$pos$x, currentNode$pos$y]
  }
  else if(dir == 2)
  {
    tCost <- roads$vroads[currentNode$pos$x, currentNode$pos$y-1]
  }
  
  fCost <- gCost + hCost + tCost
  
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

tracePath <- function(car, node)
{ 
  if(node$parentPos$x == car$x & node$parentPos$y == car$y)
  {
    return(node)
  }
  
  tracePath(car, .GlobalEnv$nodeMatrix[[node$parentPos$x, node$parentPos$y]])
}

getShortestCircuit <- function(circuit, row, cost, dim, connectivityMatrix)
{ 
  circuit[length(circuit) + 1] = row
  if(length(circuit) == dim)
  {
    x <- list(circuit = circuit, cost = cost)
    .GlobalEnv$listOfCircuits[[length(listOfCircuits) + 1]] <- x
    return(1)
  }
  
  for(col in 1 : dim)
  {
    if(connectivityMatrix[[row, col]] > -1 & !(col %in% circuit))
    { 
      getShortestCircuit(circuit, col, cost + connectivityMatrix[[row, col]], dim, connectivityMatrix)
    }
  }
}

getPathToGoal <- function(currentNode, goalNode, car, roads)
{
  # cat("\nGoal node x, y: ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y, "\n")
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
      # print("In this loop")
      if(.GlobalEnv$openList[[i]]$pos$x == goalNode[[1]]$pos$x & .GlobalEnv$openList[[i]]$pos$y == goalNode[[1]]$pos$y)
      {
        # print("Found GOal")
        # .GlobalEnv$foundGoal = T
        # cat("\nFound goal, xPos: ", .GlobalEnv$openList[[i]]$pos$x, ", yPos: ", .GlobalEnv$openList[[i]]$pos$y,"")
        
        # cat("Goal node parent x: ", .GlobalEnv$openList[[i]]$parentPos$x, " Goal node parent y:", .GlobalEnv$openList[[i]]$parentPos$y)
        
        nextMove <- tracePath(car, .GlobalEnv$openList[[i]])
        
        # print(cat(nextMove$pos$x, ", ", nextMove$pos$y))
        
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
        
        # cat("Car next move: ", car$nextMove)
        # break;
        return(car)
      }
    }
    
    # cat("\nGoal node xPos: ", goalNode[[1]]$pos$x, ", Goal node yPos: ", goalNode[[1]]$pos$y)
    # cat("\nLenght of openList: ", length(.GlobalEnv$openList))
    # cat("\nCurrent Node x, y: ", currentNode$pos$x, ", ", currentNode$pos$y,"\n")
    
    # if(car$x == 8 & car$y == 4)
    # {
    #   # cat("\nLenght of openList: ", length(.GlobalEnv$openList))
    #   # plotNodeData()
    #   cat("\nThis is the Goal node x, y: ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y)
    #   cat("\nCurrent Node x, y: ", currentNode$pos$x, ", ", currentNode$pos$y)
    #   cat("\nNearest package: ", .GlobalEnv$nearestPackage)
    #   readline("Press Enter: ")
    # }
    
    .GlobalEnv$openList <- .GlobalEnv$openList[order(sapply(.GlobalEnv$openList, `[[`, i = "fCost"))]
    currentNode <- .GlobalEnv$openList[[1]]
    currentNode$visited = T
    .GlobalEnv$openList <- .GlobalEnv$openList[-1]
    .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y]]$visited <- T
    .GlobalEnv$closedList[[length(.GlobalEnv$closedList) + 1]] <- currentNode
    
    # points(currentNode$pos$x, currentNode$pos$y, pch = 16, col = "blue", cex = 2)
  }
  
  # print(car)
  # a = readline("Press enter: ")
}

createDeliveryMatrix <- function(car, packages, dim)
{ 
  packageCoordList <- list()
  
  for(i in 1 : 5)
  {
    packageCoord <- c(packages[i, 1], packages[i, 2])
    dropOffCoord <- c(packages[i, 3], packages[i, 4])
    packageCoordList[[length(packageCoordList)+1]] <- packageCoord
    packageCoordList[[length(packageCoordList)+1]] <- dropOffCoord
  }
  
  # print(packageCoordList)
  
  connectivityMatrix <- matrix(-1, nrow = dim, ncol = dim, byrow = T)
  
  for(i in 1 : dim)
  {
    for(j in 1 : dim)
    {
      if((i %% 2 == 1 & j == i + 1) | (i %% 2 == 0 & j %% 2 == 1 & j != i - 1))
      {
        coord1 <- packageCoordList[[i]]
        coord2 <- packageCoordList[[j]]
        cost <- abs(coord1[1] - coord2[1]) + abs(coord1[2] - coord2[2])
        connectivityMatrix[[i, j]] <- cost
      }
    }
  }
  
  # print(connectivityMatrix)
  
  for(i in 1 : dim)
  {
    if(i %% 2 == 0)next
    startingCost <- abs(car$x - packageCoordList[[i]][1]) + abs(car$y - packageCoordList[[i]][2])
    getShortestCircuit(c(i), i + 1, startingCost, dim, connectivityMatrix)
  }
  
  .GlobalEnv$listOfCircuits <- .GlobalEnv$listOfCircuits[order(sapply(.GlobalEnv$listOfCircuits, `[[`, i = "cost"))]
  packageOrder <- c()
  
  for(i in 1 : length(.GlobalEnv$listOfCircuits[[1]][[1]]))
  {
    if(.GlobalEnv$listOfCircuits[[1]][[1]][i] %% 2 == 1)
    {
      if(.GlobalEnv$listOfCircuits[[1]][[1]][i] == 1)packageOrder[length(packageOrder) + 1] <- 1
      else if(.GlobalEnv$listOfCircuits[[1]][[1]][i] == 3)packageOrder[length(packageOrder) + 1] <- 2
      else if(.GlobalEnv$listOfCircuits[[1]][[1]][i] == 5)packageOrder[length(packageOrder) + 1] <- 3
      else if(.GlobalEnv$listOfCircuits[[1]][[1]][i] == 7)packageOrder[length(packageOrder) + 1] <- 4
      else if(.GlobalEnv$listOfCircuits[[1]][[1]][i] == 9)packageOrder[length(packageOrder) + 1] <- 5
    }
  }
  
  for(i in 1 : 5)
  {
    .GlobalEnv$deliveryMatrix[i, 1 : 5] <- packages[packageOrder[i], 1 : 5]
    .GlobalEnv$deliveryMatrix[i, 6] <- packageOrder[i]
  }
  # 
  # # print("\n")
  # # print("Package order:")
  # # print(packageOrder)
  # # cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist,"\n")
  # # print("Actual packages matrix: ")
  # # print(packages)
  # # # # print("tempMatrix")
  # # # # print(tempMatrix)
  # print("New packages matrix:")
  # print(.GlobalEnv$deliveryMatrix)
  # # print("done")
}

doAStar <- function(roads, car, packages)
{ 
  initializeNodes(.GlobalEnv$dim)
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$parentPos$x <- car$x
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$parentPos$y <- car$y
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$visited <- TRUE
  
  .GlobalEnv$closedList <- list()
  .GlobalEnv$closedList[[length(.GlobalEnv$closedList) + 1]] <- .GlobalEnv$nodeMatrix[[car$x, car$y]]
  if(length(car$mem) == 0)
  {
    createDeliveryMatrix(car, packages, .GlobalEnv$dim)
    car$mem[[1]] = 1
  }
  
  # readline("Press enter: ")
  
  # cat("\nHello Nearest package: ", .GlobalEnv$nearestPackage, " actual package: ", .GlobalEnv$actualPackage)
  
  if(car$load > 0)
  { 
    # cat("\nThe car is carrying package: ", car$load)
    # cat("\nThe actualpackage: ", .GlobalEnv$actualPackage, "\n")
    # print("Delivery Matrix: ")
    # print(.GlobalEnv$deliveryMatrix)
    # print("Actual Matrix: ")
    # print(packages)
    # cat("\nLOL: ", .GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 6])
    if(.GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 6] != car$load)
    {
      # print("Hey")
      .GlobalEnv$actualPackage <- car$load
      .GlobalEnv$nearestPackage <- which(.GlobalEnv$deliveryMatrix[, 6] == car$load) 
    }
    # cat("Row from delivery matrix: ", .GlobalEnv$nearestPackage, " row from packages matrix: ", .GlobalEnv$actualPackage)
    # cat("\nHello Nearest package: ", .GlobalEnv$nearestPackage, " actual package: ", .GlobalEnv$actualPackage)
  }
  
  .GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 5] <- packages[.GlobalEnv$actualPackage, 5]
  
  if(car$load == 0)
  { 
    # print("This is happening")
    x <- which(.GlobalEnv$deliveryMatrix[, 5] == 0)
    
    .GlobalEnv$nearestPackage <- x[1]
    .GlobalEnv$actualPackage <- .GlobalEnv$deliveryMatrix[nearestPackage, 6]
    # cat("\nNearest package: ", nearestPackage, "actual package: ", .GlobalEnv$actualPackage,"\n")
    goalNode <- .GlobalEnv$nodeMatrix[.GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 1], .GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 2]]
    # print(nodeMatrix[.GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 1], .GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 2]])
    # print(goalNode)
    # cat("\nShowing goal node from doAStar() when car load 0: ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y)  
  }else
  {
    # print("This is happening")
    goalNode <- .GlobalEnv$nodeMatrix[.GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 3], .GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 4]]
    # print(nodeMatrix[.GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 3], .GlobalEnv$deliveryMatrix[.GlobalEnv$nearestPackage, 4]])
    # cat("\nShowing goal node from doAStar(): ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y, "\n")
  }
  
  # cat("\nShowing goal node from doAStar(): ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y, "\n")
  currentNode <- .GlobalEnv$closedList[[1]]
  # .GlobalEnv$foundGoal <- F
  .GlobalEnv$openList <- list()
  
  # print("This is the goal node: ")
  # print(goalNode)
  # readline("Press enter: ")
  
  if(car$x == goalNode[[1]]$pos$x & car$y == goalNode[[1]]$pos$y)
  {
    # cat("\nNearest package: ", nearestPackage, "actual package: ", .GlobalEnv$actualPackage,"\n")
    # print("Actual packages")
    # print(packages)
    # print("Delivery Matrix")
    # print(deliveryMatrix)
    # cat("Car load: ", car$load)
    car$nextMove = 5
    # readline("Press enter: ")
    return(car)
  }
  
  car = getPathToGoal(currentNode, goalNode, car, roads)
  # print("This is the delivery matrix: ")
  # print(deliveryMatrix)
  # print("Actual packages array: ")
  # print(packages)
  # readline("Press enter hiyah: ")
  return(car)
}

set.seed(7774)
runDeliveryMan(carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T)
# testDM(myFunction = doAStar, verbose = 2, returnVec = T, n = 10, seed = 21, timeLimit = 250)
# packages = matrix(sample(1:dim, replace = T, 5 * 5), ncol = 5)
# packages[, 5] = rep(0, 5)
# print(packages)
# roads = makeRoadMatrices(dim)
# roads = updateRoads(roads$hroads, roads$vroads)
# print(roads)
# rm(packages)

# packages = matrix(sample(1:dim, replace = T, 5 * 5), ncol = 5)
# packages[, 5] = rep(0, 5)
# packages[1, 1] = 8
# packages[1, 2] = 3
# packages[1, 3] = 8
# packages[1, 4] = 7
# packages[1, 5] = 0
# packages[2, 1] = 1
# packages[2, 2] = 1
# packages[2, 3] = 7
# packages[2, 4] = 2
# packages[2, 5] = 0
# packages[3, 1] = 5
# packages[3, 2] = 2
# packages[3, 3] = 9
# packages[3, 4] = 2
# packages[3, 5] = 0
# packages[4, 1] = 6
# packages[4, 2] = 5
# packages[4, 3] = 4
# packages[4, 4] = 6
# packages[4, 5] = 0
# packages[5, 1] = 6
# packages[5, 2] = 2
# packages[5, 3] = 9
# packages[5, 4] = 2
# packages[5, 5] = 0
# print(packages)
# car <- list(x = 1, y = 1)

# foo(car, packages, .GlobalEnv$dim)
# 
# foo <- function(car, packages, dim)
# { 
# # print(packages)
# # readline("Press enter: ")
# # availablePackages <- which(packages[, 5] == 0)
# # cat("\nThis is car xpos: ", car$x)
# 
# minDist <- 10000
# nearestPackage <- -1
# for(i in 1 : 5)
# {
#   packagePos <- list(x = packages[i, 1], y = packages[i, 2])
#   packageDropOff <- list(x = packages[i, 3], y = packages[i, 4])
#   dist <- abs(car$x - packagePos$x) + abs(car$y - packagePos$y) +
#     abs(packageDropOff$x - packagePos$x) + abs(packageDropOff$y - packagePos$y)
#   
#   # cat("\nThis is i: ", i, " dist: ", dist)
#   
#   if(dist < minDist)
#   {
#     minDist <- dist
#     nearestPackage <- i
#   }
# }
# # cat("\nPackage coordinates: ", packages[nearestPackage, 1], ", ", packages[nearestPackage, 2])
# # cat("\nPackage drop off point: ", packages[nearestPackage, 3], ", ", packages[nearestPackage, 4], "\n")
# # cat("\nThe nearest package: ", nearestPackage, ", with a distance of: ", minDist, "\n")
# # print("This is the packages matrix:" )
# # print(packages)
# 
# tempMatrix <- matrix(NA, nrow = 5, ncol = 6, byrow = T)
# 
# # print("\nThis is the tempMatrix: \n")
# # tempRow <- tempMatrix[1, 1 : 5]
# # tempMatrix[1, 1 : 5] <- packages[nearestPackage, 1 : 5]
# # tempMatrix[nearestPackage, 1 : 5] <- tempRow
# # print(tempMatrix)
# 
# for(i in 1 : 5)
# {
#   if(i == 1 || i == nearestPackage)
#   {
#     tempMatrix[1, 1 : 5] <- packages[nearestPackage, 1 : 5]
#     tempMatrix[nearestPackage, 1 : 5] <- packages[1, 1 : 5]
#     tempMatrix[1, 6] <- nearestPackage
#     tempMatrix[nearestPackage, 6] <- 1
#   }
#   else
#   {
#     tempMatrix[i, 1 : 5] <- packages[i, 1 : 5]
#     tempMatrix[i, 6] <- i
#   }
# }
# 
# # cat("\nThis is the tempMatrix: \n")
# # print(tempMatrix)
# # cat("\nActual packages matrix: \n")  
# # print(packages)
# # cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist, "\n")
# 
# packageCoordList <- list()
# 
# for(i in 1 : 5)
# {
#   packageCoord <- c(tempMatrix[i, 1], tempMatrix[i, 2])
#   dropOffCoord <- c(tempMatrix[i, 3], tempMatrix[i, 4])
#   packageCoordList[[length(packageCoordList)+1]] <- packageCoord
#   packageCoordList[[length(packageCoordList)+1]] <- dropOffCoord
# }
# 
# # print(packageCoordList)
# connectivityMatrix <- matrix(-1, nrow = dim, ncol = dim, byrow = T)
# 
# for(i in 1 : dim)
# {
#   for(j in 1 : dim)
#   {
#     if(j!=1 & ((i %% 2 == 1 & j == (i + 1)) | (i %% 2 == 0 & j %% 2 == 1) & i != 1))
#     {
#       # connection <- 1
#       coord1 <- packageCoordList[[i]]
#       coord2 <- packageCoordList[[j]]
#       cost <- abs(coord1[1] - coord2[1]) + abs(coord1[2] - coord2[2])
#       connectivityMatrix[[i, j]] <- cost
#     }
#   }
# }
# 
# # print(connectivityMatrix)
# 
# getShortestCircuit(c(1), 2, connectivityMatrix[[1, 2]], dim, connectivityMatrix)
# # print(.GlobalEnv$listOfCircuits)
# .GlobalEnv$listOfCircuits <- .GlobalEnv$listOfCircuits[order(sapply(.GlobalEnv$listOfCircuits, `[[`, i = "cost"))]
# 
# cheapestCircuit <- .GlobalEnv$listOfCircuits[[1]]
# 
# # print(cheapestCircuit[[1]][3])
# packageOrder <- c()
# 
# for(i in 1 : length(cheapestCircuit[[1]]))
# {
#   if(cheapestCircuit[[1]][i] %% 2 == 1)
#   {
#     # print(cheapestCircuit[[1]][i])
#     packageOrder[length(packageOrder) + 1] <- cheapestCircuit[[1]][i]
#   }
# }
# 
# # cat("\n This is the package order: ")
# # print(packageOrder)
# 
# for(i in 1:length(packageOrder))
# {
#   if(packageOrder[i] == 1)packageOrder[i] <- 1
#   else if(packageOrder[i] == 3)packageOrder[i] <- 2
#   else if(packageOrder[i] == 5)packageOrder[i] <- 3
#   else if(packageOrder[i] == 7)packageOrder[i] <- 4
#   else if(packageOrder[i] == 9)packageOrder[i] <- 5
# }
# 
# # print("Temp Matrix: \n")
# # print(tempMatrix)
# # .GlobalEnv$deliveryMatrix <- matrix(0, nrow = 5, ncol = 6, byrow = T)
# 
# for(i in 1 : 5)
# {
#   .GlobalEnv$deliveryMatrix[i, 1 : 6] <- tempMatrix[packageOrder[i], 1 : 6]
#   # x <- which(deliveryMatrix[,] == packages[,])
#   # x = duplicated(rbind(.GlobalEnv$deliveryMatrix, packages))
#   # cat("\nRow: ", i, " of Delivery Matrix is equal to row: ", deliveryMatrix[i, 6], " of packages matrix")
# }
# 
# # print("\n")
# # print("Package order:")
# # print(packageOrder)
# # cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist,"\n")
# # print("Actual packages matrix: ")
# # print(packages)
# # print("New packages matrix:")
# # print(.GlobalEnv$deliveryMatrix)
# # print("done")
# }


# shit <- matrix(1, nrow = 5, ncol = 5, byrow = T)
# deliveryMatrix[, 5] <- shit[, 5]

# deliveryMatrix[1, 5] = 1
# x <- which(deliveryMatrix[, 5] == 0)
# print(x[1])
# for(i in 1 : 100)
# {
#   x = runDeliveryMan(carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0, del = 5, verbose = T)
#   if(is.na(x))
#   {
#     break
#   }
# }

# x = foo(12)
# 
# print(x)
# 
# length(x)
# 
# if(is.na(x))
# {
#   print("X is NA")
# }
# 
# foo <- function(x)
# {
#   if(x == 1)
#   {
#     print("asd")
#   }
#   else
#   {
#     return(NA)
#   }
# }

# plotNodeData <- function()
# {
#   for(i in 1:.GlobalEnv$dim)
#   {
#     for(j in 1:.GlobalEnv$dim)
#     {
#       node = .GlobalEnv$nodeMatrix[[i, j]]
#       if(node$visited == T)
#       {
#         points(node$pos$x, node$pos$y, pch = 16, col = "blue", cex = 2)
#       }else
#       {
#         points(node$pos$x, node$pos$y, pch = 16, col = "orange", cex = 1)
#       }
#     }
#   }
# }
# roads = makeRoadMatrices(dim)

# getNearestPackage <- function(car, packages)
# { 
#   # print(packages)
#   # readline("Press enter: ")
#   # cat("\n")
#   
#   availablePackages <- which(packages[, 5] == 0)
#   # print(availablePackages)
#   
#   minDist <- 10000
#   nearestPackage <- -1
#   for(i in 1 : length(availablePackages))
#   {
#     nextPackage <- list(x = packages[availablePackages[i], 1], y = packages[availablePackages[i], 2])
#     nextDropOff <- list(x = packages[availablePackages[i], 3], y = packages[availablePackages[i], 4])
#     dist <- abs(car$x - nextPackage$x) + abs(car$y - nextPackage$y) +
#             abs(nextDropOff$x - nextPackage$x) + abs(nextDropOff$y - nextPackage$y)
#     
#     if(dist < minDist)
#     {
#       minDist <- dist
#       nearestPackage <- availablePackages[i]
#     }
#   }
#   # cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist)
#   #cat("\nPackage coordinates: ", packages[nearestPackage, 1], ", ", packages[nearestPackage, 2])
#   # cat("\nPackage drop off point: ", packages[nearestPackage, 3], ", ", packages[nearestPackage, 4])
#   return(nearestPackage)
# }

# createDeliveryMatrix(car, .GlobalEnv$packages, .GlobalEnv$dim)

# # packages = matrix(sample(1:dim, replace = T, 5 * 5), ncol = 5)
# # packages[, 5] = rep(0, 5)
# #  
# # 
# runMyDeliveryMan(carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T, roads = .GlobalEnv$roads,  packages = .GlobalEnv$packages)

# runMyDeliveryMan <- function (carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T, roads = roads, packages = packages)
# {
#   roads = makeRoadMatrices(dim)
#   car = list(x = 1, y = 1, wait = 0, load = 0, nextMove = NA,
#              mem = list())
#   # packages = matrix(sample(1:dim, replace = T, 5 * del), ncol = 5)
#   # packages[, 5] = rep(0, del)
#   for (i in 1:turns) {
#     roads = updateRoads(roads$hroads, roads$vroads)
#     if (doPlot) {
#       makeDotGrid(dim, i)
#       plotRoads(roads$hroads, roads$vroads)
#       points(car$x, car$y, pch = 16, col = "blue",
#              cex = 3)
#       plotPackages(packages)
#     }
#     if (car$wait == 0) {
#       if (car$load == 0) {
#         on = packageOn(car$x, car$y, packages)
#         if (on != 0) {
#           packages[on, 5] = 1
#           car$load = on
#         }
#       }
#       else if (packages[car$load, 3] == car$x && packages[car$load,
#                                                           4] == car$y) {
#         packages[car$load, 5] = 2
#         cat("Package delivered")
#         car$load = 0
#         if (sum(packages[, 5]) == 2 * nrow(packages)) {
#           if (verbose)
#             cat("\nCongratulations! You suceeded in",
#                 i, "turns!")
#           return(i)
#         }
#       }
#       # car = carReady(roads, car, packages)
#       # car = processNextMove(car, roads, dim)
#     }
#     else {
#       car$wait = car$wait - 1
#     }
#     if (pause > 0)
#       Sys.sleep(pause)
#   }
#   cat("\nYou failed to complete the task. Try again.")
#   return(NA)
# }

# x = foo(1000)
# print(x)
# 
# 
# foo <- function(x)
# {
#   if(x == 0)
#   {
#     return (x)
#   }
#   
#   foo(x - 1)
# }



# makeDotGrid(dim, 1)
# plotRoads(roads$hroads, roads$vroads)
# packages = matrix(sample(1:dim, replace = T, 5 * 5), ncol = 5)
# packages[, 5] = rep(0, 5)
# plotPackages(packages)
# 
# # print(packages)
# # code for picking the nearest package goes here
# car <- list(x = 1, y = 1, wait = 0, load = 0, nextMove = NA, mem = list())

# getNearestPackage <- function(car)
# {
#   availablePackages <- which(packages[, 5] == 0)
#   minDist <- 10000
#   nearestPackage <- -1
#   for(i in 1 : length(availablePackages))
#   {
#     nextPackage <- list(x = packages[i, 1], y = packages[i, 2])
#     
#     dist <- abs(car$x - nextPackage$x) + abs(car$y - nextPackage$y)
#     if(dist < minDist)
#     {
#       minDist <- dist
#       nearestPackage <- i
#     }
#   }
#   cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist)
#   return(nearestPackage)
# }

# getPathToGoal <- function(currentNode, goalNode, car, roads)
# { 
#   while(!.GlobalEnv$foundGoal)
#   {
#     # check the node on the right
#     if(currentNode$pos$x + 1 < dim)
#     {
#       node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x + 1, currentNode$pos$y]]
#       
#       if(node$visited == F)
#       {
#         node <- checkNode(currentNode, node, 6, roads)
#         .GlobalEnv$nodeMatrix[[currentNode$pos$x + 1, currentNode$pos$y]] <- node
#       }
#     }
#     
#     #check the node on the left
#     if(currentNode$pos$x - 1 >= 1)
#     {
#       node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x - 1, currentNode$pos$y]]
#       
#       if(node$visited == F)
#       {
#         node <- checkNode(currentNode, node, 4, roads)
#         .GlobalEnv$nodeMatrix[[currentNode$pos$x - 1, currentNode$pos$y]] <- node
#       }
#     }
#     
#     # check the node on top
#     if(currentNode$pos$y + 1 < dim)
#     {
#       node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]]
#       
#       if(node$visited == F)
#       {
#         node <- checkNode(currentNode, node, 8, roads)
#         .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]] <- node
#       }
#     }
#     
#     # check the node on the bottom
#     if(currentNode$pos$y - 1 >= 1)
#     {
#       node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]]
#       
#       if(node$visited == F)
#       {
#         node <- checkNode(currentNode, node, 2, roads)
#         .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]] <- node
#       }
#     }
#     
#     # print(length(.GlobalEnv$openList))
#     for(i in 1:length(.GlobalEnv$openList))
#     {
#       if(.GlobalEnv$openList[[i]]$pos$x == goalNode[[1]]$pos$x & .GlobalEnv$openList[[i]]$pos$y == goalNode[[1]]$pos$y)
#       {
#         .GlobalEnv$foundGoal = T
#         # print(cat("Found goal, xPos: ", .GlobalEnv$openList[[i]]$pos$x, ", yPos: ", .GlobalEnv$openList[[i]]$pos$y))
#         
#         # cat("Goal node parent x: ", .GlobalEnv$openList[[i]]$parentPos$x, " Goal node parent y:", .GlobalEnv$openList[[i]]$parentPos$y)
#         
#         nextMove <- tracePath(car, nodeMatrix[[.GlobalEnv$openList[[i]]$parentPos$x, .GlobalEnv$openList[[i]]$parentPos$y]])
#         
#         print(cat(nextMove$pos$x, ", ", nextMove$pos$y))
#         
#         if(nextMove$pos$x > car$x)
#         {
#           car$nextMove = 6
#         }
#         else if(nextMove$pos$x < car$x)
#         {
#           car$nextMove = 4
#         }
#         else if(nextMove$pos$y > car$y)
#         {
#           car$nextMove = 8
#         }
#         else if(nextMove$pos$y < car$y)
#         {
#           car$nextMove = 2
#         }
#         
#         cat("Car next move: ", car$nextMove)
#         break;
#       }
#     }
#     
#     .GlobalEnv$openList <- .GlobalEnv$openList[order(sapply(.GlobalEnv$openList, `[[`, i = "fCost"))]
#     currentNode <- .GlobalEnv$openList[[1]]
#     .GlobalEnv$openList <- .GlobalEnv$openList[-1]
#     .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y]]$visited <- T
#     .GlobalEnv$closedList[[length(.GlobalEnv$closedList) + 1]] <- currentNode
#   }
# }

# checkNode <- function(currentNode, node, dir)
# { 
#   hCost <- abs(node$pos$x - packages[nearestPackage, 1]) + abs(node$pos$y - packages[nearestPackage, 2])
#   gCost <- currentNode$gCost + 10
#   
#   if(dir == 4)
#   {
#     tCost <- .GlobalEnv$roads$hroads[currentNode$pos$x - 1, currentNode$pos$y] 
#   }
#   else if(dir == 6)
#   {
#     tCost <- .GlobalEnv$roads$hroads[currentNode$pos$x, currentNode$pos$y]
#   }
#   else if(dir == 8)
#   {
#     tCost <- .GlobalEnv$roads$vroads[currentNode$pos$x, currentNode$pos$y]
#   }
#   else if(dir == 2)
#   {
#     tCost <- .GlobalEnv$roads$vroads[currentNode$pos$x, currentNode$pos$y-1]
#   }
#   
#   fCost <- gCost + hCost + tCost
#   # print(cat("This is node fCost: ", node$fCost))
#   
#   if(fCost < node$fCost)
#   {
#     # print("This")
#     
#     #check if this node exists in the open list
#     #if it does remove it from the open list
#     # isInOpen <- F
#     # index <- -1
#     tempList <- .GlobalEnv$openList
#     if(length(tempList) > 0)
#     {
#       for(i in 1:length(tempList))
#       {
#         # print(cat("This is the index:", i, " and this is the lenght: ", length(.GlobalEnv$openList)))
#         if(identical(node, tempList[[i]]))
#         {
#           # isInOpen <- T
#           # index <- i
#           .GlobalEnv$openList[[i]] <-NULL
#         }
#       }
#       
#       # if(isInOpen)
#       # {
#       #   .GlobalEnv$openList[[index]] <-NULL
#       # }
#     }
#     
#     node$fCost <- fCost
#     node$gCost <- gCost
#     node$parentPos$x <- currentNode$pos$x
#     node$parentPos$y <- currentNode$pos$y
#     .GlobalEnv$openList[[length(.GlobalEnv$openList) + 1]] <- node
#   }
#   
#   return(node)
# }

# runDeliveryMan(manualDM)

# openList = list()
# openList[[length(openList)+1]] = list(fCost = 14, name = "A")
# openList[[length(openList)+1]] = list(fCost = 13, name = "B")
# print(length(openList))
# openList[[2]] <- NULL
# openList[[length(openList)+1]] = list(fCost = 9, name = "C")
# openList[[length(openList)+1]] = list(fCost = 55, name = "D")

# myMatrix <- matrix(1 : 5, nrow = 5, ncol = 5, byrow = T)
# myMatrix[, 5] <- rep(0, 5)
# myMatrix[1, 1] <- 3
# myMatrix[1, 2] <- 5
# myMatrix[2, 1] <- 5
# myMatrix[2, 2] <- 3
# myMatrix[3, 1] <- 5
# myMatrix[3, 2] <- 1
# myMatrix[4, 1] <- 7
# myMatrix[4, 2] <- 1
# myMatrix[5, 1] <- 9
# myMatrix[5, 2] <- 1
# #print(myMatrix)
# 
# availablePackages <- which(myMatrix[, 5] == 0)
# car <- list(x = 1, y = 1)
# minDist <- 10000
# nearestPackage <- -1
# for(i in 1 : length(availablePackages))
# {
#   nextPackage <- list(x = myMatrix[i, 1], y = myMatrix[i, 2])
#   #print(nextPackage)
#   
#   dist <- abs(car$x - nextPackage$x) + abs(car$y - nextPackage$y)
#   if(dist < minDist)
#   {
#     #dist = abs(car$x - nextPackage$x) + abs(car$y - nextPackage$y)
#     #print(dist)
#     minDist <- dist
#     nearestPackage <- i
#   }
# }
# 
# cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist)

# someFunc <- function(openList)
# {
#   openList[[length(openList)+1]] <<- 3
#   
#   print(cat("OpenList sizeL: ", length(openList)))
# }
# 
# someFunc(openList)


# otherList = list(list(name = "Nafi", age = 33), list(name = "Aariz", age = 6), list("Zaynab"), age = 1)
# 
# openList = list()
# openList[[length(openList)+1]] = otherList[[1]]
# openList[[length(openList)+1]] = otherList[[2]]
# openList[[length(openList)+1]] = otherList[[3]]
# 
# node = otherList[[1]]
# print(openList[3])
# 
# for(i in 1:length(openList))
# {
#   if(identical(node, openList[[i]]))
#   {
#     print("Found match")
#     openList[[i]] = NULL
#   }
# }
# 
# print(openList)

# closedList = list(nodeMatrix[[car$x, car$y]])
# currentNode = closedList[1]
# openList = list()
# openList[length(openList)+1] = currentNode
# print(openList)
# # openList[1 : length(openList)] = NULL
# currentNode %in% openList

# a <- list(
#   list(fCost = 33, gCost = 10, hCost = 9),
#   list(fCost = 58, gCost = 10, hCost = 9),
#   list(fCost = 22, gCost = 10, hCost = 9),
#   list(fCost = 90, gCost = 10, hCost = 9)
# )
# 
# a = a[order(sapply(a, `[[`, i = "fCost"))]
# 
# for(i in 1:length(a))
# {
#   print(a[[i]]$fCost)
# }

#check node on top
# if(currentNode$pos$y + 1 < dim)
# {
#   node = nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]]
#   if(node$visited == F)
#   { 
#     hCost = abs(node$pos$x - myMatrix[nearestPackage, 1]) + abs(node$pos$y - myMatrix[nearestPackage, 2])
#     gCost = 10
#     tCost = roads$vroads[car$x, car$y]
#     node$fCost = gCost + hCost + tCost
#     node$parentPos$x = currentNode$pos$x
#     node$parentPos$y = currentNode$pos$y
#     # nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]] = node
#     # openList[length(openList)+1] = nodeMatrix[currentNode$pos$x, currentNode$pos$y + 1]
#   }
# }
# 
# #check node on the left
# if(currentNode$pos$y - 1 > 1)
# {
#   node = nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]]
#   if(node$visited == F)
#   {
#     hCost = abs(node$pos$x - myMatrix[nearestPackage, 1]) + abs(node$pos$y - myMatrix[nearestPackage, 2])
#     gCost = 10
#     tCost = roads$hroads[car$x - 1, car$y]
#     node$fCost = gCost + hCost + tCost
#     node$parentPos$x = currentNode$pos$x
#     node$parentPos$y = currentNode$pos$y
#     # nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]] = node
#   }
# }
# 
# openList = openList[order(sapply(openList, `[[`, i = "fCost"))]


# openList[length(openList)+1] = list(list(fCost = 15, pos = c(2, 1), parentPos = c(1,1), visited = F))
# openList[length(openList)+1] = list(list(fCost = 14, pos = c(1, 2), parentPos = c(1,1), visited = F))
# openList = openList[order(sapply(openList, `[[`, i = "fCost"))]
# openList[length(openList)+1] = NULL
# goalNode %in% openList

# cat("Cur Node xPos: ", currentNode$pos$x)
# cat("Cur Node yPos: ", currentNode$pos$x)
# cat("Pack xPos: ", myMatrix[nearestPackage, 1])
# cat("Pack yPos: ", myMatrix[nearestPackage, 2])
# cat("Node xPos: ", node$pos$x)
# cat("Pack yPos: ", node$pos$y)

# print(nodeMatrix[[1,2]])

# while(T)
# {
#   if(goalNode %in% openList)
#   {
#     print("Goal node found")
#     break
#   }
#   
#   #this is where A* happens
#   
#   #check right
#   if(currentNode$pos$x < dim)
#   {
#      
#   }
# }
library(DeliveryMan)

dim <- 10
nodeMatrix <- matrix(list(), nrow = dim, ncol = dim, byrow = T)
nearestPackage <- 1

initializeNodes <- function(dim)
{
  # print(dim)
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
  # print(goalNode)
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
  # cat("\n gCost: ", gCost)
  # cat("\n hCost: ", hCost)
  # cat("\n tCost: ", tCost)
  
  if(fCost < node$fCost)
  {
    # print("This")
    
    #check if this node exists in the open list
    #if it does remove it from the open list
    # isInOpen <- F
    # index <- -1
    tempList <- .GlobalEnv$openList
    if(length(tempList) > 0)
    {
      for(i in 1:length(tempList))
      {
        # print(cat("This is the index:", i, " and this is the lenght: ", length(.GlobalEnv$openList)))
        if(identical(node, tempList[[i]]))
        {
          # isInOpen <- T
          # index <- i
          .GlobalEnv$openList[[i]] <-NULL
          break
        }
      }
      
      # if(isInOpen)
      # {
      #   .GlobalEnv$openList[[index]] <-NULL
      # }
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
  # cat("\nNode xPos: ", node$pos$x, ", Node yPos: ", node$pos$y)
  # cat("\nNode parent xPos: ", node$parentPos$x, ", Node parent yPos: ", node$parentPos$y)
  # a = readline("Press enter: ")
  if(node$parentPos$x == car$x & node$parentPos$y == car$y)
  {
    return(node)
  }
  
  tracePath(car, .GlobalEnv$nodeMatrix[[node$parentPos$x, node$parentPos$y]])
}

getNearestPackage <- function(car, packages)
{ 
  print(packages)
  cat("\n")
  
  availablePackages <- which(packages[, 5] == 0)
  print(availablePackages)
  
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
  # cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist)
  #cat("\nPackage coordinates: ", packages[nearestPackage, 1], ", ", packages[nearestPackage, 2])
  # cat("\nPackage drop off point: ", packages[nearestPackage, 3], ", ", packages[nearestPackage, 4])
  return(nearestPackage)
}


getPathToGoal <- function(currentNode, goalNode, car, roads)
{
  cat("\nGoal node x, y: ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y, "\n")
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
    # cat("Current Node x, y: ", currentNode$pos$x, ", ", currentNode$pos$y)
    
    # if(car$x == 2 & car$y == 7)
    # {
    #   # cat("\nLenght of openList: ", length(.GlobalEnv$openList))
    #   plotNodeData()
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

doAStar <- function(roads, car, packages)
{ 
  # cat("Car xPos: ", car$x, " car yPos: ", car$y)
  initializeNodes(.GlobalEnv$dim)
  
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$parentPos$x <- car$x
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$parentPos$y <- car$y
  .GlobalEnv$nodeMatrix[[car$x, car$y]]$visited <- TRUE
  
  .GlobalEnv$closedList <- list()
  .GlobalEnv$closedList[[length(.GlobalEnv$closedList) + 1]] <- .GlobalEnv$nodeMatrix[[car$x, car$y]]
  
  
  if(car$load == 0)
  {
    .GlobalEnv$nearestPackage <- getNearestPackage(car, packages)
    # cat("\nNearest package: ", nearestPackage)
    goalNode <- nodeMatrix[packages[.GlobalEnv$nearestPackage, 1], packages[.GlobalEnv$nearestPackage, 2]]
    # cat("\nShowing goal node from doAStar() when car load 0: ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y)  
  }else
  {
    # cat("\nDropping off package")
    goalNode <- nodeMatrix[packages[.GlobalEnv$nearestPackage, 3], packages[.GlobalEnv$nearestPackage, 4]]
    # cat("\nShowing goal node from doAStar(): ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y)
  }
  
  # cat("\nShowing goal node from doAStar(): ", goalNode[[1]]$pos$x, ", ", goalNode[[1]]$pos$y, "\n")
  currentNode <- .GlobalEnv$closedList[[1]]
  # .GlobalEnv$foundGoal <- F
  .GlobalEnv$openList <- list()
  
  car = getPathToGoal(currentNode, goalNode, car, roads)
  # print(car)
  return(car)
}

runDeliveryMan(carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T)


# plotNodeData <- function()
# {
#   for(i in 1:.GlobalEnv$dim)
#   {
#     for(j in 1:.GlobalEnv$dim)
#     {
#       node = .GlobalEnv$nodeMatrix[[i, j]]
#       if(node$visited == T)
#       {
#         points(node$pos$x, node$pos$y, pch = 16, col = "blue", cex = 1)
#       }else
#       {
#         points(node$pos$x, node$pos$y, pch = 16, col = "orange", cex = 1)
#       }
#     }
#   }
# }
# roads = makeRoadMatrices(dim)
# packages = matrix(sample(1:dim, replace = T, 5 * 5), ncol = 5)
# packages[, 5] = rep(0, 5)
# runMyDeliveryMan(carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T, roads = .GlobalEnv$roads, packages = .GlobalEnv$packages)
#  
# 
# runMyDeliveryMan <- function (carReady = doAStar, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T, roads = roads, packages = packages)
# {
#   # roads = makeRoadMatrices(dim)
#   car = list(x = 1, y = 1, wait = 0, load = 0, nextMove = NA,
#              mem = list())
#   # packages = matrix(sample(1:dim, replace = T, 5 * del), ncol = 5)
#   # packages[, 5] = rep(0, del)
#   for (i in 1:turns) {
#     # roads = updateRoads(roads$hroads, roads$vroads)
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
#       car = carReady(roads, car, packages)
#       car = processNextMove(car, roads, dim)
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

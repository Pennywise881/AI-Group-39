library(DeliveryMan)

dim <- 10
nodeMatrix <- matrix(list(), nrow = dim, ncol = dim, byrow = T)

initializeNodes <- function(dim)
{
  print(dim)
  for (i in 1 : dim)
  {
    for(j in 1 : dim)
    {
      fCost <- 1000
      gCost <- 0
      # hCost <- 0
      parentPos <- list(x = -1, y = -1)
      pos <- list(x = i, y = j)
      visited <- F
      
      .GlobalEnv$nodeMatrix[i, j] <- list(list(fCost = fCost, gCost = gCost, pos = pos, parentPos = parentPos, visited = visited))
    }
  } 
}
#node <- nodeMatrix[[1, 3]]
#node$visited

#roads <- list(hroads = matrix(NA, nrow = dim, ncol = dim, byrow = T), vroads = matrix(NA, nrow = dim, ncol = dim, byrow = T))
#car = list(x = 1, y = 1, load = 0, mem = list(), nextMove = NA)
#packages = matrix(NA, )

#myAStarFunction = function(roads, car, packages)
#{
#nextPackage = which(packages[, 5] == 0)
#print(nextPackage[[1]])
#print(packages)
#cat(sprintf("\"%f\" \"%f\"\n", nextPackage[, 1], nextPackage[, 2]))
#print(car)
#nextMove = 5
#car$nextMove = nextMove
#readline("Press enter to continue: ")
#return(car)
#}

#runDeliveryMan(carReady = myAStarFunction, dim = 10, turns = 2000, doPlot = T, pause = 0.1, del = 5, verbose = T)

makeDotGrid(dim, 1)
plotRoads(roads$hroads, roads$vroads)
packages = matrix(sample(1:dim, replace = T, 5 * 5), ncol = 5)
packages[, 5] = rep(0, 5)
plotPackages(packages)


# code for picking the nearest package goes here
car <- list(x = 1, y = 1, wait = 0, load = 0, nextMove = NA, mem = list())

getNearestPackage <- function(car)
{
  availablePackages <- which(packages[, 5] == 0)
  minDist <- 10000
  nearestPackage <- -1
  for(i in 1 : length(availablePackages))
  {
    nextPackage <- list(x = packages[i, 1], y = packages[i, 2])
    
    dist <- abs(car$x - nextPackage$x) + abs(car$y - nextPackage$y)
    if(dist < minDist)
    {
      minDist <- dist
      nearestPackage <- i
    }
  }
  cat("The nearest package: ", nearestPackage, ", with a distance of: ", minDist)
  return(nearestPackage)
}


initializeNodes(dim)
## this is the starting node
nodeMatrix[[car$x, car$y]]$parentPos$x <- car$x
nodeMatrix[[car$x, car$y]]$parentPos$y <- car$y
nodeMatrix[[car$x, car$y]]$visited <- TRUE

#add the starting node to the closed list
closedList <- list()
closedList[[length(closedList)+1]] <- nodeMatrix[[car$x, car$y]]
# remove next 2 lines out in the actual program
# roads <- makeRoadMatrices(10)
# roads <- updateRoads(roads$hroads, roads$vroads)

# if the car has no load then goal node is nearest package
# else it is the drop off point of the nearest package
if(car$load == 0)
{
  nearestPackage <- getNearestPackage(car)
  cat("\nNearest package: ", nearestPackage)
  goalNode <- nodeMatrix[packages[nearestPackage, 1], packages[nearestPackage, 2]]
}else
{
  goalNode <- nodeMatrix[packages[nearestPackage, 3], packages[nearestPackage, 4]]
}

currentNode <- closedList[[1]]
foundGoal <- F
openList <- list()

getPathToGoal(currentNode, goalNode)

getPathToGoal <- function(currentNode, goalNode)
{ 
  while(!.GlobalEnv$foundGoal)
  {
    # check the node on the right
    if(currentNode$pos$x + 1 < dim)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x + 1, currentNode$pos$y]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 6)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x + 1, currentNode$pos$y]] <- node
      }
    }
    
    #check the node on the left
    if(currentNode$pos$x - 1 >= 1)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x - 1, currentNode$pos$y]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 4)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x - 1, currentNode$pos$y]] <- node
      }
    }
    
    # check the node on top
    if(currentNode$pos$y + 1 < dim)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 8)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y + 1]] <- node
      }
    }
    
    # check the node on the bottom
    if(currentNode$pos$y - 1 >= 1)
    {
      node <- .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]]
      
      if(node$visited == F)
      {
        node <- checkNode(currentNode, node, 2)
        .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y - 1]] <- node
      }
    }
    
    # print(length(.GlobalEnv$openList))
    for(i in 1:length(.GlobalEnv$openList))
    {
      if(.GlobalEnv$openList[[i]]$pos$x == goalNode[[1]]$pos$x & .GlobalEnv$openList[[i]]$pos$y == goalNode[[1]]$pos$y)
      {
        .GlobalEnv$foundGoal = T
        print(cat("Found goal, xPos: ", .GlobalEnv$openList[[i]]$pos$x, ", yPos: ", .GlobalEnv$openList[[i]]$pos$y))
        break;
      }
    }
    
    .GlobalEnv$openList <- .GlobalEnv$openList[order(sapply(.GlobalEnv$openList, `[[`, i = "fCost"))]
    currentNode <- .GlobalEnv$openList[[1]]
    .GlobalEnv$openList <- .GlobalEnv$openList[-1]
    .GlobalEnv$nodeMatrix[[currentNode$pos$x, currentNode$pos$y]]$visited <- T
    .GlobalEnv$closedList[[length(.GlobalEnv$closedList) + 1]] <- currentNode
  }
}

checkNode <- function(currentNode, node, dir)
{ 
  hCost <- abs(node$pos$x - packages[nearestPackage, 1]) + abs(node$pos$y - packages[nearestPackage, 2])
  gCost <- currentNode$gCost + 10
  
  if(dir == 4)
  {
    tCost <- .GlobalEnv$roads$hroads[currentNode$pos$x - 1, currentNode$pos$y] 
  }
  else if(dir == 6)
  {
    tCost <- .GlobalEnv$roads$hroads[currentNode$pos$x, currentNode$pos$y]
  }
  else if(dir == 8)
  {
    tCost <- .GlobalEnv$roads$vroads[currentNode$pos$x, currentNode$pos$y]
  }
  else if(dir == 2)
  {
    tCost <- .GlobalEnv$roads$vroads[currentNode$pos$x, currentNode$pos$y-1]
  }
  
  fCost <- gCost + hCost + tCost
  # print(cat("This is node fCost: ", node$fCost))
  
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

runDeliveryMan(manualDM)

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

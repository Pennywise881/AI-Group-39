library(WheresCroc)

# return the transition matrix
getTransitionMatrix <- function(edges, numOfWaterHoles)
{
  transitionMatrix <- matrix(0, nrow = numOfWaterHoles, ncol = numOfWaterHoles, byrow = T)
  # print("These are the edges: ")
  # print(edges)
  
  # print(edges[which(edges[, 1] == 1), 2])
  
  # matrix = matrix(0, nrow=numOfWaterHoles, ncol=numOfWaterHoles, byrow = T)
  
  for (waterhole in 1 : numOfWaterHoles)
  {
    options <- c(edges[which(edges[, 1] == waterhole), 2], edges[which(edges[, 2] == waterhole),1], waterhole)
    # print(connectedWaterHoles)
    prob <- 1/length(options)
    
    for (option in options)
    {
      transitionMatrix[[option, waterhole]] <- prob
    }

    # print(matrix)
  }
  return(transitionMatrix)
  # print("Dayuuuum")
  # matrix = matrix(0, nrow=numOfWaterHoles, ncol=numOfWaterHoles)
  # for (waterhole in 1:numOfWaterHoles){
  #   options = getOptions(waterhole, edges)
  #   print(options)
  #   # prob = 1/length(options)
  #   # for (option in options) {
  #   #   matrix[option, waterhole] = prob
  #   # }
  # }
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

# getOptions <- function(point, edges)
# {
#   c(edges[which(edges[,1]==point),2],edges[which(edges[,2]==point),1],point)
# }
# return the current state matrix
# return the current observation matrix

findShortestPath <- function(startNode, goalNode, edges)
{
  # cat("This is the startNode, ", startNode, ", this is the goal node: ", goalNode, "\n")
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
  # print("This is a path vector: ")
  # print(pathVector)

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
  
  # prevStateVector <- t(prevStateVector)
  # print("This is the current state vector: ")
  # print(prevStateVector)
  # readline("Press enter: ")
  
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
  # print("This is the current state vector: ")
  # print(currentStateVector)
  
  # cat("\nWater hole with highest likelihood: ", which.max(currentStateVector), " with a value of: ", max(currentStateVector), "\n")
  
  moveInfo$mem$status <- currentStateVector
  # print("This is the move info: ")
  # print(moveInfo)
  
  goalNode <- which.max(currentStateVector)
  pathToGoal <- findShortestPath(positions[3], goalNode, edges)
  # print("This is the path to goal: ")
  # print(pathToGoal)
  
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
  # print(moveInfo)
  # cat("Player is in hole: ", positions[3], " goal node: ", goalNode, "\n")
  # readline("Press enter: ")
  return(moveInfo)
}

# set.seed(18751)
# runWheresCroc(findCroc, doPlot = T, showCroc = T, pause = 1, verbose = T, returnMem = F, mem = NA)

testWC(findCroc, verbose = 2, returnVec = T, n = 500, seed = 21, timeLimit = 300)

# runMyWheresCroc(manualWC, doPlot = T, showCroc = T, pause = 1, verbose = T, returnMem = F, mem = NA)

# runMyWheresCroc <- function (makeMoves, doPlot = T, showCroc = F, pause = 1, verbose = T, 
#           returnMem = F, mem = NA) 
# {
#   positions = sample(1:40, 4)
#   points = getPoints()
#   edges = getEdges()
#   probs = getProbs()
#   move = 0
#   moveInfo = list(moves = c(), mem = list(status = 0))
#   
#   if (!all(is.na(mem))) 
#     moveInfo$mem = mem
#   first = T
#   while (!is.na(positions[1])) {
#     move = move + 1
#     if (!first) {
#       positions[1] = sample(getOptions(positions[1], edges), 
#                             1)
#       if (!is.na(positions[2]) && positions[2] > 0) {
#         positions[2] = sample(getOptions(positions[2], 
#                                          edges), 1)
#       }
#       else if (!is.na(positions[2]) && positions[2] < 0) {
#         positions[2] = NA
#       }
#       if (!is.na(positions[3]) && positions[3] > 0) {
#         positions[3] = sample(getOptions(positions[3], 
#                                          edges), 1)
#       }
#       else if (!is.na(positions[3]) && positions[3] < 0) {
#         positions[3] = NA
#       }
#       if (!is.na(positions[2]) && positions[2] == positions[1]) {
#         positions[2] = -positions[2]
#       }
#       if (!is.na(positions[3]) && positions[3] == positions[1]) {
#         positions[3] = -positions[3]
#       }
#     }
#     else first = F
#     if (doPlot) 
#       plotGameboard(points, edges, move, positions, showCroc)
#     Sys.sleep(pause)
#     readings = getReadings(positions[1], probs)
#     moveInfo = makeMoves(moveInfo, readings, positions[2:4], 
#                          edges, probs)
#     if (length(moveInfo$moves) != 2) {
#       stop("Error! Passed makeMoves function should return a vector of two elements.")
#     }
#     for (m in moveInfo$moves) {
#       if (m == 0) {
#         if (positions[1] == positions[4]) {
#           if (verbose) 
#             cat("\nCongratualations - You got croc at move ", 
#                 move)
#           if (returnMem) {
#             mem = moveInfo$mem
#             mem$status = 1
#             return(list(move = move, mem = mem))
#           }
#           return(move)
#         }
#       }
#       else {
#         if (m %in% getOptions(positions[4], edges)) {
#           positions[4] = m
#         }
#         else {
#           warning("Invalid move.")
#         }
#       }
#     }
#   }
# }

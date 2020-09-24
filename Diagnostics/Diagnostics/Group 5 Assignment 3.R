library(Diagnostics)

# foo <- matrix(data = NA, nrow = 8, ncol = 2, byrow = T)

learnFunction <- function(hist)
{
  print(names(hist))
  
  PnTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  PnTable[1, 1] <- (length(which(hist[, 1] == 0)) + 1) / (length(which(hist[, 1] == 0)) + 1 + length(which(hist[, 1] == 1)) + 1)
  PnTable[1, 2] <- (length(which(hist[, 1] == 1)) + 1) / (length(which(hist[, 1] == 0)) + 1 + length(which(hist[, 1] == 1)) + 1)
  colnames(PnTable) <- c("PN(0)", "PN(1)")
  print(PnTable)
  
  VTBTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  VTBTable[1, 1] <- (length(which(hist[, 3] == 0)) + 1) / (length(which(hist[, 3] == 0)) + 1 + length(which(hist[, 3] == 1)) + 1)
  VTBTable[1, 2] <- (length(which(hist[, 3] == 1)) + 1) / (length(which(hist[, 3] == 0)) + 1 + length(which(hist[, 3] == 1)) + 1)
  colnames(VTBTable) <- c("VTB(0)", "VTB(1)")
  print(VTBTable)

  SmTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  SmTable[1, 1] <- (length(which(hist[, 5] == 0)) + 1) / (length(which(hist[, 5] == 0)) + 1 + length(which(hist[, 5] == 1)) + 1)
  SmTable[1, 2] <- (length(which(hist[, 5] == 1)) + 1) / (length(which(hist[, 5] == 0)) + 1 + length(which(hist[, 5] == 1)) + 1)
  colnames(SmTable) <- c("Sm(0)", "Sm(1)")
  print(SmTable)
  
  # print(length(which(hist[, 4] == 0 & hist[, 3] == 0)))
  # for(i in 1 : dim(.GlobalEnv$foo)[1])
  # {
  #   for(j in 1 : dim(.GlobalEnv$foo)[2])
  #   {
  #     cat("Enter value for: ", "[", i, "]", "[", j, "]")
  #     .GlobalEnv$foo[i, j] <- readline()
  #   }
  # }
  
  .GlobalEnv$foo <- matrix(data = NA, nrow = 10000, ncol = 2, byrow = T)
  colnames(.GlobalEnv$foo) <- c("VTB", "TB")
  .GlobalEnv$foo[, 1] <- hist[, 3]
  .GlobalEnv$foo[, 2] <- hist[, 4]
  
  # cat("\nNumber of rows where VTB = 0 && TB = 0: ", length(which(.GlobalEnv$foo[, 1] == 0 & .GlobalEnv$foo[, 2] == 0)))
  # cat("\nNumber of rows where VTB = 0 && TB = 1: ", length(which(.GlobalEnv$foo[, 1] == 0 & .GlobalEnv$foo[, 2] == 1)))
  # cat("\nNumber of rows where VTB = 1 && TB = 0: ", length(which(.GlobalEnv$foo[, 1] == 1 & .GlobalEnv$foo[, 2] == 0)))
  # cat("\nNumber of rows where VTB = 1 && TB = 1: ", length(which(.GlobalEnv$foo[, 1] == 1 & .GlobalEnv$foo[, 2] == 1)))
  
  TBTable <- matrix(data = NA, nrow = 2, ncol = 2, byrow = T)
  # colnames(TBTable) <- c("VTB = 0 | TB = 0", "VTB = 0 | TB = 1", "VTB = 1 | TB = 0", "VTB = 1 | TB = 1")
  tbCol1Val <- length(which(hist[, 3] == 0 & hist[, 4] == 0)) + 1
  tbCol2Val <- length(which(hist[, 3] == 0 & hist[, 4] == 1)) + 1
  tbCol3Val <- length(which(hist[, 3] == 1 & hist[, 4] == 0)) + 1
  tbCol4Val <- length(which(hist[, 3] == 1 & hist[, 4] == 1)) + 1
  
  # cat("\ntbCol1Val: ", tbCol1Val)
  # cat("\ntbCol2Val: ", tbCol2Val)
  # cat("\ntbCol3Val: ", tbCol3Val)
  # cat("\ntbCol4Val: ", tbCol4Val, "\n")
  
  TBTable[1, 1] <- tbCol1Val / (tbCol1Val + tbCol2Val)
  TBTable[1, 2] <- tbCol2Val / (tbCol1Val + tbCol2Val)
  TBTable[2, 1] <- tbCol3Val / (tbCol3Val + tbCol4Val)
  TBTable[2, 2] <- tbCol4Val / (tbCol3Val + tbCol4Val)
  
  print(TBTable)
  
  BrTable <- matrix(data = NA, nrow = 2, ncol = 2, byrow = T)
  brCol1Val <- length(which(hist[, 5] == 0 & hist[, 7] == 0)) + 1
  brCol2Val <- length(which(hist[, 5] == 0 & hist[, 7] == 1)) + 1
  brCol3Val <- length(which(hist[, 5] == 1 & hist[, 7] == 0)) + 1
  brCol4Val <- length(which(hist[, 5] == 1 & hist[, 7] == 1)) + 1
  
  BrTable[1, 1] <- brCol1Val / (brCol1Val + brCol2Val)
  BrTable[1, 2] <- brCol2Val / (brCol1Val + brCol2Val)
  BrTable[2, 1] <- brCol3Val / (brCol3Val + brCol4Val)
  BrTable[2, 2] <- brCol4Val / (brCol3Val + brCol4Val)
  
  print(BrTable)
  
  LCTable <- matrix(data = NA, nrow = 2, ncol = 2, byrow = T)
  lcCol1Val <- length(which(hist[, 5] == 0 & hist[, 6] == 0)) + 1
  lcCol2Val <- length(which(hist[, 5] == 0 & hist[, 6] == 1)) + 1
  lcCol3Val <- length(which(hist[, 5] == 1 & hist[, 6] == 0)) + 1
  lcCol4Val <- length(which(hist[, 5] == 1 & hist[, 6] == 1)) + 1
  
  LCTable[1, 1] <- lcCol1Val / (lcCol1Val + lcCol2Val)
  LCTable[1, 2] <- lcCol2Val / (lcCol1Val + lcCol2Val)
  LCTable[2, 1] <- lcCol3Val / (lcCol3Val + lcCol4Val)
  LCTable[2, 2] <- lcCol4Val / (lcCol3Val + lcCol4Val)
  
  print(LCTable)
  
  DyTable <- matrix(data = NA, nrow = 4, ncol = 2, byrow = T)
  
  dyCol1Val <- length(which(hist[, 6] == 0 & hist[, 7] == 0 & hist[, 9] == 0)) + 1
  dyCol2Val <- length(which(hist[, 6] == 0 & hist[, 7] == 0 & hist[, 9] == 1)) + 1
  
  dyCol3Val <- length(which(hist[, 6] == 0 & hist[, 7] == 1 & hist[, 9] == 0)) + 1
  dyCol4Val <- length(which(hist[, 6] == 0 & hist[, 7] == 1 & hist[, 9] == 1)) + 1
  
  dyCol5Val <- length(which(hist[, 6] == 1 & hist[, 7] == 0 & hist[, 9] == 0)) + 1
  dyCol6Val <- length(which(hist[, 6] == 1 & hist[, 7] == 0 & hist[, 9] == 1)) + 1
  
  dyCol7Val <- length(which(hist[, 6] == 1 & hist[, 7] == 1 & hist[, 9] == 0)) + 1
  dyCol8Val <- length(which(hist[, 6] == 1 & hist[, 7] == 1 & hist[, 9] == 1)) + 1
  
  DyTable[1, 1] <- dyCol1Val / (dyCol1Val + dyCol2Val)
  DyTable[1, 2] <- dyCol2Val / (dyCol1Val + dyCol2Val)
  DyTable[2, 1] <- dyCol3Val / (dyCol3Val + dyCol4Val)
  DyTable[2, 2] <- dyCol4Val / (dyCol3Val + dyCol4Val)
  DyTable[3, 1] <- dyCol5Val / (dyCol5Val + dyCol6Val)
  DyTable[3, 2] <- dyCol6Val / (dyCol5Val + dyCol6Val)
  DyTable[4, 1] <- dyCol7Val / (dyCol7Val + dyCol8Val)
  DyTable[4, 2] <- dyCol8Val / (dyCol7Val + dyCol8Val)
  
  print(DyTable)
  
  XRTable <- matrix(data = NA, nrow = 8, ncol = 2, byrow = T)
  
  xrtableCol1Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 0))
  xrtableCol2Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 1))
  
  xrtableCol3Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 0))
  xrtableCol4Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 1))
  
  xrtableCol5Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 0))
  xrtableCol6Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 1))
  
  xrtableCol7Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 0))
  xrtableCol8Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 1))
  
  xrtableCol9Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 0))
  xrtableCol10Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 1))
  
  xrtableCol11Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 0))
  xrtableCol12Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 1))
  
  xrtableCol13Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 0))
  xrtableCol14Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 1))
  
  xrtableCol15Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 0))
  xrtableCol16Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 1))
  
  XRTable[1, 1] <- xrtableCol1Val / (xrtableCol1Val + xrtableCol2Val)
  XRTable[1, 2] <- xrtableCol2Val / (xrtableCol1Val + xrtableCol2Val)
  XRTable[2, 1] <- xrtableCol3Val / (xrtableCol3Val + xrtableCol4Val)
  XRTable[2, 2] <- xrtableCol4Val / (xrtableCol3Val + xrtableCol4Val)
  XRTable[3, 1] <- xrtableCol5Val / (xrtableCol5Val + xrtableCol6Val)
  XRTable[3, 2] <- xrtableCol6Val / (xrtableCol5Val + xrtableCol6Val)
  XRTable[4, 1] <- xrtableCol7Val / (xrtableCol7Val + xrtableCol8Val)
  XRTable[4, 2] <- xrtableCol8Val / (xrtableCol7Val + xrtableCol8Val)
  XRTable[5, 1] <- xrtableCol9Val / (xrtableCol9Val + xrtableCol10Val)
  XRTable[5, 2] <- xrtableCol10Val / (xrtableCol9Val + xrtableCol10Val)
  XRTable[6, 1] <- xrtableCol11Val / (xrtableCol11Val + xrtableCol12Val)
  XRTable[6, 2] <- xrtableCol12Val / (xrtableCol11Val + xrtableCol12Val)
  XRTable[7, 1] <- xrtableCol13Val / (xrtableCol13Val + xrtableCol14Val)
  XRTable[7, 2] <- xrtableCol14Val / (xrtableCol13Val + xrtableCol14Val)
  XRTable[8, 1] <- xrtableCol15Val / (xrtableCol15Val + xrtableCol16Val)
  XRTable[8, 2] <- xrtableCol16Val / (xrtableCol15Val + xrtableCol16Val)
  
  print(XRTable)
}

learnFunction(hist)
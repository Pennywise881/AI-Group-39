library(Diagnostics)

# foo <- matrix(data = NA, nrow = 8, ncol = 2, byrow = T)

learnFunction <- function(hist)
{
  print(names(hist))
  
  cat("Table for Pn(Pneumonia): \n")
  PnTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  PnTable[1, 1] <- (length(which(hist[, 1] == 0)) + 1) / (length(which(hist[, 1] == 0)) + 1 + length(which(hist[, 1] == 1)) + 1)
  PnTable[1, 2] <- (length(which(hist[, 1] == 1)) + 1) / (length(which(hist[, 1] == 0)) + 1 + length(which(hist[, 1] == 1)) + 1)
  colnames(PnTable) <- c("Pn = 0", "Pn = 1")
  print(PnTable)
  
  cat("\n\nTable for VTB(Visited a TB location): \n")
  VTBTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  VTBTable[1, 1] <- (length(which(hist[, 3] == 0)) + 1) / (length(which(hist[, 3] == 0)) + 1 + length(which(hist[, 3] == 1)) + 1)
  VTBTable[1, 2] <- (length(which(hist[, 3] == 1)) + 1) / (length(which(hist[, 3] == 0)) + 1 + length(which(hist[, 3] == 1)) + 1)
  colnames(VTBTable) <- c("VTB = 0", "VTB = 1")
  print(VTBTable)
  
  cat("\n\nTable for Sm(Smoker): \n")
  SmTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  SmTable[1, 1] <- (length(which(hist[, 5] == 0)) + 1) / (length(which(hist[, 5] == 0)) + 1 + length(which(hist[, 5] == 1)) + 1)
  SmTable[1, 2] <- (length(which(hist[, 5] == 1)) + 1) / (length(which(hist[, 5] == 0)) + 1 + length(which(hist[, 5] == 1)) + 1)
  colnames(SmTable) <- c("Sm = 0", "Sm = 1")
  print(SmTable)
  
  cat("\n\nTable for TB(Tuberculosis): \n")
  TBTable <- matrix(data = NA, nrow = 1, ncol = 4, byrow = T)
  colnames(TBTable) <- c("(VTB = 0 | TB = 0)", "(VTB = 0 | TB = 1)", "(VTB = 1 | TB = 0)", "(VTB = 1 | TB = 1)")
  
  tbCol1Val <- length(which(hist[, 3] == 0 & hist[, 4] == 0)) + 1
  tbCol2Val <- length(which(hist[, 3] == 0 & hist[, 4] == 1)) + 1
  tbCol3Val <- length(which(hist[, 3] == 1 & hist[, 4] == 0)) + 1
  tbCol4Val <- length(which(hist[, 3] == 1 & hist[, 4] == 1)) + 1

  TBTable[1, 1] <- tbCol1Val / (tbCol1Val + tbCol2Val)
  TBTable[1, 2] <- tbCol2Val / (tbCol1Val + tbCol2Val)
  TBTable[1, 3] <- tbCol3Val / (tbCol3Val + tbCol4Val)
  TBTable[1, 4] <- tbCol4Val / (tbCol3Val + tbCol4Val)
  
  print(TBTable)
  
  cat("\n\nTable for Br(Bronchitis): \n")
  BrTable <- matrix(data = NA, nrow = 1, ncol = 4, byrow = T)
  colnames(BrTable) <- c("(Sm = 0 | Br = 0)", "(Sm = 0 | Br = 1)", "(Sm = 1 | Br = 0)", "(Sm = 1 | Br = 1)")
  
  brCol1Val <- length(which(hist[, 5] == 0 & hist[, 7] == 0)) + 1
  brCol2Val <- length(which(hist[, 5] == 0 & hist[, 7] == 1)) + 1
  brCol3Val <- length(which(hist[, 5] == 1 & hist[, 7] == 0)) + 1
  brCol4Val <- length(which(hist[, 5] == 1 & hist[, 7] == 1)) + 1
  
  BrTable[1, 1] <- brCol1Val / (brCol1Val + brCol2Val)
  BrTable[1, 2] <- brCol2Val / (brCol1Val + brCol2Val)
  BrTable[1, 3] <- brCol3Val / (brCol3Val + brCol4Val)
  BrTable[1, 4] <- brCol4Val / (brCol3Val + brCol4Val)
  
  print(BrTable)
  
  cat("\n\nTable for LC(Lung Cancer): \n")
  LCTable <- matrix(data = NA, nrow = 1, ncol = 4, byrow = T)
  colnames(LCTable) <- c("(Sm = 0 | LC = 0)", "(Sm = 0 | LC = 1)", "(Sm = 1 | LC = 0)", "(Sm = 1 | LC = 1)")
  lcCol1Val <- length(which(hist[, 5] == 0 & hist[, 6] == 0)) + 1
  lcCol2Val <- length(which(hist[, 5] == 0 & hist[, 6] == 1)) + 1
  lcCol3Val <- length(which(hist[, 5] == 1 & hist[, 6] == 0)) + 1
  lcCol4Val <- length(which(hist[, 5] == 1 & hist[, 6] == 1)) + 1
  
  LCTable[1, 1] <- lcCol1Val / (lcCol1Val + lcCol2Val)
  LCTable[1, 2] <- lcCol2Val / (lcCol1Val + lcCol2Val)
  LCTable[1, 3] <- lcCol3Val / (lcCol3Val + lcCol4Val)
  LCTable[1, 4] <- lcCol4Val / (lcCol3Val + lcCol4Val)
  
  print(LCTable)
  
  cat("\n\nTable for Dy(Dyspnea): \n")
  DyTable <- matrix(data = NA, nrow = 1, ncol = 8, byrow = T)
  colnames(DyTable) <- c("(LC = 0 | Br = 0 | Dy = 0)", "(LC = 0 | Br = 0 | Dy = 1)", "(LC = 0 | Br = 1 | Dy = 0)", "(LC = 0 | Br = 1 | Dy = 1)",
                         "(LC = 1 | Br = 0 | Dy = 0)", "(LC = 1 | Br = 0 | Dy = 1)", "(LC = 1 | Br = 1 | Dy = 0)", "(LC = 1 | Br = 1 | Dy = 1)")
  
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
  DyTable[1, 3] <- dyCol3Val / (dyCol3Val + dyCol4Val)
  DyTable[1, 4] <- dyCol4Val / (dyCol3Val + dyCol4Val)
  DyTable[1, 5] <- dyCol5Val / (dyCol5Val + dyCol6Val)
  DyTable[1, 6] <- dyCol6Val / (dyCol5Val + dyCol6Val)
  DyTable[1, 7] <- dyCol7Val / (dyCol7Val + dyCol8Val)
  DyTable[1, 8] <- dyCol8Val / (dyCol7Val + dyCol8Val)
  
  print(DyTable)
  
  cat("\n\nTable for XR(X - ray result): \n")
  XRTable <- matrix(data = NA, nrow = 1, ncol = 16, byrow = T)
  colnames(XRTable) <- c("(Pn = 0 | TB = 0 | LC = 0 | XR = 0)", "(Pn = 0 | TB = 0 | LC = 0 | XR = 1)", 
                         "(Pn = 0 | TB = 0 | LC = 1 | XR = 0)", "(Pn = 0 | TB = 0 | LC = 1 | XR = 1)",
                         "(Pn = 0 | TB = 1 | LC = 0 | XR = 0)", "(Pn = 0 | TB = 1 | LC = 0 | XR = 1)",
                         "(Pn = 0 | TB = 1 | LC = 1 | XR = 0)", "(Pn = 0 | TB = 1 | LC = 1 | XR = 1)",
                         "(Pn = 1 | TB = 0 | LC = 0 | XR = 0)", "(Pn = 1 | TB = 0 | LC = 0 | XR = 1)",
                         "(Pn = 1 | TB = 0 | LC = 1 | XR = 0)", "(Pn = 1 | TB = 0 | LC = 1 | XR = 1)",
                         "(Pn = 1 | TB = 1 | LC = 0 | XR = 0)", "(Pn = 1 | TB = 1 | LC = 0 | XR = 1)",
                         "(Pn = 1 | TB = 1 | LC = 1 | XR = 0)", "(Pn = 1 | TB = 1 | LC = 1 | XR = 1)")
  
  xrtableCol1Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 0)) + 1
  xrtableCol2Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 1)) + 1
  
  xrtableCol3Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 0)) + 1
  xrtableCol4Val <- length(which(hist[, 1] == 0 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 1)) + 1
  
  xrtableCol5Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 0)) + 1
  xrtableCol6Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 1)) + 1
  
  xrtableCol7Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 0)) + 1
  xrtableCol8Val <- length(which(hist[, 1] == 0 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 1)) + 1
  
  xrtableCol9Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 0)) + 1
  xrtableCol10Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 0 & hist[, 8] == 1)) + 1
  
  xrtableCol11Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 0)) + 1
  xrtableCol12Val <- length(which(hist[, 1] == 1 & hist[, 4] == 0 & hist[, 6] == 1 & hist[, 8] == 1)) + 1
  
  xrtableCol13Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 0)) + 1
  xrtableCol14Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 0 & hist[, 8] == 1)) + 1
  
  xrtableCol15Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 0)) + 1
  xrtableCol16Val <- length(which(hist[, 1] == 1 & hist[, 4] == 1 & hist[, 6] == 1 & hist[, 8] == 1)) + 1
  
  XRTable[1, 1] <- xrtableCol1Val / (xrtableCol1Val + xrtableCol2Val)
  XRTable[1, 2] <- xrtableCol2Val / (xrtableCol1Val + xrtableCol2Val)
  XRTable[1, 3] <- xrtableCol3Val / (xrtableCol3Val + xrtableCol4Val)
  XRTable[1, 4] <- xrtableCol4Val / (xrtableCol3Val + xrtableCol4Val)
  XRTable[1, 5] <- xrtableCol5Val / (xrtableCol5Val + xrtableCol6Val)
  XRTable[1, 6] <- xrtableCol6Val / (xrtableCol5Val + xrtableCol6Val)
  XRTable[1, 7] <- xrtableCol7Val / (xrtableCol7Val + xrtableCol8Val)
  XRTable[1, 8] <- xrtableCol8Val / (xrtableCol7Val + xrtableCol8Val)
  XRTable[1, 9] <- xrtableCol9Val / (xrtableCol9Val + xrtableCol10Val)
  XRTable[1, 10] <- xrtableCol10Val / (xrtableCol9Val + xrtableCol10Val)
  XRTable[1, 11] <- xrtableCol11Val / (xrtableCol11Val + xrtableCol12Val)
  XRTable[1, 12] <- xrtableCol12Val / (xrtableCol11Val + xrtableCol12Val)
  XRTable[1, 13] <- xrtableCol13Val / (xrtableCol13Val + xrtableCol14Val)
  XRTable[1, 14] <- xrtableCol14Val / (xrtableCol13Val + xrtableCol14Val)
  XRTable[1, 15] <- xrtableCol15Val / (xrtableCol15Val + xrtableCol16Val)
  XRTable[1, 16] <- xrtableCol16Val / (xrtableCol15Val + xrtableCol16Val)
  
  print(XRTable)

  # x <- paste("(Pn = ",toString(1), "| TB = 1 | LC = 1)", sep = "")
  # print(x)
}

learnFunction(hist)
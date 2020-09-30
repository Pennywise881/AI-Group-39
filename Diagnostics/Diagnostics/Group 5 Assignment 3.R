library(Diagnostics)

learnFunction <- function(hist)
{
  PnTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  PnTable[1, 1] <- (length(which(hist[, 1] == 0)) + 1) / (length(which(hist[, 1] == 0)) + 1 + length(which(hist[, 1] == 1)) + 1)
  PnTable[1, 2] <- (length(which(hist[, 1] == 1)) + 1) / (length(which(hist[, 1] == 0)) + 1 + length(which(hist[, 1] == 1)) + 1)
  colnames(PnTable) <- c("(Pn=0)", "(Pn=1)")

  VTBTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  VTBTable[1, 1] <- (length(which(hist[, 3] == 0)) + 1) / (length(which(hist[, 3] == 0)) + 1 + length(which(hist[, 3] == 1)) + 1)
  VTBTable[1, 2] <- (length(which(hist[, 3] == 1)) + 1) / (length(which(hist[, 3] == 0)) + 1 + length(which(hist[, 3] == 1)) + 1)
  colnames(VTBTable) <- c("(VTB=0)", "(VTB=1)")

  SmTable <- matrix(data = NA, nrow = 1, ncol = 2, byrow = T)
  SmTable[1, 1] <- (length(which(hist[, 5] == 0)) + 1) / (length(which(hist[, 5] == 0)) + 1 + length(which(hist[, 5] == 1)) + 1)
  SmTable[1, 2] <- (length(which(hist[, 5] == 1)) + 1) / (length(which(hist[, 5] == 0)) + 1 + length(which(hist[, 5] == 1)) + 1)
  colnames(SmTable) <- c("(Sm=0)", "(Sm=1)")

  TBTable <- matrix(data = NA, nrow = 1, ncol = 4, byrow = T)
  colnames(TBTable) <- c("(VTB=0|TB=0)","(VTB=0|TB=1)","(VTB=1|TB=0)","(VTB=1|TB=1)")
  
  tbCol1Val <- length(which(hist[, 3] == 0 & hist[, 4] == 0)) + 1
  tbCol2Val <- length(which(hist[, 3] == 0 & hist[, 4] == 1)) + 1
  tbCol3Val <- length(which(hist[, 3] == 1 & hist[, 4] == 0)) + 1
  tbCol4Val <- length(which(hist[, 3] == 1 & hist[, 4] == 1)) + 1

  TBTable[1, 1] <- tbCol1Val / (tbCol1Val + tbCol2Val)
  TBTable[1, 2] <- tbCol2Val / (tbCol1Val + tbCol2Val)
  TBTable[1, 3] <- tbCol3Val / (tbCol3Val + tbCol4Val)
  TBTable[1, 4] <- tbCol4Val / (tbCol3Val + tbCol4Val)

  BrTable <- matrix(data = NA, nrow = 1, ncol = 4, byrow = T)
  colnames(BrTable) <- c("(Sm=0|Br=0)","(Sm=0|Br=1)","(Sm=1|Br=0)","(Sm=1|Br=1)")
  
  brCol1Val <- length(which(hist[, 5] == 0 & hist[, 7] == 0)) + 1
  brCol2Val <- length(which(hist[, 5] == 0 & hist[, 7] == 1)) + 1
  brCol3Val <- length(which(hist[, 5] == 1 & hist[, 7] == 0)) + 1
  brCol4Val <- length(which(hist[, 5] == 1 & hist[, 7] == 1)) + 1
  
  BrTable[1, 1] <- brCol1Val / (brCol1Val + brCol2Val)
  BrTable[1, 2] <- brCol2Val / (brCol1Val + brCol2Val)
  BrTable[1, 3] <- brCol3Val / (brCol3Val + brCol4Val)
  BrTable[1, 4] <- brCol4Val / (brCol3Val + brCol4Val)

  LCTable <- matrix(data = NA, nrow = 1, ncol = 4, byrow = T)
  colnames(LCTable) <- c("(Sm=0|LC=0)","(Sm=0|LC=1)","(Sm=1|LC=0)","(Sm=1|LC=1)")
  lcCol1Val <- length(which(hist[, 5] == 0 & hist[, 6] == 0)) + 1
  lcCol2Val <- length(which(hist[, 5] == 0 & hist[, 6] == 1)) + 1
  lcCol3Val <- length(which(hist[, 5] == 1 & hist[, 6] == 0)) + 1
  lcCol4Val <- length(which(hist[, 5] == 1 & hist[, 6] == 1)) + 1
  
  LCTable[1, 1] <- lcCol1Val / (lcCol1Val + lcCol2Val)
  LCTable[1, 2] <- lcCol2Val / (lcCol1Val + lcCol2Val)
  LCTable[1, 3] <- lcCol3Val / (lcCol3Val + lcCol4Val)
  LCTable[1, 4] <- lcCol4Val / (lcCol3Val + lcCol4Val)
  
  DyTable <- matrix(data = NA, nrow = 1, ncol = 8, byrow = T)
  colnames(DyTable) <- c("(LC=0|Br=0|Dy=0)","(LC=0|Br=0|Dy=1)","(LC=0|Br=1|Dy=0)","(LC=0|Br=1|Dy=1)",
                         "(LC=1|Br=0|Dy=0)","(LC=1|Br=0|Dy=1)","(LC=1|Br=1|Dy=0)","(LC=1|Br=1|Dy=1)")
  
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

  XRTable <- matrix(data = NA, nrow = 1, ncol = 16, byrow = T)
  colnames(XRTable) <- c("(Pn=0|TB=0|LC=0|XR=0)","(Pn=0|TB=0|LC=0|XR=1)",
                         "(Pn=0|TB=0|LC=1|XR=0)","(Pn=0|TB=0|LC=1|XR=1)",
                         "(Pn=0|TB=1|LC=0|XR=0)","(Pn=0|TB=1|LC=0|XR=1)",
                         "(Pn=0|TB=1|LC=1|XR=0)","(Pn=0|TB=1|LC=1|XR=1)",
                         "(Pn=1|TB=0|LC=0|XR=0)","(Pn=1|TB=0|LC=0|XR=1)",
                         "(Pn=1|TB=0|LC=1|XR=0)","(Pn=1|TB=0|LC=1|XR=1)",
                         "(Pn=1|TB=1|LC=0|XR=0)","(Pn=1|TB=1|LC=0|XR=1)",
                         "(Pn=1|TB=1|LC=1|XR=0)","(Pn=1|TB=1|LC=1|XR=1)")
  
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
  
  TempTable <- matrix(data = NA, nrow = 1, ncol = 4, byrow = T)
  colnames(TempTable) <- c("(Mean=0)", "(SD=0)", "(Mean=1)", "(SD=1)")
  
  mean_0 <- mean(hist[hist[, 1] == 0, 2])
  sd_0 <- sd(hist[hist[, 1] == 0, 2])
  mean_1 <- mean(hist[hist[, 1] == 1, 2])
  sd_1 <- sd(hist[hist[, 1] == 1, 2])
  
  TempTable[1, 1] <- mean_0
  TempTable[1, 2] <- sd_0
  TempTable[1, 3] <- mean_1
  TempTable[1, 4] <- sd_1
  
  network <- list(Pn = PnTable, Te = TempTable, VTB = VTBTable, TB = TBTable, Sm = SmTable,
                  LC = LCTable, Br = BrTable, XR = XRTable, Dy = DyTable)
  
  return(network)
}

getProbability <- function(network, case)
{
  colName <- paste("(Pn=", toString(case[1, 1]),")", sep = "")
  probPn <- network$Pn[, colName]
  
  if(case[1, 1] == 0)
  {
    probTe <- dnorm(case[1, 2], mean = network$Te[1, 1], sd = network$Te[1, 2])
  }
  else
  {
    probTe <- dnorm(case[1, 2], mean = network$Te[1, 3], sd = network$Te[1, 4])
  }
  
  colName <- paste("(VTB=", toString(case[1, 3]),")", sep = "")
  probVTB <- network$VTB[, colName]
  
  colName <- paste("(VTB=", toString(case[1, 3]), "|TB=", toString(case[1, 4]), ")", sep = "")
  probTB <- network$TB[, colName]
  
  colName <- paste("(Sm=", toString(case[1, 5]), ")", sep = "")
  probSm <- network$Sm[, colName]
  
  colName <- paste("(Sm=", toString(case[1, 5]), "|LC=", toString(case[1, 6]), ")", sep = "")
  probLC <- network$LC[, colName]
  
  colName <- paste("(Sm=", toString(case[1, 5]), "|Br=", toString(case[1, 7]), ")", sep = "")
  probBr <- network$Br[, colName]
  
  colName <- paste("(Pn=", toString(case[1, 1]), "|TB=", toString(case[1, 4]),
                   "|LC=", toString(case[1, 6]), "|XR=", toString(case[1, 8]), ")", sep = "")
  probXR <- network$XR[, colName]

  colName <- paste("(LC=", toString(case[1, 6]), "|Br=", toString(case[1, 7]),
                   "|Dy=", toString(case[1, 9]), ")", sep = "")
  probDy <- network$Dy[, colName]
  
  prob <- probPn * probTe * probVTB * probTB * probSm * probLC * probBr * probXR * probDy
  
  return(prob)
}

diagnoseFunction <- function(network, cases)
{
  mat <- matrix(data = NA, nrow = dim(cases)[1], ncol = 4, byrow = T)
  numberOfSamples <- 1000
  
  ## using vector of random values ## 
  # randomNumbersArray <- runif(numberOfSamples*dim(cases)[1]*4, min=0, max=1)
  # print(length(randomNumbersArray))
  # count <- 1
  
  for(caseNumber in 1 : dim(cases)[1])
  {
    case <- cases[caseNumber,]
    diseasesIndex <- which(is.na(case))

    for(i in 1 : length(diseasesIndex))
    {
      case[1, diseasesIndex[i]] <- sample(0 : 1, 1)
    }

    oldCase <- case
    newCase <- oldCase

    pOld <- getProbability(network, oldCase)

    sampleMatrix <- matrix(data = NA, nrow = numberOfSamples, ncol = 9, byrow = T)

    for(sample in 1 : numberOfSamples)
    {
      for(i in 1 : length(diseasesIndex))
      {
        newCase <- oldCase

        if(newCase[1, diseasesIndex[i]] == 0)newCase[1, diseasesIndex[i]] <- 1
        else newCase[1, diseasesIndex[i]] <- 0

        pNew <- getProbability(network, newCase)

        if(pNew > pOld)
        {
          pOld <- pNew
          oldCase <- newCase
        }
        else
        {
          pNewBypOld <- pNew / pOld
          # if(randomNumbersArray[count] < pNewBypOld)
          # {
          #   pOld <- pNew
          #   oldCase <- newCase
          # }
          # count <- count + 1
          if(runif(1, 0, 1) < pNewBypOld)
          {
            pOld <- pNew
            oldCase <- newCase
          }
        }

      }

      temp <- matrix(unlist(oldCase), nrow = 1, ncol = 9, byrow = T)
      sampleMatrix[sample,] <- temp[1,]
    }
    
    burn <- numberOfSamples * 0.1
    sampleMatrix <- tail(sampleMatrix, -burn)

    mat[caseNumber, 1] <- (length(which(sampleMatrix[, 1] == 1))) / (numberOfSamples - burn) 
    mat[caseNumber, 2] <- (length(which(sampleMatrix[, 4] == 1))) / (numberOfSamples - burn)
    mat[caseNumber, 3] <- (length(which(sampleMatrix[, 6] == 1))) / (numberOfSamples - burn)
    mat[caseNumber, 4] <- (length(which(sampleMatrix[, 7] == 1))) / (numberOfSamples - burn)
  }
  
  return(mat)
}

runDiagnostics(learnFunction, diagnoseFunction, verbose = 2)


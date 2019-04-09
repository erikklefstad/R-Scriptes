#### Load Libraries ####
library(data.table)
library(dplyr)
library(Hmisc)
library(tibble)
library(reshape2)
library(foreach)
library(svDialogs)

#### General housekeeping ####
# Adjust scientific notation setting
options(scipen = 999)

# Set number of digits
options(digits = 10)

# Set seed
Seed = 123
set.seed(seed = Seed)

#### Functions ####
# Function to identify boolean variables
idBooleanVars <- function(x){
  # Capture unique values for each variable
  types <- lapply(x, unique)
  
  # Eliminate null values from list of unique values
  types <- lapply(types, function(x){
    x[which(is.na(x) == FALSE)]
  })
  
  # Calculate length of unique values for each variable
  types <- lapply(types, length)
  
  # Select variables with only 2 unique values
  x <- select(x, c(names(types[types == 2])))
  
  # Identify variables with 2 unique values other than 0 and 1
  xUniqs <- lapply(x, function(x){
    ifelse(unique(x) != 0 & unique(x) != 1, "nonBool", "Bool")
  })
  
  xUniqs <- lapply(xUniqs, function(x){
    x[which(is.na(x) == FALSE)]
  })
  
  xUniqs <- lapply(xUniqs, unique)
  
  x <- select(x, names(xUniqs[which(xUniqs == "Bool")]))
}

#### Choose datasets ####
# Read in dataset to be imputed
x <- fread(dlg_open()$res)

# Read in target dataset (if applicable)
# y <- fread(choose.files())

# Create target dataset (if applicable)
y <- select(x, target)

y <- y$target

# Adjust dataset of independent variables appropriately
yIDs <- select(x, ID_code)
xIDs <- select(x, ID_code)

# Remove ID field(s) from independent variables dataset
x <- select(x, -ID_code)
x <- select(x, -target)

# Identify boolean variables
xBools <- idBooleanVars(x)

# Identify non-boolean variables
xNums <- select(x, setdiff(names(x), names(xBools)))

# Clear environment and memory
rm(x)
gc()

#### Imputation settings ####
# In this example, any values 2.5 standard deviations from mean will be imputed
SD_Left <- -2.5
SD_Right <- 2.5

#### Imputation process start ####
# Create lists of dataframes
nonNulls <- lapply(xNums, function(x){
  which(is.na(x) == FALSE)
})

xList <- lapply(xNums, function(x){
  x[which(is.na(x) == FALSE)]
})

xList <- lapply(xList, as.numeric)

scaled <- lapply(xList, function(x){
  round(scale(x), 2)
})

# Determine values to be imputed
imputeRight <- lapply(scaled, function(x){
  which(x > SD_Right)
})
imputeLeft <- lapply(scaled, function(x){
  which(x < SD_Left)
})

# Remove any null values
imputeLeft <- imputeLeft[which(is.na(sapply(imputeLeft, "[", 1)) == FALSE)]
imputeRight <- imputeRight[which(is.na(sapply(imputeRight, "[", 1)) == FALSE)]

#### LEFT ####
if(length(imputeLeft) > 0){
  #### Calculate values to impute ####
  # Create lists of names of columns to be imputed
  imputeLeftNames <- names(imputeLeft)
  
  # Create dataset
  xLeft <- xList[imputeLeftNames]
  
  # Create a named list of column numbers for the lower values
  leftLenList <- seq(1, length(imputeLeftNames), 1)
  leftLenList <- setNames(leftLenList, imputeLeftNames)
  
  # Create target
  targetLeft <- lapply(nonNulls[imputeLeftNames], function(x){
    y[x]
  })
  
  # Calculate means
  leftMeans <- lapply(xLeft, mean)
  
  # Calculate standard deviations
  leftSDs <- lapply(xLeft, function(x){
    sd(x) * sqrt((length(x) - 1) / length(x))
  })
  
  # Calculate variances
  leftVars <- lapply(leftSDs, function(x){
    x ^ 2
  })
  
  # Calculate covariance between each column and the target
  leftCovars <- lapply(leftLenList, function(x){
    cov(xLeft[[x]], y = targetLeft[[x]]) * (length(xLeft[[x]]) - 1) / length(xLeft[[x]])
  })
  
  # Calculate means for the target
  tgtMeansLeft <- lapply(targetLeft, mean)
  
  # Calculate standard deviations for the target
  tgtSDsLeft <- lapply(targetLeft, function(x){
    sd(x) * sqrt((length(x) - 1) / length(x))
  })
  
  # Calculate initial correlations to the target
  corrsLeft <- lapply(leftLenList, function(x){
    cor(xLeft[[x]], y = targetLeft[[x]])
  })
  
  # Create lists of dataframes to determine values to impute
  impLeftZs <- scaled[imputeLeftNames]
  
  # Determine which values to impute
  scaledLeftVals <- lapply(impLeftZs, function(x){
    x[which(x >= SD_Left & x < 0)]
  })
  
  # Determine if any lower/left columns do not have values between 0 and the lower std dev limit
  lensLeft <- lapply(scaledLeftVals, length)
  lensLeft <- which(lensLeft == 0)
  
  # Calculate new imputation values for any columns without values between 0 and the lower/upper std dev limit
  if(length(lensLeft) > 0){
    scaledLeftVals[lensLeft] <- lapply(lensLeft, function(x){
      impLeftZs[[x]][
        which(
          abs(impLeftZs[[x]]) == 
            min(abs(impLeftZs[[x]][which(impLeftZs[[x]] < 0)])))
        ]
    })
  }
  
  # Create a list of unique imputation values
  scaledLeftVals <- lapply(scaledLeftVals, function(x){
    unique(x[which(abs(x - SD_Left) == min(abs(x - SD_Left)))])
  })
  
  # Determine which cells in the dataframes have the 'impute to' values
  impLeftZs <- lapply(leftLenList, function(x){
    which(scaledLeftVals[[x]] == impLeftZs[[x]])
  })
  
  # Create lists of raw data to be imputed
  impLeftVals <- xList[imputeLeftNames]
  
  # Reduce raw data to be imputed to the 'impute to' values
  impLeftVals <- lapply(leftLenList, function(x){
    impLeftVals[[x]][impLeftZs[[x]]]
  })
  
  # Create lists with unique 'impute to' values
  impLeftVals <- lapply(impLeftVals, unique)
  
  # Calculate the max/min for each unique 'impute to' list
  impLeftValsMax <- lapply(impLeftVals, max)
  
  # Frow raw lists of data, determine which cells have values to impute
  imputeLeft <- lapply(leftLenList, function(x){
    which(xLeft[[x]] <= impLeftValsMax[[x]])
  })
  
  # Create lists of dataframes with values to impute and the respective target values
  impLeftData <- lapply(leftLenList, function(x){
    data.frame(
      x = xLeft[[x]][imputeLeft[[x]]], 
      y = targetLeft[[x]][imputeLeft[[x]]]
    )
  })
  
  #### Begin imputation value testing process ####
  # Create aggregated dataframes of values to be imputed, the N for each value, and sum of the target
  impLeftDataN <- lapply(impLeftData, function(x){
    inner_join(
      data.frame(
        aggregate(data.frame(N = x$x), list(X = x$x), length)
      ), 
      data.frame(
        aggregate(data.frame(SumY = x$y), list(X = x$x), sum)
      ), 
      by = "X")
  })
  
  # ***TO DO - provide comments
  impLeftDataN <- lapply(leftLenList, function(x){
    data.frame(X = impLeftDataN[[x]]$X, 
               N = impLeftDataN[[x]]$N, 
               Total = length(xList[imputeLeftNames][[x]]), 
               TotalN = c(0, cumsum(impLeftDataN[[x]]$N[1:nrow(impLeftDataN[[x]]) - 1])), 
               SumY = impLeftDataN[[x]]$SumY, 
               CumSumY = c(0, cumsum(impLeftDataN[[x]]$SumY[1:nrow(impLeftDataN[[x]]) - 1])),
               ImpVal = ifelse(impLeftDataN[[x]]$X %in% impLeftVals[[x]], 
                               1, 0), 
               DeltaX = (c(0, impLeftDataN[[x]]$X[2:nrow(impLeftDataN[[x]])]) - 
                           c(0, impLeftDataN[[x]]$X[1:nrow(impLeftDataN[[x]]) - 1])) * 
                 c(0, cumsum(impLeftDataN[[x]]$N[1:nrow(impLeftDataN[[x]]) - 1])), 
               DeltaMeanX = ((
                 c(0, impLeftDataN[[x]]$X[2:nrow(impLeftDataN[[x]])]) - 
                   c(0, impLeftDataN[[x]]$X[1:nrow(impLeftDataN[[x]]) - 1])
               ) * 
                 c(0, cumsum(impLeftDataN[[x]]$N[1:nrow(impLeftDataN[[x]]) - 1]))
               ) /
                 length(xList[imputeLeftNames][[x]]), 
               NewMeanX = leftMeans[[x]] + cumsum(
                 (
                   (
                     c(0, impLeftDataN[[x]]$X[2:nrow(impLeftDataN[[x]])]) - 
                       c(0, impLeftDataN[[x]]$X[1:nrow(impLeftDataN[[x]]) - 1])
                   ) * 
                     c(0, cumsum(impLeftDataN[[x]]$N[1:nrow(impLeftDataN[[x]]) - 1]))
                 ) /
                   length(xList[imputeLeftNames][[x]]))
    )
  })
  
  impLeftDataN <- lapply(impLeftDataN, function(x){
    data.frame(x, 
               OrigMeanDiff = (c(0, x$X[1:length(x$X) - 1]) - c(0, x$NewMeanX[1:length(x$NewMeanX) - 1])) * x$TotalN
    )
  })
  
  impLeftDataN <- lapply(impLeftDataN, function(x){
    data.frame(x, 
               OrigMeanDiffSq = x$OrigMeanDiff ^ 2 / x$TotalN
    )
  })
  
  impLeftDataN <- lapply(impLeftDataN, function(x){
    data.frame(x, 
               NewMeanDiff = x$OrigMeanDiff - (x$TotalN * x$DeltaMeanX) + x$DeltaX
    )
  })
  
  impLeftDataN <- lapply(impLeftDataN, function(x){
    data.frame(x, 
               NewMeanDiffSq = x$NewMeanDiff ^ 2 / x$TotalN
    )
  })
  
  impLeftDataN <- lapply(impLeftDataN, function(x){
    data.frame(x, 
               MeanDiffSqDelta = x$OrigMeanDiffSq - x$NewMeanDiffSq, 
               VarianceDelta = (x$OrigMeanDiffSq - x$NewMeanDiffSq) / (x$Total - x$TotalN)
    )
  })
  
  impLeftDataN <- lapply(leftLenList, function(x){
    data.frame(impLeftDataN[[x]], 
               RunningMeanY = (impLeftDataN[[x]]$CumSumY * (1 - tgtMeansLeft[[x]]) + 
                                 (impLeftDataN[[x]]$TotalN - impLeftDataN[[x]]$CumSumY) * 
                                 (0 - tgtMeansLeft[[x]])) / impLeftDataN[[x]]$TotalN
    )
  })
  
  impLeftDataN <- lapply(leftLenList, function(x){
    data.frame(impLeftDataN[[x]], 
               NewVar = leftVars[[x]] - c(0, cumsum(impLeftDataN[[x]]$VarianceDelta[2:nrow(impLeftDataN[[x]] - 1)]))
    )
  })
  
  impLeftDataN <- lapply(impLeftDataN, function(x){
    data.frame(x, 
               NewSD = (x$NewVar) ^ (1/2),
               DeltaXY = x$DeltaX * x$RunningMeanY / x$Total
    )
  })
  
  impLeftDataN <- lapply(leftLenList, function(x){
    data.frame(impLeftDataN[[x]], 
               NewCovar = leftCovars[[x]] + c(0, cumsum(impLeftDataN[[x]]$DeltaXY[2:nrow(impLeftDataN[[x]])])))
  })
  
  #### Calculate new correlation to target (end imputation value testing process) ####
  impLeftDataN <- lapply(leftLenList, function(x){
    data.frame(impLeftDataN[[x]], 
               NewCorr = impLeftDataN[[x]]$NewCovar / (impLeftDataN[[x]]$NewSD * tgtSDsLeft[[x]]))
  })
  
  # Determine which value provides the best correlation to the target
  toImpLeft <- lapply(impLeftDataN, function(x){
    val <- which(abs(x$NewCorr) == max(abs(x$NewCorr)))
    ifelse(val == 1,
           NA, 
           val)
  })
  
  # Create a list of maximum correlations to target (not really necessary, for testing purposes)
  toImpLeftCorrs <- lapply(leftLenList, function(x){
    ifelse(is.na(toImpLeft[[x]]), 
           NA, 
           impLeftDataN[[x]]$NewCorr[toImpLeft[[x]]])
  })
  
  # Create lists of impute values
  toImpLeft <- lapply(leftLenList, function(x){
    ifelse(is.na(toImpLeft[[x]]), 
           NA, 
           impLeftDataN[[x]]$X[toImpLeft[[x]]])
  })
  
  # Remove NAs from impute value lists
  toImpLeft <- toImpLeft[which(is.na(toImpLeft) == FALSE)]
  
}

#### RIGHT ####
if(length(imputeRight) > 0){
  #### Calculate values to impute ####
  # Create lists of names of columns to be imputed
  imputeRightNames <- names(imputeRight)
  
  # Create list of data columns to impute
  xRight <- xList[imputeRightNames]
  
  # Create list of target columns
  targetRight <- lapply(nonNulls[imputeRightNames], function(x){
    y[x]
  })
  
  # Create a named list of column numbers for the upper values
  rightLenList <- seq(1, length(imputeRightNames), 1)
  rightLenList <- setNames(rightLenList, imputeRightNames)
  
  # Calculate means
  rightMeans <- lapply(xRight, mean)
  
  # Calculate standard deviations
  rightSDs <- lapply(xRight, function(x){
    sd(x) * sqrt((length(x) - 1) / length(x))
  })
  
  # Calculate variances
  rightVars <- lapply(rightSDs, function(x){
    x ^ 2
  })
  
  # Calculate covariance between each column and the target
  rightCovars <- lapply(rightLenList, function(x){
    cov(xRight[[x]], y = targetRight[[x]]) * (length(xRight[[x]]) - 1) / length(xRight[[x]])
  })
  
  # Calculate means for the target
  tgtMeansRight <- lapply(targetRight, mean)
  
  # Calculate standard deviations for the target
  tgtSDsRight <- lapply(targetRight, function(x){
    sd(x) * sqrt((length(x) - 1) / length(x))
  })
  
  # Calculate initial correlations to the target
  corrsRight <- lapply(rightLenList, function(x){
    cor(xRight[[x]], y = targetRight[[x]])
  })
  
  # Create lists of dataframes to determine values to impute
  impRightZs <- scaled[imputeRightNames]
  
  # Determine which values to impute
  scaledRightVals <- lapply(impRightZs, function(x){
    x[which(x <= SD_Right & x > 0)]
  })
  
  # Determine if any upper/right columns do not have values between 0 and the upper std dev limit
  lensRight <- lapply(scaledRightVals, length)
  lensRight <- which(lensRight == 0)
  
  # Calculate new imputation values for any columns without values between 0 and the lower/upper std dev limit
  if(length(lensRight) > 0){
    scaledRightVals[lensRight] <- lapply(lensRight, function(x){
      impRightZs[[x]][
        which(
          abs(impRightZs[[x]]) == 
            min(abs(impRightZs[[x]][which(impRightZs[[x]] > 0)])))
        ]
    })
  }
  
  # Create a list of unique imputation values
  scaledRightVals <- lapply(scaledRightVals, function(x){
    unique(x[which(abs(x - SD_Right) == min(abs(x - SD_Right)))])
  })
  
  # Determine which cells in the dataframes have the 'impute to' values
  impRightZs <- lapply(rightLenList, function(x){
    which(scaledRightVals[[x]] == impRightZs[[x]])
  })
  
  # Create lists of raw data to be imputed
  impRightVals <- xRight
  
  # Reduce raw data to be imputed to the 'impute to' values
  impRightVals <- lapply(rightLenList, function(x){
    impRightVals[[x]][impRightZs[[x]]]
  })
  
  # Create lists with unique 'impute to' values
  impRightVals <- lapply(impRightVals, unique)
  
  # Calculate the max/min for each unique 'impute to' list
  impRightValsMin <- lapply(impRightVals, min)
  
  # Create lists of all raw data for columns needing imputation
  impRightData <- xRight
  
  # Frow raw lists of data, determine which cells have values to impute
  imputeRight <- lapply(rightLenList, function(x){
    which(impRightData[[x]] >= impRightValsMin[[x]])
  })
  
  # Create lists of dataframes with values to impute and the respective target values
  impRightData <- lapply(rightLenList, function(x){
    data.frame(
      x = impRightData[[x]][imputeRight[[x]]], 
      y = targetRight[[x]][imputeRight[[x]]]
    )
  })
  
  #### Begin imputation value testing process ####
  # Create aggregated dataframes of values to be imputed, the N for each value, and sum of the target
  impRightDataN <- lapply(impRightData, function(x){
    inner_join(
      data.frame(
        aggregate(data.frame(N = x$x), list(X = x$x), length)
      ), 
      data.frame(
        aggregate(data.frame(SumY = x$y), list(X = x$x), sum)
      ), 
      by = "X")
  })
  impRightDataN <- lapply(impRightDataN, function(x){
    x[order(x$X, decreasing = TRUE), ]
  })
  
  # ***TO DO - provide comments
  impRightDataN <- lapply(rightLenList, function(x){
    data.frame(X = impRightDataN[[x]]$X,
               N = impRightDataN[[x]]$N,
               Total = length(xRight[[x]]),
               TotalN = c(0, cumsum(impRightDataN[[x]]$N[1:nrow(impRightDataN[[x]]) - 1])),
               SumY = impRightDataN[[x]]$SumY,
               CumSumY = c(0, cumsum(impRightDataN[[x]]$SumY[1:nrow(impRightDataN[[x]]) - 1])),
               ImpVal = ifelse(impRightDataN[[x]]$X %in% impRightVals[[x]],
                               1, 0),
               DeltaX = (c(0, impRightDataN[[x]]$X[2:nrow(impRightDataN[[x]])]) -
                                     c(0, impRightDataN[[x]]$X[1:nrow(impRightDataN[[x]]) - 1])) *
                 c(0, cumsum(impRightDataN[[x]]$N[1:nrow(impRightDataN[[x]]) - 1])),
               DeltaMeanX = ((
                 c(0, impRightDataN[[x]]$X[2:nrow(impRightDataN[[x]])]) -
                   c(0, impRightDataN[[x]]$X[1:nrow(impRightDataN[[x]]) - 1])
               ) *
                 c(0, cumsum(impRightDataN[[x]]$N[1:nrow(impRightDataN[[x]]) - 1]))
               ) /
                 length(xRight[[x]]),
               NewMeanX = rightMeans[[x]] + cumsum(
                 (
                   (
                     c(0, impRightDataN[[x]]$X[2:nrow(impRightDataN[[x]])]) -
                       c(0, impRightDataN[[x]]$X[1:nrow(impRightDataN[[x]]) - 1])
                   ) *
                     c(0, cumsum(impRightDataN[[x]]$N[1:nrow(impRightDataN[[x]]) - 1]))
                 ) /
                   length(xRight[[x]])))
  })
  
  impRightDataN <- lapply(impRightDataN, function(x){
    data.frame(x, 
               OrigMeanDiff = (c(0, x$X[1:length(x$X) - 1]) - c(0, x$NewMeanX[1:length(x$NewMeanX) - 1])) * x$TotalN)
  })
  
  impRightDataN <- lapply(impRightDataN, function(x){
    data.frame(x, 
               OrigMeanDiffSq = x$OrigMeanDiff ^ 2 / x$TotalN)
  })
  
  impRightDataN <- lapply(impRightDataN, function(x){
    data.frame(x, 
               NewMeanDiff = x$OrigMeanDiff - (x$TotalN * x$DeltaMeanX) + x$DeltaX
    )
  })
  
  impRightDataN <- lapply(impRightDataN, function(x){
    data.frame(x, 
               NewMeanDiffSq = x$NewMeanDiff ^ 2 / x$TotalN
    )
  })
   
  impRightDataN <- lapply(impRightDataN, function(x){
    data.frame(x, 
               MeanDiffSqDelta = x$OrigMeanDiffSq - x$NewMeanDiffSq, 
               VarianceDelta = (x$OrigMeanDiffSq - x$NewMeanDiffSq) / (x$Total - x$TotalN)
    )
  })
  
  impRightDataN <- lapply(rightLenList, function(x){
    data.frame(impRightDataN[[x]], 
               RunningMeanY = (impRightDataN[[x]]$CumSumY * (1 - tgtMeansRight[[x]]) + 
                                 (impRightDataN[[x]]$TotalN - impRightDataN[[x]]$CumSumY) * 
                                 (0 - tgtMeansRight[[x]])) / impRightDataN[[x]]$TotalN
    )
  })
  
  impRightDataN <- lapply(rightLenList, function(x){
    data.frame(impRightDataN[[x]], 
               NewVar = rightVars[[x]] - c(0, cumsum(impRightDataN[[x]]$VarianceDelta[2:nrow(impRightDataN[[x]] - 1)]))
    )
  })
  
  impRightDataN <- lapply(impRightDataN, function(x){
    data.frame(x, 
               NewSD = (x$NewVar) ^ (1/2),
               DeltaXY = x$DeltaX * x$RunningMeanY / x$Total
    )
  })
  
  impRightDataN <- lapply(rightLenList, function(x){
    data.frame(impRightDataN[[x]], 
               NewCovar = rightCovars[[x]] + c(0, cumsum(impRightDataN[[x]]$DeltaXY[2:nrow(impRightDataN[[x]])])))
  })
  
  #### Calculate new correlation to target (end imputation value testing process) ####
  # Calculate new correlation values
  impRightDataN <- lapply(rightLenList, function(x){
    data.frame(impRightDataN[[x]], 
               NewCorr = impRightDataN[[x]]$NewCovar / (impRightDataN[[x]]$NewSD * tgtSDsRight[[x]]))
  })
  
  # Determine which value provides the best correlation to the target (if any, else NA)
  toImpRight <- lapply(impRightDataN, function(x){
    ifelse(which(abs(x$NewCorr) == max(abs(x$NewCorr))) == 1,
           NA, 
           which(abs(x$NewCorr) == max(abs(x$NewCorr))))
  })
  
  # Create a list of maximum correlations to target (not really necessary, for testing purposes)
  toImpRightCorrs <- lapply(rightLenList, function(x){
    ifelse(is.na(toImpRight[[x]]), 
           NA, 
           impRightDataN[[x]]$NewCorr[toImpRight[[x]]])
  })
  
  # Create lists of impute values
  toImpRight <- lapply(rightLenList, function(x){
    ifelse(is.na(toImpRight[[x]]), 
           NA, 
           impRightDataN[[x]]$X[toImpRight[[x]]])
  })
  
  # Remove NAs from impute value lists
  toImpRight <- toImpRight[which(is.na(toImpRight) == FALSE)]
}

#### Clear environment and memory ####
saveList <- c("x", "xBools", "xIDs", "xNums", "y", "toImpLeft", "toImpRight", "idBooleanVars", "saveList", "SD_Left", "SD_Right", 
              "impRightDataN", "impLeftDataN")
rmList <- setdiff(ls(), saveList)
rm(list = rmList)
gc()

#### Create outlier imputation flag columns ####
xNums <- data.frame(xNums)

# Calculate total number of rows/CustSiteIDs
totalN <- nrow(xNums)

# Update names of columns with values to impute
imputeLeftNames <- try(names(toImpLeft), TRUE)
imputeRightNames <- try(names(toImpRight), TRUE)

# Create list of column names for columns with upper & lower, upper only, and lower only imputed values
imputeBothNames <- try(intersect(imputeLeftNames, imputeRightNames), TRUE)

# Variables to test if which tails contain values to impute
L <- class(imputeLeftNames)
R <- class(imputeRightNames)
B <- length(imputeBothNames)

# Create named list of column numbers for columns with lower and upper imputations
if(B > 0){
  # Update names of columns with values to impute
  imputeLeftNames <- setdiff(imputeLeftNames, imputeBothNames)
  imputeRightNames <- setdiff(imputeRightNames, imputeBothNames)
  
  # Create named list for later apply functions
  bothLenList <- seq(1, length(imputeBothNames), 1)
  bothLenList <- setNames(bothLenList, imputeBothNames)
  
  # Create imputed flag columns
  impBothRows <- lapply(imputeBothNames, function(x){
    data.frame(flag = 
                 ifelse((xNums[x] < toImpLeft[x][[1]] & is.na(xNums[x]) == FALSE), 
                        1, 
                        ifelse((xNums[x] > toImpRight[x][[1]] & is.na(xNums[x]) == FALSE), 
                               1, 
                               0)
                 ))
  })
  
  # Create names for the imputed flag columns
  flagNamesBoth <- lapply(bothLenList, function(x){
    ifelse(toImpLeft[[x]] >= 0 & toImpRight[[x]] >= 0, 
           paste(imputeBothNames[[x]], "to", round(toImpLeft[[x]], 2), "above", round(toImpRight[[x]], 2), "IMP_Flag", sep = "_"), 
           ifelse(toImpLeft[[x]] < 0 & toImpRight[[x]] >= 0, 
                  paste(imputeBothNames[[x]], "to_NEG", abs(round(toImpLeft[[x]], 2)), "above", round(toImpRight[[x]], 2), "IMP_Flag", sep = "_"), 
                  ifelse(toImpLeft[[x]] >= 0 & toImpRight[[x]] < 0, 
                         paste(imputeBothNames[[x]], "to", round(toImpLeft[[x]], 2), "above_NEG", abs(round(toImpRight[[x]], 2)), "IMP_Flag", sep = "_"), 
                         paste(imputeBothNames[[x]], "to_NEG", abs(round(toImpLeft[[x]], 2)), "above_NEG", abs(round(toImpRight[[x]], 2)), "IMP_Flag", sep = "_"))))
  })
  
  # Convert to dataframe
  impBothRows <- data.frame(impBothRows)
  
  # Capture flag column names and apply to flag dataframe
  flagNamesBoth <- c(sapply(flagNamesBoth, "[[", 1))
  names(impBothRows) <- unname(flagNamesBoth)
}

# Update named list of column numbers for the lower values
if(L != "try-error"){
  # Create named list for later apply functions
  leftLenList <- seq(1, length(imputeLeftNames), 1)
  leftLenList <- setNames(leftLenList, imputeLeftNames)
  
  # Create imputed flag columns
  impLeftRows <- lapply(leftLenList, function(x){
    data.frame(
      ifelse(xNums[imputeLeftNames[x]] < toImpLeft[[x]] & is.na(xNums[imputeLeftNames[x]]) == FALSE, 
             1, 
             0)
    )
  })
  
  # Create names for the imputed flag columns
  flagNamesLeft <- lapply(leftLenList, function(x){
    ifelse(toImpLeft[[x]] >= 0, 
           paste(imputeLeftNames[[x]], "to", round(toImpLeft[[x]], 2), "IMP_Flag", sep = "_"), 
           paste(imputeLeftNames[[x]], "to_NEG", round(abs(toImpLeft[[x]]), 2), "IMP_Flag", sep = "_")
    )
  })
  
  # Convert to dataframe
  impLeftRows <- data.frame(impLeftRows)
  
  # Capture flag column names and apply to flag dataframe
  flagNamesLeft <- c(sapply(flagNamesLeft, "[[", 1))
  names(impLeftRows) <- unname(flagNamesLeft)
}

# Update named list of column numbers for the upper values
if(R != "try-error"){
  # Create named list for later apply functions
  rightLenList <- seq(1, length(imputeRightNames), 1)
  rightLenList <- setNames(rightLenList, imputeRightNames)
  
  # Create imputed flag columns
  impRightRows <- lapply(rightLenList, function(x){
    data.frame(
      ifelse(xNums[imputeRightNames[x]] > toImpRight[[x]] & is.na(xNums[imputeRightNames[x]]) == FALSE, 
             1, 
             0)
    )
  })
  
  # Create names for the imputed flag columns
  flagNamesRight <- lapply(rightLenList, function(x){
    ifelse(toImpRight[[x]] >= 0, 
           paste(imputeRightNames[[x]], "above", round(toImpRight[[x]], 2), "IMP_Flag", sep = "_"), 
           paste(imputeRightNames[[x]], "above_NEG", round(abs(toImpRight[[x]]), 2), "IMP_Flag", sep = "_")
    )
  })
  
  # Convert to dataframe
  impRightRows <- data.frame(impRightRows)
  
  # Capture flag column names and apply to flag dataframe
  flagNamesRight <- c(sapply(flagNamesRight, "[[", 1))
  names(impRightRows) <- unname(flagNamesRight)
}

#### Impute raw data ####
if(L != "try-error"){
  for(j in 1:length(toImpLeft)){
    colName <- names(toImpLeft[j])
    xNums[colName][which(xNums[colName] < toImpLeft[[j]]), ] <- toImpLeft[[j]]
  }
}

if(R != "try-error"){
  for(k in 1:length(toImpRight)){
    colName <- names(toImpRight[k])
    xNums[colName][which(xNums[colName] > toImpRight[[k]]), ] <- toImpRight[[k]]
  }
}

#### Clear environment and memory ####
saveList <- c("x", "xBools", "xIDs", "xNums", "y", "toImpLeft", "toImpRight", "idBooleanVars", "saveList", "SD_Left", "SD_Right", 
              "impRightDataN", "impLeftDataN", "L", "R", "B", "N", "impBothRows", "impLeftRows", "impRightRows")
rmList <- setdiff(ls(), saveList)
rm(list = rmList)
gc()

#### Capture correlation stats ####
impLeftDataN <- lapply(impLeftDataN, function(x){
  select(x, X, N, NewMeanX, NewCorr)
})

impRightDataN <- lapply(impRightDataN, function(x){
  select(x, X, N, NewMeanX, NewCorr)
})

#### NULLs ####
xNullsLen <- lapply(xNums, function(x){
  length(which(is.na(x) == TRUE))
})

N <- length(which(xNullsLen > 0))
nN <- length(which(xNullsLen == 0))

if(nN > 0){
  xNoNulls <- select(xNums, names(xNullsLen[which(xNullsLen == 0)]))
}

if(N > 0){
  xNullsLen <- xNullsLen[which(xNullsLen != 0)]
  
  xNulls <- select(xNums, names(xNullsLen))
  
  xNulls <- lapply(xNulls, as.numeric)
  
  xNullFlags <- lapply(xNulls, function(x){
    ifelse(is.na(x) == TRUE, 1, 0)
  })
  
  xNullsLenList <- seq(1, length(xNulls), 1)
  xNullsLenList <- setNames(xNullsLenList, names(xNulls))
  
  xNullsMedian <- lapply(xNulls, function(x){
    median(x, na.rm = TRUE)
  })
  
  xNullsMedianData <- lapply(xNullsLenList, function(x){
    ifelse(is.na(xNulls[[x]]), xNullsMedian[[x]], xNulls[[x]])
  })
  
  xNulls0 <- lapply(xNullsLenList, function(x){
    ifelse(is.na(xNulls[[x]]), 0, xNulls[[x]])
  })
  
  xNullsMedianCorr <- lapply(xNullsMedianData, function(x){
    abs(cor(x, y = y))
  })
  
  xNulls0Corr <- lapply(xNulls0, function(x){
    abs(cor(x, y = y))
  })
  
  xNullsImpList <- lapply(xNullsLenList, function(x){
    ifelse(xNullsMedianCorr[[x]] > xNulls0Corr[[x]], "med", "zero")
  })
  
  xNullsMedianData <- xNullsMedianData[which(xNullsImpList == "med")]
  xNullsMedianCorr <- xNullsMedianCorr[names(xNullsMedianData)]
  xNullsMedianData <- data.frame(xNullsMedianData)
  
  xNulls0 <- xNulls0[which(xNullsImpList == "zero")]
  xNulls0Corr <- xNulls0Corr[names(xNulls0)]
  xNulls0 <- data.frame(xNulls0)
  
  xNulls0Corr <- data.frame(corr = t(data.frame(xNulls0Corr))) %>%
    rownames_to_column(var = "variable")
  xNulls0Corr$type <- "zero"
  
  xNullsMedianCorr <- data.frame(corr = t(data.frame(xNullsMedianCorr))) %>%
    rownames_to_column(var = "variable")
  xNullsMedianCorr$type <- "median"
  
  xNullsCorr <- rbind(xNullsMedianCorr, xNulls0Corr)
  
  xNulls <- cbind(xNulls0, xNullsMedianData)
}

if(N != 0){
  if(nN > 0){
    xNums <- cbind(xNulls, xNoNulls)
  } else {
    xNums <- xNulls
  }
}

#### Clear environment and memory ####
saveList <- c("x", "xBools", "xIDs", "xNums", "y", "toImpLeft", "toImpRight", "idBooleanVars", "saveList", "SD_Left", "SD_Right", 
              "impRightDataN", "impLeftDataN", "xNullsCorr", "xNullFlags", "L", "R", "B", "N", "impBothRows", "impLeftRows", "impRightRows")
rmList <- setdiff(ls(), saveList)
rm(list = rmList)
gc()

#### Bind imputed columns and flags with original dataframe ####
xImp <- xIDs

if(length(xBools) != 0){
  xImp <- cbind(xImp, xBools)
}

xImp <- cbind(xIDs, xNums)

rm(xIDs, xNums, xBools)
gc()

if(B > 0){
  xImp <- cbind(xImp, impBothRows)
  rm(impBothRows)
  gc()
}

if(L != "try-error"){
  xImp <- cbind(xImp, impLeftRows)
  rm(impLeftRows)
  gc()
}

if(R != "try-error"){
  xImp <- cbind(xImp, impRightRows)
  rm(impRightRows)
  gc()
}

if(N > 0){
  xNullFlags <- data.frame(xNullFlags)
  xImp <- cbind(xImp, xNullFlags)
  rm(xNullFlags)
  gc()
}

#### END ####
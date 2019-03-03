#### Load Libraries ####
# List of required libraries
libList <- c("data.table", "dplyr", "Hmisc", "perturb", "tibble", "svDialogs")

# Check for user's installed packages
pkgs <- installed.packages()

# Check currently installed packages against required packages
toInstall <- setdiff(libList, pkgs[,1])

# Calculate number of packages to be installed
pkgLen <- length(toInstall)

# Install necessary packages
while (pkgLen > 0) {
  lapply(toInstall, install.packages)
  pkgs <- installed.packages()
  toInstall <- setdiff(libList, pkgs[,1])
  pkgLen <- length(toInstall)
}

# Clear environment
rm(pkgLen, toInstall, pkgs, libList)

# Load libraries
library(data.table)
library(dplyr)
library(Hmisc)
library(perturb)
library(tibble)
library(svDialogs)

options(scipen = 999)

#### Directories ####
# Identify OS type
osType <- .Platform$OS.type

# Choose directory from pop up for condition index outputs
if(osType != "windows"){
  print("Please choose directory for condition index output.")
  conddir <- dlg_dir(title = "Condition index output directory")$res
} else {
  conddir <- choose.dir(caption = "Condition index output directory")
}

conddir <- if_else(osType != "windows", 
                   paste(conddir, "/", sep = ""), 
                   paste(conddir, "\\", sep = ""))

# Choose directory from pop up for final dataset output
if(osType != "windows"){
  print("Please choose the directory for the final dataset output.")
  candVarDir <- dlg_dir(title = "Final dataset directory")$res
} else{
  candVarDir <- choose.dir(caption = "Final dataset directory")
}
candVarDir <- if_else(osType != "windows", 
                      paste(candVarDir, "/", sep = ""), 
                      paste(candVarDir, "\\", sep = ""))

#### Read in data file ####
# Choose dataset for review interactively
if(osType != "windows"){
  print("Please choose dataset")
  Candidate_Vars <- dlg_open(title = "Please choose dataset")
} else {
  Candidate_Vars <- choose.files(caption = "Please choose dataset")
}

# Read in dataset
Candidate_Vars <- fread(Candidate_Vars, header = TRUE)

#### Variables and dataframes used during review ####
# Start with columns for removed variables dataframe
logvars <- c("Review_Seq", "VarNum", "CondIndex", "Variables", "Variance", "Status")

# Number of columns for removed variables dataframe
logvarslen <- length(logvars)

# Create dataframe to capture removed variables
VarsRemovedLog <- matrix(ncol = logvarslen, nrow = 0)
VarsRemovedLog <- data.frame(VarsRemovedLog)

# Name columns in removed variables dataframe
colnames(VarsRemovedLog) <- logvars

# Create condition index filename
if(osType != "windows"){
  condindfile <- dlg_input(message = "Please enter a name for the condition index file (ie - YYYYMM_Condition_Index.csv):  ")$res
} else {
  condindfile <- winDialogString("Please enter a name for the condition index file (ie - YYYYMM_Condition_Index.csv):  ", "")
}

# Check for CSV file type
condindfile_end <- substr(condindfile, nchar(condindfile) - 3, nchar(condindfile))

# If file type isn't CSV, change to CSV
if(condindfile_end != ".csv"){
  if(grepl('\\.', condindfile)){
    fileTypeStart <- regexpr('\\.', condindfile)[1]
    condindfile <- substr(condindfile, 1, fileTypeStart - 1)
    condindfile <- paste(condindfile, ".csv", sep = "")
  }
  condindfile <- paste(condindfile, ".csv", sep = "")
}

# Create filename for final candidate variables dataset
if(osType != "windows"){
  candVarFile <- dlg_input(message = "Please enter a name for the final candidate variables file (ie - YYYYMM_Candidate_Variables.csv):  ")$res
} else {
  candVarFile <- winDialogString("Please enter a name for the final candidate variables file (ie - YYYYMM_Candidate_Variables.csv):  ", "")
}

# Check for CSV file type
candVarFileEnd <- substr(candVarFile, nchar(candVarFile) - 3, nchar(candVarFile))

# If file type isn't CSV, change to CSV
if(candVarFileEnd != ".csv"){
  if(grepl(".", candVarFile) == TRUE){
    fileTypeStart <- regexpr('\\.', candVarFile)[1]
    candVarFile <- substr(candVarFile, 1, fileTypeStart - 1)
    candVarFile <- paste(candVarFile, ".csv", sep = "")
  }
  candVarFile <- paste(candVarFile, ".csv", sep = "")
}

# Create a backup of original data
OrigData <- Candidate_Vars

# Create list of variables to remove
Remove <- c("ID", "Target")

# Create list of variables to keep/review
topvars <- names(Candidate_Vars)
topvars <- setdiff(topvars, Remove)

# Create variables used in while loop
# Initialize condition index variable with appropriately high value
CI <- 1000

# Status based on user's selection
update <- "Reviewing"

# Set index threshold
threshold <- 2.75

# Set review sequence
sequence <- 1

#### Functions ####
# Condition index
MakeCondInd <- function(Matrix, VariableList){
  # Create matrix with variables to be reviewed
  CandVarsMatrix <- select(Matrix, VariableList)
  CandVarsMatrix <- as.matrix(CandVarsMatrix)
  
  # Create condition index
  Cond_Index <- colldiag(CandVarsMatrix, add.intercept = FALSE)
  
  # Remove un-needed objects and clear memory
  rm(CandVarsMatrix)
  gc()
  
  # Create dataframe of condition indices
  condindx <- data.frame(Cond_Index$condindx)
  
  # Create dataframe of variance decomposition proportions
  pi <- data.frame(Cond_Index$pi)
  
  # Remove un-needed objects and clear memory
  rm(Cond_Index)
  gc()
  
  # Join condition indices and variance decomposition proportions
  Cond_Index <- cbind(condindx, pi)
  
  # Remove un-needed objects and clear memory
  rm(condindx, pi)
  gc()
  
  # Create a column of the current rownames
  Cond_Index <- rownames_to_column(Cond_Index, var = "rownames")
}

# Variables to review (function that looks for variables with VIF > .3)
CondIndReview <- function(Cond_Index, CondInd){
  # Calculate number of rows
  curr_row <- which(Cond_Index$cond.index == CI)
  
  # Variable to determine number of variables to review
  varslength <- 0
  
  # While loop to find row with at least one variable to review
  while(varslength < 1){
    # Create generic dataframe of row to review
    a <- data.frame(Cond_Index[curr_row, ])
    
    # Identify variables with variance decomposition proportion greater than .3 (can be changed)
    varstoreview <- c(names(a[, which(a > .3)]))
    
    # Number of variables to review
    varslength <- length(varstoreview) - 2
    
    # Update if needed
    if(varslength == 0){
      curr_row <- curr_row - 1
      next()
    }
    
    # Reduce dataframe to only variables to be reviewed
    a <- select(a, varstoreview)
  }
  
  # Capture row number
  VarNum <- a$rownames
  
  # Capture index value being reviewed
  CondIndex <- a$cond.index
  
  # Remove rownames and cond.index column from review dataframe
  a <- select(a, -rownames, -cond.index)
  
  # Convert to dataframe and transpose
  a <- data.frame(t(a))
  
  # Create a column of variables
  a <- rownames_to_column(a, var = "Variables")
  
  # Rename column with variance decomposition proportion value(s)
  colnames(a)[2] <- "Variance"
  
  # Create a column with row number
  a$VarNum <- VarNum
  
  # Create a column with index value
  a$CondIndex <- CondIndex
  
  # Create a status column (for the removed variables dataframe)
  a$Status <- "Pending"
  
  # Re-order dataframe for easier viewing
  a <- select(a, VarNum, CondIndex, everything())
}

# Maximum variance by index
MaxVar <- function(Cond_Index){
  maxvar <- matrix(ncol = 3, nrow = 0)
  maxvar <- data.frame(maxvar)
  maxvarcols <- c("Index", "Variable", "MaxVariance")
  colnames(maxvar) <- maxvarcols
  
  for(i in 1:nrow(Cond_Index)){
    rowind <- Cond_Index[i,]
    rowindex <- rowind$cond.index
    rowind <- select(rowind, -rownames, -cond.index)
    rowmaxvar <- max(rowind)
    varname <- names(rowind[which(rowind == rowmaxvar)])
    tempdf <- data.frame(Index = rowindex, Variable = varname, MaxVariance = rowmaxvar)
    maxvar <- rbind(maxvar, tempdf)
    rm(tempdf, rowind)
  }
  
  maxvar
}

#### Condition index ####
while(CI > threshold){
  #### Create condition index or view next variables in current condition index ####
  # Create condition index if user hasn't chosen to pass or see more rows, or...
  if(update == "Reviewing"){
    # Make condition index
    Cond_Index <- MakeCondInd(Matrix = Candidate_Vars, VariableList =  topvars)
    
    # If this is the first condition index created, write the first condition index created
    if(sequence == 1){
      condindfilename <- paste(conddir, condindfile, sep = "")
      fwrite(Cond_Index, file = condindfilename)
    }
    
    # Update condition index
    CI <- max(Cond_Index$cond.index)
    
    # Stop loop if condition index below threshold
    if(CI <= threshold){
      # Update user
      print("The condition index is below your threshold.")
      
      # Stop while loop
      next()
    }
  }
  
  # ...update row number to display using current condition index
  if(update != "Reviewing"){
    # Choose the next row
    maxrow <- as.numeric(unique(min(a$VarNum))) - 1
    
    # Update condition index
    CI <- Cond_Index$cond.index[maxrow]
    
    # Check condition index against threshold
    if(CI <= threshold){
      # Silly update status
      update <- "Almost too far"
      
      # Update user
      print("Sorry, the next index is below the threshold. Please review the previous menu.")
    }
  }
  
  
  #### Create dataframe with variables to review ####
  if(update != "Almost too far"){
    a <- CondIndReview(Cond_Index = Cond_Index, CondInd = CI)
  }
  
  # If user chose to see next row
  if(update == "Next"){
    a <- rbind(a, b)
  }
  
  #### Pop up for user's selection ####
  # Create a list of unique variables from the dataframe
  varsformenu <- unique(a$Variables)
  
  # Create a list of choices for the pop up menu
  ifelse(update != "Almost too far",
         {
           # If index is below threshold still:
           Choices <- c(varsformenu, "Pass", "See Another Row", "Stop")
           title <- "Please choose a variable."
         },
         {
           # If user chose to see more or pass, but next index was below threshold:
           Choices <- c(varsformenu, "Stop")
           title <- "Choose variable. Next index was below threshold."
         })
  
  # View the dataframe of possible variables to remove
  View(a)
  
  # Display pop up so user can choose to remove a variable
  if(osType != "windows"){
    ToRemoveN <- dlg_list(choices = Choices, title = title)$res
  } else {
    ToRemoveN <- select.list(Choices, title = title, graphics = TRUE, multiple = TRUE)
  }
  
  #### Loop control if user doesn't choose a variable ####
  # If user selects Stop, the loop will be stopped
  if(ToRemoveN == "Stop"){
    # Update/review status is reset
    update <- "Reviewing"
    
    # Loop is stopped
    break()
  }
  
  # If user selects Pass the loop will move to the next row
  if(ToRemoveN == "Pass"){
    # Update/review status is set to Pass
    update <- "Pass"
    
    # The loop moves to the next row
    next()
  }
  
  # If user selects to See Another Row, the loop will move to the next row
  if(ToRemoveN == "See Another Row"){
    update <- "Next"
    b <- a
    next()
  }
  
  #### Variable chosen to remove ####
  # After user chooses a variable to remove, update/review status is reset
  update <- "Reviewing"
  
  # List of variables to be removed is created
  ToRemove <- c(ToRemoveN)
  
  # Variables chosen to be removed are appended to list of all variables to remove
  Remove <- append(Remove, ToRemove)
  
  # List of variables to keep/review is updated
  topvars <- setdiff(topvars, Remove)
  
  # Status column in dataframe is updated
  a$Status <- "Kept"
  
  # Status column in dataframe is updated for variable(s) removed
  a$Status[which(a$Variables == ToRemove)] <- "Removed"
  
  # Review sequence column
  a$Review_Seq <- sequence
  
  # Dataframe of all variables removed is updated
  VarsRemovedLog <- rbind(VarsRemovedLog, a)
  
  # Update review sequence variable
  sequence <- sequence + 1
}
#### Write candidate variables dataset ####
# Create dataframe of variable with maximum variance decomposition proportion for each index
maxvar <- MaxVar(Cond_Index)
View(maxvar)

# Reduce data to top variables
Candidate_Vars <- select(OrigData, ID, topvars, Target)

# Create filename for candidate variables
candVarFileName <- paste(candVarDir, candVarFile, sep = "")

# Write to shared drive
fwrite(Candidate_Vars, file = candVarFileName)
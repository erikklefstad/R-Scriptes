# R-Scripts
Some R scripts I've written and found useful.

### Condition Index
This script creates a condition index from a given dataset and allows the user to interactively choose which variables to remove to reduce collinearity.  It determines which indices have variables with variance proportions above a given threshold.  It then chooses the index with the highest value and displays only those variables with variance proportions above the given threshold.  The user can then choose to remove a variable, see the next row in the current condition index, or stop the script.

The script continues until the condition indices are below a threshold or the user exits the script.  The final, reduced dataset and condition index are written to the desired directory.

### Impute Functions
This script efficiently Winsorizes a dataset.  It identifies a variable's values lying beyond a pre-specified number of standard deviations.  It then tests each value to determine which most increases that variables correlation to the target.  The dataset is then imputed with the value providing the highest correlation to the target.  Columns are also created to identify which values were imputed.  These 'flag' columns are named so users can identify the imputation thresholds for each column.

### Install Packages
This is a simple script I wrote after needing to install the R packages I use locally on a virtual machine.  I wrote it using only base functions for read/write, although I normally would use the read/write functions from the data.table package.

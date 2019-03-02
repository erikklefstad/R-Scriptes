# Create a file of the packages in current instance of R and write
# This is done on the local/current machine
pkgs <- installed.packages()
write.table(pkgs, file = "CurrentPackages.csv")

# Read in dataframe of packages and install
# This is done on the machine where the packages need to be installed

pkgfile <- "CurrentPackages.csv"

pkgs <- read.table(pkgfile, header = TRUE)

libs <- library()$results[, 1]

toinstall <- setdiff(pkgs$Packages, libs)

installLen <- length(toinstall)

while(installLen > 0){
  lapply(toinstall, install.packages)
  libs <- library()$results[, 1]
  toinstall <- setdiff(pkgs$Packages, libs)
  installLen <- length(toinstall)
}

# Create a file of the packages in current instance of R and write
pkgs <- installed.packages()
write.table(pkgs, file = "CurrentPackages.csv")

# Read in dataframe of packages and install
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
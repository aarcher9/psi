# install.packages("distr", repos="http://cran.us.r-project.org")
# install.packages("stats", repos="http://cran.us.r-project.org")
suppressMessages(suppressWarnings(library("distr")))
suppressMessages(suppressWarnings(library("stats")))

source("data-read-support.r")


# Analisi
df <- readData()

componentsOnlySells <- df[,7]
componentsOnlyAndSMASells <- df[,8]
componentsSMASells <- componentsOnlyAndSMASells - componentsOnlySells

print(mean(componentsOnlySells))
print(mean(componentsOnlyAndSMASells))
print(mean(componentsSMASells))

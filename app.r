# install.packages("distr", repos="http://cran.us.r-project.org")
# install.packages("stats", repos="http://cran.us.r-project.org")
suppressMessages(suppressWarnings(library("distr")))
suppressMessages(suppressWarnings(library("stats")))

source("data-read-support.r")


# Analisi
df <- readData()

weeks <- df[,1]
components.sold.through.assistance.norm <- df[,8] / sum(df[,8])
assistance.services.performed.norm <- df[,9] / sum(df[,9])

plot(
        weeks, 
        components.sold.through.assistance.norm - assistance.services.performed.norm, 
        col = "red", 
        type = "l", 
        ylim = c(-0.03, 0.03))
lines(
        weeks, 
        rep(0, 52),
        col = "gray")
# lines(
#         weeks, 
#         assistance.services.performed.norm,
#         col = "blue")

# legend(
#         "topleft",
#         legend = c("Componenti venduti tramite assistenza", "Servizi assistenza effettuati"),
#         pch = 15,
#         col = c("red", "blue"))

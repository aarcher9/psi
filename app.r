suppressMessages(suppressWarnings(library("distr")))
suppressMessages(suppressWarnings(library("stats")))

# Meglio runnare il programma con Rscript e non con R per evitare che ogni volta stampi anche tutto il file in esecuzione.

rawData <- "revenues2022.txt"

df <- read.table(
        rawData, 
        header = FALSE, 
        sep = c("\t"), 
        colClasses = c("NULL", rep("numeric", 16)), 
        col.names = 0:16, 
        numerals = "no.loss")

# Il numero di indice del vettore matcha con il nome della colonna, oltre che essere scritto (giusto per avere un riferimento nel codice).
columnsMap <- c(
        "(1) Settimana di riferimento",
        "(2) VENDITA: Trekking",
        "(3) VEDNITA: MTB",
        "(4) VENDITA: Strada",
        "(5) VENDITA: Abbigliamento",
        "(6) VENDITA: Accessori",
        "(7) VENDITA: Componenti",
        "(8) VENDITA: Componenti venduti tramite SAM",
        "(9) VENDITA: SAM",
        "(10) RICAVO: Trekking",
        "(11) RICAVO: MTB",
        "(12) RICAVO: Strada",
        "(13) RICAVO: Abbigliamento",
        "(14) RICAVO: Accessori",
        "(15) RICAVO: (Totale) Componenti venduti standalone e venduti tramite SAM",
        "(16) RICAVO: SAM"
)


# Utilities
prec <- function(x){ format(round(x, 4), nsmall = 4) }



# Analisi
weeks <- df[,1]
sale <- list(
        bikes = list(
                trekking = df[, 2],
                mtb = df[, 3],
                road = df[, 4]
        ),
        components = list(
                alone = df[, 7],
                through.assistance = df[, 8]
        ),
        assistance.performed = df[, 9]
)

income <- list(
        components = list(
                total = df[, 15]
        ),
        assistance = df[, 16]
)
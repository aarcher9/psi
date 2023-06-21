suppressMessages(suppressWarnings(library("distr")))
suppressMessages(suppressWarnings(library("stats")))

raw_data <- "revenues2022.txt"

df <- read.table(
        raw_data,
        header = FALSE,
        sep = c("\t"),
        colClasses = c("NULL", rep("numeric", 16)),
        col.names = 0:16,
        numerals = "no.loss")


weeks <- df[, 1]
sales <- list(
        bikes = list(
                trekking = df[, 2],
                mtb = df[, 3],
                road = df[, 4]
        ),
        components = list(
                alone = df[, 7],
                through.assistances = df[, 8]
        ),
        assistances = df[, 9]
)

incomes <- list(
        components = list(
                total = df[, 15]
        ),
        assistances = df[, 16]
)


# Utilities
prec <- function(x) {
        format(round(x, 4), nsmall = 4)
}


# Analisi
"Vendite soli componenti"
mean(sales$components$alone)

"Vendite componenti tramite assistenza"
mean(sales$components$through.assistances)

"Assistenze effettuate"
mean(sales$assistances)

"Ricavi vendite componenti (cumulativi)"
mean(incomes$components$total)

"Ricavi servizio di assistenza"
mean(incomes$assistances)
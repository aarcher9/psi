# install.packages("distr", repos="http://cran.us.r-project.org")
# install.packages("stats", repos="http://cran.us.r-project.org")
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
                trekking = df[,2],
                mtb = df[,3],
                road = df[,4]
        ),
        components = list(
                alone = df[,7],
                through.assistance = df[,8]
        ),
        assistance.performed = df[,9]
)

income <- list(
        components = list(
                total = df[,15]
        ),
        assistance = df[,16]
)

# Shortcut per regressione lineare
testForDependenciesAndCorrelation <- function(X, Y, notes = "", title = "", subtitle = "", yl = "", xl = "") {

        plot(
                X, Y, bty = "l",
                xlab = xl,
                ylab = yl,
                type = "p",
                col = "blue")

        lin.regr <- lm(Y ~ X)
        r <- prec(cor(X, Y))
        B <- prec(lin.regr$coefficients[2])
        abline(lin.regr, col = "red")

        title(
                main = title,
                sub = subtitle)
        mtext(
                paste("r: ", r),
                adj = 0,
                line = 0)
        mtext(
                paste("B: ", B),
                adj = 0,
                line = 1)
}

# [ 3 ] ======================================================================
# Si valuti il ricavo in funzione dei componenti relativi ad assistenza venduti e a parte, anche in funzione dei componenti venduti singolarmente.

testForDependenciesAndCorrelation(
        sale$components$through.assistance, 
        income$components$total,
        xl = "# vendite componenti con assistenza",
        yl = "ricavi componenti totali")

testForDependenciesAndCorrelation(
        sale$components$alone, 
        income$components$total,
        xl = "# vendite componenti",
        yl = "ricavi componenti totali")

# [ C:3-4 ] ======================================================================
analysis_3_4 <- function() {

        Y <- income$components$total

        X_1 <- sale$components$through.assistance
        X_2 <- sale$components$alone

        plot(
                X_1,
                Y,
                type = "p",
                col = "green",
                xlab = "X",
                ylab = "ricavi componenti totali",
                xlim = c(0, 150),
                ylim = c(0, 2000))

        lin.regr_1 <- lm(Y ~ X_1)
        r_1 <- prec(cor(X_1, Y))
        B_1 <- prec(lin.regr_1$coefficients[2])
        abline(lin.regr_1, col = "darkgreen")

        points(
                X_2,
                Y,
                col = "magenta")

        lin.regr_2 <- lm(Y ~ X_2)
        r_2 <- prec(cor(X_2, Y))
        B_2 <- prec(lin.regr_2$coefficients[2])
        abline(lin.regr_2, col = "purple")

        legend(
                "topleft",
                bty = "n",
                legend = c(
                        "# vendite componenti con assistenza",
                        paste("r: ", r_1, "B: ", B_1),
                        "# vendite componenti",
                        paste("r: ", r_2, "B: ", B_2)),
                pch = 15,
                col = c(
                        "green", 
                        "darkgreen",
                        "magenta",
                        "purple"))

        # Residui
        plot(
                X_1,
                lin.regr_1$residuals,
                type = "p",
                col = "red",
                xlab = "X",
                ylab = "residui",
                ylim = c(-500, 500))

        points(
                X_2,
                lin.regr_2$residuals,
                col = "gray",
                xlab = "# vendite componenti",
                ylab = "residui")

        legend(
                "topleft",
                bty = "n",
                legend = c(
                        "# vendite componenti con assistenza",
                        "# vendite componenti"),
                pch = 15,
                col = c(
                        "red",
                        "gray"))
}

analysis_3_4()
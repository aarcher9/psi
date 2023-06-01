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
                paste("Corr. coef. : ", r),
                adj = 0,
                line = 0)
        mtext(
                paste("B : ", B),
                adj = 0,
                line = 1)
}


# [ 1 ] ======================================================================
# Si osserva se ci sono relazioni (si) e di che tipo fra assistenze eseguite e componenti venduti tramite di esse. In realtà appare, ed è abbastanza ovvio, anche se ci interessa portare a termine un' analisi più fine.
testForDependenciesAndCorrelation(
        sale$assistance.performed, 
        sale$components$through.assistance,
        xl = "# assistenze",
        yl = "# vendite componenti con assistenza")

# [ 2 ] ======================================================================
# Si prova anche ad osservare se assistenze eseguite e componenti venduti a parte sono in relazione (i clienti potrebbero essere incentivati, dopo una riparazione ad acquistare un ulteriore ricambio di scorta).
testForDependenciesAndCorrelation(
        sale$assistance.performed, 
        sale$components$alone,
        xl = "# assistenze",
        yl = "# vendite componenti")


# [ C:1-2 ] ======================================================================
# Si provi, per chiarezza grafica e per concludere, a sovrapporre allo scorrere delle settimane l'andamento delle vendite di componenti tramite assistenza, quelli venduti da soli e il numero di servizi effettuati.
analysis_1_2 <- function() {

        plot(
                weeks,
                sale$assistance.performed,
                type = "l",
                col = "red",
                ylim = c(0, 150))
        lines(
                weeks,
                sale$components$through.assistance,
                col = "grey")
        lines(
                weeks,
                sale$components$alone,
                col = "green")
}

# analysis_1_2()

# [ 3 ] ======================================================================
# Si valuti il ricavo in funzione dei componenti relativi ad assistenza venduti e a parte, anche in funzione dei componenti venduti singolarmente.

testForDependenciesAndCorrelation(
        sale$components$through.assistance, 
        income$components$total,
        xl = "# vendite componenti con assistenza",
        yl = "# ricavi componenti totali")

testForDependenciesAndCorrelation(
        sale$components$alone, 
        income$components$total,
        xl = "# vendite componenti",
        yl = "# ricavi componenti totali")

# Confronto
analysis_3_4 <- function() {

        Y <- income$components$total

        X_1 <- sale$components$through.assistance
        X_2 <- sale$components$alone

        plot(
                X_1,
                Y,
                type = "p",
                col = "green",
                xlim = c(0, 150),
                ylim = c(0, 2000))

        abline(lm(Y ~ X_1), col = "darkgreen")

        points(
                X_2,
                Y,
                col = "magenta")

        abline(lm(Y ~ X_2), col = "purple")
}

# analysis_3_4()

# [ 5 ] ======================================================================
analysis_5 <- function() {

        X <- weeks
        Y <- sale$assistance.performed
        Y_1 <- sale$bikes$trekking
        Y_2 <- sale$bikes$mtb
        Y_3 <- sale$bikes$road

        # plot(
        #         X,
        #         Y,
        #         type = "l",
        #         col = "black")

        plot(
                X,
                c(1:52),
                col = "blue")
        # lines(
        #         X,
        #         Y_2,
        #         col = "green")
        # lines(
        #         X,
        #         Y_3,
        #         col = "magenta")

}

analysis_5()

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
testForDependenciesAndCorrelation <- function(X, Y, notes = "", title = "", subtitle = "", yl = "", xl = "", plot.only = FALSE) {

        plot(
                X, Y, bty = "l",
                xlab = xl,
                ylab = yl,
                type = "p",
                col = "blue")

        lin.regr <- lm(Y ~ X)
        r <- prec(cor(X, Y))
        A <- prec(lin.regr$coefficients[1])
        B <- prec(lin.regr$coefficients[2])

        title(
                main = title,
                sub = subtitle)
        
        if (!plot.only) {

                abline(lin.regr, col = "red")
                mtext(
                        paste("r: ", r),
                        adj = 0,
                        line = 0)
                mtext(
                        paste("B: ", B),
                        adj = 0,
                        line = 1)
                mtext(
                        paste("A: ", A),
                        adj = 0,
                        line = 2)
        }
}


# Shortcut per sovrapposizione
overlapPlots <- function(X, Y, col, leg, y.lim = c(-500, 500), show.months = FALSE) {

        colors <- c()
        legend <- c()
        i <- 1
        while (i <= length(Y)) {
         
                if (i == 1) {

                        plot(
                                X,
                                Y[[i]],
                                ylim = c(y.lim[[1]], y.lim[[2]]),
                                ylab = "Y",
                                type = "l",
                                col = col[[i]])
                        colors <- append(colors, col[[i]])
                        legend <- append(legend, leg[[i]])
                } else {

                        lines(
                                X,
                                Y[[i]],
                                col = col[[i]])

                        colors <- append(colors, col[[i]])
                        legend <- append(legend, leg[[i]])
                }

                i <- i + 1
        }

        if (show.months) {
                text(weeks[11], 0, label = "Aprile", cex = 0.7)
                text(weeks[26], 0, label = "Giugno", cex = 0.7)
                text(weeks[38], 0, label = "Agosto", cex = 0.7)
                text(weeks[48], 0, label = "Novembre", cex = 0.7)
        }

        legend(
                "topleft",
                bty = "n",
                legend = legend,
                pch = 15,
                col = colors)
}


# OBIETTIVO PRINCIPALE
# [ 2-A ] ======================================================================
# Si visualizza allo scorrere delle settimane l'andamento delle vendite di componenti tramite assistenza, quelli venduti da soli e il numero di servizi effettuati.
overlapPlots(
        weeks,
        list(
                sale$assistance.performed,
                sale$components$through.assistance,
                sale$components$alone),
        list(
                "red",
                "grey",
                "green"),
        list(
                "# assistenze",
                "# vendite componenti con assistenza",                  
                "# vendite componenti"),
        y.lim = c(0, 150),
        show.months = TRUE)

# [ 2 ] ======================================================================
# Si prova anche ad osservare se assistenze eseguite e componenti venduti a parte sono in relazione; i clienti potrebbero essere incentivati, dopo una riparazione ad acquistare un ulteriore ricambio di scorta.
# (A) Se ci si pensa però è ragionevole supporre anche che la tendenza a seguire lo stesso andamento del numero di assistenza sia da imputare alla semplice question stagionale. Quando il clima è più mite i ciclisti utilizzano la bici più frequentemente e di conseguenza faranno più acquisti, anche in generale, al punto vendita e quindi anche più riparazioni/manutenzioni. (correlation is not causation).

testForDependenciesAndCorrelation(
        sale$assistance.performed, 
        sale$components$alone,
        xl = "# assistenze",
        yl = "# vendite componenti")


# ALTRE VALUTAZIONI DI SECONDARIA IMPORTANZA
secondary_tests <- function() {
        # [ 1 ] ======================================================================
        # Si osserva se ci sono relazioni (si) e di che tipo fra assistenze eseguite e componenti venduti tramite di esse. In realtà appare, ed è abbastanza ovvio, anche se ci interessa portare a termine un' analisi più fine.
        testForDependenciesAndCorrelation(
                sale$assistance.performed, 
                sale$components$through.assistance,
                xl = "# assistenze",
                yl = "# vendite componenti con assistenza")

        # Si testi anche l'influenza delle assistenze direttamente sui ricavi dei componenti (totale poichè si dispone solo di questi, non si conosce la ripartizione dei ricavi, ma solo quella delle vendite).
        testForDependenciesAndCorrelation(
                sale$assistance.performed, 
                income$components$total,
                xl = "# assistenze",
                yl = "ricavi componenti totali")


        # [ 5 ] ======================================================================
        # Anche questo è piuttosto ovvio.
        testForDependenciesAndCorrelation(
                sale$assistance.performed, 
                income$assistance,
                xl = "# assistenze",
                yl = "ricavi assistenze")


        testForDependenciesAndCorrelation(
                weeks, 
                income$assistance,
                xl = "# settimana",
                yl = "ricavi assistenze",
                plot.only = TRUE)

        # [ 6 ] ======================================================================
        # Si verifica l'ipotesi che le bici da trekking vendute siano circa pari alle mountain bike vendute.
        t.test(
        sale$bikes$trekking,
        sale$bikes$mtb,
        alternative = "two.sided",
        var.equals = TRUE)

        # Si veda quindi se entrambe abbiano circa la stessa influenza sul numero di assistenze effettuate.
        # La vendita di bici da trekking e mountain bike è debolmente correlata con i servizi di assistenza.
        testForDependenciesAndCorrelation(
                sale$bikes$trekking, 
                sale$assistance.performed,
                xl = "# bici da trekking vendute",
                yl = "# assistenze")

        testForDependenciesAndCorrelation(
                sale$bikes$mtb, 
                sale$assistance.performed,
                xl = "# bici mtb vendute",
                yl = "# assistenze")


        # Lo stesso t-Test si è ripetuto per le altre coppie di biciclette con esito negtivo.
        t.test(
        sale$bikes$trekking,
        sale$bikes$road,
        alternative = "two.sided",
        var.equals = TRUE)

        t.test(
        sale$bikes$road,
        sale$bikes$mtb,
        alternative = "two.sided",
        var.equals = TRUE)

        # La vendita di bici da strada è debolmente correlata con i servizi di assistenza.
        testForDependenciesAndCorrelation(
                sale$bikes$road, 
                sale$assistance.performed,
                xl = "# bici da strada vendute",
                yl = "# assistenze")


        # [ 7 ] ======================================================================
        # Si controlli se ci siano altri fattori che possano influenzare le vendite di componenti.
        # Si noti che le vendite degli altri prodotti subiscano circa le stesse variazioni delle vendite dei componenti.
        overlapPlots(
                weeks,
                list(
                        sale$components$alone,
                        df[,5],
                        df[,6]),
                list(
                        "red",
                        "magenta",
                        "grey"),
                list(
                        "# componenti venduti",
                        "# abbigliamento",
                        "# accessori"),
                y.lim = c(0, 80))

        # Stando ai risultati di cui sopra ci si aspetta correlazione. Il risultato è buono ma non ottimo. Le ragioni possono essere simili a quanto riportato nel commento (A).
        testForDependenciesAndCorrelation(
                df[,5], 
                sale$components$alone,
                xl = "# vendite abbigliamento",
                yl = "# componenti venduti")

        testForDependenciesAndCorrelation(
                df[,6], 
                sale$components$alone,
                xl = "# vendite accessori",
                yl = "# componenti venduti")
}
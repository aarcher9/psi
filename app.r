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
components.sold.standalone <- df[,7]
components.sold.through.assistance.services <- df[,8]
assistance.services.performed <- df[,9]
total.components.income <- df[,15]
total.assistance.services.income <- df[,15]

# [ 1 ] ======================================================================
# Si osserva se ci sono relazioni (si) e di che tipo fra assistenze eseguite e componenti venduti tramite di esse
plot(
        assistance.services.performed,
        components.sold.through.assistance.services,
        type = "p",
        col = "blue")

assistance.related.components <- lm(components.sold.through.assistance.services ~ assistance.services.performed)

mtext("", line = 0)
mtext(
        paste("Corr. coef. : ", prec(
                cor(components.sold.through.assistance.services, assistance.services.performed))),
        line = 1)
mtext(
        paste("B : ", prec(assistance.related.components$coefficients[2])),
        line = 2)


# [ 2 ] ======================================================================
# Si prova anche ad osservare se assistenze eseguite e componenti venduti a parte sono in relazione (i clienti potrebbero essere incentivati, dopo una riparazione ad acquistare un ulteriore ricambio di scorta)
plot(
        assistance.services.performed,
        components.sold.standalone,
        type = "p",
        col = "blue")

standalone.components <- lm(components.sold.standalone ~ assistance.services.performed)

mtext("", line = 0)
mtext(
        paste("Corr. coef. : ", prec(
                cor(components.sold.standalone, assistance.services.performed))),
        line = 1)


# [ E:1-2 ] ======================================================================
# Si provi, per chiarezza grafica e per concludere, a sovrapporre allo scorrere delle settimane l'andamento delle vendite di componenti tramite assistenza, quelli venduti da soli e il numero di servizi effettuati.
plot(
        weeks,
        assistance.services.performed,
        type = "l",
        col = "red",
        ylim = c(0, 150))
lines(
        weeks,
        components.sold.through.assistance.services,
        col = "grey")
lines(
        weeks,
        components.sold.standalone,
        col = "green")


# [ 3 ] ======================================================================
# Si valuti il ricavo in funzione dei componenti relativi ad assistenza venduti.
plot(
        components.sold.through.assistance.services,
        total.components.income,
        type = "p",
        col = "gray")

income.assistance <- lm(total.components.income ~ components.sold.through.assistance.services)

abline(income.assistance, col = "red")

mtext("", line = 0)
mtext(
        paste("Corr. coef. : ", prec(
                cor(total.components.income, components.sold.through.assistance.services))),
        line = 1)
mtext(
        paste("B : ", prec(income.assistance$coefficients[2])),
        line = 2)


# [ 4 ] ======================================================================
# Si valuti il ricavo in funzione dei componenti venduti singolarmente.
plot(
        components.sold.standalone,
        total.components.income,
        type = "p",
        col = "gray")

income.standalone <- lm(total.components.income ~ components.sold.standalone)

abline(income.standalone, col = "red")

mtext("", line = 0)
mtext(
        paste("Corr. coef. : ", prec(
                cor(total.components.income, components.sold.standalone))),
        line = 1)
mtext(
        paste("B : ", prec(income.standalone$coefficients[2])),
        line = 2)


# [ E:3-4 ] ======================================================================
# Si confrontino i fit.
plot(
        components.sold.through.assistance.services,
        total.components.income,
        type = "p",
        col = "green",
        xlim = c(0, 150),
        ylim = c(300, 2000))
abline(income.assistance, col = "darkgreen")

points(
        components.sold.standalone,
        total.components.income,
        col = "magenta")
abline(income.standalone, col = "purple")
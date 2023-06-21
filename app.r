suppressMessages(suppressWarnings(library("distr")))
suppressMessages(suppressWarnings(library("stats")))
suppressMessages(suppressWarnings(library("carData")))

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
"Vendite di componenti fuori assistenza (FA)"
mean(sales$components$alone)

"Vendite componenti tramite assistenza (A)"
mean(sales$components$through.assistances)

"Assistenze effettuate"
mean(sales$assistances)

"Ricavi vendite componenti (cumulativi)"
mean(incomes$components$total)

"Ricavi servizio di assistenza"
mean(incomes$assistances)


# C'è dipendenza fra il numero di assistenza effettuate e le vendite di componenti?
X <- sales$assistances
Y <- sales$components$alone

lin.regr <- lm(Y ~ X, level = 0.95)

"Regressione lineare"
lin.mod <- summary(lin.regr)
lin.mod

r <- prec(cor(X, Y))
A <- prec(lin.regr$coefficients[1])
B <- prec(lin.regr$coefficients[2])
p.value <- prec(lin.mod$coefficients[7])
Res <- lin.regr$residuals

plot(
        X, 
        Y, 
        bty = "l",
        xlab = "# assistenze",
        ylab = "# vendite componenti FA",
        type = "p",
        col = "#5b5bb5")

title(main = "", sub = "")
        
abline(lin.regr, col = "#9d5151")
mtext(paste("A =", A, "  B =", B, "  r =", r))


t.test(Res, mu = 0, conf.level = 0.95)

hist(
        Res,
        axes = TRUE,
        xlab = "residuo",
        ylab = "frequenza",
        main = "Istogramma dei residui",
        xlim = c(-20, 20),
        ylim = c(0, 20),
        col = "darkmagenta")

# Test Kolmogorov-Smirnov 
# Ipotesi nulla: i dati appartengono ad una distribuzione normale
ks.test(unique(Res), "pnorm")


hist(
        rnorm(n = 52, mean = mean(Res), sd = sd(Res)),
        axes = TRUE,
        xlab = "residuo",
        ylab = "frequenza attesa",
        main = "Istogramma dei residui attesi",
        xlim = c(-20, 20),
        ylim = c(0, 20),
        col = "darkgreen")





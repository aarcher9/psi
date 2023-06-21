suppressMessages(suppressWarnings(library("distr")))
suppressMessages(suppressWarnings(library("stats")))
suppressMessages(suppressWarnings(library("carData")))

# https://influentialpoints.com/Training/kolmogorov-smirnov_test-principles-properties-assumptions.htm

args = commandArgs(trailingOnly = TRUE)

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
test.name <- args[1]
res.histo <- args[2]

switch(test.name,
        "intro" = {
                "Vendite di componenti fuori assistenza (FA)"
                mean(sales$components$alone)

                "Vendite componenti tramite assistenza (A)"
                mean(sales$components$through.assistances)

                "Assistenze effettuate"
                mean(sales$assistances)

                "Ricavi vendite componenti cumulativi (C)"
                mean(incomes$components$total)

                "Ricavi servizio di assistenza"
                mean(incomes$assistances)
        },
        "1" = {
                sink("Fig1.txt")
                pdf(file = "Fig1.pdf")
                X <- sales$assistances
                Y <- sales$components$alone
                X.lab = "# assistenze"
                Y.lab = "# vendite componenti (FA)"
        },
        "2" = {
                sink("Fig2.txt")
                pdf(file = "Fig2.pdf")
                X <- sales$components$alone
                Y <- incomes$components$total
                X.lab = "# vendite componenti (FA)"
                Y.lab = "$ ricavi sui componenti (C)"
        },
        "3" = {
                sink("Fig3.txt")
                pdf(file = "Fig3.pdf")
                X <- sales$components$through.assistances
                Y <- incomes$components$total
                X.lab = "# vendite componenti (A)"
                Y.lab = "$ ricavi sui componenti (C)"
        })


# Modelli e test.
lin.regr <- lm(Y ~ X)

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
        main = "Regressione Lineare",
        bty = "l",
        xlab = X.lab,
        ylab = Y.lab,
        type = "p",
        col = "#5b5bb5")
        
abline(lin.regr, col = "#9d5151")
mtext(paste("A =", A, "  B =", B, "  r =", r))

# Ipotesi nulla B = 0
t.test(Res, mu = 0, conf.level = 0.95)

# Istogramma dei residui
hist(
        Res,
        breaks = "Sturge",
        axes = TRUE,
        xlab = "residuo",
        ylab = "frequenza",
        ylim = c(0, 20),
        main = "Istogramma dei residui",
        col = "burlywood")

# Test Kolmogorov-Smirnov 
# Ipotesi nulla: i dati appartengono ad una distribuzione normale
ks.test(unique(Res), "pnorm", exact = FALSE)

# Residui
plot(
        rep(1:52), 
        Res,
        main = "Grafico dei residui",
        xlab = "#",
        ylab = "residuo")

abline(0, 0, col = "gray")

# Q-Q plot
qqnorm(
        Res, 
        pch = 1, 
        frame = FALSE,
        xlab = "quantili teorici",
        ylab = "quantili sperimentali",
        main = "Grafico Q-Q",)

qqline(Res, col = "steelblue")

dev.off()

### Semesterarbeit 
### Aufgabe 1

# Neue Funktionen einbinden
library(foreign)
library(readxl)

# Datensatz einlesen zum ersten mal
erhebung <- file.choose()

# Bei neustart von R hier ansetzen
erhebung <- read_excel("C:\\Users\\timmi\\Desktop\\ZHAW\\WaSt1\\Erhebung_1.xlsx")
erhebung <- table(erhebung)

# Data frame von erhebung erstellen 
erhebung <- data.frame(erhebung)


# Histogramm über Körpergrosse vom Datensatz erhebung
hist(erhebung$GRO,
     main = "Körpergrösse",
     xlab = "Körpergrösse [cm]",
     ylab = "Häufigkeit",
     las = 1,
     col = "deepskyblue",
     breaks=seq(140,210,length=25))

# Test mit Mittelwert
# Histogramm über Körpergrosse vom Datensatz erhebung
x <- (erhebung$GRO)

# Histogram mit Normalverteilungskurve
hist(erhebung$GRO,
     main = "Körpergrösse Normalverteilung",
     xlab = "Körpergrösse [cm]",
     ylab = "Dichte",
     las = 1,
     col = "deepskyblue",
     breaks=seq(140,210,length=25),
     freq = FALSE)

# Durchschnitt
m <- mean(x, na.rm = TRUE)
# Standartabweichung
s <- sd(x, na.rm = TRUE)
curve(dnorm(x,m,s),add = TRUE,lwd=3)
box()

m
s
# Table erstellen für Kuchendiagramm Hoe Abschluss
x <- table(erhebung$HOE_ABSCHLUSS)
x


# Definieren von Namen und Prozent anzeige
percent <- table(erhebung$HOE_ABSCHLUSS)/length(erhebung$HOE_ABSCHLUSS)*100
percent <- round(percent, 1)
percent


# Kuchendiagramm
pie(x,
    main="Höchster Abschluss",
    col = rainbow(length(x)),
    radius = 0.8,
    labels=paste(names(x),percent,"%", sep = " "))

## Noch zulösende Probleme:
## Wenn wir round auf 0 stellen runden bekommen wir einen
## totalen % satz anteil von 99%, weil R unteranderem
## den Wert 0.5 auf 0 rundet. Evt. Herr Hofer fragen evt. mit ceiling lösbar
## in den einzelnen Zeilen

######## Kennzahlen und Test 

#median
durchschnitt <- mean(erhebung$GRO, na.rm = TRUE)
durchschnitt

#quantile
quantile <- quantile(erhebung$GRO, probs = c(0.1, 0.5, 0.75), na.rm = TRUE)
quantile

# Max Ausreisser
max(erhebung$GRO, na.rm = TRUE)
# an welcher Position ist der Ausreisser
vInd <- which.max(erhebung$GRO)
vInd

min(erhebung$GRO, na.rm = TRUE)

### Aufgabe 2 

# Absolute Zahlen Geschlecht
Geschlecht <- table(erhebung$GESCHL)
Geschlecht

# Absolute zahlen Arztbesuche
Arztbesuche <- table(erhebung$ARZTBES)
Arztbesuche

# Quantile untersuchen
plot(ecdf(erhebung$ARZTBES), 
     lwd=2, 
     verticals = TRUE,
     main= "Quantile Arztbesuche untersuchen", 
     xlab = "Anzahl Arztbesuche",
     ylab = "Prozentanteil")

# zeigt extrem viele Werte an, versuche Ausprägungen zu kürzen auf 0 - 5 Besuche
nlevels(erhebung$ARZTBES)


# Typ ist double ändern auf Integer
typeof(erhebung$ARZTBES)
as.integer(erhebung$ARZTBES)

#Typ von Geschlecht
typeof(erhebung$GESCHL)


data.frame(Faktor = erhebung$GESCHL,
           Integer = erhebung$ARZTBES)



# Alle ausreisser zusammengefasst in Spalte 7 bedeutet x > 6
erhebung$ARZTBES[erhebung$ARZTBES > 6] <- 7

erhebung$GESCHL <- factor(erhebung$GESCHL)

# erster bivariater Barplot
barplot(table(erhebung$GESCHL, erhebung$ARZTBES),
        beside = TRUE,
        col = c("deepskyblue","pink"),
        xlab = "Arztbesuche", 
        ylab = "Anzahl Personen",
        main = "Anzahl Arztbesuche Männlich und Weiblich",
        names.arg = c(paste(0:6), "6+")
        )
grid()
box()
legend("topright",c("männlich","weiblich"),fill=c("deepskyblue","pink"))

# zweiter Bivariate darstellung

head(erhebung)


erhebung$GESCHL <- factor(erhebung$GESCHL)

plot(GRO ~ GEW, data = erhebung, col = GESCHL, pch = 19)

# Farbpalette ändern
mycol <- c("deepskyblue", "pink3")
palette(mycol)

# Zweite Bivariate darstellung mit PLOT
plot(GRO ~ GEW, data = erhebung, 
     col = GESCHL, 
     pch = 19,
     main = "Grösse und Gewicht nach Geschlecht",
     xlab = "Gewicht [kg]",
     ylab = "Grösse [cm]")

legend("topleft", c("Männlich", "Weiblich"), fill = palette(mycol))

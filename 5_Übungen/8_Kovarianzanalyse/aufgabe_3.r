# Lösungsvorschlag für Programmieraufgabe 3 auf ÜB 8 - Kovarianzanalyse

# Datum : 03. Juli 2024
# Autorin: belinda fleischmann

library(tidyverse)                                                             # Für Pipe (%>%), mutate(), filter()
library(afex)                                                                  # Für aov_ez()

# Daten vorbereiten
fname      <- "5_Übungen/8_Kovarianzanalyse/8-Kovarianzanalyse.csv"            # Dateiname
D          <- read.table(fname, sep = ",", header = TRUE)                      # Laden des Datensatzes
n_subs     <- nrow(D)                                                          # Anzahl Datenpunkte
D_processed <- D %>%
  mutate(ID = seq(n_subs)                                                      # Subject ID hinzufügen
  )

# Mittelwerte bestimmen
means <- D_processed %>%
  group_by(THP) %>%                                                            # Nach THP gruppieren
  summarize(                                                                   # Dataframe mit Spalte für mean erstellen
    mean_BDI = mean(BDI),
    mean_DUR = mean(DUR)
  )

# Beta-parameterschätzer und T-Teststatistiken mit der Funktion lm() bestimmen

# Model-fit für ALM für beide Modelle (ohne und mit Kovariate) erzeugen und speichern 
alm1_fit <- lm(BDI ~ THP, data = D_processed)                                  # ALM 1, nur eine kategoriale UV
alm2_fit <- lm(formula = BDI ~ THP + DUR, data = D_processed)                  # ALM 2, eine kategoriale UV plus eine kontinuierliche Kovariate

# Vollständige Berichte inkl. betaschätzwerte und t-Test Statistiken erhaltet ihr mit der funtion summary()
summary_alm1 <- summary(alm1_fit)
summary_alm2 <- summary(alm2_fit)

# Betaparameterschätzwerte aus den lm-output-Objekten ziehen
beta_hats = list()
beta_hats <- D_processed %>%
  summarize(
    alm1_beta = list(alm1_fit$coefficients),
    alm2_beta = list(alm2_fit$coefficients)
  )

# T-Statistiken für THP aus dem summary objekten ziehen
t_stat_alm1 <- summary_alm1[["coefficients"]][, "t value"]
t_stat_alm2 <- summary_alm2[["coefficients"]][, "t value"]


# Ausgabe der Ergebnisse
cat("Stichprobenmittel BDI (CBT, PHC) : ",   round(c(
                                                filter(means, THP == "CBT")$mean_BDI,
                                                filter(means, THP == "PHC")$mean_BDI),
                                              digits = 2),
    "\nStichprobenmittel DUR (CBT, PHC) : ", round(c(
                                                filter(means, THP == "CBT")$mean_DUR,
                                                filter(means, THP == "PHC")$mean_DUR),
                                              digits = 2),
    "\nBetaparameterschätzer      ALM 1 : ", round(unlist(beta_hats$alm1_beta), 2),
    "\nBetaparameterschätzer      ALM 2 : ", round(unlist(beta_hats$alm2_beta), 2),
    "\nT-Statistik                ALM 1 : ", round(t_stat_alm1, 2),
    "\nT-Statistik                ALM 2 : ", round(t_stat_alm2, 2)
  )

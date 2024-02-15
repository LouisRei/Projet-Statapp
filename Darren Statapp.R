install.packages("dplyr")
install.packages("readxl")
install.packages("summarytools")

library(readxl)
library(dplyr)
library(summarytools)

data2017 <- read_xlsx("C:\\Users\\Tonin Darren\\Downloads\\Statapp\\hospidiag_opendata\\hospidiag_opendata_2017.xlsx", sheet = "hd2017") 
data2017<- mutate(data2017,année=2017) 
data2018 <- read_xlsx("C:\\Users\\Tonin Darren\\Downloads\\Statapp\\hospidiag_opendata\\hospidiag_opendata_2018.xlsx", sheet = "hd2018") 
data2018<- mutate(data2018,année=2018) 
data2019 <- read_xlsx("C:\\Users\\Tonin Darren\\Downloads\\Statapp\\hospidiag_opendata\\hospidiag_opendata_2019.xlsx", sheet = "hd2019") 
data2019<- mutate(data2019,année=2019) 
data2022 <- read_xlsx("C:\\Users\\Tonin Darren\\Downloads\\Statapp\\hospidiag_opendata\\hospidiag_opendata_2022.xlsx", sheet = "hd2022") 
data2022<- mutate(data2017,année=2022) 
data <- bind_rows(data2022, data2019, data2018, data2017)

# Etude des variables financières
# Création d'un nouveau tableau ne contenant que les variables financières

data_Fi <- data[, c("finess", "rs","F1_D", "F1_D_bis", "F2_D", "F3_D", "F4_D", "F5_D", 
                    "F6_D", "F8_D", "F9_D", "F1_O", "F1_O_bis", "F2_O","F3_O", "F4_O",
                    "F5_O", "F6_O", "F8_O", "F9_O", "CI_F1_D","CI_F5_D",
                    "CI_F7_D", "CI_F1_O" ,"CI_F6_O", "CI_F7_O", "CI_F11_O", "CI_F24_O", 
                    "CI_F25_O", "CI_F26_O" )]

# On renomme les variables, on garde un D à la fin pour le public et un O pour
# le privé

data_Fi <- data_Fi %>%
  rename(marge_bruteD = F1_D,
         marge_brute_hors_ACD = F1_D_bis,
         EBITDAO = F1_O,
         EBITDA_hors_ACO = F1_O_bis,
         TT_exploitD = CI_F1_D,
         TT_exploit_MIGFIRD = CI_F5_D,
         TT_exploit_MERRID = CI_F7_D,
         TT_recette_exploitO = CI_F1_O,
         TT_recette_exploit_ACO = CI_F6_O,
         TT_recette_exploit_MERRIO = CI_F7_O,
         TT_charges_exploitO = CI_F11_O,
         Encours_detteO = CI_F24_O,
         roulement_nette_globaleO = CI_F25_O,
         besoin_fonds_roulementO = CI_F26_O)

# Création de nouvelles variables

data_Fi <- mutate(data_Fi, CAF  = coalesce(F2_D, F2_O)
                  ,  CAF_nette = coalesce(F3_D2, F3_O)
                  ,  Duree_dette = coalesce(F4_D, F4_O)
                  ,  Ratio_indepFI  = coalesce(F5_D, F5_O)
                  ,  Encours_dette = coalesce(F6_D, F6_O)
                  ,  Vetuste_equip =  coalesce(F8_D, F8_O)
                  ,  renouvellement_equip = coalesce(F9_D, F9_O))


# Etude des variables EBITDA et taux de Marge

# La variable EBITDAO(bénéfice avant intérêts, impôts, dépréciation et amortissement) mesure la "marge" que l'établissement privé dégage sur son exploitation "courante" pour financer ses charges financières, d'amortissement
# et de provisions, c'est-à-dire pour financer ses investissements. Elle se calcule donc comme suit:
# (EBITDA / CA) x 100 où EBITDA= Résultat net comptable + Charges financières + Impôts et taxes + Dotations aux amortissements et provisions
# ou encore EBITDA = Chiffre d’affaires annuel hors taxes — Achats et charges externes — Charges de personnel — Autres charges

# La variable 
data1 <- data_Fi[, c("finess", "rs", "EBITDAO")]
data2 <- data_Fi[, c("finess", "rs", "marge_bruteD")]

# On enlève les valeurs manquantes

data1 <- data1[!is.na(data1$EBITDAO), ]
data2 <- data2[!is.na(data2$marge_bruteD), ]

# Convertir en variales numériques
data1 <- data1 %>% 
  mutate(EBITDAO = as.numeric(EBITDAO))
data2 <- data2 %>% 
  mutate(marge_bruteD = as.numeric(marge_bruteD))

# On retire les valeurs aberrantes selon le principe de l'écart-type

# Calculer la moyenne et l'écart-type
moyenne1 <- mean(data1$EBITDAO)
ecart_type1 <- sd(data1$EBITDAO)

moyenne2 <- mean(data2$marge_bruteD)
ecart_type2 <- sd(data2$marge_bruteD)

# Définir les seuils
seuil_inf1 <- moyenne1 - 100000 * ecart_type1
seuil_sup1 <- moyenne1 + 100000 * ecart_type1

seuil_inf2 <- moyenne2 - 100000 * ecart_type2
seuil_sup2 <- moyenne2 + 100000 * ecart_type2

# Filtrer les valeurs aberrantes
data1 <- subset(data1, EBITDAO >= seuil_inf1 & EBITDAO <= seuil_sup1)

data2 <- subset(data2, marge_bruteD >= seuil_inf2 & marge_bruteD <= seuil_sup2)


# Résumé statistique
summary(data1)
descr(data1$EBITDAO)

summary(data2)
descr(data2$marge_bruteD)

# Histogrammes
hist(data1$EBITDAO)

hist(data2$marge_bruteD)


# Boîte à moustaches
boxplot(data1$EBITDAO)

boxplot(data2$marge_bruteD)


# Nuage de points (si une autre variable est présente)
plot(data$variable, data$autre_variable)
plot(data$variable, data$autre_variable)





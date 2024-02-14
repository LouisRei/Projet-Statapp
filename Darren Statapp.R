install.packages("dplyr")
install.packages("readxl")
library(readxl)
library(dplyr)
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
# Statistiques descriptives

summary(data_Fi)
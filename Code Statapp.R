install.packages("dplyr")
install.packages("readxl")
install.packages("summarytools")
install.packages("broom")
library(broom)

library(readxl)
library(readr)
library(dplyr)
library(summarytools)

data2017 <- read_xlsx("C:/Users/loui7/Desktop/Statapp/hospidiag_opendata/hospidiag_opendata_2017.xlsx", sheet = "hd2017")
df_2017<- mutate(data2017,année=2017)
data2018 <- read_xlsx("C:/Users/loui7/Desktop/Statapp/hospidiag_opendata/hospidiag_opendata_2018.xlsx", sheet = "hd2018") 
df_2018<- mutate(data2018,année=2018) 
data2019 <- read_xlsx("C:/Users/loui7/Desktop/Statapp/hospidiag_opendata/hospidiag_opendata_2019.xlsx", sheet = "hd2019") 
df_2019<- mutate(data2019,année=2019) 


panel_data <- bind_rows(df_2017,df_2018,df_2019)

# On prend seulement les variables utiles (financières et explicatives): 


panel_data <- panel_data[,c("finess", "rs","F1_D", "F1_D_bis", "F2_D", "F3_D", "F4_D", "F5_D", 
                            "F6_D", "F8_D", "F9_D", "F1_O", "F1_O_bis", "F2_O","F3_O", "F4_O",
                            "F5_O", "F6_O", "F8_O", "F9_O","année", "CI_AC1","A9","A12","P8","P9","P12","RH3","RH4","P1","P14","cat")]


panel_data <- panel_data %>%
  rename(marge_bruteD = F1_D,
         marge_brute_hors_ACD = F1_D_bis,
         EBITDAO = F1_O,
         EBITDA_sans_aide = F1_O_bis,
         nb_lit = CI_AC1,
         sejour_grave = A9, # + ou - hopital publique vs privée ?
         part_urgence = A12,
         utilisation_bloc = P9,
         productivite_biologie = P8,
         chirurgie_ambu = P12,
         activite_chirurgie= RH3,
         sureffectif_cadre = RH4,
         DMS= P1,
         utilisation_place_ambu = P14
         )

panel_data <- mutate(panel_data, CAF  = coalesce(F2_D, F2_O)
                  ,  CAF_nette = coalesce(F3_D, F3_O)
                  ,  Duree_dette = coalesce(F4_D, F4_O)
                  ,  Ratio_indepFI  = coalesce(F5_D, F5_O)
                  ,  Encours_dette = coalesce(F6_D, F6_O)
                  ,  Vetuste_equip =  coalesce(F8_D, F8_O)
                  ,  renouvellement_equip = coalesce(F9_D, F9_O))

panel_data <- select(panel_data, -c("F2_D","F2_O","F3_D","F3_O","F4_D","F4_O","F5_D","F5_O","F6_D","F6_O","F8_D","F8_O","F9_D","F9_O"))

# Trouver les IDs des établissements ayant au moins une année avec toutes les variables à NA
ids_to_remove <- panel_data %>%
  filter(is.na(CAF)) %>%  # Filtrer les lignes où toutes les colonnes sont NA
  distinct(finess) %>%                        # Garder seulement les identifiants uniques
  pull(finess)                                # Extraire les valeurs de 'id'

# On enlève les 3 années de ces établissements pour pas avoir de bug après

panel_data_cleaned <- panel_data %>%
  filter(!finess %in% ids_to_remove)



# Maintenant, la plupart des établissements ont des valeurs non nuls pour les variables. Il reste cependant quelques établissements qui ont des problèmes avec
#  durée dette, encours dette et ratio_indep fi. Puis encore quelques un avec CAF nette mais très peu.


library(plm)

pdata <- pdata.frame(panel_data_cleaned, index = c("finess", "année"))

# on definit la variable log_dms

pdata$DMS <- as.numeric(gsub(",",".", pdata$DMS))
pdata$sureffectif_cadre <- as.numeric(gsub(",",".", pdata$sureffectif_cadre))
pdata$CAF <- as.numeric(gsub(",",".", pdata$CAF))
pdata$sejour_grave <- as.numeric(gsub(",",".", pdata$sejour_grave))
pdata$chirurgie_ambu <- as.numeric(gsub(",",".", pdata$chirurgie_ambu))
pdata$nb_lit <- as.numeric(gsub(",",".", pdata$nb_lit))


# définir indicatrice de privé en interaction avec le publique, la regarder évoluer, effet taille de l'établissement (log de séjour +1), indicatrice d'ambulatoire nulle, lien entre variable genre ln(sejour ambu)*indic(privée)
# log(séjourambu/sejour hospi complete) voir SAE 

pdata <- mutate(pdata, log_dms = log(DMS+1),
                log_cadre = log(sureffectif_cadre),
                log_sejour_grave = log(sejour_grave),
                log_chirurgie_ambu = log(chirurgie_ambu + 1),
                log_lit = log(nb_lit + 1)
                )


pdata_lit_cadre <- pdata[,c("CAF", "nb_lit","DMS","sureffectif_cadre")]
pdata_lit_cadre <- pdata_lit_cadre %>%
  filter(!if_any(c(nb_lit, DMS, sureffectif_cadre), is.na))
pdata_lit_cadre <- pdata_lit_cadre %>%
  filter(!if_any(c(nb_lit, DMS, sureffectif_cadre), is.infinite))

pdata_lit_cadre$nb_lit <- as.numeric(gsub(",",".", pdata_lit_cadre$nb_lit))
pdata_lit_cadre$CAF <- as.numeric(gsub(",",".", pdata_lit_cadre$CAF))
pdata_lit_cadre$DMS <- as.numeric(gsub(",",".", pdata_lit_cadre$DMS))
pdata_lit_cadre$sureffectif_cadre <- as.numeric(gsub(",",".", pdata_lit_cadre$sureffectif_cadre))

modele_test_CAF <- lm(CAF ~   nb_lit + DMS + sureffectif_cadre, pdata_lit_cadre)

summary(modele_test_CAF)

# Régression du taux de CAF sur le log_durée moyenne de séjour + taux de non cadre soignant / cadre soignant (infirmier/aide soignant)

pdata_lit_cadre1 <- pdata[,c("CAF","log_dms","log_cadre")]
pdata_lit_cadre1 <- pdata_lit_cadre1 %>%
  filter(!if_any(c(log_dms, log_cadre), is.na))
pdata_lit_cadre1 <- pdata_lit_cadre1 %>%
  filter(!if_any(c(log_dms, log_cadre), is.infinite))

# pdata_lit_cadre$nb_lit <- as.numeric(gsub(",",".", pdata_lit_cadre$nb_lit))
pdata_lit_cadre1$CAF <- as.numeric(gsub(",",".", pdata_lit_cadre1$CAF))
pdata_lit_cadre1$log_dms <- as.numeric(gsub(",",".", pdata_lit_cadre1$log_dms))
pdata_lit_cadre1$log_cadre <- as.numeric(gsub(",",".", pdata_lit_cadre1$log_cadre))

modele_test_CAF1 <- lm(CAF ~ log_dms + log_cadre, pdata_lit_cadre1)

summary(modele_test_CAF1) 

par(mfrow = c(2, 2))  # Organiser les graphiques en une grille de 2x2
plot(modele_test_CAF1)  # Crée des graphiques de diagnostic pour le modèle


library(ggplot2)

# Graphique de la variable CAF par rapport à un prédicteur : durée moyenne de séjour
ggplot(pdata_lit_cadre1, aes(x = pdata_lit_cadre1$log_dms, y = pdata_lit_cadre1$log_cadre)) +
  geom_point() +
  labs(title = "Effet de DMS sur la CAF", x = "DMS", y = "CAF")

# Graphique de la variable CAF par rapport à un prédicteur : nombre de lit
#ggplot(pdata_lit_cadre, aes(x = pdata_lit_cadre$nb_lit, y = pdata_lit_cadre$CAF)) +
#  geom_point() +
#  labs(title = "Effet du nombre de lit sur la CAF", x = "nombre de lit", y = "CAF")

# On va maintenant rajouter le gravité des séjours, A9, et la part de chirurgie ambu


data_reg_1 <- pdata[, c("CAF","log_dms","log_cadre","log_sejour_grave","log_chirurgie_ambu")]

data_reg_1 <- data_reg_1 %>% filter(!if_any(c(log_dms, log_cadre, log_sejour_grave, log_chirurgie_ambu), is.na))

data_reg_1 <- data_reg_1 %>% filter(!if_any(c(log_dms, log_cadre, log_sejour_grave, log_chirurgie_ambu), is.infinite))

lm_caf_1 <- lm(CAF ~ log_dms + log_cadre + log_sejour_grave + log_chirurgie_ambu,data_reg_1)

summary(lm_caf_1) 

# dans cette régression on a les effets durée de séjour et gravité du séjour qui sont peu significatif 11% et 7 %, puis l'effet proportion de cadre, et l'effet proportion d'ambulatoire.
# on voit un effet négatif de l'ambulatoire sur le taux de CAF, peut-être que simplement le privée a un taux de caf plus faible ce qui fait que
# l'ambulatoire est mal vu. Pour voir enlever l'effet privée/publique, et bien étudier l'ambu sur le taux de CAF il faut séparer privée publique.
# On va utiliser une indicatrice de privée * chirurgie_ambu pour voir si côté publique l'effet est opposé.

data_reg_2 <- pdata[, c("CAF","log_dms","log_cadre","log_sejour_grave","log_chirurgie_ambu", "cat","log_lit")]

# mettre cat dans pdata pour avoir les catégorie publique / privée

data_reg_2 <- mutate(data_reg_2,
                     indic_privee = ifelse(cat == "CHR" | cat == "CH", 0, 1),
                     log_ambu_privee = log_chirurgie_ambu * indic_privee,
                     indic_ambu = ifelse(log_chirurgie_ambu > 0.1 , 0,1),
                     )

data_reg_2 <- data_reg_2 %>% filter(!if_any(c(log_dms, log_cadre, log_sejour_grave, log_chirurgie_ambu, log_ambu_privee), is.na))

data_reg_2 <- data_reg_2 %>% filter(!if_any(c(log_dms, log_cadre, log_sejour_grave, log_chirurgie_ambu, log_ambu_privee), is.infinite))

lm_caf_2 <- lm(CAF ~ log_cadre +log_ambu_privee + log_chirurgie_ambu + indic_privee + log_lit +indic_ambu,data_reg_2)

summary(lm_caf_2)

pdata$DMS <- as.numeric(gsub(",",".", pdata$DMS))
pdata$sureffectif_cadre <- as.numeric(gsub(",",".", pdata$sureffectif_cadre))
data_reg_2$CAF <- as.numeric(gsub(",",".", data_reg_2$CAF))
pdata$sejour_grave <- as.numeric(gsub(",",".", pdata$sejour_grave))
data_reg_2$log_ambu_privee <- as.numeric(gsub(",",".", data_reg_2$log_ambu_privee))


data_reg_2 <- mutate(data_reg_2,
                     pred_CAF = predict(lm_caf_2, newdata = data_reg_2))

class(data_reg_2$log_ambu_privee)
class(data_reg_2$CAF)


# Graphique de dispersion avec ligne de régression

#plot(data_reg_2$log_ambu_privee, data_reg_2$CAF)


# Calcul des effets partiels
# effets_partiels <- coef(lm_caf_2)[-1]

# Graphique des effets partiels
# barplot(effets_partiels, names.arg = names(effets_partiels), col = "skyblue", main = "Effets partiels sur CAF")


# Maintenant on va voir comment impacte la proportion de chirurgie ambulatoire sur le taux de CAF

pdata_lit_ambu <- pdata[,c("CAF", "nb_lit","DMS","chirurgie_ambu")]
pdata_lit_ambu <- pdata_lit_ambu %>%
  filter(!if_any(c(nb_lit, DMS, chirurgie_ambu), is.na))
pdata_lit_ambu <- pdata_lit_ambu %>%
  filter(!if_any(c(nb_lit, DMS, chirurgie_ambu), is.infinite))

pdata_lit_ambu$nb_lit <- as.numeric(gsub(",",".", pdata_lit_ambu$nb_lit))
pdata_lit_ambu$CAF <- as.numeric(gsub(",",".", pdata_lit_ambu$CAF))
pdata_lit_ambu$DMS <- as.numeric(gsub(",",".", pdata_lit_ambu$DMS))
pdata_lit_ambu$chirurgie_ambu <- as.numeric(gsub(",",".", pdata_lit_ambu$chirurgie_ambu))

modele_test_ambu <- lm(CAF ~   nb_lit + DMS + chirurgie_ambu, pdata_lit_ambu)

summary(modele_test_ambu)


library(broom)
tidy_model <- tidy(modele_test_CAF)
print(tidy_model)

# Maintenant on va voir comment l'activité chirurgicale impacte le taux de CAF

pdata_lit_chirurgie <- pdata[,c("CAF", "nb_lit","DMS","activite_chirurgie")]
pdata_lit_chirurgie <- pdata_lit_chirurgie %>%
  filter(!if_any(c(nb_lit, DMS, activite_chirurgie), is.na))
pdata_lit_chirurgie <- pdata_lit_chirurgie %>%
  filter(!if_any(c(nb_lit, DMS, activite_chirurgie), is.infinite))

pdata_lit_chirurgie$nb_lit <- as.numeric(gsub(",",".", pdata_lit_chirurgie$nb_lit))
pdata_lit_chirurgie$CAF <- as.numeric(gsub(",",".", pdata_lit_chirurgie$CAF))
pdata_lit_chirurgie$DMS <- as.numeric(gsub(",",".", pdata_lit_chirurgie$DMS))
pdata_lit_chirurgie$activite_chirurgie <- as.numeric(gsub(",",".", pdata_lit_chirurgie$activite_chirurgie))

modele_test_chirurgie <- lm(CAF ~   nb_lit + DMS + activite_chirurgie, pdata_lit_chirurgie)

summary(modele_test_chirurgie)


# Si on regarde les CAF nette observe t-on un changement ?

pdata_lit_cadre_net <- pdata[,c("CAF_nette", "nb_lit","DMS","sureffectif_cadre")]
pdata_lit_cadre_net <- pdata_lit_cadre_net %>%
  filter(!if_any(c(nb_lit, DMS, sureffectif_cadre,CAF_nette), is.na))
pdata_lit_cadre_net <- pdata_lit_cadre_net %>%
  filter(!if_any(c(nb_lit, DMS, sureffectif_cadre), is.infinite))

pdata_lit_cadre_net$nb_lit <- as.numeric(gsub(",",".", pdata_lit_cadre_net$nb_lit))
pdata_lit_cadre_net$CAF_nette <- as.numeric(gsub(",",".", pdata_lit_cadre_net$CAF_nette))
pdata_lit_cadre_net$DMS <- as.numeric(gsub(",",".", pdata_lit_cadre_net$DMS))
pdata_lit_cadre_net$sureffectif_cadre <- as.numeric(gsub(",",".", pdata_lit_cadre_net$sureffectif_cadre))

modele_test_CAF_net <- lm(CAF_nette ~   nb_lit + DMS + sureffectif_cadre, pdata_lit_cadre_net)

summary(modele_test_CAF_net)


# On introduit une variable de traitement et de temps pour suivre l'effet d'un traitement. Le traitement va être représenté lorsqu'il y a une nette
# évolution d'ambulatoire dans l'établissement hospitalier. On va donc créer deux groupes un qui sera traité et l'autre non.
# On utilise donc la variable : sera_traite qui  vaut 1 si son évolution d'ambulatoire dépasse un certain seuil.
# On regarde pour l'évolution d'ambulatoire la variable : chirurgie_ambu

pdata$chirurgie_ambu <- as.numeric(gsub(",",".", pdata$chirurgie_ambu))
pdata$finess <- as.numeric(gsub(",",".", pdata$finess))

pdata_did <- pdata %>% filter(!is.na(chirurgie_ambu))


pdata_did <- pdata_did %>% 
  arrange(finess, année) %>%
  group_by(finess) 

fill_last_observed <- function(x) {
  # Loop through the vector
  for(i in 2:length(x)){
    if(is.na(x[i])) {
      x[i] <- x[i-1]  # Set NA to the last observed value
    }
  }
  return(x)
}
library(tidyr)

# Calculate the number of entries per group (should equal the number of years per finess)
pdata_did <- pdata_did %>%
  mutate(count_years = n())  # Adds a column with the count of years per finess group

# Apply operations conditionally based on the count of years
pdata_did <- pdata_did %>%
  mutate(
    diff_chirurgie_ambu = ifelse(count_years > 2,
                                 dplyr::lead(chirurgie_ambu, default = chirurgie_ambu[n()]) - chirurgie_ambu,
                                 0)
  )

# Now let's split the dataframe based on the condition and apply the fill_last_observed function
data_with_3_years <- pdata_did %>%
  filter(count_years == 3)

data_with_3_years <- data_with_3_years %>%
  mutate(
    diff_chirurgie_ambu = fill_last_observed(diff_chirurgie_ambu)
  )

data_without_3_years <- pdata_did %>%
  filter(count_years != 3)

# Combine the data back together
final_data <- bind_rows(data_with_3_years, data_without_3_years) %>%
  arrange(finess, année) %>%
  mutate(
    increase_flag = ifelse(diff_chirurgie_ambu > 0.5, 1, 0)  # Assuming the threshold is 10
  ) 

pdata_did <- pdata_did %>%
  arrange(finess, année) %>%
  group_by(finess)

pdata_did <- final_data %>%
  mutate(
    # Shift the increase_flag backward by one year within each group
    prev_increase_flag = lag(increase_flag, 1),
    # Identify rows that are the third year in each group
    is_third_year = row_number() == 3
  ) %>%
  mutate(
    # If it's the third year and the previous year's increase_flag was 1, set increase_flag to 1
    increase_flag = ifelse(is_third_year & prev_increase_flag == 1, 1, increase_flag)
  ) %>%
  select(-prev_increase_flag, -is_third_year)


# Now we have 1 on second years of each finess when, the treatment will be done on these. So now we will take a new variable that will be 0 on second years
# 1 on third year for these finess that name will be : treated
# Then we will be able to do you difference in difference

# Create a treatment indicator variable
pdata_did <- pdata_did %>%
  mutate(
         variable_treatment = ifelse(année == 2018 & increase_flag == 1, 1, 0),
         variable_non_treatment = ifelse(année == 2018 & increase_flag == 0, 1, 0),
         diff_caf = ifelse(count_years > 2,
                                    dplyr::lead(CAF, default = CAF[n()]) - CAF,
                                    0)
         
         )

# Group by finess and check if there's an increase_flag in 2018
treated_finess <- pdata_did %>%
  filter(année == 2018 & increase_flag == 1) %>%
  select(finess)

# Mark establishments with treated finess in 2019
pdata_did <- pdata_did %>%
  mutate(variable_treatment = ifelse(finess %in% treated_finess$finess, 1, 0))

# Create a dummy variable for the post-treatment period
pdata_did <- pdata_did %>%
  mutate(post_treatment = ifelse(année == 2019, 1, 0))

means <- pdata_did %>%
  group_by(variable_treatment, post_treatment) %>%
  summarise(mean_CAF = mean(CAF,na.rm = TRUE))

# Extract the mean CAF for each group and each time period
mean_treated_pre <- means$mean_CAF[which(means$variable_treatment == 1 & means$post_treatment == 0)]
mean_treated_post <- means$mean_CAF[which(means$variable_treatment == 1 & means$post_treatment == 1)]
mean_control_pre <- means$mean_CAF[which(means$variable_treatment == 0 & means$post_treatment == 0)]
mean_control_post <- means$mean_CAF[which(means$variable_treatment == 0 & means$post_treatment == 1)]

# il faut normaliser les variations de caf dans les moyennes rapportés aux nombres de lits

# Calculate the DiD estimate
DiD_estimate <- (mean_treated_post - mean_treated_pre) - (mean_control_post - mean_control_pre)

# Print the DiD estimate

print(DiD_estimate)

# Create an interaction term between treatment and post-treatment
pdata_did <- pdata_did %>%
  mutate(treated = variable_treatment * diff_caf,
         non_treated = variable_non_treatment * diff_caf
         ) # i need to see the diff of  diff of CAF between treated and non treated

# Now you can run your difference-in-differences regression
did_model <- lm(CAF ~ variable_treatment + treated + log_lit + sejour_grave, data = pdata_did)

# Summary of the regression results
summary(did_model)



dt = read_csv("C:\\Users\\loui7\\Desktop\\Statapp\\data.csv")

dt <- dt[,c("finess", "rs","F1_D", "F1_D_bis", "F2_D", "F3_D", "F4_D", "F5_D", 
                            "F6_D", "F8_D", "F9_D", "F1_O", "F1_O_bis", "F2_O","F3_O", "F4_O",
                            "F5_O", "F6_O", "F8_O", "F9_O","année", "CI_AC1","A9","A12","P8","P9","P12","RH3","RH4","P1","P14","cat","SEJHP_MCO","SEJHC_MCO")]


dt <- dt %>%
  rename(marge_bruteD = F1_D,
         marge_brute_hors_ACD = F1_D_bis,
         EBITDAO = F1_O,
         EBITDA_sans_aide = F1_O_bis,
         nb_lit = CI_AC1,
         sejour_grave = A9, # + ou - hopital publique vs privée ?
         part_urgence = A12,
         utilisation_bloc = P9,
         productivite_biologie = P8,
         chirurgie_ambu = P12,
         activite_chirurgie= RH3,
         sureffectif_cadre = RH4,
         DMS= P1,
         utilisation_place_ambu = P14,
         sejour_ambu = SEJHP_MCO,
         sejour_hospi = SEJHC_MCO
  )

dt <- mutate(dt, CAF  = coalesce(F2_D, F2_O)
                     ,  CAF_nette = coalesce(F3_D, F3_O)
                     ,  Duree_dette = coalesce(F4_D, F4_O)
                     ,  Ratio_indepFI  = coalesce(F5_D, F5_O)
                     ,  Encours_dette = coalesce(F6_D, F6_O)
                     ,  Vetuste_equip =  coalesce(F8_D, F8_O)
                     ,  renouvellement_equip = coalesce(F9_D, F9_O))

dt1 <- select(dt, -c("F2_D","F2_O","F3_D","F3_O","F4_D","F4_O","F5_D","F5_O","F6_D","F6_O","F8_D","F8_O","F9_D","F9_O"))

# Trouver les IDs des établissements ayant au moins une année avec toutes les variables à NA
ids_to_remove <- dt1 %>%
  filter(is.na(CAF)) %>%  # Filtrer les lignes où toutes les colonnes sont NA
  distinct(finess) %>%                        # Garder seulement les identifiants uniques
  pull(finess)                                # Extraire les valeurs de 'id'

# On enlève les 3 années de ces établissements pour pas avoir de bug après

dt_cleaned <- dt1 %>%
  filter(!finess %in% ids_to_remove)



# Maintenant, la plupart des établissements ont des valeurs non nuls pour les variables. Il reste cependant quelques établissements qui ont des problèmes avec
#  durée dette, encours dette et ratio_indep fi. Puis encore quelques un avec CAF nette mais très peu.


library(plm)

dtf <- pdata.frame(dt_cleaned, index = c("finess", "année"))

# on definit la variable log_dms

dtf$DMS <- as.numeric(gsub(",",".", dtf$DMS))
dtf$sureffectif_cadre <- as.numeric(gsub(",",".", dtf$sureffectif_cadre))
dtf$CAF <- as.numeric(gsub(",",".", dtf$CAF))
dtf$sejour_grave <- as.numeric(gsub(",",".", dtf$sejour_grave))
dtf$chirurgie_ambu <- as.numeric(gsub(",",".", dtf$chirurgie_ambu))
dtf$nb_lit <- as.numeric(gsub(",",".", dtf$nb_lit))


# définir indicatrice de privé en interaction avec le publique, la regarder évoluer, effet taille de l'établissement (log de séjour +1), indicatrice d'ambulatoire nulle, lien entre variable genre ln(sejour ambu)*indic(privée)
# log(séjourambu/sejour hospi complete) voir SAE 

dtf <- mutate(dtf, log_dms = log(DMS+1),
                log_cadre = log(sureffectif_cadre),
                log_sejour_grave = log(sejour_grave),
                log_chirurgie_ambu = log(chirurgie_ambu + 1),
                log_lit = log(nb_lit + 1),
                sejour_tot = (sejour_ambu + sejour_hospi),
                ambu = sejour_ambu/sejour_tot * 100
)

pdata_did1 <- dtf %>% filter(!is.na(ambu))


pdata_did1 <- pdata_did1 %>% 
  arrange(finess, année) %>%
  group_by(finess) 

fill_last_observed <- function(x) {
  # Loop through the vector
  for(i in 2:length(x)){
    if(is.na(x[i])) {
      x[i] <- x[i-1]  # Set NA to the last observed value
    }
  }
  return(x)
}
library(tidyr)

# Calculate the number of entries per group (should equal the number of years per finess)
pdata_did1 <- pdata_did1 %>%
  mutate(count_years = n())  # Adds a column with the count of years per finess group

# Apply operations conditionally based on the count of years
pdata_did1 <- pdata_did1 %>%
  mutate(
    diff_chirurgie_ambu = ifelse(count_years > 2,
                                 dplyr::lead(ambu, default = ambu[n()]) - ambu,
                                 0)
  )

# Now let's split the dataframe based on the condition and apply the fill_last_observed function
data_with_3_years1 <- pdata_did1 %>%
  filter(count_years == 3)

data_with_3_years1 <- data_with_3_years1 %>%
  mutate(
    diff_chirurgie_ambu = fill_last_observed(diff_chirurgie_ambu)
  )

data_without_3_years1 <- pdata_did1 %>%
  filter(count_years != 3)

# Combine the data back together
final_data1 <- bind_rows(data_with_3_years1, data_without_3_years1) %>%
  arrange(finess, année) %>%
  mutate(
    increase_flag = ifelse(diff_chirurgie_ambu > 0.59, 1, 0)  # Assuming the threshold is 10
  ) 

summary(final_data1$diff_chirurgie_ambu)



pdata_did1 <- final_data1 %>%
  mutate(
    # Shift the increase_flag backward by one year within each group
    prev_increase_flag = lag(increase_flag, 1),
    # Identify rows that are the third year in each group
    is_third_year = row_number() == 3
  ) %>%
  mutate(
    # If it's the third year and the previous year's increase_flag was 1, set increase_flag to 1
    increase_flag = ifelse(is_third_year & prev_increase_flag == 1, 1, increase_flag)
  ) %>%
  select(-prev_increase_flag, -is_third_year)

pdata_did1 <- pdata_did1 %>%
  arrange(finess, année) %>%
  group_by(finess)

# Now we have 1 on second years of each finess when, the treatment will be done on these. So now we will take a new variable that will be 0 on second years
# 1 on third year for these finess that name will be : treated
# Then we will be able to do you difference in difference

# Create a treatment indicator variable
pdata_did1 <- pdata_did1 %>%
  mutate(
    variable_treatment = ifelse(année == 2018 & increase_flag == 1, 1, 0),
    variable_non_treatment = ifelse(année == 2018 & increase_flag == 0, 1, 0),
    diff_caf = ifelse(count_years > 2,
                      dplyr::lead(CAF, default = CAF[n()]) - CAF,
                      0)
    
  )

# Group by finess and check if there's an increase_flag in 2018
treated_finess1 <- pdata_did1 %>%
  filter(année == 2018 & increase_flag == 1) %>%
  select(finess)

# Mark establishments with treated finess in 2019
pdata_did1 <- pdata_did1 %>%
  mutate(variable_treatment = ifelse(finess %in% treated_finess1$finess, 1, 0))

# Create a dummy variable for the post-treatment period
pdata_did1 <- pdata_did1 %>%
  mutate(post_treatment = ifelse(année == 2019, 1, 0))

means1 <- pdata_did1 %>%
  group_by(variable_treatment, post_treatment) %>%
  summarise(mean_CAF = mean(CAF,na.rm = TRUE))

# Extract the mean CAF for each group and each time period
mean_treated_pre1 <- means1$mean_CAF[which(means1$variable_treatment == 1 & means1$post_treatment == 0)]
mean_treated_post1 <- means1$mean_CAF[which(means1$variable_treatment == 1 & means1$post_treatment == 1)]
mean_control_pre1 <- means1$mean_CAF[which(means1$variable_treatment == 0 & means1$post_treatment == 0)]
mean_control_post1 <- means1$mean_CAF[which(means1$variable_treatment == 0 & means1$post_treatment == 1)]

# il faut normaliser les variations de caf dans les moyennes rapportés aux nombres de lits

# Calculate the DiD estimate
DiD_estimate1 <- (mean_treated_post1 - mean_treated_pre1) - (mean_control_post1 - mean_control_pre1)

# Print the DiD estimate

print(DiD_estimate1)

# Create an interaction term between treatment and post-treatment
pdata_did1 <- pdata_did1 %>%
  mutate(treated = variable_treatment * diff_caf,
         non_treated = variable_non_treatment * diff_caf
  ) # i need to see the diff of  diff of CAF between treated and non treated

# Now you can run your difference-in-differences regression
did_model1 <- lm(CAF ~ variable_treatment + treated + log_lit + sejour_grave, data = pdata_did1)

# Summary of the regression results montrer l'étagement des coefs de traitement au fur et a mesure des quartiles avec la reg, fixer le fait d'être tel ou tel quartile
summary(did_model1)







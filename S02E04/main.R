library(data.table)
library(ggplot2)
library(plotly)
library(dplyr)

source("S02E04/src/functions.R")

# source : https://delladata.fr/coupe-du-monde-de-foot-2018-quelle-equipe-va-la-gagner/

# définit le nombre de simulations de chaque match
nbSimulations <- 10
# rajoute 1 si besoin pour avoir un nombre impair et éviter les cas de "50-50"
nbSimulations <- nbSimulations + (nbSimulations+1) %% 2 


# récupération des données (cotes et matches) dans les fichiers d'entrée
cotes   <- read.csv2("S02E04/input/cotes.csv", encoding = "UTF-8")
matches <- read.csv2("S02E04/input/matches.csv", encoding = "UTF-8")

# conversion des cotes en probabilités
cotes$Proba <- 1 - (cotes$Cote / (1 + cotes$Cote))

# affichage du barplot des probabilités de victoire finale par équipe
ggplotly(ggplot(data=cotes, aes(x=reorder(Equipe,-Proba), y=100*Proba)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Probabilités de victoire finale par équipe", x = "Equipe", y = "% de chance"))

# calcul des probabilités de victoires deux à deux
probaCroisees <- cotes %>%
  full_join(cotes, by = character()) %>%
  mutate(Proba1=Proba.x/(Proba.x+Proba.y))

# affichage de la matrice des probabilités croisées de victoire par confrontation
ggplotly(ggplot(probaCroisees, aes(x = reorder(Equipe.x, Proba1), y = reorder(Equipe.y, -Proba1))) + 
  geom_raster(aes(fill=Proba1))  +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Probabilités croisées de victoire", x = "Equipe 1", y = "Equipe 2"))

# affichage de la matrice des probabilités croisées de victoire par confrontation dans chaque groupe
ggplotly(ggplot(probaCroisees %>% filter(Groupe.x==Groupe.y & Equipe.x != Equipe.y), aes(x = reorder(Equipe.x, Proba1), y = reorder(Equipe.y, -Proba1))) + 
  geom_raster(aes(fill=Proba1))  +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  labs(title = "Probabilités croisées de victoire", x = "Equipe 1", y = "Equipe 2") +
  facet_grid(Groupe.y ~ Groupe.x, scales="free"))

# sélection des matches de poule
matchesPoules <- matches %>% filter(Tour %in% c(1,2,3))

# simulation de chaque match de poules
simulationsPoules <- simulationMatches(matchesPoules, probaCroisees, nbSimulations)

# calcul des classements par poule selon le nombre de victoires décroissantes
classementsPoules <- rbind(
  simulationsPoules[,c('Groupe','Equipe1','Resultat')] %>% rename(Equipe=Equipe1), 
  simulationsPoules[,c('Groupe','Equipe2','Resultat')] %>% rename(Equipe=Equipe2) %>% mutate(Resultat = 1-Resultat)
) %>%
  group_by(Groupe, Equipe) %>% 
  summarise(NbVictoires = sum(Resultat)) %>%
  setorder(Groupe, -NbVictoires)

# visualisation graphique des classements de chaque groupe
ggplotly(ggplot(classementsPoules, aes(x = reorder(Equipe, -NbVictoires), y = NbVictoires)) + 
           geom_bar(stat="identity") +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
           labs(title = "Classements de chaque groupe", x = "Equipe", y = "Nombre de victoires") +
           facet_wrap(. ~ Groupe, scales="free"))

# calcul des rangs de chaque équipe par poule selon le nombre de victoires décroissantes
setDT(classementsPoules)[, Rang := 1:.N, by = list(Groupe)]
classementsPoules <- classementsPoules %>% 
  mutate(RangGroupe=paste0(Rang,Groupe))

# détermination des confrontations pour les 1/8 de finale
matchesHuitiemes <- matches %>% 
  filter(Tour=="1/8 de finale") %>%
  merge(classementsPoules, by.x = "Equipe1", by.y = "RangGroupe") %>%
  merge(classementsPoules, by.x = "Equipe2", by.y = "RangGroupe") %>%
  mutate(Equipe1 = Equipe.x) %>%
  mutate(Equipe2 = Equipe.y)

# simulation de chaque match des 1/8 de finale
simulationsHuitiemes <- simulationMatches(matchesHuitiemes, probaCroisees, nbSimulations) %>%
  mutate(Gagnant=ifelse(Resultat,Equipe1,Equipe2))

# détermination des confrontations pour les 1/4 de finale
matchesQuarts <- matches %>%
  filter(Tour=="1/4 de finale") %>%
  merge(simulationsHuitiemes %>% select(c("Id","Gagnant")), by.x = "Equipe1", by.y = "Id") %>%
  merge(simulationsHuitiemes %>% select(c("Id","Gagnant")), by.x = "Equipe2", by.y = "Id") %>%
  mutate(Equipe1 = Gagnant.x) %>%
  mutate(Equipe2 = Gagnant.y)
matchesQuarts

# simulation de chaque match des 1/4 de finale
simulationsQuarts <- simulationMatches(matchesQuarts, probaCroisees, nbSimulations) %>%
  mutate(Qualifie=ifelse(Resultat,Equipe1,Equipe2))

simulationsQuarts

# détermination des confrontations pour les 1/2 finale
matchesDemies <- matches %>%
  filter(Tour=="1/2 finale") %>%
  merge(simulationsQuarts %>% select(c("Id","Qualifie")), by.x = "Equipe1", by.y = "Id") %>%
  merge(simulationsQuarts %>% select(c("Id","Qualifie")), by.x = "Equipe2", by.y = "Id") %>%
  mutate(Equipe1 = Qualifie.x) %>%
  mutate(Equipe2 = Qualifie.y)

# simulation de chaque match des 1/2 finale
simulationsDemies <- simulationMatches(matchesDemies, probaCroisees, nbSimulations) %>%
  mutate(Gagnant=ifelse(Resultat,Equipe1,Equipe2)) %>%
  mutate(Perdant=ifelse(Resultat,Equipe2,Equipe1))

simulationsDemies

# détermination des confrontations pour la petite finale
matchPetiteFinale <- matches %>%
  filter(Tour=="Finale 3e place") %>%
  merge(simulationsDemies %>% select(c("Id","Perdant")), by.x = "Equipe1", by.y = "Id") %>%
  merge(simulationsDemies %>% select(c("Id","Perdant")), by.x = "Equipe2", by.y = "Id") %>%
  mutate(Equipe1 = Perdant.x) %>%
  mutate(Equipe2 = Perdant.y)

# simulation de la petite finale
simulationPetiteFinale <- simulationMatches(matchPetiteFinale, probaCroisees, nbSimulations) %>%
  mutate(Gagnant=ifelse(Resultat,Equipe1,Equipe2)) %>%
  mutate(Perdant=ifelse(Resultat,Equipe2,Equipe1))

simulationPetiteFinale

# détermination des confrontations pour la finale
matchFinale <- matches %>%
  filter(Tour=="Finale") %>%
  merge(simulationsDemies %>% select(c("Id","Gagnant")), by.x = "Equipe1", by.y = "Id") %>%
  merge(simulationsDemies %>% select(c("Id","Gagnant")), by.x = "Equipe2", by.y = "Id") %>%
  mutate(Equipe1 = Gagnant.x) %>%
  mutate(Equipe2 = Gagnant.y)

# simulation de la finale
simulationFinale <- simulationMatches(matchFinale, probaCroisees, nbSimulations) %>%
  mutate(Gagnant=ifelse(Resultat,Equipe1,Equipe2)) %>%
  mutate(Perdant=ifelse(Resultat,Equipe2,Equipe1))

simulationFinale

message(paste0("
************** CLASSEMENT FINAL **************
| Champion du monde    | ", simulationFinale[,"Gagnant"], "
| Finaliste malheureux | ", simulationFinale[,"Perdant"], "
| Lot de consolation   | ", simulationPetiteFinale[,"Gagnant"], "
| Médaille en chocolat | ", simulationPetiteFinale[,"Perdant"]))





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

# simulation d'un nombre donné de tournois
anneeDebut <- 2022
nbTournois <- 3
palmares <- simulationTournois(anneeDebut, nbTournois, probaCroisees, nbSimulations) 

# classement historique des équipes par nombre de victoires, puis nombre de finales, etc
histo <- palmares %>% 
    group_by(Equipe, Classement, Rang) %>%
    summarize(n()) %>%
    arrange(Classement, n()) %>%
    rename("Nombre" = "n()")

histo

# affichage du barplot des probabilités de victoire finale par équipe
ggplotly(ggplot(data=histo, aes(x = reorder(Equipe, Classement), y=Nombre, fill=Rang)) + 
           geom_bar(stat="identity") +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
           labs(title = "Palmarès du tournoi", x = "Equipe"))


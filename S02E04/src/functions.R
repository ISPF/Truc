# simulation de chaque match selon une loi binomiale avec un nombre d'itérations donné à partir des probas croisées 
# la variable Resultat vaut 1 si l'équipe 1 gagne le plus souvent dans les simulations et 0 sinon
simulationMatches <- function (listeMatches, probaCroisees, nbSimulations) {
  
  simu <- listeMatches %>% 
    merge(probaCroisees, by.x = c("Equipe1","Equipe2"), by.y = c("Equipe.x","Equipe.y")) %>%
    dplyr::mutate(Resultat = ifelse(rbinom(n(), nbSimulations, Proba1)/nbSimulations > 0.5, 1, 0)) %>%
    mutate(Gagnant=ifelse(Resultat,Equipe1,Equipe2)) %>%
    mutate(Perdant=ifelse(Resultat,Equipe2,Equipe1))
  
  # print(simu[, c("Tour","Date","Gagnant","Perdant")])
  
  return(simu)
}


# simulation d'un tournoi complet
simulationTournoi <- function (annee, probaCroisees, nbSimulations) {

  print(paste0("Simulation du tournoi ", annee))
  
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
    summarise(NbVictoires = sum(Resultat), .groups="rowwise") %>%
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
  simulationsHuitiemes <- simulationMatches(matchesHuitiemes, probaCroisees, nbSimulations)
  
  # détermination des confrontations pour les 1/4 de finale
  matchesQuarts <- matches %>%
    filter(Tour=="1/4 de finale") %>%
    merge(simulationsHuitiemes %>% select(c("Id","Gagnant")), by.x = "Equipe1", by.y = "Id") %>%
    merge(simulationsHuitiemes %>% select(c("Id","Gagnant")), by.x = "Equipe2", by.y = "Id") %>%
    mutate(Equipe1 = Gagnant.x) %>%
    mutate(Equipe2 = Gagnant.y)
  
  # simulation de chaque match des 1/4 de finale
  simulationsQuarts <- simulationMatches(matchesQuarts, probaCroisees, nbSimulations) %>%
    mutate(Qualifie=ifelse(Resultat,Equipe1,Equipe2))
  
  # détermination des confrontations pour les 1/2 finale
  matchesDemies <- matches %>%
    filter(Tour=="1/2 finale") %>%
    merge(simulationsQuarts %>% select(c("Id","Qualifie")), by.x = "Equipe1", by.y = "Id") %>%
    merge(simulationsQuarts %>% select(c("Id","Qualifie")), by.x = "Equipe2", by.y = "Id") %>%
    mutate(Equipe1 = Qualifie.x) %>%
    mutate(Equipe2 = Qualifie.y)
  
  # simulation de chaque match des 1/2 finale
  simulationsDemies <- simulationMatches(matchesDemies, probaCroisees, nbSimulations)
  
  # détermination des confrontations pour la petite finale
  matchPetiteFinale <- matches %>%
    filter(Tour=="Finale 3e place") %>%
    merge(simulationsDemies %>% select(c("Id","Perdant")), by.x = "Equipe1", by.y = "Id") %>%
    merge(simulationsDemies %>% select(c("Id","Perdant")), by.x = "Equipe2", by.y = "Id") %>%
    mutate(Equipe1 = Perdant.x) %>%
    mutate(Equipe2 = Perdant.y)
  
  # simulation de la petite finale
  simulationPetiteFinale <- simulationMatches(matchPetiteFinale, probaCroisees, nbSimulations)
  
  # détermination des confrontations pour la finale
  matchFinale <- matches %>%
    filter(Tour=="Finale") %>%
    merge(simulationsDemies %>% select(c("Id","Gagnant")), by.x = "Equipe1", by.y = "Id") %>%
    merge(simulationsDemies %>% select(c("Id","Gagnant")), by.x = "Equipe2", by.y = "Id") %>%
    mutate(Equipe1 = Gagnant.x) %>%
    mutate(Equipe2 = Gagnant.y)
  
  # simulation de la finale
  simulationFinale <- simulationMatches(matchFinale, probaCroisees, nbSimulations)
  
  palmares <- rbind(
    data.frame(Annee = annee, Classement = 1, Rang = "Vainqueur", Equipe = simulationFinale[,c("Gagnant")]),
    data.frame(Annee = annee, Classement = 2, Rang = "Finaliste malheureux", Equipe = simulationFinale[,c("Perdant")]),
    data.frame(Annee = annee, Classement = 3, Rang = "Lot de consolation", Equipe = simulationPetiteFinale[,c("Gagnant")]),
    data.frame(Annee = annee, Classement = 4, Rang = "Médaille en chocolat", Equipe = simulationPetiteFinale[,c("Perdant")]),
    data.frame(Annee = annee, Classement = 5, Rang = "1/4 finaliste", Equipe = simulationsQuarts[,c("Perdant")]),
    data.frame(Annee = annee, Classement = 9, Rang = "1/8 finaliste", Equipe = simulationsHuitiemes[,c("Perdant")]),
    data.frame(Annee = annee, Classement = 17, Rang = "Eliminé en poule", Equipe = classementsPoules[Rang>2,c("Equipe")])
  )
  
  print(palmares %>% filter(Classement<5))
  
  return(palmares)
}


# simulation d'un nombre donné de tournois
simulationTournois <- function (anneeDebut, nbTournois, probaCroisees, nbSimulations) {
  
  palmares <- data.frame()
  
  lapply(1:nbTournois, function(i) { 
    
    palmares <<- rbind(palmares, simulationTournoi(anneeDebut + (i-1) * 4, probaCroisees, nbSimulations))
    
  })
  
  return(palmares)
}

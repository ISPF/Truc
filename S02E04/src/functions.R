# simulation de chaque match selon une loi binomiale avec un nombre d'itérations donné à partir des probas croisées 
# la variable Resultat vaut 1 si l'équipe 1 gagne le plus souvent dans les simulations et 0 sinon
simulationMatches <- function (listeMatches, probaCroisees, nbSimulations) {
  
  simu <- listeMatches %>% 
    merge(probaCroisees, by.x = c("Equipe1","Equipe2"), by.y = c("Equipe.x","Equipe.y")) %>%
    dplyr::mutate(Resultat = ifelse(rbinom(n(), nbSimulations, Proba1)/nbSimulations > 0.5, 1, 0))
}
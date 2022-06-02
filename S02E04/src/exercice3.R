# définit le nombre de simulations de chaque match
nbSimulations <- 10
# rajoute 1 si besoin pour avoir un nombre impair et éviter les cas de "50-50"
nbSimulations <- nbSimulations + (nbSimulations+1) %% 2 


# simulation d'un nombre donné de tournois
anneeDebut <- 2022
nbTournois <- 10
palmares <- simulationTournois(anneeDebut, nbTournois, probaCroisees, nbSimulations) 

# classement historique des équipes par nombre de victoires, puis nombre de finales, etc
histo <- palmares %>% 
  group_by(Equipe, Classement, Rang) %>%
  summarise(n(), .groups="rowwise") %>%
  arrange(Classement, n()) %>%
  rename("Nombre" = "n()")

print(histo)
  
# affichage du barplot des probabilités de victoire finale par équipe
ggplotly(ggplot(data=histo, aes(x = reorder(Equipe, Classement), y=Nombre, fill=Rang)) + 
           geom_bar(stat="identity") +
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
           labs(title = "Palmarès du tournoi", x = "Equipe"))

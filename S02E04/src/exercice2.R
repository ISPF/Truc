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

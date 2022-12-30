library(ggmosaic)
#corrige le bug d'affichage des X, non dispo sur la version du CRAN
#devtools::install_github('haleyjeppson/ggmosaic')

graphe2 <- readCSVFile(2)
graphe2[,Archipel:=stringr::str_wrap(Archipel, 12)]
graphe2[,Categorie:=stringr::str_wrap(Categorie, 20)]

r <- unique(graphe2[,Archipel])
graphe2[,Archipel:=factor(Archipel, levels=r, ordered=T)]


g2<-ggplot(graphe2) +
  geom_mosaic(aes(weight=Population,x=product(Archipel),fill=Categorie)) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_y_continuous("", labels = scales::percent)+
  xlab("")+ theme_ispf()
g2

saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)

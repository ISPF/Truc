library(ggrepel)
library(ggplot2)
library(extrafont)

library(showtext)
font_add_google("Roboto", "Roboto")
loadfonts(device = "win")
windowsFonts("Roboto" = windowsFont("Roboto Light"))
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")
cat("Création des graphiques en PDF\n")

#TxtX <- c("Réanimation", "Gynécologie-obstétrique", "Pédiatrie-néonatologie", "Neurologie", "Cardiologie", 
# "Hospitalisation transitoire", "Chirurgie orthopédique", "Chirurgie viscérale", "Gastro-entérologie", 
#"Pneumologie", "Médecine interne", "Néphrologie",
#"Hospitalisation temporaire", "ORL", "Psychiatrie", "Ophtalmologie", "Autres chirurgies", "Diabétologie", 
#"Autres (décès...)", "Oncologie", "Hématologie", 
# "Absence d'information")

graphe1 <- readCSVFile(1)
graphe1[,Service:=stringr::str_wrap(Service, 30)]
graphe1[,Service:=factor(Service, levels=rev(c("Absence d'informations", "Hématologie", "Oncologie","Autres (décès...)", "Diabétologie", "Autres chirurgies", "Ophtalmologie", "Psychiatrie", "ORL","Hospitalisation temporaire","Nephrologie","Médecine interne", "Pneumologie", "Gastro-entérologie", "Chirurgie viscérale", "Chirurgie orthopédique", "Hospitalisation transitoire", "Cardiologie", "Neurologie", "Pédiatrie-néonatologie" , "Gynécologie-obstétrique", "Réanimation"
                                                )))]

g1 <- ggplot(graphe1) +
  geom_col(aes(x = Service, y = Nb, fill=ispfPalette[2], width=0.7))+
  labs(#title = "évasans selon les services d'accueil", 
    x = "Service", y = "Nombre d'évasans") +
  #scale_x_discrete(labels = Service) +
  theme(axis.text.x = element_text(angle =45, hjust = 1, vjust = 1, size = 6)) +
  geom_vline(xintercept = c(5.5, 12.5, 21.5), colour = "gray", size = 1) +
  geom_text(aes(x = reorder(Service, -Nb), y = Nb, label = Nb, vjust = -0.25), size = 2) +
  annotate("text", x = 3, y = 750, label = "65 %", colour = "black", fontface = 2, size = 3) +
  annotate("text", x = 10, y = 750, label = "27 %", colour = "black", fontface = 2, size = 3) +
  annotate("text", x = 17, y = 750, label = "5 %", colour = "black", fontface = 2, size = 3) +
  annotate("text", x = 21.9, y = 750, label = "3 %", colour = "black", fontface = 2, size = 3)+
  theme_ispf()+
  scale_colour_manual(values=ispfPalette)+
  scale_fill_manual(values=ispfPalette) +
  theme(legend.position = "none")
g1


graphe2 <- readCSVFile(2)
graphe2$Annee <- as.integer(as.character(graphe2$variable))

g2 <-  ggplot(graphe2) + 
  geom_col(aes(x = as.factor(Annee), y = Part_mil, fill=ispfPalette[2], width=0.7)) +
  facet_wrap(~reorder(ARCHIPELS, -Part_mil)) +
  theme(axis.text.x = element_text(angle =45, size = 6, hjust = 1, vjust = 1))+
  labs(#title = "Evolution annuelle des évasans par archipel", 
    x = "Année", y = "Nombre d'évasans pour 1000 hab.") +
  theme_ispf()+
  scale_colour_manual(values=ispfPalette)+
  scale_fill_manual(values=ispfPalette) +
  theme(legend.position = "none")

ggsave("Fig_02.png", 
       width = 8, height = 5.22, dpi = 450, units = c("in"))
g2

# Graphique 03----
library(tidyverse)
library(GGally)
library(ggcorrplot)
library(ggrepel)
library(svglite) # pour enregistrer les graphiques en svg


graphe3 <- readCSVFile(3)

g3 <-  ggplot(graphe3, aes(x = Pop_Mean, y = Evasan_Mean)) +
  #stat_smooth(method = "lm", col = "grey",se = FALSE, size = 1) +
  geom_smooth(method = "lm", col = "grey") +
  geom_point(aes(color = factor(ARCHIPELS)), size = 0.5) +
  #scale_color_brewer(type = "qual", direction = -1) +
  labs(
    x = "Nombre moyen d'habitants sur la période",
    y = "Nombre moyen d'Evasans par année",
    #title = "Relation entre le nombre d'Evasans et la population (par Ã®le)",
    color = "Archipel"
  ) +
  geom_text_repel(data = filter(graphe3, ILE_DEPARTB %in% c("TAENGA", "RAIATEA", "UA POU", "UA HUKA", "TETIAROA", "FAKARAVA",
                                                                  "MARUTEA SUD", "PUKARUA", "FAAITE", "RIMATARA", "HEREHERETUE", "TEMATANGI","MOOREA" )),
                  aes(label = ILE_DEPARTB) , size = 2.5) +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  annotate(x=7500, y=8, 
           label=paste("R = ", round(cor(graphe3$Pop_Mean, graphe3$Evasan_Mean),2)), 
           geom="text", size=3, family = "Ubuntu") +
  theme(text = element_text(family = "Ubuntu")) + # choix de la police pour l'ensemble du graphique
  theme(legend.position = "bottom",legend.direction = "horizontal") +
  theme_ispf()+
  scale_colour_manual(values=ispfPalette)+
  scale_fill_manual(values=ispfPalette)

ggsave("graphe3.png", 
       width = 20, height = 13, dpi = 450, units = c("cm"))

g3

# Graphique 04----

graphe4 <- readCSVFile(4)

graphe4$SR_REA_Mean <- as.numeric(graphe4$SR_REA_Mean)

g4 <- ggplot(graphe4, aes(x = SR_REA_Mean, y = REA_SR)) +
  #stat_smooth(method = "lm", col = "grey",se = FALSE, size = 1) +
  geom_smooth(method = "lm", col = "grey") +
  geom_hline(yintercept = 1, color = "grey") + geom_vline(xintercept = 1, color = "grey") +
  #geom_point(aes(color = factor(ARCHIPELS), size = (REA_H + REA_F))) +
  geom_point(aes(color = factor(ARCHIPELS))) +
  scale_color_brewer(type = "qual", palette = "Set3", direction = -1) +
  labs(
    x = "Sexe-ratio de la population (32-64 ans)",
    y = "Sexe-ratio des Evasans (32-64 ans)",
    #title = "Sex-ratio des personnes dirigées en réanimation comparé à celui de la population",
    #subtitle = "Ensemble de la période 2010-2018",
    color = "Archipel"
  ) +
  annotate(x=1.23, y=1.75, 
           label=paste("R = ", round(cor(graphe4$SR_REA_Mean, graphe4$REA_SR),2)), 
           geom="text", size=3) +
  scale_size(expression(paste("Nb. patients \n(réanimation, 32-64 ans)")), range = c(1, 15), limits = c(1, max(graphe4$SR_REA_Mean, graphe4$REA_SR))) +
  geom_text_repel(data = filter(graphe4, ILE_DEPARTB %in% c("RAIATEA", "MOOREA", "HUAHINE",  
                                                                     "RAROIA", "HIVA OA", "MANIHI", "BORA BORA",
                                                                     "RANGIROA", "FAKARAVA", "NUKU HIVA", "RURUTU", "TUBUAI")),
                  aes(label = ILE_DEPARTB) , size = 2.5) +
  
  theme(legend.position = "bottom",legend.direction = "horizontal") +
  guides(col = guide_legend(ncol = 2)) +
  theme_ispf()+
  scale_colour_manual(values=ispfPalette)+
  scale_fill_manual(values=ispfPalette)

ggsave("graphe4.png",
       width = 20, height = 13, dpi = 450, units = c("cm"))
g4

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6.5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 9)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 5)


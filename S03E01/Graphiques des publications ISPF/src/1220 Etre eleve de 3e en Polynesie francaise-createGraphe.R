library(ggrepel)
library(forcats)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe4 <- fread("graphe4.txt", encoding = "UTF-8", nrows = 4)
graphe5 <- fread("graphe5.txt", encoding = "UTF-8")

levelsSubdi <- c("Marquises", "Australes", "Tuamotu-Gambier", "Iles sous le Vent", "Moorea", "Tahiti hors zone urbaine", "Zone urbaine de Tahiti")
levelsG3 <- c("Resté dans le même archipel", "Nouvel archipel entrée primaire", "Nouvel archipel entrée collège",
              "Parti et revenu sur le même archipel","Période hors Polynésie française")

graphe2 <- melt(graphe2,id.vars="Subdivision")
graphe3 <- melt(graphe3,id.vars="Subdivision")
graphe4 <- melt(graphe4,id.vars="Subdivision")
graphe5 <- melt(graphe5,id.vars="Subdivision")


graphe1[, Subdivision:=factor(Subdivision, levels=levelsSubdi)]
graphe2[, Subdivision:=factor(Subdivision, levels=levelsSubdi)]
graphe3[, Subdivision:=factor(Subdivision, levels=levelsSubdi)]
graphe4[, Subdivision:=factor(Subdivision, levels=rev(c("Iles du Vent", "Iles sous le Vent", "Tuamotu-Gambier", "Australes")))]
graphe5[, Subdivision:=factor(Subdivision, levels=rev(c("Iles du Vent", "Iles sous le Vent", "Marquises", "Australes", "Tuamotu-Gambier")))]


graphe2[, variable:=factor(stringr::str_wrap(variable,8), stringr::str_wrap(levelsG3,8))]
graphe2[, Subdivision:=factor(stringr::str_wrap(Subdivision,10), rev(stringr::str_wrap(levelsSubdi,10)))]

graphe3[, variable:=factor(stringr::str_wrap(variable,10), 
                           stringr::str_wrap(c("Chambre", "Autre pièce", "Espace collectif", "Là où je peux", "autre"),10))]

g1 <- ggplot(graphe1,aes(x = fct_infreq(Subdivision),y=value, fill=Subdivision)) +
  geom_bar(stat="identity", width=.8, position = "stack")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=rev(ispfPalette[1:7]))+
  scale_colour_manual(values=rev(ispfPalette[1:7]))+
  theme_ispf()+
  ylab("")+  xlab("")+
  coord_flip()+
  theme(legend.position = "none", legend.direction = "vertical")


g2 <- ggplot(graphe2[order(variable, decreasing=T)],aes(x = Subdivision,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = position_fill(reverse = TRUE))+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5, reverse = T),
            color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette[1:5])+
  scale_colour_manual(values=ispfPalette[1:5])+
  theme_ispf()+
  ylab("")+  xlab("")+
  #coord_flip()+
  theme(legend.position = "bottom", legend.direction = "horizontal")


g3 <- ggplot(graphe3,aes(x = Subdivision,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = position_fill(reverse = TRUE))+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5, reverse=T),
            color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  coord_flip()+
  theme(legend.position = "bottom", legend.direction = "horizontal")

g3

g4 <- ggplot(graphe4,aes(x = Subdivision,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::number(value, accuracy = 0.1)), 
            position = position_dodge2(width = 0.8), 
            hjust=2, color="white", size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  coord_flip()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")


g5 <- ggplot(graphe5,aes(x = Subdivision,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_dodge2(width = 0.8), 
            hjust=2,color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  coord_flip()+
  theme(legend.position = "bottom", legend.direction = "horizontal")

suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=9, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=7, units = "cm", device=cairo_pdf))

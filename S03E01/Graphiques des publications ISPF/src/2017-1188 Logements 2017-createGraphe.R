library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe1 <- melt(graphe1,id.vars="Annee")

g1 <- ggplot(graphe1,  aes(x = Annee, y = value,  fill=variable, colour=variable)) +
  geom_line()+
  scale_x_discrete(limits=c(1988,1996,2002,2007,2012,2017), labels=c(1988,1996,2002,2007,2012,2017))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  ylab("")+  xlab("")+
  theme_ispf()+  
  theme(legend.position = "bottom",
        legend.direction = "horizontal")


graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Subdivision")
graphe2$Subdivision <- factor(graphe2$Subdivision, levels=c("Tuamotu-Gambier", "Australes","Marquises","Iles Sous-Le-Vent", "Iles Du Vent"))
graphe2$variable <- factor(graphe2$variable, levels=rev(levels(graphe2$variable)))


g2 <- ggplot(arrange(graphe2, value),aes(x = Subdivision,y=value)) +
  geom_col(aes(fill = variable))+
  coord_flip()+
  scale_fill_manual(values=ispfPalette[c(7,6,5,4,3,2,1)])+
  scale_colour_manual(values=ispfPalette[c(7,6,5,4,3,2,1)])+
  theme_ispf()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",
        legend.direction = "vertical")


suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=10, units = "cm", device=cairo_pdf))


library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe1$variable <- colnames(graphe1)[2]
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")
colnames(graphe1)[2] <- "Ratio"
g1 <- ggplot(graphe1,  aes(x = Année, y = Ratio, colour=variable, fill=variable)) +
  geom_line()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  ylab("Ratio Demandes/Offres")+
  theme_ispf()+
  theme(legend.position = "none")


g1
suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=5, units = "cm", device=cairo_pdf))

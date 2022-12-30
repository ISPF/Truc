library(ggrepel)
library(gridExtra)

windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#AF7814")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
graphe3 <- fread(sprintf("graphe3.txt", i), encoding = "UTF-8", header = T)
g3 <- ggplot(graphe3, aes(x = Secteur, y=Inscriptions, fill = Secteur, group = Secteur, weight = Inscriptions)) +
  geom_bar(stat = "identity", position=position_stack())+
  geom_text(aes(label=scales::number(Inscriptions, accuracy = 1)),
            size=2, hjust=0.5, color="black", position = position_stack(0.5), size=2)+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position="none")

g3

suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=5, units = "cm", device=cairo_pdf))

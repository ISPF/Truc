library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
i <- 6


graphe1 <- fread("graphe1.txt", encoding = "UTF-8", header = T)
graphe1 <- melt(graphe1,id.vars="Année")
graphe1[, variable:=factor(stringr::str_wrap(variable,18))]
g1 <- ggplot(graphe1, aes(x=Année, y=value, fill=variable, colour=variable)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")

g1
graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Année")
g2 <- ggplot(arrange(graphe2, value),aes(x = Année,y=value)) +
  geom_col(aes(fill = variable))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3 <- melt(graphe3,id.vars="Année")
g3 <- ggplot(arrange(graphe3, value),aes(x = Année,y=value)) +
  geom_col(aes(fill = variable))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
  #                   limits=c(2008,2018))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
g3

graphe4 <- fread("graphe4.txt", encoding = "UTF-8")
graphe4 <- melt(graphe4,id.vars="Année")
graphe4$variable=factor(graphe4$variable, levels = c("Production exportée", "Production non exportée"))
g4 <- ggplot(arrange(graphe4, value),aes(x = Année,y=value)) +
  geom_col(aes(fill = variable))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
g4


suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=7, units = "cm", device=cairo_pdf))

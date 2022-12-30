library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe4 <- fread("graphe4.txt", encoding = "UTF-8")

graphe1 <- melt(graphe1,id.vars="période")
graphe2 <- melt(graphe2,id.vars="période")
graphe3 <- melt(graphe3,id.vars="période")
graphe4 <- melt(graphe4,id.vars="période")

graphe2[,value:=value/100.0]
graphe3[,value:=value/100.0]
graphe4[,value:=value/100.0]


g1 <- ggplot(graphe1,aes(x = période,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
    geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  coord_flip()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "right", legend.direction = "vertical")


g2 <- ggplot(graphe2,aes(x = période,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "stack")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "right", legend.direction = "vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g3 <- ggplot(graphe3,aes(x = période,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "stack")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "right", legend.direction = "vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g4 <- ggplot(graphe4,aes(x = période,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "stack")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "right", legend.direction = "vertical",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=5.5, units = "cm", device=cairo_pdf))

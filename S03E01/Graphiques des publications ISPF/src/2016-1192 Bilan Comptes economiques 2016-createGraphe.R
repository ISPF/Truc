library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8", header = T)
graphe1 <- melt(graphe1,id.vars="Approche Dépense")
graphe1$ApprocheDepense <- graphe1$`Approche Dépense`
graphe1$Annee <- as.character(graphe1$variable)

g1 <- 
  ggplot(graphe1, aes(x=Annee, y=value, fill=ApprocheDepense, colour=ApprocheDepense)) +
  geom_line(aes(group=ApprocheDepense))+
  geom_point(aes(group=ApprocheDepense))+
  geom_text(data=graphe1[ApprocheDepense=="Evolution PIB valeur"],aes(label=scales::percent(value, accuracy = 0.1)), vjust=-1.5, size=2.5)+
  geom_text(data=graphe1[ApprocheDepense=="Evolution PIB en volume"],aes(label=scales::percent(value, accuracy = 0.1)), vjust=2.5, size=2.5)+
  scale_y_continuous(limits = c(-0.05, 0.05),labels = scales::percent_format(accuracy = 0.1))+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")

graphe2 <- fread("graphe2.txt", encoding = "UTF-8", header = T)
graphe2 <- melt(graphe2,id.vars="Approche Dépense")
graphe2$ApprocheDepense <- stringr::str_wrap(graphe2$`Approche Dépense`,20)
graphe2$Annee <- as.integer(as.character(graphe2$variable))

g2 <- 
  ggplot(graphe2) +
  aes(x = ApprocheDepense,  fill = ApprocheDepense, colour = ApprocheDepense, weight = value, label=value) +
  geom_bar() +
  facet_wrap(vars(Annee))+
  geom_text(data=graphe2[`Approche Dépense`=="Croissance du PIB réel"],
            aes(y=value,label=scales::percent(value, accuracy = 0.1)), 
            vjust=-0.5, size=2, colour="black")+
  scale_y_continuous(limits = c(-0.02, 0.025),labels = scales::percent_format(accuracy = 0.1))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())


graphe3 <- fread("graphe3.txt", encoding = "UTF-8", header = T)
graphe3 <- melt(graphe3,id.vars="serie")
graphe3$ApprocheDepense <- stringr::str_wrap(graphe3$serie,25)
graphe3$Annee <- as.integer(as.character(graphe3$variable))

g3 <- ggplot(graphe3) +
  aes(x = Annee,  y=value, fill = ApprocheDepense, colour = ApprocheDepense) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_text(aes(y=value,label=scales::percent(value, accuracy = 0.1)), size=2, 
            vjust=1.6, color="black",
            position = position_dodge(0.9), size=2)+
  scale_y_continuous(limits = c(-0.03, 0.04),labels = scales::percent_format(accuracy = 0.1))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())

graphe4 <- fread("graphe4.txt", encoding = "UTF-8", header = T)
graphe4 <- melt(graphe4,id.vars="FBCF")

g4 <- ggplot(graphe4) +
  aes(x = FBCF, y=value, fill = variable, group = variable, weight = value) +
  geom_bar(stat = "identity", position=position_dodge())+
  geom_text(aes(label=scales::number(value, accuracy = 1)), size=2, 
            hjust=3, color="white",
            position = position_dodge(0.9), size=2)+
  coord_flip()+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
  
graphe5 <- fread("graphe5.txt", encoding = "UTF-8", header = T)
graphe5 <- melt(graphe5,id.vars="Branches")
graphe5$Annee <- as.integer(as.character(graphe5$variable))
graphe5$Branches <- stringr::str_wrap(graphe5$Branches,35)

g5 <- ggplot(graphe5) +
  aes(x = Branches, y=value, fill = variable, group = variable, weight = value) +
  geom_bar(stat = "identity", position=position_dodge())+
  geom_text(aes(label=scales::number(value, accuracy = 1)), size=2, 
            hjust=1.5, color="white",
            position = position_dodge(0.9), size=2)+
  coord_flip()+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "right",legend.direction = "vertical")


graphe6 <- fread("graphe6.txt", encoding = "UTF-8", header = T)
graphe6$Serie <- stringr::str_wrap(graphe6$Serie,25)

g6 <- ggplot(graphe6, aes(x = Serie, y=Contribution, fill = Serie, group = Serie, weight = Contribution)) +
  geom_bar(stat = "identity", position=position_stack())+
  geom_text(aes(label=scales::percent(Contribution, accuracy = 0.1)), size=2, 
            hjust=0.5, color="black",
            position = position_stack(0.5), size=2)+
  coord_flip()+
  scale_y_continuous(limits = c(-0.005, 0.025),labels = scales::percent_format(accuracy = 0.1))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position="none")


graphe7 <- fread("graphe7.txt", encoding = "UTF-8", header = T)
graphe7$Serie <- stringr::str_wrap(graphe7$Serie,25)
#graphe7 <- graphe7[order(graphe7$Repartition, decreasing = T)]

g7 <-  ggplot(graphe7, aes(x = "", y=Repartition, fill = Serie, group = Serie, weight = Repartition)) +
  geom_bar(stat = "identity")+
  coord_polar("y", start=0) +
  geom_text(aes(label=scales::percent(Repartition, accuracy = 0.1)), size=2,  color="white",position = position_stack(0.5), size=2)+
  
  theme_void()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(text = element_text(size = 8, family = "Roboto Light"),
        legend.title = element_blank())

suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe6.pdf", g6, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe7.pdf", g7, width = 9, height=5, units = "cm", device=cairo_pdf))
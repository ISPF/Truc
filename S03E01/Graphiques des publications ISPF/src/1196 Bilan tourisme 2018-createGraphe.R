library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8", header = T)
graphe1$Région <- stringr::str_wrap(graphe1$Région,12)
graphe1 <- melt(graphe1,id.vars="Région")
graphe1$variable <- stringr::str_wrap(graphe1$variable,12)
#???graphe1$Région <- as.factor(graphe1$Région)
#graphe1$Région <- factor(graphe1$Région,levels(graphe1$Région)[c(2,3,1)])

g1 <- ggplot(graphe1,aes(x = Région,y=value, fill=variable, colour=variable)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),
            position = position_stack(vjust = .5),
            color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("Répartition")+
  #xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g1


graphe2 <- fread("graphe2.txt", encoding = "UTF-8", header = T)
graphe2 <- melt(graphe2,id.vars="Année")
#graphe2$variable <- stringr::str_wrap(graphe2$variable,12)
graphe2$variable <- factor(graphe2$variable,levels(factor(graphe2$variable))[c(7,6,5,4,3,2,1)])
graphe2$Année <- as.character(graphe2$Année)


g2 <- ggplot(graphe2,aes(x = Année,y=value, fill=variable, colour=variable)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),
            position = position_stack(vjust = .5),
            color="white", size=2)+
  scale_fill_manual(values=rev(ispfPalette[1:7]))+
  scale_colour_manual(values=rev(ispfPalette[1:7]))+
  #scale_x_discrete(labels = scales::number_format(accuracy = 1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("Nombre de touristes")+ xlab("Année")+
  theme(legend.position = "right",legend.direction = "vertical")
g2



graphe3 <- fread("graphe3.txt", encoding = "UTF-8", header = T)
graphe3 <- melt(graphe3,id.vars="Année")
graphe3$variable <- factor(graphe3$variable,levels(graphe3$variable)[c(4,1,5,3,2,6)])
g3 <- ggplot(graphe3, aes(x=Année, y=value, fill=variable, colour=variable)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("Nombre de touristes")+
  theme(legend.position = "right",legend.direction = "vertical")

g3


graphe4 <- fread("graphe4.txt", encoding = "UTF-8", header = T)
colnames(graphe4)[1] <- "But"
colnames(graphe4)[2] <- "2018"
graphe4$But <- stringr::str_wrap(graphe4$But,12)
graphe4 <- melt(graphe4,id.vars="But")
graphe4$But <- factor(graphe4$But,levels(factor(graphe4$But))[c(5,7,6,4,3,2,1)])


g4 <- ggplot(graphe4,aes(x = But,y=value, fill=But, colour=But)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=scales::percent(value, accuracy = 0.1)),
            position = position_stack(vjust = .5),
            color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  #scale_x_discrete(labels = scales::number_format(accuracy = 1))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_ispf()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Répartition des touristes")+ xlab("But de séjour")+
  theme(legend.position = "right",legend.direction = "vertical")
g4

graphe5 <- fread("graphe5.txt", encoding = "UTF-8", header = T)
graphe5 <- melt(graphe5,id.vars="Année")
graphe5$Année <- factor(graphe5$Année)

g5 <- ggplot(graphe5,aes(x = variable,y=value, fill=Année, colour=Année)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 0.1)),
            position = position_dodge(width=0.9), vjust=2,
            color="white", size=1)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_ispf()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Répartition des touristes")+ xlab("Île")+
  theme(legend.position = "right",legend.direction = "vertical")
g5



graphe7 <- fread("graphe7.txt", encoding = "UTF-8", header = T)
graphe7 <- melt(graphe7,id.vars="Année")
#graphe7$Année <- as.character(graphe7$Année)

#graphe7$variable <- factor(graphe7$variable,levels(graphe7$variable)[c(4,1,5,3,2,6)])
g7 <- ggplot(graphe7, aes(x=Année, y=value, fill=variable, colour=variable)) +
  geom_line()+
  geom_point()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1,big.mark = ''))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("Nombre de touristes")+
  theme(legend.position = "right",legend.direction = "vertical")

g7



graphe8 <- fread("graphe8.txt", encoding = "UTF-8", header = T)
graphe8 <- melt(graphe8,id.vars="Année")
graphe8$Annee <- as.factor(graphe8$Annee)

graphe8_data1 <- graphe8[variable %in% c("REV PAR", "RMC")]
graphe8_data2 <- graphe8[!variable %in% c("REV PAR", "RMC")]


g8 <- ggplot(graphe8, aes(x=Année, y=value, fill=variable, colour=variable)) +
  geom_col(data=graphe8_data1, width = 0.5, position="dodge")+
  geom_line(data=graphe8_data2,aes(x=Année, y=value*42428, group=variable))+
  geom_point(data=graphe8_data2, aes(x=Année, y=value*42428))+
  scale_y_continuous(name = "F.CFP",
                     limits = c(0, 42482),
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~./42482,
                                         name = "Coefficient moyen de remplissage (CMR)", 
                                         labels = scales::percent_format(accuracy = 1)))+
  scale_fill_manual(values=rev(ispfPalette[1:3]))+
  scale_colour_manual(values=rev(ispfPalette[1:3]))+
  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")

g8





graphe9 <- fread("graphe9.txt", encoding = "UTF-8", header = T)
graphe9 <- melt(graphe9,id.vars="Région")


g9 <- ggplot(graphe9, aes(x=Région, y=value, fill=variable, colour=variable)) +
  geom_col(width = 0.5, position="dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),
            position = position_dodge(width=0.5), vjust=3,
            color="white", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
  

g9



suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=5, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=6, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe7.pdf", g7, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe8.pdf", g8, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe9.pdf", g9, width = 9, height=5, units = "cm", device=cairo_pdf))

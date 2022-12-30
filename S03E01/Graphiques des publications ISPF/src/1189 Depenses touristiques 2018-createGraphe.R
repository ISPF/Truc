library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe1 <- melt(graphe1,id.vars="Annee")

graphe1$Annee <- as.factor(graphe1$Annee)


g1 <- ggplot(graphe1, aes(x=Annee, y=value, fill=variable, colour=variable)) +
  geom_col( data=graphe1[variable=="Recettes touristiques (millions de F.CFP)"], width = 0.5)+
  geom_line(aes(x=Annee, y=value*70000, group="variable"),
            data=graphe1[variable=="Poids du tourisme dans les exportations de biens et de services"])+
  geom_point(aes(x=Annee, y=value*70000),
             data=graphe1[variable=="Poids du tourisme dans les exportations de biens et de services"])+
  #scale_x_discrete(limits=c(1995,2000,2005,2011,2013,2015,2018), labels=c(1995,2000,2005,2011,2013,2015,2018))+
  scale_y_continuous(name = "Recettes touristiques\n(millions de F.CFP)",
                     limits = c(0, 70000),
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~./70000, 
                                         name = "Poids du tourisme dans les exportations\nde biens et de services",
                                         labels = scales::percent_format(accuracy = 1)))+
  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "vertical")

graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Depense")
graphe2$Depense <- factor(graphe2$Depense, levels=c("Transports","Excursion Loisir","Commerces et autres",
                                                        "Croisière","Hôtel","Restaurant, Bar, Café",
                                                        "Dépenses moyennes totales"))

g2 <- ggplot(arrange(graphe2, value),aes(x = Depense,y=value)) +
  geom_col(aes(fill = variable))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  coord_flip()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("F.CFP")+  xlab("")+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")



graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3 <- melt(graphe3,id.vars="Pays")
graphe3$Pays <- factor(graphe3$Pays, levels=levels(factor(graphe3$Pays))[c(5,4,3,7,2,1,6,8)])
#graphe3$variable <- factor(graphe3$variable, levels=rev(levels(factor(graphe3$variable))))

g3 <- ggplot(graphe3,aes(x = Pays,y=value)) +
  geom_bar(aes(fill = stringr::str_wrap(variable, 15)), stat="identity")+
  coord_flip()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("")+  xlab("")+
  theme(legend.position = "right",
        legend.direction = "vertical")



graphe4 <- fread("graphe4.txt", encoding = "UTF-8")
graphe4$DépensesSéjour <- as.numeric(graphe4$DépensesSéjour )
graphe4$DépensesJours <- as.numeric(graphe4$DépensesJours)


g4 <- ggplot(graphe4, aes(x = DépensesSéjour, y = DépensesJours, label = Pays)) +
  geom_point() +
  theme_ispf() +
  geom_vline(xintercept = 300000) +
  geom_hline(yintercept = 21418) +
  geom_label_repel(size = 3,
                   fill = ispfPalette[1],
                   colour = "white",
                   min.segment.length = unit(0, "lines"))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  ylab("Dépenses par jour (F.CFP)")+
  xlab("Dépenses par séjour (F.CFP)")
  

suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=7, units = "cm", device=cairo_pdf))

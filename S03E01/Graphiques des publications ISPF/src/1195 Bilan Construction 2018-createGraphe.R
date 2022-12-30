library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")


graphe1 <- fread("graphe1.txt", encoding = "UTF-8", header = T)
graphe1 <- melt(graphe1,id.vars="Secteur")
graphe1$variable <- as.integer(as.character(graphe1$variable))
graphe1$Secteur <- stringr::str_wrap(graphe1$Secteur,25)



g1 <- ggplot(graphe1,aes(x = variable,y=value, fill=Secteur, colour=Secteur)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=scales::number(value, accuracy = 0.1, decimal.mark = ",")),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_x_discrete(limits=unique(graphe1$variable))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("Milliards de F.CFP")+  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")

g1


graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2 <- melt(graphe2,id.vars="Annee")
graphe2$Annee <- as.factor(graphe2$Annee)


g2 <- ggplot(graphe2, aes(x=Annee, y=value, fill=variable, colour=variable)) +
  geom_col( data=graphe2[variable!="Masse Salariale"], width = 0.5)+
  geom_line(aes(group="variable"),data=graphe2[variable=="Masse Salariale"])+
  geom_point(data=graphe2[variable=="Masse Salariale"])+
  geom_text(data=graphe2[variable=="Masse Salariale"], aes(label=scales::percent(value, accuracy = 0.1, decimal.mark = ",")),
            position = position_stack(vjust = .5),color="white", size=2)+
  #scale_x_discrete(limits=c(1995,2000,2005,2011,2013,2015,2018), labels=c(1995,2000,2005,2011,2013,2015,2018))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")

g2

graphe3 <- fread("graphe3.txt", encoding = "UTF-8", header = T)
graphe3 <- melt(graphe3,id.vars="Annee")
graphe3$Annee <- as.factor(graphe3$Annee)
graphe3$variable <- stringr::str_wrap(graphe3$variable,15)

stringr::str_wrap(c ("Montant de retenues salaire", "Salaire net d'un employé au SMIG"),15)

graphe3_set1 <- graphe3[variable %in% stringr::str_wrap(c("Montant de retenues salaire", "Salaire net d'un employé au SMIG"),15)]
graphe3_set2 <- graphe3[variable %in% stringr::str_wrap(c("Salaire net déflaté par l'indice ouvrier"),15)]
graphe3_set3 <- graphe3[variable %in% stringr::str_wrap(c("Taux de cotisations salariales"),15)]
graphe3_set4 <- graphe3_set3[Annee  %in% c(2007,2019)]

g3 <- ggplot(data=graphe3_set1, aes(x=Annee, y=value, fill=variable, colour=variable)) +
  geom_col( width = 0.5)+
  geom_line(data=graphe3_set2, aes(group="variable"))+
  geom_point(data=graphe3_set2)+
  geom_line(data=graphe3_set3, aes(x=Annee, y=value*1365200, group="variable"))+
  geom_text(data=graphe3_set1, aes(label=scales::number(value, accuracy = 1, decimal.mark = ",")),
            position = position_stack(vjust=.5), color="gray", size=2, angle=90)+
  geom_text(data=graphe3_set4, aes(y=value*1365200,
                                   label=scales::percent(value, accuracy = 0.1, decimal.mark = ",")),
            vjust = 2, color="black", size=2)+
  scale_x_discrete(breaks=c(2007,2009,2011,2013,2015,2017,2019))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~./1365200, #name = "taux de cotisations salariales",
                                         labels = scales::percent_format(accuracy = 1),
                                         breaks=c(0,2,4,6,8,10,12,13)/100))+
  theme_ispf()+
  ylab("Montant F.CFP")+xlab("Année")+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme(legend.position = "bottom",legend.direction = "horizontal")



g3

suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))

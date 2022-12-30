graphe1 <- readCSVFile(1)
graphe1 <- graphe1[,.(Année, Décès,`Taux de mortalité`)]
graphe1 <- melt(graphe1, id.vars="Année")

g1 <- ggplot() +
  
  geom_bar(graphe1,  mapping=aes(x=Année, y=Décès,                  group=1), fill=ispfPalette[1], stat="identity",width = 0.6)+
  geom_line(graphe1, mapping=aes(x=Année, y=`Taux de mortalité`*20,  group=1), colour=ispfPalette[2],)+
  scale_x_continuous(breaks=c(1983,1990,2000,2010,2019))+
  scale_y_continuous(name = "Nombre de décès", labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~./20, name = "Taux de mortalité (???)",
                                         labels = scales::number_format(accuracy = 1)))+
  theme_ispf()+
  xlab("")
g1

graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2, id.vars="Année")

g2 <- ggplot() +
  geom_area(graphe2,    mapping=aes(x=Année, y=value, group=variable, fill=variable), position=position_fill(reverse = T))+
  theme_ispf()+
  scale_x_continuous(breaks=c(1983,1990,2000,2010,2019))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_viridis(discrete = T) +
  ylab("")+
  theme(legend.position = "right", legend.direction = "vertical")
g2

graphe3 <- readCSVFile(3)
graphe3[,Date:=as.Date(paste0(AnneeDeces,"-",MoisDeces,"-01"))]
graphe3[,Mois:=format(graphe3$Date, "%b")]
graphe3[,Mois:=factor(Mois, levels=c("janv.","févr.","mars","avr.","mai","juin","juil.","août","sept.","oct.","nov.","déc."))]

library(RColorBrewer)

g3 <- ggplot(graphe3, aes(AnneeDeces, Mois, fill= Ecart)) +
  geom_tile()+
  scale_x_continuous(breaks=c(1983,1990,2000,2010,2019))+
  scale_fill_distiller(palette = "RdYlGn") +
  theme_ispf()+
  ylab("")+
  theme(legend.position = "right", legend.direction = "vertical")
g3


graphe4 <- readCSVFile(4)
graphe4[,DecesDate:=as.Date(DecesDate)]

g4 <- ggplot() +
  geom_line(graphe4, mapping=aes(x=DecesDate, y=`Décès journaliers`, group=1), colour=ispfPalette[1], size=0.1)+
  geom_line(graphe4, mapping=aes(x=DecesDate, y=`Moyenne annuelle`, group=1), colour=ispfPalette[2], size=1)+
  theme_ispf()+
  scale_x_date(date_breaks="3 months", labels = scales::date_format("%b %Y"))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,11), breaks=c(0,2,4,6,8,10))+
  ylab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")
g4

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 5.8)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 5)

library(gridExtra)
cat("Création des graphiques en PDF\n")



graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")
g1 <- ggplot(graphe1,aes(x = Année,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  scale_x_continuous(breaks=c(2015:2021))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")
graphe2_data1 <- graphe2[variable %in% c("Fret aérien")]
graphe2_data2 <- graphe2[variable %in% c("Fret maritime")]
g2 <- ggplot(graphe2, aes(x=Année, y=value, fill=variable, colour=variable)) +
  geom_line(data=graphe2_data1,aes(x=Année, y=value/50, group=variable))+
  geom_line(data=graphe2_data2, aes(x=Année, y=value), size=1.2)+
  scale_x_continuous(breaks=c(2015:2021))+
  scale_y_continuous(name = "Fret marititme",
                     limits=c(10,40),
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~.*50,
                                         name = "Fret aérien", 
                                         labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")

g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Date")
graphe3 <- graphe3[Date<=as.POSIXct("2021-12-31")]
g3 <- ggplot(graphe3, aes(x=Date, y=value, fill=variable, colour=variable)) +
  geom_line()+
  scale_x_datetime(date_breaks = "1 year", date_labels = "%Y")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(20,160), breaks = seq(20,160,20))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  theme(legend.position = "none",legend.direction = "horizontal")

g3



graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id.vars="Année")
graphe4[, variable:=stringr::str_wrap(variable,15)]


g4 <- ggplot(graphe4,aes(x = Année,y=value, fill=variable, colour=variable)) +
  geom_line()+
  scale_x_continuous(breaks=c(2000,2005,2010,2015,2021))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,8))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

g4


graphe5 <- readCSVFile(5)
graphe5 <- melt(graphe5,id.vars="Date")
graphe5[, variable:=stringr::str_wrap(variable,15)]
g5 <- ggplot(graphe5,aes(x = Date,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  scale_x_continuous(breaks=c(2015:2021))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")

g5


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 7)


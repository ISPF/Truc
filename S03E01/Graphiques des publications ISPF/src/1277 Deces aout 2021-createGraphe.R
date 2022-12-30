cat("Création des graphiques en PDF\n")


#graphe1 <- readCSVFile(1)
#graphe1 <- melt(graphe1,id.vars="Mois")
#colnames(graphe1)[3:4] <- c("2020","2021")

graphe1 <- readCSVFileAndMelt(1, keysList = "Mois", isVariableOrdered = T)
graphe1[variable=="2020.0",variable:="2020"]
graphe1[variable=="2021.0",variable:="2021"]


dataLabels <- graphe1[Mois=="aou"]

dataLabels[variable!="2021", value:=NA]


g1 <- ggplot(graphe1,aes(x = Mois,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(data=dataLabels, aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")
g1


graphe2 <- readCSVFileAndMelt(2, keysList = "Mois", isVariableOrdered = T)
graphe2[variable=="2020.0",variable:="2020"]
graphe2[variable=="2021.0",variable:="2021"]

dataLabels <- graphe2[(Mois=="déc" & variable!=2021) | (Mois=="aou" & variable==2021)]

g2 <- ggplot(graphe2,aes(x = Mois,y=value, color=variable, group=variable)) +
  geom_line(size=1.1)+
  geom_text(data=dataLabels,aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Mois")

dataLabels <- graphe3[nrow(graphe3)]

g3 <- ggplot(graphe3,aes(x = Mois,y=value, color=variable)) +
  geom_line()+
  geom_text(data=dataLabels,aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
#  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1985,2020,1))+
  scale_y_continuous(name = "Nombre de décès",labels = scales::number_format(accuracy = 1), limits=c(0,650))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")+
  guides(col = guide_legend(ncol = 2))
g3


graphe4 <- readCSVFile(4)
colnames(graphe4)[2] <- "2021"
graphe4 <- melt(graphe4,id.vars="age")

g4 <- ggplot(graphe4,aes(x = age,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  coord_flip()
g4


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6.5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6.5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 6.5)

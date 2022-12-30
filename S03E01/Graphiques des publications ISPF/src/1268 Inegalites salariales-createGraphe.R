graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Indicateur")
graphe1[,Indicateur:=factor(Indicateur, levels=c("Public", "Privé", "Ensemble"))]
g1 <- ggplot(graphe1,aes(x = Indicateur,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),vjust=-0.5, size=2)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2010,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,350000))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("F.CFP par mois")+  xlab("")+
  theme(legend.position = "bottom")

g1


graphe2 <- readCSVFile(2)
graphe2$Indicateur <- stringr::str_wrap(graphe2$Indicateur,10)
graphe2 <- graphe2[Indicateur!="Ensemble"]
lev <- graphe2$Indicateur
graphe2 <- melt(graphe2,id.vars="Indicateur")
graphe2[,Indicateur:=factor(Indicateur, levels=lev)]

g2 <- ggplot(graphe2,aes(x = Indicateur,y=value, color=variable, group=variable)) +
  geom_line()+
  geom_point()+
  geom_text_repel(aes(label=scales::number(value, accuracy = 1)), size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,500000))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("F.CFP par mois")+  xlab("")+
  theme(legend.position = "bottom")

g2


graphe3 <- readCSVFile(3)
graphe3 <- graphe3[Indicateur!="Ensemble"]
lev <- graphe3$Indicateur
graphe3 <- melt(graphe3,id.vars="Indicateur")
graphe3[,Indicateur:=factor(Indicateur, levels=lev)]

g3 <- ggplot(graphe3,aes(x = Indicateur,y=value, fill=variable, color=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),vjust=-0.5, size=1.8)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2010,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,450000))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("F.CFP par mois")+  xlab("")+
  theme(legend.position = "bottom")

g3

graphe4 <- readCSVFile(4)
colnames(graphe4) <- c("Indicateur", "value")
graphe4 <- graphe4[Indicateur!="Ensemble"]
graphe4 <- melt(graphe4,id.vars="Indicateur")




g4 <- ggplot(graphe4,aes(x = Indicateur,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),vjust=-0.5, size=2)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,320000))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("F.CFP par mois")+  xlab("")+
  theme(legend.position = "none")

g4


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 4.5)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 8)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 8.5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 6)

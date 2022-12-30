graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")
graphe1[, variable:=factor(stringr::str_wrap(variable,12))]


g1 <- ggplot(graphe1,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(2010,2019,1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g1

graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[, variable:=factor(stringr::str_wrap(variable,18))]


g2 <- ggplot(graphe2,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(2010,2019,1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g2


graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Année")
graphe3[, variable:=factor(stringr::str_wrap(variable,18))]

g3 <- ggplot(graphe3) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(1999,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     breaks=seq(0,8000,2000),
                     limits=c(0,8000))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g3

graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id.vars="Année")
graphe4[,variable:=factor(variable, levels=c("Paraha peue", "Crevette"))]


g4 <- ggplot(graphe4) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = c(2008:2019))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     breaks=seq(0,160,20))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")
g4

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)

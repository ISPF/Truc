library(forcats)
graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")
graphe1[,Année:=factor(Année)]
graphe1[,variable:=factor(variable, levels=c("Paraha peue","Crevette"))]



g1 <- ggplot(arrange(graphe1, value),aes(x = Année,y=value)) +
  geom_col(aes(fill = variable))+
  scale_fill_manual(values=rev(ispfPalette[1:2]))+
  scale_colour_manual(values=rev(ispfPalette[1:2]))+
  theme_ispf()+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")
g1

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)

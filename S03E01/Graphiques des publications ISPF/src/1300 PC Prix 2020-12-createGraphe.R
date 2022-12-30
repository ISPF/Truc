graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Date")

g1 <- ggplot(graphe1,aes(x = Date,y=value, colour=variable, group=variable)) +
  geom_line(stat="identity")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")
g1


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)

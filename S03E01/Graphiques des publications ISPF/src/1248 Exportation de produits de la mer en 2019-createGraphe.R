graphe1 <- readCSVFile(1)
graphe1[, Année:=as.Date(sprintf("%s-01-01", Année))]
graphe1[, `Exportations (F.CFP)`:=as.integer(`Exportations (F.CFP)`/10^6)]
graphe1[, `Exportations (kilos)`:=as.integer(`Exportations (kilos)`/10^3)]
colnames(graphe1)[3] <- "Exportations (Tonnes)"
grapheImpExp <- melt(graphe1,id.vars="Année")

g1 <- ggplot() + 
  geom_bar(data=grapheImpExp[variable=="Exportations (F.CFP)"], 
           mapping = aes(x = Année, y = value, fill="Exportations (F.CFP)"), stat = "identity" )+ 
  geom_line(data=grapheImpExp[variable=="Exportations (Tonnes)"],
            mapping = aes(x = Année, y = value, colour="Exportations (Tonnes)"))+
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(grapheImpExp$Année), max(grapheImpExp$Année),by = "2 year"))+
               
  scale_y_continuous(name = "Millions de F.CFP)", labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~., name = "Tonnes",
                                         labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")

g2 <- ggplot(graphe2) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''),
                     breaks = seq(1999,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     breaks=seq(0,8500,2000),
                     limits=c(0,8500))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("Tonnes")+  xlab("")+
  theme(legend.position = "bottom")
g2

graphe3 <- readCSVFile(3)
graphe3[, Année:=as.Date(sprintf("%s-01-01", Année))]
graphe3[, `Exportations (F.CFP)`:=as.integer(`Exportations (F.CFP)`/10^6)]
graphe3[, `Exportations (volume)`:=as.integer(`Exportations (volume)`/10^3)]
colnames(graphe3)[3] <- "Exportations (Tonnes)"
grapheImpExp <- melt(graphe3,id.vars="Année")

g3 <- ggplot() + 
  geom_bar(data=grapheImpExp[variable=="Exportations (F.CFP)"], 
           mapping = aes(x = Année, y = value, fill="Exportations (F.CFP)"), stat = "identity" )+ 
  geom_line(data=grapheImpExp[variable=="Exportations (Tonnes)"],
            mapping = aes(x = Année, y = value, colour="Exportations (Tonnes)"))+
  scale_x_date(date_labels = "%Y",
               breaks = seq(min(grapheImpExp$Année), max(grapheImpExp$Année),by = "2 year"))+
  
  scale_y_continuous(name = "Millions de F.CFP)", labels = scales::number_format(accuracy = 1),
                     limits=c(0,60),
                     sec.axis = sec_axis(~., name = "Tonnes",
                                         labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g3


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 8)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)

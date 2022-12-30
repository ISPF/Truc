

graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")
g1 <- ggplot() + 
  geom_bar(data=graphe1[variable!="Variation annuelle"], 
           mapping = aes(x = Année, y = value, fill=variable), stat = "identity", position="dodge")+ 
  geom_line(data=graphe1[variable=="Variation annuelle"],size=1.1, 
            mapping = aes(x = Année, y = value*1100, colour=variable))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""),)+
  scale_y_continuous(name = expression(""), 
                     labels = scales::number_format(accuracy = 1),
                     limits=c(-10, 110),
                     breaks=seq(-10,110,10),
                     sec.axis = sec_axis(~ ./1000,name = "",labels = scales::percent_format(accuracy = 1),
                                         # limits=c(-0.02, 0.12),
                                          breaks=seq(-0.01,0.11,.03)
                                         ))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank())
g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Part")
graphe2[,Part:=stringr::str_wrap(Part, 15)]

g2 <- ggplot(graphe2,aes(x = Part,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Part,y=value, fill=variable), stat="identity", position = "dodge")+
  #geom_text(aes(label=scales::percent(value, accuracy = 0.1)), color="black", size=2)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "vertical")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2



graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Année")

g3 <- ggplot(graphe3,aes(x = Année,y=value, color=variable)) +
  geom_line(size=1.1)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2011,2022,1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),name = expression(""), limits=c(85,115))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))+
  theme(legend.text=element_text(size=rel(0.6)))

g3



graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id.vars="Année")

g4 <- ggplot(graphe4,aes(x = Année,y=value, color=variable)) +
  geom_line(size=1.1)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),name = expression(""), limits=c(85,130))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))+
  theme(legend.text=element_text(size=rel(0.6)))

g4




graphe5 <- readCSVFile(5)
graphe5 <- melt(graphe5,id.vars="Année")
graphe5[,Année:=stringr::str_wrap(Année, 25)]

g5 <- ggplot(graphe5,aes(x = Année,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity")+
  #geom_text(aes(label=scales::percent(value, accuracy = 0.1)), color="black", size=2)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-0.02,0.06))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  theme(axis.text.x= element_text(size = 5))+
  guides(fill = guide_legend(nrow = 1))


g5

graphe6 <- readCSVFile(6)
graphe6 <- melt(graphe6,id.vars="Année")
graphe6[,Année:=stringr::str_wrap(Année, 15)]


g6 <- ggplot(graphe6,aes(x = Année,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Année,y=value, fill=variable), stat="identity", position="dodge")+
  #geom_text(aes(label=scales::percent(value, accuracy = 0.1)), position = position_dodge(width=1), color="black", size=2)+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  theme(axis.text.x= element_text(size = 4))+
  guides(fill = guide_legend(nrow = 1))


g6


graphe7 <- readCSVFile(7)
graphe7 <- melt(graphe7,id.vars="Année")

g7 <- ggplot(graphe7,aes(x = Année,y=value, color=variable)) +
  geom_line(size=1.1)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),name = expression(""), limits=c(90,140))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g7




graphe8 <- readCSVFile(8)
graphe8 <- melt(graphe8,id.vars="Année")

g8 <- ggplot(graphe8,aes(x = Année,y=value, color=variable)) +
  geom_line(size=1.1)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),name = expression(""), limits=c(40,120))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))+
  theme(legend.text=element_text(size=rel(0.7)))


g8


graphe9 <- readCSVFile(9)
graphe9 <- melt(graphe9,id.vars="Année")

g9 <- ggplot(graphe9,aes(x = Année,y=value, color=variable)) +
  geom_line(size=1.1)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021,2022))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),name = expression(""), limits=c(80,120))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g9






saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(9, largeurCM = 9, hauteurCM = 7)

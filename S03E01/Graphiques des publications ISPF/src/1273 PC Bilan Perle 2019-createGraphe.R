library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
i <- 4

createSimpleGGPlot <- function(i){
  graphe <- fread(sprintf("graphe%d.txt", i), encoding = "UTF-8", header = T)
  graphe <- melt(graphe,id.vars="Année")
  #graphe3$variable <- factor(graphe3$variable,levels(graphe3$variable)[c(4,1,5,3,2,6)])
  g <- ggplot(graphe, aes(x=Année, y=value, fill=variable, colour=variable)) +
    geom_line()+
    geom_point()+
    scale_y_continuous(labels = scales::number_format(accuracy = 1), )+
    scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''))+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+
    theme(legend.position = "bottom",legend.direction = "horizontal")
  g
}

graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Année")
g1 <- ggplot(graphe1,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2004,2020,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(150,350),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none")

g1

graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Année")
graphe2[,variable:=stringr::str_wrap(variable,20)]
g2 <- ggplot(graphe2,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2004,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(50,150),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom") + guides(col = guide_legend(ncol = 3))

g2



graphe3 <- readCSVFile(3)
graphe3[,`Valeur Exports des produits perliers`:=as.integer(`Valeur Exports des produits perliers`/10^9)]
graphe3 <- melt(graphe3,id.vars="Année")
graphe3[,variable:=stringr::str_wrap(variable,20)]


g3 <- ggplot() + 
  geom_bar(data=graphe3[variable!="Valeur Exports des\nproduits perliers"], 
           mapping = aes(x = factor(Année), y = value, fill=variable), stat="identity", position="dodge")+ 
  geom_line(data=graphe3[variable=="Valeur Exports des\nproduits perliers"],size=1.1,
            mapping = aes(x = factor(Année), y = value*1000,group=1, colour=variable))+
  scale_y_continuous(name = expression("Poids Exports des produits perliers (kg)"), 
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~ ./1000  , name = "Milliards de F.CFP",labels = scales::number_format(accuracy = 1),))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal",
        axis.title.x = element_blank()) + 
  guides(col = guide_legend(ncol = 2))
g3


graphe4 <- readCSVFile(4)
colnames(graphe4) <- c("Année", "Poduits perliers", "Exportations locales")
graphe4 <- melt(graphe4,id.vars="Année")
graphe4[,value:=value/10^9]

g4 <- ggplot(graphe4,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2010,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(4.5,13),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("Milliards de F.CFP")+  xlab("")+
  theme(legend.position = "bottom")+
  guides(col = guide_legend(ncol = 2))

g4



graphe5 <- readCSVFile(5)
graphe5 <- melt(graphe5,id.vars="Année")
g5 <- ggplot(graphe5,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2004,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(50,150),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g5


graphe6 <- readCSVFile(6)
graphe6 <- melt(graphe6,id.vars="Année")
g6 <- ggplot(graphe6,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2010,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(400,650),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("en F.CFP")+  xlab("")+
  theme(legend.position = "bottom")

g6


graphe7 <- readCSVFile(7)
graphe7 <- melt(graphe7,id.vars="Année")
g7 <- ggplot(graphe7,aes(x = Année,y=value, color=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2010,2019,2))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(40,100),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")

g7







saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 8)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 4.5)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 4.5)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 4.5)

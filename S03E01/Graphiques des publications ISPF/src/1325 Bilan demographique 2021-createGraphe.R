graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1[,1:3],id.vars="Annee")

dfLegendGraphe1 <- data.frame(stringsAsFactors=FALSE,
                              Type = c("Naissances en vie : 3 496 ", "Décès : 2 274", "Solde naturel : 1 222"),
                              varX = c(2015, 2015, 2010),
                              varY = c(5000, 800, 2500),
                              size=3
)
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

g1 <- ggplot(graphe1,  aes(x = Annee, y = value,  fill=variable, colour=variable)) +
  geom_line(data=graphe1[variable!="Décès"],size = 0.5,position='identity') +
  geom_line(data=graphe1[variable=="Décès"],size = 0.5,position='identity') +
  geom_label(data=dfLegendGraphe1,
             aes(x = varX, y=varY, label=Type, family="Roboto Light"), 
             size=2.5, colour=c("white", "white", "black"),
             fill=custom.col)+
  #geom_label_repel(data=graphe1[Annee==2021],
  #aes(Annee, value, label = format(value, nsmall=0, big.mark=" ")),
  #colour="white", size=3, show.legend = F)+
  #scale_colour_brewer(palette = "Set2")+
  #scale_fill_brewer(palette = "Set2")+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(limits = c(0,6000))+  ylab("")+  xlab("")+
  theme_ispf()+  theme(legend.position = "none")
g1


graphe2 <- readCSVFile(2)
graphe2$Age <- factor(graphe2$Age)
graphe2$Age <- relevel(graphe2$Age, "Moins de 5 ans")
g2 <- ggplot(graphe2,aes(x = Age,y=effectif)) +
  geom_bar(data = graphe2[Annee == 2020], stat = "identity", aes(fill = factor(Sexe)),alpha=0.8, width=0.8) +
  geom_line(data = graphe2[Annee ==2001 & Sexe=="Homme"], stat = "identity",aes(linetype=factor(Annee), group=1),size=0.5) +
  geom_line(data = graphe2[Annee ==2001 & Sexe=="Femme"], aes(linetype=factor(Annee), group=1), stat = "identity",size=0.5) +
  coord_flip() + 
  xlab("") +  ylab("")+
  scale_y_discrete(limits=c(-10000,-5000,0,5000,10000), labels=c("10 000","5 000","0","5 000", "10 000"))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g2

graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Année")
g3 <- ggplot(data=graphe3, 
       aes(Année, value, fill=variable, color=variable, group=variable)) +
  geom_line()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  ylab("")+  xlab("")+
  theme_ispf()

graphe4 <- readCSVFile(4) 
graphe4 <- melt(graphe4,id.vars="Annee")



g4 <- ggplot() +
  geom_area(data=graphe4[variable!="ICF"],
            aes(Annee, value, fill=variable, color=variable, group=variable))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  ylab("")+  xlab("")+
  theme_ispf()
g4  

graphe5 <- readCSVFile(5) 
graphe5[, Années:=factor(Années)]
g5 <- ggplot(graphe5, aes(x = Années, y = Valeur, group = Taux, color=Taux)) +
  geom_line(size = 0.5) +
  #geom_label(data=graphe5 %>% group_by(Taux) %>% arrange(desc(Années)) %>% slice(1),
             #aes(x = "1994 - 1998", label=Taux, family="Roboto Light"), size=3)+
  scale_x_discrete(breaks = levels(graphe5$Années)[c(2,7,12,17,22,27,32,37)])+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")
  #geom_label_repel(data=graphe5[Années=="2021"],
                   #aes(Années, Valeur, label = format(Valeur, digits=4, nsmall=0, big.mark=" ", decimal.mark = ",")),
                   #size=3,
                   #show.legend = FALSE)
g5


#anciens

graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1[,1:3],id.vars="Annee")

dfLegendGraphe1 <- data.frame(stringsAsFactors=FALSE,
                              Type = c("Naissances en vie : 3496 ", "Décès : 2274", "Solde naturel : 1222"),
                              varX = c(2005, 2005, 2000),
                              varY = c(5000, 1000, 3000)
)
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

g1 <- ggplot(graphe1,  aes(x = Annee, y = value,  fill=variable, colour=variable)) +
  geom_area(data=graphe1[variable!="Décès"],size = 0.5,position='identity') +
  geom_area(data=graphe1[variable=="Décès"],size = 0.5,position='identity') +
  geom_label(data=dfLegendGraphe1,
             aes(x = varX, y=varY, label=Type, family="Roboto Light"), 
             size=3, colour=c("white", "white", "black"),
             fill=custom.col)+
  #geom_label_repel(data=graphe1[Annee==2021],
  #aes(Annee, value, label = format(value, nsmall=0, big.mark=" ")),
  #colour="white", size=3, show.legend = F)+
  #scale_colour_brewer(palette = "Set2")+
  #scale_fill_brewer(palette = "Set2")+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(limits = c(0,6000))+  ylab("")+  xlab("")+
  theme_ispf()+  theme(legend.position = "bottom")
g1

######

graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1[,1:3],id.vars="Annee")

dataLabels <- graphe1[Annee=="2021"]

datalabels[variable!="2021",value:=NA]

#dfLegendGraphe1 <- data.frame(stringsAsFactors=FALSE,
#Type = c("Naissances en vie : 3496 ", "Décès : 2274", "Solde naturel : 1222"),
#varX = c(2005, 2005, 2000),
#varY = c(5000, 1000, 3000)
#)
#custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

g1 <- ggplot(graphe1,  aes(x = Annee, y = value, colour=variable)) +
  geom_line()+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(1980,2021,2))+
  geom_text(data=dataLabels, aes(label=scales::number(value, accuracy =1)), position = position_dodge(width=0.8),color="black",size=2)+
  #annotate(geom="text", x="2021", y=1000, labels="solde migratoire : 1 222", color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  ylab("")+  xlab("")+
  theme_ispf()+  theme(legend.position = "bottom")
geom_label_repel(data=graphe1[Annee=="2021"],
                 aes(Annee, Décès, label = format(Décès, digits=4, nsmall=0, big.mark=" ", decimal.mark = ",")),
                 size=3,
                 show.legend = FALSE)
g1

#######

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 7)

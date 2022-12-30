graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1[,1:3],id.vars="Annee")

dfLegendGraphe1 <- data.frame(stringsAsFactors=FALSE,
                              Type = c("Naissances en vie", "Décès", "Solde naturel"),
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
  geom_label_repel(data=graphe1[Annee==2020],
                   aes(Annee, value, label = format(value, nsmall=0, big.mark=" ")),
                   colour="white", size=3, show.legend = F)+
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
  geom_line(data = graphe2[Annee ==2000 & Sexe=="Homme"], stat = "identity",aes(linetype=factor(Annee), group=1),size=0.5) +
  geom_line(data = graphe2[Annee ==2000 & Sexe=="Femme"], aes(linetype=factor(Annee), group=1), stat = "identity",size=0.5) +
  coord_flip() + 
  xlab("") +  ylab("")+
  scale_y_discrete(limits=c(-10000,-5000,0,5000,10000), labels=c(10000,5000,0,5000, 10000))+
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
  geom_label(data=graphe5 %>% group_by(Taux) %>% arrange(desc(Années)) %>% slice(1),
             aes(x = "1994 - 1998", label=Taux, family="Roboto Light"), size=3)+
  scale_x_discrete(breaks = levels(graphe5$Années)[c(2,7,12,17,22,27,32,33)])+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  geom_label_repel(data=graphe5[Années=="2016 - 2020"],
                   aes(Années, Valeur, label = format(Valeur, digits=4, nsmall=0, big.mark=" ", decimal.mark = ",")),
                   size=3,
                   show.legend = FALSE)

g5
saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 7)

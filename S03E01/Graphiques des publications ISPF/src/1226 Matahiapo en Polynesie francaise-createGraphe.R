cat("Création des graphiques en PDF\n")

graphe1 <- readCSVFile(1)

graphe1$Age <- factor(graphe1$Age)
graphe1$Age <- relevel(graphe1$Age, "Moins de 5 ans")



g1 <- ggplot(graphe1,aes(x = Age,y=effectif)) +
  geom_bar(data = graphe1[Annee == 2019], stat = "identity", aes(fill = factor(Sexe)),alpha=0.8, width=0.8) +
  geom_line(data = graphe1[Annee ==2030 & Sexe=="Homme"], stat = "identity",aes(linetype=factor(Annee), group=1),size=0.5) +
  geom_line(data = graphe1[Annee ==2030 & Sexe=="Femme"], aes(linetype=factor(Annee), group=1), stat = "identity",size=0.5) +
  coord_flip() + 
  xlab("") +  ylab("") + theme_ispf()+
  #scale_y_continuous(limits=c(-2000,-1000,0,1000,2000))+ #, labels=c(-2000,-1000,0,1000,2000))+
  #scale_colour_brewer(palette = "Set2")+
  #scale_fill_brewer(palette = "Set2")+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

g1


graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3[, `Tranche pension`:=stringr::str_wrap(`Tranche pension`,16)]

graphe3[, `Tranche pension`:=factor(`Tranche pension`, 
                                    levels=stringr::str_wrap(c("<=80.000 FCP","Entre 80.001F et 152.913F",
                                             "Entre 152.914Fet 229.371F",
                                             "Entre 229.372F et 305.828F",
                                             ">=305.829F"),16))]


g3 <- ggplot(graphe3,aes(x = `Tranche pension`,y=`Nombre distinct de bénéficiaires`,
                   fill=`Tranche pension`),
       fill=`Nombre distinct de bénéficiaires`, colour=`Tranche pension`) +
  geom_bar(stat = "identity")+
  geom_text(aes(label=scales::percent(Poids, accuracy = 1)),
            position = position_dodge(width=0.8), vjust=0.5, hjust=2,
            color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  theme_ispf()+
  ylab("")+xlab("")+
  theme(legend.position = "none")+
  coord_flip()


graphe4 <- fread("graphe4.txt", encoding = "UTF-8", header = T)
graphe4 <- melt(graphe4,id.vars="Annee")
graphe4[,variable:=stringr::str_wrap(variable, 15)]
graphe4[,Annee:=factor(Annee)]
grapheBar <- graphe4[variable %in% stringr::str_wrap(c("Evasan interiles", "Evasan internationales"),15)]
grapheLines <- graphe4[!variable %in% stringr::str_wrap(c("Evasan interiles", "Evasan internationales"),15)]

g4 <- ggplot(mapping=aes(x=Annee, y=value, fill=variable, colour=variable)) +
  geom_bar(data=grapheBar, stat="identity",width = 0.8, position="stack")+
  #geom_text(data=grapheBar, aes(label=scales::number(value, accuracy = 1)),
  #          position = position_dodge(width=0.8), vjust=0.5,
  #          color="black", size=2)+
  geom_line(data=grapheLines, aes(group=variable, y=value/100000))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+

    scale_y_continuous(name = "Nombre",
                     limits = c(0, 11000),
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~./10, 
                                         name = "Millions de F.CFP",
                                         labels = scales::number_format(accuracy = 1)))+
  theme_ispf()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")


g4


saveGrapheFiles(1)
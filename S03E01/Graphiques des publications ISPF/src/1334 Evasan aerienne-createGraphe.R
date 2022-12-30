library(ggrepel)
library(ggplot2)
library(extrafont)

library(showtext)
font_add_google("Roboto", "Roboto")
loadfonts(device = "win")
windowsFonts("Roboto" = windowsFont("Roboto Light"))
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")
cat("Création des graphiques en PDF\n")


TxtX <- c("RÃ©animation", "GynÃ©cologie-obstÃ©trique", "PÃ©diatrie-nÃ©onatologie", "Neurologie", "Cardiologie", 
          "Hospitalisation transitoire", "Chirurgie orthopÃ©dique", "Chirurgie viscÃ©rale", "Gastro-entÃ©rologie", 
          "Pneumologie", "MÃ©decine interne", "NÃ©phrologie",
          "Hospitalisation temporaire", "ORL", "Psychiatrie", "Ophtalmologie", "Autres chirurgies", "DiabÃ©tologie", 
          "Autres (dÃ©cÃ¨s...)", "Oncologie", "HÃ©matologie", 
          "Absence d'information")

graphe1 <- readCSVFile(1)

ggplot(graphe1) +
  geom_col(aes(x = reorder(Service, -Nb), y = Nb))+
  labs(#title = "Ã‰vasans selon les services d'accueil", 
    x = "Service", y = "Nombre d'Ã‰vasans", 
    caption = "source : Samu de PolynÃ©sie, registres annuels des Ã©vacuations sanitaires par vols spÃ©ciaux 2010-2018") +
  scale_x_discrete(labels = TxtX) +
  theme(axis.text.x = element_text(angle =45, hjust = 1, vjust = 1)) +
  geom_vline(xintercept = c(5.5, 12.5, 21.5), colour = "gray", size = 1.5) +
  geom_text(aes(x = reorder(Service, -Nb), y = Nb, label = Nb, vjust = -0.25), size = 3) +
  annotate("text", x = 3, y = 750, label = "65 %", colour = "gray", fontface = 2, size = 6) +
  annotate("text", x = 10, y = 750, label = "27 %", colour = "gray", fontface = 2, size = 6) +
  annotate("text", x = 17, y = 750, label = "5 %", colour = "gray", fontface = 2, size = 6) +
  annotate("text", x = 22.1, y = 750, label = "3 %", colour = "gray", fontface = 2, size = 6)

ggsave("Fig_01.png", 
       width = 8, height = 5.22, dpi = 450, units = c("in"))

g1



graphe1 <- readCSVFile(1)
graphe1[,Service:=stringr::str_wrap(Service, 30)]
r <- graphe1[,Service]
graphe1[,Service:=factor(Service, levels=rev(c("Absence d'informations","Hématologie","Oncologie","Autres","Diabétologie","Autres chirurgies","Ophtalmologie","Psychiatrie","ORL","Hospitalisation temporaire","Nephrologie","Médecine interne","Pneumologie","Gastro-entérologie","Chirurgie vicérales","Chirurgie orthpédique","Hospitalisation transitoire","Cardiologie","Neurologie","Pédiatrie-néonatologie","Gynécologie-obstétrique","Réanimation")))]

g1 <- ggplot(graphe1) +
  geom_bar(aes(x=Service, y=Nb, fill=ispfPalette[1]), stat="identity", position = position_identity())+
  geom_text(aes(x=Service, y=Nb, label=number(Nb, accuracy = 1, decimal.mark = ",")),  size=2, vjust=-0.5, hjust=0.5)+
  scale_y_continuous(labels = number_format(accuracy = 1), limits=c(0,1500))+
  annotate(geom="text", x="Pédiatrie-néonatologie", y=800, label="65%", color="black", size=4)+
  annotate(geom="text", x="Gastro-entérologie", y=800, label="27%", color="black", size=4)+
  annotate(geom="text", x="Autres chirurgies", y=800, label="5%", color="black", size=4)+
  annotate(geom="text", x="Absence d'informations", y=800, label="3%", color="black", size=4)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "horizontal")+
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust=1))
#coord_flip()
g1


#######graphe1 <- readCSVFile(1)
#graphe1 <- melt(graphe1,id.vars="SERVICE_2022")
graphe1$SERVICE <- as.character(graphe1$variable)
graphe1$Nb <- as.integer(graphe1$value)
graphe1$SERVICE_2022 <- graphe1$`SERVICE`


###############################
#g1 <- ggplot(graphe1) + 
  aes(x="SERVICE", y=Nb, fill=SERVICE) +
  geom_bar(aes(x=SERVICE, y=Nb, fill=variable), stat="identity", position = position_identity(), fill=ispfPalette[1])+
  geom_text(data=graphe1,aes(y="Nb", label=scales::number(Nb, accuracy = 1)), vjust=-1,  size=2)+
  scale_x_continuous(limits = c(0, 10000),labels = scales::number_format(accuracy = 1))+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
  plot(x="SERVICE", y="Nb", las=2)

g1



graphe2 <- readCSVFile(2)
#r <- graphe2[,`Archipels`]
#graphe2[,`Archipels`:=factor(`Archipels`, levels=r, ordered=T)]
graphe2 <- melt(graphe2,id.vars="Archipels")
graphe2$Archipels <- graphe2$`Archipels`
graphe2$Annee <- as.integer(as.character(graphe2$variable))



g2 <-  ggplot(graphe2) +
  aes(x = Archipels,  fill = Archipels, colour = Archipels, weight = value, label=value) +
  geom_bar(position="dodge") +
  facet_wrap(vars(Archipels))+
  geom_text(data=graphe2,
            aes(y=value,label=scales::number(value, accuracy = 0.1, decimal.mark = ",")), 
            vjust=-0.5, size=2, colour="black")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,10))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "none",legend.direction = "horizontal")+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())
  #theme(legend.text=element_text(size=rel(0.7)))

g2


graphe2 <- readCSVFile(2)
r <- graphe2[,`Annee`]
#graphe2[,`Archipels`:=factor(`Archipels`, levels=r, ordered=T)]
graphe2 <- melt(graphe2,id.vars="Annee")
graphe2$Annee <- graphe2$`Annee`
graphe2$Archipels <- as.integer(as.character(graphe2$variable))



g2 <-  ggplot(graphe2) +
  aes(x = Annee,  fill = Annee, colour = Annee, weight = value, label=value) +
  geom_bar() +
  facet_wrap(vars(Annee))+
  geom_text(data=graphe2,
            aes(y=value,label=scales::percent(value, accuracy = 0.1, decimal.mark = ",")), 
            vjust=-0.5, size=2, colour="black")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits = c(0,10))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "none",legend.direction = "horizontal")+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())
#theme(legend.text=element_text(size=rel(0.7)))

g2

graphe3 <- readCSVFile(3)
input <- graphe3[, c('Pop_Mean', 'Evasan_Mean')]
#graphe3[,Poste:=stringr::str_wrap(Poste, 35)]
#r <- graphe3[,Poste]
#graphe3[,Poste:=factor(Poste, levels=r, ordered=T)]

g3 <- ggplot(graphe3) +
  geom_point(aes(x=Pop_Mean, y=Evasan_Mean), stat="identity", position = position_identity(), fill=ispfPalette[1])+
  scale_x_continuous(labels = number_format(accuracy = 1), limits=c(0,10000),)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "none")+

g3



graphe4 <- readCSVFile(4)
colnames(graphe4)[[1]] <- "Categorie"
graphe4[,Categorie:=stringr::str_wrap(Categorie, 35)]
r <- graphe4[,Categorie]
graphe4[,Categorie:=factor(Categorie, levels=r, ordered=T)]

g4 <- ggplot(graphe4) +
  geom_bar(aes(x=Categorie, y=valeur, fill=ispfPalette[1]), stat="identity", position = position_identity())+
  geom_text(aes(x=Categorie, y=valeur, label=percent(evo, accuracy = .1, decimal.mark = ",")), size=2, hjust=-0.5)+
  
  scale_y_continuous(labels = number_format(accuracy = 1), limits=c(0,17))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "none")+
  coord_flip()
g4



graphe5 <- readCSVFile(5)
graphe5[, Année:=as.Date(paste0(Année,'-01-01'))]
graphe5 <- melt(graphe5,id.vars="Année")
graphe5[,variable:=stringr::str_wrap(variable,6)]

g5 <- ggplot(graphe5, aes(x = Année,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=ispfPalette)+
  #scale_x_date(date_labels="%Y", breaks="2 year", limits = c(as.Date("2005-01-01"), as.Date("2018-01-01")))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,120))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g5


graphe6 <- readCSVFile(6)
colnames(graphe6)[2:3] <- c(2017,2018)
graphe6 <- melt(graphe6,id.vars="Date")
graphe6[,Date:=stringr::str_wrap(Date,6)]
g6 <- ggplot(graphe6, aes(x = Date,y=value, group=variable)) +
  geom_bar(aes(fill = variable), stat="identity", position="dodge", width=0.8)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g6


graphe7 <- readCSVFile(7)
colnames(graphe7)[2:3] <- c(2017,2018)
graphe7 <- melt(graphe7,id.vars="Date")
graphe7[,Date:=stringr::str_wrap(Date,6)]
g7 <- ggplot(graphe7, aes(x = Date,y=value, group=variable)) +
  geom_bar(aes(fill = variable), stat="identity", position="dodge", width=0.8)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
  #theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())
  
g7


graphe8 <- readCSVFile(8)
colnames(graphe8)[3] <- "2017"
graphe8 <- melt(graphe8,id.vars="Date")
graphe8[,Date:=stringr::str_wrap(Date,6)]

g8 <- ggplot(graphe8) +
  aes(x = Date, y = value, group = variable, color=variable) +
  geom_bar(mapping = aes(x=Date, y=value, fill=variable), stat = "identity", position="dodge" , width=0.8)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g8

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 9)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 5)

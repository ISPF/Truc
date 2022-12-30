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

graphe1 <- readCSVFile(1)
colnames(graphe1)[14] <- 2018
graphe1 <- melt(graphe1,id.vars="Approche Dépense")
graphe1$ApprocheDepense <- graphe1$`Approche Dépense`
graphe1$Annee <- as.character(graphe1$variable)


g1 <- 
  ggplot(graphe1, aes(x=Annee, y=value, fill=ApprocheDepense, colour=ApprocheDepense)) +
  geom_line(aes(group=ApprocheDepense))+
  geom_point(aes(group=ApprocheDepense))+
  geom_text(data=graphe1[ApprocheDepense=="Evolution PIB valeur"],aes(label=scales::percent(value, accuracy = 0.1)), vjust=-1,  size=2)+
  geom_text(data=graphe1[ApprocheDepense=="Evolution PIB en volume"],aes(label=scales::percent(value, accuracy = 0.1)), vjust=2, size=2)+
  scale_y_continuous(limits = c(-0.05, 0.05),labels = scales::percent_format(accuracy = 0.1))+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")

g1



graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Approche Dépense")
graphe2$ApprocheDepense <- stringr::str_wrap(graphe2$`Approche Dépense`,20)
graphe2$Annee <- as.integer(as.character(graphe2$variable))


g2 <-  ggplot(graphe2) +
  aes(x = ApprocheDepense,  fill = ApprocheDepense, colour = ApprocheDepense, weight = value, label=value) +
  geom_bar() +
  facet_wrap(vars(Annee))+
  geom_text(data=graphe2,
            aes(y=value,label=scales::percent(value, accuracy = 0.1, decimal.mark = ",")), 
            vjust=-0.5, size=2, colour="black")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), limits = c(-0.1,0.1))+
  theme_ispf()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  theme(legend.text=element_text(size=rel(0.7)))

g2

graphe3 <- readCSVFile(3)
graphe3[,Poste:=stringr::str_wrap(Poste, 35)]
r <- graphe3[,Poste]
graphe3[,Poste:=factor(Poste, levels=r, ordered=T)]

g3 <- ggplot(graphe3) +
  geom_bar(aes(x=Poste, y=valeur, fill=variable), stat="identity", position = position_identity(), fill=ispfPalette[1])+
  geom_text(aes(x=Poste, y=valeur, label=percent(evolution2017, accuracy = .1, decimal.mark = ",")),  size=2, vjust=0.1, hjust=-0.1)+
  scale_y_continuous(labels = number_format(accuracy = 1), limits=c(0,100))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "none")+
  coord_flip()
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
graphe5 <- fread("graphe5.csv", encoding = "UTF-8")
graphe5 <- melt(graphe5,id.vars="Année")
graphe5[,variable:=stringr::str_wrap(variable,6)]

g5 <- ggplot(graphe5, aes(x = Année,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=ispfPalette)+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = seq(2005,2018,1))+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.ticks.x=element_blank(), axis.text.x=element_blank())+
  theme(legend.text=element_text(size=rel(0.7)))
g5


#colnames(graphe6)[1] <- "Annee"
#graphe6$Annee <- graphe6$Date
#graphe6 <- fread("graphe6.csv", encoding = "UTF-8")


graphe6 <- readCSVFile(6)
graphe6 <- melt(graphe6,id.vars="Date")
graphe6[,variable:=stringr::str_wrap(variable,6)]
#graphe6[, Date:=as.Date(paste0(Date,'-01-01'))]

g6 <- ggplot(graphe6, aes(x=variable, y=value, fill = variable)) +
  geom_bar(stat="identity", position="dodge" )+
  facet_wrap(vars(Date))+
  scale_fill_manual(values=ispfPalette)+
  #scale_x_continuous()+
  #scale_x_date(date_labels="%Y", breaks="1 year")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,3))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "none",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
g6


graphe7 <- readCSVFile(7)
#colnames(graphe7)[2:3] <- c(2017,2018)
#graphe7 <- fread("graphe7.csv", encoding = "UTF-8")
graphe7 <- melt(graphe7,id.vars="Date")
graphe7[,Date:=stringr::str_wrap(Date,6)]

#graphe7[, Date:=as.Date(paste0(Date,'-01-01'))]

g7 <- ggplot(graphe7, aes(x=variable, y=value, fill = variable)) +
  geom_bar(stat="identity", position="dodge" )+
  facet_wrap(vars(Date))+
  scale_fill_manual(values=ispfPalette)+
  #scale_x_date(date_labels="%Y", breaks="1 year")+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "none",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1))+
  theme(legend.text=element_text(size=rel(0.7)))
g7


graphe8 <- readCSVFile(8)
#colnames(graphe8)[3] <- "2017"
#graphe8 <- fread("graphe8.csv", encoding = "UTF-8")
graphe8 <- melt(graphe8,id.vars="Date")
graphe8[,Année:=stringr::str_wrap(Date,6)]
graphe8[, Date:=as.Date(paste0(Date,'-01-01'))]

g8 <- ggplot(graphe8, aes(x=variable, y=value, fill = variable)) +
  geom_bar(stat="identity", position="dodge" )+
  facet_wrap(vars(Date))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))+
  theme(legend.text=element_text(size=rel(0.7)))
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

g8

#graphe8 <- readCSVFile(8)

#g8 <- createGenericGraphe(8, "Catégorie", str_wrap_cle = 30, str_wrap_variable = 30,
                          #legendPosition = "bottom", legendDirection = "horizontal",
                          #ylabelFunction = number_format(accuracy = 1))+
  #geom_bar(aes(x = Catégorie,y=value, fill=variable, colour=variable), stat="identity",
           #position = position_dodge())+
  #scale_y_continuous(limits=c(0,30))+
  #coord_flip()

#g8


saveGrapheFiles(1, largeurCM = 18, hauteurCM = 6.5)
saveGrapheFiles(2, largeurCM = 18, hauteurCM = 6.5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 9)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(8, largeurCM = 9, hauteurCM = 5)

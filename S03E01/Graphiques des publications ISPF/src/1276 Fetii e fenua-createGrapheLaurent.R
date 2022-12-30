cat("Création des graphiques en PDF\n")
library(ggrepel)
graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Archipel")
graphe1[,variable:=stringr::str_wrap(variable,20)]

g1 <- ggplot(graphe1,aes(x = Archipel,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 0.1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")
g1

graphe3 <- readCSVFile(3)
graphe3$Serie <- stringr::str_wrap(graphe3$Serie,25)
#graphe3 <- graphe3[order(graphe7$Repartition, decreasing = T)]

g3 <-  ggplot(graphe3, aes(x = "", y=Repartition, fill = Serie, group = Serie, weight = Repartition)) +
  geom_bar(stat = "identity")+
  coord_polar("y", start=0) +
  geom_label_repel(aes(label=paste0(Serie,"\n",scales::percent(Repartition, accuracy = 0.1, decimal.mark = ","))), size=2,  color="white",
            position = position_stack(0.5), size=2)+
  
  theme_void()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(text = element_text(size = 8, family = "Roboto Light"),
        legend.title = element_blank())+
  theme(legend.position = "none")

g3

graphe4 <- readCSVFile(4)
graphe4[,Lieu:=forcats::fct_reorder(Lieu, PCT_PF)]

g4 <- ggplot(graphe4,aes(x = Lieu,y=PCT_PF, fill=Lieu)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(PCT_PF, accuracy = 0.1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  coord_flip()
g4


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 8)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 6)

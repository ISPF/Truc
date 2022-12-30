cat("Création des graphiques en PDF\n")



graphe2 <- readCSVFile(2)
graphe2[Date=="Allocations familiales, bourses État ou Pays, etc.", Date:="Allocations (familiales, bourses État ou Pays, etc.)"]
#colnames(graphe4)[2] <- "2021"
graphe2[,Date:=stringr::str_wrap(Date, 35)]
graphe2 <- melt(graphe2,id.vars="Date")
graphe2[,Date:=factor(Date,
                      levels=c("Aucune","Autres sources","Emprunts","Salaire","Economies","Allocations (familiales, bourses\nÉtat ou Pays, etc.)" ,"Aide des parents"))]



g2 <- ggplot(graphe2,aes(x = Date,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "horizontal")+
  guides(col = guide_legend(ncol = 2))+
  coord_flip()
g2


graphe3 <- readCSVFile(3)
#colnames(graphe4)[2] <- "2021"
graphe3 <- melt(graphe3,id.vars="Date")
graphe3[,Date:=factor(Date, levels=rev(c("Plus de 70 000 F.CFP","Entre 50 000 et 70 000 F.CFP","Entre 30 000 et 50 000 F.CFP","Entre 10 000 et 30 000 F.CFP","Moins de 10 000 F.CFP")))]

g3 <- ggplot(graphe3,aes(x = Date,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  #geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "horizontal")+
  coord_flip()
g3

graphe4 <- readCSVFile(4)
graphe4[,Date:=stringr::str_wrap(Date, 25)]
graphe4 <- melt(graphe4,id.vars="Date")

g4 <- ggplot(graphe4,aes(x = Date,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Date,y=value, fill=variable), stat="identity")+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=rev(ispfPalette[1:5]))+
  scale_colour_manual(values=rev(ispfPalette[1:5]))+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  theme(legend.text=element_text(size=rel(0.7)))+
  guides(fill = guide_legend(nrow = 3))+
  coord_flip()
g4

#graphe4 <- readCSVFile(4)
#graphe4[,Date:=stringr::str_wrap(Date, 25)]
#graphe4 <- melt(graphe4,id.vars="row.names")

#g4 <- 
  max_min <- data.frame(
    "Moins de 10.000 XPF" = c(0, 100), "Entre 10.000 et 30.000 XPF" = c(20, 0), "Entre 30.000 et 50.000 XPF" = c(20, 0),
    "Entre 50.000 et 70.000 XPF" = c(20, 0), "Plus de 70.000 XPF" 
  )
#rownames(max_min) <- c("Max", "Min")
#df <- rbind(max_min, exam_scores)
#df


  #data<-cbind("Moins de 10.000 XPF","Entre 10.000 et 30.000 XPF","Entre 30.000 et 50.000 XPF","Entre 50.000 et 70.000 XPF","Plus de 70.000 XPF")
  #rownames(data) <- c("MAX","MIN","Chez des amis ou de la famille","Chez vos parents","Dans un appartement seul ou en couple","Dans une résidence étudiante")
  #View(data)+
  #colnames(data)<-labels+ 
  #datx = as.data.frame(data) ; datx+  
  
  #install.packages("fmsb") # permet d'installer le package
  #library(fmsb)
  
g4
  
  ggplot(graphe4,aes(x = Date,y=value, color=variable)) +
  geom_bar(mapping=aes(x = Date,y=value, fill=variable), stat="identity")+
  #scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''), breaks = c(1983,1988,1996,2002,2007,2012,2017))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=rev(ispfPalette[1:5]))+
  scale_colour_manual(values=rev(ispfPalette[1:5]))+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom")+
  theme(legend.text=element_text(size=rel(0.7)))+
  guides(fill = guide_legend(nrow = 3))+
  coord_flip()
g4

library(fmsb)
library(plotly)
graphe4 <- readCSVFile(4)
graphe4[,row.names:=stringr::str_wrap(row.name, 25)]
# graphe4 <- graphe4[!row.names %in% c("MAX","MIN")]
# graphe4 <- melt(graphe4,id.vars = "row.names")
# graphe4 <- graphe4[!row.names %in% c("MAX","MIN")]

graphe4 <- data.frame(graphe4[,-1],row.names=graphe4$row.names)
graphe4 <- graphe4[c(2,1,3:6),]
radarchart(graphe4)

areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))

radarchart(graphe4,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687"),      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = )   # Color of the areas

legend("topright",
       legend = rownames(graphe4)[-1:-2],
       bty = "n", pch = 20, col = c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687"),
       text.col = "grey25", pt.cex = 2)
   


graphe5 <- readCSVFile(5)
#colnames(graphe4)[2] <- "2021"
graphe5 <- melt(graphe5,id.vars="Date")


g5 <- ggplot(graphe5,aes(x = Date,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  #geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "horizontal")+
  guides(fill = guide_legend(nrow = 2))+
  coord_flip()
g5


graphe6 <- readCSVFile(6)
graphe6[,Date:=stringr::str_wrap(Date, 35)]
#colnames(graphe4)[2] <- "2021"
graphe6 <- melt(graphe6,id.vars="Date")

g6 <- ggplot(graphe6,aes(x = Date,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  #geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "horizontal")+
  guides(fill = guide_legend(nrow = 2))+
  coord_flip()
g6


graphe7 <- readCSVFile(7)
graphe7[,Date:=stringr::str_wrap(Date, 35)]
#colnames(graphe4)[2] <- "2021"
graphe7 <- melt(graphe7,id.vars="Date")

g7 <- ggplot(graphe7,aes(x = Date,y=value, fill=variable)) +
  geom_bar(stat="identity", width=.8, position = "dodge")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  #geom_text(aes(label=scales::number(value, accuracy = 1)),  position = position_dodge(width=0.8),color="black", size=2)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "horizontal")+
  guides(fill = guide_legend(nrow = 2))+
  coord_flip()
g7



saveGrapheFiles(2, largeurCM = 9, hauteurCM = 5.5)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6.5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 6.5)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 6.5)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 6.5)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 7.7)

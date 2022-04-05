setwd("~/R/Truc/S02E02/ggplot2")
source("functions.R")

graphe1 <- readCSVFile(1)
graphe1 <- melt(graphe1,id.vars="Région")

g1 <- ggplot(graphe1, aes(x = Région,y=value, group=variable,fill = variable, )) +
  geom_bar(stat="identity") +
  geom_text(data=graphe1,
            aes(x = Région,y=value, label=percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.1, size=5))+
  theme(legend.text=element_text(size=rel(0.7)))
g1


graphe2 <- readCSVFile(2)
graphe2 <- melt(graphe2,id.vars="Région")

g2 <- ggplot(graphe2, aes(x = variable,y=value, group=Région,fill = Région)) +
  geom_bar( stat="identity", position ="dodge")+
  #geom_label_repel(aes(label=number(value, accuracy = 1)),  position = position_stack(vjust = 0,reverse = T),color="white", size=3)+
  geom_text(data=graphe2, aes(label=number(value, accuracy = 1)),  position = position_dodge(width = 0.8),color="black", size=2, hjust = 0.3, vjust = -0.3)+scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,80000))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 0, vjust = -1, hjust=0.5, size=5))+
  theme(legend.text=element_text(size=rel(0.7)))
g2



graphe3 <- readCSVFile(3)
graphe3 <- melt(graphe3,id.vars="Région")
graphe3 <- readCSVFileAndMelt(3, keysList = "Région", isVariableOrdered = T)

g3 <- ggplot(graphe3, aes(x =Région,y=value, group=variable, colour = variable)) +
  geom_line(aes(x =Région,y=value, group=variable, colour = variable))+
  geom_point(aes(group=Région))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1), limits=c(0,160))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=5))+
  theme(legend.text=element_text(size=rel(0.5)))
g3


graphe4 <- readCSVFile(4)
graphe4 <- melt(graphe4,id.vars="Ile")

g4 <- ggplot(graphe4, aes(x =Ile,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(data=graphe4,
            aes(x = Ile,y=value, label=percent(value, accuracy = 1)),  position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(0,0.5))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g4


graphe5 <- readCSVFile(5)
graphe5 <- melt(graphe5,id.vars="Région")

g5 <- ggplot(graphe5, aes(x =Région,y=value, group=variable,fill = variable)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(data=graphe5,
            aes(x = Région,y=value, label=percent(value, accuracy = 1)),  position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits=c(-1,0.1))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")+
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=5))+
  theme(legend.text=element_text(size=rel(0.7)))
g5


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 5)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 7)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 7)

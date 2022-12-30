g1 <- createGenericGraphe(1, "Approche Dépense", 
                          legendPosition = "top", legendDirection = "horizontal",
                          ylabelFunction = percent_format(accuracy = 0.1), str_wrap_cle = 30)+
  geom_line(aes(x = variable,y=value, group=`Approche Dépense`, colour=`Approche Dépense`), stat="identity")+
  geom_text_repel(aes(x = variable,y=value, colour=`Approche Dépense`, label=percent(value, accuracy = .1, decimal.mark = ",")),  vjust = -0.5, size=2)+
  scale_y_continuous(labels = percent_format(accuracy = 0.1), limits=c(-0.05,0.05))+
  ylab("")+  xlab("")
#g1


graphe2 <- readCSVFile(2)
graphe2[,Poste:=stringr::str_wrap(Poste, 35)]
r <- graphe2[,Poste]
graphe2[,Poste:=factor(Poste, levels=r, ordered=T)]

g2 <- ggplot(graphe2) +
  geom_bar(aes(x=Poste, y=valeur, fill=variable), stat="identity", position = position_identity(), fill=ispfPalette[1])+
  geom_text(aes(x=Poste, y=valeur, label=percent(evolution2016, accuracy = .1, decimal.mark = ",")),  size=2, vjust=0.5, hjust=-0.5)+
  scale_y_continuous(labels = number_format(accuracy = 1), limits=c(0,95))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "none")+
  coord_flip()
#g2



g3 <- createGenericGraphe(3, "Categorie", originalOrder = T, ylabelFunction = number_format(accuracy = 1), axis.text.x.angle = 90,str_wrap_cle = 15)+
   geom_bar(aes(x=Categorie, y=value, fill=variable), stat="identity", position = position_identity(), width = 0.6)+
  geom_text(aes(x=Categorie, y=value, label=number(value, accuracy = .1, decimal.mark = ",")), vjust=-0.5,  size=2)+
  scale_y_continuous(labels = number_format(accuracy = 1), limits=c(0,45))
#g3



# g6 <- createGenericGraphe(6, "Categorie", legendPosition = "none", axis.text.x.angle = 90, ylabelFunction = number_format(accuracy = 1), str_wrap_cle = 12)+
#   geom_bar(mapping=aes(x = Categorie, y=value, fill=variable), stat="identity", position = "dodge")+
#   geom_text(aes(x = Categorie,y=value, fill=variable, label=scales::number(value, accuracy = .1)), 
#             position = position_dodge(width = 0.8),color="black", size=2, vjust = -0.5)
# g6


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
#g4

g5 <- createGenericGraphe(5, "Categorie", 
                          legendPosition = "bottom", legendDirection = "horizontal",
                          ylabelFunction = number_format(accuracy = 1))+
  geom_bar(aes(x = Categorie,y=value, fill=variable, colour=variable), stat="identity",
           position = position_dodge())+
  coord_flip()

#g5


# g8 <- createGenericGraphe(8, "Categorie", 
#                           legendPosition = "bottom", legendDirection = "horizontal",axis.text.x.angle = 90,
#                           ylabelFunction = number_format(accuracy = 1))+
#   geom_line(aes(x = variable,y=value, group=Categorie, colour=Categorie), stat="identity",
#             position = position_stack())
#   
# g8

graphe6 <- readCSVFile(6)
graphe6[,Periode:=as.Date(Periode)]

g6 <- ggplot(graphe6) +
  geom_line(aes(x = Periode,y=Investissement, group=1), color=ispfPalette[1], stat="identity")+
  scale_x_date(breaks=seq(min(graphe6$Periode), max(graphe6$Periode), by="1 year"), 
               labels=date_format("%Y"))+
  scale_y_continuous(labels = percent_format(accuracy = 1))+
  scale_fill_manual(values=ispfPalette)+
  theme_ispf()+
  ylab("")+  xlab("")+
  theme(legend.position = "none", legend.direction = "none")
  
#g6


# g6 <- createGenericGraphe(6, "Periode", axis.text.x.angle = 90,
#                           ylabelFunction = percent_format(accuracy = 1))+
#   geom_line(aes(x = Periode,y=value, group=variable, colour=variable), stat="identity")+
#   scale_x_date()
# 
# g6

g7 <- createGenericGraphe(7, "Catégorie", str_wrap_cle = 30, str_wrap_variable = 30,
                          legendPosition = "bottom", legendDirection = "horizontal",
                          ylabelFunction = number_format(accuracy = 1))+
  geom_bar(aes(x = Catégorie,y=value, fill=variable, colour=variable), stat="identity",
           position = position_dodge())+
  scale_y_continuous(limits=c(0,30))+
  coord_flip()

#g7


# graphe14 <- readCSVFile(14)
# graphe14[,Periode:=as.Date(Periode)]
# graphe14 <- melt(graphe14, id.vars = "Periode")
# 
# 
# g14 <- ggplot(graphe14) +
#   geom_line(aes(x = Periode,y=value, group=variable, color=variable), stat="identity")+
#   scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0,0.05))+
#    scale_x_date(breaks=seq(min(graphe14$Periode), max(graphe14$Periode), by="1 year"), 
#                 labels=date_format("%Y"))+
#   # scale_y_continuous(labels = percent_format(accuracy = 1))+
#   scale_fill_manual(values=ispfPalette)+
#   theme_ispf()+
#   ylab("")+  xlab("")+
#   theme(legend.position = "bottom", legend.direction = "horizontal")
# 
# g14


saveGrapheFiles(1, largeurCM = 9, hauteurCM = 4.9)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 9)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(4, largeurCM = 9, hauteurCM = 9)
saveGrapheFiles(5, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(6, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(7, largeurCM = 9, hauteurCM = 5)
#saveGrapheFiles(14, largeurCM = 9, hauteurCM = 5)

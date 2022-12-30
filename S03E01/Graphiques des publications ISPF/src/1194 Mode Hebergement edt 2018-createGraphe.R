library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8", header = T)
graphe1 <- melt(graphe1,id.vars="Annee")
graphe1$Annee <- as.factor(graphe1$Annee)

graphe1_data1 <- graphe1[variable %in% c("Dépenses tourisme terrestre","Dépenses tourisme croisière")]
graphe1_data2 <- graphe1[variable %in% c("Nombre de touristes terrestres","Nombre de touristes croisières")] 

graphe1_data1$variable <- stringr::str_wrap(graphe1_data1$variable,15)
graphe1_data2$variable <- stringr::str_wrap(graphe1_data2$variable,15)

g1 <- ggplot(graphe1, aes(x=Annee, y=value, fill=variable, colour=variable)) +
  geom_col( data=graphe1_data1, width = 0.5)+
  geom_line(data=graphe1_data2,aes(x=Annee, y=value/2.85, group=variable))+
  geom_point(data=graphe1_data2, aes(x=Annee, y=value/2.85))+
  scale_y_continuous(name = "Dépenses touristiques\n(millions de F.CFP)",
                     limits = c(0, 70000),
                     labels = scales::number_format(accuracy = 1),
                     sec.axis = sec_axis(~.*2.85,
                                         name = "Nombre de touristes", 
                                         labels = scales::number_format(accuracy = 1)))+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
            
g1


graphe2 <- fread("graphe2.txt", encoding = "UTF-8", header = T)
graphe2 <- melt(graphe2,id.vars="Hebergement")
graphe2 <- graphe2[value!=0,]
graphe2$Hebergement <- as.factor(graphe2$Hebergement)
graphe2$Hebergement <- factor(graphe2$Hebergement,levels(graphe2$Hebergement)[c(2,1,3,4)])
graphe2$variable <- stringr::str_wrap(graphe2$variable,10)


g2 <- ggplot(graphe2,aes(x = variable,y=value, fill=Hebergement, colour=Hebergement)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g2

graphe3 <- fread("graphe3.txt", encoding = "UTF-8", header = T)
graphe3 <- melt(graphe3,id.vars="Hebergement")
#graphe3 <- graphe3[value!=0,]
graphe3$Hebergement <- stringr::str_wrap(graphe3$Hebergement,15)
graphe3$Hebergement <- as.factor(graphe3$Hebergement)
graphe3$variable <- stringr::str_wrap(graphe3$variable,10)


g3 <- ggplot(graphe3,aes(x = variable,y=value, fill=Hebergement, colour=Hebergement)) +
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=scales::percent(value)), position=position_dodge(width=0.9), vjust=-0.25, color="black", size=2)+
  #geom_text(aes(label=scales::percent(value)),  position = position_dodge(vjust = .5),color="black", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g3

graphe4 <- fread("graphe4.txt", encoding = "UTF-8", header = T)
graphe4 <- melt(graphe4,id.vars="Hebergement")
graphe4 <- graphe4[value!=0,]
graphe4$Hebergement <- stringr::str_wrap(graphe4$Hebergement,10)
graphe4$Hebergement <- as.factor(graphe4$Hebergement)
graphe4$Hebergement <- factor(graphe4$Hebergement,levels(graphe4$Hebergement)[c(2,1,5,4,3)])
graphe4$variable <- stringr::str_wrap(graphe4$variable,10)

g4 <- ggplot(graphe4,aes(x = variable,y=value, fill=Hebergement, colour=Hebergement)) +
  geom_bar( stat="identity")+
  geom_text(aes(label=scales::percent(value, accuracy = 1)),  position = position_stack(vjust = .5),color="white", size=2)+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  theme_ispf()+
  theme(legend.key.height = unit(1, "cm"))+
  ylab("")+  xlab("")+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g4


graphe5 <- fread("graphe5.txt", encoding = "UTF-8", header = T)
graphe5$Hebergement <- stringr::str_wrap(graphe5$Hebergement,15)
  
g5 <-  ggplot(graphe5, aes(x = "", y=value, fill = Hebergement, group = Hebergement)) +
  geom_bar(stat = "identity")+
  coord_polar("y", start=0) +
  geom_text(aes(label=paste0(scales::number(value, accuracy = 1),
                             '\n',
                             scales::percent(value/sum(graphe5$value), accuracy = 1))),
                color="white",position = position_stack(0.5), size=2)+
  theme_void()+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+ylab("")+
  theme(text = element_text(size = 8, family = "Roboto Light"),
        legend.title = element_blank())
g5  

suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe5.pdf", g5, width = 9, height=7, units = "cm", device=cairo_pdf))

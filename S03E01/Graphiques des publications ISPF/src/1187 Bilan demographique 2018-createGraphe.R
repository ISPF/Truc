
library(ggrepel)
windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()

cat("Création des graphiques en PDF\n")

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe4 <- fread("graphe4.txt", encoding = "UTF-8", stringsAsFactors = T)
graphe2$Age <- factor(graphe2$Age)
graphe2$Age <- relevel(graphe2$Age, "Moins de 5 ans")
graphe3$variable <- stringr::str_wrap(graphe3$variable,3)
graphe3$variable <- factor(graphe3$variable)
graphe3$variable <- factor(graphe3$variable,levels(graphe3$variable)[c(5,4,3,2,1,6)])


data.frame(stringsAsFactors=FALSE,
        Type = c("Naissances en vie", "Décès", "Solde naturel"),
        varX = c(2005, 2005, 2000),
        varY = c(5000, 1000, 3000)
)



ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")
g1 <- ggplot(graphe1,  aes(x = Annee, y = Nombre,  fill=Type, colour=Type)) +
  geom_line()+
  #geom_area( size = 0.5,position='identity', alpha=.1) +
  geom_area(data=graphe1[Type!="Décès"],size = 0.5,position='identity') +
  geom_area(data=graphe1[Type=="Décès"],size = 0.5,position='identity') +
  geom_label(data=dfLegendGraphe1,
             aes(x = varX, y=varY, label=Type, family="Roboto Light"), 
             size=3, colour=c("white", "white", "black"),
             fill=custom.col)+
  geom_label_repel(data=graphe1[Annee==2018],
                   aes(Annee, Nombre, label = format(Nombre, nsmall=0, big.mark=" ")),
                   colour="white", size=3, show.legend = F)+
  #scale_colour_brewer(palette = "Set2")+
  #scale_fill_brewer(palette = "Set2")+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(limits = c(0,6000))+  ylab("")+  xlab("")+
  theme_ispf()+  theme(legend.position = "none")


g2 <- ggplot(graphe2,aes(x = Age,y=effectif)) +
  geom_bar(data = graphe2[Annee == 2018], stat = "identity", aes(fill = factor(Sexe)),alpha=0.8, width=0.8) +
  geom_line(data = graphe2[Annee =="1998" & Sexe=="Homme"], stat = "identity",aes(linetype=factor(Annee), group=1),size=0.5) +
  geom_line(data = graphe2[Annee =="1998" & Sexe=="Femme"], aes(linetype=factor(Annee), group=1), stat = "identity",size=0.5) +
  coord_flip() + 
  xlab("") +  ylab("") + theme_ispf()+
  scale_y_discrete(limits=c(-10000,-5000,0,5000,10000), labels=c(10000,5000,0,5000, 10000))+
  #scale_colour_brewer(palette = "Set2")+
  #scale_fill_brewer(palette = "Set2")+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  theme_ispf()+
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

g3 <-  ggplot(graphe3) +
  aes(x = Année, y = value, colour = variable, fill=variable) +
  geom_area(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+  ylab("")+  xlab("")+  theme_ispf()

g4 <- ggplot(graphe4) +
  aes(x = Années, y = Valeur, group = Taux, color=Taux) +
  geom_line(size = 0.5) +
  geom_label(data=graphe4 %>% group_by(Taux) %>% arrange(desc(Années)) %>% slice(1),
             aes(x = "1994 - 1998", label=Taux, family="Roboto Light"), size=3)+
  scale_x_discrete(breaks = levels(graphe4$Années)[c(1,5,10,15,20,25,30,31)])+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")+
  geom_label_repel(data=graphe4[Années=="2014 - 2018"],
                   aes(Années, Valeur, label = format(Valeur, nsmall=0, big.mark=" ", decimal.mark = ",")),
                   size=3,
                   show.legend = FALSE)


suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=7, units = "cm", device=cairo_pdf))



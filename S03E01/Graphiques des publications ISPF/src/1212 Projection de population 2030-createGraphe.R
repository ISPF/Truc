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

graphe1 <- fread("graphe1.txt", encoding = "UTF-8")
graphe1 <- melt(graphe1,id.vars="Année")
graphe1[,variable:=stringr::str_wrap(variable,10)]
g1 <- ggplot(graphe1[Année>=2000]) +
  aes(x = Année, y = value, group = variable, color=variable) +
  geom_line(size = 0.5) +
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits=c(230000,300000))+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g1
  

graphe2 <- fread("graphe2.txt", encoding = "UTF-8")
graphe2[Age=="90 ANS ET PLUS", Age:="90"]
graphe2[, Age:=as.integer(Age)]
graphe2$Age <- factor(graphe2$Age)
graphe2[Age=="90", Age:="90 ans et +"]
graphe2$...6 <- NULL

graphe2 <- melt(graphe2,id.vars="Age")
graphe2[,Année:=2018]
graphe2[,Sexe:="Femme"]
graphe2[grepl("Homme", graphe2$variable, ignore.case = T), Sexe:="Homme"]
graphe2[grepl("2030", graphe2$variable, ignore.case = T), Année:=2030]
graphe2$Sexe <- factor(graphe2$Sexe, levels=c("Homme", "Femme"))
#graphe2$Sexe <- relevel(graphe2$Sexe, "Homme")


g2 <- ggplot(graphe2,aes(x = factor(Age),y=value)) +
  geom_bar(data = graphe2[Année == 2030], stat = "identity", aes(fill=Sexe),alpha=0.8, width=0.8) +
  geom_line(data = graphe2[Année =="2018" & Sexe=="Homme"], stat = "identity",aes(linetype=factor(Année), group=1),size=0.3, color=ispfPalette[2]) +
  geom_line(data = graphe2[Année =="2018" & Sexe=="Femme"], stat = "identity",aes(linetype=factor(Année), group=1),size=0.3, color=ispfPalette[2]) +
  coord_flip() + 
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_x_discrete(breaks=c("0","25","50","75", "90 ans et +"))+
#  scale_y_continuous(limits=c(-2000,-1000,0,1000,2000), labels=c(2000,1000,0,1000,2000))+
  xlab("") +  ylab("") + theme_ispf()+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  theme(plot.margin=grid::unit(c(2,0,0,0,0), "mm"))
g2

  



graphe3 <- fread("graphe3.txt", encoding = "UTF-8")
graphe3$...8 <- NULL
graphe3[Age=="90 ANS ET PLUS", Age:="90"]
graphe3[, Age:=factor(as.integer(Age))]
graphe3[Age=="90", Age:="90 ans et +"]
graphe3 <- melt(graphe3,id.vars="Age")

unique(graphe3$variable)

graphe3[grepl("Hypothèse", variable, ignore.case = T), Hypothese:="Centrale"]
graphe3[grepl("Haute", variable, ignore.case = T), Hypothese:="Haute"]
graphe3[grepl("Basse", variable, ignore.case = T), Hypothese:="Basse"]
graphe3[grepl("Homme", variable, ignore.case = T), Sexe:="Homme"]
graphe3[grepl("Femme", variable, ignore.case = T), Sexe:="Femme"]

graphe3[, Hypothese:=factor(Hypothese)]


g3 <- 
  ggplot(graphe3, aes(x = Age,y=value, fill=Hypothese, group=Hypothese, color=Hypothese)) +
  coord_flip() + 
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  scale_x_discrete(breaks=c("0","25","50","75","90 ans et +"))+
  geom_bar(data = graphe3[Hypothese == "Centrale"], stat = "identity", alpha=0.6, width=0.9) +
  geom_line(data = graphe3[Hypothese %in% c ("Haute", "Basse") & Sexe=="Homme"])+
  geom_line(data = graphe3[Hypothese %in% c ("Haute", "Basse") & Sexe=="Femme"])+
  annotate(geom="text", x="80", y=-1500, label="Hommes", color="black", size=2)+
  annotate(geom="text", x="80", y=1500, label="Femmes", color="black", size=2)+
  #annotate(geom="text", x=2025, y=0.4, label="Projetée", color="black", size=3)+
  
  #scale_y_discrete(limits=factor(c(-2000,-1000,0,1000,2000)), labels=factor(c(2000,1000,0,1000,2000)))+
  xlab("") +  ylab("") + theme_ispf()+
  theme(legend.position = "bottom", legend.direction = "horizontal")+
  theme(plot.margin=grid::unit(c(2,0,0,0,0), "mm"))
g3




graphe4 <- fread("graphe4.txt", encoding = "UTF-8")
graphe4 <- melt(graphe4,id.vars="Année")
graphe4[grepl("moins de 15 ans", variable, ignore.case = T), Age:="Moins de 15 ans"]
graphe4[grepl("59 ans", variable, ignore.case = T), Age:="15-59 ans"]
graphe4[grepl("60 ans ", variable, ignore.case = T), Age:="60 ans et +"]
graphe4[grepl("Observation", variable, ignore.case = T), Type:="Observation"]
graphe4[grepl("Projection", variable, ignore.case = T), Type:="Projection"]
graphe4$Age <- factor(graphe4$Age)
graphe4$Age <- relevel(graphe4$Age, "Moins de 15 ans")


g4 <- ggplot() +
  aes(x = Année, y = value,  color=Age) +
  geom_line(data=graphe4[Type=="Observation" & !is.na(value)],size = 0.5) +
  geom_line(data=graphe4[Type=="Projection" & !is.na(value)],size = 0.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),limits=c(0,1))+
  annotate(geom="text", x=2010, y=0.4, label="Observée", color="black", size=2)+
  annotate(geom="text", x=2025, y=0.4, label="Projetée", color="black", size=2)+
  annotate(geom="text", x=2030, y=0.65, label="62%", color="black", size=2)+
  annotate(geom="text", x=2030, y=0.15, label="19%", color="black", size=2)+
  annotate(geom="text", x=2030, y=0.25, label="19%", color="black", size=2)+
  
  #geom_text(x=2010, y=0.4, color="black", label="Observée")+
  #geom_text(x=3, y=30, label="Scatter plot")
  
  geom_vline(xintercept=2019, linetype="9919", color="gray")+
  scale_fill_manual(values=ispfPalette)+
  scale_colour_manual(values=ispfPalette)+
  xlab("")+   ylab("")+  theme_ispf()+
  theme(legend.position = "bottom",legend.direction = "horizontal")
g4


suppressMessages(ggsave("graphe1.pdf", g1, width = 9, height=7, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe2.pdf", g2, width = 9, height=8, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe3.pdf", g3, width = 9, height=8, units = "cm", device=cairo_pdf))
suppressMessages(ggsave("graphe4.pdf", g4, width = 9, height=7, units = "cm", device=cairo_pdf))



library(ggrepel)
library(GGally)
library(patchwork)

windowsFonts("Roboto" = windowsFont("Roboto Light"))
#font_import()
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

cat("Création des graphiques en PDF\n")
i <- 4

createSimpleGGPlot <- function(i){
  graphe <- fread(sprintf("graphe%d.txt", i), encoding = "UTF-8", header = T)
  graphe <- melt(graphe,id.vars="Année")
  #graphe3$variable <- factor(graphe3$variable,levels(graphe3$variable)[c(4,1,5,3,2,6)])
  g <- ggplot(graphe, aes(x=Année, y=value, fill=variable, colour=variable)) +
    geom_line()+
    geom_point()+
    scale_y_continuous(labels = scales::number_format(accuracy = 1), )+
    scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ''))+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+
    theme(legend.position = "bottom",legend.direction = "horizontal", legend.text=element_text(size=rel(0.7)))
  g
}

graphe1 <- readCSVFile(1)
#graphe1 <- melt(graphe1,id.vars="Annee")
#graphe1$Annee <- as.factor(graphe1$Annee)

### Matthieu

 resPERTEMP  <- readCSVFile(1)
 resTROUVEMP <- readCSVFile(4)
 resPERTEMP  <- fread(sprintf("%s/graphe%s.csv", projectFolder,1), encoding = "UTF-8", header = T,na.strings = '"NA"')
 resTROUVEMP <- fread(sprintf("%s/graphe%s.csv", projectFolder,4), encoding = "UTF-8", header = T,na.strings = '"NA"')

g1<- ggcoef(resPERTEMP,exponentiate = T,
            mapping = aes(x=estimate,y=reorder(stringr::str_wrap(label,18),resPERTEMP), colour=variable),size=1.5,vline_size = 0.5)+
  theme(legend.position="none",axis.text = element_text(size=5),plot.title = element_text(size = 7))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank(), 
        title = element_text(size = 0,hjust = 0.5, family = "Roboto Light"), 
        plot.title=element_text(hjust=0.5),
        strip.text.y = element_blank(),
        # axis.text.y.left  = element_text(size=5),
        text = element_text(size = 7.5, family = "Roboto Light"),
        )+
  scale_colour_manual(values=ispfPalette) +
  theme( panel.grid.major.y = element_line(colour = "#D8D8D8"), 
         panel.grid.minor.y = element_blank(),
         panel.grid.major.x = element_blank(), 
         panel.grid.minor.x = element_blank(), 
         panel.border = element_blank(), 
         panel.background = element_blank())+
  ggtitle("Se retrouver sans emploi")+
  facet_grid(rows=vars(reorder(variable,idbis)),drop=TRUE,scales = "free")+
  ggcoef(resTROUVEMP,exponentiate = T,
         mapping = aes(x=estimate, y=reorder(stringr::str_wrap(label,18),resTROUVEMP), colour=variable) ,size=1.5,vline_size = 0.5)+
  theme(legend.position = "none",axis.text.y = element_text(size=1,color= "white"), plot.title = element_text(size = 7))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank(), 
        title = element_text(size = 0,hjust = 0.5, family = "Roboto Light"), 
        plot.title=element_text(hjust=0.5),
        strip.text.y = element_blank(),
        text = element_text(size = 7.5, family = "Roboto Light"),
        ) + 
  scale_colour_manual(values=ispfPalette) +
  theme( panel.grid.major.y = element_line(colour = "#D8D8D8"), 
         panel.grid.minor.y = element_blank(),
         panel.grid.major.x = element_blank(), 
         panel.grid.minor.x = element_blank(), 
         panel.border = element_blank(), 
         panel.background = element_blank())+
  ggtitle("Trouver un emploi")+
  facet_grid(rows=vars(reorder(variable,idbis)),drop=TRUE,scales = "free")
g1

  
  graphe2 <- readCSVFile(2)
  graphe2[,diplome2_I:=factor(diplome2_I, levels=rev(c("Général", "Enseignement supérieur", "Baccalauréat", "CAP-BEP", "CEP-DNB", "Sans diplôme")))]
  graphe2[,ACTEU:=factor(ACTEU, levels=rev(c("En emploi", "Au chômage", "Dans le halo autour du chômage", "Inactifs hors halo")))]
  g2<- ggplot( data= graphe2 , aes(x=PT,y=diplome2_I,fill= as.factor(ACTEU)))+
    geom_bar(stat = "identity")+
    labs(title = "Répartition de l'activité des individus en fonction du plus haut diplôme obtenu",
         fill="Position sur le marché du travail") + 
    guides(fill = guide_legend(reverse=TRUE))+
    theme(legend.position="bottom",axis.text = element_text(size=5),plot.title = element_text(size = 5))+
    theme(axis.title.x = element_blank())+
    theme(axis.title.y = element_blank(), axis.ticks = element_blank(),
          title = element_text(size = 5,hjust = 0.5, family = "Roboto Light"), 
          plot.title=element_text(hjust=0.5),
          strip.text.y = element_blank(),
          text = element_text(size = 5, family = "Roboto Light"),
    ) + 
    guides(fill = guide_legend(nrow = 2,reverse=TRUE)) +
    scale_fill_brewer(palette = 1)  +
    theme_ispf()+
    ylab("")+  xlab("")+
    scale_x_continuous(labels = scales::percent) +
  theme(legend.text=element_text(size=rel(0.7)))
  g2
  


graphe3 <- readCSVFile(3)
graphe3[,activfrag:=factor(activfrag, levels=rev(c("CDI", "Fragile", "Autre")))]
graphe3[,SITANT_rec:=factor(SITANT_rec, levels=rev(c("Ensemble", "Sans activité professionnelle", "Etudes ou formation", "En activité professionnelle")))]
g3<- ggplot( data= graphe3, aes(x=pct,y=SITANT_rec,fill=activfrag)) +
  geom_bar(stat = "identity")+labs(title = "Répartition du type de contrat selon la situation qui précède l'emploi"
                                   ,fill="Type de contrat")+
  guides(fill = guide_legend(reverse=TRUE))+
  theme(legend.position="bottom",axis.text = element_text(size=5),plot.title = element_text(size = 5))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_blank(), axis.ticks = element_blank(),
        title = element_text(size = 5,hjust = 0.5, family = "Roboto Light"), 
        plot.title=element_text(hjust=0.5),
        strip.text.y = element_blank(),
        text = element_text(size = 5, family = "Roboto Light"),
  ) + 
  scale_fill_manual(values=ispfPalette)  +
  theme_ispf()+
  ylab("")+  xlab("")+
  scale_x_continuous(labels = scales::percent)+
  theme(legend.text=element_text(size=rel(0.7)))
g3

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 11.25)
saveGrapheFiles(2, largeurCM = 9, hauteurCM = 6)
saveGrapheFiles(3, largeurCM = 9, hauteurCM = 6)


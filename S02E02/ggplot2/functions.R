library(ggrepel)
library(ggplot2)
library(extrafont)
library(showtext)
library(data.table)
library(scales)
#font_add_google("Roboto", "Roboto")
loadfonts(device = "win")
windowsFonts("Roboto" = windowsFont("Roboto Light"))
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
custom.col <- c(ispfPalette[2], ispfPalette[1], "#FFFFFF")

readCSVFile <- function(i){
  fread(sprintf("graphe%s.csv", i), encoding = "UTF-8", header = T)
}


readCSVFileAndMelt <- function(i, keysList, str_wrap_cle=30, str_wrap_variable=30, 
                               isOrdered=T, isVariableOrdered=F, isVariableRevOrdered=F){
  graphe <- fread(sprintf("graphe%s.csv", i), encoding = "UTF-8", header = T)
  
  if(isOrdered) {
    #conserve l'ordre d'origine
    graphe[,(keysList):=stringr::str_wrap(get(keysList), str_wrap_cle)]
    r <- graphe[,get(keysList)]
    graphe[,(keysList):=factor(get(keysList), levels=r, ordered=T)]
  }
  
  graphe <- melt(graphe,id.vars=keysList)
  #graphe[,(keysList):=stringr::str_wrap(get(keysList), str_wrap_cle)]
  if (isVariableOrdered) {
    graphe[,variable:=factor(stringr::str_wrap(variable, str_wrap_variable), ordered = T)]
    if (isVariableRevOrdered)
      graphe[,variable:=forcats::fct_rev(variable)]
  } else
    graphe[,variable:=stringr::str_wrap(variable, str_wrap_variable)]
  graphe
}

readMetaFile <- function(){
  message("Lecture de la liste des figures et tableaux de la publication\n")
  meta <<- fread(paste0(projectFolder, "/meta.csv"), encoding = "UTF-8")
  meta[, id := seq_len(.N), by = type]
}



# Code ggplot2 ------------------------------------------------------------

#ispfPalette <- c("#0071B2", "#A349A4", "#F07D17", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7")
ispfPalette <- c("#56B4E9", "#B359B4", "#FF9D37", "#009FA4", "#999999", "#FF0000", "#D55E00", "#CC79A7", "#345687")

windowsFonts("Roboto" = windowsFont("Roboto Light"))


createGenericGraphe <- function(i, keyList, includeGraphe=F,
                                originalOrder=T, isVariableOrdered=F, isVariableRevOrdered=F,
                                legendPosition="bottom", legendDirection="horizontal",
                                str_wrap_cle=15, str_wrap_variable=15,
                                ylabelFunction=scales::number_format(accuracy = 1),
                                axis.text.x.angle=0){
  
  message(sprintf("Création du graphique %s",i))
  graphe <- readCSVFileAndMelt(i, keyList, str_wrap_cle, str_wrap_variable, isOrdered = originalOrder,
                               isVariableOrdered=isVariableOrdered, isVariableRevOrdered=isVariableRevOrdered)
  if (includeGraphe)
    g <- ggplot(graphe, aes_string(x=keyList, y="value", fill="variable", colour="variable"))
  else 
    g <- ggplot()
  g <- g +
    scale_y_continuous(labels = ylabelFunction)+
    scale_fill_manual(values=ispfPalette)+
    scale_colour_manual(values=ispfPalette)+
    theme_ispf()+
    ylab("")+  xlab("")+
    theme(legend.position = legendPosition, legend.direction = legendDirection,
          axis.text.x = element_text(angle = axis.text.x.angle, hjust = 1))
  g
}


theme_ispf <- function (base_size = 8, base_family = "Roboto Light") 
{
  bgcolor <- "#FFFFFF"
  ret <- theme(rect = element_rect(fill = bgcolor, linetype = 0, colour = NA), 
               text = element_text(size = base_size, family = base_family), 
               title = element_text(size = base_size,hjust = 0.5, family = "Roboto Light"), 
               plot.title = element_text(hjust = 0.5, family = "Roboto Light"), 
               axis.title.x = element_blank(),
               axis.title.y = element_text(hjust = 0.5, family = base_family),
               panel.grid.major.y = element_line(colour = "#D8D8D8"), 
               panel.grid.minor.y = element_blank(),
               panel.grid.major.x = element_blank(), 
               panel.grid.minor.x = element_blank(), 
               panel.border = element_blank(), 
               panel.background = element_blank(),
               legend.key = element_rect(fill = "#FFFFFF00"),
               plot.margin=grid::unit(c(0,0,0,0), "mm"),
               legend.position = "bottom",
               legend.direction = "horizontal",
               legend.title = element_blank(),
               legend.margin=margin(t = 0, unit='cm'),
               legend.key.width=unit(0.2, "cm"),
               legend.text = element_text(size = base_size, family = base_family))
  ret
}

saveGrapheFiles <- function(i, largeurCM=9, hauteurCM=8){
  ggplot <- eval(parse(text=paste0("g",i)))
  pdfFile <- paste0("graphe", i, ".pdf")
  svgFile <- paste0("graphe", i, ".svg")
  suppressMessages(ggsave(pdfFile, ggplot, width = largeurCM, height=hauteurCM, units = "cm", device=cairo_pdf))  
  suppressMessages(ggsave(svgFile, ggplot, width = 5, height=4, units = "in", device=svg))
  
  #On supprime la taille d'affichage pour que le SVG s'adapte à l'écran
  #https://la-cascade.io/rendre-svg-responsif/
  f <- readLines(svgFile)
  f[2] <- "<svg xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\" viewBox=\"0 0 360 288\" version=\"1.1\">"
  writeLines(f, svgFile)
}

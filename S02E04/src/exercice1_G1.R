cotes   <- read.csv2("S02E04/input/cotes.csv", encoding = "UTF-8")
matches <- read.csv2("S02E04/input/matches.csv", encoding = "UTF-8")

setDT(cotes)
setDt(matches)

cotes[, proba:=1-Cote/(1+Cote)]




ggplot(data=cotes, aes(x=reorder(Equipe,-proba), y=proba)) + 
    geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
   scale_y_continuous(labels = scales::percent)+
  labs(title = "Probabilités de victoire finale par équipe", x = "Equipe", y = "% de chance")+
  theme_bw() + 
  theme(
    plot.title = element_text(face = "bold", size = 12),
    legend.background = element_rect(fill = "white", size = 4, colour = "white"),
    legend.justification = c(0, 1),
    legend.position = c(0, 1),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major = element_line(colour = "grey70", size = 0.2),
    panel.grid.minor = element_blank()
  )

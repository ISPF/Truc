library(lubridate)
graphe1 <- readCSVFile(1)
colnames(graphe1)[1] <- "Année"
DateMin <- max(graphe1$Date) %m+% years(-10)
g1 <- ggplot(graphe1[Date>=DateMin], aes(x=Date, y=Valeur12Mois)) +
  geom_line(color=ispfPalette[1])+
  #geom_point()+
  scale_fill_manual(values=ispfPalette)+
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     limits = c(50000,250000))+
  scale_x_date(date_labels = "%Y", date_breaks = "1 years",
               limits=as.Date(c("2011-01-01","2021-01-01")))+
  theme_ispf()+
  ylab("")+
  xlab("")+
  theme(legend.position = "none")
g1

saveGrapheFiles(1, largeurCM = 9, hauteurCM = 7)


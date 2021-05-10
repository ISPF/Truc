source("src/functions.R")

Collectes <- getSQLTableWinLinux("EnqueteEmploi", "dbo", "Collecte")
Collectes <- Collectes[Reporting >= 1]

Suivi <- getSQLTableWinLinux("EnqueteEmploi", "SOC21", "Suivi")


data <- Suivi[StatutPeriode!="Grappe à venir" & StatutCorrige != "FAC",
              .(NomGestionnaire, PrenomEnqueteur, NumSemaine, StatutCorrige)]
data[, .N, by=.(StatutCorrige)][order(N, decreasing = T)]

data2 <- data[, .(Nb=.N), by=.(PrenomEnqueteur, StatutCorrige, NumSemaine)]
data2[, StatutCorrige:=factor(StatutType)]
data2[, StatutCorrige:=forcats::fct_reorder(StatutCorrige, -Nb, min)]
levels(data2$StatutCorrige)

ggplot(data2, aes(x=NumSemaine, y=Nb, fill=StatutCorrige))+
  geom_bar(stat="identity")+
  theme_ispf()+
  scale_fill_manual(values=c(ispfPalette, viridis(10)))+
  facet_wrap(~PrenomEnqueteur, ncol = 4)+
  theme(legend.position = "right", legend.direction = "vertical")



scales::percent(0.8)


  NumFA1 <- "123457"
  NumFA2 <- "123456"
  con <- dbConnect(odbc::odbc(),Driver = "SQL Server",Server = sqlServer,Database = databaseName, encoding="ISO-8859-1")
  tableSQL <- getSQLTableWinLinux("EnqueteEmploi", collecte, "Remplacement")
  if(nrow(tableSQL[OldNumFA == NumFA1 & NewNumFA == NumFA2]) == 0) {
    remplacement <- data.table("OldNumFA" = OldNumFA, "NewNumFA" = NewNumFA) 
    dbAppendTable(con, Id(schema = "SOC21", table = "Remplacement"), remplacement, row.names = NULL, overwrite = FALSE)
    print("ok")
  } else {
    print("ko")
  }

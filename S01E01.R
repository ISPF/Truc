library(DBI)
library(data.table)
library(stringdist)
library(plumber)


# Connexion à la base -----------------------------------------------------
con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "sql", Database = "RTE")
vRechENT <- dbReadTable(con, "Web_RechercheEntreprises")
fwrite(vRechENT, "S01E01/rechENT.csv")


# Recherche avec un SQL-Like ----------------------------------------------

searchString <- "SOCREDO"
s <- toupper(iconv(gsub(' ', '', searchString),from="ISO_8859-1",to="ASCII//TRANSLIT"))
data <- fread("S01E01/rechENT.Csv")
data[,NOM_AI:=toupper(iconv(gsub(' ','', Nom), from="UTF-8",to="ASCII//TRANSLIT"))]
data[like(NOM_AI,s)]



# Recherche avec une distance de Jaro-Winkler -----------------------------
# https://fr.wikipedia.org/wiki/Distance_de_Jaro-Winkler

stringdist("TRUC", "TRUK", method="jw")
data[,jw:=stringdist(NOM_AI, s, method = "jw")]
data[order(jw)][1:10]



# Exposition avec API -----------------------------------------------------
pr("S01E01/plumber.R") %>%
  pr_run(port=4000, host="0.0.0.0")

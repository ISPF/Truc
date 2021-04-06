
#* @apiTitle api.ispf.pf
#* @apiDescription Accès aux données de l'ISPF
#* @apiTOS http://api.ispf.pf
#* @apiBasePath http://api.ispf.pf
#* @apiContact list(name = "API Support", url = "http://www.example.com/support", email = "webmaster@ispf.pf")
#* @apiLicense list(name = "Apache 2.0", url = "https://www.apache.org/licenses/LICENSE-2.0.html")
#* @apiVersion 1.0.1
#* @apiTag Indicateurs 
#* @apiTag Tourisme Serie

library(stringdist)
library(data.table)

data <- fread("rechENT.csv")
data[,NOM_AI:=toupper(iconv(gsub(' ','', Nom), from="UTF-8",to="ASCII//TRANSLIT"))]

#* RechercheRTE-JW
#* @get /SearchRTE-JW/
#* @tag RTE
function(searchString="") {
  s <- toupper(iconv(gsub(' ', '', searchString),from="ISO_8859-1",to="ASCII//TRANSLIT"))
  data[,jw:=stringdist(NOM_AI, s, method = "jw")]
  data[order(jw)][1:10]
}


#* RechercheRTE-Like
#* @get /SearchRTE_Like/
#* @tag RTE
function(searchString="") {
  s <- toupper(iconv(gsub(' ', '', searchString),from="ISO_8859-1",to="ASCII//TRANSLIT"))
  data[like(NOM_AI,s)]
}

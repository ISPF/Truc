#data <- fread("S01E01/input/vAvis.csv")
data <- fread("S01E01/input/rechENT.csv")



searchString <- "SOCREDO"

stringdist("jeff", "geff", method = "jw")

data[,jw:=100*(1-stringdist(Nom, searchString, method = "jw"))]

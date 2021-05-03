fish<-fread("data/fishLTER_red.csv")
fish$Habitat<-as.factor(fish$Habitat)
levels(fish$Habitat)<-c("Récif intérieur","erreur","Pente externe","Frangeant")
fwrite(fish,"data/fishLTER_red.csv",row.names = F,sep=";",dec=",")

coral<-fread("data/coralsLTER_red.csv")
coral$Habitat<-as.factor(coral$Habitat)
levels(coral$Habitat)<-c("Pente externe 10m","Pente externe 17m","Frangeant")
fwrite(coral,"data/coralsLTER_red.csv",row.names=F,sep=";",dec=",")

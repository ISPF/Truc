library(readxl)
library(survival)
path_base <- "S01E04/Survie/Dialyse.xls"
Dialyse   <- read_excel(path_base)
BASE      <- Dialyse

BASE$Maladie1<- 0
BASE$Maladie1[BASE$Maladie=="hypertension"] <- 1
BASE$Maladie1<- as.factor(BASE$Maladie)
BASE$Age <- as.factor(BASE$Age)
BASE$Homme <- factor(BASE$Homme,labels=c("Femme","Homme"))

summary(BASE)

table(BASE$Centre)

fit<-survfit(Surv(Temps,Status)~1,data=BASE)
summary(fit)


par(mfrow=c(1,1))
plot(fit,  xlab="Durée", ylab="Probabilité de survie")
title("Courbe de survie en fonction de l'âge")
abline(h=0.5,col =6)
abline(h=0.75,col =5)



library(ggplot2)
library(ggfortify)
autoplot(fit) + 
  ggtitle("Courbe de survie") +
  xlab("Durée")+
  ylab("Probabilité de survie") +
  geom_line(aes(y = 0.5), col=6) +
  geom_line(aes(y = 0.75), col=5)
  
fit1<-survfit(Surv(Temps,Status)~Age,data=BASE)
autoplot(fit1) + 
  ggtitle("Courbe de survie en fonction de l'âge") +
  xlab("Durée")+
  ylab("Probabilité de survie")


str(BASE)
#levels met dans l'ordre 

table(BASE$Age)
fit1<-survfit(Surv(Temps,Status)~Age,data=BASE)
summary(fit1)


#classer par ordre les ages 
BASE$Age <- as.factor(BASE$Age)
levels(BASE$Age)

plot(fit1,col=1:4, xlab="Durée (mois)", ylab = "Probabilite de survie")
legend("bottomleft",c("- 25","25-50","50-70","70+"),fill=1:4) 
title("Courbe de survie en fonction de l'âge")
abline(h=0.5,col =6)


diff = survdiff(Surv(Temps,Status)~Age, data= BASE)
pchisq(diff$chisq, length(diff$n)-1, lower.tail = FALSE)


fit2<-survfit(Surv(Temps,Status)~Centre,data=BASE)
# summary(fit2)
autoplot(fit2) + 
  ggtitle("Courbe de survie en fonction des centres") +
  xlab("Durée")+
  ylab("Probabilité de survie")

table(BASE$Centre)
plot(fit2,col=1:4, xlab="Durée", ylab = "Probabilite de survie")
legend("bottomleft",c("Centre A","Centre B","Centre C"),fill=1:3) 
title("Courbe de survie en fonction des Centre")
abline(h=0.5,col =6)
abline(h=0.75,col =3)


fit3<-survfit(Surv(Temps,Status)~Homme,data=BASE)
# summary(fit3)
autoplot(fit3) + 
  ggtitle("Courbe de survie en fonction des genres") +
  xlab("Durée")+
  ylab("Probabilité de survie")

table(BASE$Homme)
plot(fit3,col=1:4, xlab="Durée", ylab = "Probabilite de survie")
legend("bottomleft",c("Femme","Homme"),fill=1:2) 
title("Courbe de survie en fonction des genres")
abline(h=0.5,col =6)
abline(h=0.75,col =3)


fit4<-survfit(Surv(Temps,Status)~Maladie,data=BASE)
# summary(fit4)
autoplot(fit4) + 
  ggtitle("Courbe de survie en fonction des maladies") +
  xlab("Durée")+
  ylab("Probabilité de survie")


table(BASE$Maladie)
plot(fit4,col=1:4, xlab="Durée", ylab = "Probabilite de survie")
legend("bottomleft",c("Diabete","Hypertension","renale"),fill=1:3) 
title("Courbe de survie en fonction des Maladies")
abline(h=0.5,col =6)
abline(h=0.75,col =3)




#test de LogRank 
diff = survdiff(Surv(Temps,Status)~Age, data= BASE)
pchisq(diff$chisq, length(diff$n)-1, lower.tail = FALSE)

diff1 = survdiff(Surv(Temps,Status)~Centre, data= BASE)
pchisq(diff1$chisq, length(diff1$n)-1, lower.tail = FALSE)

diff2 = survdiff(Surv(Temps,Status)~Maladie, data= BASE)
pchisq(diff2$chisq, length(diff2$n)-1, lower.tail = FALSE)

diff2 = survdiff(Surv(Temps,Status)~Homme, data= BASE)
pchisq(diff2$chisq, length(diff2$n)-1, lower.tail = FALSE)

# il existe une différence significative entre les groupes des 
# trois variables ci dessous sauf pour la variable Homme 
# faut faire attention en échantillon de population 
# si elle est représentative dns la generalité 
quantile(fit1)





# modele de cox 



str(BASE)
### effet de differents facteur s

BASE$Centre<- as.factor(BASE$Centre)
BASE$Maladie<- as.factor(BASE$Maladie)
BASE$Age<- as.factor(BASE$Age)

table(BASE$Centre)
levels(BASE$Centre)

#choisir les criteres les plus faibles 
cfit1       <- coxph(Surv(Temps,Status) ~ Centre + Age + Maladie, data=BASE)
result.step <- step(cfit1,scope=list(upper = ~Centre + Age + Maladie, lower = ~1))
summary(cfit1)

#BASE$Centre <- factor(BASE$Centre, levels = c("CA","CB","CC"))


####
# il y a une variable signi au seuil de 5% Age moins de 25 
#Le fait que le patient est âgée de plus de 70, la prob de deces d'un 
#facteur de 3.9, age entre 25 et 50 ans, soit une augmentation de 2,9 fois 

#meme deduction pour les personnes age entre 50-70 ans, 



#test de concordance 
#% de paires dans la base où les observations avec le temps de survie le plus
#élevé ont la plus grande probabilité de survie prédite par le modè


#test de ratio de vraisemblance 



#Verification H des risque de proportionnels
(res.zph1 <- cox.zph(cfit1))

#P_value > 5% 
# CentreB, Moins de 25, Plus de 70, Hypertension, renale 
# On peut stratifier avec les centre malgre que l'un des deux centre 
# nest pas signi dans le modele 
# 

# p < 5% 
# CentreC, Age[50-70[ , 


#Si l’hypothèse des risques proportionnels n’est pas vérifiée pour une variable du modèle
#il est possible de stratifier le risque de base par rapport à cette variable

# examiner l'effet de la variable de stratification 

#choisir les variables avec p_value > 5% :# CentreB, Moins de 25, Plus de 70, Hypertension, renale 
#
cfit2 <- coxph(Surv(Temps,Status) ~ Age+ Maladie + strata(Centre) , data=BASE)
summary(cfit2)

# logique dans le test, la majorité ont les meme caractéristiques 
# par rapport à l'age et les maladies dans les centre de traitement 
# voir si les p_value sont supérieur a 5% et on remarque qu'en on stratifie 
# avec le centre , les p_value sont supérieur a 5% donc on accepte 
# l'H Hazard proportionnel au seuil de 5% 

cfit3 <- coxph(Surv(Temps,Status) ~ Age+ Centre + strata(Maladie) , data=BASE)
str(BASE)

# on interprete les variables par rapport a la variable de reference
# 

(res.zph <- cox.zph(cfit2))
(res.zph <- cox.zph(cfit3))

#avec stratifier pour la variables Centre, pour les pourcentage 
# Le fait que l'ind est 
# meme conclusion que celui d'avant 

str(BASE)

#regarder les coef estimé par rapport , a l'emplacement des points dns les 
# graphiques, 
# Si les valeurs des dfbeta sont faibles au regard des coefficients, on peut supposer 
# que les observations n’ont pas d’impact significatif sur les coefficients étudiés



dfbeta = residuals(cfit2, type="dfbeta") 
par(mfrow=c(2,4))
for (j in 1:5) {
  plot(dfbeta[,j], 
  ylab=names(coef(cfit2))[j]) 
  abline(h=0)}
str(BASE)

boxplot(BASE$Age)


#coorection TD3 : courbe de survie et modele de Cox 
# Centre : attention à l'interprétation a cette variable par rapport a la qualité d'un centre
# factorisé les variables qualitative 
# pas factoriser Status, on laisse en numerique... 
 
# Age il faut mettre dans l'ordre 

Dialyse$Age <- factor(Dialyse$Age, levels = c("Moins de 25","[25-50[","[50-70[","Plus de 70"))
table(Dialyse$Age)



















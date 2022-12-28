#https://fr.wikipedia.org/wiki/M%C3%A9thode_de_Schulze
#https://fr.wikipedia.org/wiki/Vote_%C3%A0_second_tour_instantan%C3%A9

library(votesys)
library(data.table)

dt <- fread("S03E01/TRUC S03.csv")
dt2 <- data.table(sujet=colnames(dt))
dt2[,id:=.I]
dt2 <- dt2[!id %in% c(1,27)]
dt2[,id:=letters[1:.N]]
setcolorder(dt2, "id")
colnames(dt2) <- c("idSujet", "Sujet")

#nbSujets <- nrow(sujets)
#votes <- matrix(nrow = 100,ncol=nbSujets)
#votes <- t(mapply(1:nrow(votes), FUN=function(x)
#  sample(1:nbSujets,nbSujets,replace=FALSE)))

#sujets <- fread("S01E02/sujets.csv", encoding = "UTF-8")
sujets <- copy(dt2)
#votes <- as.matrix(fread("S01E02/votes.csv"))
votes <- as.matrix(fread("S03E01/votes03.csv"))

votes[is.na(votes)]<-9
colnames(votes) <- sujets$Sujet

vote <- create_vote(votes, xtype = 1)
resultatVote <- cdc_schulze(vote)
Winner <- resultatVote$winner
OrderedWinners <- resultatVote$summary_m[,3]
OrderedWinners <- OrderedWinners[order(OrderedWinners,decreasing = T)]

Winner
OrderedWinners
t(t(OrderedWinners))

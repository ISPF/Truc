#https://fr.wikipedia.org/wiki/M%C3%A9thode_de_Schulze
#https://fr.wikipedia.org/wiki/Vote_%C3%A0_second_tour_instantan%C3%A9

library(votesys)
library(data.table)

#nbSujets <- nrow(sujets)
#votes <- matrix(nrow = 100,ncol=nbSujets)
#votes <- t(mapply(1:nrow(votes), FUN=function(x)
#  sample(1:nbSujets,nbSujets,replace=FALSE)))

trucs02 <- fread("S02E01/trucs02.csv", encoding = "UTF-8")
trucs02[trucs02==""] <- NA
trucs02[trucs02=="1 (Premier choix)"] <- 1
trucs02[,1:=NULL]

sujets <- colnames(trucs02)
votes <- as.matrix(trucs02)
mode(votes) = "numeric"
colnames(votes) <- sujets

vote <- create_vote(votes, xtype = 1)
resultatVote <- cdc_schulze(vote)
Winner <- resultatVote$winner
OrderedWinners <- resultatVote$summary_m[,3]
OrderedWinners <- OrderedWinners[order(OrderedWinners,decreasing = T)]

Winner
OrderedWinners
t(t(OrderedWinners))

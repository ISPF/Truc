#https://fr.wikipedia.org/wiki/M%C3%A9thode_de_Schulze
#https://fr.wikipedia.org/wiki/Vote_%C3%A0_second_tour_instantan%C3%A9

library(votesys)
library(data.table)

#nbSujets <- nrow(sujets)
#votes <- matrix(nrow = 100,ncol=nbSujets)
#votes <- t(mapply(1:nrow(votes), FUN=function(x)
#  sample(1:nbSujets,nbSujets,replace=FALSE)))

sujets <- fread("S01E02/sujets.csv")
votes <- as.matrix(fread("S01E02/votes.csv"))
colnames(votes) <- sujets$Sujet

vote <- create_vote(votes, xtype = 1)
resultatVote <- cdc_schulze(vote)
Winner <- resultatVote$winner
OrderedWinners <- resultatVote$summary_m[,3]
OrderedWinners <- OrderedWinners[order(OrderedWinners,decreasing = T)]

votes
OrderedWinners
library(votesys)
library(data.table)

sujets <- fread("S01E02/sujets.csv")
nbSujets <- nrow(sujets)

#votes <- as.matrix(fread("S01E02/votes.csv"))
votes <- matrix(nrow = 100,ncol=nbSujets)
votes <- t(mapply(1:nrow(votes), FUN=function(x)
  sample(1:nbSujets,nbSujets,replace=FALSE)))
colnames(votes) <- sujets$Sujet
votes

vote <- create_vote(votes, xtype = 1)
resultatVote <- irv_method(vote)
resultatVote$winner

#check_dup_wrong()

#https://en.wikipedia.org/wiki/Instant-runoff_voting
#https://fr.wikipedia.org/wiki/Vote_%C3%A0_second_tour_instantan%C3%A9

#y <- cdc_simple(vote)
#y <- cdc_rankedpairs(vote)



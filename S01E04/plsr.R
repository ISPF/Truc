library(FactoMineR)
library(pls)
library(ggplot2)

data(decathlon)

set.seed(123)
sample <- sample(1:nrow(decathlon),nrow(decathlon)/5*4,replace = F)
train  <- decathlon[sample,]
test   <- decathlon[setdiff(1:nrow(decathlon),sample),]


# pcr------------------------------------
# ACP
res_pca <- PCA(decathlon[,1:10],ind.sup = setdiff(1:nrow(decathlon),sample))
summary(res_pca)

str(res_pca)

train_proj <- as.data.frame(res_pca$ind$coord)
test_proj  <- as.data.frame(res_pca$ind.sup$coord)

train_proj <- merge(train_proj,train["Points"], by="row.names")
test_proj  <- merge(test_proj,test["Points"], by="row.names")

model_pcr <- lm(Points~Dim.1+Dim.2+Dim.3, train_proj)
summary(model_pcr)
plot(model_pcr)

str(model_pcr)
predict(model_pcr)

ggplot() +
  geom_point(aes(x =  model_pcr$model$Points, y = predict(model_pcr))) +
  geom_line(aes(x = model_pcr$model$Points, y = model_pcr$model$Points),
            colour = 'red', linetype=2) +
  ggtitle('Régression linéaire sur composantes principales', subtitle = "Echantillon d'apprentissage") +
  xlab('Réel') +
  ylab('Prédit')

ggplot() +
  geom_point(aes(x =  test$Points, y = predict(model_pcr,test_proj))) +
  geom_line(aes(x = test$Points, y = test$Points),
            colour = 'red', linetype=2) +
  ggtitle('Régression linéaire sur composantes principales', subtitle = "Echantillon Test") +
  xlab('Réel') +
  ylab('Prédit')

# plsr ----------------------------

plsr_fit <- plsr(Points ~ ., ncomp=6, data = train[,c(1:10,12)], scale=T )
summary(plsr_fit)

plot(RMSEP(plsr_fit), legendpos = "topright")

plot(plsr_fit, ncomp = 2, asp = 1, line = TRUE)
plot(test$Points,predict(plsr_fit,test[,c(1:10,12)])[,,"1 comps"])

plot(plsr_fit, ncomp = 2, asp = 1, line = TRUE)

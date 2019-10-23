names(rshpCoffeeCkQ) <- c("YMD", "Q", "model")
#rshpTriQ.Pred.Loess10
names(rshpTriQ.Pred.Loess10) <- c("YMD", "Q", "model")
#rshpTriQ.Pred.Loess25
names(rshpTriQ.Pred.Loess25) <- c("YMD", "Q", "model")
#rshpTriQ.Pred.Loess50
names(rshpTriQ.Pred.Loess50) <- c("YMD", "Q", "model")
#rshpTriQ.Pred.Linear
names(rshpTriQ.Pred.Linear) <- c("YMD", "Q", "model")


rshpTAllQPred %>% group_by(model)

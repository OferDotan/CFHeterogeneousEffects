#
# Analysis ignoring clusters
#
dr.score = tau.hat + W.treatment / cf$W.hat *
  (Y.outcome - cf$Y.hat - (1 - cf$W.hat) * tau.hat) -
  (1 - W.treatment) / (1 - cf$W.hat) * (Y.outcome - cf$Y.hat + cf$W.hat * tau.hat)

cf.noclust = causal_forest(X.vars[,selected.idx], Y.outcome, W.treatment,
                           Y.hat = Y.hat, W.hat = W.hat,
                           tune.parameters = "all")

ATE.noclust = average_treatment_effect(cf.noclust)
paste("95% CI for the ATE:", round(ATE.noclust[1], 3),
      "+/-", round(qnorm(0.975) * ATE.noclust[2], 3))

test_calibration(cf.noclust)

tau.hat.noclust = predict(cf.noclust)$predict
plot(C.vill.id, tau.hat.noclust)


nfold = 5
vill.id.levels = unique(C.vill.id)
cluster.folds = sample.int(nfold, length(vill.id.levels), replace = TRUE)

tau.hat.crossfold = rep(NA, length(Y.outcome))
for (foldid in 1:nfold) {
  print(foldid)
  infold = C.vill.id %in% vill.id.levels[cluster.folds == foldid]
  cf.fold = causal_forest(X.vars[!infold, selected.idx], Y.outcome[!infold], W.treatment[!infold],
                          Y.hat = Y.hat[!infold], W.hat = W.hat[!infold],
                          tune.parameters = "all")
  pred.fold = predict(cf.fold, X.vars[infold, selected.idx])$predictions
  tau.hat.crossfold[infold] = pred.fold
}

cf.noclust.cpy = cf.noclust
cf.noclust.cpy$predictions = tau.hat.crossfold
cf.noclust.cpy$clusters = C.vill.id
test_calibration(cf.noclust.cpy)

Rloss = mean(((Y.outcome - Y.hat) - tau.hat * (W.treatment - W.hat))^2)
Rloss.noclust = mean(((Y.outcome - Y.hat) - tau.hat.noclust * (W.treatment - W.hat))^2)
Rloss.crossfold = mean(((Y.outcome - Y.hat) - tau.hat.crossfold * (W.treatment - W.hat))^2)

c(Rloss.noclust - Rloss, Rloss.crossfold - Rloss)

summary(aov(dr.score$predictions ~ factor(C.vill.id)))

#
# Analaysis without fitting the propensity score
#

cf.noprop = causal_forest(X_vars[,selected.idx], Y.outcome, W.treatment,
                          Y.hat = Y.hat, W.hat = mean(W.treatment),
                          tune.parameters = "all",
                          equalize.cluster.weights = TRUE,
                          clusters = C.vill.id)
tau.hat.noprop = predict(cf.noprop)$predictions

ATE.noprop = average_treatment_effect(cf.noprop)
paste("95% CI for the ATE:", round(ATE.noprop[1], 3),
      "+/-", round(qnorm(0.975) * ATE.noprop[2], 3))

###this is not working yet.
pdf("tauhat_noprop.pdf")
pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

plot.new
plot(tau.hat, tau.hat.noprop,
     xlim = range(tau.hat, tau.hat.noprop),
     ylim = range(tau.hat, tau.hat.noprop))
     xlab = "orthogonalized causal forest estimates"
     ylab = "non-orthogonalized causal forest"
abline(0, 1, lwd = 2, lty = 2, col = 4)
par = pardef
dev.off()


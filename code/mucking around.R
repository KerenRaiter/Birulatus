# learning about AICs and getting wi AIC, the model weight.

fitM1 = lm(data$M1~data$BM)
logM1 = logLik(fitM1)
aicM1 = 2*(n+1) - 2*logM1

AIC <- c(-105885.1, -105121.2, -109740.6, -117007, -105858.8, -108601.9, -108856.9)
delta <- AIC-min(AIC)
denominator <- sum(exp(-0.5*delta))
weights <- exp(-0.5*delta)/denominator
weights   # result: 0 0 0 1 0 0 0, i.e. only the model with the minimum AIC has relevance to the final answer.
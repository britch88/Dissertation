
library(hglm) |> suppressPackageStartupMessages()

form <- formula(rate ~ 1 + Year + metstatcat + income.ratio + pov.rate +
                  single.headed + corrections.pct + nonwhite)


suppressWarnings(HGLM_iid <- hglm(fixed = form,
                                  random = ~1 | state,
                                  data = reg.dat2,
                                  family = gaussian()))

reg.dat3 <- reg.dat2
tempx$HGLM_re <- unname(HGLM_iid$ranef)


############## Adding spatial
library(hglm) |> suppressPackageStartupMessages()
library(spatialreg)
neighb.data <- poly2nb(reg.dat2, queen=T)
W <- as(spdep::nb2listw(neighb.data, style = "B"), "CsparseMatrix")

suppressWarnings(HGLM_iid <- hglm(fixed = form,
                                  random = ~1 | NOX_ID,
                                  data = boston_487,
                                  family = gaussian()))
boston_93$HGLM_re <- unname(HGLM_iid$ranef)


state.effects <- as.data.frame(unique(reg.dat2$state))

suppressWarnings(HGLM_car <- hglm(fixed = form,
                                  random = ~1 | state,
                                  data = reg.dat2,
                                  family = gaussian(),
                                  rand.family = CAR(D=W)))


tempx$HGLM_ss <- HGLM_car$ranef[,1]


### another option
library(R2BayesX) |> suppressPackageStartupMessages()

BX_iid <- bayesx(update(form, . ~ . + sx(NOX_ID, bs = "re")),
                 family = "gaussian", data = boston_487,
                 method = "MCMC", iterations = 12000,
                 burnin = 2000, step = 2, seed = 123)

boston_93$BX_re <- BX_iid$effects["sx(NOX_ID):re"][[1]]$Mean





library(spaMM)
# fit the model
m_spamm <- fitme(rate ~ 1 + Year + metstatcat + income.ratio + pov.rate +
                   single.headed + corrections.pct + nonwhite + Matern(1 | state), data = reg.dat2, family = "poisson") # this take a bit of time
# model summary
summary(m_spamm)




# Full model
model4 <-lmer(rate ~ 1 + Year + metstatcat + income.ratio + pov.rate +
                single.headed + corrections.pct + nonwhite + residuals +
                (1|state), data=reg.dat2) 



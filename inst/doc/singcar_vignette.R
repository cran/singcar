## ---- setup, include=FALSE----------------------------------------------------
options(prompt = 'R> ', continue = '+ ')

library(singcar)
library(MASS)
library(lme4)
library(lmerTest)

## ----scload, eval= FALSE, echo = TRUE-----------------------------------------
#  install.packages("singcar")
#  library("singcar")

## ----dataset, echo = TRUE-----------------------------------------------------
head(size_weight_illusion) 

## ----patex, echo = TRUE-------------------------------------------------------
caseA <- size_weight_illusion[1, "V_SWI"]
contA <- size_weight_illusion[-1, "V_SWI"]
caseB <- size_weight_illusion[1, "K_SWI"]
contB <- size_weight_illusion[-1, "K_SWI"]
caseAGE <- size_weight_illusion[1, "YRS"]
contAGE <- size_weight_illusion[-1, "YRS"]

## ----tdcode, echo=TRUE--------------------------------------------------------
TD(case = caseA, controls = contA, alternative = "less", conf_level = 0.95)

## ----rsdtcode, echo = TRUE----------------------------------------------------
RSDT(case_a = caseA, case_b = caseB, controls_a = contA, controls_b = contB,
      alternative = "two.sided")

## ----btdcode, echo=TRUE, cache=TRUE-------------------------------------------
BTD(case = caseA, controls = contA, alternative = "less",
     iter = 10000, int_level = 0.95)

## ----bsdtcode, echo = TRUE, cache = TRUE--------------------------------------
BSDT(case_a = caseA, case_b = caseB, controls_a = contA, controls_b = contB,
      alternative = "two.sided", iter = 10000, unstandardised = FALSE,
      calibrated = TRUE)

## ----btdcov, echo=TRUE, cache=TRUE--------------------------------------------
BTD_cov(case_task = caseA, case_covar = caseAGE, control_task = contA,
         control_covar = contAGE, alternative = "less", int_level = 0.95,
         iter = 10000, use_sumstats = FALSE)

## ----bsdtcov, echo=TRUE, cache=TRUE-------------------------------------------
BSDT_cov(case_tasks = c(caseA, caseB), case_covar = caseAGE, 
          control_tasks = cbind(contA, contB), control_covar = contAGE, 
          alternative = "two.sided", int_level = 0.95, iter = 10000,
          calibrated = TRUE, use_sumstats = FALSE)

## ----pkgload, eval=FALSE, echo = TRUE-----------------------------------------
#  install.packages(c("lme4", "lmerTest", "MASS"))
#  library("lme4")
#  library("lmerTest")
#  library("MASS") # For multivariate simulation

## ----simdatlmm, echo = TRUE---------------------------------------------------
simdata <-  expand.grid(case = c(1, rep(0, 29)), item = factor(1:4))
simdata$ID <- factor(rep(1:30, 4)) 

set.seed(123) # For replicability
mu <- c(100, 80, 50, 20)  
sigma <- matrix(c(15^2, 108, 78, 30,
                  108, 10^2, 12, 15,
                  78,  12, 10^2, 25,
                  30,  15,  25,  5^2), nrow = 4, byrow = T)
dv <- mvrnorm(30, mu = mu, Sigma = sigma) # Dependent variable

dv[1, ] <- dv[1, ] + c(-30, -20, -20,  -10) # Case scores
simdata$dv <- c(dv)

## ----lmm1, echo=TRUE----------------------------------------------------------
model1 <- lmer(dv ~ case + (1|ID) + (1|item), data = simdata)
summary(model1)[["coefficients"]]

## ----tdcomp, echo = TRUE------------------------------------------------------
agg_data <- rowMeans(dv)
TD(agg_data[1], agg_data[-1], alternative = "two.sided")[["statistic"]]

## ----simdatlmm2, echo = TRUE--------------------------------------------------
simdata <-  expand.grid(case = c(1, rep(0, 29)),
                        item = factor(1:2), condition = factor(1:2))
simdata$ID <- factor(rep(1:30, 4)) 
contrasts(simdata$condition) <- contr.sum(2) # Effect coding

set.seed(123) # For replicability
mu <- c(100, 80, 50, 20)  
sigma <- matrix(c(15^2, 120,    45,  7,
                  120, 10^2,    11, 15,
                  45,    11,  10^2, 40,
                  7,     15,    40, 5^2), nrow = 4, byrow = T)
dv <- mvrnorm(30, mu = mu, Sigma = sigma) # Dependent variable
dv[1, ] <- dv[1, ] + c(-30, -20, -0,  -0) # Case scores
simdata$dv <- c(dv)

## ----lmm2, echo = TRUE--------------------------------------------------------
model2 <- lmer(dv ~ case*condition + (condition|ID) + (1| item), data = simdata)
round(summary(model2)[["coefficients"]], 5)

## ----rsdtcomp, echo=TRUE------------------------------------------------------
agg_data1 <- rowMeans(dv[ , 1:2])
agg_data2 <- rowMeans(dv[ , 3:4])
RSDT(agg_data1[1], agg_data2[1], agg_data1[-1], agg_data2[-1])[["p.value"]]

## ----mtdcode, eval=FALSE, echo=TRUE-------------------------------------------
#  MTD(case = c(caseA, caseB), controls = cbind(contA, contB),
#      conf_level = 0.95, method = c("pd", "pchi", "pf", "pmd"),
#      mahalanobis_dist = NULL, k = NULL, n = NULL)

## ----TD-power-----------------------------------------------------------------
TD_power(case = 70,
         mean = 100,
         sd = 15,
         sample_size = 16,
         power = NULL,
         alternative = "less",
         alpha = 0.05,
         spec = 0.005)

## ----TDpowersize--------------------------------------------------------------
TD_power(case = 70,
         mean = 100,
         sd = 15,
         sample_size = NULL,
         power = 0.6,
         alternative = "less",
         alpha = 0.05,
         spec = 0.005)

## ----BTDpower-----------------------------------------------------------------
BTD_power(case = 70,
         mean = 100,
         sd = 15,
         sample_size = 15,
         alternative = "less",
         alpha = 0.05,
         nsim = 1000,
         iter = 1000)

## ----BTDcovpower--------------------------------------------------------------
covars <- matrix(c(0, 1,
                   0, 1), ncol = 2, byrow = TRUE)
BTD_cov_power(case = -2,
              case_cov = c(0.2, -0.6),
              control_task = c(0, 1),
              control_covar = covars,
              cor_mat = diag(3) + 0.3 - diag(c(0.3, 0.3, 0.3)),
              sample_size = 15,
              alternative = "less",
              alpha = 0.05,
              nsim = 100,
              iter = 100)

## ----RSDTpower----------------------------------------------------------------
RSDT_power(case_a = 70,
           case_b = 55,
           mean_a = 100,
           mean_b = 50,
           sd_a = 15,
           sd_b = 10,
           r_ab = 0.5,
           sample_size = 15,
           alternative = "two.sided",
           alpha = 0.05,
           nsim = 1000)

## ----BSDTcovpower-------------------------------------------------------------
cor_mat <- matrix(c(1,   0.5, 0.6,
                    0.5,   1, 0.3,
                    0.6, 0.3,   1), ncol = 3, byrow = TRUE)
BSDT_cov_power(case_tasks = c(70, 55),
               case_cov = 65,
               control_tasks = matrix(c(100, 15,
                                        50, 10), ncol = 2, byrow = TRUE),
               control_covar = c(50, 25),
               cor_mat = cor_mat,
               sample_size = 15,
               alternative = "two.sided",
               alpha = 0.05,
               nsim = 100,
               iter = 100,
               calibrated = TRUE)

## ----powercode, eval=FALSE, echo=TRUE-----------------------------------------
#  powerLMM <- function(nsim, case_impairment = c(0, 0, 0, 0),
#                       compare_against = c("RSDT", "BSDT"), alpha = 0.05) {
#    require(lme4)
#    require(lmerTest)
#    require(MASS)
#    comptest <- match.arg(compare_against)
#    pdt <- vector(length = nsim)
#    plm <- vector(length = nsim)
#    simdata <-  expand.grid(case = c(1, rep(0, 29)),
#                            item = factor(1:2), condition = factor(1:2))
#    simdata$ID <- factor(rep(1:30, 4))
#    contrasts(simdata$condition) <- contr.sum(2)
#    mu <- c(100, 80, 50, 20)
#    sigma <- matrix(c(15^2, 120,    45,  7,
#                      120, 10^2,    11, 15,
#                      45,    11,  10^2, 40,
#                      7,     15,    40, 5^2), nrow = 4, byrow = T)
#    for (i in 1:nsim){
#      dv <- mvrnorm(30, mu = mu, Sigma = sigma)
#      dv[1, ] <- dv[1, ] + case_impairment
#      simdata$dv <- c(dv)
#      model2 <- lmer(dv ~ case*condition + (condition|ID) + (1| item), data = simdata)
#  
#      agg_data1 <- rowMeans(dv[ ,1:2])
#      agg_data2 <- rowMeans(dv[ ,3:4])
#      if (comptest == "RSDT"){
#        pdt[i] <- RSDT(agg_data1[1], agg_data2[1],
#                       agg_data1[-1], agg_data2[-1])[["p.value"]]
#      } else {
#        pdt[i] <- BSDT(agg_data1[1], agg_data2[1],
#                       agg_data1[-1], agg_data2[-1])[["p.value"]]
#      }
#      plm[i] <- summary(model2)[["coefficients"]][4, 5]
#    }
#    if (comptest == "RSDT"){
#      data.frame(RSDT = sum(pdt < 0.05)/nsim, LMM = sum(plm < 0.05)/nsim)
#    } else {
#      data.frame(BSDT = sum(pdt < 0.05)/nsim, LMM = sum(plm < 0.05)/nsim)
#    }
#  }


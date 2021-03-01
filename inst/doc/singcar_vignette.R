## ---- setup, include=FALSE----------------------------------------------------
options(prompt = 'R> ', continue = '+ ')

library(singcar)

## ----loadpack, eval=FALSE-----------------------------------------------------
#  install.packages("singcar")
#  library("singcar")

## ----datahead-----------------------------------------------------------------
head(size_weight_illusion)

## ----VSWI---------------------------------------------------------------------
PAT_VSWI <- size_weight_illusion[1, "V_SWI"]
CON_VSWI <- size_weight_illusion[-1, "V_SWI"] 

## ----TD-----------------------------------------------------------------------
TD(case = PAT_VSWI,
   controls = CON_VSWI,
   sd = NULL,
   sample_size = NULL,
   alternative = "less",
   conf_int = TRUE,
   conf_level = 0.95,
   conf_int_spec = 0.01,
   na.rm = FALSE)

## ----BTD----------------------------------------------------------------------
set.seed(42)
BTD(case = PAT_VSWI,
    controls = CON_VSWI,
    sd = NULL,
    sample_size = NULL,
    alternative = "less",
    int_level = 0.95,
    iter = 10000,
    na.rm = FALSE)

## ----covariate----------------------------------------------------------------
PAT_age <- size_weight_illusion[1, "YRS"] 
CON_age <- size_weight_illusion[-1, "YRS"]

## ----BTD-cov------------------------------------------------------------------
BTD_cov(case_task = PAT_VSWI,
        case_covar = PAT_age,
        control_task = CON_VSWI,
        control_covar = CON_age,
        alternative = "less",
        int_level = 0.95,
        iter = 1000,
        use_sumstats = FALSE,
        cor_mat = NULL,
        sample_size = NULL)

## ----KSWI---------------------------------------------------------------------
PAT_KSWI <- size_weight_illusion[1, "K_SWI"] 
CON_KSWI <- size_weight_illusion[-1, "K_SWI"] 

## ----RSDT---------------------------------------------------------------------
RSDT(case_a = PAT_VSWI,
     case_b = PAT_KSWI,
     controls_a = CON_VSWI,
     controls_b = CON_KSWI,
     sd_a = NULL,
     sd_b = NULL,
     sample_size = NULL,
     r_ab = NULL,
     alternative = "two.sided",
     na.rm = FALSE)

## ----BSDT---------------------------------------------------------------------
BSDT(case_a = PAT_VSWI,
     case_b = PAT_KSWI,
     controls_a = CON_VSWI,
     controls_b = CON_KSWI,
     sd_a = NULL,
     sd_b = NULL,
     sample_size = NULL,
     r_ab = NULL,
     alternative = "two.sided",
     int_level = 0.95,
     iter = 10000,
     unstandardised = FALSE,
     calibrated = TRUE,
     na.rm = FALSE)

## ----BSDT_cov-----------------------------------------------------------------
BSDT_cov(case_tasks = c(PAT_VSWI, PAT_KSWI),
         case_covar = PAT_age,
         control_tasks = cbind(CON_VSWI, CON_KSWI),
         control_covar = CON_age,
         alternative = "two.sided",
         int_level = 0.95,
         calibrated = TRUE,
         iter = 1000,
         use_sumstats = FALSE,
         cor_mat = NULL,
         sample_size = NULL)

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


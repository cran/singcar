## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(singcar)

## ----installation, eval=FALSE-------------------------------------------------
#  install.packages("devtools")
#  library("devtools")
#  install_github("jorittmo/singcar")
#  library("singcar")

## ----TD-----------------------------------------------------------------------
# Extracting scores from the visual size-weight illusion from size_weight_illusion 
DF_V_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "V_SWI"] # Patient
CON_V_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "V_SWI"] # Controls

TD(case = DF_V_SWI, controls = CON_V_SWI, conf_int = TRUE)
 

## ----BTD----------------------------------------------------------------------
# Extracting scores from the visual size-weight illusion from size_weight_illusion 
DF_V_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "V_SWI"] # Patient 
CON_V_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "V_SWI"] # Controls

BTD(case = DF_V_SWI, controls = CON_V_SWI)

## ----BTD_cov------------------------------------------------------------------
# Extracting scores from the visual size-weight illusion from size_weight_illusion 
DF_V_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "V_SWI"] # Patient
CON_V_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "V_SWI"] # Controls

# Extracting the coviariate below
DF_age <- size_weight_illusion[size_weight_illusion$PPT == "DF", "YRS"] # Patient
CON_age <- size_weight_illusion[size_weight_illusion$PPT != "DF", "YRS"] # Controls

BTD_cov(case_task = DF_V_SWI, case_covar = DF_age, control_task = CON_V_SWI,
        control_covar = CON_age, iter = 100)

## ----UDT----------------------------------------------------------------------
# Extracting scores from the visual size-weight illusion from size_weight_illusion 
DF_V_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "V_SWI"] # Patient
CON_V_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "V_SWI"] # Controls

# Extracting scores from the kinesthetic size-weight illusion from size_weight_illusion 
DF_K_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "K_SWI"] # Patient
CON_K_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "K_SWI"] # Controls

UDT(case_a = DF_V_SWI, case_b = DF_K_SWI, controls_a = CON_V_SWI, controls_b = CON_K_SWI)

## ----RSDT---------------------------------------------------------------------
# Extracting scores from the visual size-weight illusion from size_weight_illusion 
DF_V_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "V_SWI"] # Patient
CON_V_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "V_SWI"] # Controls

# Extracting scores from the kinesthetic size-weight illusion from size_weight_illusion 
DF_K_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "K_SWI"] # Patient
CON_K_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "K_SWI"] # Controls

RSDT(case_a = DF_V_SWI, case_b = DF_K_SWI, controls_a = CON_V_SWI, controls_b = CON_K_SWI)

## ----BSDT---------------------------------------------------------------------

# Extracting scores from the visual size-weight illusion from size_weight_illusion 
DF_V_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "V_SWI"] # Patient
CON_V_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "V_SWI"] # Controls

# Extracting scores from the kinesthetic size-weight illusion from size_weight_illusion 
DF_K_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "K_SWI"] # Patient
CON_K_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "K_SWI"] # Controls

BSDT(case_a = DF_V_SWI, case_b = DF_K_SWI, controls_a = CON_V_SWI, controls_b = CON_K_SWI, iter = 1000)

## ----BSDT_cov-----------------------------------------------------------------
# Extracting scores from the visual size-weight illusion from size_weight_illusion 
DF_V_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "V_SWI"] # Patient
CON_V_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "V_SWI"] # Controls

DF_K_SWI <- size_weight_illusion[size_weight_illusion$PPT == "DF", "K_SWI"] # Patient
CON_K_SWI <- size_weight_illusion[size_weight_illusion$PPT != "DF", "K_SWI"] # Controls

# Extracting the coviariate below
DF_age <- size_weight_illusion[size_weight_illusion$PPT == "DF", "YRS"] # Patient
CON_age <- size_weight_illusion[size_weight_illusion$PPT != "DF", "YRS"] # Controls

BSDT_cov(case_tasks = c(DF_V_SWI, DF_K_SWI ), case_covar = DF_age,
         control_tasks = cbind(CON_V_SWI, CON_K_SWI), control_covar = CON_age, iter = 1000)

## ----power--------------------------------------------------------------------
TD_power(case = -2, power = 0.8, mean = 0, sd = 1, alternative = "two.sided")

TD_power(case = 70, sample_size = 10, mean = 100, sd = 15, alternative = "less", alpha = 0.1)

RSDT_power(case_a = 70, case_b = 20, mean_a = 100, mean_b = 25, sd_a = 15, sd_b = 10, sample_size = 10) 

# Takes long time to compute therefore iterations and number of simulations are low. Iter corresponds
# to number of simulations in BTD_cov, nsim to the number of simulations in the power calculator.
BTD_cov_power(case = -2, case_cov = 0, control_task = c(0, 1),
             control_covar = c(0, 1), cor_mat = diag(2), sample_size = 10, nsim = 50, iter = 50)



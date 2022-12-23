###############################################################################
###############################################################################
################## Random-Intercept Cross-Lagged Panel Model ##################
###############################################################################
###############################################################################

### This script for a cross-lagged panel model is written for Anja Schoots from
# Leiden University for her research on student agency by Toby Hackmann and 
# Mike Rijnders

## In this script we will run the different nested cross-lagged panel models
## and inspect the fits to see which model we should use.

# First, open the data
require(readxl)
require(lavaan)
require(psych)


setwd("C:/Users/hackmannt/OneDrive - Universiteit Leiden/Documents OneDrive/T Studie/Statistical Science/Statistical Consulting/2. Data")
#data <- na.omit(read_xlsx("sim1.xlsx", col_names=TRUE))
#data <- read.csv("sim2.csv", header=TRUE, sep = ",", dec = ".", row.names = NULL)
load("simData.Rdata")
data = AllData
rm(AllData)

## Then, you need to set the working directory to the one containing the scripts
setwd("C:/Users/hackmannt/OneDrive - Universiteit Leiden/Documents OneDrive/T Studie/Statistical Science/Statistical Consulting/6. R scripts/CLPM model selection")


# Then transform the data from the long format into wide format
source("long_to_wide.R")


# Now we run the four models
source("CLPM_config.R")
source("CLPM_metric.R")
source("CLPM_scalar.R")
source("CLPM_residual.R")

# Fit the models
configural.fit <- cfa(model.config, data = data[, -1], 
                  estimator = "MLR", se = "robust", 
                  missing = "ML",std.lv = TRUE, optim.dx.tol=0.01)
metric.fit <- cfa(model.metric, data = data[, -1], 
                  estimator = "MLR", se = "robust", 
                  missing = "ML",std.lv = TRUE, optim.dx.tol=0.01)
scalar.fit <- cfa(model.scalar, data = data[, -1], 
                  estimator = "MLR", se = "robust", 
                  missing = "ML",std.lv = TRUE, optim.dx.tol=0.01)
residual.fit <- cfa(model.residual, data = data[, -1], 
                  estimator = "MLR", se = "robust", 
                  missing = "ML",std.lv = TRUE, optim.dx.tol=0.01)


# Save the models
setwd("C:/Users/hackmannt/OneDrive - Universiteit Leiden/Documents OneDrive/T Studie/Statistical Science/Statistical Consulting/8. Fitted models")
save(configural.fit, file = "fit_config_sampleVary.Rdata")
save(metric.fit, file = "fit_metric_sampleVary.Rdata")
save(scalar.fit, file = "fit_scalar_sampleVary.Rdata")
save(residual.fit, file = "fit_residual_sampleVary.Rdata")

# Alternatively, we load the models
setwd("C:/Users/hackmannt/OneDrive - Universiteit Leiden/Documents OneDrive/T Studie/Statistical Science/Statistical Consulting/8. Fitted models")
load("fit_config_sampleAll.Rdata")
load("fit_metric_sampleSim.Rdata")
load("fit_scalar_sampleSim.Rdata")
load("fit_residual_sampleSim.Rdata")


# Now we have ran the four models, we look at their fits and select
model.comparison = round( cbind ( configural = inspect( configural.fit, 'fit.measures' ), 
               metric = inspect( metric.fit, 'fit.measures' ), 
               scalar = inspect( scalar.fit, 'fit.measures' ),
               residual = inspect( residual.fit, 'fit.measures') ), 3 )


################################################################################
## Find the table of important fit measures
d_chisq = model.comparison["chisq.scaled", 2:4]-model.comparison["chisq.scaled", 1:3]
df = model.comparison["df.scaled", ]
d_df = df[2:4]-df[1:3]
scale = model.comparison["chisq.scaling.factor", ]
cd = (df[2:4]*scale[2:4]-df[1:3]*scale[1:3])/d_df
chisq.T = d_chisq/cd
p = dchisq(chisq.T, df=d_df)

fit.decisions <- rbind(model.comparison["npar", ], c('-',d_chisq), c('-', p), model.comparison["cfi.robust",],
                       c('-', model.comparison["cfi.robust",2:4]-model.comparison["cfi.robust",1:3]),
                       c('-',model.comparison["bic", 2:4]-model.comparison["bic", 1:3]),
                       c('-', model.comparison["aic", 2:4]-model.comparison["aic", 1:3])
                       )
colnames(fit.decisions) = c("Configural", "Metric", "Scalar", "Residual")
rownames(fit.decisions) = c("#Parameters", "\U0394\U03C7\U00B2", "p-value", "Robust CFI", "\U0394 CFI", "\U0394 BIC", "\U0394 AIC")

rm(d_chisq, df, d_df, scale, cd, chisq.T, p)

fit.decisions
save(fit.decisions, file="sampleSim_fit_metrics.Rdata")

load("sampleSim_fit_metrics.Rdata")


################################################################################
## Determine which model we choose for client
chisq_options = which( as.double(fit.decisions[3, 2:4]) > 0.05 )
if (length(chisq_options) > 0){
  chisq_choice = chisq_options[length(chisq_options)]
} else {
  chisq_choice = 1
}
cfi_options = which( as.double(fit.decisions[5, 2:4]) < -0.01 )
if (length(cfi_options) > 0){
  cfi_choice = cfi_options[1]
} else {
  cfi_choice = 4
}
aic_options = which( as.double(fit.decisions[7, 2:4]) > 0 )
if (length(aic_options) > 0){
  aic_choice = aic_options[1]
} else {
  aic_choice = 4
}
bic_options = which( as.double(fit.decisions[6, 2:4]) > 0 )
if (length(bic_options) > 0){
  bic_choice = bic_options[1]
} else {
  bic_choice = 4
}
choices = rbind(chisq_choice, cfi_choice, aic_choice, bic_choice)
colnames(choices) = "Index"
rownames(choices) = c("ChiSq test", "CFI", "AIC", "BIC")
choices

counts = c(sum(choices == 1), sum(choices == 2), sum(choices == 3), sum(choices == 4))
if (length(max(counts))==1){
  index = which.max(counts)
  print(paste("The model of choice according to the largest number of fit metrics is the ", colnames(fit.decisions)[index], " model"))
} else {
  index = counts[4]
  print(paste("Since the fit measures are tied, we choose the one which is best according to the BIC for most parsimony, 
              which is the ", colnames(fit.decisions)[index], " model"))
}



source("RICLPM.R")
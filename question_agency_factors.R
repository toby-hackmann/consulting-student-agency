require(readxl)
require(plyr)
require(psych)
require(paran)
require(GPArotation)

# Load the data into R using the read.table function
setwd("C:/Users/hackmannt/OneDrive - Universiteit Leiden/Documents OneDrive/T Studie/Statistical Science/Statistical Consulting/1. Raw Data")
data <- na.omit(read_xlsx("raw_data.xlsx", col_names=TRUE))

# If we want to save anything, we need to put in in another folder, set it now
setwd("C:/Users/hackmannt/OneDrive - Universiteit Leiden/Documents OneDrive/T Studie/Statistical Science/Statistical Consulting/2. Data")

# For this script we are interested in all the answers to the questions
# that concern student agency. We look at the names to see what we need
names(data)

# The questions on the agency factors are variables 6 through 74
# We use na.omit to remove the last rows that contain NA's
agency_data <- na.omit(data[, 6:74])
new_labels <- c("VM1", "VM2", "VM3", "VM4", "VM5", "VM6", "VM7", "VM8",
                "VM9", "VM10", "VM11", "SE1", "SE2", "SE3", "SE4", "SE5",
                "SE6", "SE7", "SE8", "SE9", "SE10", "SE11", "SE12",
                "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8",
                "F1", "F2", "F3", "F4", "F5", "F6", "F7", "SR1", "SR2",
                "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", "SR9", "SR10",
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8",
                "SRf1", "SRf2", "SRf3", "SRf4", "SRf5", "SRf6", "SRf7",
                "SRf8", "SRf9", "SRf10", "SRf11", "SRf12", "SRf13")
colnames(agency_data) <- new_labels
first_two <- c(1, 2, 12, 13, 24, 25, 32, 33, 39, 40, 49, 50, 57, 58)

# Now we have the raw data on the 69 questions concerning agency

## We justify the use of these variables to look for underlying
# factors concerning agency because of their relation to the issue
# All questions have to do with agency and there are no other
# variables taken into account. W
summary(agency_data)

# There is no missing data. The data is all ordinal answers to 
# survey questions, with 7 levels. One neutral, 3 disagreement
# levels and 3 agreement levels

## Plot all histograms
multi.hist(agency_data)
# The histograms seem decently symmetrical and all follow as closely
# to normal distribution as you can expect. Since there are not more
# than 7 categories and they are not wildly asymetrical, we can use
# factor analysis on this data.

# Now we look at the correlation matrix to see if the data is
# sufficiently correlated to use EFA
data_correlation = cor(agency_data)
# There seem to be a lot of correlations over 0.3, indicating
# that we can do EFA, now do Bartletts sphericity test:
cortest.bartlett(agency_data)
# With a chi-sq value of 32552, this is massively significant
# KMO test should give values over 0.7 and over 0.9 is 'marvelous'
KMO(agency_data)
# With only 2 KMO values under 0.9 (in the 0.8s) this is also good

# First we decide on the number of factors. There are two main 
# subjective choices: 1 factor to account for all agency, two
# factors to account for agency factors and features or seven
# factors to account for all found in literature:

# First look at PCA eigenvalues
pca <- prcomp(agency_data)
plot(pca$sdev[1:20], type = 'b')
# The scree plot doesn't tell much, the first component is the
# largest, and there is no real elbow after that, probably 1?

# Parallel analysis
fa_paran <- paran(agency_data, graph=TRUE, cfa = TRUE)


# Parallel analysis tells us to use 7 dimensions
# but the last 3 are below random eigenvalues,
# so 4 also seems usefull

# We will use a common factor analysis model instead of PCA
# using the maximum likelihood methodology. We will use an
# an orthogonal rotation method.
# We use the found 7 and 4 factors that paran advised
#fa_13 <- fa(agency_data, nfactors = 13, rotate = "promax")
#fa_7 <- fa(agency_data, nfactors = 7, rotate = "promax")
fa_4 <- fa(agency_data, nfactors = 4, rotate = "promax")
#fa_1 <- fa(agency_data, nfactors = 1, rotate = "varimax")

#fa_13
#fa_7
fa_4
#fa_1

#summary(fa_1)
summary(fa_4)

#fa_4_names <- c("Action", "Reflection", "Consciousness", "Value/Intention")

#load_7 <- data.frame(matrix(as.numeric(fa_7$loadings), 69, dimnames=attributes(fa_7[["loadings"]])$dimnames))
load_4 <- data.frame(matrix(as.numeric(fa_4$loadings), 69, dimnames=attributes(fa_4[["loadings"]])$dimnames))
#load_1 <- data.frame(matrix(as.numeric(fa_1$loadings), 69, dimnames=attributes(fa_1[["loadings"]])$dimnames))

biplot(fa_4, choose = c(1,2))
biplot(fa_4, choose = c(1,3))
biplot(fa_4, choose = c(1,4))
 
F1 <-load_4[load_4$MR2 > 0.60, ]
F2 <-load_4[load_4$MR1 > 0.60, ]
F3 <-load_4[load_4$MR3 > 0.60, ]
F4 <-load_4[load_4$MR4 > 0.60, ]



#biplot(fa_7, choose = c(1,2))
#biplot(fa_7, choose = c(1,3))
#biplot(fa_7, choose = c(1,4))
#biplot(fa_7, choose = c(1,5))
#biplot(fa_7, choose = c(1,6))
#biplot(fa_7, choose = c(1,7))

#biplot(fa_13, choose = c(1,12))


# From the biplots it also looks like 4 factors is the best.
# The first factor can mostly be considered to be about both
# the factor self-efficacy and feature self-reactiveness, 
# these appear closely related. The second factor is mostly about
# self-reflectiveness, adjusting the plan if it is not going well.
# A middle ground between these is forethought, which is about 
# formulating the plans that you want to execute and probably
# also helps in reflection. The third factor is mostly about
# intentionality and values/morals. If one feels a moral obligation
# towards agency, the intention appears to follow that. 
# The fourth factor is all about consciousness

# With 7 factors we don't gain any clarity, the last three factors
# contain noise between questions on the same topic, no real answers.
# They definitely don't split the 7 categories neatly.

# Using only 1 factor is acceptable according to the test
# it doesn't explain many of the differences, but explains about
# 36% of the variance, which is decent, but is misses the subtleties.

#new_var <- factor.scores(agency_data, fa_4)
#data <- cbind.data.frame(data, new_var$scores)
#colnames(data)[100:103] <- c("Action", "Reflection", "Consciousness", "Value/Intention")

#low_action <- data[data["Action"]<(-2.45), ]
#means <- colMeans(low_action)[87:103]


cor.mat <- cor(load_4)
eigen(cor.mat)

###############################################################################
###############################################################################
################## Random-Intercept Cross-Lagged Panel Model ##################
###############################################################################
###############################################################################

### This script for a cross-lagged panel model is written for Anja Schoots from
# Leiden University for her research on student agency by Toby Hackmann and 
# Mike Rijnders

## Load required packages
require(lavaan)
require(psych)


#new_labels <- c("VM1", "VM2", "VM3", "VM4", "VM5", 
#                "SE1", "SE2", "SE3", "SE4", "SE5", "SE6", 
#                "I1", "I2", "I3", "I4", "I5", "I6", 
#                "SR1", "SR2", "SR3", "SR4", 
#                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8",
#                "SRf1", "SRf2", "SRf3", "SRf4", "SRf5", "SRf6", "SRf7",
#                "SRf8", "SRf9", "SRf10", "SRf11",
#                "IB1", "IB2", "IB3", 
#                "WB1", "WB2", "WB3", "WB4", "WB5", "WB6", "WB7", "WB8", 
#                "Cijfers", "Tekort")


## Build CLPM
riclpm.1 <- '
            ########################## RI_CLPM MODEL ###########################
            # This is the second model, testing for weak measurement invariance
            # in the cross-lagged panel model
            
            
            
            
            ####################################################################
            ####################################################################
            ######################## MEASUREMENT MODEL #########################
            ####################################################################
            ####################################################################
            
            ######################## LATENT VARIABLES ##########################            
            # First we define the latent variables, the first digit is the
            # question number, the digit after the period is the time.
            # "=~" means "latent variable is measured by", and "NA*" before the
            # first variable means that the loading of the first is not fixed
            # to 1, as is normally the case
            # Latent Variable 1: Effectiveness (E)
            # Latent Variable 2: Reflectiveness (R)
            # Latent Variable 3: Consciousness (C)
            # Latent Variable 4: Intention (I)
            # Latent Variable 5: Ill-being (IB)
            # Latent Variable 6: Well-being (WB)
            
            ## This is the metric model, so we add labels that fix variable
            ## loadings at different times. These are the "se1l" labels, which
            ## stand for the question code, the question number and the "l"
            ## for "loadings"
            
            # Time 1
            E.1  =~ NA*SE1.1 + SE2.1 + SE3.1 + SE4.1 + SE5.1 + SE6.1 
                    + SR1.1 + SR2.1 + SR3.1 + SR4.1
            R.1  =~ NA*SRf1.1 + SRf2.1 + SRf3.1 + SRf4.1 + SRf5.1 + SRf6.1
                    + SRf7.1 + SRf8.1 + SRf9.1 + SRf10.1 + SRf11.1
            C.1  =~ NA*C1.1 + C2.1 + C3.1 + C4.1 + C5.1 + C6.1 + C7.1 + C8.1
            I.1  =~ NA*VM1.1 + VM2.1 + VM3.1 + VM4.1 + VM5.1 
                    + I1.1 + I2.1 + I3.1 + I4.1 + I5.1 + I6.1
            IB.1 =~ NA*IB1.1 + IB2.1 + IB3.1
            WB.1 =~ NA*WB1.1 + WB2.1 + WB3.1 + WB4.1 + WB5.1 + WB6.1 
                    + WB7.1 + WB8.1
            Grades.1 =~ NA*Cijfers.1

            # Time 2
            E.2  =~ NA*SE1.2 + SE2.2 + SE3.2 + SE4.2 + SE5.2 + SE6.2 
                    + SR1.2 + SR2.2 + SR3.2 + SR4.2
            R.2  =~ NA*SRf1.2 + SRf2.2 + SRf3.2 + SRf4.2 + SRf5.2 + SRf6.2
                    + SRf7.2 + SRf8.2 + SRf9.2 + SRf10.2 + SRf11.2
            C.2  =~ NA*C1.2 + C2.2 + C3.2 + C4.2 + C5.2 + C6.2 + C7.2 + C8.2
            I.2  =~ NA*VM1.2 + VM2.2 + VM3.2 + VM4.2 + VM5.2 
                    + I1.2 + I2.2 + I3.2 + I4.2 + I5.2 + I6.2
            IB.2 =~ NA*IB1.2 + IB2.2 + IB3.2
            WB.2 =~ NA*WB1.2 + WB2.2 + WB3.2 + WB4.2 + WB5.2 + WB6.2 
                    + WB7.2 + WB8.2
            Grades.2 =~ NA*Cijfers.2

            # Time 3
            E.3  =~ NA*SE1.3 + SE2.3 + SE3.3 + SE4.3 + SE5.3 + SE6.3 
                    + SR1.3 + SR2.3 + SR3.3 + SR4.3
            R.3  =~ NA*SRf1.3 + SRf2.3 + SRf3.3 + SRf4.3 + SRf5.3 + SRf6.3
                    + SRf7.3 + SRf8.3 + SRf9.3 + SRf10.3 + SRf11.3
            C.3  =~ NA*C1.3 + C2.3 + C3.3 + C4.3 + C5.3 + C6.3 + C7.3 + C8.3
            I.3  =~ NA*VM1.3 + VM2.3 + VM3.3 + VM4.3 + VM5.3 
                    + I1.3 + I2.3 + I3.3 + I4.3 + I5.3 + I6.3
            IB.3 =~ NA*IB1.3 + IB2.3 + IB3.3
            WB.3 =~ NA*WB1.3 + WB2.3 + WB3.3 + WB4.3 + WB5.3 + WB6.3 
                    + WB7.3 + WB8.3

            Grades.3 =~ NA*Cijfers.3
            
            
            ######################### INTERCEPTS ###############################
            # Next we will add which regressed variables have an intercept.
            # "~ 1" means "has an intercept" and the labels befor the 1 mean the
            # set intercept label for a variable.
            
            
            # Free latent means from t = 2 onward (only do this in combination with 
            # constraints on intercepts; without these, this would not be identified)
            E.2 + E.3 + R.2 + R.3 + C.2 + C.3 + I.2 + I.3 + IB.2 + IB.3 + WB.2 + WB.3 + Grades.2 + Grades.3 ~ 1
            
            
            ####################################################################
            ####################################################################
            ########################### BETWEEN PART ###########################
            ####################################################################
            ####################################################################
            # These are the random intercepts, we use one for the agency 
            # variables, one for the well-being and one for the performance.
            
            RI.E1 =~  1*SE1.1 + 1*SE1.2 + 1*SE1.3
            RI.E2 =~  1*SE2.1 + 1*SE2.2 + 1*SE2.3
            RI.E3 =~  1*SE3.1 + 1*SE3.2 + 1*SE3.3
            RI.E4 =~  1*SE4.1 + 1*SE4.2 + 1*SE4.3
            RI.E5 =~  1*SE5.1 + 1*SE5.2 + 1*SE5.3
            RI.E6 =~  1*SE6.1 + 1*SE6.2 + 1*SE6.3
            RI.E7 =~  1*SR1.1 + 1*SR1.2 + 1*SR1.3
            RI.E8 =~  1*SR2.1 + 1*SR2.2 + 1*SR2.3
            RI.E9 =~  1*SR3.1 + 1*SR3.2 + 1*SR3.3
            RI.E10 =~ 1*SR4.1 + 1*SR4.2 + 1*SR4.3
            
            RI.R1 =~  1*SRf1.1 + 1*SRf1.2 + 1*SRf1.3
            RI.R2 =~  1*SRf2.1 + 1*SRf2.2 + 1*SRf2.3
            RI.R3 =~  1*SRf3.1 + 1*SRf3.2 + 1*SRf3.3
            RI.R4 =~  1*SRf4.1 + 1*SRf4.2 + 1*SRf4.3
            RI.R5 =~  1*SRf5.1 + 1*SRf5.2 + 1*SRf5.3
            RI.R6 =~  1*SRf6.1 + 1*SRf6.2 + 1*SRf6.3
            RI.R7 =~  1*SRf7.1 + 1*SRf7.2 + 1*SRf7.3
            RI.R8 =~  1*SRf8.1 + 1*SRf8.2 + 1*SRf8.3
            RI.R9 =~  1*SRf9.1 + 1*SRf9.2 + 1*SRf9.3
            RI.R10 =~  1*SRf10.1 + 1*SRf10.2 + 1*SRf10.3
            RI.R11 =~  1*SRf11.1 + 1*SRf11.2 + 1*SRf11.3
            
            RI.C1 =~  1*C1.1 + 1*C1.2 + 1*C1.3
            RI.C2 =~  1*C2.1 + 1*C2.2 + 1*C2.3
            RI.C3 =~  1*C3.1 + 1*C3.2 + 1*C3.3
            RI.C4 =~  1*C4.1 + 1*C4.2 + 1*C4.3
            RI.C5 =~  1*C5.1 + 1*C5.2 + 1*C5.3
            RI.C6 =~  1*C6.1 + 1*C6.2 + 1*C6.3
            RI.C7 =~  1*C7.1 + 1*C7.2 + 1*C7.3
            RI.C8 =~  1*C8.1 + 1*C8.2 + 1*C8.3
            
            RI.I1 =~  1*VM1.1 + 1*VM1.2 + 1*VM1.3
            RI.I2 =~  1*VM2.1 + 1*VM2.2 + 1*VM2.3
            RI.I3 =~  1*VM3.1 + 1*VM3.2 + 1*VM3.3
            RI.I4 =~  1*VM4.1 + 1*VM4.2 + 1*VM4.3
            RI.I5 =~  1*VM5.1 + 1*VM5.2 + 1*VM5.3
            RI.I6 =~  1*I1.1 + 1*I1.2 + 1*I1.3
            RI.I7 =~  1*I2.1 + 1*I2.2 + 1*I2.3
            RI.I8 =~  1*I3.1 + 1*I3.2 + 1*I3.3
            RI.I9 =~  1*I4.1 + 1*I4.2 + 1*I4.3
            RI.I10 =~  1*I5.1 + 1*I5.2 + 1*I5.3
            RI.I11 =~  1*I6.1 + 1*I6.2 + 1*I6.3
            
            RI.IB1 =~ 1*IB1.1 + 1*IB1.2 + 1*IB1.3
            RI.IB2 =~ 1*IB2.1 + 1*IB2.2 + 1*IB2.3
            RI.IB3 =~ 1*IB3.1 + 1*IB3.2 + 1*IB3.3
            
            RI.WB1 =~  1*WB1.1 + 1*WB1.2 + 1*WB1.3
            RI.WB2 =~  1*WB2.1 + 1*WB2.2 + 1*WB2.3
            RI.WB3 =~  1*WB3.1 + 1*WB3.2 + 1*WB3.3
            RI.WB4 =~  1*WB4.1 + 1*WB4.2 + 1*WB4.3
            RI.WB5 =~  1*WB5.1 + 1*WB5.2 + 1*WB5.3
            RI.WB6 =~  1*WB6.1 + 1*WB6.2 + 1*WB6.3
            RI.WB7 =~  1*WB7.1 + 1*WB7.2 + 1*WB7.3
            RI.WB8 =~  1*WB8.1 + 1*WB8.2 + 1*WB8.3
            
            RI.P =~   1*Grades.1 + 1*Grades.2 + 1*Grades.3 
            
            
            ####################################################################
            ####################################################################
            ###################### Additional Constraints ######################
            ####################################################################
            ####################################################################
            # Set correlations between the random intercepts and within-variables
            # at the first time point to 0
            RI.E1 + RI.E2 + RI.E3 + RI.E4 + RI.E5 + RI.E6 + RI.E7 + RI.E8 + RI.E9 + RI.E10 ~~ 0*E.1 + 0*R.1 + 0*C.1 + 0*I.1 + 0*IB.1 + 0*WB.1 + 0*Grades.1
            RI.R1 + RI.R2 + RI.R3 + RI.R4 + RI.R5 + RI.R6 + RI.R7 + RI.R8 + RI.R9 + RI.R10 + RI.R11 ~~ 0*E.1 + 0*R.1 + 0*C.1 + 0*I.1 + 0*IB.1 + 0*WB.1 + 0*Grades.1
            RI.C1 + RI.C2 + RI.C3 + RI.C4 + RI.C5 + RI.C6 + RI.C7 + RI.C8 ~~ 0*E.1 + 0*R.1 + 0*C.1 + 0*I.1 + 0*IB.1 + 0*WB.1 + 0*Grades.1
            RI.I1 + RI.I2 + RI.I3 + RI.I4 + RI.I5 + RI.I6 + RI.I7 + RI.I8 + RI.I9 + RI.I10 + RI.I11 ~~ 0*E.1 + 0*R.1 + 0*C.1 + 0*I.1 + 0*IB.1 + 0*WB.1 + 0*Grades.1
            RI.IB1 + RI.IB2 + RI.IB3 ~~ 0*E.1 + 0*R.1 + 0*C.1 + 0*I.1 + 0*IB.1 + 0*WB.1 + 0*Grades.1
            RI.WB1 + RI.WB2 + RI.WB3 + RI.WB4 + RI.WB5 + RI.WB6 + RI.WB7 + RI.WB8 ~~ 0*E.1 + 0*R.1 + 0*C.1 + 0*I.1 + 0*IB.1 + 0*WB.1 + 0*Grades.1
            RI.P ~~ 0*E.1 + 0*R.1 + 0*C.1 + 0*I.1 + 0*IB.1 + 0*WB.1 + 0*Grades.1


'
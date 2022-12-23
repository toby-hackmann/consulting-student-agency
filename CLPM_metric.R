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
require(MBESS)


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
model.metric <- '
            ########################## METRIC MODEL ############################
            # This is the second model used to test for the level of constraint
            # where we add constraints to the factor loadings. This means that 
            # the effect of a question on the latent variable will now be the 
            # same at time 1, 2 and 3. This adds an additional restriction, but
            # a very logical one. This reduces the number of parameters, but 
            # makes for a more interpretable solution.


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
            E.1  =~ se1l*SE1.1 + se2l*SE2.1 + se3l*SE3.1 + se4l*SE4.1 
                    + se5l*SE5.1 + se6l*SE6.1 + sr1l*SR1.1 + sr2l*SR2.1 
                    + sr3l*SR3.1 + sr4l*SR4.1
            R.1  =~ srf1l*SRf1.1 + srf2l*SRf2.1 + srf3l*SRf3.1 + srf4l*SRf4.1 
                    + srf5l*SRf5.1 + srf6l*SRf6.1 + srf7l*SRf7.1 + srf8l*SRf8.1 
                    + srf9l*SRf9.1 + srf10l*SRf10.1 + srf11l*SRf11.1
            C.1  =~ c1l*C1.1 + c2l*C2.1 + c3l*C3.1 + c4l*C4.1 + c5l*C5.1 
                    + c6l*C6.1 + c7l*C7.1 + c8l*C8.1
            I.1  =~ vm1l*VM1.1 + vm2l*VM2.1 + vm3l*VM3.1 + vm4l*VM4.1 
                    + vm5l*VM5.1 + i1l*I1.1 + i2l*I2.1 + i3l*I3.1 + i4l*I4.1 
                    + i5l*I5.1 + i6l*I6.1
            IB.1 =~ ib1l*IB1.1 + ib2l*IB2.1 + ibl3*IB3.1
            WB.1 =~ wb1l*WB1.1 + wb2l*WB2.1 + wb3l*WB3.1 + wb4l*WB4.1 
                    + wb5l*WB5.1 + wb6l*WB6.1 + wb7l*WB7.1 + wb8l*WB8.1
            Grades.1 =~ cijfl*Cijfers.1

            # Time 2
            E.2  =~ se1l*SE1.2 + se2l*SE2.2 + se3l*SE3.2 + se4l*SE4.2 
                    + se5l*SE5.2 + se6l*SE6.2 + sr1l*SR1.2 + sr2l*SR2.2 
                    + sr3l*SR3.2 + sr4l*SR4.2
            R.2  =~ srf1l*SRf1.2 + srf2l*SRf2.2 + srf3l*SRf3.2 + srf4l*SRf4.2 
                    + srf5l*SRf5.2 + srf6l*SRf6.2 + srf7l*SRf7.2 + srf8l*SRf8.2 
                    + srf9l*SRf9.2 + srf10l*SRf10.2 + srf11l*SRf11.2
            C.2  =~ c1l*C1.2 + c2l*C2.2 + c3l*C3.2 + c4l*C4.2 + c5l*C5.2 
                    + c6l*C6.2 + c7l*C7.2 + c8l*C8.2
            I.2  =~ vm1l*VM1.2 + vm2l*VM2.2 + vm3l*VM3.2 + vm4l*VM4.2 
                    + vm5l*VM5.2 + i1l*I1.2 + i2l*I2.2 + i3l*I3.2 + i4l*I4.2 
                    + i5l*I5.2 + i6l*I6.2
            IB.2 =~ ib1l*IB1.2 + ib2l*IB2.2 + ibl3*IB3.2
            WB.2 =~ wb1l*WB1.2 + wb2l*WB2.2 + wb3l*WB3.2 + wb4l*WB4.2 
                    + wb5l*WB5.2 + wb6l*WB6.2 + wb7l*WB7.2 + wb8l*WB8.2
            Grades.2 =~ cijfl*Cijfers.2

            # Time 3
            E.3  =~ se1l*SE1.3 + se2l*SE2.3 + se3l*SE3.3 + se4l*SE4.3 
                    + se5l*SE5.3 + se6l*SE6.3 + sr1l*SR1.3 + sr2l*SR2.3 
                    + sr3l*SR3.3 + sr4l*SR4.3
            R.3  =~ srf1l*SRf1.3 + srf2l*SRf2.3 + srf3l*SRf3.3 + srf4l*SRf4.3 
                    + srf5l*SRf5.3 + srf6l*SRf6.3 + srf7l*SRf7.3 + srf8l*SRf8.3 
                    + srf9l*SRf9.3 + srf10l*SRf10.3 + srf11l*SRf11.3
            C.3  =~ c1l*C1.3 + c2l*C2.3 + c3l*C3.3 + c4l*C4.3 + c5l*C5.3 
                    + c6l*C6.3 + c7l*C7.3 + c8l*C8.3
            I.3  =~ vm1l*VM1.3 + vm2l*VM2.3 + vm3l*VM3.3 + vm4l*VM4.3 
                    + vm5l*VM5.3 + i1l*I1.3 + i2l*I2.3 + i3l*I3.3 + i4l*I4.3 
                    + i5l*I5.3 + i6l*I6.3
            IB.3 =~ ib1l*IB1.3 + ib2l*IB2.3 + ibl3*IB3.3
            WB.3 =~ wb1l*WB1.3 + wb2l*WB2.3 + wb3l*WB3.3 + wb4l*WB4.3 
                    + wb5l*WB5.3 + wb6l*WB6.3 + wb7l*WB7.3 + wb8l*WB8.3
            Grades.3 =~ cijfl*Cijfers.3





            ########################## REGRESSIONS #############################
            # Now we will define all the regressions in the model that will be 
            # checked. "~" means "is regressed on".



            ######################### INTERCEPTS ###############################
            # Next we will add which regressed variables have an intercept.
            # "~ 1" means "has an intercept"

            VM1.1 ~1
            VM1.2 ~1
            VM1.3 ~1
            VM2.1 ~1
            VM2.2 ~1
            VM2.3 ~1
            VM3.1 ~1
            VM3.2 ~1
            VM3.3 ~1
            VM4.1 ~1
            VM4.2 ~1
            VM4.3 ~1
            VM5.1 ~1
            VM5.2 ~1
            VM5.3 ~1
            
            SE1.1 ~1
            SE1.2 ~1
            SE1.3 ~1
            SE2.1 ~1
            SE2.2 ~1
            SE2.3 ~1
            SE3.1 ~1
            SE3.2 ~1
            SE3.3 ~1
            SE4.1 ~1
            SE4.2 ~1
            SE4.3 ~1
            SE5.1 ~1
            SE5.2 ~1
            SE5.3 ~1
            SE6.1 ~1
            SE6.2 ~1
            SE6.3 ~1
            
            I1.1 ~1
            I1.2 ~1
            I1.3 ~1
            I2.1 ~1
            I2.2 ~1
            I2.3 ~1
            I3.1 ~1
            I3.2 ~1
            I3.3 ~1
            I4.1 ~1
            I4.2 ~1
            I4.3 ~1
            I5.1 ~1
            I5.2 ~1
            I5.3 ~1
            I6.1 ~1
            I6.2 ~1
            I6.3 ~1
            
            SR1.1 ~1
            SR1.2 ~1
            SR1.3 ~1
            SR2.1 ~1
            SR2.2 ~1
            SR2.3 ~1
            SR3.1 ~1
            SR3.2 ~1
            SR3.3 ~1
            SR4.1 ~1
            SR4.2 ~1
            SR4.3 ~1
            
            C1.1 ~1
            C1.2 ~1
            C1.3 ~1
            C2.1 ~1
            C2.2 ~1
            C2.3 ~1
            C3.1 ~1
            C3.2 ~1
            C3.3 ~1
            C4.1 ~1
            C4.2 ~1
            C4.3 ~1
            C5.1 ~1
            C5.2 ~1
            C5.3 ~1
            C6.1 ~1
            C6.2 ~1
            C6.3 ~1
            C7.1 ~1
            C7.2 ~1
            C7.3 ~1
            C8.1 ~1
            C8.2 ~1
            C8.3 ~1
            
            SRf1.1 ~1
            SRf1.2 ~1
            SRf1.3 ~1
            SRf2.1 ~1
            SRf2.2 ~1
            SRf2.3 ~1
            SRf3.1 ~1
            SRf3.2 ~1
            SRf3.3 ~1
            SRf4.1 ~1
            SRf4.2 ~1
            SRf4.3 ~1
            SRf5.1 ~1
            SRf5.2 ~1
            SRf5.3 ~1
            SRf6.1 ~1
            SRf6.2 ~1
            SRf6.3 ~1
            SRf7.1 ~1
            SRf7.2 ~1
            SRf7.3 ~1
            SRf8.1 ~1
            SRf8.2 ~1
            SRf8.3 ~1
            SRf9.1 ~1
            SRf9.2 ~1
            SRf9.3 ~1
            SRf10.1 ~1
            SRf10.2 ~1
            SRf10.3 ~1
            SRf11.1 ~1
            SRf11.2 ~1
            SRf11.3 ~1
            
            IB1.1 ~1
            IB1.2 ~1
            IB1.3 ~1
            IB2.1 ~1
            IB2.2 ~1
            IB2.3 ~1
            IB3.1 ~1
            IB3.2 ~1
            IB3.3 ~1
            
            WB1.1 ~1
            WB1.2 ~1
            WB1.3 ~1
            WB2.1 ~1
            WB2.2 ~1
            WB2.3 ~1
            WB3.1 ~1
            WB3.2 ~1
            WB3.3 ~1
            WB4.1 ~1
            WB4.2 ~1
            WB4.3 ~1
            WB5.1 ~1
            WB5.2 ~1
            WB5.3 ~1
            WB6.1 ~1
            WB6.2 ~1
            WB6.3 ~1
            WB7.1 ~1
            WB7.2 ~1
            WB7.3 ~1
            WB8.1 ~1
            WB8.2 ~1
            WB8.3 ~1
            
            Cijfers.1 ~1
            Cijfers.2 ~1
            Cijfers.3 ~1



            #################### VARIANCES AND COVARIANCES #####################
            # Next we define the variances and covariances between different 
            # variables. "~~" means "is correlated with" and the "1*" means that
            # the correlation is fixed to 1, while "0*" fixes it to 0.
            # Using a label to fix correlation is done to fix correlations 
            # between each different time point measure to the same value
            
            ## First we fix the variances of all latent variables to 1
            E.1 ~~ 1*E.1
            E.2 ~~ 1*E.2
            E.3 ~~ 1*E.3
            R.1 ~~ 1*R.1
            R.2 ~~ 1*R.2
            R.3 ~~ 1*R.3
            C.1 ~~ 1*C.1
            C.2 ~~ 1*C.2
            C.3 ~~ 1*C.3
            I.1 ~~ 1*I.1
            I.2 ~~ 1*I.2
            I.3 ~~ 1*I.3
            IB.1 ~~ 1*IB.1
            IB.2 ~~ 1*IB.2
            IB.3 ~~ 1*IB.3
            WB.1 ~~ 1*WB.1
            WB.2 ~~ 1*WB.2
            WB.3 ~~ 1*WB.3

            Grades.1 ~~ 1*Grades.1
            Grades.2 ~~ 1*Grades.2
            Grades.3 ~~ 1*Grades.3
            

            ## Next we set labels to define the correlations between the same
            ## variable at different points in time, the "vm1cov" type things
            ## are labels that fix a value, in this case the covariance between
            ## a questions results between time 1 and 2 to time 2 and 3.
            VM1.1 ~~ vm1cov*VM1.2
            VM1.2 ~~ vm1cov*VM1.3
            VM2.1 ~~ vm2cov*VM2.2
            VM2.2 ~~ vm2cov*VM2.3
            VM3.1 ~~ vm3cov*VM3.2
            VM3.2 ~~ vm3cov*VM3.3
            VM4.1 ~~ vm4cov*VM4.2
            VM4.2 ~~ vm4cov*VM4.3
            VM5.1 ~~ vm5cov*VM5.2
            VM5.2 ~~ vm5cov*VM5.3
            
            SE1.1 ~~ se1cov*SE1.2
            SE1.2 ~~ se1cov*SE1.3
            SE2.1 ~~ se2cov*SE2.2
            SE2.2 ~~ se2cov*SE2.3
            SE3.1 ~~ se3cov*SE3.2
            SE3.2 ~~ se3cov*SE3.3
            SE4.1 ~~ se4cov*SE4.2
            SE4.2 ~~ se4cov*SE4.3
            SE5.1 ~~ se5cov*SE5.2
            SE5.2 ~~ se5cov*SE5.3
            SE6.1 ~~ se6cov*SE6.2
            SE6.2 ~~ se6cov*SE6.3
            
            I1.1 ~~ i1cov*I1.2
            I1.2 ~~ i1cov*I1.3
            I2.1 ~~ i2cov*I2.2
            I2.2 ~~ i2cov*I2.3
            I3.1 ~~ i3cov*I3.2
            I3.2 ~~ i3cov*I3.3
            I4.1 ~~ i4cov*I4.2
            I4.2 ~~ i4cov*I4.3
            I5.1 ~~ i5cov*I5.2
            I5.2 ~~ i5cov*I5.3
            I6.1 ~~ i6cov*I6.2
            I6.2 ~~ i6cov*I6.3
            
            SR1.1 ~~ sr1cov*SR1.2
            SR1.2 ~~ sr1cov*SR1.3
            SR2.1 ~~ sr2cov*SR2.2
            SR2.2 ~~ sr2cov*SR2.3
            SR3.1 ~~ sr3cov*SR3.2
            SR3.2 ~~ sr3cov*SR3.3
            SR4.1 ~~ sr4cov*SR4.2
            SR4.2 ~~ sr4cov*SR4.3
            
            C1.1 ~~ c1cov*C1.2
            C1.2 ~~ c1cov*C1.3
            C2.1 ~~ c2cov*C2.2
            C2.2 ~~ c2cov*C2.3
            C3.1 ~~ c3cov*C3.2
            C3.2 ~~ c3cov*C3.3
            C4.1 ~~ c4cov*C4.2
            C4.2 ~~ c4cov*C4.3
            C5.1 ~~ c5cov*C5.2
            C5.2 ~~ c5cov*C5.3
            C6.1 ~~ c6cov*C6.2
            C6.2 ~~ c6cov*C6.3
            C7.1 ~~ c7cov*C7.2
            C7.2 ~~ c7cov*C7.3
            C8.1 ~~ c8cov*C8.2
            C8.2 ~~ c8cov*C8.3
            
            SRf1.1 ~~ srf1cov*SRf1.2
            SRf1.2 ~~ srf1cov*SRf1.3
            SRf2.1 ~~ srf2cov*SRf2.2
            SRf2.2 ~~ srf2cov*SRf2.3
            SRf3.1 ~~ srf3cov*SRf3.2
            SRf3.2 ~~ srf3cov*SRf3.3
            SRf4.1 ~~ srf4cov*SRf4.2
            SRf4.2 ~~ srf4cov*SRf4.3
            SRf5.1 ~~ srf5cov*SRf5.2
            SRf5.2 ~~ srf5cov*SRf5.3
            SRf6.1 ~~ srf6cov*SRf6.2
            SRf6.2 ~~ srf6cov*SRf6.3
            SRf7.1 ~~ srf7cov*SRf7.2
            SRf7.2 ~~ srf7cov*SRf7.3
            SRf8.1 ~~ srf8cov*SRf8.2
            SRf8.2 ~~ srf8cov*SRf8.3
            SRf9.1 ~~ srf9cov*SRf9.2
            SRf9.2 ~~ srf9cov*SRf9.3
            SRf10.1 ~~ srf10cov*SRf10.2
            SRf10.2 ~~ srf10cov*SRf10.3
            SRf11.1 ~~ srf11cov*SRf11.2
            SRf11.2 ~~ srf11cov*SRf11.3
            
            IB1.1 ~~ ib1cov*IB1.2
            IB1.2 ~~ ib1cov*IB1.3
            IB2.1 ~~ ib2cov*IB2.2
            IB2.2 ~~ ib2cov*IB2.3
            IB3.1 ~~ ib3cov*IB3.2
            IB3.2 ~~ ib3cov*IB3.3
            
            WB1.1 ~~ wb1cov*WB1.2
            WB1.2 ~~ wb1cov*WB1.3
            WB2.1 ~~ wb2cov*WB2.2
            WB2.2 ~~ wb2cov*WB2.3
            WB3.1 ~~ wb3cov*WB3.2
            WB3.2 ~~ wb3cov*WB3.3
            WB4.1 ~~ wb4cov*WB4.2
            WB4.2 ~~ wb4cov*WB4.3
            WB5.1 ~~ wb5cov*WB5.2
            WB5.2 ~~ wb5cov*WB5.3
            WB6.1 ~~ wb6cov*WB6.2
            WB6.2 ~~ wb6cov*WB6.3
            WB7.1 ~~ wb7cov*WB7.2
            WB7.2 ~~ wb7cov*WB7.3
            WB8.1 ~~ wb8cov*WB8.2
            WB8.2 ~~ wb8cov*WB8.3

            Cijfers.1 ~~ cijfcov*Cijfers.2
            Cijfers.2 ~~ cijfcov*Cijfers.3

            '

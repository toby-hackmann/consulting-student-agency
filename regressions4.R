regressions4 <- '
            ########################## REGRESSIONS #############################
            # Now we will define all the regressions in the model that will be 
            # checked. "~" means "is regressed on".
            
            WE.1 + WR.1 + WC.1 + WI.1 + WIB.1 + WWB.1 + WGrades.1 ~ WE.2 + WR.2 + WC.2 + WI.2 + WIB.2 + WWB.2 + WGrades.2 
            WE.2 + WR.2 + WC.2 + WI.2 + WIB.2 + WWB.2 + WGrades.2 ~ WE.3 + WR.3 + WC.3 + WI.3 + WIB.3 + WWB.3 + WGrades.3 
            
            
            
            ######## Estimate correlations within the same wave ################
            
            # Wave 1
            WE.1 ~~ WR.1
            WE.1 ~~ WC.1
            WE.1 ~~ WI.1
            WE.1 ~~ WIB.1
            WE.1 ~~ WWB.1
            WE.1 ~~ WGrades.1
            WR.1 ~~ WC.1
            WR.1 ~~ WI.1
            WR.1 ~~ WIB.1
            WR.1 ~~ WWB.1
            WR.1 ~~ WGrades.1
            WC.1 ~~ WI.1
            WC.1 ~~ WIB.1
            WC.1 ~~ WWB.1
            WC.1 ~~ WGrades.1
            WI.1 ~~ WIB.1
            WI.1 ~~ WWB.1
            WI.1 ~~ WGrades.1
            WIB.1 ~~ WWB.1
            WIB.1 ~~ WGrades.1
            WWB.1 ~~ WGrades.1
            
            # Wave 2
            WE.2 ~~ WR.2
            WE.2 ~~ WC.2
            WE.2 ~~ WI.2
            WE.2 ~~ WIB.2
            WE.2 ~~ WWB.2
            WE.2 ~~ WGrades.2
            WR.2 ~~ WC.2
            WR.2 ~~ WI.2
            WR.2 ~~ WIB.2
            WR.2 ~~ WWB.2
            WR.2 ~~ WGrades.2
            WC.2 ~~ WI.2
            WC.2 ~~ WIB.2
            WC.2 ~~ WWB.2
            WC.2 ~~ WGrades.2
            WI.2 ~~ WIB.2
            WI.2 ~~ WWB.2
            WI.2 ~~ WGrades.2
            WIB.2 ~~ WWB.2
            WIB.2 ~~ WGrades.2
            WWB.2 ~~ WGrades.2
            
            # Wave 3
            WE.3 ~~ WR.3
            WE.3 ~~ WC.3
            WE.3 ~~ WI.3
            WE.3 ~~ WIB.3
            WE.3 ~~ WWB.3
            WE.3 ~~ WGrades.3
            WR.3 ~~ WC.3
            WR.3 ~~ WI.3
            WR.3 ~~ WIB.3
            WR.3 ~~ WWB.3
            WR.3 ~~ WGrades.3
            WC.3 ~~ WI.3
            WC.3 ~~ WIB.3
            WC.3 ~~ WWB.3
            WC.3 ~~ WGrades.3
            WI.3 ~~ WIB.3
            WI.3 ~~ WWB.3
            WI.3 ~~ WGrades.3
            WIB.3 ~~ WWB.3
            WIB.3 ~~ WGrades.3
            WWB.3 ~~ WGrades.3
'
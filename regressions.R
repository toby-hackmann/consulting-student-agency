regressions <- '
            ########################## REGRESSIONS #############################
            # Now we will define all the regressions in the model that will be 
            # checked. "~" means "is regressed on".
            
            E.1 + R.1 + C.1 + I.1 + IB.1 + WB.1 + Grades.1 ~ E.2 + R.2 + C.2 + I.2 + IB.2 + WB.2 + Grades.2 
            E.2 + R.2 + C.2 + I.2 + IB.2 + WB.2 + Grades.2 ~ E.3 + R.3 + C.3 + I.3 + IB.3 + WB.3 + Grades.3 
            
            
            
            ######## Estimate correlations within the same wave ################
            
            # Wave 1
            E.1 ~~ R.1
            E.1 ~~ C.1
            E.1 ~~ I.1
            E.1 ~~ IB.1
            E.1 ~~ WB.1
            E.1 ~~ Grades.1
            R.1 ~~ C.1
            R.1 ~~ I.1
            R.1 ~~ IB.1
            R.1 ~~ WB.1
            R.1 ~~ Grades.1
            C.1 ~~ I.1
            C.1 ~~ IB.1
            C.1 ~~ WB.1
            C.1 ~~ Grades.1
            I.1 ~~ IB.1
            I.1 ~~ WB.1
            I.1 ~~ Grades.1
            IB.1 ~~ WB.1
            IB.1 ~~ Grades.1
            WB.1 ~~ Grades.1
            
            # Wave 2
            E.2 ~~ R.2
            E.2 ~~ C.2
            E.2 ~~ I.2
            E.2 ~~ IB.2
            E.2 ~~ WB.2
            E.2 ~~ Grades.2
            R.2 ~~ C.2
            R.2 ~~ I.2
            R.2 ~~ IB.2
            R.2 ~~ WB.2
            R.2 ~~ Grades.2
            C.2 ~~ I.2
            C.2 ~~ IB.2
            C.2 ~~ WB.2
            C.2 ~~ Grades.2
            I.2 ~~ IB.2
            I.2 ~~ WB.2
            I.2 ~~ Grades.2
            IB.2 ~~ WB.2
            IB.2 ~~ Grades.2
            WB.2 ~~ Grades.2
            
            # Wave 3
            E.3 ~~ R.3
            E.3 ~~ C.3
            E.3 ~~ I.3
            E.3 ~~ IB.3
            E.3 ~~ WB.3
            E.3 ~~ Grades.3
            R.3 ~~ C.3
            R.3 ~~ I.3
            R.3 ~~ IB.3
            R.3 ~~ WB.3
            R.3 ~~ Grades.3
            C.3 ~~ I.3
            C.3 ~~ IB.3
            C.3 ~~ WB.3
            C.3 ~~ Grades.3
            I.3 ~~ IB.3
            I.3 ~~ WB.3
            I.3 ~~ Grades.3
            IB.3 ~~ WB.3
            IB.3 ~~ Grades.3
            WB.3 ~~ Grades.3
'
###############################################################################
###############################################################################
######################### Long-to-Wide dataframe script #######################
###############################################################################
###############################################################################

# We need to remove all questions that we have decided to remove from the
# questionnaire. If this is new questionnaire data, this code should not be run
# and be commented out instead
# We make vectors what we keep and we add the number of variables before it.
#keep.init = c(1:5)
#keep.vm = c(3, 6, 7, 8, 10)+5
#keep.se = c(1, 3, 4, 5, 6, 7)+5+11
#keep.i = c(2, 3, 4, 5, 7, 8)+5+11+12
#keep.sr = c(2, 7, 8, 9)+5+11+12+8+7
#keep.c = c(1:8)+5+11+12+8+7+10
#keep.srf = c(1, 2, 5:13)+5+11+12+8+7+10+8
#keep.ib = c(1:3)+5+11+12+8+7+10+8+13
#keep.wb = c(1:4, 6, 7, 8, 10)+5+11+12+8+7+10+8+13+3
#keep.last = c(1,2)+5+11+12+8+7+10+8+13+3+10
#keep = c(keep.init,keep.vm, keep.se, keep.i, keep.sr, keep.c, keep.srf, keep.ib, keep.wb, keep.last)
#data <- data[, keep]


# We turn the dataframe from the long format to the wide format in this script

data.1 <- data[data$Time == 1, ]
data.2 <- data[data$Time == 2, ]
data.3 <- data[data$Time == 3, ]


# Rename the question variables:
new_labels <- c("VM1", "VM2", "VM3", "VM4", "VM5", 
                "SE1", "SE2", "SE3", "SE4", "SE5", "SE6", 
                "I1", "I2", "I3", "I4", "I5", "I6", 
                "SR1", "SR2", "SR3", "SR4", 
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8",
                "SRf1", "SRf2", "SRf3", "SRf4", "SRf5", "SRf6", "SRf7",
                "SRf8", "SRf9", "SRf10", "SRf11",
                "IB1", "IB2", "IB3", 
                "WB1", "WB2", "WB3", "WB4", "WB5", "WB6", "WB7", "WB8", 
                "Cijfers")

# Change 87 to 89 after removing the mean values
colnames(data.1)[6:57] <- paste0(new_labels, ".1")
colnames(data.2)[6:57] <- paste0(new_labels, ".2")
colnames(data.3)[6:57] <- paste0(new_labels, ".3")

data <- merge(data.1[, 1:57], data.2[, c(1, 6:57)], by = "Leerlingnummer")
data <- merge(data, data.3[, c(1, 6:57)], by = "Leerlingnummer")

rm(data.1, data.2, data.3, new_labels)




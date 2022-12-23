###############################################################################
###############################################################################
######################### Long-to-Wide dataframe script #######################
###############################################################################
###############################################################################

# We turn the dataframe from the long format to the wide format in this script

#data.1 <- data["Time" == 1]
#data.2 <- data["Time" == 2]
#data.3 <- data["Time" == 3]

# Rename the question variables:
new_labels <- c("VM1", "VM2", "VM3", "VM4", "VM5", "VM6", "VM7", "VM8",
                "VM9", "VM10", "VM11", "SE1", "SE2", "SE3", "SE4", "SE5",
                "SE6", "SE7", "SE8", "SE9", "SE10", "SE11", "SE12",
                "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8",
                "F1", "F2", "F3", "F4", "F5", "F6", "F7", "SR1", "SR2",
                "SR3", "SR4", "SR5", "SR6", "SR7", "SR8", "SR9", "SR10",
                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8",
                "SRf1", "SRf2", "SRf3", "SRf4", "SRf5", "SRf6", "SRf7",
                "SRf8", "SRf9", "SRf10", "SRf11", "SRf12", "SRf13",
                "WB1", "WB2", "WB3", "WB4", "WB5", "WB6", "WB7", "WB8", "WB9",
                "WB10", "WB11", "WB12", "WB13", "Cijfers", "Tekort")
# Change 87 to 89 after removing the mean values
colnames(data.1)[6:89] <- paste0(new_labels, ".1")
colnames(data.2)[6:89] <- paste0(new_labels, ".2")
colnames(data.3)[6:89] <- paste0(new_labels, ".3")

data <- merge(data.1, data.2[, c(1, 6:89)], by = "Leerlingnummer")
data <- merge(data, data.3[, c(1, 6:89)], by = "Leerlingnummer")




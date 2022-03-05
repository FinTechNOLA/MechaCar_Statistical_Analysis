# Deliverable 1
#Linear Regression to Predict MPG
library(dplyr)
#Import CSV and Read as DataFrame
MechaCarChallenge <- read.csv(file="MechaCar_mpg.csv", check.names = F, stringsAsFactors = F)

# Perform multiple linear regression model
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = MechaCarChallenge)

# Summary
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD, data = MechaCarChallenge))

# Deliverable 2
# Import CSV adn read as a DataFrame
SCoilChallenge <- read.csv(file = "Suspension_Coil.csv", check.names = F, stringsAsFactors = F)

# Summary
total_summary <- SCoilChallenge %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# Lot Summary
lot_summary <- SCoilChallenge %>% group_by(Manufacturing_Lot) %>% summarize(Mean = mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')

# Deliverable 3
#T-Test on Suspension Coils
t.test(SCoilChallenge$PSI, mu=1500)

t.test(subset(SCoilChallenge,Manufacturing_Lot=="Lot1")$PSI, mu=1500)
t.test(subset(SCoilChallenge,Manufacturing_Lot=="Lot2")$PSI, mu=1500)
t.test(subset(SCoilChallenge,Manufacturing_Lot=="Lot3")$PSI, mu=1500)


data_clean <- read.csv("data_clean.csv")

model <- lm(data_clean$ViolentCrimesPerPop ~ data_clean$medIncome + data_clean$whitePerCap +
              data_clean$blackPerCap + data_clean$HispPerCap + data_clean$NumUnderPov + 
              data_clean$PctUnemployed + data_clean$HousVacant +
              data_clean$MedRent + data_clean$NumStreet)

summary(model)


# START LAB 8

risk_prediction_error <- function(Level, Air.Pollution, OccuPational.Hazards, Genetic.Risk, 
                                  Balanced.Diet, Obesity, Smoking, Passive.Smoker,
                                  Chest.Pain, Coughing.of.Blood, Fatigue, Weight.Loss,
                                  Shortness.of.Breath, Swal1ing.Difficulty,
                                  Clubbing.of.Finger.Nails, Snoring){
  
  patient.dat <- data.frame(Level, Air.Pollution, OccuPational.Hazards, Genetic.Risk, 
                            Balanced.Diet, Obesity, Smoking, Passive.Smoker,
                            Chest.Pain, Coughing.of.Blood, Fatigue, Weight.Loss,
                            Shortness.of.Breath, Swal1ing.Difficulty,
                            Clubbing.of.Finger.Nails, Snoring)
  
  rows <- nrow(patient.dat) 
  f <- 0.6
  upper_bound <- floor(f * rows)
  permuted_patient.dat <- patient.dat[sample(rows) , ]
  train.dat <- permuted_patient.dat[1:upper_bound, ]
  test.dat <- permuted_patient.dat[(upper_bound+1): rows, ]
  risky.lm <- lm(Level ~ Air.Pollution + OccuPational.Hazards + Genetic.Risk,
                 data = train.dat)
  predicted.dat <- predict(risky.lm, newdata=test.dat)
  print(predicted.dat)
  delta <- predicted.dat - test.dat$Level
  rmse <- sqrt(sum(delta^2)/rows)
  return(rmse)
  
}

# Minimum 1000 Entries

patient_data <- read.csv("patient_data.csv")

# PREDICTORS

# "index"                    "Patient.Id"               "Age"                      "Gender"                   "Air.Pollution"           
# "Alcohol.use"              "Dust.Allergy"             "Occupational.Hazards"     "Genetic.Risk"             "chronic.Lung.Disease"    
# "Balanced.Diet"            "Obesity"                  "Smoking"                  "Passive.Smoker"           "Chest.Pain"              
# "Coughing.of.Blood"        "Fatigue"                  "Weight.Loss"              "Shortness.of.Breath"      "Wheezing"                
# "Swallowing.Difficulty"    "Clubbing.of.Finger.Nails" "Frequent.Cold"            "Dry.Cough"                "Snoring"                 

# PREDICTED

# "Level"                  

# Data Cleaning and Sanity
 
head(patient_data$Air.Pollution)
table(patient_data$Air.Pollution)
summary(patient_data$Air.Pollution)
length(patient_data$Air.Pollution)
sum(is.na(patient_data))

#jpeg('Air_Pollution.jpg')

plot(patient_data$Air.Pollution,
    patient_data$Level, main="Risk of Lung Cancer Following Air Pollution",
    xlab="Air Pollution", ylab="Risk Level")

# Appropriate Regression Model

# pollution.lm <- lm( patient_data$Level ~
# 
#                                   patient_data$Age +
#                                   patient_data$Air.Pollution +
#                                   patient_data$Alcohol.use +
#                                   patient_data$Dust.Allergy +
#                                   patient_data$OccuPational.Hazards +
#                                   patient_data$Genetic.Risk +
#                                   patient_data$Balanced.Diet +
#                                   patient_data$Obesity +
#                                   patient_data$Smoking +
#                                   patient_data$Passive.Smoker +
#                                   patient_data$Chest.Pain +
#                                   patient_data$Coughing.of.Blood +
#                                   patient_data$Fatigue +
#                                   patient_data$Weight.Loss +
#                                   patient_data$Shortness.of.Breath +
#                                   patient_data$Wheezing +
#                                   patient_data$Swal1ing.Difficulty +
#                                   patient_data$Clubbing.of.Finger.Nails +
#                                   patient_data$Frequent.Cold +
#                                   patient_data$Dry.Cough +
#                                   patient_data$Snoring, data = patient_data )

risk.lm <- lm( patient_data$Level ~
                     
                     patient_data$Air.Pollution +
                     patient_data$OccuPational.Hazards +
                     patient_data$Genetic.Risk +
                     patient_data$Balanced.Diet +
                     patient_data$Obesity +
                     patient_data$Smoking +
                     patient_data$Passive.Smoker +
                     patient_data$Chest.Pain +
                     patient_data$Coughing.of.Blood +
                     patient_data$Fatigue +
                     patient_data$Weight.Loss +
                     patient_data$Shortness.of.Breath +
                     patient_data$Swal1ing.Difficulty +
                     patient_data$Clubbing.of.Finger.Nails +
                     patient_data$Snoring, data = patient_data )

par(mfrow=c(2,2))
plot(risk.lm)
summary(risk.lm)
qqnorm(resid(risk.lm))
qqline(resid(risk.lm))

# Demonstrate Training and Testing Ideas

rows <- nrow(patient_data) 
f <- 0.6
upper_bound <- floor(f * rows)
permute_patient_data.dat <- patient_data[sample(rows) , ]
train.dat <- permute_patient_data.dat[1:upper_bound, ]
test.dat <- permute_patient_data.dat[(upper_bound+1): rows, ]

risky_train.lm <- lm( Level ~
                  
                  Air.Pollution +
                  OccuPational.Hazards +
                  Genetic.Risk +
                  Balanced.Diet +
                  Obesity +
                  Smoking +
                  Passive.Smoker +
                  Chest.Pain +
                  Coughing.of.Blood +
                  Fatigue +
                  Weight.Loss +
                  Shortness.of.Breath +
                  Swal1ing.Difficulty +
                  Clubbing.of.Finger.Nails +
                  Snoring, data = train.dat )
 
 # Make Reasonably Good Predictions  
 
predicted.dat <- predict(risky_train.lm, newdata=test.dat)

delta <- predicted.dat - test.dat$Level
rmse <- sqrt(sum(delta^2)/rows)
print(rmse)
print(sd(train.dat$Level))
print(sd(test.dat$Level))

data_by_gender <- patient_data %>% 
 group_by(Gender) %>% 
 summarize(
   count = n(),
   med_risk = median(Level),
   med_obesity = median(Obesity),
   med_smoking = median(Smoking),
   med_snoring = median(Snoring),
   error = risk_prediction_error( Level, Air.Pollution, OccuPational.Hazards, Genetic.Risk, 
                                  Balanced.Diet, Obesity, Smoking, Passive.Smoker,
                                  Chest.Pain, Coughing.of.Blood, Fatigue, Weight.Loss,
                                  Shortness.of.Breath, Swal1ing.Difficulty,
                                  Clubbing.of.Finger.Nails, Snoring )
   )

# data_by_gender <- patient_data %>%
#   group_by(Gender) %>%
#   summarize(
#     count = n(),
#     med_risk = median(Level),
#     med_obesity = median(Obesity),
#     med_smoking = median(Smoking),
#     med_snoring = median(Snoring),
#     error = risk_prediction_error(Level, Air.Pollution, OccuPational.Hazards, Genetic.Risk)
#   )

head(data_by_gender)



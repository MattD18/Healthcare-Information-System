library("dplyr")
library("ggplot2")
#need to initialize functions from Cross Validation for LM
heart_attack <- read.csv("~/Documents/ORIE 4741/ORIE 4741 Project/heart_attack.csv", row.names=1, stringsAsFactors=FALSE)

area_survival <- heart_attack%>%
  group_by(Health.Service.Area) %>%
  summarise(survival_rate = mean(Survived, na.rm=TRUE),
            percent_male = mean(Male,na.rm=TRUE),
            percent_white = mean(White,na.rm=TRUE),
            percent_black = mean(Black,na.rm=TRUE),
            percent_other_race = mean(Other_Race,na.rm=TRUE),
            percent_PI = mean(private_insurance,na.rm=TRUE),
            percent_medicare = mean(medicare,na.rm=TRUE),
            percent_medicaid = mean(medicaid,na.rm=TRUE),
            percent_old50_69 = mean(old50_69,na.rm=TRUE),
            percent_old70 = mean(old70,na.rm=TRUE),
            avg_severity = mean(APR.Severity.of.Illness.Code, na.rm =TRUE),
            count = n())

county_survival <- heart_attack%>%
  group_by(Hospital.County) %>%
  summarise(survival_rate = mean(Survived, na.rm=TRUE),
            percent_male = mean(Male,na.rm=TRUE),
            percent_white = mean(White,na.rm=TRUE),
            percent_black = mean(Black,na.rm=TRUE),
            percent_other_race = mean(Other_Race,na.rm=TRUE),
            percent_PI = mean(private_insurance,na.rm=TRUE),
            percent_medicare = mean(medicare,na.rm=TRUE),
            percent_medicaid = mean(medicaid,na.rm=TRUE),
            percent_old50_69 = mean(old50_69,na.rm=TRUE),
            percent_old70 = mean(old70,na.rm=TRUE),
            avg_severity = mean(APR.Severity.of.Illness.Code, na.rm =TRUE),
            count = n())

large_county_survival <- subset(county_survival,county_survival$count >= 20)

Long_Island_data <- subset(heart_attack, heart_attack$Health.Service.Area == "Long Island")

Long_Island_data <- Long_Island_data[,c(2,11,25,41,42,43,44,45,46,47,48,49,40)]
Long_Island_data$Length.of.Stay <- as.integer(as.character(Long_Island_data$Length.of.Stay))
fit_LI <- Survived ~ Length.of.Stay + APR.Severity.of.Illness.Code + Male + medicare + old50_69 + old70 + medicaid
model <- lm(fit_LI,Long_Island_data)
summary(model)
error <- k_cross_validation(5,Long_Island_data,fit_LI)


NYC_data <- subset(heart_attack, heart_attack$Health.Service.Area == "New York City")
NYC_data <- NYC_data[,c(2,11,25,41,42,43,44,45,46,47,48,49,40)]
NYC_data$Length.of.Stay <- as.integer(as.character(NYC_data$Length.of.Stay))
fit_NYC <- Survived ~ Length.of.Stay + APR.Severity.of.Illness.Code + Male + medicare + medicaid+ old50_69 + old70
model <- lm(fit_NYC,NYC_data)
summary(model)
error <- k_cross_validation(5,NYC_data,fit_NYC)

heart_attack <-heart_attack[,c(2,11,25,41,42,43,44,45,46,47,48,49,40)]
heart_attack <- heart_attack[complete.cases(heart_attack),]
heart_attack$Length.of.Stay <- as.integer(as.character(heart_attack$Length.of.Stay))
fit_general <- Survived ~ Length.of.Stay + APR.Severity.of.Illness.Code + Male + medicare 
model <- lm(fit_general,heart_attack)
summary(model)
error <- k_cross_validation(5,heart_attack,fit_general)


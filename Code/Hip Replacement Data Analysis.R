library("dplyr")
HR_data = subset(data,data$CCS.Procedure.Description == "HIP REPLACEMENT,TOT/PRT")

#HR_data$Total.Charges <- as.character(HR_data$Total.Charges)
#HR_data$Total.Charges <- substr(HR_data$Total.Charges,2,100)
#HR_data$Total.Charges <- as.numeric(HR_data$Total.Charges)


HR_data <- subset(HR_data, HR_data$APR.DRG.Description == "HIP JOINT REPLACEMENT")
HR_data <- subset(HR_data, HR_data$CCS.Diagnosis.Description != "TUBERCULOSIS")
HR_data <- subset(HR_data, HR_data$CCS.Diagnosis.Description != "OTHR INFLAMM SKIN COND")
HR_data <- subset(HR_data, HR_data$CCS.Diagnosis.Description != "ARM FRACTURE")
HR_data <- subset(HR_data, HR_data$CCS.Diagnosis.Description != "OTHER FRACTURE")

HR_data = subset(HR_data,HR_data$Total.Charges < 250000)
hist(HR_data$Total.Charges,breaks = 50)


WNY <- subset(HR_data, HR_data$Health.Service.Area == "Western NY")
FL  <- subset(HR_data, HR_data$Health.Service.Area == "Finger Lakes")
ST  <- subset(HR_data, HR_data$Health.Service.Area == "Southern Tier")
CNY <- subset(HR_data, HR_data$Health.Service.Area == "Central NY")
Capital <- subset(HR_data, HR_data$Health.Service.Area == "Capital/Adiron")
HV <- subset(HR_data, HR_data$Health.Service.Area == "Hudson Valley")
NYC  <- subset(HR_data, HR_data$Health.Service.Area == "New York City")
LI <- subset(HR_data, HR_data$Health.Service.Area == "Long Island")

hist(HR_data$Total.Charges,breaks = 50, xlab = "Total Charges (Dollars)", main = "All Health Service Areas")

hist(WNY$Total.Charges,breaks = 50)
hist(FL$Total.Charges,breaks = 50)
hist(ST$Total.Charges,breaks = 50)
hist(CNY$Total.Charges,breaks = 50, xlab = "Total Charges (Dollars)", main = "Central New York")
hist(Capital$Total.Charges,breaks = 50)


hist(HV$Total.Charges,breaks = 50)
hist(NYC$Total.Charges,breaks = 50, xlab = "Total Charges (Dollars)", main = "New York City")
hist(LI$Total.Charges,breaks = 50)

#gender shouldn't make a difference
gender <- HR_data%>%
  group_by(Gender) %>%
  summarise(num = n(), mean = mean(Total.Charges, na.rm=TRUE),sd=sd(Total.Charges, na.rm=TRUE))

#area makes a difference
area <- HR_data%>%
  group_by(Health.Service.Area) %>%
  summarise(num = n(), mean = mean(Total.Charges, na.rm=TRUE),sd=sd(Total.Charges, na.rm=TRUE))

#age makes a difference 
age <- HR_data%>%
  group_by(Age.Group) %>%
  summarise(num = n(), mean = mean(Total.Charges, na.rm=TRUE),sd=sd(Total.Charges, na.rm=TRUE))

#race makes a difference 
race <- HR_data%>%
  group_by(Race) %>%
  summarise(Number_of_Patients = n(), Mean_Cost = mean(Total.Charges, na.rm=TRUE),SD=sd(Total.Charges, na.rm=TRUE))

#makes a difference
admit_type <- HR_data%>%
  group_by(Type.of.Admission) %>%
  summarise(num = n(), mean = mean(Total.Charges, na.rm=TRUE),sd=sd(Total.Charges, na.rm=TRUE))

#hospital
hospital <- HR_data%>%
  group_by(Facility.Name,Health.Service.Area, Operating.Provider.License.Number) %>%
  summarise(num = n(), mean_charge = mean(Total.Charges, na.rm=TRUE),sd=sd(Total.Charges, na.rm=TRUE))

hospital <- hospital%>%
  group_by(Facility.Name, Health.Service.Area) %>%
  summarise(num = n(), mean = mean(mean_charge, na.rm = TRUE))

LI_hospital = subset(hospital, hospital$Health.Service.Area == "Long Island")

#payment
payment <- HR_data%>%
  group_by(Payment.Typology.1) %>%
  summarise(num = n(), mean = mean(Total.Charges, na.rm=TRUE),sd=sd(Total.Charges, na.rm=TRUE))

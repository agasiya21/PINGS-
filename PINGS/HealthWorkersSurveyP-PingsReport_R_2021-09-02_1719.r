#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('HealthWorkersSurveyP-PingsReport_DATA_2021-09-02_1719.csv')
#Setting Labels

label(data$record_id)="Record ID"
label(data$sex)="Sex"
label(data$age)="Age"
label(data$edu)="Highest educational level"
label(data$other_specify)="Specify other"
label(data$stroke)="Suffered a stroke? "
label(data$relative_stroke)="Relative suffered a stroke? "
label(data$occupation)="Primary occupation "
label(data$other_occup)="Kindly specify your primary occupation"
label(data$facility)="Type of health facility "
label(data$facility_level)="Level of the facility"
label(data$unit)="Unit"
label(data$location)="Location of your primary workplace"
label(data$experience)="Years of Health work experience "
label(data$facility_other)="Kindly specify the type of facility"
label(data$approved)="PINGS meets my approval."
label(data$appealing)="PINGS is appealing to me "
label(data$like)="I like PINGS "
label(data$welcome)="I welcome PINGS "
label(data$fitting)="PINGS seems fitting "
label(data$suitable)="PINGS seems suitable"
label(data$applicable)="PINGS seems applicable "
label(data$good_match)="PINGS seems like a good match (for stroke)"
label(data$implementable)="PINGS is implementable "
label(data$possible)="PINGS seems possible "
label(data$doable)="PINGS seems doable "
label(data$easy)="PINGS seems easy to use "
#Setting Units


#Setting Factors(will create new variable for factors)
data$sex = factor(data$sex,levels=c("1","2"))
data$edu = factor(data$edu,levels=c("1","2","3","4","5","6","7"))
data$stroke = factor(data$stroke,levels=c("1","2"))
data$relative_stroke = factor(data$relative_stroke,levels=c("1","2"))
data$occupation = factor(data$occupation,levels=c("1","2","3","4","5","10","11","12","6","7","8","9"))
data$facility = factor(data$facility,levels=c("1","2","3","4"))
data$facility_level = factor(data$facility_level,levels=c("1","2","3","4","5","6","7","8","9"))
data$location = factor(data$location,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"))
data$approved = factor(data$approved,levels=c("1","2","3","4","5"))
data$appealing = factor(data$appealing,levels=c("1","2","3","4","5"))
data$like = factor(data$like,levels=c("1","2","3","4","5"))
data$welcome = factor(data$welcome,levels=c("1","2","3","4","5"))
data$fitting = factor(data$fitting,levels=c("1","2","3","4","5"))
data$suitable = factor(data$suitable,levels=c("1","2","3","4","5"))
data$applicable = factor(data$applicable,levels=c("1","2","3","4","5"))
data$good_match = factor(data$good_match,levels=c("1","2","3","4","5"))
data$implementable = factor(data$implementable,levels=c("1","2","3","4","5"))
data$possible = factor(data$possible,levels=c("1","2","3","4","5"))
data$doable = factor(data$doable,levels=c("1","2","3","4","5"))
data$easy = factor(data$easy,levels=c("1","2","3","4","5"))

levels(data$sex)=c("Male","Female")
levels(data$edu)=c("Certificate","Diploma","Bachelors Degree","MBChB","Masters","PhD","Other (specify)")
levels(data$stroke)=c("Yes","No")
levels(data$relative_stroke)=c("Yes","No")
levels(data$occupation)=c("Nurse","Medical Doctor","Biomedical Lab scientist","Physiotherapist","Nutritionist","Health Information Officer,","Disease Control Officer,","Physician Assistant","Public health practitioner","Pharmacist","Research Scientist","Other")
levels(data$facility)=c("Government","Faith-based institutions","Private/NGO","Quasi-government")
levels(data$facility_level)=c("Teaching hospital","Regional","District Hospital","Clinic","Polyclinic","Health Center","Research Center","NGO","Other")
levels(data$location)=c("Greater Accra","Ashanti","Eastern","Western","Upper East","Upper West","Oti","Volta","Western North","North-East","Northern","Bono","Bono East","Ahafo","Savannah","Central")
levels(data$approved)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$appealing)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$like)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$welcome)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$fitting)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$suitable)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$applicable)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$good_match)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$implementable)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$possible)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$doable)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")
levels(data$easy)=c("Completely disagree","Disagree","Neither agree nor disagree","Agree","Completely agree")

write_rds(data, file = "pings")
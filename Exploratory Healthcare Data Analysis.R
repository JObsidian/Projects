#############
# OBJECTIVE #
#############
# The task is to analyze the clinical and financial data of patients hospitalized for a certain condition. 
# You are required to join the data given in different tables, and find insights about the drivers of cost of care.

################
# READING DATA #
################
demographics_data = read.csv("demographics_clean.csv",header=T)
clinical_data = read.csv("clinical_data_clean.csv",header=T)
bill_id_data = read.csv("bill_id_clean.csv",header=T)
bill_amount_data = read.csv("bill_amount_clean.csv",header=T)

#################
# DATA CLEANING #
#################
# Before analysing, data cleaning has to be done as there were some data which were not coherent. 
# An example would be in the demographics data where the gender, race and resident_status had duplicating categories. 
# In the bill data, there were duplicated entries as well and thus, the total bill amount for a patient had to be combined. 
# In the clinica_data, there were some patient with more than one entry. Hence, combining the length of stay in the hospital was necessary.
# Columns such as "number" was created to indicate the number of visits the patient made assuming that each bill represents one visit.
# Another column "duration" was created to compute the total duration the patient stayed in the hospital.
# In total, there are 3000 unique patients in this dataset.
# categorical variables: gender, race, resident_status, year, month
# continuous variables: medical_history_1 ... medical_history 7, preop_medication_1 ... preop_medication6, symptom_1 ... symptom_5, 
# lab_result_1 ... lab_result3, weight, height, total_amount, number, age, total_duration.

# Joining the Data
library(dplyr)
library(plyr)
library(eeptools)
library(magrittr)
# Joining clinical and demographics data by patient_id
clinical_demo = full_join(clinical_data, demographics_data)
# Joining bill_id and bill_amount by bill_id. 
bill = full_join(bill_id_data, bill_amount_data)
# Summing up the total amount for each individual patients
bill_unique = ddply(bill,.(patient_id),summarize,total_amount=sum(amount),number=length(patient_id))
combined_data = full_join(clinical_demo, bill_unique,by="patient_id") # for analysis of individual patients
# Calculating the duration of stay
combined_data$duration = as.Date(combined_data$date_of_discharge, format = "%d/%m/%y") - as.Date(combined_data$date_of_admission, format = "%d/%m/%y")
# Calculating the age of the patient
combined_data$date_of_birth = as.Date(combined_data$date_of_birth,format="%d/%m/%y") %>% format("19%y/%m/%d") %>% as.Date("%Y/%m/%d")
combined_data$age = round(age_calc(combined_data$date_of_birth, enddate = Sys.Date(), units = "years", precise = TRUE))
# summing up the total duration of stay for each patient
combined_data1 = combined_data[,c(1,33)]
total_duration = ddply(combined_data1,.(patient_id), summarize, total_duration = sum(duration))
unique_id = combined_data[!duplicated(combined_data$patient_id), ]
# complete data of each unique patient
complete_data = left_join(unique_id, total_duration, by ="patient_id")[-33]
# convert date of admission and date of discharge so that we can analyst in terms of the month and year
library(lubridate)
complete_data$month = month(as.POSIXlt(complete_data$date_of_admission, format="%d/%m/%y"))
complete_data$year = year(as.POSIXlt(complete_data$date_of_admission, format="%d/%m/%y"))
# Removing columns not needed for analysis 
complete_data = complete_data[-c(1,2,3,30)]
# Adding an average amount spent per visit column
complete_data$average_amount = (complete_data$total_amount)/(complete_data$number)

############################
# CREATING DUMMY VARIABLES #
############################
# creating dummy variables for categorical data
library(fastDummies)
library(caret)

complete_data_numerical = fastDummies::dummy_cols(complete_data, select_columns = c("gender","race","resident_status","year","month"))
complete_data_numerical = complete_data_numerical[-c(24,25,26,31,32)]
complete_data_numerical = sapply(complete_data_numerical[,c(1:53)],as.numeric)

############################
# MISSING VALUE IMPUTATION #
############################
library(mice)
md.pattern(complete_data_numerical)
tempData = mice(complete_data_numerical, m=5, maxit=50, meth="pmm", seed=500)
complete_data_numerical = complete(tempData,1)
complete_data$medical_history_2 = complete_data_numerical$medical_history_2
complete_data$medical_history_5 = complete_data_numerical$medical_history_5
summary(complete_data)

#############################
# RESIDENT STATUS V.S COSTS #
#############################
# total singaporean amount
singaporean = complete_data[complete_data$resident_status == "Singaporean",]
# total pr amount
pr = complete_data[complete_data$resident_status == "PR",]
# total foreigner amount 
foreigner = complete_data[complete_data$resident_status == "Foreigner",]
# aggregation 
singaporean_amount = aggregate((average_amount/nrow(singaporean)) ~ year, singaporean, FUN=sum)
pr_amount = aggregate((average_amount/nrow(pr)) ~ year, pr, FUN=sum)
foreigner_amount = aggregate((average_amount/nrow(foreigner)) ~ year, foreigner, FUN=sum)
# creating a new dataframe containing resident status and average amount spent per person per visit
singaporean_name = rep("Singaporean", nrow(singaporean_amount))
pr_name = rep("PR", nrow(pr_amount))
foreigner_name = rep("Foreigner", nrow(foreigner_amount))
singaporean_amount$resident_status = singaporean_name
pr_amount$resident_status = pr_name
foreigner_amount$resident_status = foreigner_name
# re-naming the columns
names(singaporean_amount) = c("year","average_amount","resident_status")
names(pr_amount) = c("year","average_amount","resident_status")
names(foreigner_amount) = c("year","average_amount","resident_status")
# combining the dataframes
resident_status_comparison = rbind(singaporean_amount, pr_amount, foreigner_amount)
# plotting the chart
g = ggplot(resident_status_comparison, aes(factor(year), average_amount, fill=factor(year)))
g + geom_bar(stat = "identity") + facet_grid(.~resident_status) + 
  xlab("Year") + ylab("Average amount spent in one visit") + 
  ggtitle("Avreage amount spent by the different resident status in one visit") +
  theme_bw(base_size=18) + theme(plot.title = element_text(hjust = 0.5))
  dev.copy(png, file = "plot1.png", height = 500, width = 800)
  dev.off()

# Conclusion: we see that foreigners in general pay a much higher price, followed by PR and Singaporean. 
# Conclusion: there is also downward trend in general for PR and Singaporeans healthcare cost.
# Conclusion: On the contrary, the healthcare cost for foreigners seems to have increased since 2011

##################
# RACE V.S COSTS #
##################
# Total Chinese amount
chinese = complete_data[complete_data$race == "Chinese",]
# Total Malay amount
malay = complete_data[complete_data$race == "Malay",]
# Total Indian amount 
indian = complete_data[complete_data$race == "Indian",]
# Total Others amount
others = complete_data[complete_data$race == "Others",]
# aggregation 
chinese_amount = aggregate((average_amount/nrow(chinese)) ~ year, chinese, FUN=sum)
malay_amount = aggregate((average_amount/nrow(malay)) ~ year, malay, FUN=sum)
indian_amount = aggregate((average_amount/nrow(indian)) ~ year, indian, FUN=sum)
others_amount = aggregate((average_amount/nrow(others)) ~ year, others, FUN=sum)
# creating a new dataframe containing races and average amount spent per person per visit
chinese_name = rep("Chinese", nrow(chinese_amount))
malay_name = rep("Malay", nrow(malay_amount))
indian_name = rep("Indian", nrow(indian_amount))
others_name = rep("Others", nrow(others_amount))
chinese_amount$race = chinese_name
malay_amount$race = malay_name
indian_amount$race = indian_name
others_amount$race = others_name
# re-naming the columns
names(chinese_amount) = c("year","average_amount","race")
names(malay_amount) = c("year","average_amount","race")
names(indian_amount) = c("year", "average_amount", "race")
names(others_amount) = c("year","average_amount","race")
# combining the dataframes
race_comparison = rbind(chinese_amount, malay_amount, indian_amount, others_amount)
# plot
g = ggplot(race_comparison, aes(factor(year), average_amount, fill=factor(year)))
g + geom_bar(stat = "identity") + facet_grid(.~race) + xlab("Year") + 
  ylab("Average amount spent in one visit") + 
  ggtitle("Avreage amount spent by the different races in one visit") + 
  theme_bw(base_size=18) + theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, file = "plot2.png", height = 500, width = 800)
dev.off()
# Conclusion: Healthcare costs has in general decreased except for "others".

####################
# GENDER V.S COSTS #
####################
# Total male amount
male = complete_data[complete_data$gender == "Male",]
# Total female amount
female = complete_data[complete_data$gender == "Female",]
# aggregation 
male_amount = aggregate((average_amount/nrow(male)) ~ year, male, FUN=sum)
female_amount = aggregate((average_amount/nrow(female)) ~ year, female, FUN=sum)
# creating a new dataframe containing genders and average amount spent per person per visit
male_name = rep("Male", nrow(male_amount))
female_name = rep("Female", nrow(female_amount))
male_amount$gender = male_name
female_amount$gender = female_name
# re-naming the columns
names(male_amount) = c("year","average_amount","gender")
names(female_amount) = c("year","average_amount","gender")
# combining the dataframes
gender_comparison = rbind(male_amount, female_amount)
# plot
g = ggplot(gender_comparison, aes(factor(year), average_amount, fill=factor(year)))
g + geom_bar(stat = "identity") + facet_grid(.~gender) + xlab("Year") + 
  ylab("Average amount spent in one visit") + 
  ggtitle("Avreage amount spent by the different genders in one visit") + 
  theme_bw(base_size=18) + theme(plot.title = element_text(hjust = 0.5))
  dev.copy(png, file = "plot3.png", height = 500, width = 800)
  dev.off()
# Conclusion: Healthcare costs is similar between the two genders. Although it was much higher for males in year 2011 as compared to females.

############################
# MONTH V.S TOTAL PATIENTS #
############################
library(plyr)
gender_peak = count(complete_data, c("month", "gender")) 
g = ggplot(gender_peak, aes(factor(gender), freq, fill=factor(gender)))
g + geom_bar(stat = "identity") + facet_grid(.~month) + xlab("Gender") + 
  ylab("Total number of patients") + ggtitle("Total number of patients by months") + 
  theme_bw(base_size=15) + theme(plot.title = element_text(hjust = 0.5))
  dev.copy(png, file = "plot4.png", height = 480, width = 680)
  dev.off()
# Conclusion: Lowest number of patients is during september, december period. 
# Conclusion: Higher number of patients is during october.
# Conclusion: There were more female patients in January and February
# Conlcusion: There were more male patients in October and November

#################
# AGE V.S COSTS #
#################
library(lattice)
qplot(age, average_amount, data = complete_data, facets = .~resident_status) + 
  geom_smooth(method = "lm") + 
  ggtitle("Comparison of age and the average amount spent per visit") +
  theme_bw(base_size=15) + theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, file = "plot5.png", height = 480, width = 680)
dev.off()
# We see that in general, the average costs increases as age increases.
# In particular, the costs for foreigners is way higher than PR and Singaporean as represented by steeper slope.

######################
# SYMPTOMS V.S COSTS #
######################
library(corrgram)
corrgram(complete_data[,c(14,15,16,17,18,33)], main="Correlation between Symptoms and Costs")
dev.copy(png, file = "plot6.png", height = 480, width = 680)
dev.off()
# Conclusion: All symptoms are positively correlated with the costs. 
# It seems that patients with symptom_5 tends to incur much higher costs as compared to other symptoms.

###############################
# PRE-OP MEDICATION V.S COSTS #
###############################
library(corrgram)
corrgram(complete_data[,c(8,9,10,11,12,13,33)], main="Correlation between Preop_medication and Costs")
dev.copy(png, file = "plot7.png", height = 480, width = 680)
dev.off()
# Conclusion: It seems that patients with preop_medication1,2,4,5,6 incur higher costs. 

#############################
# MEDICAL HISTORY V.S COSTS #
#############################
library(corrgram)
corrgram(complete_data[,c(1,2,3,4,5,6,7,33)], main="Correlation between medical_history and Costs")
dev.copy(png, file = "plot8.png", height = 480, width = 680)
dev.off()
# Conclusion: Patients with medical_history_1 is much more likely to incur higher costs.

#################
# BMI V.S COSTS #
#################
# Calculating the BMIs and categorizing it
complete_data$BMI = complete_data$weight/(complete_data$height/100)^2
complete_data$BMIcategory = cut(complete_data$BMI, breaks=c(-Inf, 18.5, 25, 30,Inf), labels=c("underweight", "normal","overweight","obese"))

ggplot(complete_data, aes(x=complete_data$BMIcategory, y=average_amount, fill=complete_data$BMIcategory)) +
  geom_boxplot()+ labs(title="BMI V.S Costs", x="BMI Category", y = "Average Costs") +
  theme_bw(base_size=15) + theme(plot.title = element_text(hjust = 0.5)) + theme(legend.position="none")
dev.copy(png, file = "plot9.png", height = 480, width = 680)
dev.off()

# Distributions of the different BMI groups
obese = complete_data[complete_data$BMIcategory == "obese",]
overweight = complete_data[complete_data$BMIcategory == "overweight",]
normal = complete_data[complete_data$BMIcategory == "normal",]
underweight = complete_data[complete_data$BMIcategory == "underweight",]

# Plot of Obese BMI
ggplot(obese, aes(x=BMI)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  geom_vline(aes(xintercept=mean(obese$BMI)), color="blue", linetype="solid", size=1) +
  geom_vline(aes(xintercept=median(obese$BMI)), color="green", linetype="dashed", size=1) +
  labs(title="Obese BMI Density Curve",x="BMI", y = "Density") +
  theme_bw(base_size=15) + theme(plot.title = element_text(hjust = 0.5)) 
dev.copy(png, file = "plot10.png", height = 480, width = 680)
dev.off()

# Plot of Overweight BMI
ggplot(overweight, aes(x=BMI)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  geom_vline(aes(xintercept=mean(overweight$BMI)), color="blue", linetype="solid", size=1) +
  geom_vline(aes(xintercept=median(overweight$BMI)), color="green", linetype="dashed", size=1) +
  labs(title="Overweight BMI Density Curve",x="BMI", y = "Density") +
  theme_bw(base_size=15) + theme(plot.title = element_text(hjust = 0.5)) 
dev.copy(png, file = "plot11.png", height = 480, width = 680)
dev.off()

# Plot of Normal BMI
ggplot(normal, aes(x=BMI)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  geom_vline(aes(xintercept=mean(normal$BMI)), color="blue", linetype="solid", size=1) +
  geom_vline(aes(xintercept=median(normal$BMI)), color="green", linetype="dashed", size=1) +
  labs(title="Normal BMI Density Curve",x="BMI", y = "Density") +
  theme_bw(base_size=15) + theme(plot.title = element_text(hjust = 0.5)) 
dev.copy(png, file = "plot12.png", height = 480, width = 680)
dev.off()

# Plot of Underweight BMI
ggplot(underweight, aes(x=BMI)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")  +
  geom_vline(aes(xintercept=mean(underweight$BMI)), color="blue", linetype="solid", size=1) +
  geom_vline(aes(xintercept=median(underweight$BMI)), color="green", linetype="dashed", size=1) +
  labs(title="Underweight BMI Density Curve",x="BMI", y = "Density") +
  theme_bw(base_size=15) + theme(plot.title = element_text(hjust = 0.5)) 
dev.copy(png, file = "plot13.png", height = 480, width = 680)
dev.off()

# Conducting wilcoxon rank sum test (non-parametric)
wilcox.test(obese$average_amount, overweight$average_amount)
wilcox.test(obese$average_amount, normal$average_amount)
wilcox.test(normal$average_amount, overweight$average_amount)

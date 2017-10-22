train_model = rpart()

setwd("C:/R")
traindata = read.csv('train.csv',header = T,stringsAsFactors = FALSE)
testdata = read.csv('test.csv',header=T, stringsAsFactors = FALSE)

head(traindata)
str(traindata)


table(traindata$Survived)
prop.table(table(traindata$Survived, traindata$Sex))
prop.table(table(traindata$Sex))
barplot(abc,main="",xlab = "")

traindata$Survived

sum(is.na(traindata$Age))
help(mean)
traindata$Age
mean(traindata$Age, na.rm = T)

colnames(traindata)

head(traindata)
age_data_train = traindata[which(!(is.na(traindata$Age))),c(2,6)]
age_data_train["class_group"] <- "name"   # add age_class
age_data_train$class_group[which(age_data_train$Age >=0 & age_data_train$Age <10)] ="0to9"
age_data_train$class_group[which(age_data_train$Age >=10 & age_data_train$Age <20)] ="10to19"
age_data_train$class_group[which(age_data_train$Age >=20 & age_data_train$Age <30)] ="20to29"
age_data_train$class_group[which(age_data_train$Age >=30 & age_data_train$Age <40)] ="30to39"
age_data_train$class_group[which(age_data_train$Age >=40 & age_data_train$Age <50)] ="40to49"
age_data_train$class_group[which(age_data_train$Age >=50 & age_data_train$Age <60)] ="50to59"
age_data_train$class_group[which(age_data_train$Age >=60)] ="60plus"
survival_age = table(age_data_train$class_group[which(age_data_train$Survived == 1)])
died_age = table(age_data_train$class_group[which(age_data_train$Survived == 0)])
general_surviving_rate = table(traindata$Survived)[2]/nrow(traindata) # gives general survival rate
tot_age_group = survival_age + died_age # gives number of records for each age group
rate_surviving_by_age = survival_age /tot_age_group # gives rate of survival for each age group
rate_surviving_compared_general_rate = rate_surviving_by_age /general_surviving_rate # compares each age survival rate compared to general survival rate
rate_surviving_compared_general_rate # rate of survival compared to general population
# found 0-9 1.5 * chance of survival  and 30-39 1.14* chance of survial
barplot(rate_surviving_compared_general_rate, main = "% compared to general population",sub = "1.0 is general pop rate",xlab = "Age Groups", ylab = "Percent")

traindata$Fare
is.na(traindata$Fare)


traindata["Fare_group"] <-0   # add age_class
traindata$Fare_group[which(traindata$Fare >=0 & traindata$Fare <10)] ="0to9"
traindata$Fare_group[which(traindata$Fare >=10 & traindata$Fare <30)] ="10to29"
traindata$Fare_group[which(traindata$Fare >=30 & traindata$Fare <60)] ="30to60"
traindata$Fare_group[which(traindata$Fare >=60 & traindata$Fare <90)] ="60to90"
traindata$Fare_group[which(traindata$Fare >=90 & traindata$Fare <200)] ="90to200"
traindata$Fare_group[which(traindata$Fare >=200)] ="200plus"
survival_fare = table(traindata$Fare_group[which(traindata$Survived == 1)])
died_fare = table(traindata$Fare_group[which(traindata$Survived == 0)])
died_fare
survival_fare
total_by_fare = survival_fare+died_fare
total_by_fare
survival_percent_per_fare = survival_fare/total_by_fare
survival_percent_per_fare
survival_fare_general = survival_percent_per_fare/general_surviving_rate
barplot(survival_fare_general, main = "Survivial percent by Fare", xlab = "Fare Groups",ylab = "Percent")




general_surviving_rate = sum(grepl(1,traindata$Survived))/nrow(traindata) # gives general survival rate
survive_by_sex = table(traindata$Sex[which(traindata$Survived == 1)])# survived by sex
died_by_sex = table(traindata$Sex[which(traindata$Survived == 0)])# died by sex
total_per_sex = survive_by_sex + died_by_sex # total count per sex
survive_percent_per_sex = survive_by_sex / total_per_sex # get percentage that survived per sex
survive_percent_per_sex
died_by_sex
survive_per_sex_comp_to_gen_pop = survive_percent_per_sex / general_surviving_rate # sex survival rate compared rate to general rate
survive_per_sex_comp_to_gen_pop# percent per sex compared to generl population survived
barplot(survive_per_sex_comp_to_gen_pop, main = "survival_percent by sex compared to general population",xlab = "Sex",ylab = "Percent compare to General Population")


traindata$Pclass

general_surviving_rate = sum(grepl(1,traindata$Survived))/nrow(traindata) # gives general survival rate
survive_by_Pclass = table(traindata$Pclass[which(traindata$Survived == 1)])# survived by sex
died_by_Pclass = table(traindata$Pclass[which(traindata$Survived == 0)])# died by sex
total_per_Pclass = survive_by_Pclass + died_by_Pclass # total count per sex
survive_percent_per_Pclass = survive_by_Pclass / total_per_Pclass # get percentage that survived per sex
survive_percent_per_Pclass
died_by_Pclass
survive_per_Pclass_comp_to_gen_pop = survive_percent_per_Pclass / general_surviving_rate # sex survival rate compared rate to general rate
survive_per_Pclass_comp_to_gen_pop# percent per sex compared to generl population survived
barplot(survive_per_Pclass_comp_to_gen_pop, main = "survival_percent by plass compared to general population",xlab = "PClass",ylab = "Percent compare to General Population")


Survived_by_Sibsp= table(traindata$SibSp[which(traindata$Survived==1)])
died_by_Sibsp= table(traindata$SibSp[which(traindata$Survived==0 & traindata$SibSp<5)])
Total_per_Sibsp = Survived_by_Sibsp+died_by_Sibsp
Survive_Percent_per_Sibsp= Survived_by_Sibsp/Total_per_Sibsp
Survive_per_Sibsp_to_gen_pop= Survive_Percent_per_Sibsp/general_surviving_rate
barplot(Survive_per_Sibsp_to_gen_pop, main = "Survival percent by Sibsp", xlab = "Sibsb",ylab = "Percent compared to general population")

#With our Visualization for age we found out that children with Age 0-9 were the once most likely to survive. While people between age 20-29 were least likely to survive.
#We can see in survivial by fare graph that people who paid more were more likely to survive.
#More number of females survived than males.
#People with pclass 1 had more survivors than people with pclass 2 and 3


traindata["title"]=NA
traindata$title[grep("Miss\\.",traindata$Name)]="Miss"
traindata$title[grep("Mrs\\.",traindata$Name)]="Mrs"
traindata$title[grep("Mr\\.",traindata$Name)]="Mr"
traindata$title[grep("Master\\.",traindata$Name)]="Master"
traindata$title[grep("Dr\\.",traindata$Name)]="Doctor"


miss=mean(traindata$Age[which(traindata$title=="Miss")],na.rm = TRUE)
mrs=mean(traindata$Age[which(traindata$title=="Mrs")],na.rm = TRUE)
mr=mean(traindata$Age[which(traindata$title=="Mr")],na.rm = TRUE)
master=mean(traindata$Age[which(traindata$title=="Master")],na.rm = TRUE)
doctor=mean(traindata$Age[which(traindata$title=="Doctor")],na.rm = TRUE)



testdata["title"]=NA
testdata$title[grep("Miss\\.",testdata$Name)]="Miss"
testdata$title[grep("Mrs\\.",testdata$Name)]="Mrs"
testdata$title[grep("Mr\\.",testdata$Name)]="Mr"
testdata$title[grep("Master\\.",testdata$Name)]="Master"
testdata$title[grep("Dr\\.",testdata$Name)]="Doctor"


testdata$Age[which(testdata$title=="Miss" & is.na(testdata$Age))]=round(miss)
testdata$Age[which(testdata$title=="Mrs" & is.na(testdata$Age))]=round(mrs)
testdata$Age[which(testdata$title=="Mr" & is.na(testdata$Age))]=round(mr)
testdata$Age[which(testdata$title=="Master" & is.na(testdata$Age))]=round(master)
testdata$Age[which(testdata$title=="Doctor" & is.na(testdata$Age))]=round(doctor)



install.packages('rpart')
library('rpart')


mymodel = rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=traindata, method="class")
predictions = predict(mymodel, newdata = testdata, type="class")
submitcsv = data.frame(PassengerId = testdata$PassengerId, Survived = predictions)
write.csv(submitcsv, file = "ashwin_titanic_2.csv", row.names = FALSE)




barplot(rate_surviving_compared_general_rate, main = "% compared to general population",sub = "1.0 is general pop rate",xlab = "Age Groups", ylab = "Percent")
barplot(survival_fare_general, main = "Survivial percent by Fare", xlab = "Fare Groups",ylab = "Percent")
barplot(survive_per_sex_comp_to_gen_pop, main = "survival_percent by sex compared to general population",xlab = "Sex",ylab = "Percent compare to General Population")
barplot(survive_per_Pclass_comp_to_gen_pop, main = "survival_percent by plass compared to general population",xlab = "PClass",ylab = "Percent compare to General Population")
barplot(Survive_per_Sibsp_to_gen_pop, main = "Survival percent by Sibsp", xlab = "Sibsb",ylab = "Percent compared to general population")


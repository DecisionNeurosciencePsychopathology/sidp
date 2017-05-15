#to read a new dataset:

library(readxl)
library(readr)
library(lme4) # this is the one to use for modeling
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
source(file.path(getMainDir(), "Miscellaneous", "Global_Functions.R"))
library(R.matlab)
library(xtable)
library(Hmisc)

setwd("C:/Users/a/Documents/GitHub/sidp")

sidp2 <- read_excel("sidp2.xlsx")
View(sidp2)

##abreviations used:
## dob = date of birth
## uppsp = impulsive behavior scale
## barratt = barratt impulsivity scale
## spsi = social problem solving inventory
## mcq = monetary choice questionnaire: delayed discounting
## doi = decision-outcome inventory: lifetime, 10 past years
## admc = decision-making competence: sunk cost (sc), admc framing (f)
## exit = executive interview (neuropsych)
## ma = McArthur social status: questions 1 to 7
## anx = anxiety: lifetime, current
## subst = substance: lifetime, current

#creating a smaller dataset without the suspicious participant coded "5" for subgroup.
#ssidp <- sidp[(sidp$subgroup !=5),]

#transforming columns to the correct types
sidp2$ocpd <- sidp2$ocpd1 + sidp2$ocpd2 + sidp2$ocpd4 + sidp2$ocpd5 + sidp2$ocpd6 + sidp2$ocpd8
sidp2$avoid <- sidp2$avoid2 + sidp2$avoid3 + sidp2$avoid4 + sidp2$avoid5 + sidp2$avoid6
sidp2$bpd <- sidp2$bpd1 + sidp2$bpd3 + sidp2$bpd4+ sidp2$bpd5+ sidp2$bpd6+ sidp2$bpd7+ sidp2$bpd8+ sidp2$bpd9
sidp2$narc <- sidp2$narc1 + sidp2$narc2 + sidp2$narc3 + sidp2$narc4 + sidp2$narc5 + sidp2$narc7 + sidp2$narc8 + sidp2$narc9
sidp2$schztyp <- sidp2$schztyp1 + sidp2$schztyp2 + sidp2$schztyp3 + sidp2$schztyp4 + sidp2$schztyp5 + sidp2$schztyp6 + sidp2$schztyp7 + sidp2$schztyp9
sidp2$antso <- sidp2$antso1 + sidp2$antso2 + sidp2$antso3 + sidp2$antso5 + sidp2$antso6 + sidp2$antso7 + sidp2$antsoC

sidp2 <- transform(sidp2, age = as.numeric(age), neuroticism = as.numeric(neuroticism), extraversion = as.numeric(extraversion), openness = as.numeric(openness), conscientiousness = as.numeric(conscientiousness), agreeableness = as.numeric(agreeableness))
sidp2 <- transform(sidp2, ID = as.factor(ID), gender = as.factor(gender), subgroup = as.factor(subgroup))



#counting number of persons for each subgroup
nb.subgroup <- table(sidp2$subgroup)
View(nb.subgroup)


#creating a global category of all PDs together
sidp2$allpd <- sidp2$narc + sidp2$bpd + sidp2$ocpd + sidp2$antso + sidp2$schztyp + sidp2$avoid

#creating normalized values for PD traits according to maximum scores for each PD.
#ssidp$narc.norm <- ssidp$narc/ssidp$narc.ref
#ssidp$antisoc.norm <- ssidp$antisoc/ssidp$antisoc.ref
#ssidp$bpd.norm <- ssidp$bpd/ssidp$bpd.ref
#ssidp$ocpd.norm <- ssidp$ocpd/ssidp$ocpd.ref
#ssidp$avoid.norm <- ssidp$avoid/ssidp$avoid.ref
#ssidp$schtyp.norm <- ssidp$schtyp/ssidp$schtyp.ref
#ssidp$allpd.norm <- ssidp$allpd/(ssidp$narc.ref + ssidp$antisoc.ref + ssidp$bpd.ref + ssidp$ocpd.ref + ssidp$avoid.ref + ssidp$schtyp.ref)


#sidp$clusterA <- sidp$schtyp
#sidp$clusterB <- sidp$antisoc + sidp$narc + sidp$bpd
#sidp$clusterC <- sidp$ocpd + sidp$avoid



#creating subplots with demographic data about the sample
demog0 <- table(sidp2$subgroup)
barplot(demog0, main="subjects distribution",
        xlab="subgroups",
        legend = rownames(sidp2$subgroup), col=c("yellow"), beside=TRUE)

demog1 <- table(sidp2$gender, sidp2$subgroup)
barplot(demog1, main="gender distribution",
        xlab="red = women, blue = men", col=c("red", "darkblue"),
        beside=TRUE)

demog2bis <- hist(sidp2$age)
demog2 <- table(sidp2$subgroup, sidp2$age, )
barplot(demog2, main="age at baseline",
        xlab="subgroups", col=c("yellow"), beside=TRUE)


#creating an additional, smaller dataset in order to sum traits for each subgroup and to adjust these values to each subgroup's size.
pd2 <- data.frame("subgroups" = c(1,2,4,6,7),
                   "narcTraits" = c(sum(sidp2$narc[sidp2$subgroup == 1], na.rm = TRUE), sum(sidp2$narc[sidp2$subgroup == 2], na.rm = TRUE), sum(sidp2$narc[sidp2$subgroup == 4], na.rm = TRUE), sum(sidp2$narc[sidp2$subgroup == 6], na.rm = TRUE), sum(sidp2$narc[sidp2$subgroup == 7], na.rm = TRUE)),
                  "bpdTraits" = c(sum(sidp2$bpd[sidp2$subgroup == 1], na.rm = TRUE), sum(sidp2$bpd[sidp2$subgroup == 2], na.rm = TRUE), sum(sidp2$bpd[sidp2$subgroup == 4], na.rm = TRUE), sum(sidp2$bpd[sidp2$subgroup == 6], na.rm = TRUE), sum(sidp2$bpd[sidp2$subgroup == 7], na.rm = TRUE)),
                 "antisocTraits" = c(sum(sidp2$antso[sidp2$subgroup == 1], na.rm = TRUE), sum(sidp2$antso[sidp2$subgroup == 2], na.rm = TRUE), sum(sidp2$antso[sidp2$subgroup == 4], na.rm = TRUE), sum(sidp2$antso[sidp2$subgroup == 6], na.rm = TRUE), sum(sidp2$antso[sidp2$subgroup == 7], na.rm = TRUE)),
                 "avoidTraits" = c(sum(sidp2$avoid[sidp2$subgroup == 1], na.rm = TRUE), sum(sidp2$avoid[sidp2$subgroup == 2], na.rm = TRUE), sum(sidp2$avoid[sidp2$subgroup == 4], na.rm = TRUE), sum(sidp2$avoid[sidp2$subgroup == 6], na.rm = TRUE), sum(sidp2$avoid[sidp2$subgroup == 7], na.rm = TRUE)),
                 "ocpdTraits" = c(sum(sidp2$ocpd[sidp2$subgroup == 1], na.rm = TRUE), sum(sidp2$ocpd[sidp2$subgroup == 2], na.rm = TRUE), sum(sidp2$ocpd[sidp2$subgroup == 4], na.rm = TRUE), sum(sidp2$ocpd[sidp2$subgroup == 6], na.rm = TRUE), sum(sidp2$ocpd[sidp2$subgroup == 7], na.rm = TRUE)),
                 "schtypTraits" = c(sum(sidp2$schztyp[sidp2$subgroup == 1], na.rm = TRUE), sum(sidp2$schztyp[sidp2$subgroup == 2], na.rm = TRUE), sum(sidp2$schztyp[sidp2$subgroup == 4], na.rm = TRUE), sum(sidp2$schztyp[sidp2$subgroup == 6], na.rm = TRUE), sum(sidp2$schztyp[sidp2$subgroup == 7], na.rm = TRUE)),
                 "allpdTraits" = c(sum(sidp2$allpd[sidp2$subgroup == 1], na.rm = TRUE), sum(sidp2$allpd[sidp2$subgroup == 2], na.rm = TRUE), sum(sidp2$allpd[sidp2$subgroup == 4], na.rm = TRUE), sum(sidp2$allpd[sidp2$subgroup == 6], na.rm = TRUE), sum(sidp2$allpd[sidp2$subgroup == 7], na.rm = TRUE)))

#pd <- transform(pd, subgroups = as.factor(subgroups))

View(pd2)

summary()

# add a barplot for each normalized personality disorder.
barplot(pd2$narcTraits, pd2$subgroups, main="narcissistic traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd2$bpdTraits, pd2$subgroups, main="borderline traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd2$antisocTraits, pd2$subgroups, main="antisocial traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd2$ocpdTraits, pd2$subgroups, main="ocpd traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd2$schtypTraits, pd2$subgroups, main="schizotypal traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd2$avoidTraits, pd2$subgroups, main="avoidant traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd2$allpdTraits, pd2$subgroups, main="all pd traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

#make a historgram about the distribution of scores for narcissism for each subgroup
hist(sidp2$narc[sidp2$subgroup==1], plot=TRUE, xlab="max scores/subjects")
hist(sidp2$narc[sidp2$subgroup==2], plot=TRUE, xlab="max scores/subjects")
hist(sidp2$narc[sidp2$subgroup==4], plot=TRUE, xlab="max scores/subjects")
hist(sidp2$narc[sidp2$subgroup==6], plot=TRUE, xlab="max scores/subjects")
hist(sidp2$narc[sidp2$subgroup==7], plot=TRUE, xlab="max scores/subjects")
hist(sidp2$narc, plot=TRUE, xlab="max scores/subjects")

hist(sidp2$neuroticism, xlab="neuroticism")
hist(sidp2$openness, xlab="openness")
hist(sidp2$agreeableness, xlab="agreeableness")
hist(sidp2$conscientiousness, xlab="conscientiousness")
hist(sidp2$extraversion, xlab="extraversion")

hist(sidp2$bpd, xlab="bpd")
hist(sidp2$antso, xlab="antso")
hist(sidp2$narc, xlab="narc")
hist(sidp2$avoid, xlab="avoid")
hist(sidp2$ocpd, xlab="ocpd")
hist(sidp2$schztyp, xlab="schztyp")
hist(sidp2$allpd, xlab="all pds")


par(mfrow=c(1,1))
par(mfrow=c(3,4))

#barplots for the sidp traits that are not normally distributed

barplot(sidp2$avoid, sidp2$subgroup, main="avoidant traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)





#add a boxplot
boxplot(sidp2$narc~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="narc traits", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$bpd~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="bpd traits", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$antso~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="antso traits", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$ocpd~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="ocpd traits", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$avoid~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="avoid traits", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$schztyp~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="schztyp traits", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$allpd~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="all traits", xlab="subgroups 1,2,4,6,7")

boxplot(sidp2$neuroticism~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="neuroticism", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$extraversion~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="extraversion", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$openness~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="openness", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$agreeableness~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="agreeableness", xlab="subgroups 1,2,4,6,7")
boxplot(sidp2$conscientiousness~sidp2$subgroup, data=sidp2, varwidth=TRUE, notch=TRUE, main="conscientiousness", xlab="subgroups 1,2,4,6,7")



#linear model for the 5-Factor model, with and without age and gender
f0.neuroticism <- lm(sidp2$neuroticism ~ sidp2$subgroup)
summary(f0.neuroticism)
f.neuroticism <- lm(sidp2$neuroticism ~ sidp2$subgroup + sidp2$age + sidp2$gender)
summary(f.neuroticism)

f0.openness <- lm(sidp2$openness ~ sidp2$subgroup)
summary(f0.openness)
f.openness <- lm(sidp2$openness ~ sidp2$subgroup + sidp2$age + sidp2$gender)
summary(f.openness)

f0.agreeableness <- lm(sidp2$agreeableness ~ sidp2$subgroup)
summary(f0.agreeableness)
f.agreeableness <- lm(sidp2$agreeableness ~ sidp2$subgroup + sidp2$age + sidp2$gender)
summary(f.agreeableness)

f0.conscientiousness <- lm(sidp2$conscientiousness ~ sidp2$subgroup)
summary(f0.conscientiousness)
f.conscientiousness <- lm(sidp2$conscientiousness ~ sidp2$subgroup + sidp2$age + sidp2$gender)
summary(f.conscientiousness)

f0.extraversion <- lm(sidp2$extraversion ~ sidp2$subgroup)
summary(f0.extraversion)
f.extraversion <- lm(sidp2$extraversion ~ sidp2$subgroup + sidp2$age + sidp2$gender)
summary(f.extraversion)

#negative binomial generalized negative model for each SIDP personality trait, including age
ml0.narc <- glm.nb(sidp2$narc ~ sidp2$subgroup, data = sidp2)
summary(ml0.narc)
ml.narc <- glm.nb(sidp2$narc ~ sidp2$subgroup + sidp2$age + sidp2$gender, data = sidp2)
summary(ml.narc)

ml.bpd <- glm.nb(sidp2$bpd ~ sidp2$subgroup + sidp2$age + sidp2$gender, data = sidp2)
summary(ml.bpd)

ml.antso <- glm.nb(sidp2$antso ~ sidp2$subgroup + sidp2$age + sidp2$gender, data = sidp2)
summary(ml.antso)

ml.ocpd <- glm.nb(sidp2$ocpd ~ sidp2$subgroup + sidp2$age + sidp2$gender, data = sidp2)
summary(ml.ocpd)

ml.avoid <- glm.nb(sidp2$avoid ~ sidp2$subgroup + sidp2$age + sidp2$gender, data = sidp2)
summary(ml.avoid)

ml.schztyp <- glm.nb(sidp2$schztyp ~ sidp2$subgroup + sidp2$age + sidp2$gender, data = sidp2)
summary(ml.schztyp)

ml.allpd <- glm.nb(sidp2$allpd ~ sidp2$subgroup + sidp2$age + sidp2$gender, data = sidp2)
summary(ml.allpd)

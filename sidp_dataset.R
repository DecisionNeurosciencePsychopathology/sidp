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

setwd('C:\Users\a\Documents\GitHub\sidp')

sidp <- read_excel("sidp.xlsx")
View(sidp)

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
ssidp <- sidp[(sidp$subgroup !=5),]

#transforming columns to the correct types
ssidp <- transform(ssidp, lethality = as.numeric(lethality), antisoc.ref = as.numeric(antisoc.ref), bpd.ref = as.numeric(bpd.ref), narc.ref = as.numeric(narc.ref), ocpd.ref = as.numeric(ocpd.ref))
ssidp <- transform(ssidp, id = as.factor(id), subgroup.comment = as.factor(subgroup.comment), subgroup = as.factor(subgroup), gender = as.factor(gender), ethnicity = as.factor(ethnicity), race = as.factor(race), anx.life = as.factor(anx.life), anx.current = as.factor(anx.current), subst.life = as.factor(subst.life), subst.current = as.factor(subst.current))

#save(sidp,file="sidp.Rda")


#counting number of persons for each subgroup
nb.subgroup <- table(ssidp$subgroup)
View(nb.subgroup)

#creating a global category of all PDs together
ssidp$allpd <- ssidp$narc + ssidp$bpd + ssidp$ocpd + ssidp$antisoc + ssidp$schtyp + ssidp$avoid

#creating normalized values for PD traits according to maximum scores for each PD.
ssidp$narc.norm <- ssidp$narc/ssidp$narc.ref
ssidp$antisoc.norm <- ssidp$antisoc/ssidp$antisoc.ref
ssidp$bpd.norm <- ssidp$bpd/ssidp$bpd.ref
ssidp$ocpd.norm <- ssidp$ocpd/ssidp$ocpd.ref
ssidp$avoid.norm <- ssidp$avoid/ssidp$avoid.ref
ssidp$schtyp.norm <- ssidp$schtyp/ssidp$schtyp.ref
ssidp$allpd.norm <- ssidp$allpd/(ssidp$narc.ref + ssidp$antisoc.ref + ssidp$bpd.ref + ssidp$ocpd.ref + ssidp$avoid.ref + ssidp$schtyp.ref)


#sidp$clusterA <- sidp$schtyp
#sidp$clusterB <- sidp$antisoc + sidp$narc + sidp$bpd
#sidp$clusterC <- sidp$ocpd + sidp$avoid

attach(mtcars)
par(mfrow=c(3,4))

#creating subplots with demographic data about the sample
demog0 <- table(ssidp$subgroup)
barplot(demog0, main="subjects distribution",
        xlab="subgroups",
        legend = rownames(ssidp$subgroup.comment), col=c("yellow"), beside=TRUE)

demog1 <- table(ssidp$gender, ssidp$subgroup)
barplot(demog1, main="gender distribution",
        xlab="red = women, blue = men", col=c("red", "darkblue"),
        beside=TRUE)

demog2 <- table(ssidp$age.baseline, ssidp$subgroup)
barplot(demog2, main="age distribution",
        xlab="subgroups", col=c("yellow"), beside=TRUE)


#creating an additional, smaller dataset in order to sum traits for each subgroup and to adjust these values to each subgroup's size.
pd <- data.frame("subgroups" = c(1,2,4,6,7),
                   "narcTraits" = c(sum(ssidp$narc.norm[ssidp$subgroup == 1])/nb.subgroup["1"], sum(ssidp$narc.norm[ssidp$subgroup == 2])/nb.subgroup["2"], sum(ssidp$narc.norm[ssidp$subgroup == 4])/nb.subgroup["4"], sum(ssidp$narc.norm[ssidp$subgroup == 6])/nb.subgroup["6"], sum(ssidp$narc.norm[ssidp$subgroup == 7])/nb.subgroup["7"]),
                  "bpdTraits" = c(sum(ssidp$bpd.norm[ssidp$subgroup == 1])/nb.subgroup["1"], sum(ssidp$bpd.norm[ssidp$subgroup == 2])/nb.subgroup["2"], sum(ssidp$bpd.norm[ssidp$subgroup == 4])/nb.subgroup["4"], sum(ssidp$bpd.norm[ssidp$subgroup == 6])/nb.subgroup["6"], sum(ssidp$bpd.norm[ssidp$subgroup == 7])/nb.subgroup["7"]),
                 "antisocTraits" = c(sum(ssidp$antisoc.norm[ssidp$subgroup == 1])/nb.subgroup["1"], sum(ssidp$antisoc.norm[ssidp$subgroup == 2])/nb.subgroup["2"], sum(ssidp$antisoc.norm[ssidp$subgroup == 4])/nb.subgroup["4"], sum(ssidp$antisoc.norm[ssidp$subgroup == 6])/nb.subgroup["6"], sum(ssidp$antisoc.norm[ssidp$subgroup == 7])/nb.subgroup["7"]),
                 "avoidTraits" = c(sum(ssidp$avoid.norm[ssidp$subgroup == 1], na.rm = TRUE)/nb.subgroup["1"], sum(ssidp$avoid.norm[ssidp$subgroup == 2], na.rm = TRUE)/nb.subgroup["2"], sum(ssidp$avoid.norm[ssidp$subgroup == 4], na.rm = TRUE)/nb.subgroup["4"], sum(ssidp$avoid.norm[ssidp$subgroup == 6], na.rm = TRUE)/nb.subgroup["6"], sum(ssidp$avoid.norm[ssidp$subgroup == 7], na.rm = TRUE)/nb.subgroup["7"]),
                 "ocpdTraits" = c(sum(ssidp$ocpd.norm[ssidp$subgroup == 1])/nb.subgroup["1"], sum(ssidp$ocpd.norm[ssidp$subgroup == 2])/nb.subgroup["2"], sum(ssidp$ocpd.norm[ssidp$subgroup == 4])/nb.subgroup["4"], sum(ssidp$ocpd.norm[ssidp$subgroup == 6])/nb.subgroup["6"], sum(ssidp$ocpd.norm[ssidp$subgroup == 7])/nb.subgroup["7"]),
                 "schtypTraits" = c(sum(ssidp$schtyp.norm[ssidp$subgroup == 1], na.rm = TRUE)/nb.subgroup["1"], sum(ssidp$schtyp.norm[ssidp$subgroup == 2], na.rm = TRUE)/nb.subgroup["2"], sum(ssidp$schtyp.norm[ssidp$subgroup == 4], na.rm = TRUE)/nb.subgroup["4"], sum(ssidp$schtyp.norm[ssidp$subgroup == 6], na.rm = TRUE)/nb.subgroup["6"], sum(ssidp$schtyp.norm[ssidp$subgroup == 7], na.rm = TRUE)/nb.subgroup["7"]),
                 "allpdTraits" = c(sum(ssidp$allpd.norm[ssidp$subgroup == 1])/nb.subgroup["1"], sum(ssidp$allpd.norm[ssidp$subgroup == 2])/nb.subgroup["2"], sum(ssidp$allpd.norm[ssidp$subgroup == 4])/nb.subgroup["4"], sum(ssidp$allpd.norm[ssidp$subgroup == 6])/nb.subgroup["6"], sum(ssidp$allpd.norm[ssidp$subgroup == 7])/nb.subgroup["7"]))

#pd <- transform(pd, subgroups = as.factor(subgroups))

View(pd)


# add a barplot for each normalized personality disorder.
barplot(pd$narcTraits, main="narcissistic traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd$bpdTraits, pd$subgroups, main="borderline traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd$antisocTraits, pd$subgroups, main="antisocial traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd$ocpdTraits, pd$subgroups, main="ocpd traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd$schtypTraits, pd$subgroups, main="schizotypal traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd$avoidTraits, pd$subgroups, main="avoidant traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)

barplot(pd$allpdTraits, pd$subgroups, main="all pd traits",
        xlab="subgroups 1,2,4,6,7", beside=TRUE)


#linear model for each personality disorder, including subgroup, age and gender
f.narc = lm(ssidp$narc ~ ssidp$subgroup + ssidp$age.baseline + ssidp$gender)
summary(f.narc)

f.bpd = lm(ssidp$bpd ~ ssidp$subgroup + ssidp$age.baseline + ssidp$gender)
summary(f.bpd)

f.ocpd = lm(ssidp$ocpd ~ ssidp$subgroup + ssidp$age.baseline + ssidp$gender)
summary(f.ocpd)

f.antisoc = lm(ssidp$antisoc ~ ssidp$subgroup + ssidp$age.baseline + ssidp$gender)
summary(f.antisoc)

f.schtyp = lm(ssidp$schtyp ~ ssidp$subgroup + ssidp$age.baseline + ssidp$gender)
summary(f.schtyp)

f.avoid = lm(ssidp$avoid ~ ssidp$subgroup + ssidp$age.baseline + ssidp$gender)
summary(f.avoid)

f.allpd = lm(ssidp$allpd ~ ssidp$subgroup + ssidp$age.baseline + ssidp$gender)
summary(f.allpd)


#linear model for each personality disorder, including age, gender and level of education
d.narc = lm(ssidp$narc ~ ssidp$education + ssidp$age.baseline + ssidp$gender)
summary(d.narc)

d.bpd = lm(ssidp$bpd ~ ssidp$education + ssidp$age.baseline + ssidp$gender)
summary(d.bpd)

d.ocpd = lm(ssidp$ocpd ~ ssidp$education + ssidp$age.baseline + ssidp$gender)
summary(d.ocpd)

d.antisoc = lm(ssidp$antisoc ~ ssidp$education + ssidp$age.baseline + ssidp$gender)
summary(d.antisoc)

d.schtyp = lm(ssidp$schtyp ~ ssidp$educatio  + ssidp$age.baseline + ssidp$gender)
summary(d.schtyp)

d.avoid = lm(ssidp$avoid ~ ssidp$education + ssidp$age.baseline + ssidp$gender)
summary(d.avoid)

d.allpd = lm(ssidp$allpd ~ ssidp$education + ssidp$age.baseline + ssidp$gender)
summary(d.allpd)


#linear model for each personality disorder, including delay discounting, age and gender
g.narc = lm(ssidp$narc ~ ssidp$mcq + ssidp$age.baseline + ssidp$gender)
summary(g.narc)

g.bpd = lm(ssidp$bpd ~ ssidp$mcq + ssidp$age.baseline + ssidp$gender)
summary(g.bpd)

g.ocpd = lm(ssidp$ocpd ~ ssidp$mcq + ssidp$age.baseline + ssidp$gender)
summary(g.ocpd)

g.antisoc = lm(ssidp$antisoc ~ ssidp$mcq + ssidp$age.baseline + ssidp$gender)
summary(g.antisoc)

g.schtyp = lm(ssidp$schtyp ~ ssidp$mcq + ssidp$age.baseline + ssidp$gender)
summary(g.schtyp)

g.avoid = lm(ssidp$avoid ~ ssidp$mcq + ssidp$age.baseline + ssidp$gender)
summary(g.avoid)

g.allpd = lm(ssidp$allpd ~ ssidp$mcq + ssidp$age.baseline + ssidp$gender)
summary(g.allpd)

#linear model for each personality disorder, including sunk cost, age and gender
g2.narc = lm(ssidp$narc ~ ssidp$admc.sc + ssidp$age.baseline + ssidp$gender)
summary(g2.narc)

g2.bpd = lm(ssidp$bpd ~ ssidp$admc.sc + ssidp$age.baseline + ssidp$gender)
summary(g2.bpd)

g2.ocpd = lm(ssidp$ocpd ~ ssidp$admc.sc + ssidp$age.baseline + ssidp$gender)
summary(g2.ocpd)

g2.antisoc = lm(ssidp$antisoc ~ ssidp$admc.sc + ssidp$age.baseline + ssidp$gender)
summary(g2.antisoc)

g2.schtyp = lm(ssidp$schtyp ~ ssidp$admc.sc + ssidp$age.baseline + ssidp$gender)
summary(g2.schtyp)

g2.avoid = lm(ssidp$avoid ~ ssidp$admc.sc + ssidp$age.baseline + ssidp$gender)
summary(g2.avoid)

g2.allpd = lm(ssidp$allpd ~ ssidp$admc.sc + ssidp$age.baseline + ssidp$gender)
summary(g2.allpd)

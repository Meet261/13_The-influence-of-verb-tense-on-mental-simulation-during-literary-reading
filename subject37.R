library(reshape)
library(gdata)
library(ggplot2)

## Read in stories, remove titles, add word scores and change . to NaN ##
Subject <- 37
# A
raw_litao_37 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/A/litao_37_prep/Output/IA_ALLVARS.txt")
pretest_A <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-hetismuis.txt")
raw_litao_37 <- raw_litao_37[-c(1, 2, 3),]
raw_litao_37$simulation <- pretest_A$TOTAAL
raw_litao_37$concreteness <- pretest_A$CONCRETENESS
raw_litao_37$lemmafrequency <- pretest_A$LEXFREQ2
raw_litao_37$wordlength <- pretest_A$WORDLENGTH

raw_litao_37$IA_FIRST_RUN_DWELL_TIME[raw_litao_37$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litao_37$IA_FIRST_FIXATION_DURATION[raw_litao_37$IA_FIRST_FIXATION_DURATION == "."] <- NaN

# B
raw_litbm_37 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/B/litbm_37_prep/Output/IA_ALLVARS.txt")
pretest_B_O <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-hoedewolvendansen.txt")
pretest_B_M <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-hoedewolvendansen_VT.txt")
raw_litbm_37 <- raw_litbm_37[-c(1, 2, 3, 4),]
raw_litbm_37$simulation <- pretest_B_M$TOTAAL
raw_litbm_37$concreteness <- pretest_B_M$CONCRETENESS
raw_litbm_37$lemmafrequency <- pretest_B_M$LEXFREQ2
raw_litbm_37$wordlength <- pretest_B_M$WORDLENGTH

raw_litbm_37$IA_FIRST_RUN_DWELL_TIME[raw_litbm_37$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litbm_37$IA_FIRST_FIXATION_DURATION[raw_litbm_37$IA_FIRST_FIXATION_DURATION == "."] <- NaN

# C
raw_litco_37 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/C/litco_37_prep/Output/IA_ALLVARS.txt")
pretest_C <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-deinvaller.txt")
raw_litco_37 <- raw_litco_37[-c(1, 2),]
raw_litco_37$simulation <- pretest_C$TOTAAL
raw_litco_37$concreteness <- pretest_C$CONCRETENESS
raw_litco_37$lemmafrequency <- pretest_C$LEXFREQ2
raw_litco_37$wordlength <- pretest_C$WORDLENGTH

raw_litco_37$IA_FIRST_RUN_DWELL_TIME[raw_litco_37$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litco_37$IA_FIRST_FIXATION_DURATION[raw_litco_37$IA_FIRST_FIXATION_DURATION == "."] <- NaN

# D
raw_litdm_37 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/D/litdm_37_prep/Output/IA_ALLVARS.txt")
pretest_D <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-zeisoveral.txt")
raw_litdm_37 <- raw_litdm_37[-c(1, 2, 3),]
raw_litdm_37$simulation <- pretest_D$TOTAAL
raw_litdm_37$concreteness <- pretest_D$CONCRETENESS
raw_litdm_37$lemmafrequency <- pretest_D$LEXFREQ2
raw_litdm_37$wordlength <- pretest_D$WORDLENGTH

raw_litdm_37$IA_FIRST_RUN_DWELL_TIME[raw_litdm_37$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litdm_37$IA_FIRST_FIXATION_DURATION[raw_litdm_37$IA_FIRST_FIXATION_DURATION == "."] <- NaN

# Am = PAST_M
# Ao = PRES_O
# Bm = PAST_M
# Bo = PRES_O
# Cm = PRES_M
# Co = PAST_O
# Dm = PRES_M
# Do = PAST_O

## Sort stories by condition and extract relevant variables, add sim and app scores ##
load(file = "U:/surfdriveRU/Thesis analyse/PC analyse/simfactors.Rda")
load(file = "U:/surfdriveRU/Thesis analyse/PC analyse/appfactors.Rda")

# Presented in past, original tense present -> PAST_M
tense <- "Past"
version <- "Manipulated"
simPC1 <- simfactors$TC1[simfactors$Participant==37 & simfactors$Condition_new=="PAST_M"]
simPC2 <- simfactors$TC2[simfactors$Participant==37 & simfactors$Condition_new=="PAST_M"]
simPC3 <- simfactors$TC3[simfactors$Participant==37 & simfactors$Condition_new=="PAST_M"]
simPC4 <- simfactors$TC4[simfactors$Participant==37 & simfactors$Condition_new=="PAST_M"]

appPC1 <- appfactors$TC1[appfactors$Participant==37 & appfactors$Condition_new=="PAST_M"]
appPC2 <- appfactors$TC2[appfactors$Participant==37 & appfactors$Condition_new=="PAST_M"]
appPC3 <- appfactors$TC3[appfactors$Participant==37 & appfactors$Condition_new=="PAST_M"]


PAST_M_37 <- data.frame(Subject, tense, version, raw_litbm_37$TRIAL_INDEX, raw_litbm_37$IA_LABEL, raw_litbm_37$simulation, raw_litbm_37$concreteness, raw_litbm_37$lemmafrequency, raw_litbm_37$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litbm_37$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litbm_37$IA_FIRST_FIXATION_DURATION)))
names(PAST_M_37) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Presented in present, original tense present -> PRES_O
tense <- "Present"
version <- "Original"
simPC1 <- simfactors$TC1[simfactors$Participant==37 & simfactors$Condition_new=="PRES_O"]
simPC2 <- simfactors$TC2[simfactors$Participant==37 & simfactors$Condition_new=="PRES_O"]
simPC3 <- simfactors$TC3[simfactors$Participant==37 & simfactors$Condition_new=="PRES_O"]
simPC4 <- simfactors$TC4[simfactors$Participant==37 & simfactors$Condition_new=="PRES_O"]

appPC1 <- appfactors$TC1[appfactors$Participant==37 & appfactors$Condition_new=="PRES_O"]
appPC2 <- appfactors$TC2[appfactors$Participant==37 & appfactors$Condition_new=="PRES_O"]
appPC3 <- appfactors$TC3[appfactors$Participant==37 & appfactors$Condition_new=="PRES_O"]

PRES_O_37 <- data.frame(Subject, tense, version, raw_litao_37$TRIAL_INDEX, raw_litao_37$IA_LABEL, raw_litao_37$simulation, raw_litao_37$concreteness, raw_litao_37$lemmafrequency, raw_litao_37$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litao_37$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litao_37$IA_FIRST_FIXATION_DURATION)))
names(PRES_O_37) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Presented in present, original tense past -> PRES_M
tense <- "Present"
version <- "Manipulated"
simPC1 <- simfactors$TC1[simfactors$Participant==37 & simfactors$Condition_new=="PRES_M"]
simPC2 <- simfactors$TC2[simfactors$Participant==37 & simfactors$Condition_new=="PRES_M"]
simPC3 <- simfactors$TC3[simfactors$Participant==37 & simfactors$Condition_new=="PRES_M"]
simPC4 <- simfactors$TC4[simfactors$Participant==37 & simfactors$Condition_new=="PRES_M"]

appPC1 <- appfactors$TC1[appfactors$Participant==37 & appfactors$Condition_new=="PRES_M"]
appPC2 <- appfactors$TC2[appfactors$Participant==37 & appfactors$Condition_new=="PRES_M"]
appPC3 <- appfactors$TC3[appfactors$Participant==37 & appfactors$Condition_new=="PRES_M"]

PRES_M_37 <- data.frame(Subject, tense, version, raw_litdm_37$TRIAL_INDEX, raw_litdm_37$IA_LABEL, raw_litdm_37$simulation, raw_litdm_37$concreteness, raw_litdm_37$lemmafrequency, raw_litdm_37$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litdm_37$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litdm_37$IA_FIRST_FIXATION_DURATION)))
names(PRES_M_37) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Presented in past, original tense past -> PAST_O
tense <- "Past"
version <- "Original"
simPC1 <- simfactors$TC1[simfactors$Participant==37 & simfactors$Condition_new=="PAST_O"]
simPC2 <- simfactors$TC2[simfactors$Participant==37 & simfactors$Condition_new=="PAST_O"]
simPC3 <- simfactors$TC3[simfactors$Participant==37 & simfactors$Condition_new=="PAST_O"]
simPC4 <- simfactors$TC4[simfactors$Participant==37 & simfactors$Condition_new=="PAST_O"]

appPC1 <- appfactors$TC1[appfactors$Participant==37 & appfactors$Condition_new=="PAST_O"]
appPC2 <- appfactors$TC2[appfactors$Participant==37 & appfactors$Condition_new=="PAST_O"]
appPC3 <- appfactors$TC3[appfactors$Participant==37 & appfactors$Condition_new=="PAST_O"]

PAST_O_37 <- data.frame(Subject, tense, version, raw_litco_37$TRIAL_INDEX, raw_litco_37$IA_LABEL, raw_litco_37$simulation, raw_litco_37$concreteness, raw_litco_37$lemmafrequency, raw_litco_37$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litco_37$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litco_37$IA_FIRST_FIXATION_DURATION)))
names(PAST_O_37) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Remove raw data frames
keep(PAST_M_37, PRES_M_37, PAST_O_37, PRES_O_37, sure = T)

## "Remove" first word of each page, except on the first page where the title was the first word (but the title has already been removed) ##
PAST_M_37$gazedur[!duplicated(PAST_M_37$page) & PAST_M_37$page > 1] <- NaN
PAST_M_37$fixdur[!duplicated(PAST_M_37$page) & PAST_M_37$page > 1] <- NaN

PRES_O_37$gazedur[!duplicated(PRES_O_37$page) & PRES_O_37$page > 1] <- NaN
PRES_O_37$fixdur[!duplicated(PRES_O_37$page) & PRES_O_37$page > 1] <- NaN

PRES_M_37$gazedur[!duplicated(PRES_M_37$page) & PRES_M_37$page > 1] <- NaN
PRES_M_37$fixdur[!duplicated(PRES_M_37$page) & PRES_M_37$page > 1] <- NaN

PAST_O_37$gazedur[!duplicated(PAST_O_37$page) & PAST_O_37$page > 1] <- NaN
PAST_O_37$fixdur[!duplicated(PAST_O_37$page) & PAST_O_37$page > 1] <- NaN

## "Remove" AoI > 1200 or < 50 ms ##

prevalues_gaze <- sum(PAST_O_37$gazedur > 0, na.rm=T)+sum(PAST_M_37$gazedur > 0, na.rm=T)+
  sum(PRES_M_37$gazedur > 0, na.rm=T)+sum(PRES_O_37$gazedur > 0, na.rm=T)

prevalues_fix <- sum(PAST_O_37$fixdur > 0, na.rm=T)+sum(PAST_M_37$fixdur > 0, na.rm=T)+
  sum(PRES_M_37$fixdur > 0, na.rm=T)+sum(PRES_O_37$fixdur > 0, na.rm=T)

PAST_M_37$gazedur[PAST_M_37$gazedur  > 1200 | PAST_M_37$gazedur < 50] <- NaN
PAST_M_37$fixdur[PAST_M_37$fixdur > 1200 | PAST_M_37$fixdur < 50] <- NaN

PRES_O_37$gazedur[PRES_O_37$gazedur > 1200 | PRES_O_37$gazedur < 50] <- NaN
PRES_O_37$fixdur[PRES_O_37$fixdur > 1200 | PRES_O_37$fixdur < 50] <- NaN

PRES_M_37$gazedur[PRES_M_37$gazedur > 1200 | PRES_M_37$gazedur < 50] <- NaN
PRES_M_37$fixdur[PRES_M_37$fixdur > 1200 | PRES_M_37$fixdur < 50] <- NaN

PAST_O_37$gazedur[PAST_O_37$gazedur > 1200 | PAST_O_37$gazedur < 50] <- NaN
PAST_O_37$fixdur[PAST_O_37$fixdur > 1200 | PAST_O_37$fixdur < 50] <- NaN

postvalues_gaze <- sum(PAST_O_37$gazedur > 0, na.rm=T)+sum(PAST_M_37$gazedur > 0, na.rm=T)+
  sum(PRES_M_37$gazedur > 0, na.rm=T)+sum(PRES_O_37$gazedur > 0, na.rm=T)

postvalues_fix <- sum(PAST_O_37$fixdur > 0, na.rm=T)+sum(PAST_M_37$fixdur > 0, na.rm=T)+
  sum(PRES_M_37$fixdur > 0, na.rm=T)+sum(PRES_O_37$fixdur > 0, na.rm=T)

dataloss_fix <- prevalues_fix-postvalues_fix
dataloss_fix_percentage <- (1-(postvalues_fix/prevalues_fix))*100
dataloss_gaze <- prevalues_gaze-postvalues_gaze
dataloss_gaze_percentage <- (1-(postvalues_gaze/prevalues_gaze))*100

## Combine four stories/conditions
subject37 <- rbind(PAST_O_37, PAST_M_37, PRES_O_37, PRES_M_37)

## Add covariates
load(file = "U:/surfdriveRU/Thesis analyse/ART.Rda")
subject37$ART <- ART$ART[ART$Participant == 37]
subject37$ReadingExperience <- ART$A1[ART$Participant == 37]


keep(subject37, sure = T)

## Add stories
subject37$story[subject37$tense=="Present" & subject37$version=="Original"] <- "HetIsMuis"
subject37$story[subject37$tense=="Present" & subject37$version=="Manipulated"] <- "ZeIsOveral"
subject37$story[subject37$tense=="Past" & subject37$version=="Original"] <- "DeInvaller"
subject37$story[subject37$tense=="Past" & subject37$version=="Manipulated"] <- "HoeDeWolvenDansen"


save(subject37,file="U:/surfdriveRU/Thesis analyse/LMM analyse/subject37.Rda")

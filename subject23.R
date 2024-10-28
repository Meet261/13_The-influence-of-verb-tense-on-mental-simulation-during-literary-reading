library(reshape)
library(gdata)
library(ggplot2)

## Read in stories, remove titles, add word scores and change . to NaN ##
Subject <- 23
# A
raw_litao_23 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/A/litao_23_prep/Output/IA_ALLVARS.txt")
pretest_A <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-hetismuis.txt")
raw_litao_23 <- raw_litao_23[-c(1, 2, 3),]
raw_litao_23$simulation <- pretest_A$TOTAAL
raw_litao_23$concreteness <- pretest_A$CONCRETENESS
raw_litao_23$lemmafrequency <- pretest_A$LEXFREQ2
raw_litao_23$wordlength <- pretest_A$WORDLENGTH

raw_litao_23$IA_FIRST_RUN_DWELL_TIME[raw_litao_23$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litao_23$IA_FIRST_FIXATION_DURATION[raw_litao_23$IA_FIRST_FIXATION_DURATION == "."] <- NaN

# B
raw_litbm_23 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/B/litbm_23_prep/Output/IA_ALLVARS.txt")
pretest_B_O <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-hoedewolvendansen.txt")
pretest_B_M <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-hoedewolvendansen_VT.txt")
raw_litbm_23 <- raw_litbm_23[-c(1, 2, 3, 4),]
raw_litbm_23$simulation <- pretest_B_M$TOTAAL
raw_litbm_23$concreteness <- pretest_B_M$CONCRETENESS
raw_litbm_23$lemmafrequency <- pretest_B_M$LEXFREQ2
raw_litbm_23$wordlength <- pretest_B_M$WORDLENGTH

raw_litbm_23$IA_FIRST_RUN_DWELL_TIME[raw_litbm_23$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litbm_23$IA_FIRST_FIXATION_DURATION[raw_litbm_23$IA_FIRST_FIXATION_DURATION == "."] <- NaN

# C
raw_litcm_23 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/C/litcm_23_prep/Output/IA_ALLVARS.txt")
pretest_C <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-deinvaller.txt")
raw_litcm_23 <- raw_litcm_23[-c(1, 2),]
raw_litcm_23$simulation <- pretest_C$TOTAAL
raw_litcm_23$concreteness <- pretest_C$CONCRETENESS
raw_litcm_23$lemmafrequency <- pretest_C$LEXFREQ2
raw_litcm_23$wordlength <- pretest_C$WORDLENGTH

raw_litcm_23$IA_FIRST_RUN_DWELL_TIME[raw_litcm_23$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litcm_23$IA_FIRST_FIXATION_DURATION[raw_litcm_23$IA_FIRST_FIXATION_DURATION == "."] <- NaN

# D
raw_litdo_23 <- read.delim("U:/surfdriveRU/Thesis analyse/Preprocessed data/D/litdo_23_prep/Output/IA_ALLVARS.txt")
pretest_D <- read.delim("U:/surfdriveRU/Thesis Experiment/Stimuli/pre-test-zeisoveral.txt")
raw_litdo_23 <- raw_litdo_23[-c(1, 2, 3),]
raw_litdo_23$simulation <- pretest_D$TOTAAL
raw_litdo_23$concreteness <- pretest_D$CONCRETENESS
raw_litdo_23$lemmafrequency <- pretest_D$LEXFREQ2
raw_litdo_23$wordlength <- pretest_D$WORDLENGTH

raw_litdo_23$IA_FIRST_RUN_DWELL_TIME[raw_litdo_23$IA_FIRST_RUN_DWELL_TIME == "."] <- NaN
raw_litdo_23$IA_FIRST_FIXATION_DURATION[raw_litdo_23$IA_FIRST_FIXATION_DURATION == "."] <- NaN

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
simPC1 <- simfactors$TC1[simfactors$Participant==23 & simfactors$Condition_new=="PAST_M"]
simPC2 <- simfactors$TC2[simfactors$Participant==23 & simfactors$Condition_new=="PAST_M"]
simPC3 <- simfactors$TC3[simfactors$Participant==23 & simfactors$Condition_new=="PAST_M"]
simPC4 <- simfactors$TC4[simfactors$Participant==23 & simfactors$Condition_new=="PAST_M"]

appPC1 <- appfactors$TC1[appfactors$Participant==23 & appfactors$Condition_new=="PAST_M"]
appPC2 <- appfactors$TC2[appfactors$Participant==23 & appfactors$Condition_new=="PAST_M"]
appPC3 <- appfactors$TC3[appfactors$Participant==23 & appfactors$Condition_new=="PAST_M"]


PAST_M_23 <- data.frame(Subject, tense, version, raw_litbm_23$TRIAL_INDEX, raw_litbm_23$IA_LABEL, raw_litbm_23$simulation, raw_litbm_23$concreteness, raw_litbm_23$lemmafrequency, raw_litbm_23$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litbm_23$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litbm_23$IA_FIRST_FIXATION_DURATION)))
names(PAST_M_23) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Presented in present, original tense present -> PRES_O
tense <- "Present"
version <- "Original"
simPC1 <- simfactors$TC1[simfactors$Participant==23 & simfactors$Condition_new=="PRES_O"]
simPC2 <- simfactors$TC2[simfactors$Participant==23 & simfactors$Condition_new=="PRES_O"]
simPC3 <- simfactors$TC3[simfactors$Participant==23 & simfactors$Condition_new=="PRES_O"]
simPC4 <- simfactors$TC4[simfactors$Participant==23 & simfactors$Condition_new=="PRES_O"]

appPC1 <- appfactors$TC1[appfactors$Participant==23 & appfactors$Condition_new=="PRES_O"]
appPC2 <- appfactors$TC2[appfactors$Participant==23 & appfactors$Condition_new=="PRES_O"]
appPC3 <- appfactors$TC3[appfactors$Participant==23 & appfactors$Condition_new=="PRES_O"]

PRES_O_23 <- data.frame(Subject, tense, version, raw_litao_23$TRIAL_INDEX, raw_litao_23$IA_LABEL, raw_litao_23$simulation, raw_litao_23$concreteness, raw_litao_23$lemmafrequency, raw_litao_23$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litao_23$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litao_23$IA_FIRST_FIXATION_DURATION)))
names(PRES_O_23) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Presented in present, original tense past -> PRES_M
tense <- "Present"
version <- "Manipulated"
simPC1 <- simfactors$TC1[simfactors$Participant==23 & simfactors$Condition_new=="PRES_M"]
simPC2 <- simfactors$TC2[simfactors$Participant==23 & simfactors$Condition_new=="PRES_M"]
simPC3 <- simfactors$TC3[simfactors$Participant==23 & simfactors$Condition_new=="PRES_M"]
simPC4 <- simfactors$TC4[simfactors$Participant==23 & simfactors$Condition_new=="PRES_M"]

appPC1 <- appfactors$TC1[appfactors$Participant==23 & appfactors$Condition_new=="PRES_M"]
appPC2 <- appfactors$TC2[appfactors$Participant==23 & appfactors$Condition_new=="PRES_M"]
appPC3 <- appfactors$TC3[appfactors$Participant==23 & appfactors$Condition_new=="PRES_M"]

PRES_M_23 <- data.frame(Subject, tense, version, raw_litcm_23$TRIAL_INDEX, raw_litcm_23$IA_LABEL, raw_litcm_23$simulation, raw_litcm_23$concreteness, raw_litcm_23$lemmafrequency, raw_litcm_23$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litcm_23$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litcm_23$IA_FIRST_FIXATION_DURATION)))
names(PRES_M_23) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Presented in past, original tense past -> PAST_O
tense <- "Past"
version <- "Original"
simPC1 <- simfactors$TC1[simfactors$Participant==23 & simfactors$Condition_new=="PAST_O"]
simPC2 <- simfactors$TC2[simfactors$Participant==23 & simfactors$Condition_new=="PAST_O"]
simPC3 <- simfactors$TC3[simfactors$Participant==23 & simfactors$Condition_new=="PAST_O"]
simPC4 <- simfactors$TC4[simfactors$Participant==23 & simfactors$Condition_new=="PAST_O"]

appPC1 <- appfactors$TC1[appfactors$Participant==23 & appfactors$Condition_new=="PAST_O"]
appPC2 <- appfactors$TC2[appfactors$Participant==23 & appfactors$Condition_new=="PAST_O"]
appPC3 <- appfactors$TC3[appfactors$Participant==23 & appfactors$Condition_new=="PAST_O"]

PAST_O_23 <- data.frame(Subject, tense, version, raw_litdo_23$TRIAL_INDEX, raw_litdo_23$IA_LABEL, raw_litdo_23$simulation, raw_litdo_23$concreteness, raw_litdo_23$lemmafrequency, raw_litdo_23$wordlength, simPC1, simPC2, simPC3, simPC4, appPC1, appPC2, appPC3, as.numeric(as.character(raw_litdo_23$IA_FIRST_RUN_DWELL_TIME)), as.numeric(as.character(raw_litdo_23$IA_FIRST_FIXATION_DURATION)))
names(PAST_O_23) <- c("subject", "tense", "version", "page","word","simulation", "conreteness", "lemmafrequency", "wordlength", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "gazedur","fixdur")

# Remove raw data frames
keep(PAST_M_23, PRES_M_23, PAST_O_23, PRES_O_23, sure = T)

## "Remove" first word of each page, except on the first page where the title was the first word (but the title has already been removed) ##
PAST_M_23$gazedur[!duplicated(PAST_M_23$page) & PAST_M_23$page > 1] <- NaN
PAST_M_23$fixdur[!duplicated(PAST_M_23$page) & PAST_M_23$page > 1] <- NaN

PRES_O_23$gazedur[!duplicated(PRES_O_23$page) & PRES_O_23$page > 1] <- NaN
PRES_O_23$fixdur[!duplicated(PRES_O_23$page) & PRES_O_23$page > 1] <- NaN

PRES_M_23$gazedur[!duplicated(PRES_M_23$page) & PRES_M_23$page > 1] <- NaN
PRES_M_23$fixdur[!duplicated(PRES_M_23$page) & PRES_M_23$page > 1] <- NaN

PAST_O_23$gazedur[!duplicated(PAST_O_23$page) & PAST_O_23$page > 1] <- NaN
PAST_O_23$fixdur[!duplicated(PAST_O_23$page) & PAST_O_23$page > 1] <- NaN

## "Remove" AoI > 1200 or < 50 ms ##

prevalues_gaze <- sum(PAST_O_23$gazedur > 0, na.rm=T)+sum(PAST_M_23$gazedur > 0, na.rm=T)+
  sum(PRES_M_23$gazedur > 0, na.rm=T)+sum(PRES_O_23$gazedur > 0, na.rm=T)

prevalues_fix <- sum(PAST_O_23$fixdur > 0, na.rm=T)+sum(PAST_M_23$fixdur > 0, na.rm=T)+
  sum(PRES_M_23$fixdur > 0, na.rm=T)+sum(PRES_O_23$fixdur > 0, na.rm=T)

PAST_M_23$gazedur[PAST_M_23$gazedur  > 1200 | PAST_M_23$gazedur < 50] <- NaN
PAST_M_23$fixdur[PAST_M_23$fixdur > 1200 | PAST_M_23$fixdur < 50] <- NaN

PRES_O_23$gazedur[PRES_O_23$gazedur > 1200 | PRES_O_23$gazedur < 50] <- NaN
PRES_O_23$fixdur[PRES_O_23$fixdur > 1200 | PRES_O_23$fixdur < 50] <- NaN

PRES_M_23$gazedur[PRES_M_23$gazedur > 1200 | PRES_M_23$gazedur < 50] <- NaN
PRES_M_23$fixdur[PRES_M_23$fixdur > 1200 | PRES_M_23$fixdur < 50] <- NaN

PAST_O_23$gazedur[PAST_O_23$gazedur > 1200 | PAST_O_23$gazedur < 50] <- NaN
PAST_O_23$fixdur[PAST_O_23$fixdur > 1200 | PAST_O_23$fixdur < 50] <- NaN

postvalues_gaze <- sum(PAST_O_23$gazedur > 0, na.rm=T)+sum(PAST_M_23$gazedur > 0, na.rm=T)+
  sum(PRES_M_23$gazedur > 0, na.rm=T)+sum(PRES_O_23$gazedur > 0, na.rm=T)

postvalues_fix <- sum(PAST_O_23$fixdur > 0, na.rm=T)+sum(PAST_M_23$fixdur > 0, na.rm=T)+
  sum(PRES_M_23$fixdur > 0, na.rm=T)+sum(PRES_O_23$fixdur > 0, na.rm=T)

dataloss_fix <- prevalues_fix-postvalues_fix
dataloss_fix_percentage <- (1-(postvalues_fix/prevalues_fix))*100
dataloss_gaze <- prevalues_gaze-postvalues_gaze
dataloss_gaze_percentage <- (1-(postvalues_gaze/prevalues_gaze))*100

## Combine four stories/conditions
subject23 <- rbind(PAST_O_23, PAST_M_23, PRES_O_23, PRES_M_23)

## Add covariates
load(file = "U:/surfdriveRU/Thesis analyse/ART.Rda")
subject23$ART <- ART$ART[ART$Participant == 23]
subject23$ReadingExperience <- ART$A1[ART$Participant == 23]


keep(subject23, sure = T)

## Add stories
subject23$story[subject23$tense=="Present" & subject23$version=="Original"] <- "HetIsMuis"
subject23$story[subject23$tense=="Present" & subject23$version=="Manipulated"] <- "DeInvaller"
subject23$story[subject23$tense=="Past" & subject23$version=="Original"] <- "ZeIsOveral"
subject23$story[subject23$tense=="Past" & subject23$version=="Manipulated"] <- "HoeDeWolvenDansen"

save(subject23,file="U:/surfdriveRU/Thesis analyse/LMM analyse/subject23.Rda")

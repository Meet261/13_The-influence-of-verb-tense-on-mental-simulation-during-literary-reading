## Combine eye-tracking data of all participants ##

load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject01.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject02.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject03.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject04.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject05.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject06.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject07.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject08.Rda")
#load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject09.Rda") #rejected because of odd reading behavior
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject10.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject11.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject12.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject13.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject14.Rda")
#load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject15.Rda") #rejected because of technical malfunctioning due to contact lenses
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject16.Rda")
#load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject17.Rda") #rejected because of technical malfunctioning due to eye lashes
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject18.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject19.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject20.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject21.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject22.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject23.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject24.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject25.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject26.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject27.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject28.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject29.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject30.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject31.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject32.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject33.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject34.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject35.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject36.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject37.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject38.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject39.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject40.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject41.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject42.Rda")
load("U:/surfdriveRU/Thesis analyse/LMM analyse/subject43.Rda")

eyetrackingdata <- rbind(subject01, subject02, subject03, subject04, subject05, subject06, subject07, subject08, subject10, subject11,
                         subject12, subject13, subject14, subject16, subject18, subject19, subject20, subject21, subject22, subject23, 
                         subject24, subject25, subject26, subject27, subject28, subject29, subject30, subject31, subject32, subject33, 
                         subject34, subject35, subject36, subject37, subject38, subject39, subject40, subject41, subject42, subject43)

eyetrackingdata$conreteness[eyetrackingdata$conreteness == NaN] <- NA
eyetrackingdata$conreteness[eyetrackingdata$conreteness == "#N/A"] <- NA
eyetrackingdata$lemmafrequency[eyetrackingdata$lemmafrequency == NaN] <- NA
eyetrackingdata$lemmafrequency[eyetrackingdata$lemmafrequency == "#N/A"] <- NA

# Add position of word in sentence (pre-fabricated list made in Python)
positionlist <- read.delim("U:/surfdriveRU/Thesis analyse/LMM analyse/positionlist.txt", header = FALSE)
eyetrackingdata$position <- positionlist$V1

# Reorder and rename variables
eyetrackingdata <- eyetrackingdata[c("subject", "story", "tense", "version", "page", "position", "word", "simulation", "conreteness", "lemmafrequency", "wordlength", "gazedur", "fixdur", "simPC1", "simPC2", "simPC3", "simPC4", "appPC1", "appPC2", "appPC3", "ART", "ReadingExperience")]
names(eyetrackingdata)[names(eyetrackingdata) == "gazedur"] <- "first_run_dwell_time"
names(eyetrackingdata)[names(eyetrackingdata) == "fixdur"] <- "first_fixation_duration"
names(eyetrackingdata)[names(eyetrackingdata) == "conreteness"] <- "concreteness"

# Remove variables that are irrelevant for further analysis
eyetrackingdata$simPC1 <- NULL
eyetrackingdata$simPC2 <- NULL
eyetrackingdata$simPC3 <- NULL
eyetrackingdata$simPC4 <- NULL
eyetrackingdata$appPC1 <- NULL
eyetrackingdata$appPC2 <- NULL
eyetrackingdata$appPC3 <- NULL

# Save output
save(eyetrackingdata,file="U:/surfdriveRU/Thesis analyse/LMM analyse/eyetrackingdata.Rda")
write.table(eyetrackingdata, "U:/surfdriveRU/Thesis analyse/LMM analyse/eyetrackingdata.txt", sep="\t", row.names = F)

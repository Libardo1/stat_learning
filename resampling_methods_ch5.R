#Loading Data
prostate_cancer <- read.table("~/Documents/github/stat_learning/data/prostate_caner.dat", quote="\"")
colnames(prostate_cancer)[1] <- "idCode"
colnames(prostate_cancer)[2] <- "tumor"
colnames(prostate_cancer)[3] <- "age"
colnames(prostate_cancer)[4] <- "race"
colnames(prostate_cancer)[5] <- "rectalExamResult"
colnames(prostate_cancer)[6] <- "capsularInvolvement"
colnames(prostate_cancer)[7] <- "antigenValue"
colnames(prostate_cancer)[8] <- "tumorVolume"
colnames(prostate_cancer)[1] <- "gleasonScore"


##################################################################
# Titel:      On the L1-Acquisition of Amplification in English - Part 2
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2019-02-08
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2019. "On the L1-Acquisition 
#             of Amplification in English - Part 2",
#             unpublished R script, The University of Queensland.
###############################################################
rm(list=ls(all=T))                                      # clean current workspace
setwd("D:\\Uni\\Projekte\\02-Intensification\\Amp1L")   # set wd
# activate packages
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(tibble)
source("D:\\R/mlr.summary.R")
source("D:\\R/blr.summary.R")
source("D:\\R/meblr.summary.R")
source("D:\\R/ModelFittingSummarySWSDGLM.R") # for Mixed Effects Model fitting (step-wise step-up): Binary Logistic Mixed Effects Models
source("D:\\R/ModelFittingSummarySWSUGLM.R") # for Mixed Effects Model fitting (step-wise step-down): Binary Logistic Mixed Effects Models
source("D:\\R/ModelFittingSummarySWSULogReg.R") # for Fixed Effects Model fitting: Binary Logistic Models
###############################################################
# Setting options
options(stringsAsFactors = F)
# define image directors
imageDirectory<-"images"
# Specify pathnames of the corpra
bio.path <- "D:\\Uni\\Projekte\\02-Intensification\\07IntensifierAcquisition\\IntAcqJCLAD/biowc.txt"
int.path <- "int.txt"
###############################################################
### ---               PART TWO
###############################################################
###                   START
###############################################################
# read in data
bio <- read.table(bio.path, sep = "\t", header=TRUE)
int <- read.table(int.path, sep = "\t", header=TRUE)
# extract examples
exp <- int[int$absint == 1, ]
# join data
int <- int %>%
  left_join(bio, by = c("file", "spk")) %>%
  select(file, visit, styp, spk, age, gender, wc, status, 
         pre, post, prepos, prelex, tokenlex, absint) %>%
  mutate(Age = str_replace_all(age, "\\..*", "")) %>%
  select(-age) %>%
  mutate(Age_year = str_replace_all(Age, ";.*", "")) %>%
  mutate(month = as.numeric(str_replace_all(Age, ".*;", ""))/1.2)%>%
  mutate(month = round(month, 1)) %>%
  mutate(month = str_replace_all(month, "\\.", "")) %>%
  mutate(Age_decimal = paste(Age_year, ".", month, sep = "")) %>%
  mutate(Age_decimal = str_replace_all(Age_decimal, "NA.NA", "NA")) %>%
  rename(PreviousContext = pre) %>%
  rename(FollowingContext = post) %>%
  rename(Speaker = spk) %>%
  rename(Visit = visit) %>% 
  rename(SituationType = styp) %>% 
  rename(Amplified = absint) %>% 
  rename(Gender = gender)  %>% 
  rename(File = file) %>% 
  select(-month)
# inspect data
head(int)

# remove all speakers who are neither child or mother
BRO <- c("BR1", "BR2", "BR3", "BRO", "BRO1", "BRO2")
CHI <- c("CHI")
FAM <- c("ANT", "AUN", "CO3", "COS", "COU", "COU2", "COU3", "GFA", 
         "GMA", "GPA", "GRA", "GRF", "GRM", "UNC")
FAT <- c("FAT")
MOT <- c("MOT")
rmv <- c("ADU", "ALX", "BAB", "BFD", "BRI", "CAL", "CLI", "EX1", "EX2", 
         "FE1", "FE2", "FE3", "FEM", "FIA", "FR1", "FRE", "FRI", "INV", 
         "INV1", "INV2", "INV3", "JAK", "JES", "KID", "LIS", "MA1", "MA2", 
         "MAL", "MAN", "MAR", "MIA", "PLA", "SAN", "SHE", "SS", "STE", 
         "VIS", "WOM", "XXX")
SIS <- c("SI1", "SI2", "SI3", "SIB", "SIS", "SIS1", "SIS2", "SIS3")
int$Speaker <- ifelse(int$Speaker %in% BRO, "BRO",
                       ifelse(int$Speaker %in% CHI, "CHI",
                       ifelse(int$Speaker %in% FAM, "FAM",
                       ifelse(int$Speaker %in% FAT, "FAT",
                       ifelse(int$Speaker %in% MOT, "MOT",
                       ifelse(int$Speaker %in% SIS, "SIS", "rmv")))))) 
nrow(int)

data <- int %>%
  filter(Speaker != "rmv") %>%
  mutate(Age_year = factor(Age_year))
nrow(data)

# recode SituationType
data$SituationType_categorical <- ifelse(data$SituationType == "br", "formal",
              ifelse(data$SituationType == "tp", "informal",
              ifelse(data$SituationType == "mt", "informal",
              ifelse(data$SituationType == "er", "formal",
              ifelse(data$SituationType == "et", "formal",
              ifelse(data$SituationType == "lw", "formal",
              ifelse(data$SituationType == "re", "formal",
              ifelse(data$SituationType == "md", "formal", data$SituationType))))))))
# remove SituationType md and re from data (too few: md = 368, re = 325)
data <- data %>%
  # remove rare situation types
  filter(SituationType != "re") %>%
  filter(SituationType != "md") %>%
  # recode SituationType
  mutate(SituationType = str_replace_all(SituationType, "tp", "toy play")) %>% 
  mutate(SituationType = str_replace_all(SituationType, "mt", "meal time")) %>% 
  mutate(SituationType = str_replace_all(SituationType, "er", "elicited report")) %>% 
  mutate(SituationType = str_replace_all(SituationType, "et", "experimental task")) %>% 
  mutate(SituationType = str_replace_all(SituationType, "br", "book reading")) %>% 
  mutate(SituationType = str_replace_all(SituationType, "lw", "letter writing"))  %>% 
  # extract Variant
  mutate(Variant = ifelse(Amplified == "0", "0", tolower(prelex)))   %>% 
  # rename columns
  rename(Adjective = tokenlex) %>%
  rename(Words = wc) %>%
  # clean Adjectives
  mutate(Adjective = tolower(Adjective)) %>%
  # recode SituationType
  mutate(Status = str_replace_all(status, "pred", "Predicative")) %>% 
  mutate(Status = str_replace_all(Status, "attr", "Attributive"))  %>% 
  # remove columns
  select(-prelex, -status)
# recode age and visit 
data <- data %>%
  mutate(AgeGroup = ifelse(Age_year == "3" | Age_year == "4" , "3-4", 
                           ifelse(Age_year == "5" | Age_year == "6" , "5-6", 
                           ifelse(Age_year == "7" | Age_year == "8" , "7-8",
                           ifelse(Age_year == "9" | Age_year == "10" , "9-10", Age_year))))) %>%
  mutate(Visit = ifelse(Visit == "hv1", "Visit1", 
                        ifelse(Visit == "hv2", "Visit2",
                        ifelse(Visit == "hv3", "Visit3",
                        ifelse(Visit == "hv5", "Visit5",
                        ifelse(Visit == "hv7", "Visit7", Visit))))))
# factorize variables
data <- data %>%
  mutate(File = factor(File),
         Visit = factor(Visit),
         SituationType = factor(SituationType),
         Speaker = factor(Speaker),
         Gender = factor(Gender),
         SituationType_categorical = factor(SituationType_categorical),
         Status = factor(Status),
         AgeGroup = factor(AgeGroup))
# inspect data
str(data)

###############################################################
# define forms that require removal
sups <- c(".*most.*", ".*more.*") 
negs <- c(".*not.*", ".*never.*", ".*n't.*")
downtoners <- c(".*sort/.*", ".*kind/.*", ".* bit/.*", ".*somewhat.*", ".*fairly.*", 
                ".*rather.*", ".*reasonably.*", ".*slightly.*", ".*comparatively.*", ".*semi.*", 
                ".*relatively.*", ".*little.*", ".*somehow.*", ".*almost.*", ".*partly.*", 
                ".*hardly.*", ".* less.*", ".*barely.*", ".* just/.*")
specialforms <- c(".* too.*", ".*quite.*")
PostContextdowntoners <- c(".*enough.*")
nonpropadj <- c("only", "other", "much", "many", "cheaper", "cheaperr", "bests", "larger", "like", "morer", "uhm", "uhr")
# find items to be removed
supsidx <- unique(grep(paste(sups,collapse="|"), data$PreviousContext, value=F))
negsidx <- unique(grep(paste(negs,collapse="|"), data$PreviousContext, value=F))
downtonersidx <- unique(grep(paste(downtoners,collapse="|"), data$PreviousContext, value=F))
specialformsidx <- unique(grep(paste(specialforms,collapse="|"), data$PreviousContext, value=F))
PostContextdowntonersidx <- unique(grep(paste(PostContextdowntoners,collapse="|"), data$FollowingContext, value=F))
nonpropadjidx <- unique(grep(paste(nonpropadj,collapse="|"), data$Adjective, value=F))
# combine indices
idxs <- unique(c(supsidx, negsidx, downtonersidx, specialformsidx, PostContextdowntonersidx, nonpropadjidx))
# remove forms that requhcie removal
data <- data[-idxs,]
# remove empty values
data <- data[!data$Variant == "", ]
# inspect data
nrow(data)

###############################################################
#                       WARNING
# remove items basde on frequency of amplification
pintadjtb <- table(data$Adjective, data$Variant)
pintadjtb <- pintadjtb[,2:ncol(pintadjtb)]
pintadjtb2 <- apply(pintadjtb, 1, function(x){
  x <- ifelse(x > 1, 1, x)})
pintadjtb3 <- colSums(pintadjtb2)
# never amplified
neveramplified <- names(pintadjtb3)[which(pintadjtb3 == 0 )]
onceamplified <- names(pintadjtb3)[which(pintadjtb3 == 1 )]
twiceamplified <- names(pintadjtb3)[which(pintadjtb3 > 1 )]
# perform removal
data <- data %>%
  # remove adjectives that were never amplified
  mutate(Remove = ifelse(Adjective %in% neveramplified, "remove", "keep")) %>%
  filter(Remove == "keep") %>%
  # remove adjectives that were once amplified
  mutate(Remove = ifelse(Adjective %in% onceamplified, "remove", "keep")) %>%
  filter(Remove == "keep") %>%
  # remove adjectives that were twice amplified
  #mutate(Remove = ifelse(Adjective %in% twiceamplified, "remove", "keep")) %>%
  #filter(Remove == "keep") %>%
  select(-Remove)
# remove superfluous columns
data <- data %>%
  select(-PreviousContext, - FollowingContext, -prepos)
nrow(data)

# remove hungry from data set
data <- data %>%
  filter(Adjective != "hungry")
nrow(data)

# remove speakers other than child and mother
data <- data[data$Speaker == "CHI" | data$Speaker == "MOT", ]
# remove rare age brackets
data <- droplevels( data[-which(data$Age_year == "11"), ] )
data <- droplevels( data[-which(data$Age_year == "12"), ] )
###############################################################
# create data of children only
chint <- data %>%
  filter(Speaker == "CHI")
# create data of mothers only
motint <- data %>%
  filter(Speaker == "MOT")
##############################################################
# tabulate data
ftchint <- table(chint$Variant)
ftchint <- ftchint[order(ftchint, decreasing = T)]
ftchintp <- round(ftchint/sum(ftchint)*100, 2)
Table01 <- data.frame(ftchint, ftchintp)
Table01[,3] <- NULL
colnames(Table01) <- c("Variant", "AbsoluteFrequency", "Percent")
Table01$PercentOfAmplified <- c(0, round(Table01[2:nrow(Table01),2]/sum(Table01[2:nrow(Table01),2])*100, 2))
Table01

# save data to disc
write.table(Table01, "Table01.txt", sep = "\t", row.names = F)
###############################################################
#                Visualization
###############################################################
# tabulate data
p0d <- chint %>%
  select(Speaker, AgeGroup, Amplified) %>%
  filter(Speaker == "CHI") %>%
  group_by(AgeGroup)  %>%
  mutate(AmplifiedSum = sum(Amplified),
         Adjectives = n(),
         Percent = round(AmplifiedSum/Adjectives*100, 2))  %>%
  select(AgeGroup, Percent, Adjectives)
p0d <- unique(p0d)
p0d

# plot data
png("images/AmplifiedAge_Bar.png") # save plot
barplot(p0d$Percent, col = "grey80", ylim = c(-4, 20), 
        ylab = "Percent (amplified adjectives)",
        xlab = "Age", axes = T, beside = T,
        main = "Children's use of Amplifiers by Age")
text(seq(0.7, 4.3, 1.2), p0d$Percent + 2, cex = .8, labels = p0d$Percent)
text(seq(0.7, 4.3, 1.2), -1.8, 
     labels = c(paste("N(slots):\n", p0d$Adjectives[1], sep = ""), p0d$Adjectives[2:length(p0d$Adjectives)]), cex = .8)
box()
dev.off()
###############################################################
# prepare data p1
p1d <- chint%>%
  select(AgeGroup, Amplified)%>%
  rename(Age = AgeGroup)%>%
  group_by(Age)%>%
  mutate(Amplified = Amplified*100)
p1dmeans <- tapply(p1d$Amplified, p1d$Age, mean)
table(chint$AgeGroup)

p1 <- ggplot(p1d, aes(Age, Amplified)) +
  stat_summary(fun.y = mean, geom = "point", size = 1) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = .5) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_set(theme_light(base_size = 20)) +
  theme(legend.position="none") +
  labs(x = "Age", y = "Percent (amp. adjectives)", colour = "Age") +
  scale_color_manual(values = c("grey30")) +
  geom_text(mapping = NULL, 
            label = c(paste("N(slots):\n", p0d$Adjectives[1], sep = "")), 
            x = 1, y = 1, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "1407", 
            x = 2, y = 1, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "955", 
            x = 3, y = 1, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "860", 
            x = 4, y = 1, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(p1dmeans[1], 2), sep = ""), 
            x = 1, y = p1dmeans[1] +5, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(p1dmeans[2], 2), sep = ""), 
            x = 2, y = p1dmeans[2] +5, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(p1dmeans[3], 2), sep = ""), 
            x = 3, y = p1dmeans[3] +5, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(p1dmeans[4], 2), sep = ""), 
            x = 4, y = p1dmeans[4] +6, colour = "grey30", size = 5) +
  theme(legend.position="none") 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"AmplifiedAge.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p1

########################################
# restructure data
p7d <- chint %>%
  select(AgeGroup, Variant) %>%
  filter(Variant != "0")
amps <- c("very", "really",   "so", "real")
p7d$Variant <- ifelse(p7d$Variant %in% amps, p7d$Variant, "other")

p7d <- p7d %>%
  group_by(AgeGroup) %>%
  count(AgeGroup, Variant) %>%
  mutate(freq = round((n / sum(n)*100), 1)) %>%
  select(AgeGroup, Variant, freq) %>%
  spread(Variant, freq) %>%
  rownames_to_column() %>%
  rename(Age = rowname)
p7d$Age <- as.double(p7d$Age)
p7d

# plot
p7 <- ggplot(p7d, aes(Age, very)) +  
  geom_smooth(aes(y = very, color = "very", linetype = "very")) +  
  geom_smooth(aes(y = so, color = "so", linetype = "so")) +   
  geom_smooth(aes(y = really, color = "really", linetype = "really")) +   
  geom_smooth(aes(y = real, color = "real", linetype = "real")) +   
  geom_smooth(aes(y = other, color = "other", linetype = "other")) +   
  guides(color=guide_legend(override.aes=list(fill=NA))) +   
  scale_linetype_manual(values=c("dashed", "dotted", "dotted", "solid", "twodash"), 
                        name= "", breaks = c("other", "real", "really",  "so", "very"),
                        labels = c("other", "real", "really",  "so", "very")) + 
  scale_colour_manual(values=c("darkblue", "gold", "indianred4", "gray20", "gray70"), 
                      name="", breaks= c("other", "real", "really",  "so", "very"),  
                      labels = c("other", "real", "really",  "so", "very")) +
  theme_set(theme_light(base_size = 20)) +
  theme(legend.position="top") +  
  scale_x_continuous(breaks=c(1, 2, 3, 4), 
                     labels=c("3-4", "5-6", "7-8", "9-10")) +
  scale_y_continuous(name="Percent", limits=c(0, 75)) + 
  labs(x = "Age", y = "Percent")
ggsave(file = paste(imageDirectory,"PercentVarAge.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p7

###############################################################
# prepare data p2
ag <- chint %>%
  select(File, AgeGroup)
ag <- unique(ag)
pmd2 <- motint %>%
  left_join(ag, by = "File") %>%
  select(-Age) %>%
  rename(Age = AgeGroup.y) %>%
  select(Age, Amplified, SituationType) %>%
  mutate(Amplified = Amplified*100)
texttypemeans <- round(tapply(pmd2$Amplified, pmd2$Age, mean), 1)
table(pmd2$Age)

p2 <- ggplot(pmd2, aes(Age, Amplified, colour = Age)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1.25) +
  coord_cartesian(ylim = c(0, 20)) +
  theme_set(theme_bw(base_size = 20)) +
  theme(legend.position="none") +
  labs(x = "Age of child", y = "Percent (amp. adjectives slots)", colour = "agecat2") +
  scale_color_manual(values = c("grey30", "grey30", "grey30", "grey30")) +
  geom_text(mapping = NULL, label = "N (slots) = 6,154", 
            x = 1, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "3,177", 
            x = 2, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "1194", 
            x = 3, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "955", 
            x = 4, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[1], 2), sep = ""), 
            x = 1, y = texttypemeans[1] +5, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[2], 2), sep = ""), 
            x = 2, y = texttypemeans[2] +5, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[3], 2), sep = ""), 
            x = 3, y = texttypemeans[3] +5, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[4], 2), sep = ""), 
            x = 4, y = texttypemeans[4] +6, colour = "grey30", size = 5) +
  theme(legend.position="none") 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"AmpAgeMot.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p2

########################################
# restructure data
cd <- chint %>%
  select(File, AgeGroup)
p8d <- motint %>%
  left_join(cd, by = c("File")) %>%
  select(-Age) %>%
  rename(Age = AgeGroup.y) %>%
  select(Variant, Age) %>%
  filter(Variant != "0") %>%
  na.omit()
amps <- c("very", "really",   "so", "real")
p8d$Variant <- ifelse(p8d$Variant %in% amps, p8d$Variant, "other")
library(tidyr)
library(tibble)
p8d <- p8d %>%
  group_by(Age) %>%
  count(Age, Variant) %>%
  mutate(freq = round((n / sum(n)*100), 1)) %>%
  select(Age, Variant, freq) %>%
  spread(Variant, freq) %>%
  rownames_to_column() %>%
  rename(AgeGroup = rowname)
p8d$AgeGroup <- as.double(p8d$AgeGroup)
p8d

# plot
p8 <- ggplot(p8d, aes(AgeGroup, very)) +  
  geom_smooth(aes(y = very, color = "very", linetype = "very")) +  
  geom_smooth(aes(y = so, color = "so", linetype = "so")) +   
  geom_smooth(aes(y = really, color = "really", linetype = "really")) +   
  geom_smooth(aes(y = real, color = "real", linetype = "real")) +   
  geom_smooth(aes(y = other, color = "other", linetype = "other")) +   
  guides(color=guide_legend(override.aes=list(fill=NA))) +   
  scale_linetype_manual(values=c("dashed", "dotted", "dotted", "solid", "twodash"), 
                        name= "", breaks = c("other", "real", "really",  "so", "very"),
                        labels = c("other", "real", "really",  "so", "very")) + 
  scale_colour_manual(values=c("darkblue", "gold", "indianred4", "gray20", "gray70"), 
                      name="", breaks= c("other", "real", "really",  "so", "very"),  
                      labels = c("other", "real", "really",  "so", "very")) +
  theme_set(theme_light(base_size = 20)) +
  theme(legend.position="top") +  
  scale_x_continuous(breaks=c(1, 2, 3, 4), 
                     labels=c("3-4", "5-6", "7-8", "9-10")) +
  scale_y_continuous(name="Percent", limits=c(0, 75)) + 
  labs(x = "Age of Child", y = "Percent")
ggsave(file = paste(imageDirectory,"PercentVarAgeMot.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p8

###############################################################
# restructure data
p2d <- chint %>%
  group_by(AgeGroup, SituationType) %>%
  summarize(Adjectives = n(), Amplifiers = sum(Amplified)) %>%
  select(AgeGroup, SituationType, Adjectives, Amplifiers) %>%
  mutate(Percent = round(Amplifiers/Adjectives*100,1)) %>%
  filter(SituationType != "experimental task") %>%
  filter(SituationType != "letter writing") %>%
  rename(Age = AgeGroup)

# plot
p2 <- ggplot(p2d, aes(Age, Percent, fill = SituationType)) +  
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_set(theme_light(base_size = 20)) + 
  scale_fill_manual(values=c("gray20", 
                             "gray40",
                             "gray60",
                             "gray80")) + 
  geom_text(aes(label=Percent), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5)
ggsave(file = paste(imageDirectory,"PercentAmpSitAge.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p2

########################################
# restructure data
p3d <- pd %>%
  group_by(visit, styp) %>%
  summarize(Adjectives = n(), Amplifiers = sum(absint)) %>%
  select(visit, styp, Adjectives, Amplifiers) %>%
  mutate(Percent = round(Amplifiers/Adjectives*100,1)) %>%
  rename(SituationType = styp, HomeVisit = visit)
# plot
p3 <- ggplot(p3d, aes(HomeVisit, Percent, fill = SituationType)) +  
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("gray20", 
                             "gray35",
                             "gray50",
                             "gray65",
                             "gray80")) + 
  geom_text(aes(label=Percent), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) + 
  theme_bw()     
ggsave(file = paste(imageDirectory,"PercentAmpSitVisit.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p3
########################################
# restructure data
p4d <- pd %>%
  filter(styp == "meal time") %>%
  group_by(agecat2) %>%
  summarize(Adjectives = n(), Amplifiers = sum(absint)) %>%
  select(agecat2, Adjectives, Amplifiers) %>%
  mutate(Percent = round(Amplifiers/Adjectives*100,1)) %>%
  rename(Age = agecat2)
# plot
p4 <- ggplot(p4d, aes(Age, Percent)) +
  scale_fill_manual(values=c("gray80")) +
  geom_bar(stat="identity", position=position_dodge(),
           color = "black", fill = "gray80") + 
  geom_text(aes(label=Percent), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5) + 
  theme_bw()     
ggsave(file = paste(imageDirectory,"PercentAmpAgeMt.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p4

###############################################################
###                    MOT STYP
###############################################################
# prepare data
p5d <- motint
# restructure data
p5d <- p5d %>%
  group_by(agecat2, styp) %>%
  summarize(Adjectives = n(), Amplifiers = sum(absint)) %>%
  select(agecat2, styp, Adjectives, Amplifiers) %>%
  mutate(Percent = round(Amplifiers/Adjectives*100,1)) %>%
  rename(SituationType = styp, Age = agecat2)
# plot
p5 <- ggplot(p5d, aes(Age, Percent, fill = SituationType)) +  
  geom_bar(stat="identity", position=position_dodge()) +
  theme_set(theme_light(base_size = 20)) + 
  scale_fill_manual(values=c("gray20", 
                             "gray35",
                             "gray50",
                             "gray65",
                             "gray80")) + 
  geom_text(aes(label=Percent), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5)
ggsave(file = paste(imageDirectory,"PercentAmpSitMot.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p5

########################################
# restructure data
p6d <- pmd %>%
  group_by(visit, styp) %>%
  summarize(Adjectives = n(), Amplifiers = sum(absint)) %>%
  select(visit, styp, Adjectives, Amplifiers) %>%
  mutate(Percent = round(Amplifiers/Adjectives*100,1)) %>%
  rename(SituationType = styp, HomeVisit = visit)
# plot
p6 <- ggplot(p6d, aes(HomeVisit, Percent, fill = SituationType)) +  
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("gray20", 
                             "gray35",
                             "gray50",
                             "gray65",
                             "gray80")) + 
  geom_text(aes(label=Percent), vjust=1.6, color="white",
            position = position_dodge(0.9), size=3.5) + 
  theme_bw()     
ggsave(file = paste(imageDirectory,"PercentAmpSitVisitMot.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p6
########################################
# restructure data
p7d <- pd %>%
  select(agecat2, pint) %>%
  filter(pint != "0")
amps <- c("very", "really",   "so", "real")
p7d$pint <- ifelse(p7d$pint %in% amps, p7d$pint, "other")
library(tidyr)
library(tibble)
p7d <- p7d %>%
  group_by(agecat2) %>%
  count(agecat2, pint) %>%
  mutate(freq = round((n / sum(n)*100), 1)) %>%
  select(agecat2, pint, freq) %>%
  spread(pint, freq) %>%
  rownames_to_column() %>%
  rename(Age = rowname)
p7d$Age <- as.double(p7d$Age)
p7d

# plot
p7 <- ggplot(p7d, aes(Age, very)) +  
  geom_smooth(aes(y = very, color = "very", linetype = "very")) +  
  geom_smooth(aes(y = so, color = "so", linetype = "so")) +   
  geom_smooth(aes(y = really, color = "really", linetype = "really")) +   
  geom_smooth(aes(y = real, color = "real", linetype = "real")) +   
  geom_smooth(aes(y = other, color = "other", linetype = "other")) +   
  guides(color=guide_legend(override.aes=list(fill=NA))) +   
  scale_linetype_manual(values=c("dashed", "dotted", "dotted", "solid", "twodash"), 
                        name= "", breaks = c("other", "real", "really",  "so", "very"),
                        labels = c("other", "real", "really",  "so", "very")) + 
  scale_colour_manual(values=c("darkblue", "gold", "indianred4", "gray20", "gray70"), 
                      name="", breaks= c("other", "real", "really",  "so", "very"),  
                      labels = c("other", "real", "really",  "so", "very")) +
  theme_set(theme_light(base_size = 20)) +
  theme(legend.position="top") +  
  scale_x_continuous(breaks=c(1, 2, 3, 4), 
                     labels=c("3-4", "5-6", "7-8", "9-10")) +
  scale_y_continuous(name="Percent", limits=c(0, 75)) + 
  labs(x = "Age", y = "Percent")
ggsave(file = paste(imageDirectory,"PercentVarAge.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p7

########################################
# restructure data
cd <- pd %>%
  select(file, agecat2)
p8d <- pmd %>%
  left_join(cd, by = c("file")) %>%
  select(pint, agecat2.y) %>%
  rename(agecat2 = agecat2.y, Variant = pint) %>%
  na.omit() %>%
  filter(Variant != "0")
amps <- c("very", "really",   "so", "real")
p8d$Variant <- ifelse(p8d$Variant %in% amps, p8d$Variant, "other")
library(tidyr)
library(tibble)
p8d <- p8d %>%
  group_by(agecat2) %>%
  count(agecat2, Variant) %>%
  mutate(freq = round((n / sum(n)*100), 1)) %>%
  select(agecat2, Variant, freq) %>%
  spread(Variant, freq) %>%
  rownames_to_column() %>%
  rename(Age = rowname)
p8d$Age <- as.double(p8d$Age)
p8d

# plot
p8 <- ggplot(p8d, aes(Age, very)) +  
  geom_smooth(aes(y = very, color = "very", linetype = "very")) +  
  geom_smooth(aes(y = so, color = "so", linetype = "so")) +   
  geom_smooth(aes(y = really, color = "really", linetype = "really")) +   
  geom_smooth(aes(y = real, color = "real", linetype = "real")) +   
  geom_smooth(aes(y = other, color = "other", linetype = "other")) +   
  guides(color=guide_legend(override.aes=list(fill=NA))) +   
  scale_linetype_manual(values=c("dashed", "dotted", "dotted", "solid", "twodash"), 
                        name= "", breaks = c("other", "real", "really",  "so", "very"),
                        labels = c("other", "real", "really",  "so", "very")) + 
  scale_colour_manual(values=c("darkblue", "gold", "indianred4", "gray20", "gray70"), 
                      name="", breaks= c("other", "real", "really",  "so", "very"),  
                      labels = c("other", "real", "really",  "so", "very")) +
  theme_set(theme_light(base_size = 20)) +
  theme(legend.position="top") +  
  scale_x_continuous(breaks=c(1, 2, 3, 4), 
                     labels=c("3-4", "5-6", "7-8", "9-10")) +
  scale_y_continuous(name="Percent", limits=c(0, 75)) + 
  labs(x = "Age of Child", y = "Percent")
ggsave(file = paste(imageDirectory,"PercentVarAgeMot.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p8

###############################################################
###                     TTR: CHI INT
###############################################################
# plot type-token-ratio of intensifiers against age
chintint <- chint[chint$absint == 1, ]
inttb08 <- table(chintint$prelex, chintint$agecat2)
# calculate token freq
tokf08 <- colSums(inttb08)
# calculate type freq
typf08 <- t(apply(inttb08, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf08 <- colSums(typf08)
# calculate type-token-ratio
chittr08 <- data.frame(1:4, round(typf08/tokf08, 2), typf08, tokf08)
rownames(chittr08) <- c("3-4", "5-6", "7-8", "9-10")
colnames(chittr08) <- c("Age", "TTR", "Types", "Tokens")
chittr08 <- chittr08[chittr08$Tokens > 1,]
# plot data
png("images/ttrchiageint.png") # save plot
plot(chittr08[,1:2], xlab = "Age", ylab = "Type-Token-Ratio (TTR): Intensifiers",
     ylim = c(0,1), pch = 19#,
     #main = "Children's TTRs of intensifiers by age"
)
abline(lm(chittr08[,2]~chittr08[,1]), col="red", lty = 2) # regression line (y~x)
lws08 <- unlist(lowess(chittr08[,2],chittr08[,1]))
lines(lws08[1:length(lws08)/2], col="blue", lty = 3)
legend("topright", inset = .05, c("linear regression line", "smoothed/lowess regression line"),
       horiz = F, lty = c(2,3), col = c("red", "blue"))
grid()
dev.off()
###############################################################
###############################################################
###############################################################
###               TABLE 2
###############################################################
###############################################################
###############################################################
# tabulate data
ftbchinta <- ftable(chint$pint, chint$agecat2)
rwnms <- as.vector(unlist(attr(ftbchinta, "row.vars")))
rownames(ftbchinta) <- rwnms
svrwnms <- as.vector(unlist(attr(ftbchinta, "dimnames")))
ftbchinta <- ftbchinta
rownames(ftbchinta) <- svrwnms
colnames(ftbchinta) <- as.vector(unlist(attr(ftbchinta, "col.vars")))
ptbchinta <- prop.table(ftbchinta, margin=2)*100
ptbchinta

# save data to disc
write.table(ptbchinta, "ptbchinta.txt", sep = "\t", row.names = F)
###############################################################
###                 WARNING
###############################################################
# tabulate data
chintint <- chint[chint$absint == 1,]
ftbchinta <- ftable(chintint$pint, chintint$agecat2)
rwnms <- as.vector(unlist(attr(ftbchinta, "row.vars")))
rownames(ftbchinta) <- rwnms
svrwnms <- as.vector(unlist(attr(ftbchinta, "dimnames")))
ftbchinta <- ftbchinta
rownames(ftbchinta) <- svrwnms
colnames(ftbchinta) <- as.vector(unlist(attr(ftbchinta, "col.vars")))
ptbchintaint <- prop.table(ftbchinta, margin=2)*100
ptbchintaint

# save data to disc
write.table(ptbchintaint, "ptbchintaint.txt", sep = "\t", row.names = F)
###############################################################
# plot data
pretty <- ptbchintaint[3,]
real <- ptbchintaint[4,]
really <- ptbchintaint[5,]
so <- ptbchintaint[6,]
totally <- ptbchintaint[7,]
very <- ptbchintaint[8,]
wicked <- ptbchintaint[10,]
# save plot
png("images/IntsChiMtAttrAge.png",  
    width = 680, height = 480) # save plot
plot(very, col = "black", ylim = c(0,60), type = "l", lty = 1, ann = F,
     lwd = 3, axes = F)
axis(1, at = 1:4, c("3-4", "5-6", "7-8", "9-10"), las = 1)
axis(2,  at = seq(0,60, 10), seq(0,60, 10), las = 2)
mtext("Age", 1, 3, las = 1)
mtext("%", 2, 3, las = 2)
lines(so, col = "black", lty = 2, lwd = 3)
lines(really, col = "black", lty = 3, lwd = 3)
lines(real, col = "gray", lty = 4, lwd = 3)
lines(pretty, col = "gray", lty = 5, lwd = 3)
lines(totally, col = "gray", lty = 6, lwd = 3)
lines(wicked, col = "gray", lty = 7, lwd = 3)
legend(2, 60, inset=.05, title="", bty = "n", cex = .85,
       c("very", "so", "really", "real", "pretty", "totally", "wicked"), lwd = 2, 
       lty = c(1:7), col = c(rep("black", 3), rep("gray", 7)), horiz=F)
grid()
dev.off()
###############################################################
###############################################################
###############################################################
###                     TABLE 3
###############################################################
###############################################################
###############################################################
tb3d <- chint %>%
  group_by(pint) %>%
  summarize(n = n()) %>%
  mutate(freq = round((n / sum(n)*100), 1)) %>%
  arrange(-freq)
tb3d

pintchint <- table(chint$pint)
pinttbchint <- table(chint$pint)[order(table(chint$pint), decreasing = T)]
pintnames <- as.vector(names(pinttbchint))
pintn <- as.vector(pinttbchint)
pintprcnt <- round(pintn/sum(pintn)*100, 2)
pintprcnt2 <-  c(0, round(pintn[2:length(pintn)]/sum(pintn[2:length(pintn)])*100, 2))
pinttbchi <- data.frame(pintnames, pintn, pintprcnt, pintprcnt2)
colnames(pinttbchi) <- c("Intensifier", "TokenFrequency", "PercentageSlots", "PercentageIntensifiers")
pinttbchi <- rbind(pinttbchi, c("Total", sum(as.vector(pinttbchi$TokenFrequency)), "", ""))
rownames(pinttbchi) <- NULL
head(pinttbchi)

# save data to disc
write.table(pinttbchi, "images/pinttbchi.txt", 
            sep = "\t", row.names = F)
###############################################################
chdf <- chint[chint$pint != "0",]
tbadj <- table(chdf$token)
tbadj <- tbadj[order(tbadj, decreasing = T)]
tbadj <- tbadj[which(tbadj > 10)]
freqadj <- names(tbadj)
dffadj <- chdf[chdf$token %in% freqadj,]
dffadj <- dffadj[, c(17, 16, 11)] # age pint adj
tst5 <- table(chint$pint)
infreqint <- names(tst5[which(tst5 < 10)])
dffadj$pint <- as.vector(unlist(sapply(dffadj$pint, function(x){
  x <- ifelse(x %in% infreqint, "other", x) } )))
dffadj <- dffadj[dffadj$pint != "0",]
# tabulate by age
chdf <- ftable(dffadj$token, dffadj$pint, dffadj$agecat2)
chdf

colSums(chdf)

###############################################################
# chdf with rel freqs
freqchdf <- as.vector(unlist(chdf))
freqadjs <- rep(rep(as.vector(unlist(attr(chdf, "row.vars")[1])), 1, each = 6), 4)
freqints <- rep(rep(as.vector(unlist(attr(chdf, "row.vars")[2])), 4), 4)
freqage <- rep(as.vector(unlist(attr(chdf, "col.vars")[1])), 1, each = 24)
length(freqchdf)
length(freqadjs)
length(freqints)
length(freqage)
relfreqdf <- data.frame(freqage, freqadjs, freqints, freqchdf)
relfreqdf$sum <- rep(as.vector(unlist(tapply(relfreqdf$freqchdf, 
                                             list(relfreqdf$freqadjs, relfreqdf$freqage), sum))), 1, each = 6)
relfreqdf$pcnt <- round(relfreqdf$freqchdf/relfreqdf$sum*100, 1)
relfreqdf$pcnt <- as.vector(unlist(sapply(relfreqdf$pcnt, function(x){
  x <- ifelse(is.na(x) == T, 0, x)})))
relfreqdf <- relfreqdf[relfreqdf$pcnt != 0,]
head(relfreqdf)

# save data to disc
write.table(relfreqdf, "intadjagepcnt.txt", 
            sep = "\t", row.names = T)
###############################################################
###############################################################
###############################################################
###               PLOTS
###############################################################
###############################################################
###############################################################
# extract tokenfrequency of intensifiers
pinttokentbchildes <- table(chint$pint, chint$tokenlex)
pinttokentbchildes <- pinttokentbchildes[2:nrow(pinttokentbchildes), ]
pinttokentbchildes <- pinttokentbchildes[rowSums(pinttokentbchildes) > 1, ]
head(pinttokentbchildes)

# extract typefrequency of tokenectives
pinttokentbchildestyp <- t(apply(pinttokentbchildes, 1, function(x) { 
  x <- ifelse(x > 1, 1, x) } ))
#head(pinttokentbchildestyp)

# claculate lexical diversity measure
lexdivchildes <- rowSums(pinttokentbchildestyp)/rowSums(pinttokentbchildes)
lexdivchildes <- lexdivchildes[order(lexdivchildes)]
# plot
png("images/LexDivIntAdjChildes.png",  width = 800, height = 480) # save plot
plot(lexdivchildes, ylim = c(0,1), pch = 20, xlab = "", ylab = "Lexical Diversity Measure", axes = F)
lines(lexdivchildes, lty = 3, col = "red")
axis(1, 1:length(lexdivchildes), names(lexdivchildes), las = 2, cex.axis = .75)
axis(2, seq(0,1,.2), seq(0,1,.2), las = 2, cex.axis = .75)
text(1:length(lexdivchildes), lexdivchildes+.05, round(lexdivchildes, 2), cex = .75)
box()
# end plot
dev.off()
###############################################################
###############################################################
###############################################################
###              SEMANTIC VECTOR SPACE MODELS
###############################################################
###############################################################
###############################################################
# inspect data
head(chint)

# tabulate data
t1 <- tapply(chint$absint, list(chint$tokenlex, chint$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3ire <- t3
t3ire <- t3ire[, 2: ncol(t3ire)]
# remove adjectives that were not intensified
t3ire <- t3ire[rowSums(t3ire) > 0, ]
# save row and column names
colnamesire <- colnames(t3ire)
rownamesire <- rownames(t3ire)
# turn dataframe into matrix
svsmire <- as.matrix(t3ire)
# convert token frequency to type frequency
#svsmire <- apply(svsmire, 1, function(x) { x <- ifelse(x > 1, 1, x) } )
svsmire <- t(svsmire)
#svsmire <- svsmire[, colSums(svsmire) >= 2]
#svsmire <- svsmire[rowSums(svsmire) >= 2, ]
svsmire

# compute expected values
svsmire.exp <- chisq.test(svsmire)$expected
# calculate PMI and PPMI
svsmire.PMI <- log2(svsmire/svsmire.exp)
svsmire.PPMI <- ifelse(svsmire.PMI < 0, 0, svsmire.PMI)
# calculate cosine similarity
svsmire.tmp1 <- svsmire.PPMI
svsmire.cos <- cossim(svsmire.tmp1)
#round(svsmire.cos, 2)
###############################################################
###         CLUSTER ANALYSIS
###############################################################
# find max value that is not 1
svsmire.cos.test <- apply(svsmire.cos, 1, function(x){
  x <- ifelse(x == 1, 0, x) } )
maxval <- max(svsmire.cos.test)
# create distance matrix
svsmire.dist <- 1 - (svsmire.cos/maxval)
clustd <- as.dist(svsmire.dist)
# create distance matrix
#clustd <- dist(svsmire.cos, method = "euclidean") # create distance matrix (eucledian method: not good when dealing with many dimensions)
#clustd <- dist(svsmire.cos, method = "maximum")   # create distance matrix (maximum method: here the difference between points dominates)
clustd <- dist(svsmire.cos, method = "manhattan") # create distance matrix (manhattan method: most popular choice)
#clustd <- dist(svsmire.cos, method = "canberra")  # create distance matrix (canberra method: for count data)
#clustd <- dist(svsmire.cos, method = "binary")    # create distance matrix (binary method: for binary data only!)
#clustd <- dist(svsmire.cos, method = "minkowski") # create distance matrix (minkowski method: is not a true distance measure)

# find optimal number of clusters
asw <- as.vector(unlist(sapply(2:nrow(svsmire)-1, function(x) pam(clustd, k = x)$silinfo$avg.width)))
# determine the optimal number of clusters (max width is optimal)
optclust <- which(asw == max(asw))+1 # optimal number of clusters

# inspect clustering with optimal number of clusters
svsmire.clust <- pam(clustd, optclust)
svsmire.clust$clustering

#cd <- hclust(clustd, method="single")    # create cluster object (single linkage)
cd <- hclust(clustd, method="ward.D")    # create cluster object (ward.D linkage)
#cd <- hclust(clustd, method="ward.D2")   # create cluster object (ward.D2 linkage)
#cd <- hclust(clustd, method="average")   # create cluster object (average linkage)
#cd <- hclust(clustd, method="mcquitty")  # create cluster object (mcquitty linkage)
#cd <- hclust(clustd, method="median")    # create cluster object (median linkage)
#cd <- hclust(clustd, method="centroid")  # create cluster object (centroid linkage)

# plot cluster solution
png("images/ClustCoha.png",  width = 900, height = 300) # save plot
plot(cd, hang= -1)
rect.hclust(cd, k = 2)
dev.off()
# validate clustering
#pclustire <- pvclust(t(svsmire), method.hclust= "ward.D", method.dist = "manhattan")
#png("images/PclustCoha.png",  width = 680, height = 480) # save plot
#plot(pclustire, hang = -1)
#pvrect(pclustire, alpha = 0.95)
#dev.off()
###############################################################
###                  NETWORK ANALYSIS
###                      TYPES
###############################################################
t3ire# <- t3ire[, 2: ncol(t3ire)]
# remove adjectives that were not intensified
t3ire <- t3ire[rowSums(t3ire) > 0, ]
# save row and column names
colnamesire <- colnames(t3ire)
rownamesire <- rownames(t3ire)
# turn dataframe into matrix
svsmire <- as.matrix(t3ire)
# remove items that occurred less than 5 times
svsmire <- svsmire[rowSums(svsmire) > 1, ] # token
svsmire <- svsmire[,colSums(svsmire) > 1] #pint
svsmiretyp <- apply(svsmire, 1, function(x) { x <- ifelse(x > 1, 1, x) } )
tiretyp <- t(svsmiretyp)
#tiretyp <- svsmire
# inspect data
tiretyp

# create network
a = network(tiretyp, directed = F)
# The parameter directed is specified as FALSE because in our case, artificialNet is a symetrical matrix.
# It is really convenient to plot a nice network
png("images/NetworkMidAgeIre.png",  width = 680, height = 480) # save plot
plot(a, displaylabels = T, label.cex = .75)
dev.off()
###############################################################
###############################################################
###############################################################
###             COLL ANA
###############################################################
###############################################################
###############################################################
# collex function
collex <- function(data = data, cv1 = cv1){
  # set up rslttb
  rslttb <- matrix(c("int", "adj", "or", "p"), ncol = 4)
  colnames(rslttb) <- c("Intensifier", "Adjective", "OddsRatio", "p-Value")
  rvs <- 1:nrow(t3)
  # define column values
  cv0 <- 1
  # set up table
  sapply(rvs, function(x){
    # extract values
    b2 <- t3[x,cv1] # freq adj with int
    b3 <- sum(t3[x,])-b2 # freq adj without int
    c2 <- sum(t3[,cv1])-b2   # freq int general without int
    c3 <- sum(t3)-(sum(t3[,cv1])+b3) # freq adj without int general
    # set up table
    collextb <- matrix(c(b2, b3, c2, c3), ncol = 2, byrow = F)
    # add row names
    rownames(collextb) <- c("Int", "NoInt")
    # add column names
    colnames(collextb) <- c("Adj", "AdjGen")
    # perform fisher's exact test
    rslt <- fisher.test(collextb)
    # set up table with results
    rslttb <- list(c(colnames(data)[cv1], rownames(data)[x],
                     as.vector(unlist(rslt[3])), as.vector(unlist(rslt[1]))))
    # return results
    return(rslttb)
  } )
}
###############################################################
###############################################################
###############################################################
###             ALL
###############################################################
###############################################################
################################################################ NZE
# create table
t1 <- tapply(chint$absint, list(chint$tokenlex, chint$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
t3nze <- t3
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 3)
real  <- collex(data = t3, cv1 = 4)
really  <- collex(data = t3, cv1 = 5)
so  <- collex(data = t3, cv1 = 6)
very  <- collex(data = t3, cv1 = 8)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf <- as.data.frame(collextab)
# add colnames
colnames(collexdf) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf)
collexdf$corr05 <- rep(corr05, nrow(collexdf))
corr01 <- 0.01/nrow(collexdf)
collexdf$corr01 <- rep(corr01, nrow(collexdf))
corr001 <- 0.001/nrow(collexdf)
collexdf$corr001 <- rep(corr001, nrow(collexdf))
# calculate corrected significance status
collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf <- collexdf[as.numeric(collexdf$p) <= .05, ]
corrsigcollexdf <- collexdf[collexdf$sig != "n.s.", ]
# inspect results
sigcollexdf

corrsigcollexdf

###############################################################
# determine intesifiers for hist coll ana
ints <- c("pretty", "real", "really", "so", "very")
###############################################################
###                  COLLEXEME ANALYSIS : 3-4
###############################################################
# collocation analysis age 3-4
df34 <- chint[chint$agecat2 == "3-4",]
t1 <- tapply(df34$absint, list(df34$tokenlex, df34$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 1)
real  <- collex(data = t3, cv1 = 2)
really  <- collex(data = t3, cv1 = 3)
so  <- collex(data = t3, cv1 = 4)
very  <- collex(data = t3, cv1 = 5)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf34 <- as.data.frame(collextab)
# add colnames
colnames(collexdf34) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf34)
collexdf34$corr05 <- rep(corr05, nrow(collexdf34))
corr01 <- 0.01/nrow(collexdf34)
collexdf34$corr01 <- rep(corr01, nrow(collexdf34))
corr001 <- 0.001/nrow(collexdf34)
collexdf34$corr001 <- rep(corr001, nrow(collexdf34))
# calculate corrected significance status
collexdf34$sig <- as.vector(unlist(sapply(collexdf34$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf34 <- collexdf34[as.numeric(collexdf34$p) <= .05, ]
corrsigcollexdf34 <- collexdf34[collexdf34$sig != "n.s.", ]
# inspect results
sigcollexdf34

corrsigcollexdf34

###############################################################
###                  COLLEXEME ANALYSIS : 5-6
###############################################################
ints <- c("awful", "pretty", "real", "really", "so", "totally", "very")
# collocation analysis age 5-6
df56 <- chint[chint$agecat2 == "5-6",]
t1 <- tapply(df56$absint, list(df56$tokenlex, df56$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 1)
real  <- collex(data = t3, cv1 = 2)
really  <- collex(data = t3, cv1 = 3)
so  <- collex(data = t3, cv1 = 4)
very  <- collex(data = t3, cv1 = 5)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf56 <- as.data.frame(collextab)
# add colnames
colnames(collexdf56) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf56)
collexdf56$corr05 <- rep(corr05, nrow(collexdf56))
corr01 <- 0.01/nrow(collexdf56)
collexdf56$corr01 <- rep(corr01, nrow(collexdf56))
corr001 <- 0.001/nrow(collexdf56)
collexdf56$corr001 <- rep(corr001, nrow(collexdf56))
# calculate corrected significance status
collexdf56$sig <- as.vector(unlist(sapply(collexdf56$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf56 <- collexdf56[as.numeric(collexdf56$p) <= .05, ]
corrsigcollexdf56 <- collexdf56[collexdf56$sig != "n.s.", ]
# inspect results
sigcollexdf56

corrsigcollexdf56

###############################################################
###                  COLLEXEME ANALYSIS : 7-8
###############################################################
ints <- c("awful", "pretty", "real", "really", "so", "totally", "very")
# collocation analysis age 7-8
df78 <- chint[chint$agecat2 == "7-8",]
t1 <- tapply(df78$absint, list(df78$tokenlex, df78$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 1)
real  <- collex(data = t3, cv1 = 2)
really  <- collex(data = t3, cv1 = 3)
so  <- collex(data = t3, cv1 = 4)
very  <- collex(data = t3, cv1 = 5)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf78 <- as.data.frame(collextab)
# add colnames
colnames(collexdf78) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf78)
collexdf78$corr05 <- rep(corr05, nrow(collexdf78))
corr01 <- 0.01/nrow(collexdf78)
collexdf78$corr01 <- rep(corr01, nrow(collexdf78))
corr001 <- 0.001/nrow(collexdf78)
collexdf78$corr001 <- rep(corr001, nrow(collexdf78))
# calculate corrected significance status
collexdf78$sig <- as.vector(unlist(sapply(collexdf78$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf78 <- collexdf78[as.numeric(collexdf78$p) <= .05, ]
corrsigcollexdf78 <- collexdf78[collexdf78$sig != "n.s.", ]
# inspect results
sigcollexdf78

corrsigcollexdf78

###############################################################
###                  COLLEXEME ANALYSIS : 9-10
###############################################################
ints <- c("awful", "pretty", "real", "really", "so", "totally", "very")
# collocation analysis age 9-10
df910 <- chint[chint$agecat2 == "9-10",]
t1 <- tapply(df910$absint, list(df910$tokenlex, df910$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 1)
real  <- collex(data = t3, cv1 = 2)
really  <- collex(data = t3, cv1 = 3)
so  <- collex(data = t3, cv1 = 4)
totally  <- collex(data = t3, cv1 = 5)
very  <- collex(data = t3, cv1 = 6)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
totally <- matrix(unlist(totally),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, totally, very)
# convert into data frame
collexdf910 <- as.data.frame(collextab)
# add colnames
colnames(collexdf910) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf910)
collexdf910$corr05 <- rep(corr05, nrow(collexdf910))
corr01 <- 0.01/nrow(collexdf910)
collexdf910$corr01 <- rep(corr01, nrow(collexdf910))
corr001 <- 0.001/nrow(collexdf910)
collexdf910$corr001 <- rep(corr001, nrow(collexdf910))
# calculate corrected significance status
collexdf910$sig <- as.vector(unlist(sapply(collexdf910$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf910 <- collexdf910[as.numeric(collexdf910$p) <= .05, ]
corrsigcollexdf910 <- collexdf910[collexdf910$sig != "n.s.", ]
# inspect results
sigcollexdf910

corrsigcollexdf910

################################################################ 
###                MOTHER
# create table
t1 <- tapply(motint$absint, list(motint$tokenlex, motint$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
t3nze <- t3
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 4)
real  <- collex(data = t3, cv1 = 5)
really  <- collex(data = t3, cv1 = 6)
so  <- collex(data = t3, cv1 = 7)
very  <- collex(data = t3, cv1 = 10)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf <- as.data.frame(collextab)
# add colnames
colnames(collexdf) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf)
collexdf$corr05 <- rep(corr05, nrow(collexdf))
corr01 <- 0.01/nrow(collexdf)
collexdf$corr01 <- rep(corr01, nrow(collexdf))
corr001 <- 0.001/nrow(collexdf)
collexdf$corr001 <- rep(corr001, nrow(collexdf))
# calculate corrected significance status
collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf <- collexdf[as.numeric(collexdf$p) <= .05, ]
corrsigcollexdf <- collexdf[collexdf$sig != "n.s.", ]
# inspect results
sigcollexdf

corrsigcollexdf

###############################################################
###                 WARNING
###############################################################
# tabulate data
chintint <- chint[chint$absint == 1,]
ftbchinta <- ftable(chintint$pint, chintint$agecat2)
rwnms <- as.vector(unlist(attr(ftbchinta, "row.vars")))
rownames(ftbchinta) <- rwnms
svrwnms <- as.vector(unlist(attr(ftbchinta, "dimnames")))
ftbchinta <- ftbchinta
rownames(ftbchinta) <- svrwnms
colnames(ftbchinta) <- as.vector(unlist(attr(ftbchinta, "col.vars")))
ptbchintaint <- prop.table(ftbchinta, margin=2)*100
ptbchintaint

# save data to disc
write.table(ptbchintaint, "ptbchintaint.txt", sep = "\t", row.names = F)
###############################################################
# plot data
pretty <- ptbchintaint[3,]
real <- ptbchintaint[4,]
really <- ptbchintaint[5,]
so <- ptbchintaint[6,]
totally <- ptbchintaint[7,]
very <- ptbchintaint[8,]
wicked <- ptbchintaint[10,]
# save plot
png("images/IntsChiMtAttrAge.png",  
    width = 680, height = 480) # save plot
plot(very, col = "black", ylim = c(0,60), type = "l", lty = 1, ann = F,
     lwd = 3, axes = F)
axis(1, at = 1:4, c("3-4", "5-6", "7-8", "9-10"), las = 1)
axis(2,  at = seq(0,60, 10), seq(0,60, 10), las = 2)
mtext("Age", 1, 3, las = 1)
mtext("%", 2, 3, las = 2)
lines(so, col = "black", lty = 2, lwd = 3)
lines(really, col = "black", lty = 3, lwd = 3)
lines(real, col = "gray", lty = 4, lwd = 3)
lines(pretty, col = "gray", lty = 5, lwd = 3)
lines(totally, col = "gray", lty = 6, lwd = 3)
lines(wicked, col = "gray", lty = 7, lwd = 3)
legend(2, 60, inset=.05, title="", bty = "n", cex = .85,
       c("very", "so", "really", "real", "pretty", "totally", "wicked"), lwd = 2, 
       lty = c(1:7), col = c(rep("black", 3), rep("gray", 7)), horiz=F)
grid()
dev.off()
###########################################################################
###        CHANGES IN ADJ FREQ
###########################################################################
# tabulate data
ftbadjschildes <- ftable(chint$tokenlex, chint$agecat2)
rwnms <- as.vector(unlist(attr(ftbadjschildes, "row.vars")))
#ftbadjschildes <- ftbadjschildes[3:nrow(ftbadjschildes),]
rownames(ftbadjschildes) <- rwnms#[3:length(rwnms)]
svrwnms <- as.vector(unlist(attr(ftbadjschildes, "dimnames")))[which(rowSums(ftbadjschildes) >= 2)]
ftbadjschildes <- ftbadjschildes[which(rowSums(ftbadjschildes) >= 2),]
rownames(ftbadjschildes) <- svrwnms
colnames(ftbadjschildes) <- c("3-4", "5-6", "7-8", "9-10")
ptbadjschildes <- prop.table(ftbadjschildes, margin=2)*100
#ptbintschildes <- ptbintschildes[rowSums(ptbintschildes) > 1, ]
ptbadjschildes

# save data to disc
write.table(ptbadjschildes, "ptbadjschildes.txt", sep = "\t", row.names = F)
###########################################################################
adjage <- 1:4
adjlm <- ptbadjschildes
head(adjlm)

str(adjlm)

nrow(adjlm)
sigadj <- apply(adjlm, 1, function(x){
  x <- lm(x ~ adjage)
  x <- summary(x)[4][[1]][[8]]})

sigadjs <- which(sigadj < .05)
sigadjs

###########################################################################
tbf1 <- table(chint$tokenlex)
tbf2  <- tbf1[order(tbf1, decreasing = T)]
head(tbf2)

##############################################################
###                     TTR: CHI INT
###############################################################
# plot type-token-ratio of intensifiers against age
chintint <- chint[chint$absint == 1, ]
inttb08 <- table(chintint$prelex, chintint$agecat2)
# calculate token freq
tokf08 <- colSums(inttb08)
# calculate type freq
typf08 <- t(apply(inttb08, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf08 <- colSums(typf08)
# calculate type-token-ratio
chittr08 <- data.frame(1:4, round(typf08/tokf08, 2), typf08, tokf08)
rownames(chittr08) <- c("3-4", "5-6", "7-8", "9-10")
colnames(chittr08) <- c("Age", "TTR", "Types", "Tokens")
chittr08 <- chittr08[chittr08$Tokens > 1,]
# plot data
png("images/ttrchiageint.png") # save plot
plot(chittr08[,1:2], xlab = "Age", ylab = "Type-Token-Ratio (TTR): Amplifiers",
     ylim = c(0,.5), pch = 19, axes = F,main = "Children's TTRs of Amplifiers by Age"
)
axis(1, at = 1:4, c("3-4", "5-6", "7-8", "9-10"), las = 1)
axis(2,  at = seq(0, .5, .1), seq(0, .5, .1), las = 1)
abline(lm(chittr08[,2]~chittr08[,1]), col="red", lty = 2) # regression line (y~x)
lws08 <- unlist(lowess(chittr08[,2],chittr08[,1]))
lines(lws08[1:length(lws08)/2], col="blue", lty = 3)
legend("topright", inset = .05, c("linear regression line", "smoothed/lowess regression line"),
       horiz = F, lty = c(2,3), col = c("red", "blue"))
grid()
dev.off()
###############################################################
###                     TTR: CHI ADJ
###############################################################
# plot type-token-ratio of intensifiers against age
chintint <- chint[chint$absint == 1, ]
inttb08 <- table(chintint$tokenlex, chintint$agecat2)
# calculate token freq
tokf08 <- colSums(inttb08)
# calculate type freq
typf08 <- t(apply(inttb08, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf08 <- colSums(typf08)
# calculate type-token-ratio
chittr08 <- data.frame(1:4, round(typf08/tokf08, 2), typf08, tokf08)
rownames(chittr08) <- c("3-4", "5-6", "7-8", "9-10")
colnames(chittr08) <- c("Age", "TTR", "Types", "Tokens")
chittr08 <- chittr08[chittr08$Tokens > 1,]
# plot data
png("images/ttrchiageint.png") # save plot
plot(chittr08[,1:2], xlab = "Age", ylab = "Type-Token-Ratio (TTR): Adjectives",
     ylim = c(0,1), pch = 19, axes = F,main = "Children's TTRs of Adjectives by Age"
)
axis(1, at = 1:4, c("3-4", "5-6", "7-8", "9-10"), las = 1)
axis(2,  at = seq(0, 1, .2), seq(0, 1, .2), las = 1)
abline(lm(chittr08[,2]~chittr08[,1]), col="red", lty = 2) # regression line (y~x)
lws08 <- unlist(lowess(chittr08[,2],chittr08[,1]))
lines(lws08[1:length(lws08)/2], col="blue", lty = 3)
legend("topright", inset = .05, c("linear regression line", "smoothed/lowess regression line"),
       horiz = F, lty = c(2,3), col = c("red", "blue"))
grid()
dev.off()
###############################################################
###                     MOT INT FREQ
###############################################################
head(motint)

png("images/intagepctmot.png") # save plot
ag <- chint[,c(1, 17)]
ag <- unique(ag)
library(plyr)
motint10 <- join(motint, ag, by = "file", type = "right", match = "all")
motint10 <- motint10[, c(1:16, 18)]
motint10 <- motint10
# tabulate data
inttb10 <- table(motint10$absint, motint10$agecat2)
nmot10 <- ftable(motint10$spk, motint10$agecat2)
nmot10 <- colSums(nmot10)
# calculate percentages of intensified slots
pcnt10 <- round(inttb10[2,]/(inttb10[1,]+inttb10[2,])*100, 2)
# plot data
barplot(pcnt10, col = "grey80", ylim = c(-2, 20), ylab = "Percent (intensified slots of all slots)",
        xlab = "Age of Child", axes = T, beside = T,
        main = "Mothers' use of intensifiers\nagainst the age of her child")
text(seq(0.7, 9.1, 1.2), pcnt10 + 1, cex = .8, labels = pcnt10)
text(seq(0.7, 9.1, 1.2), -1, labels = c(paste("N(slots):\n", nmot10[1], sep = ""),
                                        nmot10[2: length(nmot10)]), cex = .8)
tbmotabsint <- table(motint10$absint)
m10 <- round(tbmotabsint[2]/(tbmotabsint[1]+tbmotabsint[2])*100, 2)
lines(0:10, rep(m10, length(0:10)), col="red", lty = 2, lwd = 1)
text(8.2, m10 + 1, cex = .8, paste("mean = ", m10, collapse = ""), col = "red")
grid()
box()
dev.off()
###############################################################
###
###############################################################
png("images/intagestyppctmot.png",
    width = 900, height = 480)
# tabulate data
ag <- chint[,c(1, 17)]
ag <- unique(ag)
motint14 <- join(motint, ag, by = "file", type = "left", match = "all")
motint14 <- motint14[, c(1:16, 18)]
inttb14 <- ftable(motint14$styp, motint14$absint, motint14$agecat2)
nmot14 <- ftable(motint14$styp, motint14$spk, motint14$agecat2)
nmot14 <- colSums(nmot14)
# calculate percentages of intensified slots
pcnt14br <- round(inttb14[2,]/(inttb14[1,]+inttb14[2,])*100, 1)
pcnt14et <- round(inttb14[4,]/(inttb14[3,]+inttb14[4,])*100, 1)
pcnt14mt <- round(inttb14[8,]/(inttb14[7,]+inttb14[8,])*100, 1)
pcnt14tp <- round(inttb14[10,]/(inttb14[9,]+inttb14[10,])*100, 1)
pcnt14 <- rbind(pcnt14br, pcnt14et, pcnt14mt, pcnt14tp)
colnames(pcnt14) <- c("3-4", "5-6", "7-8", "9-10")
# plot data
barplot(pcnt14, col = c("grey 20", "grey 40", "grey 60", "grey80"), ylim = c(-2, 30),
        ylab = "Percent (intensified slots of all slots)",
        xlab = "Age of motld", axes = T, beside = T,
        main = "Mother's Use of intensifiers by situation type")
text(seq(1.5, 16.5, 5), pcnt14[1,] + 1, cex = .8, labels = pcnt14[1,])
text(seq(2.5, 17.5, 5), pcnt14[2,] + 1, cex = .8, labels = pcnt14[2,])
text(seq(3.5, 18.5, 5), pcnt14[3,] + 1, cex = .8, labels = pcnt14[3,])
text(seq(4.5, 19.5, 5), pcnt14[4,] + 1, cex = .8, labels = pcnt14[4,])
legend("topleft", inset = .05, c("book reading (br)", "elicited report (er)",
                                 "meal time (mt)", "toy play (tp)"),
       horiz = F,  fill = c("grey 20", "grey 40", "grey 60", "grey80"))
grid()
box()
dev.off()

###############################################################
# prepare data p10
# restructure data
cd <- pd %>%
  select(file, agecat2)
p10d <- pmd %>%
  left_join(cd, by = c("file")) %>%
  select(absint, styp, agecat2.y) %>%
  dplyr::rename(Age = agecat2.y, SituationType = styp, Amplified = absint) %>%
  na.omit() %>%
  dplyr::group_by(Age, SituationType) %>%
  dplyr::count(Amplified) %>%
  tidyr::spread(Amplified, n)%>%
  replace_na(list(`1` = 0)) %>%
  mutate(Percent = `1` /(`1` + `0`)*100) %>%
  select(Age, SituationType, Percent) %>%
  filter(SituationType != "elicited report")
p10d$Percent <- round(p10d$Percent, 1)

# plot
p10 <- ggplot(p10d, aes(Age, Percent, fill = SituationType)) +  
  geom_bar(stat="identity", position=position_dodge()) + 
  theme_set(theme_light(base_size = 20)) + 
  scale_fill_manual(values=c("gray20", 
                             "gray40",
                             "gray60",
                             "gray80")) + 
  geom_text(aes(label=Percent), vjust=1.6, color="white",
            position = position_dodge(0.9), size=5) + 
  labs(x = "Age of Child", y = "Percent")    
ggsave(file = paste(imageDirectory,"PercentAmpSitAgeMot.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p10

###############################################################
###                       MOT TTR INT
###############################################################
# plot type-token-ratio of intensifiers against age
motint15 <- motint[motint$absint == 1, ]
# tabulate data
ag <- chint[,c(1, 17)]
ag <- unique(ag)
motint15 <- join(motint15, ag, by = "file", type = "left", match = "all")
motint15 <- motint15[, c(1:16, 18)]
inttb15 <- table(motint15$prelex, motint15$agecat2)
# calculate token freq
tokf15 <- colSums(inttb15)
# calculate type freq
typf15 <- t(apply(inttb15, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf15 <- colSums(typf15)
# calculate type-token-ratio
motttr15 <- data.frame(1:4, round(typf15/tokf15, 2), typf15, tokf15)
motttr15 <- na.omit(motttr15)
colnames(motttr15) <- c("Age", "TTR", "Types", "Tokens")
# plot data
png("images/ttrmotageint.png") # save plot
plot(motttr15[, 1:2], xlab = "Age of Child", ylab = "Type-Token-Ratio (TTR): Intensifiers",
     ylim = c(0,1), pch = 19#,
     #main = "Mother's TTRs of intensifiers\nby age of child"
)
abline(lm(motttr15[,2]~motttr15[,1]), col="red", lty = 2) # regression line (y~x)
lws15 <- unlist(lowess(motttr15[,2],motttr15[,1]))
lines(lws15[1:length(lws15)/2], col="blue", lty = 3)
legend("topright", inset = .05, c("linear regression line", "smoothed/lowess regression line"),
       horiz = F, lty = c(2,3), col = c("red", "blue"))
grid()
dev.off()
###############################################################
###                       MOT TTR ADJ
###############################################################
motadj16 <- motint[motint$absint == 1, ]
# tabulate data
ag <- chint[,c(1, 17)]
ag <- unique(ag)
motadj16 <- join(motadj16, ag, by = "file", type = "left", match = "all")
motadj16 <- motadj16[, c(1:16, 18)]
adjtb16 <- table(motadj16$tokenlex, motadj16$agecat2)
# calculate token freq
tokf16 <- colSums(adjtb16)
# calculate type freq
typf16 <- t(apply(adjtb16, 1, function(x) x <- ifelse(x > 0, 1, 0)))
typf16 <- colSums(typf16)
# calculate type-token-ratio
motttr16 <- data.frame(1:4, round(typf16/tokf16, 2), typf16, tokf16)
motttr16 <- na.omit(motttr16)
colnames(motttr16) <- c("Age", "TTR", "Types", "Tokens")
# plot data
png("images/ttrmotageadj.png") # save plot
plot(motttr16[, 1:2], xlab = "Age of Child", ylab = "Type-Token-Ratio (TTR): Intensifiers",
     ylim = c(0,1),pch = 19,
     main = "Mother's Type-Token-Ratios\nof intensified adjectives against the age of her child")
abline(lm(motttr16[,2]~motttr16[,1]), col="red") # regression line (y~x)
lws16 <- unlist(lowess(motttr16[,2],motttr16[,1]))
lines(lws16[1:length(lws16)/2], col="blue")
grid()
dev.off()
###############################################################
###                 WARNING
###############################################################
motadj16 <- motint[motint$absint == 1, ]
# tabulate data
ag <- chint[,c(1, 17)]
ag <- unique(ag)
motadj16 <- join(motadj16, ag, by = "file", type = "left", match = "all")
motadj16 <- motadj16[, c(1:16, 18)]
ftbchinta <- ftable(motadj16$pint, motadj16$agecat2)
rwnms <- as.vector(unlist(attr(ftbchinta, "row.vars")))
rownames(ftbchinta) <- rwnms
svrwnms <- as.vector(unlist(attr(ftbchinta, "dimnames")))
ftbchinta <- ftbchinta
rownames(ftbchinta) <- svrwnms
colnames(ftbchinta) <- as.vector(unlist(attr(ftbchinta, "col.vars")))
ptbchintaint <- prop.table(ftbchinta, margin=2)*100
ptbchintaint

###############################################################
# plot data
pretty <- ptbchintaint[4,]
real <- ptbchintaint[5,]
really <- ptbchintaint[6,]
so <- ptbchintaint[7,]
totally <- ptbchintaint[9,]
very <- ptbchintaint[10,]
# save plot
png("images/IntsMotPredAge.png",  
    width = 680, height = 480) # save plot
plot(very, col = "black", ylim = c(0,60), type = "l", lty = 1, ann = F,
     lwd = 3, axes = F)
axis(1, at = 1:4, c("3-4", "5-6", "7-8", "9-10"), las = 1)
axis(2,  at = seq(0,60, 10), seq(0,60, 10), las = 2)
mtext("Age", 1, 3, las = 1)
mtext("%", 2, 3, las = 2)
lines(so, col = "black", lty = 2, lwd = 3)
lines(really, col = "black", lty = 3, lwd = 3)
lines(real, col = "gray", lty = 4, lwd = 3)
lines(pretty, col = "gray", lty = 5, lwd = 3)
lines(totally, col = "gray", lty = 6, lwd = 3)
legend(2, 60, inset=.05, title="", bty = "n", cex = .85,
       c("very", "so", "really", "real", "pretty", "totally"), lwd = 2, 
       lty = c(1:6), col = c(rep("black", 3), rep("gray", 7)), horiz=F)
grid()
dev.off()

################################################################ 
###                MOTHER
# create table
t1 <- tapply(motint$absint, list(motint$tokenlex, motint$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
t3nze <- t3
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 4)
real  <- collex(data = t3, cv1 = 5)
really  <- collex(data = t3, cv1 = 6)
so  <- collex(data = t3, cv1 = 7)
very  <- collex(data = t3, cv1 = 10)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf <- as.data.frame(collextab)
# add colnames
colnames(collexdf) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf)
collexdf$corr05 <- rep(corr05, nrow(collexdf))
corr01 <- 0.01/nrow(collexdf)
collexdf$corr01 <- rep(corr01, nrow(collexdf))
corr001 <- 0.001/nrow(collexdf)
collexdf$corr001 <- rep(corr001, nrow(collexdf))
# calculate corrected significance status
collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf <- collexdf[as.numeric(collexdf$p) <= .05, ]
corrsigcollexdf <- collexdf[collexdf$sig != "n.s.", ]
# inspect results
sigcollexdf

corrsigcollexdf

###############################################################
###############################################################
###############################################################
###                         SBC
###############################################################
###############################################################
###############################################################
# read in data
biosbc <- read.table("D:\\Homepage\\docs\\data/BiodataSbcae.txt", sep = "\t", header=TRUE)
# set parameters
context <- 80
all.pre = T
# define search strings
search.pattern <-  c("[A-Z]{0,1}[a-z]{0,}-{0,1}[a-z]{2,}\\/JJ")
##########################################################
# start searches
sbcint <- ConcR("D:\\Uni\\Korpora\\Edited\\SBCPosTagged", search.pattern, context, all.pre = T)
# retrieve speaker
sbcint$all.pre <- gsub("([A-Z]{2,20}[0-9{0,1}]:)", "qwertz\\1", sbcint$all.pre)
sbcint$all.pre <- gsub(".*qwertz", "", sbcint$all.pre)
sbcint$all.pre <- gsub(":.*", "", sbcint$all.pre)
sbcint$all.pre <- tolower(sbcint$all.pre)
# clean column names
colnames(sbcint)[5] <- "spk"
colnames(biosbc)[3] <-  "file"
colnames(biosbc)[4] <-  "spk"
# clean file name
sbcint$file <- gsub(".txt", "", sbcint$file, fixed = T)
sbcint$file <- tolower(sbcint$file)
# join data sets
sbc <- join(sbcint, biosbc, by = c("file", "spk"), type = "left")
# select only female speakers
sbc <- sbc[sbc$gender == "female",]
# code status
sbc$status <- as.vector(unlist(sapply(sbc$post, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(" .*", "", x)
  x <- gsub(".*\\/", "", x)
  x <- gsub("NNS", "NN", x)
  x <- ifelse(x == "NN", "attr", "pred") } )))
# code previous element (pos)
sbc$prepos <- as.vector(unlist(sapply(sbc$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".*\\/", "", x)
  x <- gsub(" .*", "", x) } )))
# code previous element (lexeme)
sbc$prelex <- as.vector(unlist(sapply(sbc$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".* ", "", x)
  x <- gsub("\\/.*", "", x) } )))
# code intensifiers
sbc$prelex <- tolower(sbc$prelex)
# select only women between 19 and 50
sbc <- sbc[sbc$age.exact >= 19, ]
sbc <- sbc[sbc$age.exact <= 49, ]
sbc <- sbc[sbc$status == "pred",]
intensifiers <- c("absolutely", "actually", "aggressively", "amazingly",
                  "appallingly", "awful", "awfully", "badly", "bloody", "certainly", "clearly",
                  "complete", "dead", "completely", "considerably", "crazy", "decidedly", "definitely",
                  "distinctly", "dreadfully", "enormously", "entirely", "especially", "exactly",
                  "exceedingly", "exceptionally", "excruciatingly", "extraordinarily", "extremely",
                  "fiercely", "firmly", "frightfully", "fucking", "fully", "genuinely", "greatly",
                  "grossly", "heavily", "highly", "hopelessly", "horrendously", "hugely",
                  "immediately", "immensely", "incredibly", "infinitely", "intensely", "irrevocably",
                  "mad", "mighty", "obviously", "openly", "overwhelmingly", "particularly",
                  "perfectly", "plenty", "positively", "precisely", "pretty", "profoundly",
                  "purely", "real", "really", "remarkably", "seriously", "shocking",
                  "significant", "significantly", "so", "specially", "specifically", "strikingly",
                  "strongly", "super", "surely", "terribly", "terrifically", #"too",
                  "total",
                  "totally", "traditionally", "true", "truly", "ultra", "utterly", "very",
                  "viciously", "well", "wholly", "wicked", "wildly")
sbc$pint <- as.vector(unlist(apply(sbc, 1, function(x){
  x <- ifelse(x[23] %in% intensifiers, x <- x[23], 0)})))
table(sbc$pint)

sbc$absint <- as.vector(unlist(sapply(sbc$pint, function(x){
  x <- ifelse(x == "0", 0, 1)})))

sbc$tokenlex <- as.vector(unlist(sapply(sbc$token, function(x){
  x <- gsub("\\/.*", "", x)
  x <- tolower(x)})))
table(sbc$absint)

table(sbc$pint)

head(sbc)

##############################################################
tst1 <- table(sbc$tokenlex, sbc$pint)
tst2 <- tst1[, 2: ncol(tst1)]
tst3 <- apply(tst2, 1, function(x){ x <- ifelse(x > 1, 1, x)})
tst4 <- colSums(tst3)
varadj <- names(tst4)[which(tst4 > 1)]
nrow (sbc)

sbc <- sbc[sbc$tokenlex %in% varadj,]
nrow (sbc)

# inspect data
head(sbc)

##########################################################
###                     FREQ INT ALL
##########################################################
# plot frequency of intensification by child, mothers, and female sbc speakers
# tabulate data
cmp <- cbind(table(chint$absint), table(motint$absint), table(sbc$absint))
cmp <- rbind(cmp, round(cmp[2,]/(cmp[1,]+cmp[2,])*100,2))
colnames(cmp) <- c("Child", "Mother (CDS)", "Female (SBC)")
# save plot
png("images/cmp-all.png") # save plot
# plot data
barplot(cmp[3,], col = c("grey 40", "grey 60", "grey80"), ylim = c(0, 50),
        ylab = "Percent (amplified slots of all slots)",
        xlab = "", axes = T, beside = T,
        main = "Amplifier use across corpora\nin informal settings")
text(seq(0.7, 3.1, 1.2), cmp[3,] + 5, cex = .8, labels = cmp[3,])
legend("topleft", inset = .05, c("Children", "Mothers (CDS)", "Female SBC speakers"),
       horiz = F,  fill = c("grey 40", "grey 60", "grey80"))
grid()
box()
dev.off()
###############################################################
###                 WARNING
###############################################################
# tabulate data
sbcint <- sbc[sbc$absint == 1,]
ftbsbcnta <- ftable(sbcint$pint, sbcint$age)
rwnms <- as.vector(unlist(attr(ftbsbcnta, "row.vars")))
rownames(ftbchinta) <- rwnms
svrwnms <- as.vector(unlist(attr(ftbsbcnta, "dimnames")))
ftbchinta <- ftbsbcnta
rownames(ftbsbcnta) <- svrwnms
colnames(ftbsbcnta) <- as.vector(unlist(attr(ftbchinta, "col.vars")))
ptbsbcntaint <- prop.table(ftbsbcnta, margin=2)*100
ptbsbcntaint

# save data to disc
write.table(ptbchintaint, "images/ptbchintaint.txt", sep = "\t", row.names = F)
###############################################################
# plot data
pretty <- ptbsbcntaint[4,]
real <- ptbsbcntaint[5,]
really <- ptbsbcntaint[6,]
so <- ptbsbcntaint[7,]
very <- ptbsbcntaint[9,]
# save plot
png("images/IntsSbcMtAttrAge.png",  
    width = 680, height = 480) # save plot
plot(very, col = "black", ylim = c(0,70), type = "l", lty = 1, ann = F,
     lwd = 3, axes = F)
axis(1, at = 1:4, c("19-25", "26-33", "34-41", "42-49"), las = 1)
axis(2,  at = seq(0,70, 10), seq(0,70, 10), las = 2)
mtext("Age", 1, 3, las = 1)
mtext("%", 2, 3, las = 2)
lines(so, col = "black", lty = 2, lwd = 3)
lines(really, col = "black", lty = 3, lwd = 3)
lines(real, col = "gray", lty = 4, lwd = 3)
lines(pretty, col = "gray", lty = 5, lwd = 3)
legend(2, 70, inset=.05, title="", bty = "n", cex = .85,
       c("very", "so", "really", "real", "pretty"), lwd = 2, 
       lty = c(1:5), col = c(rep("black", 3), rep("gray", 5)), horiz=F)
grid()
dev.off()

########################################
# restructure data
p9d <- sbc %>%
  select(age, pint) %>%
  filter(pint != "0")
amps <- c("very", "really", "so", "real")
p9d$pint <- ifelse(p9d$pint %in% amps, p9d$pint, "other")
p9d$Age <- ifelse(p9d$age == "19-25", 1,
                  ifelse(p9d$age == "26-33", 2,
                         ifelse(p9d$age == "34-41", 3,
                                ifelse(p9d$age == "42-49", 4, p9d$age))))
p9d$Age <- as.double(p9d$Age)
library(tidyr)
library(tibble)
p9d <- p9d %>%
  dplyr::count(Age, pint) %>%
  dplyr::group_by(Age) %>%
  dplyr::mutate(Percent = n / sum(n)*100) %>%
  select(-n) %>%
  spread(pint, Percent) %>% 
  replace_na(list(other = 0, real = 0, very = 0))
p9d

# plot
p9 <- ggplot(p9d, aes(Age, very)) +  
  geom_smooth(aes(y = very, color = "very", linetype = "very")) +  
  geom_smooth(aes(y = so, color = "so", linetype = "so")) +   
  geom_smooth(aes(y = really, color = "really", linetype = "really")) +   
  geom_smooth(aes(y = real, color = "real", linetype = "real")) +   
  geom_smooth(aes(y = other, color = "other", linetype = "other")) +   
  guides(color=guide_legend(override.aes=list(fill=NA))) +   
  scale_linetype_manual(values=c("dashed", "dotted", "dotted", "solid", "twodash"), 
                        name= "", breaks = c("other", "real", "really",  "so", "very"),
                        labels = c("other", "real", "really",  "so", "very")) + 
  scale_colour_manual(values=c("darkblue", "gold", "indianred4", "gray20", "gray70"), 
                      name="", breaks= c("other", "real", "really",  "so", "very"),  
                      labels = c("other", "real", "really",  "so", "very")) +
  theme_set(theme_light(base_size = 20)) +
  theme(legend.position="top") +  
  scale_x_continuous(breaks=c(1, 2, 3, 4), 
                     labels=c("19-25", "26-33", "34-41", "42-49")) +
  scale_y_continuous(name="Percent", limits=c(0, 75)) + 
  labs(x = "Age", y = "Percent")
ggsave(file = paste(imageDirectory,"PercentVarAgeSbc.png",sep="/"), 
       width = 30,  height = 15, units = c("cm"),  dpi = 320)
p9

###############################################################
# prepare data p3
mydata <- data.frame(sbc$age, sbc$absint)
colnames(mydata) <- gsub("sbc.", "", colnames(mydata))
mydata <- na.omit(mydata)
mydata$absint <- mydata$absint*100
texttypemeans <- round(tapply(mydata$absint, mydata$age, mean), 1)
p3 <- ggplot(mydata, aes(age, absint, colour = age)) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, size = 1.25) +
  coord_cartesian(ylim = c(0, 75)) +
  theme_set(theme_bw(base_size = 20)) +
  theme(legend.position="none") +
  labs(x = "Age", y = "Percent (intensified slots of all slots)", colour = "age") +
  scale_color_manual(values = c("grey30", "grey30", "grey30", "grey30")) +
  geom_text(mapping = NULL, label = "N (slots) = 121", 
            x = 1, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "279", 
            x = 2, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "16", 
            x = 3, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = "81", 
            x = 4, y = 3, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[1], 2), sep = ""), 
            x = 1, y = texttypemeans[1] +20, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[2], 2), sep = ""), 
            x = 2, y = texttypemeans[2] +20, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[3], 2), sep = ""), 
            x = 3, y = texttypemeans[3] +30, colour = "grey30", size = 5) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(texttypemeans[4], 2), sep = ""), 
            x = 4, y = texttypemeans[4] +20, colour = "grey30", size = 5) +
  theme(legend.position="none") 
# activate (remove #) to save
imageFile <- paste(imageDirectory,"AbsintAgecat2Sbc.png",sep="/")
ggsave(file = imageFile)
# activate (remove #) to show
p3

################################################################ 
###                SBC
# create table
t1 <- tapply(sbcint$absint, list(sbcint$tokenlex, sbcint$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
t3nze <- t3
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 3)
real  <- collex(data = t3, cv1 = 4)
really  <- collex(data = t3, cv1 = 5)
so  <- collex(data = t3, cv1 = 6)
very  <- collex(data = t3, cv1 = 8)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf <- as.data.frame(collextab)
# add colnames
colnames(collexdf) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf)
collexdf$corr05 <- rep(corr05, nrow(collexdf))
corr01 <- 0.01/nrow(collexdf)
collexdf$corr01 <- rep(corr01, nrow(collexdf))
corr001 <- 0.001/nrow(collexdf)
collexdf$corr001 <- rep(corr001, nrow(collexdf))
# calculate corrected significance status
collexdf$sig <- as.vector(unlist(sapply(collexdf$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf <- collexdf[as.numeric(collexdf$p) <= .05, ]
corrsigcollexdf <- collexdf[collexdf$sig != "n.s.", ]
# inspect results
sigcollexdf

corrsigcollexdf

###############################################################
###                  COLLEXEME ANALYSIS : 19-25
###############################################################
ints <- c("awful", "pretty", "real", "really", "so", "totally", "very")
# collocation analysis age 19-25
df1925 <- sbcint[sbcint$age == "19-25",]
t1 <- tapply(df1925$absint, list(df1925$tokenlex, df1925$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 1)
real  <- collex(data = t3, cv1 = 2)
really  <- collex(data = t3, cv1 = 3)
so  <- collex(data = t3, cv1 = 4)
very  <- collex(data = t3, cv1 = 5)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf1925 <- as.data.frame(collextab)
# add colnames
colnames(collexdf1925) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf1925)
collexdf1925$corr05 <- rep(corr05, nrow(collexdf1925))
corr01 <- 0.01/nrow(collexdf1925)
collexdf1925$corr01 <- rep(corr01, nrow(collexdf1925))
corr001 <- 0.001/nrow(collexdf1925)
collexdf1925$corr001 <- rep(corr001, nrow(collexdf1925))
# calculate corrected significance status
collexdf1925$sig <- as.vector(unlist(sapply(collexdf1925$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf1925 <- collexdf1925[as.numeric(collexdf1925$p) <= .05, ]
corrsigcollexdf1925 <- collexdf1925[collexdf1925$sig != "n.s.", ]
# inspect results
sigcollexdf1925

corrsigcollexdf1925

###############################################################
###                  COLLEXEME ANALYSIS : 26-33
###############################################################
ints <- c("awful", "pretty", "real", "really", "so", "totally", "very")
# collocation analysis age 26-33
df2633 <- sbcint[sbcint$age == "26-33",]
t1 <- tapply(df2633$absint, list(df2633$tokenlex, df2633$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
pretty  <- collex(data = t3, cv1 = 1)
real  <- collex(data = t3, cv1 = 2)
really  <- collex(data = t3, cv1 = 3)
so  <- collex(data = t3, cv1 = 4)
very  <- collex(data = t3, cv1 = 5)
# extract informaltion
pretty <- matrix(unlist(pretty),ncol=4,byrow=TRUE)
real <- matrix(unlist(real),ncol=4,byrow=TRUE)
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(pretty, real, really, so, very)
# convert into data frame
collexdf2633 <- as.data.frame(collextab)
# add colnames
colnames(collexdf2633) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf2633)
collexdf2633$corr05 <- rep(corr05, nrow(collexdf2633))
corr01 <- 0.01/nrow(collexdf2633)
collexdf2633$corr01 <- rep(corr01, nrow(collexdf2633))
corr001 <- 0.001/nrow(collexdf2633)
collexdf2633$corr001 <- rep(corr001, nrow(collexdf2633))
# calculate corrected significance status
collexdf2633$sig <- as.vector(unlist(sapply(collexdf2633$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf2633 <- collexdf2633[as.numeric(collexdf2633$p) <= .05, ]
corrsigcollexdf2633 <- collexdf2633[collexdf2633$sig != "n.s.", ]
# inspect results
sigcollexdf2633

corrsigcollexdf2633

###############################################################
###                  COLLEXEME ANALYSIS : 34-41
###############################################################
ints <- c("awful", "pretty", "real", "really", "so", "totally", "very")
# collocation analysis age 34-41
df3441 <- sbcint[sbcint$age == "34-41",]
t1 <- tapply(df3441$absint, list(df3441$tokenlex, df3441$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
really  <- collex(data = t3, cv1 = 1)
so  <- collex(data = t3, cv1 = 2)
# extract informaltion
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(really, so)
# convert into data frame
collexdf3441 <- as.data.frame(collextab)
# add colnames
colnames(collexdf3441) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf3441)
collexdf3441$corr05 <- rep(corr05, nrow(collexdf3441))
corr01 <- 0.01/nrow(collexdf3441)
collexdf3441$corr01 <- rep(corr01, nrow(collexdf3441))
corr001 <- 0.001/nrow(collexdf3441)
collexdf3441$corr001 <- rep(corr001, nrow(collexdf3441))
# calculate corrected significance status
collexdf3441$sig <- as.vector(unlist(sapply(collexdf3441$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf3441 <- collexdf3441[as.numeric(collexdf3441$p) <= .05, ]
corrsigcollexdf3441 <- collexdf3441[collexdf3441$sig != "n.s.", ]
# inspect results
sigcollexdf3441

corrsigcollexdf3441

###############################################################
###                  COLLEXEME ANALYSIS : 42-49
###############################################################
ints <- c("awful", "pretty", "real", "really", "so", "totally", "very")
# collocation analysis age 42-49
df4249 <- sbcint[sbcint$age == "42-49",]
t1 <- tapply(df4249$absint, list(df4249$tokenlex, df4249$pint), table)
t2 <- apply(t1, 1, function(x) ifelse(is.na(x) == T, 0, x))
t3 <- t(t2)
t3 <- t3[, 2:ncol(t3)]
t3 <- t3[rowSums(t3) > 0, ]
### WARNING!
# apply collex function to ints
which(colnames(t3) %in% ints)
### WARNING!
# apply collex function
really  <- collex(data = t3, cv1 = 1)
so  <- collex(data = t3, cv1 = 2)
very  <- collex(data = t3, cv1 = 3)
# extract informaltion
really <- matrix(unlist(really),ncol=4,byrow=TRUE)
so <- matrix(unlist(so),ncol=4,byrow=TRUE)
very <- matrix(unlist(very),ncol=4,byrow=TRUE)
# set up table with results
collextab <- rbind(really, so, very)
# convert into data frame
collexdf4249 <- as.data.frame(collextab)
# add colnames
colnames(collexdf4249) <- c("Intensifier", "Adjective", "OddsRatio", "p")
# perform bonferroni correction
corr05 <- 0.05/nrow(collexdf4249)
collexdf4249$corr05 <- rep(corr05, nrow(collexdf4249))
corr01 <- 0.01/nrow(collexdf4249)
collexdf4249$corr01 <- rep(corr01, nrow(collexdf4249))
corr001 <- 0.001/nrow(collexdf4249)
collexdf4249$corr001 <- rep(corr001, nrow(collexdf4249))
# calculate corrected significance status
collexdf4249$sig <- as.vector(unlist(sapply(collexdf4249$p, function(x){
  x <- as.numeric(x)
  x <- ifelse(x <= corr05, "p<.05",
              ifelse(x <= corr01, "p<.01",
                     ifelse(x <= corr001, "p<.001", "n.s."))) } )))
# remove non-significant combinations
sigcollexdf4249 <- collexdf4249[as.numeric(collexdf4249$p) <= .05, ]
corrsigcollexdf4249 <- collexdf4249[collexdf4249$sig != "n.s.", ]
# inspect results
sigcollexdf4249

corrsigcollexdf4249






















###############################################################
###############################################################
### --- tabulate data
###############################################################
###############################################################
###############################################################
# create data for tabularizaion
wcmot <- by(motint$wc, list(motint$file), unique)
motwc <- as.vector(unlist(wcmot))
motfile <- as.vector(unlist(names(wcmot)))
mdat <- data.frame(1:length(motfile), motfile, motwc)
motadj <- as.vector(unlist(table(motint$file)))
motabsint <- as.vector(unlist(by(motint$absint, list(motint$file), sum)))
motrat <- round(motabsint/motadj, 2)
###
motint$awful <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "awful", 1, 0) } )))
motint$awfully <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "awfully", 1, 0) } )))
motint$pretty <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "pretty", 1, 0) } )))
motint$real <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "real", 1, 0) } )))
motint$really <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "really", 1, 0) } )))
motint$so <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "so", 1, 0) } )))
motint$too <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "too", 1, 0) } )))
motint$very <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "very", 1, 0) } )))
motint$wonderfully <- as.vector(unlist(sapply(motint$prelex, function(x){ x <- ifelse(x == "wonderfully", 1, 0) } )))
###
motawful <- as.vector(unlist(by(motint$awful, list(motint$file), sum)))
motawfully <- as.vector(unlist(by(motint$awfully, list(motint$file), sum)))
motpretty <- as.vector(unlist(by(motint$pretty, list(motint$file), sum)))
motreal <- as.vector(unlist(by(motint$real, list(motint$file), sum)))
motreally <- as.vector(unlist(by(motint$really, list(motint$file), sum)))
motso <- as.vector(unlist(by(motint$so, list(motint$file), sum)))
mottoo <- as.vector(unlist(by(motint$too, list(motint$file), sum)))
motvery <- as.vector(unlist(by(motint$very, list(motint$file), sum)))
motwonderfully <- as.vector(unlist(by(motint$wonderfully, list(motint$file), sum)))
# creat data frame
mdat <- data.frame(1:length(motfile), motfile, motwc, motabsint, motadj, motrat, motawful,
                   motawfully, motpretty, motreal, motreally, motso, mottoo, motvery, motwonderfully)
# clean column names
colnames(mdat)[1:2] <- c("id", "file")
# inspect data
head(mdat)

###############################################################
# join child and mother data
data2 <- join(chint, mdat, by = "file", type = "left", match = "all")
# add child column
data2$chi <- gsub(".*\\/", "", data2$file)
data2$chi <- gsub("[0-9]", "", data2$chi)
data2$chi <- gsub("[a-z]{2,2}$", "", data2$chi)
# inspect data
head(data2)

###############################################################
# extract file based data
filechiwcagesex <- data.frame(data2$file, data2$wc, data2$agecat, data2$gender)
chiwcagesex <- unique(filechiwcagesex)
colnames(chiwcagesex) <- c("file", "chiwc", "age", "sex")
###
filemotwc <- data.frame(data2$file, data2$motwc, data2$motabsint)
motwc <- unique(filemotwc)
colnames(motwc) <- c("file", "motwc", "motabsint")
###
absint <- as.vector(unlist(by(data2$absint, list(data2$file), sum)))
fileabsint <- names(by(data2$absint, list(data2$file), sum))
fileabsint <- data.frame(fileabsint, absint)
colnames(fileabsint) <- c("file", "absint")
# combine information
data3 <- join(chiwcagesex, motwc, by = "file", type = "left", match = "all")
data4 <- join(data3, fileabsint, by = "file", type = "left", match = "all")
# add child column
data4$chi <- gsub(".*\\/", "", data4$file)
data4$chi <- gsub("[0-9]", "", data4$chi)
data4$chi <- gsub("[a-z]{2,2}$", "", data4$chi)
# add styp column
data4$styp <- gsub("HV[0-9]\\/", "", data4$file)
data4$styp <- gsub("\\/.*", "", data4$styp)
data4$styp <- tolower(data4$styp)
# remove rows with incomplete information
data4 <- na.omit(data4)
# inspect data
head(data4)

write.table(data4, "data4.txt", row.names = F, sep="\t")
###############################################################
# create agecat based data set
info <- by(data4$chi, list(data4$styp, data4$sex, data4$age), length)
nspk <-  as.vector(unlist(by(data4$chi, list(data4$styp, data4$sex, data4$age), length)))
chiwc <- as.vector(unlist(by(data4$chiwc, list(data4$styp, data4$sex, data4$age), sum)))
absint <- as.vector(unlist(by(data4$absint, list(data4$styp, data4$sex, data4$age), sum)))
motabsint <- as.vector(unlist(by(data4$motabsint, list(data4$styp, data4$sex, data4$age), sum)))
motwc <-  as.vector(unlist(by(data4$motwc, list(data4$styp, data4$sex, data4$age), sum)))
chiptw <- round(absint/chiwc*1000, 2)
motptw <- round(motabsint/motwc*1000, 2)
###
styp <- rep(c("br", "et", "lw", "mt", "tp"), 16)
sex <- rep(c(rep("female", 5), rep("male", 5)), 8)
age <- c(rep(3, 10), rep(4, 10), rep(5, 10),rep(6, 10),rep(7, 10),rep(8, 10),rep(9, 10), rep(10, 10))
#combine data to table
df1 <- data.frame(age, sex, styp, nspk, absint, chiwc, chiptw, motabsint, motwc, motptw)
# remove incolplete rows
df1 <- na.omit(df1)
# inspect results
head(df1)

write.table(df1, "df1.txt", row.names = F, sep="\t")
###############################################################
# create agecat based data set
info2 <- by(data4$chi, list(data4$sex, data4$age), length)
nspk2 <-  by(data4$chi, list(data4$sex, data4$age), table)
nspk2 <- sapply(nspk2, function(x){ x <- length(x) } )
chiwc2 <- as.vector(unlist(by(data4$chiwc, list(data4$sex, data4$age), sum)))
absint2 <- as.vector(unlist(by(data4$absint, list(data4$sex, data4$age), sum)))
motabsint2 <- as.vector(unlist(by(data4$motabsint, list(data4$sex, data4$age), sum)))
motwc2 <-  as.vector(unlist(by(data4$motwc, list(data4$sex, data4$age), sum)))
chiptw2 <- round(absint2/chiwc2*1000, 2)
motptw2 <- round(motabsint2/motwc2*1000, 2)
###
sex2 <- rep(c("female", "male"), 8)
age2 <- c(rep(3, 2), rep(4, 2), rep(5, 2),rep(6, 2),rep(7, 2),rep(8, 2),rep(9, 2), rep(10, 2))
#combine data to table
df2 <- data.frame(age2, sex2, nspk2, absint2, chiwc2, chiptw2, motabsint2, motwc2, motptw2)
# remove incolplete rows
sumspk2 <- as.vector(unlist(length(table(data4$chi))))
sumabsint2 <- as.vector(unlist(sum(data4$absint)))
sumchiwc2 <- sum(chiwc2)
sumchiptw2 <- round(sumabsint2/sumchiwc2*1000, 2)
summotabsint2 <- sum(motabsint2)
summotwc2 <- sum(motwc2)
summotptw2 <- round(summotabsint2/summotwc2*1000, 2)
total <- c("", "", sumspk2, sumabsint2, sumchiwc2, sumchiptw2, summotabsint2, summotwc2, summotptw2)
# add to table
df2 <- rbind(df2, total)
# inspect results
head(df2)

write.table(df2, "df2.txt", row.names = F, sep="\t")
###############################################################
###############################################################
###############################################################
# create agecat based data set
nspk0 <-  length(names(table(data4$chi)))
chiwc0 <- as.vector(unlist(sum(data4$chiwc)))
absint0 <- as.vector(unlist(sum(data4$absint)))
motabsint0 <- as.vector(unlist(sum(data4$motabsint)))
motwc0 <-  as.vector(unlist(sum(data4$motwc)))
chiptw0 <- round(absint0/chiwc0*1000, 2)
motptw0 <- round(motabsint0/motwc0*1000, 2)
#combine data to table
df0 <- cbind(nspk0, absint0, chiwc0, chiptw0)
df0 <- rbind(df0, c("", motabsint0, motwc0, motptw0))
# add row and column names
colnames(df0) <- c("Speakers (N)", "Intensifiers (N)", "Words (N)", "Rel. Freq. (ptw)")
rownames(df0) <- c("Children", "Mothers")
# inspect results
head(df0)

write.table(df0, "df0.txt", row.names = F, sep="\t")
###############################################################
# add ptw to data4
data4$intptw <- round(data4$absint/data4$chiwc*1000, 2)
# plot genders (scatterplot)
png("images/intagegenderptw.png") # save plot
scatterplot(intptw ~ as.numeric(age)|sex, data=data4, reg.line = FALSE, xlab = "Age",
            ylab = "Rel. Frequency", axes = F)
axis(1, at= 1:8, labels = 3:10)
axis(2, at = seq(0, 25, 5), labels = seq(0, 25, 5))
grid()
dev.off()
# plot styp (scatterplot)
png("images/intagestypptw.png") # save plot
scatterplot(intptw ~ as.numeric(age)|styp, data=data4, reg.line = FALSE, xlab = "Age",
            ylab = "Rel. Frequency", axes = F)
axis(1, at= 1:8, labels = 3:10)
axis(2, at = seq(0, 25, 5), labels = seq(0, 25, 5))
grid()
dev.off()
###############################################################
###############################################################
###############################################################
# analogous plot in ggplot2
###############################################################
###############################################################
###############################################################
# analogous plot in ggplot2
chint$absint <- chint$absint*100
agemeans <- as.vector(by(chint$absint, chint$agecat2, mean))
agemeans <- agemeans[complete.cases(agemeans)]
p0 <- ggplot(chint, aes(agecat2, absint, color = agecat2)) +
  scale_fill_brewer() +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_set(theme_bw(base_size = 20)) +
  coord_cartesian(ylim = c(0, 50)) +
  labs(x = "Age of Child", y = "Percent of Adj. Slots", colour = "agecat2", cex = .75) +
  scale_color_manual(values = c("grey20",  "grey20", "grey20", "grey20")) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[1], 2), sep = ""), x = 1, y = agemeans[1] + 10, colour = "grey20", size = 4) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[2], 2), sep = ""), x = 2, y = agemeans[2] + 10, colour = "grey20", size = 4) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[3], 2), sep = ""), x = 3, y = agemeans[3] + 10, colour = "grey20", size = 4) +
  geom_text(mapping = NULL, label = paste("mean=\n", round(agemeans[4], 2), sep = ""), x = 4, y = agemeans[4] + 10, colour = "grey20", size = 4) +
  theme(legend.position="none", axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intageggplot.png",sep="/")
ggsave(file = imageFile)
chint$absint <- chint$absint/100
# activate (remove #) to show
p0

###
chint$absint <- chint$absint*100
chintcomp <- chint[is.na(chint$gender) == F, ]
p1 <- ggplot(chintcomp, aes(agecat2, absint, colour = gender)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = gender)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ gender, nrow = 1) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "", y = "Percent of Adj. Slots", colour = "gender") +
  theme(legend.position="none", axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold")) +
  scale_color_manual(values = c("red", "lightblue"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intAgeSexggplot.png",sep="/")
ggsave(file = imageFile)
chint$absint <- chint$absint/100
# activate (remove #) to show
p1

###
chint$absint <- chint$absint*100
chintsf <- chint[chint$styp != "et",]
p2 <- ggplot(chintsf, aes(agecat2, absint, colour = styp)) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "point", aes(group = styp)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  facet_wrap(~ styp, nrow = 1) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_set(theme_bw(base_size = 20)) +
  labs(x = "Age of Child", y = "Percent of Adj. Slots", colour = "styp", cex = .5) +
  theme(legend.position="none", axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold")) +
  scale_color_manual(values = c("grey30", "grey30", "grey30", "grey30", "grey30", "grey30", "grey30"))
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intAgeStypggplot.png",sep="/")
ggsave(file = imageFile)
chint$absint <- chint$absint/100
# activate (remove #) to show
p2

###
chint$absint <- chint$absint*100
chintint <- chint#[chint$absint != 0,]
chintint$very <- ifelse(chintint$pint == "very", 1, 0)
chintint$so <- ifelse(chintint$pint == "so", 1, 0)
chintint$real <- ifelse(chintint$pint == "real", 1, 0)
chintint$really <- ifelse(chintint$pint == "really", 1, 0)
chintint$pretty <- ifelse(chintint$pint == "pretty", 1, 0)
chintint$other <- as.vector(unlist(sapply(chintint$pint, function(x){
  x <- ifelse(x == "awful", 1, 
              ifelse(x == "fucking", 1,
                     ifelse(x == "totally", 1,
                            ifelse(x == "well", 1,
                                   ifelse(x == "wicked", 1, 0)))))  })))
very <- with(chintint, do.call(rbind, tapply(very, agecat2, function(x) c(M = mean(x), SD = sd(x)))))
so <- with(chintint, do.call(rbind, tapply(so, agecat2, function(x) c(M = mean(x), SD = sd(x)))))
real <- with(chintint, do.call(rbind, tapply(real, agecat2, function(x) c(M = mean(x), SD = sd(x)))))
really <- with(chintint, do.call(rbind, tapply(really, agecat2, function(x) c(M = mean(x), SD = sd(x)))))
pretty <- with(chintint, do.call(rbind, tapply(pretty, agecat2, function(x) c(M = mean(x), SD = sd(x)))))
other <- with(chintint, do.call(rbind, tapply(other, agecat2, function(x) c(M = mean(x), SD = sd(x)))))
chintintg <- rbind(very, so, real, really, pretty, other)
df3 <- data.frame(rep(c("very", "so", "real", "really", "pretty", "other"), each = 4), rep(c("3-4", "5-6", "7-8", "9-10"), 6), chintintg)
colnames(df3) <- c("pint", "age", "Percent", "sd")
df3

# Use position_dodge to move overlapped errorbars horizontally
p3 <- ggplot(df3, aes(x=age, y=Percent, group=pint, color=pint)) + 
  geom_errorbar(aes(ymin=Percent-sd, ymax=Percent+sd), width=.1, 
                position=position_dodge(0.05)) +
  geom_line() + geom_point()+
  scale_color_brewer(palette="Paired")+theme_minimal()
# activate (remove #) to save
imageFile <- paste(imageDirectory,"intTypeAgeggplot.png",sep="/")
ggsave(file = imageFile)
chint$absint <- chint$absint/100
# activate (remove #) to show
p3

###############################################################
###############################################################
###############################################################
### --- statz
###############################################################
###############################################################
###############################################################
# remove all incomplete cases from data set
data2 <- chint[complete.cases(chint),]
data2$so <- as.vector(unlist(sapply(data2$pint, function(x){
  x <- ifelse(x == "so", 1, 0)})))



################################################################
### ---        Model Building - GLMEBR
################################################################
# set options
options(contrasts  =c("contr.treatment", "contr.poly"))
data.dist <- datadist(data2)
options(datadist = "data.dist")
data2$ageedit <- as.numeric(data2$ageedit)
# a few words on glm vs lrm: Baayen (2008:196-197) states that lrm should be
# the function of choice in cases where each row contains
# exactly 1 success OR failure (1 or 0) while glm is preferrable if there are two
# columns holding the number of successes and the number of failures
# respectively. i have tried it both ways and both functions work fine if
# each row contains exactly 1 success OR failure but only glm can handle the
# latter case.
# generate initial saturated regression model including
# all variables and their interactions
m0.glm = glm(so ~ 1, family = binomial, data = data2) # baseline model glm
m0.lrm = lrm(so ~ 1, data = data2, x = T, y = T) # baseline model lrm
# inspect results
summary(m0.glm)

m0.lrm

###########################################################################
# create model with a random intercept for childid
m0.lmer <- lmer(so ~ (1|file), data = data2, family = binomial) # do not use: deprecated!
# Baayen (2008:278-284) uses the call above but the this call is now longer
# up-to-date because the "family" parameter is deprecated
# we switch to glmer (suggested by R) instead but we will also
# create a lmer object of the final minimal adequate model as some functions
# will not (yet) work on glmer
m0.glmer = glmer(so ~ (1|file), data = data2, family = binomial)

# results of the lmer object
print(m0.lmer, corr = F)

# check if including the random effect is permitted by comparing the aic from the glm to aic from the glmer model
aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

# the aic of the glm object is smaller which shows that including the random
# intercepts is NOT justified
###
# testing whether random intercepts are justified using a intlihood ratio test
lrt <- function (obj1, obj2) {
  L0 <- logLik(obj1)
  L1 <- logLik(obj2)
  L01 <- as.vector(- 2 * (L0 - L1))
  df <- attr(L1, "df") - attr(L0, "df")
  list(L01 = L01, df = df,
       "p-value" = pchisq(L01, df, lower.tail = FALSE))
}
lrt(obj1=m0.glm, obj2=m0.glmer)

# the test confirms that including random intercepts is NOT justified
# inspect results
summary(m0.glm)

summary(m0.glmer)

###########################################################################
# load packages
require(foreign)
require(nnet)
require(ggplot2)
require(reshape2)

m1.mnr <- multinom(pint ~ ageedit + gender, data = data2)
m1.mnr

summary(m1.mnr)

z <- summary(m1.mnr)$coefficients/summary(m1.mnr)$standard.errors
z

exp(coef(m1.mnr))

head(pp <- fitted(m1.mnr))

dses <- data.frame(ses = c("low", "middle", "high"), write = mean(ml$write))
predict(m1.mnr, newdata = dses, "probs")


# model fitting
# fit the model to find the "best" model, i.e. the minimal adequate model
# we will use a step-wise step up, i.e. forward, procedure although with step-wise step-up
# the intlyhood of type II errors is much higher than with step-wise
# step down, i.e. backward elimination(cf. Field, Miles & Field 2012:265)
#	automated model fitting
m1.glm <- glm(so ~ ageedit, family = binomial, data = data2)
m2.glm <- glm(so ~ ageedit + gender, family = binomial, data = data2)
m3.glm <- glm(so ~ ageedit + gender + styp, family = binomial, data = data2)
m4.glm <- glm(so ~ ageedit + gender + styp + ageedit : gender, family = binomial, data = data2)
m5.glm <- glm(so ~ ageedit + gender + styp + ageedit : gender + ageedit : styp, family = binomial, data = data2)
m6.glm <- glm(so ~ ageedit + gender + styp + ageedit : gender + ageedit : styp + gender : styp, family = binomial, data = data2)
m7.glm <- glm(so ~ ageedit + gender + styp + ageedit : gender + ageedit : styp + gender : styp + ageedit : gender : styp, family = binomial, data = data2)

# we compare all models because this way, we get an overview of model paramerets
# and can check which model has the lowerst AIC, BIC, and the highest X^2 value
anova(m0.glm, m1.glm, m2.glm, m3.glm, m4.glm, m5.glm, m6.glm, m7.glm, test = "Chi")

# use customized model comparison function
# create comparisons
m1.m0 <- anova(m1.glm, m0.glm, test = "Chi") # not sig
m1.m0

m2.m0 <- anova(m2.glm, m0.glm, test = "Chi") # not sig
m2.m0

m3.m0 <- anova(m3.glm, m0.glm, test = "Chi") # not sig
m3.m0

m4.m0 <- anova(m4.glm, m0.glm, test = "Chi")
m4.m0

m5.m4 <- anova(m5.glm, m4.glm, test = "Chi")



# model 5: m4.glm is our final model
# create a list of the model comparisons
mdlcmp <- list(m1.m0, m2.m1, m3.m2, m4.m3, m5.m4, m6.m5, m7.m6)
# apply function
mdl.cmp.glm.swsu <- mdl.fttng.glm.swsu(mdlcmp)
# inspect output
mdl.cmp.glm.swsu

write.table(mdl.cmp.glm.swsu, "mdl_cmp_glm_swsd.txt", sep="\t")
###########################################################################
mfin.glm <- glm(absint ~ agecat + motrat + styp, family = binomial, data = data2)
# rename final minimal adeqaute model
mlr.glm <- mfin.glm

# test if the final minimal adequate model performs better than the base-line model
anova(mlr.glm, m0.glm, test = "Chi")

# inspect results of the final minimal adequate model
print(mlr.glm, corr = F)

# alternative result display (anova)
anova(mlr.glm)

# alternative result display (anova)
anovasummary <- Anova(mlr.glm, type = "III", test = "Wald")

# extract the parameters of the fixed effects for the report
# to do that, we compare the model with only the random effect to a model with
# the random effect and the fixed effect for age
age.effect.glm <- anova(m0.glm, m1.glm, test = "Chi") # effect of age

# we test the effect of motrat, by adding stype and compare a
# model with motrat to a model without motrat
motrat.effect.glm <- anova(m2.glm, m3.glm, test = "Chi") # effect of motrat

# we test the effect of styp, by adding stype and compare a
# model with styp to a model without styp
styp.effect.glm <- anova(m4.glm, m3.glm, test = "Chi") # effect of styp

###########################################################################
### --- extracting and calculating model fit parameters
# we now create a lmr object equivalent to the final minimal adequate model
# but without the random effect
mlr.lrm <- lrm(absint ~ agecat + motrat + styp, data = data2, x = T, y = T)
# we now create a lmer object equivalent to the final minimal adequate model
mlr.lmer <- lmer(absint ~ agecat + motrat + styp + (1|chi), data = data2, family = binomial)

# now we check if the fixed effects of the lrm and the lmer model correlate (cf Baayen 2008:281)
cor.test(coef(mlr.lrm), fixef(mlr.lmer))

# the fixed effects correlate very strongly - this is good as it suggests that
# the coefficient estimates are very stable
# calculate prediction accuracy
pred <- as.vector(unlist(sapply(fitted(mlr.glm), function(x){
  x <- ifelse(x > .5, 1, 0) } )))
accuracy <- table(pred, data2$absint)
accuracy <- sum(diag(accuracy))/sum(accuracy)
accuracy <- round(accuracy*100, 2)
# extract summary of final model
blrm.summary <- blrm.summary(mlr.glm, mlr.lrm, accuracy)
blrm.summary

write.table(blrm.summary, "blrmsummary.txt", sep="\t")
###########################################################################
# we activate the package Hmisc (if not already active)
library(Hmisc)
# we now extract model fit parameters (cf Baayen 2008:281)
probs = 1/(1+exp(-fitted(mlr.glm)))
probs = binomial()$linkinv(fitted(mlr.glm))
somers2(probs, as.numeric(data2$absint))

# the model fit values indicate a very good fit:
# C
# "The measure named C is an index of concordance between the predicted
# probability and the observed response. [...] When C takes the value 0.5, the
# predictions are random, when it is 1, prediction is perfect. A value above
# 0.8 indicates that the model may have some real predictive capacity."
# (Baayen 2008:204)
# "Somers? Dxy,
# a rank correlation between predicted probabilities and observed responses, [...]
# ranges between 0 (randomness) and 1 (perfect prediction)." (Baayen 2008:204)

# extract Pseudo R^2 values for binomial logistic mixed effects model
my.lmer.nagelkerke(c("absint ~ agecat + motrat + styp", "(1 | childid)"), data)

###########################################################################
# model diagnostics: plot fitted against residuals
plot(mlr.glm)


###############################################################
#inspect the results meblrm.summary(glm0, glm1, glmer0, glmer1, dpvar)
meblrmsc <- meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, data$intcmsm) #
write.table(meblrmsc, "meblrmsc.txt", sep="\t")
################################################################
################################################################
################################################################
################################################################
### --- VERY IMPORTANT OBJECTS
################################################################
################################################################
################################################################
# inspect very important objects
head(data)

int.df1

# glmer
mdl.cmp.glmersc.swsu

mdl.cmp.glmersc.swsu.gries

age.effect.glmersc

styp.effect.glmersc

stypegender.effect.glmersc

meblrm.summary(m0.glm, m1.glm, m0.glmer, mlr.glmer, data$intcmsm)

anovasummary

###############################################################
###############################################################
###############################################################
### ---                   THE END
###############################################################
###############################################################
###############################################################

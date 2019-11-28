##################################################################
# Titel:      On the L1-Acquisition of Amplification in English - Part 2
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2019-11-28
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
# set options
options(stringsAsFactors = F)
# define image directors
imageDirectory<-"images"
# activate packages
library(dplyr)
# load functions
source("D:\\R/ConcR_2.3.r")
##########################################################
# create kwic
# set parameters
pathname <- "D:\\Uni\\Korpora\\Edited\\HSLLD-PosTagged"
context <- 80
all.pre = T
search.pattern <-  c("[:alnum:]{0,}-{0,1}[:alnum:]{2,}\\/JJ[:alnum:]{0,}")
# start searches
concampl1 <- ConcR(pathname, search.pattern, context, all.pre = T)
# activate packages
library(dplyr) 
library(stringr)
# data processing
ampl1conc <- concampl1 %>%
  dplyr::rename(File = file, Speaker = all.pre, Token = token, 
                PreceedingContext = pre, SubsequentContext = post) %>%
  dplyr::mutate(File = str_replace_all(File, ".txt", "")) %>%
  dplyr::mutate(File = str_replace_all(File, "-", "/")) %>%
  dplyr::mutate(Speaker = str_replace_all(Speaker, ".* ([A-Z]{2,}_{0,1}[0-9]{0,1}:)", 
                                          "\\1")) %>%
  dplyr::mutate(Speaker = str_replace_all(Speaker, ":.*", "")) %>%
  dplyr::mutate(Function = str_trim(SubsequentContext, side = "both")) %>%
  dplyr::mutate(Function = str_replace_all(Function, " .*", "")) %>%
  dplyr::mutate(Function = str_replace_all(Function, ".*\\/", "")) %>%
  dplyr::mutate(Function = str_replace_all(Function, "NNS", "NN")) %>%
  dplyr::mutate(Function = ifelse(Function == "NN", "Attributive", "Predicative"))
# load bio data
bio <- read.delim("datatables/bio.txt", sep = "\t", header = T, quote = "")
# join bio data and conc data
ampl1 <- dplyr::left_join(bio, ampl1conc)
# data processing
ampl1 <- ampl1 %>%
  dplyr::mutate(Id = 1:nrow(ampl1)) %>%
  dplyr::select(-all) %>%
  dplyr::mutate(PreviousPoS  = str_trim(PreceedingContext, side = "both")) %>%
  dplyr::mutate(PreviousPoS  = str_replace_all(PreviousPoS, ".* ", "")) %>%
  dplyr::mutate(PreviousPoS  = str_replace_all(PreviousPoS, ".*/", "")) %>%
  dplyr::mutate(PoSTag  = str_replace_all(Token, ".*/", "")) %>%
  dplyr::mutate(Token  = str_replace_all(Token, "/.*", "")) %>%
  dplyr::rename(Adjective = Token) %>%
  dplyr::rename(Activities = Actvities, Visit = Situation) %>%
  dplyr::mutate(Activities  = str_replace_all(Activities, fixed("("), "qwertz")) %>%
  dplyr::mutate(Activities  = str_replace_all(Activities, "qwertz.*", "")) %>%
  dplyr::mutate(Visit  = str_replace_all(Visit, ";.*", ""))  %>%
  dplyr::mutate(PreviousWord  = str_trim(PreceedingContext, side = "both")) %>%
  dplyr::mutate(PreviousWord  = str_replace_all(PreviousWord, ".* ", "")) %>%
  dplyr::mutate(PreviousWord  = str_replace_all(PreviousWord, "/.*", ""))
# code amplifiers
variants <- c("awful", "awfully",  "completely", "extra", "fucking",  
              "pretty", "radically", "real", "really", "so", "terrible",  
              "total", "totally", "very", "wonderfully")
# code amplifiers
ampl1 <- ampl1 %>%
  dplyr::mutate(Variant  = ifelse(PreviousWord %in% variants, PreviousWord, "0")) %>%
  dplyr::mutate(Amplified  = ifelse(PreviousWord %in% variants, "yes", "no"))
# add bioinformation to speakers
female <- c("ANT", "AUN",  "FE1", "FE2", "FE3",  "FEM", "GMA", "GRF",  "GRM",
            "JES", "LIS", "MEG",  "MIA",  "MOT",  "SHE",  "SI1",  "SI2",  "SI3",
            "SIS",  "SIS1", "SIS2", "SIS3", "WOM")
male <- c("BFD",  "BR1",  "BR2",  "BR3",  "BRO", "BRO1", "BRO2", "FAT", "GFA", "GPA",
          "JAK", "MA1",  "MA2",  "MAL",  "MAL2", "MAN",   "ROB", "STE", "TYR",  "UNC")
ampl1 <- ampl1 %>%
  dplyr::mutate(Gender = ifelse(Speaker %in% female, "female",
                                ifelse(Speaker %in% male, "male",
                                       ifelse(Speaker == "MOT|Aunt", "female", Gender))))
# convert Age to Decimal
ampl1 <- ampl1 %>%
  dplyr::mutate(OriginalAge = Age) %>%
  dplyr::mutate(Age = str_replace_all(Age, fixed("."), "qwertz")) %>%
  dplyr::mutate(Age = str_replace_all(Age, "qwertz.*", "")) %>%
  dplyr::mutate(Year = str_replace_all(Age, ";.*", "")) %>%
  dplyr::mutate(Month = str_replace_all(Age, ".*;", "")) %>%
  dplyr::mutate(Month = as.numeric(Month)/12*100) %>%
  dplyr::mutate(Age = paste(Year, ".", Month, sep = "")) %>%
  dplyr::select(-Year, -Month)
# code interlocutor
parent <- c("FAT", "MOT")
caregiver <- c("ANT", "AUN", "GFA", "GMA", "GPA", "GRA", "GRF", "GRM", "UNC")
sibling <- c("BR1", "BR2", "BR3", "BRO", "BRO1", "BRO2", "CHI2", "COS", 
             "COU", "COU2", "COU3", "SI1", "SI2", "SI3", "SIB", "SIS", 
             "SIS1", "SIS2", "SIS3", "FR1", "FR2", "FRE", "FRI")
child <- c("CHI")
# perform recoding
ampl1 <- ampl1 %>%
  dplyr::mutate(Interlocutor = ifelse(Speaker %in% parent, "PrimaryCaregiver",
                                      ifelse(Speaker %in% caregiver, "SecondaryCaregiver",
                                      ifelse(Speaker %in% sibling, "PeerSibling", 
                                      ifelse(Speaker %in% child, "Child", "Other")))))
# recode Location
ampl1 <- ampl1 %>%
  dplyr::mutate(Location = str_replace_all(Location, " .*", ""))
# recode Visit
ampl1 <- ampl1  %>%
  dplyr::mutate(Visit = str_replace_all(File, "/.*", "")) %>%
  dplyr::mutate(Visit = str_replace_all(Visit, "[:alpha:]", ""))
# recode Activities
ampl1 <- ampl1 %>%
  dplyr::rename(Activity = Activities) %>%
  dplyr::mutate(Activity = str_replace_all(File, ".*/", "")) %>%
  dplyr::mutate(Activity = substr(Activity, 4, 5)) %>%
  dplyr::mutate(SituationType = ifelse(Activity == "br", "formal",
                                  ifelse(Activity == "tp", "informal",
                                         ifelse(Activity == "mt", "informal",
                                                ifelse(Activity == "er", "formal",
                                                       ifelse(Activity == "et", "formal",
                                                              ifelse(Activity == "lw", "formal",
                                                                     ifelse(Activity == "re", "formal",
                                                                            ifelse(Activity == "md", "formal", Activity))))))))) %>%
  dplyr::mutate(Activity = ifelse(Activity == "br", "Book reading",
                                       ifelse(Activity == "tp", "Toy play",
                                              ifelse(Activity == "mt", "Meal time",
                                                     ifelse(Activity == "er", "Elicited report",
                                                            ifelse(Activity == "et", "Experimental task",
                                                                   ifelse(Activity == "lw", "Letter writing",
                                                                          ifelse(Activity == "re", "Book reading",
                                                                                 ifelse(Activity == "md", "Meal time", Activity)))))))))
# remove language
ampl1 <- ampl1 %>%
  dplyr::select(-Language)
# add number of participants
ampl1 <- ampl1 %>%
  dplyr::mutate(ParticipantNumber = str_count(Participants, ",") +1)
# add age groups
ampl1 <- ampl1 %>%
  dplyr::mutate(AgeGroup = str_replace_all(Age, fixed("."), "qwertz")) %>%
  dplyr::mutate(AgeGroup = str_replace_all(AgeGroup, "qwertz.*", "")) %>%
  dplyr::mutate(AgeGroup = ifelse(AgeGroup == "3" | AgeGroup == "4" , "3-4", 
                         ifelse(AgeGroup == "5" | AgeGroup == "6" , "5-6", 
                                ifelse(AgeGroup == "7" | AgeGroup == "8" , "7-8",
                                       ifelse(AgeGroup == "9" | AgeGroup == "10" , "9-10", 
                                              ifelse(AgeGroup == "11" | AgeGroup == "12" , "11-12",AgeGroup))))))
# inspect data
head(ampl1); str(ampl1)

# save data to disc
write.table(ampl1, file = "datatables/ampl1.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE, quote = F)
###############################################################
#                       END PART 2
###############################################################

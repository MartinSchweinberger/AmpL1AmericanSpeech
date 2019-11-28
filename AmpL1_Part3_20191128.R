##################################################################
# Titel:      On the L1-Acquisition of Amplification in English - Part 3
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
# clean current workspace
rm(list=ls(all=T))                                      
# set options
options(stringsAsFactors = F)
# define image directors
imageDirectory<-"images"
# activate packages
library(dplyr)
# load function
source("D:\\R/ConcR_2.6_LoadedFiles.r")
###############################################################
# load and process data
ampl1 <- read.delim("datatables/ampl1.txt", sep = "\t", header = T, quote = "")
# inspect data
head(ampl1)

##########################################################
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
supsidx <- unique(grep(paste(sups,collapse="|"), ampl1$PreceedingContext, value=F))
negsidx <- unique(grep(paste(negs,collapse="|"), ampl1$PreceedingContext, value=F))
downtonersidx <- unique(grep(paste(downtoners,collapse="|"), ampl1$PreceedingContext, value=F))
specialformsidx <- unique(grep(paste(specialforms,collapse="|"), ampl1$PreceedingContext, value=F))
PostContextdowntonersidx <- unique(grep(paste(PostContextdowntoners,collapse="|"), ampl1$SubsequentContext, value=F))
nonpropadjidx <- unique(grep(paste(nonpropadj,collapse="|"), ampl1$Adjective, value=F))
# combine indices
idxs <- unique(c(supsidx, negsidx, downtonersidx, specialformsidx, PostContextdowntonersidx, nonpropadjidx))
# inspect data
nrow(ampl1)

# remove forms that require removal
ampl1 <- ampl1[-idxs,]
# remove empty values
ampl1 <- ampl1[!ampl1$Variant == "", ]
data <- ampl1[ampl1$PoSTag== "JJ", ]
# inspect data
nrow(data)
# remove adjectives that were never amplified
ampadjs <- data %>%
  dplyr::filter(Amplified == "yes")
ampadjs <-  names(table(ampadjs$Adjective))
data <- data %>%
  dplyr::filter(Adjective %in% ampadjs)
# inspect data
nrow(data)

# inspect data
head(data)

# save data to disc
write.table(data, file = "datatables/data.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE, quote = F)
###############################################################
#                       END PART 3
###############################################################

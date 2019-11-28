##################################################################
# Titel:      On the L1-Acquisition of Amplification in English - Part 1
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2019-11-28
# Contact:    martin.schweinberger.hh@gmail.com
# Disclaimer: If you have questions,suggestions or you found errors
#             or in case you would to provide feedback, questions
#             write an email to martin.schweinberger.hh@gmail.com.
# Citation:   If you use this script or results thereof, please cite it as:
#             Schweinberger, Martin. 2019. "On the L1-Acquisition 
#             of Amplification in English - Part 1",
#             unpublished R script, The University of Queensland.
###############################################################
rm(list=ls(all=T))                                      # clean current workspace
library(stringr)                                        # load packages
options(stringsAsFactors = F)                           # set options
options(scipen = 999)                                   # set options
imageDirectory<-"images"                                # define image directory
# define paths to data
corpus <- "D:\\Uni\\Korpora\\Original\\CHILDES\\Eng-NA-MOR\\HSLLD"
###############################################################
#                       START
###############################################################
# choose the files
cha = list.files(path = corpus, pattern = ".cha$", all.files = T,
                 full.names = T, recursive = T, ignore.case = T, include.dirs = T)
# extract biodata
bio <- sapply(cha, function(x) {
  file <- gsub("D:\\Uni\\Korpora\\Original\\CHILDES\\Eng-NA-MOR\\HSLLD/", "", x, fixed = T)
  file <- gsub(".cha", "", file, fixed = T)
  x <- scan(x, what = "char", sep = "\t", quiet = T, quote = "", skipNul = T)
  x <- gsub("\n", " ", x, fixed = T)
  x <- gsub("\t", " ", x, fixed = T)
  x <- str_trim(x, side = "both")
  x <- gsub(" {2,}", " ", x)
  x <- paste(x, collapse = " ", sep = " ")
  x <- str_replace_all(x, "\\*.*", "")
  rps <- str_count(x, "@ID")
  # name
  nam <- str_replace_all(file, ".*/", "")
  # id
  pid <- str_replace_all(x, ".*@PID", "@PID")
  # pid
  pid <- str_replace_all(x, ".*@PID: ", "")
  pid <- str_replace_all(pid, "@.*", "")
  pid <- str_trim(pid, side = "both")
  pid <- gsub(" {2,}", " ", pid)
  # languages
  lan <- str_replace_all(x, ".*@Languages: ", "")
  lan <- str_replace_all(lan, "@.*", "")
  lan <- str_trim(lan, side = "both")
  lan <- gsub(" {2,}", " ", lan)
  # participants
  par <- str_replace_all(x, ".*@Participants: ", "")
  par <- str_replace_all(par, "@.*", "")
  par <- str_trim(par, side = "both")
  par <- gsub(" {2,}", " ", par)
  # birth of child
  bir <- str_replace_all(x, ".*@Birth of CHI: ", "")
  bir <- str_replace_all(bir, "@.*", "")
  bir <- str_trim(bir, side = "both")
  bir <- gsub(" {2,}", " ", bir)
  # comment
  com <- str_replace_all(x, ".*@Comment: ", "")
  com <- str_replace_all(com, "@.*", "")
  com <- str_trim(com, side = "both")
  com <- gsub(" {2,}", " ", com)
  # date
  dat <- str_replace_all(x, ".*@Date: ", "")
  dat <- str_replace_all(dat, "@.*", "")
  dat <- str_trim(dat, side = "both")
  dat <- gsub(" {2,}", " ", dat)
  # location
  loc <- str_replace_all(x, ".*@Location: ", "")
  loc <- str_replace_all(loc, "@.*", "")
  loc <- str_trim(loc, side = "both")
  loc <- gsub(" {2,}", " ", loc)
  # situation
  sit <- str_replace_all(x, ".*@Situation: ", "")
  sit <- str_replace_all(sit, "@.*", "")
  sit <- str_trim(sit, side = "both")
  sit <- gsub(" {2,}", " ", sit)
  # activities
  act <- str_replace_all(x, ".*@Activities: ", "")
  act <- str_trim(act, side = "both")
  act <- gsub(" {2,}", " ", act)
  # ids
  ids <- as.vector(unlist(strsplit(gsub("(@ID)", "~~~\\1", x), "~~~")))
  ids <- as.vector(unlist(ids[2:length(ids)]))
  ids <- str_replace_all(ids, "@ID: ", "")
  ids <- str_replace_all(ids, "@.*", "")
  ids <- str_replace_all(ids, "\\|{2,}", "|")
  ids <- str_replace_all(ids, "eng\\|HSLLD\\|", "")
  ids <- gsub("\\|$", "", ids, perl = T)
  # agechild
  age <- str_replace_all(ids, "CHI\\|", "")
  age <- str_replace_all(age, "\\|.*", "")
  age <- str_replace_all(age, "[A-Z]{2,}[0-9]{0,2}", "NA")
  # genderchild
  sex <- str_replace_all(ids, ".*female", "female")
  sex <- str_replace_all(sex, ".*\\|male", "male")
  sex <- str_replace_all(sex, "male.*", "male")
  sex<- as.vector(unlist(sapply(sex, function(x){
    x <- ifelse(nchar(x) > 8, "NA", x)})))
  # ids 2
  ids <- str_replace_all(ids, "[0-9]{1,2};[0-9]{1,2}.{0,1}[0-9]{0,2}\\|{0,1}[a-z]{0,6}\\|", "")
  ids <- str_trim(ids, side = "both")
  ids <- str_replace_all(ids, "\\|$", "")
  # spk
  spk <- str_replace_all(ids, "\\|.*", "")
  # create a table of results
  vec <- c(file, pid, nam, lan, par, bir, com, dat, loc, sit, act, x)
  cntnt <- c(rep(vec, rps))
  df <- matrix(cntnt, ncol= 12, byrow = T)
  mt <- cbind(ids, spk, age, sex, df)
  colnames(mt) <- c("Id", "Speaker", "Age", "Gender", "File", "pid", "Name", 
                    "Language", "Participants", "DateOfBirth", "Comment", 
                    "DateOfRecording", "Location", "Situation", "Actvities", "all")
  return(mt)
})
# collapse all tables ampl1ameo 1 table
biotb <- do.call("rbind", bio)
biotb <- as.data.frame(biotb)
# inspect data
#head(biotb)

# save data to disc
write.table(biotb, file = "datatables/biotb.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE, quote = F)
###############################################################
# extract wordcounts
# count words for each speaker
wc <- sapply(cha, function(x) {
  file <- gsub("D:\\Uni\\Korpora\\Original\\CHILDES\\Eng-NA-MOR\\HSLLD/", "", x, fixed = T)
  file <- gsub(".cha", "", file, fixed = T)
  x <- scan(x, what = "char", sep = "\t", quiet = T, quote = "", skipNul = T)
  x <- gsub("\n", " ", x, fixed = T)
  x <- gsub("\t", " ", x, fixed = T)
  x <- str_trim(x, side = "both")
  x <- gsub(" {2,}", " ", x)
  orig <- paste(x, collapse = " ", sep = " ")
  full <- strsplit(gsub("([*]{1,1}[A-Z]{2,4}[0-9]{0,1}:)", "~~~\\1", orig), "~~~")
  full2 <- unlist(full)
  meta <- full2[1]
  full2 <- full2[2:length(full2)]
  x <- strsplit(gsub("([%|*][a-z|A-Z]{2,4}[0-9]{0,1}:)", "~~~\\1", orig), "~~~")
  txt <- sapply(x, function(y){
    y <- str_replace_all(y, "%.*", "")
    y <- str_replace_all(y, "@.*", "")
    y <- str_replace_all(y, "\\.|\\?|;|,|\\[|\\]|!|\\+|\\(|\\)|<|>|[0]", "")
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)
    y <- y[y != ""] })
  x <- str_split(txt, ":")
  spk <- sapply(x, function(y){
    y <- y[1] })
  file <- as.vector(unlist(rep(file, length(spk))))
  wrds <- sapply(x, function(y){
    y <- y[2]
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)
  })
  cnt <- sapply(x, function(y){
    y <- y[2]
    y <- str_trim(y, side = "both")
    y <- gsub(" {2,}", " ", y)
    y <- str_split(y, " ")
    y <- sapply(y, function(z){ z <- ifelse(z == "", 0, length(z)) })
    y <- unique(y)
  })
  spk <- gsub("*", "", spk, fixed = T)
  df <- matrix(data = c(file, full2, txt, spk, wrds, cnt, rep(meta, length(txt))), ncol = 7, byrow = F)
})
# collapse all tables ampl1ameo 1 table
wc <- do.call("rbind", wc)
wc <- as.data.frame(wc)
colnames(wc) <- c("File", "FullUtterance", "SpeakerPlusAtterance", "Speaker", 
                  "Utterance", "WordCount", "Meta")
# clean colnames
colnames(wc) <- gsub("X.", "", colnames(wc), fixed = T)
colnames(wc) <- gsub(".", "", colnames(wc), fixed = T)
# clean read data
wc$Utterance <- gsub("\"", "", wc$Utterance, fixed = T)
# clean data
wc$Utterance <- as.vector(unlist(sapply(wc$Utterance, function(x){
  x <- gsub("/", "", x, fixed = T)
  x <- gsub("=", "", x, fixed = T)
  x <- gsub("&", "", x, fixed = T)
  x <- gsub("*", "", x, fixed = T)
  x <- gsub("^", "", x, fixed = T)
  x <- gsub("_", "", x, fixed = T)
  x <- gsub("?", "", x, fixed = T) } )))
# remove empty lines
wc <- wc[wc$Utterance != "",]
###############################################################
# WARNING: up to here: wordcount per utterance
# load package
library(dplyr)
# extract wordcount per speaker
wcps <- wc %>%
  dplyr::select(File, Speaker, WordCount) %>%
  dplyr::group_by(File, Speaker) %>%
  dplyr::summarise(WordCount = sum(as.numeric(WordCount)))
# inspect data
head(wcps)

# save data to disc
write.table(wcps, file = "datatables/wordcount.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE, quote = F)
# join bio and wc
bio <- left_join(biotb, wcps)
# save data to disc
write.table(bio, file = "datatables/bio.txt", sep = "\t", 
            row.names = FALSE, col.names = TRUE, quote = F)
###############################################################
#                       END PART 1
###############################################################

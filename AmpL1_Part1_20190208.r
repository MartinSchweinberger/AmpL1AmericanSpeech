##################################################################
# Titel:      On the L1-Acquisition of Amplification in English - Part 1
# R version:  3.5.1 (2018-07-02) -- "Feather Spray"
# Autor:      Martin Schweinberger
# Date:       2019-02-08
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
setwd("D:\\Uni\\Projekte\\02-Intensification\\Amp1L")   # set wd
library(stringr)                                        # load packages
options(stringsAsFactors = F)                           # set options
options(scipen = 999)                                   # set options
imageDirectory<-"images"                                # define image directory
corpus <- "D:\\Uni\\Korpora\\Original\\CHILDES\\Eng-NA-MOR\\HSLLD"
bio.path <- "D:\\Uni\\Projekte\\02-Intensification\\07IntensifierAcquisition\\IntAcqJCLAD/biowc.txt"
###############################################################
###                   START
###############################################################
# Prepare for loading corpus
# Choose the files
cha = list.files(path = corpus, pattern = ".cha$", all.files = T,
  full.names = T, recursive = T, ignore.case = T, include.dirs = T)
###############################################################
# extract biodata
#cha <- cha[1:5]
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
  colnames(mt) <- c("id", "spk", "age", "gender", "file", "pid", "name", "language", "participants", "birth", "comment", "date", "location", "situation", "actvities", "all")
  return(mt)
  })
# collapse all tables into 1 table
biotb <- do.call("rbind", bio)
biotb <- as.data.frame(biotb)
# inspect data
head(biotb)

###############################################################
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
# collapse all tables into 1 table
wc <- do.call("rbind", wc)
wc <- as.data.frame(wc)
colnames(wc) <- c("file", "full", "spkutt", "spk", "utt", "wc", "meta")
# inspect data
head(wc)

###############################################################
# save data to disc
write.table(wc, file = "wc.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = F)
#wc <- read.table("wc.txt", sep = "\t", header = T, quote = "", comment.char = "")
###############################################################
# clean colnames
colnames(wc) <- gsub("X.", "", colnames(wc), fixed = T)
colnames(wc) <- gsub(".", "", colnames(wc), fixed = T)
# clean read data
wc$utt <- gsub("\"", "", wc$utt, fixed = T)
# clean data
wc$utt <- as.vector(unlist(sapply(wc$utt, function(x){
  x <- gsub("/", "", x, fixed = T)
  x <- gsub("=", "", x, fixed = T)
  x <- gsub("&", "", x, fixed = T)
  x <- gsub("*", "", x, fixed = T)
  x <- gsub("^", "", x, fixed = T)
  x <- gsub("_", "", x, fixed = T)
  x <- gsub("?", "", x, fixed = T) } )))
# inspect data
head(wc)

# remove empty lines
wc <- wc[wc$utt != "",]
###############################################################
# split data into smaller chunks
pos01 <- wc$utt[1:50000]
pos02 <- wc$utt[50001:100000]
pos03 <- wc$utt[100001:150000]
pos04 <- wc$utt[150001:200000]
pos05 <- wc$utt[200001:250000]
pos06 <- wc$utt[250001:300000]
pos07 <- wc$utt[300001:nrow(wc)]
# load functions and activate libraries
source("D:\\R/POStagObject.R")
library(NLP)
library(openNLP)
library(openNLPmodels.en)
# pos tagging data
hslldpos01 <- POStag(object = pos01)
hslldpos01 <- as.vector(unlist(hslldpos01))
writeLines(hslldpos01, con = "hslldpos01.txt", sep = "\n", useBytes = FALSE)
###
hslldpos02 <- POStag(object = pos02)
hslldpos02 <- as.vector(unlist(hslldpos02))
writeLines(hslldpos02, con = "hslldpos02.txt", sep = "\n", useBytes = FALSE)
###
hslldpos03 <- POStag(object = pos03)
hslldpos03 <- as.vector(unlist(hslldpos03))
writeLines(hslldpos03, con = "hslldpos03.txt", sep = "\n", useBytes = FALSE)
###
hslldpos04 <- POStag(object = pos04)
hslldpos04 <- as.vector(unlist(hslldpos04))
writeLines(hslldpos04, con = "hslldpos04.txt", sep = "\n", useBytes = FALSE)
###
hslldpos05 <- POStag(object = pos05)
hslldpos05 <- as.vector(unlist(hslldpos05))
writeLines(hslldpos05, con = "hslldpos05.txt", sep = "\n", useBytes = FALSE)
###
hslldpos06 <- POStag(object = pos06)
hslldpos06 <- as.vector(unlist(hslldpos06))
writeLines(hslldpos06, con = "hslldpos06.txt", sep = "\n", useBytes = FALSE)
###
hslldpos07 <- POStag(object = pos07)
hslldpos07 <- as.vector(unlist(hslldpos07))
writeLines(hslldpos07, con = "hslldpos07.txt", sep = "\n", useBytes = FALSE)
###############################################################
# list pos tagged elements
postag.files = c("hslldpos01.txt", "hslldpos02.txt", "hslldpos03.txt", "hslldpos04.txt",
                 "hslldpos05.txt", "hslldpos06.txt", "hslldpos07.txt")
# load pos tagged elements
hslldpos <- as.vector(unlist(sapply(postag.files, function(x) {
  x <- scan(x, what = "char", sep = "\n", quote = "", quiet = T, skipNul = T)
  x <- gsub(" {2,}", " ", x)
  x <- str_trim(x, side = "both")
  x <- str_replace_all(x, fixed("\n"), " ")
  } )))
# unlist pos tagged elements and add to data frame
wc$hslldpos <- hslldpos
# clean data frame
wc4 <- apply(wc, 2, function(x){
  x <- str_replace_all(x, fixed("\""), "") } )
# convert into data frame
df5 <- as.data.frame(wc4)
###############################################################
# save data after pos tagging
write.table(df5, "hslld_postagged.txt", sep = "\t", row.names = F, col.names = T)
# read data
#df5 <- read.table("hslld_postagged.txt", sep = "\t", header = T, quote = "", comment.char = "")
###############################################################
# clean data frame
df5 <- apply(df5, 2, function(x){
  x <- str_replace_all(x, fixed("\""), "") } )
# convert into data frame
df5 <- as.data.frame(df5)
# add id column
df5$id <- 1:nrow(df5)
# clean colnames
colnames(df5) <- c("fl", "cntnt", "spkutt", "spk", "utt", "wc", "meta", "pos", "id")
# clean postagged utts
df5$pos <- as.vector(unlist(sapply(df5$pos, function(x){
  x <- gsub("/''", "", x, fixed = T)
  x <- gsub("/``", "", x, fixed = T)
  x <- str_trim(x, side = "both")
  x <- gsub(" {2,}", " ", x)} )))
###############################################################
# save data after post-pos-tagging clean up
write.table(df5, "hslld_postagged_2.txt", sep = "\t", row.names = F, col.names = T)
# read data
#df5 <- read.table("hslld_postagged_2.txt", sep = "\t", header = T, quote = "", comment.char = "")
###############################################################
# clean data frame
df5 <- apply(df5, 2, function(x){
  x <- str_replace_all(x, fixed("\""), "") } )
# convert into data frame
df5 <- as.data.frame(df5)
# clean colnames
colnames(df5) <- c("fl", "cntnt", "spkutt", "spk", "utt", "wc", "meta", "pos", "id")
# attach file subfile speaker id to pos-tagged string
df5$spksupos <- as.vector(unlist(apply(df5, 1, function(x){
  x <- paste(x[4], ": ", x[8], sep = "", collapse = "")
  } )))
# repair file names
df5$fl <- as.vector(unlist(sapply(df5$fl, function(x){
  x <- gsub("/", "-", x, fixed = T) } )))
# split file subfile speaker pos-tagged su by file subfile
hslld.tmp1 <- by(df5$spksupos, df5$fl, paste)
# rename data
hslld <- hslld.tmp1
# save result to disc
lapply(seq_along(hslld), function(i)writeLines(text = unlist(hslld[i]),
    con = paste("D:\\Uni\\Korpora\\Edited\\HSLLD-PosTagged/", names(hslld)[i],".txt", sep = "")))
###############################################################
# tabulate wc
wctb <- data.frame(wc$file, wc$spk, wc$wc)
# clean colnames
colnames(wctb) <- str_replace_all(colnames(wctb), "wc.", "")
# clean content
wctb$file <- gsub("\"", "", wctb$file, fixed = T)
wctb$spk <- gsub("\"", "", wctb$spk, fixed = T)
wctb$wc <- gsub("\"", "", wctb$wc, fixed = T)
# combine file and speaker
wctb$filespk <- as.vector(unlist(apply(wctb, 1, function(x){
  x <- paste(x[1], "$", x[2], collapse = "")
  x <- str_replace_all(x, " ", "") })))
wctb$wc <- gsub("\"", "", wctb$wc, fixed = T)
wctb$wc <- as.numeric(wctb$wc)
wctb2 <- ddply(wctb, "wctb$filespk", transform, wc=sum(wc))
wctb2 <- subset(wctb2, !duplicated(filespk))
# combine bio and wc
biowc <- join(biotb, wctb2, by = c("file", "spk"), type = "left")
# inspect data
head(biowc)

# update gender column
biowc$gender <- with(biowc, ifelse(biowc$spk =="MOT", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS1", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS2", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SIS3", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SI1", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SI2", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="SI3", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="WOM", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="ANT", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="AUN", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="GMA", "female", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="FAT", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="UNC", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BRO", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BRO1", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BRO2", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR1", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR2", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR3", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BR3", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="BAB", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="GFA", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="GPA", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="MAN", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="MAL", "male", biowc$gender))
biowc$gender <- with(biowc, ifelse(biowc$spk =="MAL2", "male", biowc$gender))
# create a column holding the socio economic status (ses)
biowc$ses <- biowc$comment
biowc$ses <- str_replace_all(biowc$ses, ".*SES", "")
biowc$ses <- str_replace_all(biowc$ses, ".*lower.*", "lower")
biowc$ses <- str_replace_all(biowc$ses, ".*middle.*", "middle")
biowc$ses <- as.vector(unlist(sapply(biowc$ses, function(x){
  x <- ifelse(x == "lower", "lower",
    ifelse(x == "middle", "middle", NA)) })))
# save results to disc
write.table(biowc, file = "biowc.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
###############################################################
###                    SEARCH
###############################################################
# set parameters
pathname <- "D:\\Uni\\Korpora\\Edited\\HSLLD-PosTagged"
context <- 80
all.pre = T
# define search strings
search.pattern <-  c("[A-Z]{0,1}[a-z]{0,}-{0,1}[a-z]{2,}\\/JJ")
##########################################################
# start searches
int <- ConcR(pathname, search.pattern, context, all.pre = T)
# extract speaker
int$all.pre <- gsub(".* ([A-Z]{2,}_{0,1}[0-9]{0,1}:)", "\\1", int$all.pre)
int$all.pre <- str_replace_all(int$all.pre, ":.*", "")
# code status
int$status <- as.vector(unlist(sapply(int$post, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(" .*", "", x)
  x <- gsub(".*\\/", "", x)
  x <- gsub("NNS", "NN", x)
  x <- ifelse(x == "NN", "attr", "pred")
  } )))
# code previous element (pos)
int$prepos <- as.vector(unlist(sapply(int$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".*\\/", "", x)
  x <- gsub(" .*", "", x) } )))
# code previous element (lexeme)
int$prelex <- as.vector(unlist(sapply(int$pre, function(x){
  x <- str_trim(x, side = "both")
  x <- gsub(".* ", "", x)
  x <- gsub("\\/.*", "", x) } )))
# clean file
int$file <- as.vector(unlist(sapply(int$file, function(x){
  x <- gsub(".txt", "", x, fixed = T)
  x <- gsub("-", "/", x, fixed = T) } )))
##########################################################
###                 WARNING!
# extract all attributive adjectives
#int <- int[int$status == "attr",]
##########################################################
# inspect data
head(int)

# extract all potential intensifiers
JJ <- int[int$prepos == "JJ",]
RB <- int[int$prepos == "RB",]
# combine tabels
test <- rbind(JJ, RB)
# extract vector of potential intensifiers
pints <- rownames(table(test$prelex, test$prepos))
# show list of potential intensifiers
pints

# code intensifiers
int$absint <- as.vector(unlist(sapply(tolower(int$prelex), function(x){
  x <- ifelse(x == "awful", 1,
    ifelse(x == "awfully", 1,
    ifelse(x == "completely", 1,
    ifelse(x == "extra", 1,
    ifelse(x == "fucking", 1,
    ifelse(x == "pretty", 1,
    ifelse(x == "radically", 1,
    ifelse(x == "real", 1,
    ifelse(x == "really", 1,
    ifelse(x == "so", 1,
    ifelse(x == "terrible", 1,
    ifelse(x == "total", 1,
    ifelse(x == "totally", 1,
    ifelse(x == "very", 1,
    ifelse(x == "wonderfully", 1, 0))))))))))))))) } )))
# inspect all pos tags preceeding attr adjectives
#table(int$prepos)
# reorganize data frames
# colnames(int)
int <- data.frame(1:nrow(int), int$file, int$file, int$file, int$file, int$all.pre, int$pre, int$token, int$post,
  int$status, int$prepos, int$prelex, int$absint, int$token)
# adapt column names
colnames(int) <- c("id", "file", "visit", "styp", "chi", "spk", "pre", "token", "post", "status", "prepos",
  "prelex", "absint", "tokenlex")
# clean visit
int$visit <- tolower(gsub("\\/.*", "", int$visit))
# clean styp
int$styp <- tolower(gsub("HV[0-9]{1,1}\\/", "", int$styp))
int$styp <- gsub("\\/.*", "", int$styp)
# clean chi
int$chi <- tolower(gsub(".*\\/", "", int$chi))
int$chi <- gsub("[a-z]{2,2}[0-9]{1,1}", "", int$chi)
# clean tokenlex
int$tokenlex <- gsub("\\/.*", "", int$tokenlex)
int$tokenlex <- str_trim(int$tokenlex, side = "both")
# inspect data
head(int)

test <- int[int$absint == 1,]
p1 <- test$prelex
p2 <- test$tokenlex
p3 <- gsub("^ ", "", test$post)
p3 <- gsub(" .*", "", p3)
p3 <- gsub("/.*", "", p3)
test2 <- paste(p1, " ", p2, " ", p3, sep = " ")
table(test2)

nrow(int)

int <- int[int$tokenlex != "xxx",]
nrow(int)

int$rmv <- gsub("^ ", "", int$post)
int$rmv  <- gsub(" .*", "", int$rmv )
int$rmv  <- gsub("/.*", "", int$rmv )
int <- int[int$rmv != "xxx",]
nrow(int)

int <- int[int$rmv != "yyy",]
nrow(int)

colnames(int)
int <- int[, c(1:14)]
colnames(int)

# inspect data
head(int)

###############################################################
# save table to disc
write.table(int, file = "int.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
###############################################################
###                   END of PART 1
###############################################################


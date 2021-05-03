#install.packages(c("xlsx", "data.table", "installr", "stringr", "audio", "rPraat", "seewave", "stringr", "tuneR", "devtools", "ggplot2", "ggpubr", "dplyr"))

#library(installr)
#updateR()
library(devtools)
library(dplyr)
#install_github("dalejbarr/elan") #need to install Rtools on system
library(elan)
library(rPraat)
library(audio)
library(tuneR)
library(stringr)
library(seewave)
library(ggplot2)
library(ggpubr)
library(data.table)
library("xlsx")
library(stringi)

####Getting a total phone count from csv files########################################

#sets the name of the folder containing eaf files
languageFolder <- './rawinput'

allphones <- NULL
#create master vector of all characters
data_table_file <- "nsyixcn.csv"

#sets the name of the folder containing eaf files
languageFolder <- './rawinput'

#add folder to file name
data_table_path <- paste(languageFolder, data_table_file, sep = "/")

#load praat spreadsheet
### Encoding note, when loading and saving table, no not set to Unicode. Instead convert to unicode after it's loaded. If loaded and saved as unicode, and then reloaded, there are encoding errors
data_table <- read.csv(data_table_path, header=FALSE, encoding="UTF-8")
data_table$V1 <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", data_table$V1))
tempphones <- data_table$V1
  #splits annotations into characters
allphones <- tolower(unlist(strsplit(paste(tempphones, collapse = ' '), split = "")))
bad_characters <- c("\\.",  "/", "<", ">", "?", "&",  ":", "*", "!", " ", "")
for (bad_ch in bad_characters) {
  allphones <- gsub(bad_ch, "", allphones)
}




diacritics <- c("̓", "̌", "ʷ")
diacritics <- c("<U+0313>", "<U+030C>", "<U+02B7>")
allphones <- enc2native(allphones)
diacritics <- enc2native(diacritics)

for(i in 1:length(allphones)) {
  diacritics <- c("<U+0313>", "<U+030C>", "<U+02B7>")
  if (i>1) {
    #print(paste("phone: ", allphones[i], "previous phone: ", allphones[i-1]))
    if (allphones[i] %in% diacritics & !(allphones[i-1] %in% diacritics) & allphones[i+1] %in% diacritics & allphones[i+2] %in% diacritics) {
      previous_phone <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i-1]))
      phone <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i]))
      phone1 <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i+1]))
      phone2 <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i+2]))
      allphones[i-1] <- paste0(allphones[i-1], allphones[i], allphones[i+1], allphones[i+2])
      #print(paste("3 diacritics, ", "phone: ", phone, " previous_phone: ", previous_phone, " phone+1: ", phone1, " phone+2: ", phone2,  " new previous phone: ", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i-1]))))
    } else if (allphones[i] %in% diacritics & !(allphones[i-1] %in% diacritics) & allphones[i+1] %in% diacritics) {
      previous_phone <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i-1]))
      phone <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i]))
      phone1 <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i+1]))
      allphones[i-1] <- paste0(allphones[i-1], allphones[i], allphones[i+1])
      #print(paste("2 diacritics, ", "phone: ", phone, " previous_phone: ", previous_phone, " phone+1: ", phone1,  " new previous phone: ", stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones[i-1]))))
    } else if (allphones[i] %in% diacritics & !(allphones[i-1] %in% diacritics) & i > 1) {
      allphones[i-1] <- paste0(allphones[i-1], allphones[i])
      #print(paste("1 diacritics, new previous phone: ", allphones[i-1]))
    }
  }
}

allphones <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", allphones))
#final cleanup remove spaces and empty diacritic marks
allphones <- gsub(",", "", allphones)
allphones <- gsub("_", "", allphones)
allphones <- gsub(";", "", allphones)
allphones <- gsub("·", "", allphones)
allphones <- gsub("\\[", "", allphones)
allphones <- gsub("]", "", allphones)
allphones <- gsub("\\(", "", allphones)
allphones <- gsub("\\)", "", allphones)
allphones <- gsub("\"", "", allphones)
allphones <- gsub("\\\\", "", allphones)
allphones <- gsub("\\*", "", allphones)
allphones <- gsub("\\?", "", allphones)

allphones <- allphones[allphones != ""]
allphones <- allphones[allphones != "̓"]
allphones <- allphones[allphones != "̌"]
allphones <- allphones[allphones != "ʷ"]
unique(allphones)


df_allphones <- as.data.frame(table(allphones))

df_allphones <- setorder(df_allphones, -Freq)


ggplot(df_allphones, aes(x=reorder(allphones, -Freq) , y=Freq)) + 
  geom_bar(stat = "identity")
reorder(day, -perc)

#save dataframes as csv
#set directory to save to
output <- getwd() 

write.csv(df_allphones, paste(output, "allphones", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_vowels, paste(output, "vowels", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_consonants, paste(output, "consonants", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")

#generate plot


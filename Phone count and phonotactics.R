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

####Getting a total phone count for a directory of eaf files########################################

onom_word_list <- c("gaːgaːwoː",  "haː",  "kiːkiː",  "gãː",  "hoːloː",  "gə̃ː",  "hoguː",  "mo",  "seːseː",  "βuːwoː",  "ɛː",  "iʒː",  "kɛkɛkɛ",  "uː",  "waː",  "wo",  "ɸoːloː",  "βː",  "suːwaːlaː",  "suː",  "ɸoːloː",  "βː",  "gõːgõː",  "siːgɛ̃ĩ",  "gaːdaː",  "boː",  "buː",  "guːloː",  "wijaː",  "ɸuːtukuː", "saː",  "sõːwɛ̃ː")

#sets the name of the folder containing eaf files
languageFolder <- './rawinput'
#creates list of eaf files
fls <- list.files(languageFolder, full.names = T, pattern = '-1.eaf')
fls
allphones <- NULL
#create master vector of all characters
for(i in fls){
  #loads eaf file data into R
  eaf <- efileAnnotations(i)

  #selects transcription tiers
  eaf <- eaf[grep('Transcription', eaf$TIER_ID),]
  
  #subsets the dataframe to the annotations
  eaf_ann <- eaf$VALUE
  
  ##data cleanup
  data <- eaf_ann
  data <- gsub("\\:|\\;", "ː", data)
  data <- sub("\\(.*\\)", "", data)
  data <- gsub("<unintelligible>|<unclear>|<|>|-|--|->|\\?|\\.", "", data)
  data <- gsub("\\(.*", "", data)
  data <- data[!grepl('\\)|\n', data)]
  data <- gsub(' ː| :', 'ː', data)
  data <- gsub('ɪ ̆', 'ɪ̆', data)
  data <- gsub('o ̆', 'ŏ', data)
  data <- gsub('u ̆', 'ŭ', data)
  data <- gsub('i ̆', 'ĭ', data)
  data <- gsub('a ̆', 'ă', data)
  data <- gsub('e ̆', 'ĕ', data)
  data <- gsub('ə ̆', 'ə̆', data)
  data <- gsub('ɔ ̆', 'ɔ̆', data)
  data <- gsub('ɛ ̆', 'ɛ̆', data)
  data <- gsub('ɛ ́', 'ɛ́', data)
  data <- gsub('ɪ ́', 'ɪ́', data)
  data <- gsub('o ́', 'ó', data)
  data <- gsub('u ́', 'ú', data)
  data <- gsub('i ́', 'í', data)
  data <- gsub('a ́', 'á', data)
  data <- gsub('e ́', 'é', data)
  data <- gsub('ə ́', 'ə́', data)
  data <- gsub('ɔ ́', 'ɔ́', data)
  data <- gsub('ə ̃', 'ə̃', data)
  data <- gsub('ɪ ̃', 'ɪ̃', data)
  data <- gsub('o ̃', 'õ', data)
  data <- gsub('u ̃', 'ũ', data)
  data <- gsub('ɛ ̃', 'ɛ̃', data)
  data <- gsub('a ̃', 'ã', data)
  data <- gsub('e ̃', 'ẽ', data)
  data <- gsub('ɔ ̃', 'ɔ̃', data)
  data <- gsub('i ̃', 'ĩ', data)
  data <- gsub('ə̃ ̆', 'ə̃̆', data)
  data <- gsub('ɪ̃ ̆', 'ɪ̃̆', data)
  data <- gsub('ã ̆', 'ã̆', data)
  data <- gsub('õ ̆', 'õ̆', data)
  data <- gsub('ũ ̆', 'ũ̆', data)
  data <- gsub('ɛ̃ ̆', 'ɛ̃̆', data)
  data <- gsub('ẽ ̆', 'ẽ̆', data)
  data <- gsub('ɔ̃ ̆', 'ɔ̃̆', data)
  data <- gsub('ĩ ̆', 'ĩ̆', data)
  
  data <- gsub("ɾ", "l", data)
  data <- gsub("ʃ", "s", data)
  data <- gsub("f", "ɸ", data)
  data <- gsub("r", "l", data)
  data <- data[data!=" "]
  data <- data[data!=""]
  eaf_ann <- data
  
  #splits annotations into characters
  tempphones <- tolower(unlist(strsplit(paste(eaf_ann, collapse = ' '), split = "")))
  #fix colons for length
  tempphones <- sub(":", "ː", tempphones)
  
  #removes spaces
  tempphones <- tempphones[tempphones!=" "]
  
  #append characters from the file onto a running vector of all characters in all files
  allphones <- append(allphones, tempphones)
}
allphones <- enc2native(allphones)
#correct long vowels and nasal vowels
for(i in 1:length(allphones)) {
  diacritics <- c("<U+0306>", "´", "~", "<U+02D0>")
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
allphones <- allphones[allphones!=" "]
allphones <- allphones[allphones!="ː"]
allphones <- allphones[allphones!="ːː"]
allphones <- allphones[allphones!=" "]
allphones <- allphones[allphones!="ːːː"]
allphones <- allphones[allphones!="~"]
allphones <- allphones[allphones!="~ː"]
allphones <- allphones[allphones!="~̆"]
allphones <- allphones[allphones!="ː~"]
allphones <- allphones[allphones!="~̆̆"]
allphones <- allphones[allphones!="´"]
allphones <- gsub("ːːː", "ːː", allphones)

allphones <- allphones[allphones!="(" & allphones!=")" & allphones!="(" & allphones!= "̆" & allphones!= "\n" & allphones!= "̃ːː" & allphones!= "<" & allphones!= "-" & allphones!= "̆ː" & allphones!= ";" & allphones!="/" & allphones!= "?" & allphones!= "-ː" & allphones!="́ː"    ]

# merge and standardize vowel categories
allphones <- sub("ɔ", "o", allphones)
allphones <- sub("ɪː", "ɛː", allphones)
allphones <- sub("ɪ", "i", allphones)
allphones <- sub("e", "ɛ", allphones)

#create vowel subset
vowel_set <- c("aː", "ɛː", "ɛ", "i", "oː", "uː", "u", "o", "iː", "ɛːː", "aːː", "a", "ɛ~", "ə", "ə~", "ə~ː", "iːː", "oːː", "ɛ~ː", "ĭ", "ŏ", "ɛ̆", "ŭ", "ă", "i~ː", "a~ː", "o~ːː", "ĭː", "i~", "ə~ːː", "o~ː", "o~̆", "i~ːː", "a~̆", "ɛ̆ː", "uːː", "ɛ~ːː", "a~", "oː~", "o~", "əː", "i~̆", "ɛ´ː", "u~ː")
#create consonants subset
consonant_set <- c("s", "m", "l", "g", "j", "h", "d", "k", "n", "w", "b", "t", "ɸ", "v", "p", "ŋ", "sː", "mː", "mːː", "ß", "ŋːː", "ŋː", "x", "c", "r", "w~", "l̆", "ʒːː", "ʒ", "ʙ", "z", "ßːː", "ßː")

#create vowel subset
vowels <- allphones[allphones %in% vowel_set]
#create consonants subset
consonants <- allphones[allphones %in% consonant_set]

unique(allphones)
unique(vowels)
unique(consonants)

#generate frequency dataframes
df_allphones <- as.data.frame(table(allphones))
df_consonants <- as.data.frame(table(consonants))
df_vowels <- as.data.frame(table(vowels))

#order dataframes by frequency
df_allphones <- setorder(df_allphones, -Freq)
df_consonants <- setorder(df_consonants, -Freq)
df_vowels <- setorder(df_vowels, -Freq)

#add proportion column
add_proportion <- function(x) {
  x$proportion <- x$Freq/sum(x$Freq)
  return(x)
}
df_allphones <- add_proportion(df_allphones)
df_consonants <- add_proportion(df_consonants)
df_vowels <- add_proportion(df_vowels)


#save dataframes as csv
#set directory to save to
output <- getwd() 

write.csv(df_allphones, paste(output, "allphones", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_vowels, paste(output, "vowels", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_consonants, paste(output, "consonants", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")

#generate plot


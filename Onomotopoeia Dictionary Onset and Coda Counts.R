##generate a list of all onsets and codas, and all syllable types, for a dictionary of unique words
#install.packages(c("installr", "stringr", "audio", "rPraat", "seewave", "stringr", "tuneR", "devtools", "ggplot2", "ggpubr", "dplyr"))
#library(installr)
#updateR()
library(stringi)
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

####Getting a total onset and coda count for a directory of eaf files########################################

onom_word_list <- c("gaːgaːwoː",  "haː",  "kiːkiː",  "gãː",  "hoːloː",  "gə̃ː",  "hoguː",  "mo",  "seːseː",  "βuːwoː",  "ɛː",  "iʒː",  "kɛkɛkɛ",  "uː",  "waː",  "wo",  "ɸoːloː",  "βː",  "suːwaːlaː",  "suː",  "ɸoːloː",  "βː",  "gõːgõː",  "siːgɛ̃ĩ",  "gaːdaː",  "boː",  "buː",  "guːloː",  "wijaː",  "ɸuːtukuː", "saː",  "sõːwɛ̃ː")

#Create vowel set variable
vowelset <- c("a", "e", "i", "o", "u", "aː", "ɪː", "ɛ", "ɛː", "oː", "uː", "iː", "ɪ", "ɛːː", "aːː", "ɪːː", "ə", "iːː", "oːː", "eː", "ɔː", "ɔ", "e")
consonantset <- c("s", "m", "l", "g", "j", "h", "d", "k", "n", "w", "b", "t", "ɸ", "v", "ɾ", "ʃ", "p", "ŋ", "ʃː", "mː", "mːː", "β", "ŋːː", "ŋː", "x", "c", "r", "ʒːː", "ʒ", "ʙ", "ɡ", "z", "f", "βːː", "βː")
coda_consonantset <- c("n", "m", "ŋ", "l", "b", "ŋːː", "ŋː", "k", "s", "z")
odd_onsets <- c("bs", "nd","ŋw", "kw", "mj", "sk", "dw", "st", "bd", "ks", "zd", "cl")
#create master vector of all characters

allphones <- NULL
allonsets <-NULL
allcodas <- NULL
dict <- NULL

data <- onom_word_list
data <- gsub("\\:|\\;", "ː", data)

data <- sub("\\(.*\\)", "", data)
data <- gsub("<unintelligible>|<unclear>|<|>|-|--|->|\\?|\\.", "", data)
data <- gsub("\\(.*", "", data)
data <- data[!grepl('\\)|\n', data)]
data <- gsub(' ː| :', 'ː', data)
data <- gsub('ɪ ̆', 'ɪ', data)
data <- gsub('o ̆', 'o', data)
data <- gsub('u ̆', 'u', data)
data <- gsub('i ̆', 'i', data)
data <- gsub('a ̆', 'a', data)
data <- gsub('e ̆', 'e', data)
data <- gsub('ə ̆', 'ə', data)
data <- gsub('ɔ ̆', 'ɔ', data)
data <- gsub('ɛ ̆', 'ɛ', data)
data <- gsub('ɛ ́', 'ɛ', data)
data <- gsub('ɪ ́', 'ɪ', data)
data <- gsub('o ́', 'o', data)
data <- gsub('u ́', 'u', data)
data <- gsub('i ́', 'i', data)
data <- gsub('a ́', 'a', data)
data <- gsub('e ́', 'e', data)
data <- gsub('ə ́', 'ə', data)
data <- gsub('ɔ ́', 'ɔ', data)
data <- gsub('ə ̃', 'ə', data)
data <- gsub('ɪ ̃', 'ɪ', data)
data <- gsub('o ̃', 'o', data)
data <- gsub('u ̃', 'u', data)
data <- gsub('ɛ ̃', 'ɛ', data)
data <- gsub('a ̃', 'a', data)
data <- gsub('e ̃', 'e', data)
data <- gsub('ɔ ̃', 'ɔ', data)
data <- gsub('i ̃', 'i', data)
data <- gsub('ə̃ ̆', 'ə', data)
data <- gsub('ɪ̃ ̆', 'ɪ', data)
data <- gsub('ã ̆', 'a', data)
data <- gsub('õ ̆', 'o', data)
data <- gsub('ũ ̆', 'u', data)
data <- gsub('ɛ̃ ̆', 'ɛ', data)
data <- gsub('ẽ ̆', 'e', data)
data <- gsub('ɔ̃ ̆', 'ɔ', data)
data <- gsub('ĩ ̆', 'i', data)
data <- gsub('ĭ̆', 'i', data)
data <- gsub('ɛ̆̆', 'ɛ', data)
data <- gsub("ə̃", "ə",data)
data <- gsub("ŭ", "u", data)
data <- gsub("ĭ", "i", data)
data <- gsub("ɛ̆", "ɛ", data)
data <- gsub("ă", "a", data)
data <- gsub("ĭ", "i", data)
data <- gsub("ŏ", "o", data)

data <- gsub("ɾ", "l", data)
data <- gsub("ʃ", "s", data)
data <- gsub("f", "ɸ", data)
data <- gsub("r", "l", data)

data <- enc2native(data)
data <- gsub("~", "", data)
data <- gsub("<U\\+0306>", "", data)
data <- gsub("~", "", data)
data <- gsub("~", "", data)
#converts back to unicode and prints
#data <- cat(stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", data)))
#converts back to unicode and doesn't print
data <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", data))
onom_word_list <- data

#splits annotations into words
tempwords <- tolower(unlist(strsplit(paste(onom_word_list, collapse = ' '), split = " ")))
#remove blank annotations
tempwords <- tempwords[tempwords!=""]

#standardize vowels, remove length, then add words to dictionary
new_words <- tempwords
new_words <- gsub("e", "ɛ", new_words)
new_words <- gsub("e", "ɛ", new_words)   
new_words <- gsub("ɪː", "ɛ", new_words)
new_words <- gsub("ɪ", "i", new_words)
new_words <- gsub("ɔ", "o", new_words)
new_words <- gsub("e", "ɛ", new_words)
new_words <- gsub("ː", "", new_words)
new_words <- gsub("f", "ɸ", new_words)
new_words <- gsub("unclɛar", "", new_words)
new_words  <- enc2native(new_words)
new_words <- gsub("<U+0306>", "", new_words)
new_words <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", new_words))
new_words <- unique(new_words)
#print(paste(file, "word count: ", length(new_words)))
dict <- append(new_words, dict)
dict <- unique(dict)
#remove blank entries
dict <- dict[dict!=""]

#split each word into characters, parse into codas and onsets, and save lists or all codas and all onsets
for(w in dict) {
  #testword
  #w <- "tlaːtamvan"
  #split word into characters
  wphone <- unlist(strsplit(w, split = ""))
  #recombine vowels with following length symbol
  for(i in 1:length(wphone)) {
    if (i>1&i<=length(wphone)){
      if ((wphone[i] == "ː" & wphone[i-1] %in% vowelset)){
        wphone[i-1] <- paste0(wphone[i-1], "ː")
        ##remove any lone length symbols
        wphone <- wphone[wphone!="ː"]
      }
    }
  }
  tempwcoda <- NULL
  fullwcoda <- NULL
  tempwonset <- NULL
  fullwonset <- NULL
  codalist <- NULL
  onsetlist <- NULL
  for(i in 1:length(wphone)) {
#test printing
    #i<-6
    #print("wphone")
    #print(wphone[i])
    #if it's the final phone and a consonant, add it as a coda, this will cause problems for complex final codas
    if(wphone[i] %in% consonantset & i==length(wphone)) {
      tempwcoda <- wphone[i]
      codalist <- append(codalist, tempwcoda)
#test printing
      #print("finalcoda")
      #print(tempwcoda)
      #if it's not the initial or final phone
    } else if (i>1 & i < length(wphone)) {
      #if it's after a vowel and before a consonant and is an eligible coda consonant, add as a coda
      if(wphone[i-1] %in% vowelset & !(wphone[i+1] %in% vowelset) & wphone[i] %in% coda_consonantset) {
        tempwcoda <- append(tempwcoda, wphone[i])
        #test printing
        #print("nonfinal coda")
        #print(tempwcoda)
        #if the phone is a non-final consonant that is not a valid coda consonant, add it to the tempwonset
      } else if (wphone[i] %in% consonantset){
        tempwonset <- append(tempwonset, wphone[i])
        #test printing
        #print("tempwonset")
        #print(tempwonset)
        #if it's a vowel and there is an onset before it, save the onset phones to onsetlist as a single string, and reset tempwonset to NULL
      } else if (!(wphone[i] %in% consonantset | is.null(tempwonset))) {
        #merge tempwonset vector to a single string 
        fullwonset <- paste0(tempwonset, sep = "", collapse = "")
        #TEST PRINTING
        #print("fullwonset")
        #print(fullwonset)
        #print("vowel")
        #print(wphone[i])
        #testing for odd onset clusters, if onset is more than one phone, print the word
        if (fullwonset %in% odd_onsets) {
          print("word with odd onset")
          print(w)
        }
        #save tempwonset to onset list
        onsetlist <- append(onsetlist, fullwonset)
        codalist <- append(codalist, tempwcoda)
        #testing printing
        #print("fullonset")
        #print(fullwonset)
        tempwonset <- NULL
        fullwonset <- NULL
        tempwcoda <- NULL
      }
      #if it is an initial consonant, add it to the tempwonset
    } else if (i==1 & wphone[i] %in% consonantset) {
      tempwonset <- append(tempwonset, wphone[i])
      #test printing
      #print("tempwonset")
      #print(tempwonset)
    }
  }
  allcodas <- append(allcodas, codalist)
  allonsets <- append(allonsets, onsetlist)
}

#clean up codas and onsets
allonsets <- gsub("dj", "j", allonsets)
allonsets <- gsub("bd", "b", allonsets)
allonsets <- gsub("nd", "d", allonsets)
allonsets <- gsub("ŋg", "g", allonsets)
allcodas <- gsub("ʃ", "s", allcodas)

unique(allcodas)
unique(allonsets)

#create dataframe of frequencies of codas and onsets and save csv
coda_df <- as.data.frame(table(allcodas))
onset_df <- as.data.frame(table(allonsets))

coda_df$proportion <- coda_df$Freq/sum(coda_df$Freq)
onset_df$proportion <- onset_df$Freq/sum(onset_df$Freq)

output <- getwd() 

write.csv(coda_df, paste(output, "onomatopoeia_codas", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(onset_df, paste(output, "onomatopoeia_onsets", sep="/"), row.names = FALSE, fileEncoding = "UTF-8")

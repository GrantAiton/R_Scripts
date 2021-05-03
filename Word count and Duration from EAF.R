#install.packages(c("installr", "stringr", "audio", "rPraat", "seewave", "stringr", "tuneR", "devtools"))

#library(installr)
#updateR()
library(devtools)
#install_github("dalejbarr/elan") #need to install Rtools on system
library(elan)
library(rPraat)
library(audio)
library(tuneR)
library(stringr)
library(seewave)


####Getting a total word count for a directory of eaf files########################################

#sets the name of the folder containing eaf files
languageFolder <- './rawinput'

#creates list of eaf files
fls <- list.files(languageFolder, full.names = T, pattern = 'eaf')

#creates empty variable for a vector of all words
allwords <- NULL

#appends all annotations from each file into a single vector
for(i in fls){
  #loads eaf file data into R
  eaf <- efileAnnotations(i)
  #selects transcription tiers
  eaf <- eaf[grep('Transcription', eaf$TIER_ID),]
  
  #subsets the dataframe to the annotations
  eaf_ann <- eaf$VALUE
  #splits annotations into words
  tempwords <- tolower(unlist(strsplit(paste(eaf_ann, collapse = ' '), ' ')))
  #append words onto a running vector of all words in all files
  print(length(tempwords))
  allwords <- append(allwords, tempwords)
}
length(tempwords)
length(allwords)


####getting a total duration for a directory of eaf files###########################################

#sets the name of the folder containing eaf files
languageFolder <- './rawinput'

#creates list of eaf files
fls <- list.files(languageFolder, full.names = T, pattern = 'eaf')

##subsets only files with -1 in the file name
#fls <- fls[grep('-1', fls)]

#creates empty variable for total duration
annotation_length <- 0

for(i in fls){
  eaf <- efileAnnotations(i)
  #change times to secs
  eaf$startTime <- eaf$t0 / 1000
  eaf$endTime <- eaf$t1 / 1000
  #selects transcription tiers
  eaf <- eaf[grep('Transcription', eaf$TIER_ID),]
  for(i in 1:nrow(eaf)){
    annotation_length <- annotation_length+(eaf$endTime[i]-eaf$startTime[i])
  }
  print(annotation_length)
}
#Convert lenght to hours and print
annotation_length/60/60


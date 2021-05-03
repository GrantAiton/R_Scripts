install.packages(c("installr", "stringr", "audio", "rPraat", "seewave", "stringr", "tuneR", "devtools"))

#library(installr)
#updateR()
library(devtools)
install_github("dalejbarr/elan") #need to install Rtools on system
library(elan)
library(rPraat)
library(audio)
library(tuneR)
library(stringr)
library(seewave)

#sets the name of the language to set files for
languageFolder <- './rawinput'
fls <- list.files(languageFolder, full.names = T, pattern = 'eaf')

dict <- NULL

transcriptionDuration <- NULL

#iterates fro each individual file
for(i in fls){

  dfi <- efileAnnotations(i)

  dfi <- dfi[grep('Transcription', dfi$TIER_ID),]
  dfi <- dfi[!grepl('GWA', dfi$TIER_ID),]
  
  dfi$VALUE <- gsub('<|>|-|\\(|\\)', '', dfi$VALUE)
  
  for(filei in sort(unique(dfi$TIER_ID))){
    df <- dfi[dfi$TIER_ID == filei,]
    #TextGrid names
    tmpTGnames <-gsub('elan', 'tgs', i)
    tmpTGnames <- gsub('eaf', 'TextGrid',tmpTGnames)
    tmpTGnames <- gsub('\\.TextGrid', paste0('_', gsub('_.*', '', filei), '\\.TextGrid'), tmpTGnames)

    #WAV names
    tmpWAVnames <-gsub('elan', 'audios', i)
    tmpWAVnames <-gsub('eaf', 'wav', tmpWAVnames)
    
    tmpWAVnamesSave <-gsub('\\.wav', paste0('_', gsub('_.*', '', filei), '\\.wav'), tmpWAVnames)
    tmpWAVnamesSave <-gsub('audios', 'wavs', tmpWAVnamesSave)
    
    df <- df[df$VALUE != '',]
    
    #change times to secs
    df$startTime <- df$t0 / 1000
    df$endTime <- df$t1 / 1000
    
    #all transcription to lower case
    tmp_dict = tolower(unlist(strsplit(paste(df$VALUE, collapse = ' '), ' ')))
    #gets rid of empty entries
    tmp_dict = tmp_dict[tmp_dict != '']
    
    #adds the words to the dictionary
    dict = append(dict, tmp_dict)
    
    #calculate transcription duration
    #transcriptionDuration <- append(transcriptionDuration, sum(df$endTime - df$startTime))
    transcriptionDuration <- sum(df$endTime - df$startTime)

    #Creates the TEXTGRID............................................
    #get audio file duration
    #tmpWAVnames <- gsub('-1|-2|-3|-4|-5', '', tmpWAVnames)

    s1 <-readWave(tmpWAVnames)
    sound_length <- round(length(s1@left) / s1@samp.rate, 4)
    
    savewav(s1, filename = tmpWAVnamesSave)
    
    #create TG from time information
    tg <- tg.createNewTextGrid(0, sound_length)
    #add tier
    tg <- tg.insertNewIntervalTier(tg, 1, "transcription")
    
    #adds intervals to tiers
    for(k in 1:nrow(df)){
      tg <- tg.insertInterval(tg, "transcription", df$startTime[k], df$endTime[k], df$VALUE[k])
    }
    
    #saves the TextGrid
    tg.write(tg = tg, fileNameTextGrid = tmpTGnames)
  }

}

#speaker gender:

# ailrt0301-1_WK.Pitch m
# ailot0317-1_DDD.Pitch f
# ailot0319-1_TBU.Pitch m 
# ailot0311-1_LK.Pitch f
# ailot0313-1_DDD.Pitch f
# ailot0315-1_DDD.Pitch f
# ailot0303-1_WK.Pitch m
# ailot0305-1_WK.Pitch m 
# ailot0307-1_SSU.Pitch m
# ailot0309-1_MK.Pitch f
# ailot0293-1_SH.Pitch m
# ailot0295-1_WK.Pitch m
# ailot0297-1_WK.Pitch m
# ailos0284-1_MK.Pitch m
# ailot0285-1_WK.Pitch m
# ailot0287-1_WK.Pitch 
# ailot0289-1_SH.Pitch m
# ailot0291-1_SH.Pitch 
# ailos0279-1_WK.Pitch
# ailos0280-1_WK.Pitch
# ailos0281-1_WK.Pitch
# ailoa0505-1_JS.Pitch m
# ailoa0505-1_LK.Pitch f 
# ailoa0507-1_ES.Pitch m
# ailoa0509-1_ES.Pitch m
#ailoa0505-1_ASU.Pitch f

dict <- gsub("\\:|\\;", "ː", dict)

dict <- sub("\\(.*\\)", "", dict)
dict <- gsub("<unintelligible>|<unclear>|<|>|-|--|->|\\?|\\.", "", dict)
dict <- gsub("\\(.*", "", dict)
dict <- dict[!grepl('\\)|\n', dict)]

write.csv(data.frame(dict), 'dict.csv', row.names = F)

#dict <- read.csv('dict.csv')
#dict <- as.character(dict$dict)

#Creates the dictionary from all transcriptions
unique_dict = sort(unique(dict))

write.csv(data.frame(unique_dict), 'unique_dict.csv', row.names = F)

#empty variable to estore the phonemic representation of the words
entries = NULL

#Creates the phonemic representations
for(ii in 1:length(unique_dict)){
  tmpE =unique_dict[ii]
  
  if(is.na(as.numeric(tmpE))){
    tmpE = paste(unlist(strsplit(tmpE, '')), collapse = ' ')
  }
  
  entries = append(entries, tmpE)
  
}

#stores entries and phonemes as a dataframe
pronDict <- data.frame(unique_dict, entries)

#cleaning of entries and pronunciation dictionary
pronDict$entries <- gsub(' ː| :', 'ː', pronDict$entries)

pronDict$entries <- gsub('ɪ ̆', 'ɪ̆', pronDict$entries)
pronDict$entries <- gsub('o ̆', 'ŏ', pronDict$entries)
pronDict$entries <- gsub('u ̆', 'ŭ', pronDict$entries)
pronDict$entries <- gsub('i ̆', 'ĭ', pronDict$entries)
pronDict$entries <- gsub('a ̆', 'ă', pronDict$entries)
pronDict$entries <- gsub('e ̆', 'ĕ', pronDict$entries)
pronDict$entries <- gsub('ə ̆', 'ə̆', pronDict$entries)
pronDict$entries <- gsub('ɔ ̆', 'ɔ̆', pronDict$entries)
pronDict$entries <- gsub('ɛ ̆', 'ɛ̆', pronDict$entries)

pronDict$entries <- gsub('ɛ ́', 'ɛ́', pronDict$entries)
pronDict$entries <- gsub('ɪ ́', 'ɪ́', pronDict$entries)
pronDict$entries <- gsub('o ́', 'ó', pronDict$entries)
pronDict$entries <- gsub('u ́', 'ú', pronDict$entries)
pronDict$entries <- gsub('i ́', 'í', pronDict$entries)
pronDict$entries <- gsub('a ́', 'á', pronDict$entries)
pronDict$entries <- gsub('e ́', 'é', pronDict$entries)
pronDict$entries <- gsub('ə ́', 'ə́', pronDict$entries)
pronDict$entries <- gsub('ɔ ́', 'ɔ́', pronDict$entries)

pronDict$entries <- gsub('ə ̃', 'ə̃', pronDict$entries)
pronDict$entries <- gsub('ɪ ̃', 'ɪ̃', pronDict$entries)
pronDict$entries <- gsub('o ̃', 'õ', pronDict$entries)
pronDict$entries <- gsub('u ̃', 'ũ', pronDict$entries)
pronDict$entries <- gsub('ɛ ̃', 'ɛ̃', pronDict$entries)
pronDict$entries <- gsub('a ̃', 'ã', pronDict$entries)
pronDict$entries <- gsub('e ̃', 'ẽ', pronDict$entries)
pronDict$entries <- gsub('ɔ ̃', 'ɔ̃', pronDict$entries)
pronDict$entries <- gsub('i ̃', 'ĩ', pronDict$entries)

pronDict$entries <- gsub('ə̃ ̆', 'ə̃̆', pronDict$entries)
pronDict$entries <- gsub('ɪ̃ ̆', 'ɪ̃̆', pronDict$entries)
pronDict$entries <- gsub('ã ̆', 'ã̆', pronDict$entries)
pronDict$entries <- gsub('õ ̆', 'õ̆', pronDict$entries)
pronDict$entries <- gsub('ũ ̆', 'ũ̆', pronDict$entries)
pronDict$entries <- gsub('ɛ̃ ̆', 'ɛ̃̆', pronDict$entries)
pronDict$entries <- gsub('ẽ ̆', 'ẽ̆', pronDict$entries)
pronDict$entries <- gsub('ɔ̃ ̆', 'ɔ̃̆', pronDict$entries)
pronDict$entries <- gsub('ĩ ̆', 'ĩ̆', pronDict$entries)

#saves the dictionary as a Tab-Separated file
write.table(pronDict, file = 'eibela_asis.txt', sep = '\t', quote = F, row.names = F, col.names = F)

uniqueEntries <- unique(unlist(strsplit(as.character(pronDict$entries), ' ')))









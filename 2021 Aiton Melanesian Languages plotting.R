####set data and libraries

library(stringi)
#library(devtools)
library(dplyr)
#install_github("dalejbarr/elan") #need to install Rtools on system
library(elan)
#library(rPraat)
#library(audio)
#library(tuneR)
library(stringr)
#library(seewave)
library(ggplot2)
#library(ggpubr)
source("get_chain_lengths get_chain_values.R")
source("get_ref_distance.R")

praat_table_file <- "2021.04.18 praat_table_updated_fourthpass.csv"

#sets the name of the folder containing eaf files
languageFolder <- './rawinput'

fls <- list.files(languageFolder, full.names = F, pattern = 'eaf')
full_fls <- list.files(languageFolder, full.names = T, pattern = 'eaf')
MC_files <- full_fls[grepl("-3", full_fls)]
full_fls <- full_fls[grepl("-2", full_fls)]

#add folder to file name
praat_table_path <- paste(languageFolder, praat_table_file, sep = "/")

#load praat spreadsheet
### Encoding note, when loading and saving table, no not set to Unicode. Instead convert to unicode after it's loaded. If loaded and saved as unicode, and then reloaded, there are encoding errors
praat_table <- read.csv(praat_table_path, header=TRUE, encoding="")

#####for each MC_word_tier_ID in the praat table, get the graid, refind, isnref and distance#####
#get_MC_ref_distance <- function(index, MC_eaf, MC_word_tier_ID_as_index = TRUE)

value_list <- NULL
distance_list <- NULL
graid_list <- NULL
refind_list <- NULL
isnref_list <- NULL
for(file in MC_files) {
  MC_eaf <- efileAnnotations(file)
  #set corpus value
  for (corpus in unique(praat_table$corpus)) {
    if (grepl(corpus, file)) {
      corpus_name <- corpus
    }
  }
  
  all_file_MC_word_IDs <- praat_table$MC_word_tier_ID[praat_table$corpus==corpus_name]
  valid_MC_word_IDs <- unique(all_file_MC_word_IDs[grepl("a", all_file_MC_word_IDs)])
  for(MC_word_ID in valid_MC_word_IDs) {
    #MC_word_ID <- valid_MC_word_IDs[1]
    #MC_word_ID <- "a4000"
    graid_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="graid" & MC_eaf$ANNOTATION_REF==MC_word_ID]
    # #testing
    MC_eaf$VALUE[MC_eaf$TIER_ID=="grammatical_word" & MC_eaf$ANNOTATION_ID==MC_word_ID]
    
    refind_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="refind" & MC_eaf$ANNOTATION_REF==graid_ANN_ID]
    
    isnref_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="isnref" & MC_eaf$ANNOTATION_REF==refind_ANN_ID]
    previous_value_length <- length(value_list)
    previous_distance_length <- length(distance_list)
    previous_graid_length <- length(graid_list)
    previous_refind_length <- length(refind_list)
    previous_isnref_length <- length(isnref_list)
    previous_lengths <- c(previous_value_length, previous_distance_length, previous_graid_length, previous_refind_length, previous_isnref_length)
    value_list <- append(value_list, MC_eaf$VALUE[MC_eaf$TIER_ID=="grammatical_word" & MC_eaf$ANNOTATION_ID==MC_word_ID])
    distance_list <- append(distance_list, get_MC_ref_distance(MC_word_ID, MC_eaf, MC_word_tier_ID_as_index = TRUE))
    graid_list <- append(graid_list, MC_eaf$VALUE[MC_eaf$TIER_ID=="graid" & MC_eaf$ANNOTATION_ID==graid_ANN_ID])
    refind_list <- append(refind_list, MC_eaf$VALUE[MC_eaf$TIER_ID=="refind" & MC_eaf$ANNOTATION_ID==refind_ANN_ID])
    isnref_list <- append(isnref_list, MC_eaf$VALUE[MC_eaf$TIER_ID=="isnref" & MC_eaf$ANNOTATION_ID==isnref_ANN_ID])
    lists <- list(value_list, distance_list, graid_list, refind_list, isnref_list)
    list_names <- c("value_list", "distance_list", "graid_list", "refind_list", "isnref_list")
    new_value_length <- length(value_list)
    new_distance_length <- length(distance_list)
    new_graid_length <- length(graid_list)
    new_refind_length <- length(refind_list)
    new_isnref_length <- length(isnref_list)
    new_lengths <- c(new_value_length, new_distance_length, new_graid_length, new_refind_length, new_isnref_length)

    for(index in 1:length(previous_lengths)) {
      if (previous_lengths[index] == new_lengths[index] & previous_lengths[index] > 0) {
        message(paste("Error: No value for ", list_names[index], "MC_word_ID: ", MC_word_ID))
        assign(list_names[index], append(unlist(lists[index]), NA))
        if (previous_lengths[index]+1 == new_lengths[index] & previous_lengths[index] > 0) {
          message("Error corrected")
        }
      }
    }
  }
}  

# Error: No value for  refind_list MC_word_ID:  "a4000"
# Error: No value for  isnref_list MC_word_ID:  a4000
# Error: No value for  refind_list MC_word_ID:  a4001
# Error: No value for  isnref_list MC_word_ID:  a4001

length(NULL)
length(valid_MC_word_IDs)
length(value_list)
length(distance_list)
length(graid_list)
length(refind_list)
length(isnref_list)

MC_file_ail0505_values_df <- data.frame(distance_list, graid_list, refind_list, isnref_list)
MC_file_ail0317_values_df <- data.frame(distance_list, graid_list, refind_list, isnref_list)
MC_file_ail0319_values_df <- data.frame(distance_list, graid_list, refind_list, isnref_list)
MC_file_all_values_df <- data.frame(value_list, distance_list, graid_list, refind_list, isnref_list)

data_file <- "MC_file_all_values_df.csv"

#add folder to file name
praat_table_path <- paste(languageFolder, data_file, sep = "/")

#load praat spreadsheet
### Encoding note, when loading and saving table, no not set to Unicode. Instead convert to unicode after it's loaded. If loaded and saved as unicode, and then reloaded, there are encoding errors
#MC_file_all_values_df <- read.csv(praat_table_path, header=TRUE, encoding="")

#####create top, erg, abs, top, dt, 0 columns#####

variable_names <- c("forms", "meanings", "animacy")
forms <- c("0", "dt", "abs", "erg", "top")
meanings <- c("NEW", "u", "ag", "pat")
animacy <- c("1", "2", "h", "anm")
variables <- list(forms, meanings, animacy)

forms_list <- NULL
meanings_list <- NULL
animacy_list <- NULL

col <- 4
for(variable_index in 1:length(variables)) {
  #variable_index <- 1
  variable <- unlist(variables[variable_index])                                              #testing
  for (label_index in 1:length(variable)) {
    #label_index <- 1
    col <- col + 1 #testing
    label_name_list <- paste(gsub("0", "zero", gsub("2", "two", gsub("1", "one", as.character(variable[label_index])))), "list", sep = "", collapse = "")
    assign(label_name_list, NULL)
    for(annotation in MC_file_all_values_df$graid_list) {
      #annotation <- MC_file_all_values_df$graid_list[1]                                                                     #testing
      if(grepl(as.character(variable[label_index]), annotation)) {
        assign(label_name_list, append(eval(parse(text = label_name_list)), "TRUE"))
      } else {
        assign(label_name_list, append(eval(parse(text = label_name_list)), "FALSE"))
      }
    }
    MC_file_all_values_df <- cbind(MC_file_all_values_df, eval(parse(text = label_name_list))) 
    names(MC_file_all_values_df)[names(MC_file_all_values_df) == names(MC_file_all_values_df)[col]] <- label_name_list
  }
}


MC_file_all_values_df_file<- "22.04.2021_MC_file_all_values_df.csv"

MC_file_all_values_df_path <- paste(languageFolder, MC_file_all_values_df_file, sep = "/")

#load praat spreadsheet
### Encoding note, when loading and saving table, no not set to Unicode. Instead convert to unicode after it's loaded. If loaded and saved as unicode, and then reloaded, there are encoding errors
MC_file_all_values_df <- read.csv(MC_file_all_values_df_path, header=TRUE, encoding="")

#####create top, erg, abs, top, dt, 0 vs distance histograms#####
MC_file_all_values_df_distance_numeric <- MC_file_all_values_df[MC_file_all_values_df$distance_list != "<NA>"& !is.na(MC_file_all_values_df$distance_list) & MC_file_all_values_df$distance_list != "NEW",]
MC_file_all_values_df_referents <- MC_file_all_values_df[MC_file_all_values_df$distance_list != "<NA>"& !is.na(MC_file_all_values_df$distance_list),]

MC_file_all_values_df_distance_numeric$distance_list <- as.numeric(MC_file_all_values_df_distance_numeric$distance_list)

MC_file_all_values_df_distance_numeric <- MC_file_all_values_df_distance_numeric[MC_file_all_values_df_distance_numeric$distance_list <20,]


MC_file_all_values_df_distance_numeric_top <- MC_file_all_values_df_distance_numeric[MC_file_all_values_df_distance_numeric$toplist=="TRUE",]
MC_file_all_values_df_distance_numeric_case <- MC_file_all_values_df_distance_numeric[MC_file_all_values_df_distance_numeric$erglist=="TRUE"|MC_file_all_values_df_distance_numeric$abslist=="TRUE"|MC_file_all_values_df_distance_numeric$toplist=="TRUE",]
MC_file_all_values_df_distance_numeric_no_case <- MC_file_all_values_df_distance_numeric[MC_file_all_values_df_distance_numeric$erglist=="FALSE"|MC_file_all_values_df_distance_numeric$abslist=="FALSE" & grepl("np",MC_file_all_values_df_distance_numeric$graid_list),]
MC_file_all_values_df_distance_numeric_dt <- MC_file_all_values_df_distance_numeric[MC_file_all_values_df_distance_numeric$dtlist=="TRUE",]


ggplot(data=MC_file_all_values_df_distance_numeric_no_case, aes(as.numeric(distance_list))) + 
  geom_histogram() +
  scale_x_log10() +
  labs(title = "Absence of Case Marking by Referent Distance", x = "Distance to Previous Reference", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=MC_file_all_values_df_distance_numeric_case, aes(as.numeric(distance_list))) + 
  geom_histogram() +
  scale_x_log10()  +
  labs(title = "Case Marking by Referent Distance", x = "Distance to Previous Reference", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data=MC_file_all_values_df_distance_numeric_dt, aes(as.numeric(distance_list))) + 
  geom_histogram() +
  scale_x_log10()  +
  labs(title = "Dislocated Topic Occurence by Referent Distance", x = "Distance to Previous Reference", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

#####get frequencies of dt&new, dt&!new, u&abs, pat&abs, ag&abs, u&erg, pat$erg, ag$erg#####

variables <- list(meanings, animacy)
variable_names <- c("forms", "meanings", "animacy")
forms <- c("dt", "abs|erg|top", "np", "0")
meanings <- c("u", "ag", "pat")
animacys <- c("1", "2", "h", "anm", "inanm")

case_new_freq <- sum(grepl("abs|erg|top", MC_file_all_values_df_referents$graid_list) & grepl("new", MC_file_all_values_df_referents$isnref_list))
case_not_new_freq <- sum(grepl("abs|erg|top", MC_file_all_values_df_referents$graid_list) & !grepl("new", MC_file_all_values_df_referents$isnref_list))

eval(parse(text = label_name_list))
Condition <- NULL
Condition_Present <- NULL
Condition_Absent <- NULL
Conditioning_factor <- NULL
  
for(form_index in 1:length(forms)) {
  #form_index <- 2  #testing
  form <- forms[form_index]
  if (form == "np") {
    for (meaning_index in 1:length(meanings)) {
      #meaning_index <- 1
      meaning <- meanings[meaning_index]
      Condition <- append(Condition, gsub("0", "zero", gsub("2", "two", gsub("1", "one", paste(as.character(forms[form_index]), "_", as.character(meanings[meaning_index]), sep = "", collapse = "")))))
      Condition_Present <- append(Condition_Present, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & !grepl("dt|abs|erg|top", MC_file_all_values_df_referents$graid_list) & (grepl(meanings[meaning_index], MC_file_all_values_df_referents$graid_list)|grepl(meanings[meaning_index], MC_file_all_values_df_referents$isnref_list))), Condition_Present)
      Condition_Absent <- append(Condition_Absent, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & !grepl("dt|abs|erg|top", MC_file_all_values_df_referents$graid_list) & (!grepl(meanings[meaning_index], MC_file_all_values_df_referents$graid_list) & !grepl(meanings[meaning_index], MC_file_all_values_df_referents$isnref_list))), Condition_Absent)
      Conditioning_factor <- append(Conditioning_factor, "meaning")
    }
    for (animacy_index in 1:length(animacys)) {
      #animacy_index <- 1
      animacy <- animacys[animacy_index]
      if (animacy == "inanm") {
        Condition <- append(Condition, gsub("0", "zero", gsub("2", "two", gsub("1", "one", paste(as.character(forms[form_index]), "_", as.character(animacys[animacy_index]), sep = "", collapse = "")))))
        Condition_Present <- append(Condition_Present, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & !grepl("1|2|h|anm", MC_file_all_values_df_referents$graid_list) & !grepl("1|2|h|anm", MC_file_all_values_df_referents$refind_list)))
        Condition_Absent <- append(Condition_Absent, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & grepl("1|2|h|anm", MC_file_all_values_df_referents$graid_list)))
        Conditioning_factor <- append(Conditioning_factor, "animacy")
      } else { 
        Condition <- append(Condition, gsub("0", "zero", gsub("2", "two", gsub("1", "one", paste(as.character(forms[form_index]), "_", as.character(animacys[animacy_index]), sep = "", collapse = "")))))
        Condition_Present <- append(Condition_Present, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & !grepl("dt|abs|erg|top", MC_file_all_values_df_referents$graid_list) & (grepl(animacys[animacy_index], MC_file_all_values_df_referents$graid_list)|grepl(animacys[animacy_index], MC_file_all_values_df_referents$isnref_list))))
        Condition_Absent <- append(Condition_Absent, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & !grepl("dt|abs|erg|top", MC_file_all_values_df_referents$graid_list) & (!grepl(animacys[animacy_index], MC_file_all_values_df_referents$graid_list) & !grepl(animacys[animacy_index], MC_file_all_values_df_referents$isnref_list))))
        Conditioning_factor <- append(Conditioning_factor, "animacy")
      }
    }
  } else {
    for (meaning_index in 1:length(meanings)) {
      #meaning_index <- 2
      meaning <- meanings[meaning_index]
      Condition <- append(Condition, gsub("0", "zero", gsub("2", "two", gsub("1", "one", paste(as.character(forms[form_index]), "_", as.character(meanings[meaning_index]), sep = "", collapse = "")))))
      Condition_Present <- append(Condition_Present, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & (grepl(meanings[meaning_index], MC_file_all_values_df_referents$graid_list)|grepl(meanings[meaning_index], MC_file_all_values_df_referents$isnref_list))))
      Condition_Absent <- append(Condition_Absent, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & (!grepl(meanings[meaning_index], MC_file_all_values_df_referents$graid_list) & !grepl(meanings[meaning_index], MC_file_all_values_df_referents$isnref_list))))
      Conditioning_factor <- append(Conditioning_factor, "meaning")
    }
    for (animacy_index in 1:length(animacys)) {
      #animacy_index <- 1
      animacy <- animacys[animacy_index]
      if (animacy == "inanm") {
        Condition <- append(Condition, gsub("0", "zero", gsub("2", "two", gsub("1", "one", paste(as.character(forms[form_index]), "_", as.character(animacys[animacy_index]), sep = "", collapse = "")))))
        Condition_Present <- append(Condition_Present, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & !grepl("1|2|h|anm", MC_file_all_values_df_referents$graid_list) & !grepl("1|2|h|anm", MC_file_all_values_df_referents$refind_list)))
        Condition_Absent <- append(Condition_Absent, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & grepl("1|2|h|anm", MC_file_all_values_df_referents$graid_list)))
        Conditioning_factor <- append(Conditioning_factor, "animacy")
      } else { 
        Condition <- append(Condition, gsub("0", "zero", gsub("2", "two", gsub("1", "one", paste(as.character(forms[form_index]), "_", as.character(animacys[animacy_index]), sep = "", collapse = "")))))
        Condition_Present <- append(Condition_Present, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & (grepl(animacys[animacy_index], MC_file_all_values_df_referents$graid_list)|grepl(animacys[animacy_index], MC_file_all_values_df_referents$isnref_list))))
        Condition_Absent <- append(Condition_Absent, sum(grepl(forms[form_index], MC_file_all_values_df_referents$graid_list) & (!grepl(animacys[animacy_index], MC_file_all_values_df_referents$graid_list) & !grepl(animacys[animacy_index], MC_file_all_values_df_referents$isnref_list))))
        Conditioning_factor <- append(Conditioning_factor, "animacy")
      }
    }
  }
}

MC_file_all_values_df[as.logical(MC_file_all_values_df[,paste(feature, "list", sep="", collapse="")]),]

Conditions <- data.frame(Condition, Condition_Present, Condition_Absent, Conditioning_factor)
Conditions$Condition_Present_Normalized <- Conditions$Condition_Present/nrow(MC_file_all_values_df_referents)
Conditions$Condition_Absent_Normalized <- Conditions$Condition_Absent/nrow(MC_file_all_values_df_referents)
Conditions$Condition_Difference <- (Conditions$Condition_Present_Normalized - Conditions$Condition_Absent_Normalized) 

Conditions_meaning <- Conditions[Conditions$Conditioning_factor=="meaning",]
Conditions_animacy <- Conditions[Conditions$Conditioning_factor=="animacy",]


ggplot(Conditions_meaning, aes(x=Condition, y=Condition_Difference)) +
  geom_col() +
  coord_flip() +
  labs(title = "Form Frequency by Animacy", x = "Form/Animacy Values", y = "Form/Animacy Frequencies") +
  theme(plot.title = element_text(hjust = 0.5))

  #scale_fill_discrete(name="Word Class", labels = c("All Word Classes", "Onomatopoeia"))  
#####boxplot of chain length
#get_chain_lengths <- function(MC_eaf)

chain_lengths <- NULL
for (file in MC_files) {
  MC_eaf <- efileAnnotations(file)
  MC_eaf_chain_lengths <- get_chain_lengths(MC_eaf)
  chain_lengths <- append(MC_eaf_chain_lengths, chain_lengths)
  
  print(length(MC_eaf_chain_lengths))
}

ggplot() +
  aes(chain_lengths) + 
  geom_histogram() +
  labs(title = "Clause Chain Length", x = "Length", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot() + aes(chain_lengths)+ geom_histogram(binwidth=1, colour="black", fill="white")


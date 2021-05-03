#####This function adds the values from a multi-cast annotated file and adds them to a csv file



get_MC <- function(praat_table , MC_files) {
  
  #find and write multi-CAST values
  #Multi-CAST tiernames
  MC_segnum_name <- "utterance_id"
  MC_utterance_name <- "utterance"
  MC_word_tier_name <- "grammatical_words"
  MC_gloss_tier_name <- "gloss"
  MC_graid_tier_name <- "graid"
  MC_refind_tier_name <- "refind"
  MC_isnref_tier_name <- "isnref"
  MC_save_variable <- 1
  ######find and write multi-CAST values######
  #Check if MC_eaf file is already loaded, and if not load it and clean up values

  if(!exists("MC_eaf_file") & sum(grepl(praat_table$corpus[index], MC_files))>0) {
    MC_eaf_file <- MC_files[grepl(praat_table$corpus[index], MC_files)]
    MC_eaf <- efileAnnotations(MC_eaf_file)
    #remove long and short vowel symbols, make lowercase, remove hyphens and clitic boundaries
    MC_eaf$VALUE <- enc2native(MC_eaf$VALUE)
    MC_eaf$VALUE <- gsub("<U\\+0306>", "", MC_eaf$VALUE)
    MC_eaf$VALUE <- gsub("<U\\+02D0>", "", MC_eaf$VALUE)
    MC_eaf$VALUE <- gsub("~", "", MC_eaf$VALUE)
    MC_eaf$VALUE <- gsub("-", "", MC_eaf$VALUE)
    MC_eaf$VALUE <- gsub("=", "", MC_eaf$VALUE)
    MC_eaf$VALUE <- gsub("<U\\+0283>", "s", MC_eaf$VALUE)
    Encoding(MC_eaf$VALUE) <- "UTF-8"
    MC_eaf$VALUE <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", MC_eaf$VALUE))
    #to lower must be after unicode escape conversion
    MC_eaf$VALUE <- tolower(MC_eaf$VALUE)
  }
  #if the index has an MC eaf file different from the last file, reload the file
  if(exists("MC_eaf_file") & !grepl(praat_table$corpus[index], MC_eaf_file)) {
    if(!grepl(praat_table$corpus[index], MC_eaf_file) & sum(grepl(praat_table$corpus[index], MC_files))>0){
      MC_eaf_file <- MC_files[grepl(praat_table$corpus[index], MC_files)]
      MC_eaf <- efileAnnotations(MC_eaf_file)
      #remove long and short vowel symbols, make lowercase, remove hyphens and clitic boundaries
      MC_eaf$VALUE <- enc2native(MC_eaf$VALUE)
      MC_eaf$VALUE <- gsub("<U\\+0306>", "", MC_eaf$VALUE)
      MC_eaf$VALUE <- gsub("<U\\+02D0>", "", MC_eaf$VALUE)
      MC_eaf$VALUE <- gsub("~", "", MC_eaf$VALUE)
      MC_eaf$VALUE <- gsub("-", "", MC_eaf$VALUE)
      MC_eaf$VALUE <- gsub("=", "", MC_eaf$VALUE)
      MC_eaf$VALUE <- gsub("<U\\+0283>", "s", MC_eaf$VALUE)
      Encoding(MC_eaf$VALUE) <- "UTF-8"
      MC_eaf$VALUE <- stri_unescape_unicode(gsub("<U\\+(....)>", "\\\\u\\1", MC_eaf$VALUE))
      #to lower must be after unicode escape conversion
      MC_eaf$VALUE <- tolower(MC_eaf$VALUE)
    }
  }
  #If the current file has a MC eaf availaible, find and write the values
  if(exists("MC_eaf_file") & sum(grepl(praat_table$corpus[index], MC_files))>0) {
    if (sum(grepl(praat_table$corpus[index], MC_files)) > 0 & length(MC_eaf$ANNOTATION_ID[grepl(MC_segnum_name, MC_eaf$TIER_ID) & MC_eaf$t0 == praat_table$sentenceOnset[index]*1000 & MC_eaf$t1 == praat_table$sentenceOffset[index]*1000]) != 0) {
      MC_segnum_ANN_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_segnum_name, MC_eaf$TIER_ID) & MC_eaf$t0 == praat_table$sentenceOnset[index]*1000 & MC_eaf$t1 == praat_table$sentenceOffset[index]*1000]
    }
    for(MC_ANN_ID in MC_segnum_ANN_ID) {
      #test
      #MC_ANN_ID <- MC_segnum_ANN_ID
      if(length(MC_eaf$VALUE[grepl(MC_utterance_name, MC_eaf$TIER_ID, ignore.case=TRUE) & grepl(word, MC_eaf$VALUE, ignore.case=TRUE) & MC_ANN_ID == MC_eaf$ANNOTATION_REF])!=0) {
        MC_segnum_value <- MC_eaf$VALUE[grepl(MC_segnum_name, MC_eaf$TIER_ID) & MC_ANN_ID == MC_eaf$ANNOTATION_ID]
        #find flex_word annotation ID, subset for flex_word tier, value should equal the value of the word in the praat table, and the annotation ref should equal the annotation ID of the segnum tier
        #This needs to be set up as a for loop to catch multiple instances of the same word in a segment
        MC_segnum_ANN_ID <- MC_ANN_ID
        MC_utterance_ID <- MC_eaf$ANNOTATION_ID[MC_utterance_name == MC_eaf$TIER_ID & MC_segnum_ANN_ID == MC_eaf$ANNOTATION_REF]
        MC_word_tier_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_word_tier_name, MC_eaf$TIER_ID) & MC_utterance_ID == MC_eaf$ANNOTATION_REF & grepl(word, MC_eaf$VALUE, ignore.case=TRUE)] #"noËÉËmÉË" 
        #if the word can't find a match, try matching for a less exact match, ie an inflected form
        if (length(MC_word_tier_ID) < 1) {
          word <- MC_eaf$VALUE[grepl(MC_word_tier_name, MC_eaf$TIER_ID) & grepl(word, MC_eaf$VALUE, ignore.case=TRUE) & grepl(MC_ANN_ID, MC_eaf$ANNOTATION_REF)][1]
          MC_word_tier_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_word_tier_name, MC_eaf$TIER_ID) & word == MC_eaf$VALUE & grepl(MC_ANN_ID, MC_eaf$ANNOTATION_REF)]
        }
      }
    }
    
    #####find MC_word annotation ID#####
    #subset for flex_word tier, value should equal the value of the word in the praat table, and the annotation ref should equal the annotation ID of the segnum tier
    #This needs to be set up as a for loop to catch multiple instances of the same word in a segment_number
    #find flex_word annotation ID, subset for flex_word tier, value should equal the value of the word in the praat table, and the annotation ref should equal the annotation ID of the segnum tier
    #This needs to be set up as a for loop to catch multiple instances of the same word in a segment_number
    
    #if the word appears more than once in the segment, determine which one it is.
    if(length(MC_word_tier_ID[!is.na(MC_word_tier_ID)]) > 0) {
      MC_word_tier_IDs <- MC_word_tier_ID
      MC_word_count_variable <- paste0("MC", gsub("-", "", MC_word_tier_ID[1]))
      if(!exists(MC_word_count_variable)) {
        assign(MC_word_count_variable, MC_word_count)
      }
      #test printing
      #print(paste("More than one MC_word_tier_ID found:", word, "word count: ", eval(parse(text = MC_word_count_variable))))
      #default to the first phone and first instance of the word
      MC_phone_count <- MC_phone_count+1
      #print(paste("MC_phone_count: ", MC_phone_count, ", phone: ", phone))
      #once the first instance is complete, add one to the index to move on to the next instance of the word
      MC_word_tier_ID <- MC_word_tier_IDs[eval(parse(text = MC_word_count_variable))]
      #test printing
      #print(paste("MC_word_tier_ID", MC_word_tier_ID))
      #if this is the last phone of the word, increase the MC_word_count variable by 1
      if(MC_phone_count==nchar(word)) {
        #test printing
        last <- praat_table$segment[index]
        Encoding(last) <- "UTF-8"
        last <- enc2utf8(last)
        #print(paste("last letter", last))
        assign(MC_word_count_variable, eval(parse(text = MC_word_count_variable))+1)
        #test printing
        #print(paste("new MC_word_count", eval(parse(text = MC_word_count_variable))))
        MC_phone_count <- 0
        #test printing
        #print(paste("new MC_phone_count", MC_phone_count))
      }
      #after last phone of last word, reset variables
      if(eval(parse(text = MC_word_count_variable))==length(MC_word_tier_IDs) & MC_phone_count==nchar(sub("Ë", "", word))) {
        assign(MC_word_count_variable, 1)
        MC_phone_count <- 0
        MC_word_count <- 1
        #print(paste("last phone of last word", "phone: ", phone,"word: ", word))
        #print(paste("MC_phone_count reset", "MC_word_count reset"))
      }
    }
    
    ######find MC_Gloss annotation ID######
    if(length(MC_word_tier_ID[!is.na(MC_word_tier_ID)]) > 0) {
      #find word annotation ID, subset for word tier, value should equal the value of the word in the praat table, and the annotation ref should equal the annotation ID of the segnum tier
      MC_gloss_tier_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_gloss_tier_name, MC_eaf$TIER_ID) & MC_word_tier_ID == MC_eaf$ANNOTATION_REF]
      #use word_tier_ID to find component morphemes, POS's, and glosses, combine multiple values into single string seperated by "+", since there may be multiple morphemes, there needs to be a for loop
      MC_gloss <- MC_eaf$VALUE[grepl(MC_gloss_tier_name, MC_eaf$TIER_ID) & MC_word_tier_ID == MC_eaf$ANNOTATION_REF]
      #test for NA's and don't add NAs to the list
      
      ###########find MC_graid annotation ID######
      MC_graid_tier_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_graid_tier_name, MC_eaf$TIER_ID) & MC_word_tier_ID == MC_eaf$ANNOTATION_REF]
      #use word_tier_ID to find component morphemes, POS's, and glosses, combine multiple values into single string seperated by "+", since there may be multiple morphemes, there needs to be a for loop
      MC_graid <- MC_eaf$VALUE[grepl(MC_graid_tier_name, MC_eaf$TIER_ID) & MC_word_tier_ID == MC_eaf$ANNOTATION_REF]
      
      ###########find MC_refind annotation ID######
      MC_refind_tier_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_refind_tier_name, MC_eaf$TIER_ID) & MC_graid_tier_ID == MC_eaf$ANNOTATION_REF]
      #use word_tier_ID to find component morphemes, POS's, and glosses, combine multiple values into single string seperated by "+", since there may be multiple morphemes, there needs to be a for loop
      MC_refind <- MC_eaf$VALUE[grepl(MC_refind_tier_name, MC_eaf$TIER_ID) & MC_graid_tier_ID == MC_eaf$ANNOTATION_REF]
      
      ###########find MC_isnref annotation ID######
      MC_isnref_tier_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_isnref_tier_name, MC_eaf$TIER_ID) & MC_refind_tier_ID == MC_eaf$ANNOTATION_REF]
      #use word_tier_ID to find component morphemes, POS's, and glosses, combine multiple values into single string seperated by "+", since there may be multiple morphemes, there needs to be a for loop
      MC_isnref <- MC_eaf$VALUE[grepl(MC_isnref_tier_name, MC_eaf$TIER_ID) & MC_refind_tier_ID == MC_eaf$ANNOTATION_REF]
      
      ######MC_grammatical words########
      MC_utterance_list <- MC_eaf$VALUE[grepl(MC_word_tier_name, MC_eaf$TIER_ID) & MC_utterance_ID == MC_eaf$ANNOTATION_REF]
      MC_utterance <- paste0(MC_utterance_list, collapse = "+")
      
      ######MC_utterance_graid#########
      MC_utterance_word_IDs <- MC_eaf$ANNOTATION_ID[grepl(MC_word_tier_name, MC_eaf$TIER_ID) & MC_utterance_ID == MC_eaf$ANNOTATION_REF]
      MC_utterance_graid_list <- NULL
      MC_utterance_graid_IDs <- NULL
      for (ID in MC_utterance_word_IDs) {
        MC_utterance_graid <- MC_eaf$VALUE[grepl(MC_graid_tier_name, MC_eaf$TIER_ID) & ID == MC_eaf$ANNOTATION_REF]
        MC_utterance_graid_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_graid_tier_name, MC_eaf$TIER_ID) & ID == MC_eaf$ANNOTATION_REF]
        if(length(MC_utterance_graid) > 0) {
          if(!is.na(MC_utterance_graid) & MC_utterance_graid != "NA") {
            MC_utterance_graid_list <- append(MC_utterance_graid_list, MC_utterance_graid)
            MC_utterance_graid_IDs <- append(MC_utterance_graid_IDs, MC_utterance_graid_ID)
          }  
        }  
      }
      MC_utterance_graid <- paste(MC_utterance_graid_list, collapse = "+")
      
      ######MC_utterance_refind#######
      MC_utterance_refind_list <- NULL
      MC_utterance_refind_IDs <- NULL
      for (ID in MC_utterance_graid_IDs) {
        MC_utterance_refind <- MC_eaf$VALUE[grepl(MC_refind_tier_name, MC_eaf$TIER_ID) & ID == MC_eaf$ANNOTATION_REF]
        MC_utterance_refind_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_refind_tier_name, MC_eaf$TIER_ID) & ID == MC_eaf$ANNOTATION_REF]
        if(length(MC_utterance_refind) > 0) {
          if(!is.na(MC_utterance_refind) & MC_utterance_refind != "NA") {
            MC_utterance_refind_list <- append(MC_utterance_refind_list, MC_utterance_refind)
            MC_utterance_refind_IDs <- append(MC_utterance_refind_IDs, MC_utterance_refind_ID)
          }
        }
      }
      MC_utterance_refind <- paste(MC_utterance_refind_list, collapse = "+")
      
      ######MC_utterance_isnref########
      MC_utterance_isnref_list <- NULL
      MC_utterance_isnref_IDs <- NULL
      for (ID in MC_utterance_refind_IDs) {
        #ID <- MC_utterance_graid_IDs[4]
        MC_utterance_isnref <- MC_eaf$VALUE[grepl(MC_isnref_tier_name, MC_eaf$TIER_ID) & ID == MC_eaf$ANNOTATION_REF]
        MC_utterance_isnref_ID <- MC_eaf$ANNOTATION_ID[grepl(MC_isnref_tier_name, MC_eaf$TIER_ID) & ID == MC_eaf$ANNOTATION_REF]
        if(length(MC_utterance_isnref) > 0) {
          if(!is.na(MC_utterance_isnref) & MC_utterance_isnref != "NA") {
            MC_utterance_isnref_list <- append(MC_utterance_isnref_list, MC_utterance_isnref)
            MC_utterance_isnref_IDs <- append(MC_utterance_isnref_IDs, MC_utterance_isnref_ID)
          }
        }
      }
      MC_utterance_isnref <- paste(MC_utterance_isnref_list, collapse = "+")  
      
      
      #####Write MC values to Praat table#####
      if (length(MC_graid) > 0) {
        if (MC_graid != "" & !is.na(MC_graid) & MC_graid != "NA" ) {
          praat_table$MC_graid[index] <- MC_graid
        }
      }
      if (length(MC_refind) > 0) {
        if (MC_refind != "" & !is.na(MC_refind) & MC_refind != "NA" ) {
          praat_table$MC_refind[index] <- MC_refind
        }
      }
      if (length(MC_isnref) > 0) {
        if (MC_isnref != "" & !is.na(MC_isnref) & MC_isnref != "NA" ) {
          praat_table$MC_isnref[index] <- MC_isnref
        }
      }
      if (length(MC_utterance_graid) > 0) {
        if (MC_utterance_graid != "" & !is.na(MC_utterance_graid) & MC_utterance_graid != "NA" ) {
          praat_table$MC_utterance_graid[index] <- MC_utterance_graid
        }
      }
      if (length(MC_utterance_refind) > 0) {
        if (MC_utterance_refind != "" & !is.na(MC_utterance_refind) & MC_utterance_refind != "NA" ) {
          praat_table$MC_utterance_refind[index] <- MC_utterance_refind
        }
      }
      if (length(MC_utterance_isnref) > 0) {
        if (MC_utterance_isnref != "" & !is.na(MC_utterance_isnref) & MC_utterance_isnref != "NA" ) {
          praat_table$MC_utterance_isnref[index] <- MC_utterance_isnref
        }
      }
      if (length(MC_segnum_ANN_ID) > 0) {
        if (MC_segnum_ANN_ID != "" & !is.na(MC_segnum_ANN_ID) & MC_segnum_ANN_ID != "NA" ) {
          praat_table$MC_segnum_ANN_ID[index] <- MC_segnum_ANN_ID
        }
      }
      if (length(MC_word_tier_ID) > 0) {
        if (MC_word_tier_ID != "" & !is.na(MC_word_tier_ID) & MC_word_tier_ID != "NA" ) {
          praat_table$MC_word_tier_ID[index] <- MC_word_tier_ID
        }
      }
      
      #####Test printing result######
      print(paste("MC_graid: ", MC_graid, "MC_refind: ", MC_refind, "MC_isnref: ", MC_isnref))
      #####
    }
  }  
  ######after every hundredth segment is finished, or the last segment of the praat_table is done, or the last segment of an eaf file is done write the results to a backup file#####
  print(paste("word: ", word, "segmented_morphemes: ", segmented_morphemes, "word_POS: ", word_POS, "word_gloss: ",  word_gloss, "segment_number: ", segnum_value))
  if(length(segnum_ANN_ID) > 0 & length(next_segnum_ANN_ID[1]) > 0){
    if((segnum_ANN_ID[1] != next_segnum_ANN_ID[1] & save_variable %% 10 == 0)|index == nrow(praat_table)|praat_table$corpus[index]!=praat_table$corpus[index+1]) {
      #modify to save after a segment with segment and index in name
      save_variable <- save_variable+1
      output <- getwd()
      write.csv(praat_table, paste(output, languageFolder, paste("praat_table_updated_", "last_segnum_", segnum_value, "_index_", index, ".csv", sep=""), sep="/"), row.names = FALSE, fileEncoding = "")
      print(paste("praat_table_updated, ", "last_segnum :", segnum_value, sep = ""))
    }
  }
}
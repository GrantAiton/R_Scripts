#####Looks at a referent in a Multi-CAST eaf file and returns whether the referent is correferential 
#with the previous referent, with the next referent, or with the next final clause

#ID_value refers to the input annotation id, and specifies which tier value of the the annotation id
#possible values are "grammatical_word", "graid", "refind", and "isnref" 
#output refers to what value is desired, ie what the input annotation is compared against
#possible values are "previous", "next", "next_final", and "previous_final"
#target refers to the syntactic role of the argument that the input referent is compared against
#possible values are "a", "s", "p", "sub" and "any"

#necessary packages:
#install_github("dalejbarr/elan") #need to install Rtools on system
library(elan)

#testing
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

MC_eaf <- efileAnnotations(MC_files[1])

get_SR <- function(ID, MC_eaf, ID_value = "refind", output = "previous", target = "sub") {
#find refind tier and annotation value
  input_tier <- MC_eaf$TIER_ID[MC_eaf$ANNOTATION_ID==ID,]
  if(input_tier == "refind") {
    refind_ANN_ID <- ID
  } else if(input_tier == "grammatical_word") {
    graid_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$ANNOTATION_REF==ID,]
    refind_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$ANNOTATION_REF==graid_ANN_ID,]
  } else if(input_tier == "graid")
    refind_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$ANNOTATION_REF==ID,]
  } else if(input_tier == "isnref") {
    refind_ANN_ID <- MC_eaf$ANNOTATION_REF[MC_eaf$ANNOTATION_ID==ID,]
  } else {
    message("Input tier not found")
  }
  if(length(refind_ANN_ID)==0|is.na(refind_ANN_ID)) {
    message("refind_ANN_ID not found")
  }

####build data.frame of all isnref values####

####use isn_ref_ANN_ID to match index in data.frame####

####scavange ref_distance code to find previous_referent or next_referent####

#####check graid value of previous_referent or next_referent#####
#if the syntactic role of the previous_referent or next_referent matches the target, then check 
#if the syntactic role of the previous_referent or next_referent doesn't match the target, 
#move on to the next referent in the refind data.frame

####check clause_type value of previous_referent or next_referent####
#once a previous_referent or next_referent is found with graid that a matches a target value:
#match graid of previous_referent or next_referent to graid data.frame index
#find preceding clause boundary corresponding to a medial or final clause
#if clause type is compatable with the output value, return result as TRUE or FALSE 
#this logical states whether the two referents are coreferential

}
#####calcualte the number of referent in a Multi-CAST file between this referent and the previous mention of this referent
#index is a number of the row in the MC_eaf multi-cast file from the refind tier
#MC_eaf_refind is the subset of the MC_eaf file of only the refind tier
library(elan)

get_MC_ref_distance <- function(index, MC_eaf, MC_word_tier_ID_as_index = FALSE) {
  #Multi-CAST tiernames
  MC_eaf_refind <- MC_eaf[MC_eaf$TIER_ID == "refind" & MC_eaf$VALUE != "",]
  distance <- 0
  found <- FALSE
  if (MC_word_tier_ID_as_index) {
    #use MC_word_tier_ID to find refind value and set index
    graid_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="graid" & MC_eaf$ANNOTATION_REF==index]
    refind_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="refind" & MC_eaf$ANNOTATION_REF==graid_ANN_ID]
    index <- match(refind_ANN_ID, MC_eaf_refind$ANNOTATION_ID)
  }
  if (length(index)>0) {
    if(is.na(index)) {
      distance <- NA
    } else {
      if(index > 1) {
        previous_referent_index <- index - 1
        while (found == FALSE) {
          if (MC_eaf_refind$VALUE[index] != MC_eaf_refind$VALUE[previous_referent_index]) {
            distance <- distance + 1
            previous_referent_index <- previous_referent_index - 1
            if(previous_referent_index == 1) {
              distance <- "NEW"
              found <- TRUE
            }
          } else {
            found <- TRUE
          }
        }
      }
    }
  }
  return(distance)
}

#testing
# #creates list of eaf files
# languageFolder <- './rawinput'
# fls <- list.files(languageFolder, full.names = F, pattern = 'eaf')
# full_fls <- list.files(languageFolder, full.names = T, pattern = 'eaf')
# MC_files <- full_fls[grepl("-3", full_fls)]
# full_fls <- full_fls[grepl("-2", full_fls)]
# MC_eaf_file <- MC_files[1]
# MC_eaf <- efileAnnotations(MC_eaf_file)
# get_MC_ref_distance("a3842", MC_eaf, MC_word_tier_ID_as_index = TRUE)
# MC_word_tier_ID <- MC_eaf_refind$ANNOTATION_REF[1]
# MC_eaf[MC_eaf$ANNOTATION_ID==MC_word_tier_ID,]
# 
# graid_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="graid" & MC_eaf$ANNOTATION_REF==MC_word_tier_ID]
# refind_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="refind" & MC_eaf$ANNOTATION_REF==graid_ANN_ID]
# isnref_ANN_ID <- MC_eaf$ANNOTATION_ID[MC_eaf$TIER_ID=="isnref" & MC_eaf$ANNOTATION_REF==refind_ANN_ID]
# 
# index <- match(refind_ANN_ID, MC_eaf_refind$ANNOTATION_ID)
# 
# 
# 
# MC_eaf_refind[MC_eaf_refind$TIER_ID=="refind" & MC_eaf_refind$ANNOTATION_REF==graid_ANN_ID,]
# 
# MC_eaf_refind[index,]


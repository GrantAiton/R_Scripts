#####clause chain distance
# input is a Multi-CAST character vector, out put is a vector of numbers equal to the length of all clause chains in the file.



get_chain_lengths <- function(MC_eaf) {
  MC_eaf_graid_values <- MC_eaf[MC_eaf$TIER_ID=="graid"&MC_eaf$VALUE!="",]$VALUE
  chain_lengths <- NULL
  chain_length <- 1
  for(value in MC_eaf_graid_values) {
    if(value == "##") {
      chain_lengths <- append(chain_lengths, chain_length)
      chain_length <- 1
    } else if(value == "#") {
      chain_length <- chain_length + 1
    }
  }
  return(chain_lengths)
}

get_chain_values <- function(MC_eaf) {
  MC_eaf_graid_values <- MC_eaf[MC_eaf$TIER_ID=="graid" & MC_eaf$VALUE!="",]$VALUE
  chain_values <- NULL
  chain_value <- NULL
  final_clause <- FALSE
  for(value in MC_eaf_graid_values) {
    #value <- MC_eaf_graid_values[5]
    if((value == "##"|value == "##neg") & !final_clause) {
      chain_value <- append(chain_value, value)
      final_clause <- TRUE
    } else if((value == "##"|value == "##neg") & final_clause) {
      chain_values <- append(chain_values, paste(chain_value, collapse = " "))
      chain_value <- NULL
      chain_value <- append(chain_value, value)
    } else if((value == "#"|value == "#neg") & final_clause) {
      chain_values <- append(chain_values, paste(chain_value, collapse = " "))
      chain_value <- NULL
      chain_value <- append(chain_value, value)
      final_clause <- FALSE
    } else if(value == "#"|value == "#neg") {
      final_clause <- FALSE
      chain_value <- append(chain_value, value)
    } else if ((value == "##"|value == "##neg") & is.null(chain_value) & !final_clause) {
      chain_value <- append(chain_value, value)
      final_clause <- TRUE
    } else {
      chain_value <- append(chain_value, value)
    }
  }
  return(chain_values)
}

get_clause_count <- function(MC_eaf) {
  MC_eaf_graid_values <- MC_eaf[MC_eaf$TIER_ID=="graid" & MC_eaf$VALUE!="",]$VALUE
  clause_count <- 0
  for(value in MC_eaf_graid_values) {
    if(value == "##"|value == "##neg"|value == "#"|value == "#neg") {
      clause_count <- clause_count + 1
    }
  }
  return(clause_count)
}

#testing
#creates list of eaf files
#languageFolder <- './rawinput'
#fls <- list.files(languageFolder, full.names = F, pattern = 'eaf')
#full_fls <- list.files(languageFolder, full.names = T, pattern = 'eaf')
#MC_files <- full_fls[grepl("-3", full_fls)]
#full_fls <- full_fls[grepl("-2", full_fls)]
#MC_eaf_file <- MC_files[1]
#MC_eaf <- efileAnnotations(MC_eaf_file)


#get_chain_lengths(MC_eaf)
#get_chain_values(MC_eaf)[41]
#get_clause_count(MC_eaf)
#MC_eaf_graid_values[1:50]

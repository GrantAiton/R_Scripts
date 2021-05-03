#####breaks praat_table into csvs suitable for import into ELAN

output <- getwd()

extract_eaf_tier_from_csv <- function(csv, file, tier_name, output, write = TRUE) {
  #subset csv to only entries from file of interest
  file_csv <- csv[csv$corpus==file,]
  #create column vectors
  tier <- tier_name
  annotation <- file_csv[,tier_name]
  begin_time <- file_csv$onset
  end_time <- file_csv$offset
  duration <- file_csv$dur
  #combime vectors into dataframe
  tier_df <- data.frame(tier, annotation, begin_time, end_time, duration)
  #save csv
  if (write = TRUE) {
    write.csv(tier_df, paste(output, languageFolder, paste(file, "_eaf_tier_", tier_name, ".csv", sep=""), sep="/"), row.names = FALSE, fileEncoding = "")
  }
}

##testing
praat_table_file <- "2021.04.18 praat_table_updated_fourthpass.csv"
languageFolder <- './rawinput'

#add folder to file name
praat_table_path <- paste(languageFolder, praat_table_file, sep = "/")

#load praat spreadsheet
### Encoding note, when loading and saving table, no not set to Unicode. Instead convert to unicode after it's loaded. If loaded and saved as unicode, and then reloaded, there are encoding errors
praat_table <- read.csv(praat_table_path, header=TRUE, encoding="")
csv <- praat_table
file <- "ailot0317"
tier_names <- c("F1", "F2", "F3", "dur", "pitch", "intensity", "tokenuniqueid")

tier_name <- tier_names[1]
for(tier in tier_names) {
  extract_eaf_tier_from_csv(praat_table, "ailot0317", tier, output)
}



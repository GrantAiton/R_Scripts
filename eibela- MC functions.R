
# comvert an EAF file into an intermediary TXT format
convertEAF <- function(.tiernames,
					   .readFrom,
					   .writeTo) {

	# find all EAF files in the specified directory
	files <- list.files(path = .readFrom,
						pattern = ".eaf",
						full.names = TRUE,
						recursive = FALSE)
	
	# if no files have been found, abort
	if (length(files) < 1) { stop("No EAF files found.") }
	
	# prepare and export all files
	for (i in 1:length(files)) {
		filename <- gsub("^.*/(.+)\\.eaf$", "\\1", files[i])
	
		# read EAF
		message(paste0("Reading file '", filename, ".eaf'..."))
		tmp <- readEAF(files[i], filename)
		
		# write EAF to TSV
		message(paste0(" Writing file '", filename, "_raw-text.txt'..."))
		writeEAF(tmp, filename, .writeTo)
		
		message("  Done!")
	}
}


# -------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------- #

# read an EAF file
readEAF <- function(.file, .filename) {
	
	# read EAF files
	raweaf <- read_xml(.file, encoding = "UTF-8")

	# read time slots
	time_id <- xml_attr(xml_find_all(raweaf, "//TIME_SLOT"), "TIME_SLOT_ID")
	time_val <- xml_attr(xml_find_all(raweaf, "//TIME_SLOT"), "TIME_VALUE")
	time_tab <- data.table(time_id, time_val)
	
	# read relevant tiers
	segments_tab <- readEAFtier(raweaf, .tiernames["segments"], .timeAligned = TRUE)
	translations_tab <- readEAFtier(raweaf, .tiernames["translations"])
	swords_tab <- readEAFtier(raweaf, .tiernames["words"])
	morphs_tab <- readEAFtier(raweaf, .tiernames["morphs"])
	sgloss_tab <- readEAFtier(raweaf, .tiernames["glosses"])
	
	# merge gwords into utterances
	utterance_tab <- unique(swords_tab, by = "ref")
	
	for (i in 1:nrow(utterance_tab)) {
		tmp <- paste0(swords_tab[ref == utterance_tab[i, ref], value], collapse = " ")
		utterance_tab[i, value := tmp]
	}
	
	# merge segmented morphemes into words
	gwords_tab <- unique(morphs_tab, by = "ref")
	
	for (i in 1:nrow(gwords_tab)) {
		tmp <- paste0(morphs_tab[ref == gwords_tab[i, ref], value], collapse = "")
		gwords_tab[i, value := tmp]
	}
	
	# merge segmented glosses into words,
	# adding missing affix and clitic boundary markers
	tmp <- copy(morphs_tab)
	tmp[, ref_word := ref]
	tmp[, ref := id]

	gls <- sgloss_tab[tmp[, 2:4], on = "ref"][!is.na(value), ]
	
	gls[grepl("^=", i.value) & !grepl("^=", value), value := paste0("=", value)]
	gls[grepl("=$", i.value) & !grepl("=$", value), value := paste0(value, "=")]
	
	gls[grepl("^-", i.value) & !grepl("^-", value), value := paste0("-", value)]
	gls[grepl("-$", i.value) & !grepl("-$", value), value := paste0(value, "-")]
	
	gls[grepl("^\\*", i.value) & !grepl("^\\*", value), value := paste0("*", value)]
	gls[grepl("\\*$", i.value) & !grepl("\\*$", value), value := paste0(value, "*")]
	
	ggloss_tab <- unique(gls, by = "ref_word")

	for (i in 1:nrow(ggloss_tab)) {
		tmp <- paste0(gls[ref_word == ggloss_tab[i, ref_word], value], collapse = "")
		ggloss_tab[i, value := tmp]
	}
	
	ggloss_tab[, c("i.value", "ref_word") := NULL]
	
	
	# -------------------------------------------------------------------------------- #
	
	# set value of utterance IDs
	segments_tab[, value := paste0("eibela_", .filename, "_", 
								   sprintf("%04d", seq(0, nrow(segments_tab) - 1, 1)))]
	
	# select only timeslots used for segments
	start_tab <- time_tab[time_id %in% segments_tab$start, ]
	end_tab <- time_tab[time_id %in% segments_tab$end, ]	
	
	# rename columns in tables for easier identification
	setnames(gwords_tab, 1:3, paste0("gwords_", names(gwords_tab)))
	setnames(ggloss_tab, 1:3, paste0("gloss_", names(ggloss_tab)))
	setnames(utterance_tab, 1:3, paste0("utterance_", names(utterance_tab)))
	setnames(segments_tab, 1:4, paste0("segments_", names(segments_tab)))
	setnames(translations_tab, 1:3, paste0("translations_", names(translations_tab)))
	setnames(start_tab, 1:2, paste0("start_", c("id", "value")))
	setnames(end_tab, 1:2, paste0("end_", c("id", "value")))
	
	
	# -------------------------------------------------------------------------------- #

	# set a key for preserving row order
	gwords_tab[, key := seq(0, nrow(gwords_tab) - 1, 1)]
	
	# merge tables by shared IDs and refs
	tiers <- merge(gwords_tab, ggloss_tab, by.x = "gwords_id", by.y = "gloss_ref", all = TRUE)
	tiers <- merge(tiers, utterance_tab, by.x = "gwords_ref", by.y = "utterance_id", all = TRUE)
	tiers <- merge(tiers, segments_tab, by.x = "utterance_ref", by.y = "segments_id", all = TRUE)	
	tiers <- merge(tiers, translations_tab, by.x = "utterance_ref", by.y = "translations_ref", all = TRUE)
	tiers <- merge(tiers, start_tab, by.x = "segments_start", by.y = "start_id", all = TRUE)
	tiers <- merge(tiers, end_tab, by.x = "segments_end", by.y = "end_id", all = TRUE)
	
	# reset row order
	setorder(tiers, key)
	
	# select columns
	ttab <- tiers[, c("start_value", "end_value",
					  "segments_value", "utterance_value", "translations_value",
					  "gwords_value", "gloss_value")]
	
	# rename columns
	setnames(ttab, 1:7, c("start", "end", "uid", "utterance", "translation", "gwords", "gloss"))
	
	# copy values of higher-level tiers to all encompassed rows
	ttab[, start := start[which(!is.na(start))], by = cumsum(!is.na(start))]
	ttab[, end := end[which(!is.na(end))], by = cumsum(!is.na(end))]
	ttab[, uid := uid[which(!is.na(uid))], by = cumsum(!is.na(uid))]
	ttab[, utterance := utterance[which(!is.na(utterance))], by = cumsum(!is.na(utterance))]
	ttab[, translation := translation[which(!is.na(translation))], by = cumsum(!is.na(translation))]
		

	# return table
	return(ttab)
}


# -------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------- #

# read a block of XML data corresponding to an EAF tier
readEAFtier <- function(.file, .tier, .timeAligned = FALSE) {
	
	# select between time-aligned and referential tiers
	if (.timeAligned) {
		# compose search pattern
		pattern <- paste0("//TIER[@TIER_ID='",
						  .tier,
						  "']/ANNOTATION/ALIGNABLE_ANNOTATION")
		
		# read XML data
		id <- xml_attr(xml_find_all(.file, pattern), "ANNOTATION_ID")
		start <- xml_attr(xml_find_all(.file, pattern), "TIME_SLOT_REF1")
		end <- xml_attr(xml_find_all(.file, pattern), "TIME_SLOT_REF2")
		value <- xml_text(xml_find_all(.file, paste0(pattern, "/ANNOTATION_VALUE")))

		# return as table
		return(data.table(id, start, end, value))
		
	} else {
		# compose search pattern
		pattern <- paste0("//TIER[@TIER_ID='",
						  .tier,
						  "']/ANNOTATION/REF_ANNOTATION")
		
		# read XML data
		id <- xml_attr(xml_find_all(.file, pattern), "ANNOTATION_ID")
		ref <- xml_attr(xml2::xml_find_all(.file, pattern), "ANNOTATION_REF")
		value <- xml_text(xml_find_all(.file, paste0(pattern, "/ANNOTATION_VALUE")))

		# return as table
		return(data.table(id, ref, value))
	}
}


# -------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------- #
# -------------------------------------------------------------------------------- #

# write intermediary table to file
writeEAF <- function(.x, .filename, .writeTo) {
	
	# collapse associated tiers
	gwords <- .x[, paste0(gwords, collapse = "$"), by = "uid"]
	gloss <- .x[, paste0(gloss, collapse = "$"), by = "uid"]
	tmp <- gwords[gloss, on = "uid"]
	setnames(tmp, 2:3, c("gwords", "gloss"))
	
	y <- unique(.x[, 1:5])[tmp, on = "uid"]

	
	# check locale, add trailing (back)slash
	if (grepl("/", .writeTo)) {
		if (substr(.writeTo, nchar(.writeTo), nchar(.writeTo)) != "/") {
			.writeTo <- paste0(.writeTo, "/")
		}
	} else {
		if (substr(.writeTo, nchar(.writeTo), nchar(.writeTo)) != "\\") {
			.writeTo <- paste0(.writeTo, "\\")
		}		
	}

	# compose output path
	output <- paste0(.writeTo, .filename, "_raw-text.txt")
	
	
	# write TSV file
	fwrite(y,
		   file = output,
		   sep = "\t",
		   quote = FALSE,
		   col.names = FALSE,
		   showProgress = FALSE,
		   verbose = FALSE)
}

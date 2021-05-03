
# install necessary packages
install.packages(c("data.table", "xml2"))

# load packages
library(data.table,
		xml2)

# load functions
source("eibela-functions.R")


# Use this function to convert all EAF files in the specified directory to TXT files
# NOTE: Tier names must be exact, and exactly the same in all files!
#       If need be, manually edit the EAF files to ensure conformity!

convertEAF(.tiernames <- c(segments = "A_phrase-segnum-en",         # segment IDs
						   translations = "A_phrase-gls-en",        # translations
						   words = "A_word-txt-qaa-fonipa-x-eib",   # unsegmented word units
						   morphs = "A_morph-txt-qaa-fonipa-x-eib", # segmented word units
						   glosses = "A_morph-gls-en"),             # morphological glosses
		   .readFrom <- getwd(),
		   .writeTo <- getwd())



# INSTRUCTIONS FOR IMPORTING THE TXT FILES INTO ELAN

#  1. In ELAN, go to 'File' -> 'Import' -> 'CSV / Tab-delimited Text File'
#  2. Open one of the exported TXT files
#  3. Set the first two columns to 'Begin Time' and 'End Time', and the rest to 'Annotation'
#  4. If it isn't already, set 'Specify delimiter' to 'Tab'

#  5. Go to 'Type' -> 'Add new tier type'
#  6. Rename type 'default' to 'time_alignment'
#  7. Add two new types:
#     'symbolic_association' with stereotype 'Symbolic Association', and
#     'symbolic_subdivision' with stereotype 'Symbolic Subdivision'

#  7. Rename 'Tier-0' to 'utterance_id'

#  8. Go to 'Tier' -> 'Copy Tier' and copy 'Tier-1' with new parent 'utterance_id'
#     and type 'symbolic_association'
#  9. Rename the new tier to 'utterance' and delete the old 'Tier-1'

# 10. Go to 'Tier' -> 'Add New Tier' and add two tiers (with any name) with 'Tier-3' and
#     'Tier-4' as parents and type 'symbolic_subdivision'
# 11. Go to 'Tier' -> 'Tokenize Tier', select 'Tier-3' as the source (the destination
#     tier should set itself automatically), specify '$' as the custom token delimiter, and
#	  click 'Start'
# 12. Do the same for 'Tier-4'

# 13. Copy the new tier with the tokenized object language words to be child of the
#     'utterance' tier, with tier type 'symbolic_subdivision' (!)
# 14. Rename this tier to 'grammatical_words'

# 14. Copy the new tier with the tokenized glosses to be the child of the 'grammatical_words'
#     tier, with the tier type 'symbolic_association' (!)
# 15. Rename this tier to 'gloss'

# 16. Delete tiers 'Tier-3' and 'Tier-4' and their children

# 17. Add three new tiers:
#     tier 'graid' as child of 'grammatical_words' with type 'symbolic_association'
#     tier 'refind' as child of 'graid' with type 'symbolic_association', and
#     tier 'isnref' as child of 'refind' with type 'symbolic_association'

# 18. Copy tier 'Tier-2' to be child of the 'utterance' tier with type 'symbolic_association'
# 19. Rename this tier to 'utterance_translation'
# 20. Delete tier 'Tier-2'


# Done! Now you've got a perfectly MC-conforming EAF file -- sans annotations.
# Once you get the hang of this, it shouldn't take more than a minute or two to prepare a text.

# If you like, you can add an additional tier 'add_comments' for comments (e.g. as a child of
# the 'graid' or 'utterance' tiers) and use 'Create Annotations on Dependent Tiers' on the
# 'graid', 'refind', and 'isnref' tiers so save yourself some clicking.

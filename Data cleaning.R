data <- eaf_ann

data <- gsub("\\:|\\;", "ː", data)

data <- sub("\\(.*\\)", "", data)
data <- gsub("<unintelligible>|<unclear>|<|>|-|--|->|\\?|\\.", "", data)
data <- gsub("\\(.*", "", data)
data <- data[!grepl('\\)|\n', data)]


data <- gsub(' ː| :', 'ː', data)

data <- gsub('ɪ ̆', 'ɪ̆', data)
data <- gsub('o ̆', 'ŏ', data)
data <- gsub('u ̆', 'ŭ', data)
data <- gsub('i ̆', 'ĭ', data)
data <- gsub('a ̆', 'ă', data)
data <- gsub('e ̆', 'ĕ', data)
data <- gsub('ə ̆', 'ə̆', data)
data <- gsub('ɔ ̆', 'ɔ̆', data)
data <- gsub('ɛ ̆', 'ɛ̆', data)

data <- gsub('ɛ ́', 'ɛ́', data)
data <- gsub('ɪ ́', 'ɪ́', data)
data <- gsub('o ́', 'ó', data)
data <- gsub('u ́', 'ú', data)
data <- gsub('i ́', 'í', data)
data <- gsub('a ́', 'á', data)
data <- gsub('e ́', 'é', data)
data <- gsub('ə ́', 'ə́', data)
data <- gsub('ɔ ́', 'ɔ́', data)

data <- gsub('ə ̃', 'ə̃', data)
data <- gsub('ɪ ̃', 'ɪ̃', data)
data <- gsub('o ̃', 'õ', data)
data <- gsub('u ̃', 'ũ', data)
data <- gsub('ɛ ̃', 'ɛ̃', data)
data <- gsub('a ̃', 'ã', data)
data <- gsub('e ̃', 'ẽ', data)
data <- gsub('ɔ ̃', 'ɔ̃', data)
data <- gsub('i ̃', 'ĩ', data)

data <- gsub('ə̃ ̆', 'ə̃̆', data)
data <- gsub('ɪ̃ ̆', 'ɪ̃̆', data)
data <- gsub('ã ̆', 'ã̆', data)
data <- gsub('õ ̆', 'õ̆', data)
data <- gsub('ũ ̆', 'ũ̆', data)
data <- gsub('ɛ̃ ̆', 'ɛ̃̆', data)
data <- gsub('ẽ ̆', 'ẽ̆', data)
data <- gsub('ɔ̃ ̆', 'ɔ̃̆', data)
data <- gsub('ĩ ̆', 'ĩ̆', data)

eaf_ann <- data
unique(allphones)
valid <- c("s", "m", "aː", "l", "g", "ɛː", "ɛ", "j", "i", "h", "oː", "uː", "d", "u", "k", "n", "w", "o", "b", "iː", "t", "ɛːː", "aːː", "ɸ", "v", "ɾ", "a", "ə", "ʃ", "p", "iːː", "oːː", "ŋ", "ʃː", "mː", "mːː", "β", "ŋːː", "ŋː", "x", "c", "r", "ʒːː", "ʒ", "uːː", "ʙ", "ɡ", "əː", "z", "f", "βːː", "βː")
allphones <- allphones[allphones %in% valid]

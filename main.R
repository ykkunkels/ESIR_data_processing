
######################################
### ESIR_data_processing           ###
### YKK, LH, BK, MP                ###
### 02/10/2024                     ###
### ~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~###


## 1. Load and / or Install required packages----
# if (!require("htmltools")) {
#   install.packages("htmltools", dep = TRUE)
# }




#! n. Add component: Processing raw output from RedCap---- 


#! n. Add component: Seperating and sanatizing author names & email addresses----


#! n. Add component: Adding tags----

# load packages
library(dplyr)
library(stringr)
library(ggplot2)

# load data
dat <- read.csv("ESM_Item_Rep_selection.csv") # this is the format you can download from the repository!
# means that when we finalize the script, the data should be in the final format
# for this bit of the code
dat_all <- dat # save the whole dataset
dat <- dat[which(dat$item_ID %in% 1:3317), ]  # work on a subset (items 1-3317);
# these items were in the repository before the tag system was implemented in 
# the submission form -> tags assigned by the repository team

# # clean and process the descriptions and items
# dat$description_clean <- dat %>%
#   mutate(description = str_to_lower(description),   # convert to lowercase
#          description = str_replace_all(description, "\\(.*?\\)", ""),   # remove content within parentheses
#          description = str_replace_all(description, "[-]", ""),   # remove hyphens and bring words together
#          description = str_replace_all(description, "[:;]", " "),   # replace colons and semicolons with spaces
#          description = str_replace_all(description, "\\s*/\\s*", " "),   # replace slashes with spaces
#          description = str_trim(description),   # trim leading and trailing spaces
#          description = str_replace_all(description, "\\s{2,}", " "),   # replace multiple spaces with one space
#          description = str_replace_all(description, "[\\(\\)\\[\\]]", "")) %>%  # remove singular parentheses and brackets
#   pull(description)
# dat <- dat %>% 
#   mutate(english_clean = str_to_lower(english))

# # automatic tag assignment
# tags <-
#   data.frame(
#     tag = c(
#       "activity",
#       "anxiety",
#       "appraisal",
#       "arousal",
#       "attention",
#       "avoidance",
#       "body image",
#       "cognition",
#       "coping",
#       "depression",
#       "emotion regulation",
#       "event",
#       "fatigue",
#       "identity",
#       "location",
#       "methodological",
#       "motivation",
#       "negative affect",
#       "physical health",
#       "pleasure",
#       "positive affect",
#       "psychosis",
#       "rumination",
#       "self-esteem",
#       "self-harm",
#       "sleep",
#       "social context",
#       "social interaction",
#       "social support",
#       "stress",
#       "substance use",
#       "suicidality",
#       "symptom",
#       "worry"
#     ),
#     search_words_item = c(
#       "activity;what are you doing;what am i doing",
#       "anxiety;anxious;nervous;worried;uneasy;distressed",
#       "appraisal;evaluation;experience of",
#       "arousal;alert;excited;awake;energy",
#       "attention;focus;concentration;mindful;distract",
#       "avoidance;avoid;ignore",
#       "body image;body;image;appearance;looks",
#       "cognition;cognitive;reasoning;memory;perception;attention;concentrat",
#       "coping;cope;dealing with;manage;managing;resilience",
#       "depression;depressed;mdd;major depressive disorder;mood disorder;hopelessness;melancholy;worthlessness",
#       "emotion regulation;regulation;regulate;emotion control",
#       "event;occur;happen;occasion",
#       "fatigue;tired;exhausted;drained;sleepy",
#       "identity",
#       "location;where are you;where am i;where;place;environment;setting",
#       "methodological;disturb;burden;careless responding;compliance",
#       "motivation;motivated;driven",
#       "negative affect;sad;angry;anxious;stressed;down;upset;irritated;frustrated;unhappy;blue;guilty",
#       "physical health;illness;injury;exercise;pain;hungry;hunger;discomfort;physiological",
#       "pleasure;anhedonia;enjoyment;reward",
#       "positive affect;happy;energetic;satisfied;calm;joyful;cheerful;content;excited;optimistic;relaxed;at ease;hopeful",
#       "psychosis;psychotic;hallucinate;hallucination;delusion;paranoia;paranoid;schizophren;reality;suspicious",
#       "rumination;ruminat;overthink;dwell;obsess",
#       "self-esteem;selfesteem;esteem;confiden;selfworth;worth;selfrespect;insecure",
#       "self-harm;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
#       "sleep;insomnia",
#       "social context;am i alone;are you alone;who am i with;who are you with;i am with others;i am in company;are you in company;are you with others",
#       "social interaction;interacting",
#       "social support;comfortable;valued;belong",
#       "stress;pressure'overwhelm;tense",
#       "substance use;substance;alcohol;cigarette;smoke;cannabis;drugs;caffeine;coffee;used",
#       "suicidality;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
#       "symptom",
#       "worry;worried;concerned;anxious;nervous;preoccupied;uneasy"
#     ),
#     search_words_description = c(
#       "activity;what are you doing;what am i doing;doing",
#       "anxiety;anxious;nervous;worried;uneasy;distressed",
#       "appraisal;evaluation;experience of",
#       "arousal;alertness;alert;excited;awake;energy",
#       "attention;focus;concentration;mindful;distract",
#       "avoidance;avoid;ignore",
#       "body image;body;image;appearance;looks",
#       "cognition;cognitive;reasoning;memory;perception;attention;concentrat",
#       "coping;cope;dealing with;manage;managing;resilience",
#       "depression;depressed;mdd;major depressive disorder;mood disorder;hopelessness;melancholy;worthlessness",
#       "emotion regulation;regulation;regulate;emotion control",
#       "event;occur;happen;occasion",
#       "fatigue;tired;exhausted;drained;sleepy",
#       "identity",
#       "location;where are you;where;place;environment;setting",
#       "methodological;disturb;burden;careless responding;compliance",
#       "motivation;motivated;driven",
#       "negative affect;sad;angry;anxious;stressed;down;upset;irritated;frustrated;unhappy;blue; guilty",
#       "physical health;illness;injury;exercise;pain;hungry;hunger;discomfort;physiological",
#       "pleasure;anhedonia;enjoyment;reward",
#       "positive affect;happy;energetic;satisfied;calm;joyful;cheerful;content;excited;optimistic;relaxed;at ease",
#       "psychosis;psychotic;hallucinate;hallucination;delusion;paranoia;paranoid;schizophren;reality;suspicious",
#       "rumination;ruminat;overthink;dwell;obsess",
#       "self-esteem;selfesteem;esteem;confiden;selfworth;worth;selfrespect;insecure",
#       "self-harm;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
#       "sleep;insomnia",
#       "social context",
#       "social interaction",
#       "social support;comfortable;valued;belong",
#       "stress;pressure;overwhelm;tense",
#       "substance use;substance;alcohol;cigarette;smoke;cannabis;drugs;caffeine;coffee;used",
#       "suicidality;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
#       "symptom",
#       "worry;worried;concerned;anxious;nervous;preoccupied;uneasy"
#     )
#   )
# dat$tag <- rep("", times = nrow(dat))
# for (i in 1:nrow(dat)) {
#   matched_tags <- c()
#   for (j in 1:nrow(tags)) {
#     search_terms_item <- unlist(strsplit(tags$search_words_item[j], ";"))
#     search_terms_description <- unlist(strsplit(tags$search_words_description[j], ";"))
#     match_in_english <- any(str_detect(dat$english_clean[i], regex(search_terms_item, ignore_case = TRUE)))
#     match_in_description <- any(str_detect(dat$description_clean[i], regex(search_terms_description, ignore_case = TRUE)))
#     if (match_in_english || match_in_description) {
#       matched_tags <- c(matched_tags, tags$tag[j])
#     }
#   }
#   if (dat$evening[i] %in% c("Everyday at 8PM", "x", "yes", "Yes")) {
#     matched_tags <- c(matched_tags, "evening")
#   }
#   if (dat$morning[i] %in% c("yes", "Yes", "x")) {
#     matched_tags <- c(matched_tags, "morning")
#   }
#   if (length(matched_tags) > 0) {
#     dat$tag[i] <- paste(matched_tags, collapse = ";")
#   }
# }

# manual tag assignment part 1: integrate Milla's corrections to automated ones
# check1 <- read.csv("ESIR_tags_R.csv")
# sum(grepl("; ", check1$tag_automated))
# sum(grepl(" ;", check1$tag_automated))
# sum(grepl("; ", check1$check1_tag_remove))
# sum(grepl(" ;", check1$check1_tag_remove)) # check there are no spaces
# check1$tag_automated_check1 <- rep(NA, times = nrow(check1)) # create new col
# # edit the tags
# for (i in 1:nrow(check1)) {
#   if (check1$check1_tag_remove[i] != "") {
#     remove_tags <-
#       str_split(string = check1$check1_tag_remove[i],
#                 pattern = ";")
#     automated_tags <-
#       str_split(string = check1$tag_automated[i], pattern = ";")
#     for (k in 1:length(remove_tags[[1]])) {
#       automated_tags[[1]] <-
#         automated_tags[[1]][-which(remove_tags[[1]][k] == automated_tags[[1]])]
#     }
#     check1$tag_automated_check1[i] <-
#       paste(automated_tags[[1]], collapse = ";")
#   }
#   if (check1$check1_tag_add[i] != "") {
#     add_tags <-
#       str_split(string = check1$check1_tag_add[i], pattern = ";")
#     automated_tags <-
#       str_split(string = check1$tag_automated_check1[i],
#                 pattern = ";")
#     for (j in 1:length(add_tags[[1]])) {
#       if (sum(add_tags[[1]][j] == automated_tags[[1]]) == 0) {
#         automated_tags[[1]][length(automated_tags[[1]]) + 1] <-
#           add_tags[[1]][j]
#       }
#     }
#     check1$tag_automated_check1[i] <-
#       paste(automated_tags[[1]], collapse = ";")
#   }
# }
# for (i in 1:nrow(check1)) {
#   if (is.na(check1$tag_automated_check1[i])) {
#     check1$tag_automated_check1[i] <- check1$tag_automated[i]
#   }
# }
# # check for random ; and spaces
# for (i in 1:nrow(check1)) {
#   if (grepl(check1$tag_automated_check1[i], " ")) {
#     check1$tag_automated_check1[i] <- gsub(" ", "", check1$tag_automated_check1[i])
#   }
#   if (substring(check1$tag_automated_check1[i], 1, 1) == ";") {
#     check1$tag_automated_check1[i] <- gsub(";", "", check1$tag_automated_check1[i])
#   }
# }
# check1 <- check1[, c(1:4, 12, 6:9, 5, 10:11)] # reorder
# write.csv(check1, file = "check1.csv")

# # manual tag assignment
# dat_manual <- read.csv("ESIR_tags_manual.csv")
# # columns
# # tag_automated = the tag assigned by the automated tag assignment code (see above)
# # check1_tag_remove = tags that should be removed from the automated tags according to checker 1 
# # check1_tag_add = tags that should be added to the automated tags according to checker 1
# # tag_automated_check1 = tags after checker 1's suggestions were implemented
# # check2_tag_remove = tags that should be removed from the automated tags according to checker 2
# # check2_tag_add = tags that should be added to the automated tags according to checker 2
# # check2_tag_new = completely new tags suggested by checker 2 (not in the tag list), not used here
# # edited by = checkers
# 
# # check that there are no spaces
# sum(grepl(" ; ", dat_manual$check2_tag_remove))
# sum(grepl(" ;", dat_manual$check2_tag_remove))
# sum(grepl("; ", dat_manual$check2_tag_remove))
# sum(grepl(" ; ", dat_manual$check2_tag_add))
# sum(grepl(" ;", dat_manual$check2_tag_add))
# sum(grepl("; ", dat_manual$check2_tag_add))
# sum(grepl(" ; ", dat_manual$tag_automated_check1))
# sum(grepl(" ;", dat_manual$tag_automated_check1))
# sum(grepl("; ", dat_manual$tag_automated_check1))
# dat_manual$check2_tag_add <- gsub("; ", ";", dat_manual$check2_tag_add)
# 
# # check that all tags are separated by ;
# fix_tag_format <- function(tag_column, valid_tags) {
#   sapply(tag_column, function(x) {
#     if (is.na(x) || x == "") return("") 
#         tag_parts <- unlist(strsplit(x, ";"))
#         tag_parts <- trimws(tag_parts[nzchar(tag_parts)])
#         valid_parts <- tag_parts[tag_parts %in% valid_tags]
#         return(paste(unique(valid_parts), collapse = ";"))
#   })
# }
# valid_tag_list <- tags$tag
# dat_manual$tag_automated_check1 <- fix_tag_format(dat_manual$tag_automated_check1, valid_tag_list)
# dat_manual$check2_tag_add <- fix_tag_format(dat_manual$check2_tag_add, valid_tag_list)
# dat_manual$check2_tag_remove <- fix_tag_format(dat_manual$check2_tag_remove, valid_tag_list)
# 
# 
# # create new column for final tags
# process_tags <- function(automated, remove, add) {
#   automated <- ifelse(is.na(automated) | automated == "", "", automated)
#   remove <- ifelse(is.na(remove) | remove == "", "", remove)
#   add <- ifelse(is.na(add) | add == "", "", add)
#   auto_tags <- if (automated != "") unlist(strsplit(automated, ";")) else character(0)
#   remove_tags <- if (remove != "") unlist(strsplit(remove, ";")) else character(0)
#   add_tags <- if (add != "") unlist(strsplit(add, ";")) else character(0)
#   tag_finals <- setdiff(auto_tags, remove_tags)
#   tag_finals <- unique(c(tag_finals, add_tags))
#   return(ifelse(length(tag_finals) > 0, paste(tag_finals, collapse = ";"), ""))
# }
# dat_manual$tag_final <- mapply(process_tags, 
#                                dat_manual$tag_automated_check1, 
#                                dat_manual$check2_tag_remove, 
#                                dat_manual$check2_tag_add)

# load tags
tags_plain <- read.csv("tags_plain.csv")

# add to the matching item IDs
dat <- merge(dat, tags_plain, by = "item_ID", all.x = TRUE)
colnames(dat)[colnames(dat) == "tag_final"] <- "tag"

# create frequency table
all_tags <- unlist(strsplit(dat$tag, ";"))
all_tags <- all_tags[nzchar(all_tags)]
tag_freq <- as.data.frame(table(all_tags))
colnames(tag_freq) <- c("Tag", "Frequency")
tag_freq <- tag_freq %>% arrange(desc(Frequency))

# visualize
ggplot(tag_freq, aes(x = reorder(Tag, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = Frequency), vjust = -0.3, color = "black", size = 3) +  # Add text above bars
  theme_minimal() +
  labs(title = "Tag Frequency", x = "Tag", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# create final dataframe
write.csv(dat, "ESM_Item_Rep_selection_tags.csv")




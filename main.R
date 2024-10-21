
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

# load data
dat <- read.csv("ESM_Item_Rep_selection.csv") # this is the format you can download from the repository!
# means that when we finalize the script, the data should be in the final format
# for this bit of the code

# clean and process the descriptions and items
dat$description_clean <- dat %>%
  mutate(description = str_to_lower(description),   # convert to lowercase
         description = str_replace_all(description, "\\(.*?\\)", ""),   # remove content within parentheses
         description = str_replace_all(description, "[-]", ""),   # remove hyphens and bring words together
         description = str_replace_all(description, "[:;]", " "),   # replace colons and semicolons with spaces
         description = str_replace_all(description, "\\s*/\\s*", " "),   # replace slashes with spaces
         description = str_trim(description),   # trim leading and trailing spaces
         description = str_replace_all(description, "\\s{2,}", " "),   # replace multiple spaces with one space
         description = str_replace_all(description, "[\\(\\)\\[\\]]", "")) %>%  # remove singular parentheses and brackets
  pull(description)
dat <- dat %>% 
  mutate(english_clean = str_to_lower(english))

# assign tags to items: tags were decided on by the ESM item repository team
tags <-
  data.frame(
    tag = c(
      "activity",
      "anxiety",
      "appraisal",
      "arousal",
      "attention",
      "avoidance",
      "body image",
      "cognition",
      "coping",
      "depression",
      "emotion regulation",
      "event",
      "fatigue",
      "identity",
      "location",
      "methodological",
      "motivation",
      "negative affect",
      "physical health",
      "pleasure",
      "positive affect",
      "psychosis",
      "rumination",
      "self-esteem",
      "self-harm",
      "sleep",
      "social context",
      "social interaction",
      "social support",
      "stress",
      "substance use",
      "suicidality",
      "symptom",
      "worry"
    ),
    search_words_item = c(
      "activity;what are you doing;what am i doing",
      "anxiety;anxious;nervous;worried;uneasy;distressed",
      "appraisal;evaluation;experience of",
      "arousal;alert;excited;awake;energy",
      "attention;focus;concentration;mindful;distract",
      "avoidance;avoid;ignore",
      "body image;body;image;appearance;looks",
      "cognition;cognitive;reasoning;memory;perception;attention;concentrat",
      "coping;cope;dealing with;manage;managing;resilience",
      "depression;depressed;mdd;major depressive disorder;mood disorder;hopelessness;melancholy;worthlessness",
      "emotion regulation;regulation;regulate;emotion control",
      "event;occur;happen;occasion",
      "fatigue;tired;exhausted;drained;sleepy",
      "identity",
      "location;where are you;where am i;where;place;environment;setting",
      "methodological;disturb;burden;careless responding;compliance",
      "motivation;motivated;driven",
      "negative affect;sad;angry;anxious;stressed;down;upset;irritated;frustrated;unhappy;blue;guilty",
      "physical health;illness;injury;exercise;pain;hungry;hunger;discomfort;physiological",
      "pleasure;anhedonia;enjoyment;reward",
      "positive affect;happy;energetic;satisfied;calm;joyful;cheerful;content;excited;optimistic;relaxed;at ease;hopeful",
      "psychosis;psychotic;hallucinate;hallucination;delusion;paranoia;paranoid;schizophren;reality;suspicious",
      "rumination;ruminat;overthink;dwell;obsess",
      "self-esteem;selfesteem;esteem;confiden;selfworth;worth;selfrespect;insecure",
      "self-harm;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
      "sleep;insomnia",
      "social context;am i alone;are you alone;who am i with;who are you with;i am with others;i am in company;are you in company;are you with others",
      "social interaction;interacting",
      "social support;comfortable;valued;belong",
      "stress;pressure'overwhelm;tense",
      "substance use;substance;alcohol;cigarette;smoke;cannabis;drugs;caffeine;coffee;used",
      "suicidality;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
      "symptom",
      "worry;worried;concerned;anxious;nervous;preoccupied;uneasy"
    ),
    search_words_description = c(
      "activity;what are you doing;what am i doing;doing",
      "anxiety;anxious;nervous;worried;uneasy;distressed",
      "appraisal;evaluation;experience of",
      "arousal;alertness;alert;excited;awake;energy",
      "attention;focus;concentration;mindful;distract",
      "avoidance;avoid;ignore",
      "body image;body;image;appearance;looks",
      "cognition;cognitive;reasoning;memory;perception;attention;concentrat",
      "coping;cope;dealing with;manage;managing;resilience",
      "depression;depressed;mdd;major depressive disorder;mood disorder;hopelessness;melancholy;worthlessness",
      "emotion regulation;regulation;regulate;emotion control",
      "event;occur;happen;occasion",
      "fatigue;tired;exhausted;drained;sleepy",
      "identity",
      "location;where are you;where;place;environment;setting",
      "methodological;disturb;burden;careless responding;compliance",
      "motivation;motivated;driven",
      "negative affect;sad;angry;anxious;stressed;down;upset;irritated;frustrated;unhappy;blue; guilty",
      "physical health;illness;injury;exercise;pain;hungry;hunger;discomfort;physiological",
      "pleasure;anhedonia;enjoyment;reward",
      "positive affect;happy;energetic;satisfied;calm;joyful;cheerful;content;excited;optimistic;relaxed;at ease",
      "psychosis;psychotic;hallucinate;hallucination;delusion;paranoia;paranoid;schizophren;reality;suspicious",
      "rumination;ruminat;overthink;dwell;obsess",
      "self-esteem;selfesteem;esteem;confiden;selfworth;worth;selfrespect;insecure",
      "self-harm;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
      "sleep;insomnia",
      "social context",
      "social interaction",
      "social support;comfortable;valued;belong",
      "stress;pressure;overwhelm;tense",
      "substance use;substance;alcohol;cigarette;smoke;cannabis;drugs;caffeine;coffee;used",
      "suicidality;selfharm;harm;suicide;suicidality;suicidal;cutting;selfinjury;injury",
      "symptom",
      "worry;worried;concerned;anxious;nervous;preoccupied;uneasy"
    )
  )
dat$tag <- rep("", times = nrow(dat))
for (i in 1:nrow(dat)) {
  matched_tags <- c()
  for (j in 1:nrow(tags)) {
    search_terms_item <- unlist(strsplit(tags$search_words_item[j], ";"))
    search_terms_description <- unlist(strsplit(tags$search_words_description[j], ";"))
    match_in_english <- any(str_detect(dat$english_clean[i], regex(search_terms_item, ignore_case = TRUE)))
    match_in_description <- any(str_detect(dat$description_clean[i], regex(search_terms_description, ignore_case = TRUE)))
    if (match_in_english || match_in_description) {
      matched_tags <- c(matched_tags, tags$tag[j])
    }
  }
  if (dat$evening[i] %in% c("Everyday at 8PM", "x", "yes", "Yes")) {
    matched_tags <- c(matched_tags, "evening")
  }
  if (dat$morning[i] %in% c("yes", "Yes", "x")) {
    matched_tags <- c(matched_tags, "morning")
  }
  if (length(matched_tags) > 0) {
    dat$tag[i] <- paste(matched_tags, collapse = ";")
  }
}

# TO ADD: tags we assign manually 

# TO ADD: remove extra columns

# create final dataframe
dat_final <- dat %>%
  select(item_ID, label, english, description, tag)
write.csv(dat_final, "ESIR_tags_automatic.csv")




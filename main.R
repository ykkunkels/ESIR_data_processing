
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

# load tags
tags_plain <- read.csv("tags_plain.csv")

# add to the matching item IDs
dat <- merge(dat, tags_plain, by = "item_ID", all.x = TRUE)
colnames(dat)[colnames(dat) == "tag_final"] <- "tag"
dat$tag[dat$tag == ""] <- NA
# note that any further rows will automatically be assigned NA for the tags

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




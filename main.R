
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


#! n. Adding tags----

# load packages
library(dplyr)
library(stringr)
library(ggplot2)

# load data
dat <- read.csv("ESM_Item_Rep_selection.csv") # this is the format you can download from the repository!
# THIS NEEDS TO CHANGE?

# load tags
tags_plain <- read.csv("tags_plain.csv")

# add to the matching item IDs
dat <- merge(dat, tags_plain, by = "item_ID", all.x = TRUE)
colnames(dat)[colnames(dat) == "tag_final"] <- "tag"
dat$tag[dat$tag == ""] <- NA
# note that any further rows will automatically be assigned NA for the tags


#! n. Clean scale columns----
dat$scale_type <- rep(NA, times = nrow(dat))

      


# create final dataframe
write.csv(dat, "ESM_Item_Rep_selection_tags.csv")

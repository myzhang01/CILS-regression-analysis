
library(tidyverse)
library(sjlabelled)
library(dplyr)

load('ICPSR_20520/DS0001/20520-0001-Data.rda')

cils <- as_tibble(da20520.0001)

labels <- attributes(cils)$variable.labels

cils <- remove_all_labels(cils)

numeric <- cils %>% 
  select(!where(is.factor))

nonnumeric <- cils %>% 
  select(where(is.factor))

test <- nonnumeric %>% 
  apply(MARGIN = 2, FUN = function(x) length(unique(x)))

unordered <- nonnumeric[test > 6]
ordered <- nonnumeric[test <= 6]

ordered_num <- ordered %>% 
  lapply(FUN = as.integer) %>% 
  lapply(FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))

#Need Debate on V5, V7, V18, V42, V206, P67, P74C, P76, C8, C9, V430 series
#Some of the column at the end repeat
unordered_from_ordered <- select(ordered, V2, V5, V7, V18, V34A, V39A, V42,
                                 V88, V89, V90, V97, V111A, V111B, V112A,
                                 V112B, V113A, V113B, V147, V206, V237, 
                                 V242, V297, P2A, P4, P12, P13, P14, P15, 
                                 P37, P38, P40, P42, P45B, P50, P59A, P61, 
                                 P74A, P74C, P76, P82, P140, P142, C1,
                                 C8, C9, C10, C11, C12, C13, V416, V417, 
                                 V418, V425, V430B, V430C, V430D, V430E,
                                 V431, V432, V436, V438, V441, V447, V443,
                                 V442)

#need Debate on V4, V205, V414B, V415B, V440
ordered_from_unordered <- select(unordered, V29A, V29C, V29D, V29E, V30, V31, 
                                 V94, V205, V236, V241, V294, V296, P56, P134,
                                 V414B, V415B, V422, V440, V446)






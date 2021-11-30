
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



-----------------------
# Sam's part - dealing with all the unordered data

# Use the correct variable labels after subsetting. 
var_labs_cils <- attr(cils, "variable.labels")
var_labs_unordered <- var_labs_cils[names(unordered)]
attr(unordered, "variable.labels") <- var_labs_unordered


# Consider converting the following columns to dummies: 
dummies = unordered %>%  subset(select = c(V34B, V46, V228, V266,V295,V323,P3,P30,P53A,P65,P81))
print(unname(var_labs_cils[names(dummies)])) # print all the variable labels to discuss with Max and Isaac.

# Consider converting the following columns to dummies as a group: 
dummies_language = unordered %>% subset(select = c(V50,V56,V58,V59,V76, V250, V256 ,V258,V259,V276, P26A,P26B,P27A,P26B,P27A,P27B))
print(unname(var_labs_cils[names(dummies_language)]))

dummies_reason_come_to_US = unordered %>% subset(select = c(V66,V69))
print(unname(var_labs_cils[names(dummies_reason_come_to_US)])) #? is there a way to use %>% 

# Consider converting them to binary variables. 
binary = unordered %>% subset(select = c(V87,V246,V287, P34, P48, P68A, P70,P71))
print(unname(var_labs_cils[names(binary)])) 

# Consider converting them to ordered variables. 
unordered_to_ordered_sam = unordered %>% subset(select = c(V29A, V29C, V29D, V29E, V30, V31, V36, V41, V94, V96  ,V236 ,V294 ,V296 ,P17 ,P31 ,P47, P56, P72 ,P132 ,P134))
print(unname(var_labs_cils[names(unordered_to_ordered_sam)])) 

# Substituting occupations using numeric prestige score: 
occupations_to_scores = unordered %>% subset(select = c(V32, V37, V233, V238, V263, V62, V64, V264))
print(unname(var_labs_cils[names(occupations_to_scores)])) 

# Substituting using continents or using V278: 
countries_to_continents = unordered %>% subset(select = c(V9 ,V15 ,V21 ,V21A , P6, P7 ,P16, P19, P33A))
print(unname(var_labs_cils[names(countries_to_continents)])) 

# Other  Substitutions 
select_two = unordered %>% subset(select = c(P35,P36,P49,P51,V323A,V323))
print(unname(var_labs_cils[names(select_two)])) 

use_constructed_variables = unordered %>% subset(select = c(V50, V56,  V58, V59))
print(unname(var_labs_cils[names(use_constructed_variables)])) 

# Consider dropping: 
not_informative = unordered %>% subset(select = c(V4,V204,V205,P51))
print(unname(var_labs_cils[names(select_two)])) 

mostly_NA=  unordered %>% subset(select = c(P39A, P41A, P43A ,P45A, P46 ,P53B, P53C, P83, P85))
print(unname(var_labs_cils[names(mostly_NA)])) 

# Discuss: V28,  (V8, V14 > dummies)


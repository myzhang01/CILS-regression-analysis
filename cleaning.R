
library(tidyverse)
library(sjlabelled)  # for remove_all_labels()


# Import ------------------------------------------------------------------

load('ICPSR_20520/DS0001/20520-0001-Data.rda')

labels <- attributes(da20520.0001)$variable.labels

cils <- as_tibble(da20520.0001) %>%
  filter(!is.na(V421)) %>%  # remove all observations with missing income
  remove_all_labels() %>%
  select(!matches('V4[[:alnum:]]{2,}'))  # remove 2005 variables
cils_inc_missing <- as_tibble(da20520.0001) %>%
  filter(is.na(V421))


# Deal with numeric variables ---------------------------------------------

numeric <- cils %>%
  select(where(is.double)) %>%
  select(!CASEID)

numeric_na <- numeric %>%
  lapply(FUN = function(x) sum(is.na(x)) / length(x)) %>%
  unlist()
numeric_na <- names(numeric_na[numeric_na >= 0.5])  # the 0.5 is the threshold for removing NAs

numeric <- numeric %>%
  select(!all_of(numeric_na)) %>%
  mutate(across(.cols = everything(), is.na, .names = '{.col}_NA')) %>%
  mutate(across(.cols = ends_with('_NA'), as.double)) %>%
  # mean imputation: for all columns not ending with NA, replace missing value
  # with the mean of the column
  mutate(across(.cols = !ends_with('_NA'), ~if_else(is.na(.), mean(., na.rm = TRUE), .)))


# Deal with non-numeric variables -----------------------------------------

nonnumeric <- cils %>%
  select(where(is.factor))

nonnumeric_na <- nonnumeric %>%
  lapply(FUN = function(x) sum(is.na(x)) / length(x)) %>%
  unlist()
nonnumeric_na <- names(nonnumeric_na[nonnumeric_na >= 0.5])

nonnumeric <- nonnumeric %>%
  select(!all_of(nonnumeric_na)) %>%
  mutate(across(.cols = everything(), addNA))

nonnumeric_mat <- model.matrix(~., data = nonnumeric)[ ,-1]



# older code below --------------------------------------------------------

unordered <- nonnumeric %>%
  select(where(function(x) nlevels(x) > 6))  # does not count NA as a level,
                                             # therefore only 90 vars selected
                                             # instead of 98 in previous code
ordered <- nonnumeric %>%
  select(where(function(x) nlevels(x) <= 6))

#ordered_num <- ordered %>%
#  lapply(FUN = as.integer) %>%
#  lapply(FUN = function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))


# Removing mostly NA columns ----------------------------------------------

na_proportions <- da20520.0001 %>%
  #summarize(across(.fns = function(x) sum(is.na(x)) / length(x)))
  lapply(FUN = function(x) sum(is.na(x)) / length(x))
names(na_proportions) <- labels

# Fix unordered and ordered -----------------------------------------------

#Need Debate on V5, V7, V18, V42, V206, P67, P74C, P76, C8, C9, V430 series
#Some of the column at the end repeat
false_ordered <- ordered %>%
  select(V2,    V5,    V7,    V18,   V34A,
         V39A,  V42,   V88,   V89,   V90,
         V97,   V111A, V111B, V112A, V112B,
         V113A, V113B, V147,  V206,  V237,
         V242,  V297,  P2A,   P4,    P12,
         P13,   P14,   P15,   P37,   P38,
         P40,   P42,   P45B,  P50,   P59A,
         P61,   P74A,  P74C,  P76,   P82,
         P140,  P142,  C1,    C8,    C9,
         C10,   C11,   C12,   C13)
#V416, V417, V418, V425, V430B, V430C, V430D,
#V430E, V431, V432, V436, V438, V441, V447,
#V443, V442

#need Debate on V4, V205, V414B, V415B, V440
false_unordered <- unordered %>%
  select(V29A, V29C, V29D, V29E, V30,
         V31,  V205, V236, V241, P56,  # V94, V294, V296 correctly caught by line 24
         P134)
#V414B, V415B, V422, V440, V446


# Dealing with unordered variables ----------------------------------------

# relabel unordered variables
labels_unordered <- labels[names(unordered)]
attr(unordered, "variable.labels") <- labels_unordered

# Consider converting the following columns to dummies:
dummies <- unordered %>%
  select(V34B, V46, V228, V266, V295, V323, P3, P30, P53A, P65, P81)
labels[names(dummies)]  # print dummy variable names

# Consider converting the following columns to dummies as a group:
dummies_language <- unordered %>%
  select(V50, V56, V58, V59, V76, V250, V256, V258, V259, V276, P26A, P26B,
         P27A, P26B, P27A, P27B)
labels[names(dummies_language)]

dummies_reason_come_to_US <- unordered %>%
  select(V66, V69)
labels[names(dummies_reason_come_to_US)]

# Consider converting them to binary variables.
binary <- unordered %>%
  select(V87, V246, V287, P34, P48, P68A) # P70, P71 never selected, they have 6 levels
labels[names(binary)]

# Consider converting them to ordered variables.
false_unordered_sam <- unordered %>%
  select(V29A, V29C, V29D, V29E, V30, V31, V236,  # V94, V96, V294, V296 already correctly caught
         P17, P31, P47, P56, P132, P134)     # P72 never selected, 6 levels
labels[names(false_unordered_sam)]

# Substituting occupations using numeric prestige score:
occupations_to_scores <- unordered %>%
  select(V32, V37, V233, V238, V263, V62, V64, V264)
labels[names(occupations_to_scores)]

# Substituting using continents or using V278:
countries_to_continents <- unordered %>%
  select(V9 ,V15 ,V21 ,V21A , P6, P7 ,P16, P19, P33A)
labels[names(countries_to_continents)]

# Other Substitutions
select_two <- unordered %>%
  select(P35, P36, P49, P51, V323A, V323)
labels[names(select_two)]

use_constructed_variables <- unordered %>%
  select(V50, V56, V58, V59)
labels[names(use_constructed_variables)]

# Consider dropping:
not_informative <- unordered %>%
  select(V4,V204,V205,P51)
labels[names(not_informative)]

mostly_NA <- unordered %>%
  select(P39A, P41A, P43A, P45A, P46, P53B, P53C, P83, P85)
labels[names(mostly_NA)]

# Discuss: V28,
# (V8, V14 > dummies)


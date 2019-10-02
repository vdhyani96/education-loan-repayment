library(dplyr)

setwd("C:/Users/admin/Desktop/R/Coursera - Predicting Student Loan Repayment/Dataset")

# read the file most recent cohorts (2014_15_PP)
schoolDf <- read.csv("Most-Recent-Cohorts-All-Data-Elements.csv")

# I will ignore the features representing share of male and female students, i.e., UGDS_MEN, UGDS_WOMEN
# Well if both are complementary, I think to use share of female, UGDS_WOMEN
# I will also select Institute Name

schoolDf_sub <- schoolDf %>% select(INSTNM, STATE = STABBR, CONTROL, HIGHDEG,
                                    CSDEG = PCIP11, COSTA = COSTT4_A, COSTP = COSTT4_P,
                                    UGDS, UGDS_WOMEN, UGDS_NRA, AGE_ENTRY, COMP_ORIG_YR4_RT,
                                    MEDIAN_HH_INC, PCTFLOAN, MD_EARN_WNE_P8, GRAD_DEBT_MDN, CDR3, COMPL_RPY_3YR_RT)

# First, I will redefine some features: Control and Highdeg

controlTable <- data.frame(CONTROL = c(1, 2, 3), OWNERSHIP = c("Public", "Private nonprofit", "Private for-profit"))

degTable <- data.frame(HIGHDEG = c(0, 1, 2, 3, 4), Degree = c("Non-degree-granting", "Certificate degree", "Associate degree",
                                                              "Bachelor's degree", "Graduate degree"))

# Now Inner join with the subset of school Df

schoolDf_sub2 <- inner_join(x = schoolDf_sub, y = controlTable)

# and once more

schoolDf_sub2 <- inner_join(x = schoolDf_sub2, y = degTable)

# now copy and paste the newly added features to CONTROL and HIGHDEG and remove the last two features afterwards

schoolDf_sub2$CONTROL <- schoolDf_sub2$OWNERSHIP
schoolDf_sub2$HIGHDEG <- schoolDf_sub2$Degree
schoolDf_sub2 <- schoolDf_sub2[, c(-19, -20)] # that's how we exclude certain columns

# now, view the structure

str(schoolDf_sub2)
summary(schoolDf_sub2)

# Now, will operate on COSTA and COSTP
# first it is required to convert factor to character before converting to numeric
schoolDf_sub2$COSTA <- as.numeric(as.character(schoolDf_sub2$COSTA))
schoolDf_sub2$COSTP <- as.numeric(as.character(schoolDf_sub2$COSTP))

# now, do the thing
schoolDf_sub2$COSTA[is.na(schoolDf_sub2$COSTA) & !is.na(schoolDf_sub2$COSTP)] <- 0

schoolDf_sub2$COSTP[is.na(schoolDf_sub2$COSTP) & !is.na(schoolDf_sub2$COSTA)] <- 0

# now use mutate to create a feature for the cost of attendance (FEATURE ENGINEERING)
# Now, I learned about the transmute() function that simplifies this task.

schoolDf_cost <- schoolDf_sub2 %>% mutate(ATDCOST = COSTA + COSTP)

summary(schoolDf_cost$ATDCOST)

# remove the old cost features, proceeding this way
schoolDf_cost2 <- schoolDf_cost[, c(1:5, 19, 8:18)] # also moved the new feature to correct place

# lets view the dataset upto now
summary(schoolDf_cost2)

# we observe here two features, CSDEG and UGDS_NRA, each having a large number of 0s. If these were
# categorical data, there'd no problem, but they are actually numeric and so they wouldn't serve us
# much as predictors. So, let's get rid of them.

schoolDf2 <- schoolDf_cost2[, c(-5, -9)]

# after some initial refinements, let's go ahead for more
summary(schoolDf2)

# some institutes seem like having duplicate entries. So clear them.
schoolDf2 <- unique(schoolDf2) # 13 entries cleared; now 16
summary(schoolDf2)

# 1. Remove those rows that have pctloan = 0 & grad median debt = privacy suppressed and repayment rate = null | (probably non-random)
# 2. remove those rows that have grad_debt_median = null and pctloan = 0 or null
# 3. Convert numerical type columns into numeric

# can also use subset instead of filter -- everything else will remain same
schoolDf2_sub <- schoolDf2 %>% filter(!(PCTFLOAN == "0" & (GRAD_DEBT_MDN == "PrivacySuppressed" | GRAD_DEBT_MDN == "NULL") 
                                        & (COMPL_RPY_3YR_RT == "NULL" | COMPL_RPY_3YR_RT == "PrivacySuppressed")))
summary(schoolDf2_sub)

# INSTNM still seems to have many duplicate entries for institutes
InsTable <- schoolDf2_sub$INSTNM %>% table() %>% sort(decreasing = TRUE) %>% data.frame()

# Moreover, I just realized that the levels are showing for those INSTNM too that are already dropped when
# I filtered them out a few lines above. I can do something like below to drop the unnecessary levels in all factor variables
schoolDf2_sub <- droplevels(schoolDf2_sub)
str(schoolDf2_sub)
# and again
InsTable <- schoolDf2_sub$INSTNM %>% table() %>% sort(decreasing = TRUE) %>% data.frame()


# below codes are just to check if PRivacySuppressed and NUll convert to NA after changing to numeric or not. They do.
temp <- schoolDf2_sub
temp$Repayment <- schoolDf2_sub$COMPL_RPY_3YR_RT %>% as.character() %>% as.numeric()
# Simply remove temp now
rm(temp)
###################################################################

# at this time or maybe before, I may like to remove big variables from the workspace such as schoolDf as well


# Upon inspection, I found that there are many rows that have too many NAs in different columns that maybe used in prediction. 
# I can make a new feature containing the no. of occurences of NA values in each row. Then I can remove
# those rows that have too many NAs (saw tip in StackOverflow)
# UPDATE: I can also bolster my inspection result by providing a visualization, like missing map function
#         using the Amelia package, as done in the Kaggle script of mice imputation
# let's move all of this (comments and also the visualization) to the new file missingness.R


# For achieving that, I will first have to convert all factors to numeric form

str(schoolDf2_sub)
summary(schoolDf2_sub)

# set of features to be converted to numeric
toNumeric <- names(schoolDf2_sub[, -c(1:5)])

nextSchoolDf <- schoolDf2_sub

# lapply(), as the LHS demands list!
nextSchoolDf[toNumeric] <- nextSchoolDf[toNumeric] %>% lapply(FUN = function(x) { as.numeric(as.character(x)) })
# too many warnings, NAs introduced by coercion, good!

str(nextSchoolDf)
summary(nextSchoolDf)

# simply write this dataset now into a CSV file and continue operating on it in the next R script
# which will be called "missingness.R"

getwd()
write.csv(nextSchoolDf, file = "mungRang.csv", row.names = FALSE)





#####################################XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX#########################################






# Let's see how many NA's does every feature carry, in a nicer form ----->>>>> Not here, not now
# using sapply because I want a vector back in the LHS
schoolDf_cost2 %>% sapply(FUN = function(x) { sum(x == "NULL")})
print(NAvals)

# NA represents a missing value, Null doesn't. So, replace null with NA.




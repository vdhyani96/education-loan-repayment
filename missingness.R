library(dplyr)
# install.packages("Amelia")
library(Amelia)  # for missmap() function, a visualization of missingness
library(ggplot2)
library(mice)
library(lattice)
library(gridExtra)

setwd("C:/Users/admin/Desktop/R/Coursera - Predicting Student Loan Repayment/Dataset")

nextSchoolDf <- read.csv("mungRang.csv")
str(nextSchoolDf)
summary(nextSchoolDf)

# first of all, let's show the Amelia package missingness visualization
missmap(nextSchoolDf, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)
# So, this is the map of missingness in our dataset. We can see some horizontal lines stretching
# across from one direction to another. These are the rows that contain many NA values. We will try
# to get rid of those rows. One more thing we can observe from the visualization is that a few 
# features have contiguous NAs in the lower part of the dataset. 


# attaching a new feature countNA that stores the number of NAs in each row
# rowSums is used here to find the sum of occurences of NAs across each row
nextSchoolDf$countNA <- rowSums(is.na(nextSchoolDf))

# most probably, the first 4 features don't have any NAs as can be seen in the summary. 
# So, we'll move ahead with the fact that only the rest 11 features can have NAs.
# by the way, I can't use INSTNM as predictor, that leaves only 14 features to model. There is
# some problem with using STATE as well, as there are 54 levels, which is not allowed in Random Forest

# Now, we're gonna tabulate the countNA feature
misRows <- data.frame(table(nextSchoolDf$countNA))
names(misRows) <- c("NACount", "Freq")
# Now, let's also calculate the cumulative frequency
misRows$CumFreq <- cumsum(misRows$Freq)

# visualize the NA counts and CumFreq (bar or area plots)
# turns out, I need to mention group = 1 if my NACount is factor, i.e.
# ggplot(misRows, aes(x = NACount, y = CumFreq, group = 1)). group = 1 means all the factor values 
# are in the same group and while creating the plot, just join the points together. We can convert it also
# to numeric; then, group won't be required. But, it doesn't look good with numeric. 
# The X-axis has now points showing 6.5 as NACount. Let's revert back, from as.numeric(NACount)
# So, remember, the x variable should be a numeric and not factor for geom_area
# btw, geom_line seems more helpful than geom_area
ggplot(misRows, aes(x = NACount, y = CumFreq, group = 1)) +
  geom_line() +
  geom_point(size = 2) + 
  geom_text(aes(label = CumFreq), hjust = 0, vjust = 1.5)


# Our goal basically is to allow as less NAs as possible in a row by deleting those that have many NAs while
# also keeping as many observations as possible in the dataset.
# As we can see from the plot, upto 5 the slope is quite good. From 6, the plot 
# seems to be saturating. I can let 5 NAs be my threshold that I want to keep.
# The rest I can get rid of. They are probably lost causes. Those having NACount >= 6 
# are the long horizontal lines that we saw in the missmap. 

noLinesDf <- nextSchoolDf %>% filter(countNA <= 5)
# 6485 rows

# Let's again create a missmap with this new dataset
missmap(noLinesDf, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8)
summary(noLinesDf)

# again, plotting missmaps together
par(mfrow = c(1,2))
missmap(nextSchoolDf, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8, main = "Before")
missmap(noLinesDf, col=c('grey', 'steelblue'), y.cex=0.5, x.cex=0.8, main = "After")


# Now we are about to go for MICE imputation. Before doing that
# I will create a new feature called INST_ENSIZE which is going to be an
# institution feature. It will be derived from the continuous feature UGDS. UGDS directly can't
# be used effectively as a predictor because it's an absolute value (not relative like %age) 
# and the number of degree seeking students enrolling can vary hugely
# across different school. (can use boxplot to show) INST_ENSIZE will be a categorical feature.

# NO LET'S MICE IMPUTE THE MISSING VALUES FIRST AND THEN CREATE INST_ENSIZE. MICE FOR 
# CATEGORY WILL BE DIFFICULT AFTER CREATING THIS NEW FEATURE. 
# NO!!!!!!!!!!!! NOT DIFFICULT AT ALL! METHOD = CART



ggplot(noLinesDf, aes(x = "UGDS", y = UGDS)) + 
  geom_boxplot() +
  ggtitle("Distribution of enrollment at different schools")
# from the boxplot, it is evident that the range of values for UGDS is extremely big. There 
# are also many outliers present in the feature. We'll break UGDS down into categories to overcome 
# the impact of outliers on the predictive model that we're gonna build. 

# first plot a histogram of UGDS to determine how many categories to break 
# it into.

ggplot(noLinesDf, aes(x = UGDS)) + 
  geom_histogram()
# we can see from here that the distribution of UGDS is positively skewed, or skewed to the right.
# (also bookmarked).

summary(noLinesDf$UGDS)
# since mean > median, it is definitely highly skewed to the right ()

# let's do some counting first before making categories

sum(noLinesDf$UGDS <= 136, na.rm = TRUE)
# 1549; NAs were causing trouble in sum function, so removed them from addition.

sum(noLinesDf$UGDS >= 20000, na.rm = TRUE)
# 145

# Let's categorize logically in this way:
# 0 - 150 (near the 1st Qt.) -- Very Small
# 151 - 500 (near the Median) -- Small
# 501 - 2500 (near the 3rd Qt.) -- Medium
# 2501 - 15000 -- Large
# 15001 and above (only 243 obsv) -- Very Large 

noLinesDf$INST_ENSIZE <- NA

noLinesDf$INST_ENSIZE[noLinesDf$UGDS <= 150] <- "Very Small"

noLinesDf$INST_ENSIZE[noLinesDf$UGDS > 150 & noLinesDf$UGDS <= 500] <- "Small"

noLinesDf$INST_ENSIZE[noLinesDf$UGDS > 500 & noLinesDf$UGDS <= 2500] <- "Medium"

noLinesDf$INST_ENSIZE[noLinesDf$UGDS > 2500 & noLinesDf$UGDS <= 15000] <- "Large"

noLinesDf$INST_ENSIZE[noLinesDf$UGDS > 15000] <- "Very Large"

noLinesDf$INST_ENSIZE <- as.factor(noLinesDf$INST_ENSIZE)
summary(noLinesDf$INST_ENSIZE)

# before confirming and deleting UGDS, visualize using barplot

ggplot(noLinesDf, aes(x = INST_ENSIZE, fill = INST_ENSIZE)) + 
  geom_bar(alpha = 0.5) + 
  xlab("Institute Enrollment Size")

# fair enough! Let's replace UGDS with INST_ENSIZE

sizeSchoolDf <- noLinesDf[, c(1:4, 17, 5:16)]

# let's not do this; let's retain the UGDS column, but will not use as a predictor, NOTTTT!!!!
# sizeSchoolDf$UGDS <- sizeSchoolDf$INST_ENSIZE
# sizeSchoolDf <- sizeSchoolDf[, c(1:16)]
# colnames(sizeSchoolDf)[6] <- "INST_ENSIZE"

# maybe I should've first MICE imputed and then created this feature, since NAs in all numeric features
# are imputed all together --- NOTTTTT

# this dataframe to go forward, not sizeSchoolDf
miceImpDf <- noLinesDf

miceImpDf$UGDS <- miceImpDf$INST_ENSIZE
miceImpDf <- miceImpDf[, c(1:16)]
colnames(miceImpDf)[6] <- "INST_ENSIZE"

str(miceImpDf)
summary(miceImpDf)
# will exclude the following features from mice:
# INSTNM, countNA, COMPL_RPY_3YR_RT

# Can go 2 different ways from here:
# 1. Exclude the abovementioned features, perform mice and rejoin features and proceed as planned before.
# 2. Impute Repayment rate too, and then later break into training and test, now test could be verified.

# 1. excluding...

# set a random seed

set.seed(165)
mice_mod <- mice(miceImpDf[, !names(miceImpDf) %in% c("INSTNM", "countNA", "COMPL_RPY_3YR_RT")], 
                 method = "cart")
# cart can be used for both categorical and continuous data

# inspect the imputed dataset
mice_mod
mice_mod$imp$AGE_ENTRY

# let's inspect using plots
densityplot(mice_mod)    # from the lattice package
# we expect the distributions to be similar(though not identical); which they seem to be
# so complete imputation

mice_output <- complete(mice_mod)
summary(mice_output)

# let's place this into the same dataframe
verifyMice <- miceImpDf
verifyMice[, c(2:14)] <- mice_output
summary(verifyMice)


# visualize the categorical variable INST_ENSIZE
plot1 <- ggplot(miceImpDf, aes(x = INST_ENSIZE, fill = INST_ENSIZE)) +
  geom_bar(alpha = 0.5) + 
  ggtitle("Before Imputation")

plot2 <- ggplot(verifyMice, aes(x = INST_ENSIZE, fill = INST_ENSIZE)) +
  geom_bar(alpha = 0.5) + 
  ggtitle("After Imputation")

grid.arrange(plot1, plot2, nrow = 1, ncol = 2)     # from the gridExtra package
# pretty nice!

par(las = 2, mfrow = c(1,2))
boxplot(miceImpDf[, c(5, 7:14)])
boxplot(verifyMice[, c(5, 7:14)])
# nothing looks out of ordinary when comparing

# finally, I have my imputed dataset!
imputedDf <- verifyMice[, -16]
str(imputedDf)
summary(imputedDf)

# now I can work on building a model after shuffling the rows and splitting 
set.seed(243)
imputedDf2 <- imputedDf[sample(nrow(imputedDf)), ]

# now split into train and test sets ;)
# train <- imputedDf2 %>% filter(!is.na(COMPL_RPY_3YR_RT))
# test <- imputedDf2 %>% filter(is.na(COMPL_RPY_3YR_RT))


# write into individual CSV files!
# write.csv(train, file = "CSCtrain.csv", row.names = FALSE)
# write.csv(test, file = "CSCtest.csv", row.names = FALSE)


# NOTE:- Now both the divisions will have all the factor levels which is desirable here. 
# Recall from Trevor's tutorial! 
# BUT!!!!!! After reading them back, I think all the levels will disappear. 
# So, I can either (1) read them, rowbind them, factor them to unify the levels again, or (2) just write
# the imputedDf2 as a CSV, read it and simply split into train and test dataframes in a new file!

# will go with (2)

write.csv(imputedDf2, file = "ImpT&TCombined.csv", row.names = FALSE)


# GOOD!






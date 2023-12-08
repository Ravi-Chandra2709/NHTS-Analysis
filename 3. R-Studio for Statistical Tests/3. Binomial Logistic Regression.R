install.packages("MASS")
install.packages("dplyr")
library(MASS)
library(dplyr)

#### Imprort Dataset ####
FL2001 <- read.csv("oversampled_data_2001_AllVariables.csv", header = TRUE)
FL2009 <- read.csv("oversampled_data_2009_AllVariables.csv", header = TRUE)
FL2017 <- read.csv("oversampled_data_2017_AllVariables.csv", header = TRUE)
FL2017ALL <- read.csv("oversampled_data_2017_All.csv", header = TRUE)
summary(FL2001)
summary(FL2009)
summary(FL2017)

#### Justify type of variables ####
FL2001$HHFAMINC2 <- factor(FL2001$HHFAMINC2, levels = c("0", "1", "2"))
levels(FL2001$HHFAMINC2) <- list(Low = "0",
                                 Medium = "1",
                                 High = "2")
FL2009$HHFAMINC2 <- factor(FL2009$HHFAMINC2, levels = c("0", "1", "2"))
levels(FL2009$HHFAMINC2) <- list(Low = "0",
                                 Medium = "1",
                                 High = "2")
FL2017$HHFAMINC2 <- factor(FL2017$HHFAMINC2, levels = c("0", "1", "2"))
levels(FL2017$HHFAMINC2) <- list(Low = "0",
                                 Medium = "1",
                                 High = "2")
FL2001$WRKTRANS2 <- factor(FL2001$WRKTRANS)
levels(FL2001$WRKTRANS2) <- list(Public = c("2", "5", "9", "10", "13", "15"),
                                 Private = c("0", "3", "6", "7", "8"))
FL2009$WRKTRANS2 <- factor(FL2009$WRKTRANS)
levels(FL2009$WRKTRANS2) <- list(Public = c("6", "8", "9", "10", "14", "15", "16", "19", "20", "21", "22", "23"),
                                 Private = c("1", "2", "3", "12", "13", "18"))
FL2017$WRKTRANS2 <- factor(FL2017$WRKTRANS)
levels(FL2017$WRKTRANS2) <- list(Public = c("4", "5", "10", "11", "12", "13"),
                                 Private = c("1", "2", "6", "7"))
FL2009$SCHTRN3 <- factor(FL2009$SCHTRN2)
levels(FL2009$SCHTRN3) <- list(SchoolBus = "2",
                               Car = c("1", "4", "6"))

#### Binomial Logistic Regression (Mode choice vs. Household Family Income Group) ####
bw2001 <- glm(WRKTRANS2 ~ HHFAMINC2, family = binomial(link = 'logit'), data = FL2001)
summary(bw2001)
bw2009 <- glm(WRKTRANS2 ~ HHFAMINC2, family = binomial(link = 'logit'), data = FL2009)
summary(bw2009)
bw2017 <- glm(WRKTRANS2 ~ HHFAMINC2, family = binomial(link = 'logit'), data = FL2017)
summary(bw2017)
bs2009 <- glm(SCHTRN3 ~ HHFAMINC2, family = binomial(link = 'logit'), data = FL2009)
summary(bs2009)

#### Frequency Tables ####
table(FL2001$HHFAMINC2, FL2001$WRKTRANS2)
table(FL2009$HHFAMINC2, FL2009$WRKTRANS2)
table(FL2017$HHFAMINC2, FL2017$WRKTRANS2)
table(FL2009$HHFAMINC2, FL2009$SCHTRN3)

#### Chi-square Test ####
chisq.test(FL2001$HHFAMINC2, FL2001$WRKTRANS2)
chisq.test(FL2009$HHFAMINC2, FL2009$WRKTRANS2)
chisq.test(FL2017$HHFAMINC2, FL2017$WRKTRANS2)
chisq.test(FL2009$HHFAMINC2, FL2009$SCHTRN3)
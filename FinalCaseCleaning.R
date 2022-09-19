##############################################################
##############################################################
## Cleaning data for total cash
##############################################################
##############################################################

load("TOTALCASH.RData")


CASH <- rbind(CASH.TRAIN,CASH.KAGGLE)


#GENDER - fine
summary(CASH$GENDER)

#MARITAL_STATUS - combine NULL and Unknown?
summary(CASH$MARITAL_STATUS)
plot(LOG_TOTAL_CASH ~ MARITAL_STATUS, data=CASH)
AOV <- aov(LOG_TOTAL_CASH ~ MARITAL_STATUS, data=CASH)
TUKEY <- TukeyHSD(AOV)
multcompLetters4(AOV,TUKEY)
#Data suggests these represent different things since stat. sig difference

#PHONE_NUMBER - fine
plot(LOG_TOTAL_CASH ~ PHONE_NUMBER, data=CASH)

#EMAIL_ADDRESS - fine
plot(LOG_TOTAL_CASH ~ EMAIL_ADDRESS, data=CASH)

#However, remember the direction of causality here.  We obtain emails and phone numbers
#when people donate, so it's no surprise that the average donation amounts are higher
#for people with these quantities recorded.  Going out and getting more emails/phone numbers
#on file won't necessarily increase the amount these people are donating (though obviously, 
#somehow connecting these alumni and re-engaging will help).


#BIRTH_YEAR - needs to be <= DEGREE_1_GRAD_YEAR, and really less than
summary(CASH$BIRTH_YEAR)

#DEGREE_1_GRAD_YEAR-16 or so.  That's not the case here
mean(CASH$BIRTH_YEAR >= CASH$DEGREE_1_GRAD_YEAR-16)

#A potentially good idea is to replace birthyear with "age at graduation"
#You could replace the BS values with "Unknown", and then have categories for
#20, 21, 22, 23, 24, 25+ or something like that.  
age.at.graduation <- CASH$DEGREE_1_GRAD_YEAR - CASH$BIRTH_YEAR
summary(age.at.graduation)

grad <- c()
grad[ age.at.graduation <= 15 ] <- "Unknown"  #assume no one grads this early
grad[ age.at.graduation > 15 & age.at.graduation <= 20 ] <- "Before21"
grad[ age.at.graduation == 21 ] <- "21"
grad[ age.at.graduation == 22 ] <- "22"
grad[ age.at.graduation == 23 ] <- "23"
grad[ age.at.graduation == 24 ] <- "24"
grad[ age.at.graduation >= 25 & age.at.graduation <= 29 ] <- "late20s"
grad[ age.at.graduation >= 30 & age.at.graduation <= 39 ] <- "30s"
grad[ age.at.graduation >= 40 & age.at.graduation <= 49 ] <- "40s"
grad[ age.at.graduation >= 50 ] <- "50sAndAbove"
grad <- factor(grad)
summary(grad)
#Add to data, ordering the factor levels accordingly
CASH$AGE_AT_GRADUATION <- factor(grad,ordered = TRUE,
                                   levels=c("Unknown","Before21","21","22","23","24","late20s","30s","40s","50sAndAbove"))
summary(CASH$AGE_AT_GRADUATION)
plot(LOG_TOTAL_CASH ~ AGE_AT_GRADUATION, data=CASH)

#DEGREE_1_GRAD_YEAR - Fine as is.  BUT, you could make categories out of it (like 40s, 50s, 60s, etc.)
plot(LOG_TOTAL_CASH ~ DEGREE_1_GRAD_YEAR, data=CASH)
summary(CASH$DEGREE_1_GRAD_YEAR)
gradyear <- CASH$DEGREE_1_GRAD_YEAR
year <- c()
year[gradyear <= 1949] <- "1940s"
year[gradyear >= 1950 & gradyear <= 1959] <- "1950s"
year[gradyear >= 1960 & gradyear <= 1969] <- "1960s"
year[gradyear >= 1970 & gradyear <= 1979] <- "1970s"
year[gradyear >= 1980 & gradyear <= 1989] <- "1980s"
year[gradyear >= 1990 & gradyear <= 1999] <- "1990s"
year[gradyear >= 2000 & gradyear <= 2010] <- "2000s"
year[gradyear >= 2010 & gradyear <= 2019] <- "2010s"
year <- factor(year)
summary(year)
#Add to data, ordering the factor levels accordingly
CASH$DEGREE_1_GRAD_YEAR <- factor(year,ordered = TRUE,
                                 levels=c("1940s","1950s","1960s","1970s","1980s","1990s",
                                          "2000s","2010s"))
summary(CASH$DEGREE_1_GRAD_YEAR)
plot(LOG_TOTAL_CASH ~ DEGREE_1_GRAD_YEAR, data=CASH)

CASH$BIRTH_YEAR <- NULL


#DEGREE_1_TYPE  - fine
plot(LOG_TOTAL_CASH ~ DEGREE_1_TYPE, data=CASH)


#TWITTER- FINE
plot(LOG_TOTAL_CASH ~ TWITTER, data=CASH)
#LINKEDIN- FINE
plot(LOG_TOTAL_CASH ~ LINKEDIN, data=CASH)

#FACEBOOK- FINE
plot(LOG_TOTAL_CASH ~ FACEBOOK, data=CASH)

#NUM_UT_FAMILY_RELATIONSHIPS - fine
plot(LOG_TOTAL_CASH ~ NUM_UT_FAMILY_RELATIONSHIPS, data=CASH)
abline(lm(LOG_TOTAL_CASH ~ NUM_UT_FAMILY_RELATIONSHIPS, data=CASH))

#NUM_STUDENT_ACTIVITIES - fine
plot(LOG_TOTAL_CASH ~ NUM_STUDENT_ACTIVITIES, data=CASH)
abline(lm(LOG_TOTAL_CASH ~ NUM_STUDENT_ACTIVITIES, data=CASH))

#FRATERNITY_SORORITY - fine
plot(LOG_TOTAL_CASH ~ FRATERNITY_SORORITY, data=CASH)


#BAND - fine
plot(LOG_TOTAL_CASH ~ BAND, data=CASH)

#HONOR_SOCIETY - fine
plot(LOG_TOTAL_CASH ~ HONOR_SOCIETY, data=CASH)

#STUDENT_GOVT - fine
plot(LOG_TOTAL_CASH ~ STUDENT_GOVT, data=CASH)


#NUM_SPORTS - fine (though could do a Yes/No replacement)

plot(LOG_TOTAL_CASH ~ NUM_SPORTS, data=CASH)
abline(lm(LOG_TOTAL_CASH ~ NUM_SPORTS, data=CASH))
summary(CASH$NUM_SPORTS)
numsports <- CASH$NUM_SPORTS
table(numsports)
sports <- c()
sports[numsports == 0] <- "No"
sports[numsports != 0] <- "Yes"
sports <- factor(sports)
summary(sports)
CASH$SPORTS_YES_NO <- factor(sports)
CASH$NUM_SPORTS <- NULL
summary(CASH$SPORTS_YES_NO)

#ALUMNI_ACTIVITIES - fine (though could do a Yes/No replacement)
plot(LOG_TOTAL_CASH ~ ALUMNI_ACTIVITIES, data=CASH)
abline(lm(LOG_TOTAL_CASH ~ ALUMNI_ACTIVITIES, data=CASH))
table(CASH$ALUMNI_ACTIVITIES)
alumniactivity <- CASH$ALUMNI_ACTIVITIES
table(alumniactivity)
activity <- c()
activity[alumniactivity == 0] <- "No"
activity[alumniactivity != 0] <- "Yes"
activity <- factor(activity)
summary(activity)
CASH$ALUM_ACTIVITY_YES_NO <- factor(activity)
CASH$ALUMNI_ACTIVITIES <- NULL
summary(CASH$ALUM_ACTIVITY_YES_NO)

#NUM_ALUMNI_EVENTS - fine (though could do categories like 0, 1, 2, 3 or more)
plot(LOG_TOTAL_CASH ~ NUM_ALUMNI_EVENTS, data=CASH)
abline(lm(LOG_TOTAL_CASH ~ NUM_ALUMNI_EVENTS, data=CASH))
table(CASH$NUM_ALUMNI_EVENTS)
alumnievents <- CASH$NUM_ALUMNI_EVENTS
events <- c()
events[alumnievents == 0] <- "None"
events[alumnievents == 1 | alumnievents == 2] <- "1or2"
events[alumnievents >= 3] <- "3orMore"
events <- factor(events)
CASH$NUM_ALUMNI_EVENTS <- factor(events, ordered = TRUE,
                                 levels = c("None", "1or2", "3orMore"))
table(CASH$NUM_ALUMNI_EVENTS)

#Estimated Capacity is awful
levels(CASH$ESTIMATED_GIFT_CAPACITY)
#Combine blank, NULL, with Unable to rate
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("","NULL","Unable to rate")) ] <- "UnableToRate"
#Combine the redundant levels; how tedious
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$2K - $3K","$2K-$3K") ) ] <- "2Kto3K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$3K - $5K","$3K-$5K") ) ] <- "3Kto5K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$5K - $7.5K","$5K-$7.5K") ) ] <- "5Kto7.5K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$10K - $15K","$10K-$15K") ) ] <- "10Kto15K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$20K - $25k","$20K-$25k") ) ] <- "20Kto25K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$25K - $30K","$25K-$30K") ) ] <- "25Kto30K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$40K - $50K","$40K-$50K") ) ] <- "40Kto50K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$50K-$75K","$50K - $75K") ) ] <- "50Kto75K"
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$75K-$100K","$75K - $100K") ) ] <- "75Kto100K"

#Values in the millions are quite rare, so combine anything over a million into a single category
levels(CASH$ESTIMATED_GIFT_CAPACITY)[ which( levels(CASH$ESTIMATED_GIFT_CAPACITY) %in% c("$1MM - $5MM","$5MM+") ) ] <- "Over1mil"


#Strip away spaces, dashes, and dollar signs from levels
levels(CASH$ESTIMATED_GIFT_CAPACITY) <- gsub(" - ","to",levels(CASH$ESTIMATED_GIFT_CAPACITY) )
levels(CASH$ESTIMATED_GIFT_CAPACITY) <- gsub("-","to",levels(CASH$ESTIMATED_GIFT_CAPACITY) )
levels(CASH$ESTIMATED_GIFT_CAPACITY) <- gsub("\\$","",levels(CASH$ESTIMATED_GIFT_CAPACITY) )
summary(CASH$ESTIMATED_GIFT_CAPACITY)  
levels(CASH$ESTIMATED_GIFT_CAPACITY)

#Now order the levels....
CASH$ESTIMATED_GIFT_CAPACITY <- factor(CASH$ESTIMATED_GIFT_CAPACITY,ordered = TRUE,
                                         levels=c("UnableToRate","< 1K","1Kto2K","2Kto3K","3Kto5K","5Kto7.5K","7.5Kto10K",
                                                  "10Kto15K","15Kto20K","20Kto25K","25Kto30K","30Kto40K","40Kto50K","50Kto75K",
                                                  "75Kto100K","100Kto200K","200Kto300K","300Kto500K","500Kto1MM","Over1mil"))
summary(CASH$ESTIMATED_GIFT_CAPACITY)
plot(LOG_TOTAL_CASH~ESTIMATED_GIFT_CAPACITY,data=CASH)


#ESTIMATED_INCOME - actually not bad, just strip away $ and -
table(CASH$ESTIMATED_INCOME)  
levels(CASH$ESTIMATED_INCOME)[ which( levels(CASH$ESTIMATED_INCOME) %in% c("NULL","Unable to rate") ) ] <- "NULL"
levels(CASH$ESTIMATED_INCOME) <- gsub("-","to",levels(CASH$ESTIMATED_INCOME) )
levels(CASH$ESTIMATED_INCOME) <- gsub("\\$","",levels(CASH$ESTIMATED_INCOME) )
summary(CASH$ESTIMATED_INCOME)  
#Order 
CASH$ESTIMATED_INCOME <- factor(CASH$ESTIMATED_INCOME,ordered=TRUE,
                                  levels=c("NULL","1to50K","50Kto100K","100Kto250K","250Kto500K","500K+"))
plot(LOG_TOTAL_CASH~ESTIMATED_INCOME,data=CASH)



#ESTIMATED_TOTAL_ASSETS - strip out - and %, probably make sense to combine the VERY RICH (25MM+together
table(CASH$ESTIMATED_TOTAL_ASSETS) #COMBINE Null/Unable, and the really high values
levels(CASH$ESTIMATED_TOTAL_ASSETS)[ which( levels(CASH$ESTIMATED_TOTAL_ASSETS) %in% c("NULL","Unable to rate") ) ] <- "NULL"

levels(CASH$ESTIMATED_TOTAL_ASSETS) <- gsub("-","to",levels(CASH$ESTIMATED_TOTAL_ASSETS) )
levels(CASH$ESTIMATED_TOTAL_ASSETS) <- gsub("\\$","",levels(CASH$ESTIMATED_TOTAL_ASSETS) )

levels(CASH$ESTIMATED_TOTAL_ASSETS)[ which(levels(CASH$ESTIMATED_TOTAL_ASSETS) %in% c("25MMto50MM","50MMto100MM","100MMto500MM","500MM+"))] <- "25MM+"
summary(CASH$ESTIMATED_TOTAL_ASSETS) 

CASH$ESTIMATED_TOTAL_ASSETS <- factor(CASH$ESTIMATED_TOTAL_ASSETS,ordered = TRUE,
                                        levels=c("NULL","<25K","25Kto50K","50Kto100K","100Kto500K","500Kto1MM",
                                                 "1MMto5MM","5MMto10MM","10MMto25MM","25MM+"))
plot(LOG_TOTAL_CASH~ESTIMATED_TOTAL_ASSETS,data=CASH)






#The 3rd party scores all look ok!
summary(CASH$ANNUITY_LIKELIHOOD)
plot(LOG_TOTAL_CASH~ANNUITY_LIKELIHOOD,data=CASH)
abline(lm(LOG_TOTAL_CASH ~ ANNUITY_LIKELIHOOD, data=CASH))

summary(CASH$BEQUEST_LIKELIHOOD)
plot(LOG_TOTAL_CASH~ANNUITY_LIKELIHOOD,data=CASH)
abline(lm(LOG_TOTAL_CASH ~ BEQUEST_LIKELIHOOD, data=CASH))


summary(CASH$CRT_LIKELIHOOD)
plot(LOG_TOTAL_CASH~CRT_LIKELIHOOD,data=CASH)
abline(lm(LOG_TOTAL_CASH ~ CRT_LIKELIHOOD, data=CASH))

####My additional features
TEMP <- CASH[,grep("TOT_.*GAMES",names(CASH))] 
names(TEMP) <- c("Football","MenBB","WomenBB")
most <- c()
for (i in 1:nrow(TEMP)) {
  if( sum(TEMP[i,]) == 0 ) { most[i] <- "none" } else {
    most[i] <- names(TEMP)[which.max(TEMP[i,])]
  }
}
CASH$FavGame <- factor(most)
plot(LOG_TOTAL_CASH~FavGame,data=CASH)

CASH$TOT_WBB_GAMES <- NULL
CASH$TOT_MBB_GAMES <- NULL
CASH$TOT_FB_GAMES <- NULL

CASH$ClickReceiveRatio <- CASH$TOT_EMAIL_CLICK/CASH$TOT_EMAIL_RECEIVED
CASH$ClickReceiveRatio[which(is.na(CASH$ClickReceiveRatio))] <- 0
plot(LOG_TOTAL_CASH~ClickReceiveRatio,data=CASH)
abline(lm(LOG_TOTAL_CASH ~ ClickReceiveRatio, data=CASH))
table(cut(CASH$ClickReceiveRatio, breaks = 5))

CASH$OpenReceiveRatio <- CASH$TOT_EMAIL_OPEN/CASH$TOT_EMAIL_RECEIVED
CASH$OpenReceiveRatio[which(is.na(CASH$OpenReceiveRatio))] <- 0
plot(LOG_TOTAL_CASH~OpenReceiveRatio,data=CASH)
abline(lm(LOG_TOTAL_CASH ~ OpenReceiveRatio, data=CASH))
table(cut(CASH$OpenReceiveRatio, breaks = 5))


CASH$ClickOpenRatio <- CASH$TOT_EMAIL_CLICK/CASH$TOT_EMAIL_OPEN
CASH$ClickOpenRatio[which(is.na(CASH$ClickOpenRatio))] <- 0
CASH$ClickOpenRatio[which(!is.finite(CASH$ClickOpenRatio))] <- 0
plot(LOG_TOTAL_CASH~ClickOpenRatio,data=CASH)
abline(lm(LOG_TOTAL_CASH ~ ClickOpenRatio, data=CASH))
table(cut(CASH$ClickOpenRatio, breaks = 5))


CASH$TOT_EMAIL_OPEN <- NULL
CASH$TOT_EMAIL_CLICK <- NULL
CASH$TOT_EMAIL_RECEIVED <- NULL



##Alright, we're good to go now.  Split back into training/holdout, and save the
#work that's been done into an .RData file

CASH.TRAIN.CLEAN <- head(CASH,nrow(CASH.TRAIN))
CASH.KAGGLE.CLEAN <- tail(CASH,nrow(CASH.KAGGLE))

summary(CASH.TRAIN.CLEAN)
summary(CASH.KAGGLE.CLEAN)

CASH.TRAIN.CLEAN$ID <- NULL

save(CASH.TRAIN.CLEAN, CASH.KAGGLE.CLEAN, file = "TOTALCASH.CLEAN.Rdata")


# This code is written by Pierre Michel B. Hardy in partial fulfillment of his master thesis
# This code contains additional data wrangling before running the regression
# Namely, it's about taking out the effects of individual artist effects from the variables. 



# Packages #########
library(readxl)
library(dplyr)
library(xlsx)
library(varhandle)



# Data #########
dat <- read_excel("DATA COLLECTED - READY FOR ANALYSIS.xlsx")



# Main Code ########

# turn T2H into numeric. Apparently, it's in character
dat$T2H <- as.numeric(dat$T2H)

# make another copy to reduce risk of having to repeat
dat2 <- dat

# group by artist, take the mean value of variable, then subtract the mean from each
# this first one is my test to make sure the code does what I want it to
dat3 <- dat2 %>%
  group_by(arist.id) %>%
  transmute(dance.mean = mean(danceability),
            danceability2 = danceability - mean(danceability),
            dancebility.orig = danceability) 

# this code snippet has been tested against excel on a few artists
# the results are consistent with what I want to happen
# this code works!

# okay, time to to do this for all columns
dat4 <- dat2 %>%
  group_by(arist.id) %>%
  transmute(danceability2 = danceability - mean(danceability),
            loudness2 = loudness - mean(loudness),
            speechiness2 = speechiness - mean(speechiness),
            acousticness2 = acousticness - mean(acousticness),
            instrumentalness2 = instrumentalness - mean(instrumentalness),
            liveness2 = liveness - mean(liveness),
            tempo2 = tempo - mean(tempo),
            track.duration_ms2 = track.duration_ms - mean(track.duration_ms),
            track.popularity2 = track.popularity - mean(track.popularity),
            T2H2 = T2H - mean(T2H),)

# just going to put back the columns I didn't transform but lost from the transmutation
leftover <- select(dat2, c("key","mode","track.id","track.name","track.album.id",
                           "track.album.name","artist.name","time.signature.dummy"))

# Last job: turning the moods into binary
mood.binary <- select(dat2, c("track.mood"))
mood.binary <- to.dummy(mood.binary$track.mood, "mood")

# volt em in
dat5 <- cbind(as.data.frame(dat4), leftover, mood.binary)



# Export the dataset ########
write.xlsx(dat5, "DATA COLLECTED - READY FOR ANALYSIS - ARTIST EFFECTS.xlsx")



# Next Steps #####
# That's it! Artist effects are taken care off and now we're ready for analysis

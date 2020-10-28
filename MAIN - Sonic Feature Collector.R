# Made by Pierre Michel B. Hardy for the partial fulfillment of his Master Thesis 
# DATA COLLECTOR
# This code is the main code for collecting the bulk of the dataset for the thesis
# It contains information about the moods, sonic features, and other details of the track 
# (name, artist, etc)



# packages ##########
library(spotifyr)
library(readxl)
library(dplyr)
library(data.table)
library(writexl)
library(SparkR)
library(tidyr)



# API setup ##########
Sys.setenv(SPOTIFY_CLIENT_ID = '94af31474e8b488dbb95e8134bc9f61b')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '6e483800f9ba43e3b2faf8cf7a6f05be')
access_token <- get_spotify_access_token()



# Code for getting data #########
#  This is the playlist in Spotify that contains the Billboard Album 200 2019
x <- "3J9lHeQ5TFHMFmrTUlo2iC" 

# Get the bulk of data from API
y <- get_playlist_audio_features("pierremichel_hardy", x)

#  To see all columns given
# names(y)                                                



# Code for tidying up ########
# Select the columns
col.nums <- c(6,7,8,9,10,11,12,13,14,15,16,17,19,31,36,37,47,49)

# Filter the columns
dat.narrowed <-  y[,col.nums]

# Extract the nested artist information
artist.info <- y[,28]
for (i in 1:nrow(artist.info)){
  artist.info[i,2] <- artist.info[[1]][[i]][2] #  artist id
  artist.info[i,3] <- artist.info[[1]][[i]][3] #  artist name
}
artist.info <- artist.info[,2:3]
colnames(artist.info) <- c("arist.id","artist.name")

# Append on main dataset so far
dat.narrowed2 <- cbind(dat.narrowed, artist.info)



# Time to transform to include mood ###########
# Descriptions
# Happy <- energy >= 0.5 valence >= 0.5
# Relaxed <- energy <= 0.5 valence >= 0.5
# Angry <- energy >= 0.5 valence <= 0.5
# Sad <- energy <= 0.5 valence <= 0.5

# Duplicate track for safety
dat.narrowed3 <- dat.narrowed2

# Add the column for track moods
dat.narrowed3[,ncol(dat.narrowed2)+1] <- "x"

# Rename the created column
colnames(dat.narrowed3)[ncol(dat.narrowed3)] <- "track.mood"

# Create function for mood.track
mood.func <- function(e,v){
  if(e >= 0.5 && v >= 0.5){
    m <- "Happy"
  } else if (e <= 0.5 && v >= 0.5) {
    m <- "Relaxed"
  } else if (e >= 0.5 && v <= 0.5) {
    m <- "Angry" 
  } else if (e <= 0.5 && v <= 0.5) {
    m <- "Sad"
  } else {
    m <- "N A N I !"
  }
  return(m)
}

# Populate track.mood column
dat.narrowed4 <- dat.narrowed3 %>%
  dplyr::group_by(track.id) %>%
  dplyr::mutate(track.mood = mood.func(energy, valence))



# Final touch: create the time signature dummy #########
# function for this
tim.sig.func <- function(x){
  if (x==4){
    y <- 1
  } else {
    y <- 0
  }
  return(y)
}

# apply function
dat.narrowed5 <- dat.narrowed4 %>%
  dplyr::group_by(track.id) %>%
  dplyr::mutate(time.signature.dummy = tim.sig.func(time_signature))

# Clean up remnants of calculations
rem <- c(-2,-10, -13)
final.dat <- dat.narrowed5[,rem]



 # Export into excel #########
write_xlsx(final.dat, "C:/Users/pierr/OneDrive/Documents/Maastricht University/Masters/Thesis 2/R Stuff/READY FOR T2H.xlsx")
write_xlsx(dat.narrowed2, "C:/Users/pierr/OneDrive/Documents/Maastricht University/Masters/Thesis 2/R Stuff/RAW DATA.xlsx")



# Next steps ##########
# Next will be to use my algorithm to calculate the T2H
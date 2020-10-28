library(readxl)
library(ggplot2)
library(dplyr)
RAW_DATA <- read_excel("RAW DATA.xlsx")
dat.t2h <- read_excel("DATA COLLECTED - READY FOR ANALYSIS.xlsx")
View(RAW_DATA)

dat <- RAW_DATA

dat.t2h$T2H <- as.numeric(dat.t2h$T2H)
dat.labels <- read_excel("DATA COLLECTED - READY FOR ANALYSIS - ARTIST EFFECTS.xlsx")

hist(dat.t2h$T2H, xlab="T2H", main="Histogram of the T2H of Songs in 2019's Hit Albums",
     breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,
              125, 130, 135, 140, 145,150, 155,160,165,170,175,180,185,190,195,200,205,210,
              220))

plot(dat$energy, dat$valence, xlab="Energy", ylab="Valence", 
     main="Scatterplot of the Energy and Valence of Songs in 2019's Hit Albums")
abline(h=0.5, col="red")
abline(v=0.5, col="red")
text(0.99, 0.99, "Happy", col="red")
text(0.01, 0.99, "Relaxed", col="red")
text(0.99, 0.01, "Angry", col="red")
text(0.01, 0.01, "Sad", col="red")

sum(dat.t2h$mood.Happy)
sum(dat.t2h$mood.Sad)
sum(dat.t2h$mood.Relaxed)
sum(dat.t2h$mood.Angry)

#labels
mytally <- data.frame(
  Label = c("Sony","Universal","Warner","Indie"),
  Tally = c(sum(dat.labels$Sony),sum(dat.labels$Universal), sum(dat.labels$Warner),
            sum(dat.labels$Indie))
)
mytallymat <- mytally$Tally
barplot(mytallymat, 
        main="Labels in the 2019 Billboard 200 Albums Year-end Charts",
        names.arg = c("Sony","Universal","Warner","Indie"))
#acoustic
hist(dat$acousticness, main="Histogram of the Acousticness of 2019 Hit Songs")
#dance
hist(dat$danceability, main="Histogram of the Danceability of 2019 Hit Songs")
#isntrument
hist(dat$instrumentalness, main="Histogram of the Instrumentalness of 2019 Hit Songs")
#live
hist(dat$liveness, main="Histogram of the Liveness of 2019 Hit Songs")
#loud
hist(dat$loudness, main="Histogram of the Loudness of 2019 Hit Songs")
#mode
mytally2 <- data.frame(
  Label = c("Minor","Major"),
  Tally = c((nrow(dat)-sum(dat$mode)), sum(dat$mode))
)
mytallymat2 <- mytally2$Tally
barplot(mytallymat2, 
        main="Modes in the 2019 Billboard 200 Albums Year-end Charts",
        names.arg = c("Minor","Major"))
#speechiness
hist(dat$speechiness, main="Histogram of the Speechiness of 2019 Hit Songs")
#tempo
hist(dat$tempo, main="Histogram of the Tempo of 2019 Hit Songs")
#time signature
mytally3 <- data.frame(
  Label = c("4/4","Other"),
  Tally = c(sum(dat.labels$time.signature.dummy),
            (nrow(dat.labels)-sum(dat.labels$time.signature.dummy)))
)
mytallymat3 <- mytally3$Tally
barplot(mytallymat3, 
        main="Time Signatures in the 2019 Billboard 200 Albums Year-end Charts",
        names.arg = c("4/4","Others"))
#duration
hist(dat$track.duration_ms, main="Histogram of the Duration of 2019 Hit Songs")





### scatterplots for nonlinear potential ####
# acoustic
plot(dat.labels$acousticness2, dat.labels$track.popularity2)
# linear

# dance
plot(dat.labels$danceability2, dat.labels$track.popularity2)
# linear

# instrument
plot(dat.labels$instrumentalness2, dat.labels$track.popularity2)
# kinda linear?

# liveness
plot(dat.labels$liveness2, dat.labels$track.popularity2)
# linear

# loudness
plot(dat.labels$loudness2, dat.labels$track.popularity2)
# linear

# mode
plot(dat.labels$mode, dat.labels$track.popularity2)
# linear

# speech
plot(dat.labels$speechiness2, dat.labels$track.popularity2)
# kinda linear

# tempo
plot(dat.labels$tempo2, dat.labels$track.popularity2)
# linear
# tempo unadjusted
plot(dat$tempo, dat$track.popularity)
# still seems linear

# duration
plot(dat.labels$track.duration_ms2, dat.labels$track.popularity2)

# t2h
plot(dat.labels$T2H2, dat.labels$track.popularity2)

# moods

# tempo quad equation
temp.eq <- function(x){
  ((0.0004414*(x*x))+(0.007537*x))
}
min.temp <- min(dat.labels$tempo2)
max.temp <- max(dat.labels$tempo2)
plot(x=min.temp:max.temp, y=temp.eq(min.temp:max.temp),
     main="Plot of the Tempo Polynomial",
     ylab="",
     xlab="Range of tempos")

dur.eq <- function(x){
  ((-0.00000000004411*(x*x))+(0.00001464*x))
}
min.dur <- min(dat.labels$track.duration_ms2)
max.dur <- max(dat.labels$track.duration_ms2)
plot(x=min.dur:max.dur, y=dur.eq(min.dur:max.dur),
     main="Plot of the Track Duration Polynomial",
     ylab="",
     xlab="Range of Track Durations")


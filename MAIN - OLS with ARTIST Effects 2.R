# pacakges
library(readxl)
library(dplyr)
library(rms)
library(Hmisc)
library(corrplot)
library(car)
library(jtools)

# data
dat.art.eff <- read_excel("DATA COLLECTED - READY FOR ANALYSIS - ARTIST EFFECTS.xlsx")
dat <- read_excel("DATA COLLECTED - READY FOR ANALYSIS.xlsx")
dat.art.eff$T2H<-as.numeric(as.character(dat$T2H))
dat$T2H<-as.numeric(as.character(dat$T2H))
vars.eff <- c("danceability2","loudness2","speechiness2", "acousticness2",
              "instrumentalness2","liveness2","tempo2","track.duration_ms2",
              "track.popularity2","T2H2","key","mode","time.signature.dummy",
              "mood.Angry","mood.Happy","mood.Relaxed","mood.Sad","Sony","Universal",
              "Warner","Indie")
vars <- c("danceability","loudness","speechiness", "acousticness",
          "instrumentalness","liveness","tempo","track.duration_ms",
          "track.popularity","T2H","key","mode","time.signature.dummy",
          "mood.Angry","mood.Happy","mood.Relaxed","mood.Sad","Sony","Universal",
          "Warner","Indie")
dat.art.eff1 <- select(dat.art.eff, vars.eff)
dat2 <- select(dat, vars)

#lm

lm.eff1 <- lm(formula = dat.art.eff1$track.popularity2~. , data=dat.art.eff1)
summary(lm.eff1)
alias(lm(formula = dat.art.eff1$track.popularity2~. , data=dat.art.eff1))

# take two minus sad 
vars.eff2 <- c("danceability2","loudness2","speechiness2", "acousticness2",
              "instrumentalness2","liveness2","tempo2","track.duration_ms2",
              "track.popularity2","T2H2","key","mode","time.signature.dummy",
              "mood.Angry","mood.Happy","mood.Relaxed","Sony","Universal",
              "Warner","Indie")
dat.art.eff2 <- select(dat.art.eff, vars.eff2)
lm.eff2 <- lm(formula = dat.art.eff2$track.popularity2~. , data=dat.art.eff2)
summary(lm.eff2)

# try to detect again 
vif(lm.eff2)

# try again remove the labels
vars.eff3 <- c("danceability2","loudness2","speechiness2", "acousticness2",
               "instrumentalness2","liveness2","tempo2","track.duration_ms2",
               "track.popularity2","T2H2","key","mode","time.signature.dummy",
               "mood.Angry","mood.Happy","mood.Relaxed","Sony",
               "Warner","Indie")
dat.art.eff3 <- select(dat.art.eff, vars.eff3)
lm.eff3 <- lm(formula = dat.art.eff3$track.popularity2~. , data=dat.art.eff3)
summary(lm.eff3)

# try to detect again 
vif(lm.eff3)

# remove keys
vars.eff4 <- c("danceability2","loudness2","speechiness2", "acousticness2",
               "instrumentalness2","liveness2","tempo2","track.duration_ms2",
               "track.popularity2","T2H2","mode","time.signature.dummy",
               "mood.Angry","mood.Happy","mood.Relaxed","Sony",
               "Warner","Indie")
dat.art.eff4 <- select(dat.art.eff, vars.eff4)
lm.eff4 <- lm(formula = dat.art.eff4$track.popularity2~. , data=dat.art.eff4)
summary(lm.eff4)

# try to detect again 
vif(lm.eff4)


# dance
plot(dat.art.eff$danceability2,dat.art.eff$track.popularity2)
abline(lm(dat.art.eff$track.popularity2~dat.art.eff$danceability2))

# experimenting with interactions

# all the yes ####
lm.t2hxtimesig <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                       acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                       track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                       mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                       T2H2*time.signature.dummy, data=dat.art.eff3) 

summary(lm.t2hxtimesig) #  yes

lm.t2hxwarner <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                      acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                      track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                      mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                      T2H2*Warner, data=dat.art.eff3) 

summary(lm.t2hxwarner) #  yes

# all the maybe ####
lm.t2hxrelax <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                     acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*mood.Relaxed, data=dat.art.eff3)

summary(lm.t2hxrelax) #  MAYBE

lm.t2hxsony <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                    acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                    track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                    mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                    T2H2*Sony, data=dat.art.eff3) 

summary(lm.t2hxsony) #  maybe

# all the no ####
lm.eff4 <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
  acousticness2 + instrumentalness2 + liveness2 + tempo2 + track.duration_ms2 + T2H2 + key 
  + mode + time.signature.dummy + mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + 
  Indie, data=dat.art.eff3)

summary(lm.eff3)
summary(lm.eff4)

lm.t2hxhappy <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
  acousticness2 + instrumentalness2 + liveness2 + tempo2 + track.duration_ms2 + T2H2 + key 
  + mode + time.signature.dummy + mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + 
  Indie + T2H2*mood.Happy, data=dat.art.eff3)

summary(lm.t2hxhappy) #  nope

lm.t2hxangry <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                    acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*mood.Angry, data=dat.art.eff3)

summary(lm.t2hxangry) #  nope

lm.t2hxsad <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                     acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*mood.Sad, data=dat.art.eff1) 

summary(lm.t2hxsad) #  nope

lm.t2hxdance <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                   acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                   track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                   mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                   T2H2*danceability2, data=dat.art.eff3) 

summary(lm.t2hxdance) #  nope

lm.t2hxloud <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                     acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*loudness2, data=dat.art.eff3) 

summary(lm.t2hxloud) #  nope

lm.t2hxspeech <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                    acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                    track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                    mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                    T2H2*speechiness2, data=dat.art.eff3) 

summary(lm.t2hxspeech) #  nope

lm.t2hxacous <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                      acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                      track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                      mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                      T2H2*acousticness2, data=dat.art.eff3) 

summary(lm.t2hxacous) #  nope

lm.t2hxinstru <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                     acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*instrumentalness2, data=dat.art.eff3) 

summary(lm.t2hxinstru) #  nope

lm.t2hxlive <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                      acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                      track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                      mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                      T2H2*liveness2, data=dat.art.eff3) 

summary(lm.t2hxlive) #  nope

lm.t2hxtempo <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                    acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                    track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                    mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                    T2H2*tempo2, data=dat.art.eff3) 

summary(lm.t2hxtempo) #  nope

lm.t2hxdur <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                     acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*track.duration_ms2, data=dat.art.eff3) 

summary(lm.t2hxdur) #  nope

lm.t2hxmode <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                   acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                   track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                   mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                   T2H2*mode, data=dat.art.eff3) 

summary(lm.t2hxmode) #  nope

lm.t2hxIndie <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                    acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                    track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                    mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                    T2H2*Indie, data=dat.art.eff3) 

summary(lm.t2hxIndie) #  nope

lm.t2hxkey <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                     acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*key, data=dat.art.eff3) 

summary(lm.t2hxkey) #  nope

lm.t2hxkey <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                   acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                   track.duration_ms2 + T2H2 + key + mode + time.signature.dummy + 
                   mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                   T2H2*key, data=dat.art.eff3) 

summary(lm.t2hxkey) #  nope

lm.t2hxmoods <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                   acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                   track.duration_ms2 + T2H2 + mode + time.signature.dummy + 
                   mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*mood.Relaxed + T2H2*mood.Angry + T2H2*mood.Happy
                   ,data=dat.art.eff3) 

summary(lm.t2hxmoods) 


lm.t2hxmoods2 <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                     acousticness2 + instrumentalness2 + liveness2 + tempo2 + 
                     track.duration_ms2 + T2H2 + mode + time.signature.dummy + 
                     mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                     T2H2*mood.Relaxed + T2H2*mood.Angry + T2H2*mood.Happy + 
                      mood.Relaxed*instrumentalness2
                   ,data=dat.art.eff3) 

summary(lm.t2hxmoods2) 

# experimenting with nonlinear
lm.quad <- lm(formula = track.popularity2~ danceability2 + loudness2 + speechiness2 + 
                        acousticness2 + instrumentalness2 + I(liveness2**2) +liveness2 + 
                        I(tempo2**2) + tempo2 + I(track.duration_ms2**2) + 
                        track.duration_ms2 + T2H2 + mode + time.signature.dummy + 
                        mood.Angry + mood.Happy + mood.Relaxed + Sony + Warner + Indie + 
                        T2H2*mood.Relaxed + T2H2*mood.Angry + T2H2*mood.Happy
                        ,data=dat.art.eff3) 

summary(lm.quad) 

vif(lm.t2hxmoods)
vif(lm.quad)

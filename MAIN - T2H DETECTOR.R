# This is created by Pierre Michel B. Hardy in partial fulfillment of his master thesis
# This is the code for detecting the earliest occurenc of the hook in a song
# This is partly based on the chroma based approach used by Bartsch & Wakefield (2001)
# to detect a chorus of the song. 



# Packages required #########
library(spotifyr)
library(dplyr)
library(pheatmap)
library(readxl)
library(xlsx)



# API stuff #########
Sys.setenv(SPOTIFY_CLIENT_ID = '94af31474e8b488dbb95e8134bc9f61b')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '6e483800f9ba43e3b2faf8cf7a6f05be')

access_token <- get_spotify_access_token()



# Data laoding ##########
x <- read_excel("READY FOR T2H.xlsx",col_names=TRUE)

# making a column for T2H
x[,20] <- "x"
colnames(x)[20] <- "T2H"

# we only need the uri for the loop
z <- select(x, c("track.id"))



# Main code ########

# make a loop to run this code for every song
# in hindsight, I acknowledge making a function would've been a more elegant solution
# but I will roll with this for now
for (k in 1:nrow(z)){
  
# tryCatch so errors will not derail the process
tryCatch({
  
# extracting data
# extracting sections information
# sections are the identified different parts of the song by Echo Nest's own algorithm 
sections <- get_track_audio_analysis(z[k,1], access_token)$sections

# extracting segments information
# segments are where the song is subdivided and analysed by Echo Nest
# I need this to get information of the song's pitch in every point in a song
segments <- get_track_audio_analysis(z[k,1], access_token)$segments

# for renaming the columns 
pitches <- c("Start","Duration", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", 
             "A#", "B")

# creation of main data frame, using the sections identified by Echo Nest
main.df <- select(sections, c("start", "duration"))

# number of rows/number of distinct parts in a song
section.n <- nrow(sections)

# loop for getting the chroma representation of each part of the song
for (i in 1:(section.n-1)){
  # data wrangling in order to get the chroma representation of each section
  subseg <- filter(segments, segments$start>sections[i,1])
  subseg <-filter(subseg, subseg$start<sections[i+1,1])
  subseg.pitches <- subseg$pitches
  testmat <- matrix(unlist(subseg.pitches), ncol = 12, byrow = TRUE)
  # chroma representation is the average pitch of each section
  chroma <- colMeans(testmat)
  # more data wrangling to make one row = one chroma rep of one section of a song
  for (j in 1:length(chroma)){
    main.df[i, j+2]<-chroma[j]
  }
}

# because of how the data warngling of the loop is made, we need one more single iteration
# for the last section of the song. (to be specific, see line 66, which is gone in the 
# next part of the code)
# there's probably a more elegant way to do this but this is simple in my head and it 
# works. 
subseg <- filter(segments, segments$start>sections[section.n,1])
subseg.pitches <- subseg$pitches
testmat <- matrix(unlist(subseg.pitches), ncol = 12, byrow = TRUE)
chroma <- colMeans(testmat)
for (j in 1:length(chroma)){
  main.df[section.n, j+2]<-chroma[j]
}

# now we have the complete chroma rep of the whole song
# now make a matrix copy that only contains the pitches for the main algorithm
feat.mat <- select(main.df, c("V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13",
                              "V14"))
feat.mat <- as.matrix(feat.mat)

# here it is. The meat and potatoes of the algorithm 
# it's basically a cosine similarity calculation of one part of a song to all the other
# parts of the song
cos.sim <- function(ix) 
{
  A = feat.mat[ix[1],]
  B = feat.mat[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   

# this part actually uses the function. This basically makes a similarity matrix
n <- nrow(feat.mat) 
cmb <- expand.grid(i=1:n, j=1:n) 

# this single line is the main event
C <- matrix(apply(cmb,1,cos.sim),n,n)

###

# this is mainly fixing up for the visualization/heat map
# but it's nice to be organized
# names are the starting times of each section of the song
names <- c(as.character(round(sections$start,digits=0)))
# song.dur is the song's duration
song.dur <- round(sections[nrow(sections),1] + sections[nrow(sections),2], digits=0)
# making row and columns names proper
rownames(C) <- names
colnames(C) <- names

# this code below visualizes what this algorithm did
# it is "turned off" so the algorithm doesn't visualize while running
# feel free to "turn it on" to see pretty pictures :)
# pheatmap(C, display_numbers = TRUE, number_format = "%.7f", cluster_rows=F, 
#          cluster_cols=F)

# so now, to find the hook, we need to find the section of the song that's repeated the
# most. Then citing the earliest occurence of it as the T2H. 
# So finding the most repeated part, we first single out the column with the highest
# average similarity score because it is a sign that that column is the section of the 
# song that contains the hook
col.aves <- colMeans(C)

# this part hand picks the column with the highest average
id.chorus <- sort(col.aves, decreasing=T)[1]
id.chorus.index <- order(col.aves, decreasing=T)[1]
col.select <- C[,id.chorus.index]

# here, we select only the sections that has above median similarity
# because tho hooks are repeated, some parts are more similar than others
# artists do not just repeat parts with augmenting. They also introduce an additional
# element to keep songs from getting bland. That's why we pick the above median 
# similarities to filter out parts that are not similar but keeping those that are 
# similar enough to believe it is the hook
col.select2 <- col.select[col.select > median(col.select)]

# this picks the earliest occurance from the selection we made
# we then conclude that this is the t2h
earliest <- col.select2[1]

# we then record this
results <- main.df[which(col.select==earliest),1]
x[k,20] <- results

# prints which song is just finished analysing
# just a little way to monitor the progress of the algorithm
print(k)
# just in case one row causes us problems, we can skip it
  }, error=function(e){cat("Whoops", conditionMessage(e),"\n")})
}




# write up the final product! ##########
write.xlsx(x, "DATA COLLECTED - READY FOR ANALYSIS.xlsx")




# next steps ##########
# this concludes the data collection part of the thesis. 
# onto data analysis!


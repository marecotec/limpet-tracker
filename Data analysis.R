#####preliminary tasks

#First we need to read in the data, let's set a location

#Now lets load the files
data1 <- read.csv("07032016Field.csv",header = T) #The main data frame
data2 <- read.csv("25022016FieldDummy.csv", header = T) #data for calibration
tide1 <- read.csv("tideHeights.csv", header = T) #Tide data (will be collated after fieldwork)
temp1 <-read.csv("25022016TempData.csv", header = T) #taken from tinytag as necessary

#load libraries
#library(accelerometry) #this is for human activity
#library(GGIR) #this is also for human activity. needs a specifc brand of pedometer
#library(randomForest) #this could be used for behavioural classification
#library(adehabitatLT) #this could be used for trajectory processing but needs gps data
#library(trip) #can be used to calculate drip duration and heading. may require gps coords
#library(crawl) #Requires argos float data and GPS?
library(zoo) #we can ge a rolling mean with this
library(psych) #for statistical analysis
library(dplyr) #robust statistics package
library(quantmod) #findPeaks function
library(pracma) #adds MA functions
library(car) #adds levenes test functions

##We need to write a code to detect the waggle of the limpets
##Bearing in mind, each wagle strength is different and there is a difference in
##rapid and long waggles
##we also need to detect the duration of waggle events 
##and be able to compare responses in different environments

#define functions
ma <- function(x,n){stats::filter(x,rep(1/n,n), sides = 2)}

###########################################################################################
#plot the acceleration and heading of the entire dataset for data1. 240 is minute of data
#keep ma as 240 for best smoothing
plot.new()
par(mfrow=c(4,1))
plot(data1$xac[80000:90000], type = "l", col= "blue",  xlab = "", ylab = "x acceleration")
lines(ma(data1$xac[80000:90000], n=240), col = "black", lwd=2)

plot(data1$yac[80000:90000], type = "l", col= "blue", xlab = "", ylab = "y acceleration")#, ylim = c(-3.5,-2.5))
lines(ma(data1$yac[80000:90000], n=240), col = "black", lwd=2)

plot(data1$zac[80000:90000], type = "l", col= "blue", xlab = "", ylab = "z acceleration")
lines(ma(data1$zac[80000:90000], n=240), col = "black", lwd=2)

plot(data1$head[80000:90000], type = "l", col= "blue", xlab = "time" , ylab = "heading")
lines(ma(data1$head[80000:90000], n=240), col = "black", lwd=2)
##################################################################################
#This is to do hard and soft iron callibration if necessary

#Magnetic compensation and offset correction
Ox <- ((max(data1$xmag)+min(data1$xmag))/2)
Oy <- ((max(data1$ymag)+min(data1$ymag))/2)
Oz <- ((max(data1$zmag)+min(data1$zmag))/2)

Mx <- data1$xmag - Ox
My <- data1$ymag - Oy
Mz <- data1$zmag - Oz

#Calculate heading after hard iron correction
HeadI <- atan2(My, Mz)
HeadI <- HeadI *(180/3.141592654)

headma <- ma(HeadI, n=240)
plot(headma, lwd = 2, col = "red", ylim = c(38,43))
###############################################################################################
#Do Pitch and roll calculations for selected data sections

#Setup
Caccelx <- data1$xac[15000:25000] * (10/2^(10-1))
Caccely <- data1$yac[15000:25000] * (10/2^(10-1))
Caccelz <- data1$zac[15000:25000] * (10/2^(10-1))

#Pitch
P1 <- rad2deg(atan(Caccely/sqrt((Caccelx^2)+(Caccelz^2))))

#Roll
R1 <- rad2deg(atan((Caccelx*-1)/Caccelz))
#############################################################################
#Calculate overall dynamic body acceleration for each chunk of movement
#We could use a bin of 14400 recordings (1 hour)
#this will tell us how active the limpet is over a time period
#do we need to adjust zac (heave)?

#create ma objects
xma <- ma(data2$xac[40000:50000], n=240)
yma <- ma(data2$yac[40000:50000], n=240)
zma <- ma(data2$zac[40000:50000], n=240)
headma <- ma(data2$head[40000:50000], n=240)

#adjust  axes
#this is for a moving average for ODBA
xadj <- na.omit(data2$xac[40000:50000] - xma)
yadj <- na.omit(data2$yac[40000:50000] - yma)
zadj <- na.omit(data2$zac[40000:50000] - zma)

#Subtract moving averages from raw data
xDBA <-xadj - ma(xadj, n = 240)
yDBA <-yadj - ma(yadj, n = 240)
zDBA <-zadj - ma(zadj, n = 240)

#convert to positive
xDBA <- ifelse(xDBA < 0, xDBA *-1, xDBA)
yDBA <- ifelse(yDBA < 0, yDBA *-1, yDBA)
zDBA <- ifelse(zDBA < 0, zDBA *-1, zDBA)

#remove NAs
xDBA <- na.omit(xDBA)
yDBA <- na.omit(yDBA)
zDBA <- na.omit(zDBA)

#Now we have a moving average for each axis, we need to sum the data in
#groups of 14400 and create another object with those values.
#Then we can graph it and see the times at which the limpet is most active.
#We could compare this with the tidal data that will be downloaded at the end of each month

#I think this is how to do it for one section but I need to be able to include each section
xaDBA <- sum(xDBA)
yaDBA <- sum(yDBA)
zaDBA <- sum(zDBA)

ODBA1 <- sum(xaDBA, yaDBA, zaDBA)/750 #750 is the time it takes to do 1 waggle
ODBA2 <- sum(xaDBA, yaDBA, zaDBA)/750
###################################################################################
#We are going to make a moving window to look for sd of peaks 
#each feeding behaviour is 1500 measures or 6 minutes and 15 seconds approx
#case by case feeding behaviour (750 measures)
#functions are from the zoo package
#For a GLM we can calculate feed and non feed intervals for similar sized sections
#and compare them using the lm command?
#we should get some numbers between the 2?

#first we need to generate data for each section. Feeding
xsd <- na.omit(as.numeric(rollmean(na.omit(ma(xadj, n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))
ysd <- na.omit(as.numeric(rollmean(na.omit(ma(yadj, n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))
zsd <- na.omit(as.numeric(rollmean(na.omit(ma(zadj, n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))
headsd <- na.omit(as.numeric(rollmean(na.omit(ma(hadj, n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))



#Not feeding
xsd2 <- na.omit(as.numeric(rollmean(na.omit(ma(xadj2, n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))
ysd2 <- na.omit(as.numeric(rollmean(na.omit(ma(yadj2, n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))
zsd2 <- na.omit(as.numeric(rollmean(na.omit(ma(zadj2, n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))
headsd2 <- na.omit(as.numeric(rollmean(na.omit(ma(data2$head[135000:145000], n=240)), k = 750, width = 750, align = "left", partial  = TRUE)))
##############################################################################################################################################
#ANOVA

#import data
stats <- read.csv("StatsD.csv", header = T)


anva <- aov(data=stats, xmax ~ Day * Move )


#More ANOVA !!!!!USE THIS ONE!!!!!
layout(matrix(c(1,2,3,4),2,2)) #plots statistics
plot(anva)
summary(anva)
drop1(anva,~., test ="F") #returns ANOVA with type III rather than type I error

lf <- leveneTest(data = stats, logx ~ Type * Move)
stats$logx <- log(stats$xmax)
tuk <- TukeyHSD(anva)

#Lab results
labstats <- read.csv("labStats.csv", header = T)
labanova <- aov(data = labstats, speed ~ Sensor)
levene <- leveneTest(data = labstats,speed ~Sensor, center = mean)

plot(labanova)
drop1(labanova,~.,test= "F")
TukeyHSD(labanova)

#Between and within factor analysis
#IV between: Day
#IV within: move
mfava <- aov(logx ~ Day + Error(Move/(Day)), data=stats)
############################################################################################
#Lets flatten the x acceleration of the 23022106 data
## Close enough

summary(data1$xac)

data1$xac <- ifelse( -5.5 < data1$xac, data1$xac - 0.75,ifelse(data1$xac < -6.0, data1$xac +0.75, data1$xac +0))


par(mfrow = c(1,1))
plot(data1$xac[3000:150000], type = "l", col = "blue")
#######################################################################################################
#Colourful graphs

#format the time data
data1$milis <- as.POSIXct((data1$milis+0.1)/1000, origin = "2016-03-07 16:05:00")
data2$milis <- as.POSIXct((data2$milis+0.1)/1000, origin = "2016-02-25 18:43:00")

#create amoving average column
data1$xma1 <- ma(data1$zac, n=240)
data2$xma2 <- ma(data2$xac, n=240)

#remove 0s
data1$xma1 <- data1$xma1[is.na(data1$xma1)] <- 0
data2$xma2 <- data2$xma2[is.na(data2$xma2)] <- 0

#plot graphs
par(mfrow = c(1,1))

plot(x = data1$milis[3000:160000],y = data1$head[3000:160000], type = "l", col = "darkslategray", ylab = "Heading (Degrees)", xlab = "Time of Day",
     mar = c(1,4,4,2) + 0.1)
  lines(x = data1$milis[3000:160000], y = ma(data1$head[3000:160000], n = 240), type = "l", col = "black", lwd = 2)

  plot(x = data2$milis[142081:152081],y = data2$xac[142081:152081], type = "l", col = "darkslategray" , ylab = "Sway acceleration (G)", xlab = "Time of day",
     mar = c(1,4,4,2) + 0.1)
  lines(x = data2$milis[142081:152081], y = ma(data2$xac[142081:152081], n = 240), type = "l", col = "black") 
  
plot(x= tide$Time[1:45], y = tide$Height[1:45], type = "l", lwd = 2, col = "black", xlab = "Time of Day",
     ylab = "Tide Height (m)", mar = c(5,4,1,2)+0.1)
    


#Tidal graph - format start time data
 tide <- read.csv("allTide.csv", header = T)
 tide$Time <- tide$Time *60
 tide$Time <- as.POSIXct(tide$Time, origin = "2016-03-05 14:38:00")
 
 
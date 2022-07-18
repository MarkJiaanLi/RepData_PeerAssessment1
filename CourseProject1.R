setwd("~/Downloads")
data <- read.csv("activity.csv")
View(data)
stepsPerDate <- aggregate(steps~date, data, sum, na.rm=TRUE) 
##To sum all steps in "data" and make a new data frame
##aggregate() splits a dataframe into subsets
hist(stepsPerDate$steps, xlab="Daily Steps", ylab="Frequency/Days")

"Calculate and report the mean and median of the total number of steps taken per day
"
meansteps <- mean(stepsPerDate$steps)
mediansteps <- median(stepsPerDate$steps)

datainterval <- aggregate(steps~interval, data, mean)
View(datainterval)
timeseries <- ggplot(datainterval,aes(interval,steps))
timeseries + geom_line()

maxinterval <- datainterval[which.max(datainterval$steps),]$interval
##use which.max() to find the max steps and the corresponding interval

totalNA <- sum(is.na(data$steps))
##Show total number of NA values in "steps" column of "data" dataframe

data[is.na(data)] = 0
datanoNA <- data
stepsPerDatenoNA <- aggregate(steps~date, datanoNA, sum)
hist(stepsPerDatenoNA$steps, xlab="Daily Steps", ylab="Frequency/Days")
meanstepsnoNA <- mean(stepsPerDatenoNA$steps)
medianstepsnoNA <- median(stepsPerDatenoNA$steps)
##Replace the previous steps for the dataset without NA values

dataYesNA <- read.csv("activity.csv") ##Beacuse original "data" data frame was 
##overwritten by data[is.na(data)] = 0
dataYesNA$day <- strptime(dataYesNA[,2], "%Y-%m-%d")
dataYesNA$weekdays <- weekdays(dataYesNA$day)
df1 = subset(dataYesNA,dataYesNA$weekdays == "星期六" |
               dataYesNA$weekdays == "星期天")
df2 = subset(dataYesNA,dataYesNA$weekdays != "星期六" &
               dataYesNA$weekdays != "星期天")
##Make new columns. "day" converts the "date" into the data type 
##that can be read by weekdays() function.
##For some reason the weekdays are shown in Chinese.

df1mean <- aggregate(steps~interval, df1, mean)
View(df1mean)
df2mean <- aggregate(steps~interval, df2, mean)
View(df2mean)

par(mfrow = c(1, 2))
with(df1mean,plot(interval,steps, type="l", xlab="Interval - Weekend"))
with(df2mean,plot(interval,steps, type="l", xlab="Interval - Weekend"))

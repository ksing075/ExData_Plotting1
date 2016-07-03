Electricity <- read.table("household_power_consumption.txt", header=TRUE, sep=";")
attach(Electricity)

*Convert factor variables to numeric and change ? to NA
Active <- as.numeric(levels(Global_active_power))[Global_active_power]
Reactive <- as.numeric(levels(Global_reactive_power))[Global_reactive_power]
Vol <- as.numeric(levels(Voltage))[Voltage]
Intensity <- as.numeric(levels(Global_intensity))[Global_intensity]
S1 <- as.numeric(levels(Sub_metering_1))[Sub_metering_1]
S2 <- as.numeric(levels(Sub_metering_2))[Sub_metering_2]

*Combine above numeric variables to dataset
newdata <- cbind(Electricity, Active, Reactive, Vol, Intensity, S1, S2)

*Remove factor columns from dataset and rearranging column order
mydata <- newdata[-c(3,4,5,6,7,8)] 
mydata <- mydata[,c(1,2,4,5,6,7,8,9,3)]
colnames(mydata)[9] <- "S3"

*Changing Date class from factor to date
Date2 <- as.Date(mydata$Date, "%d/%m/%Y")
mydata2 <- cbind(mydata,Date2)
mydata2 <- mydata2[-c(1)]
mydata2 <- mydata2[,c(9,1,2,3,4,5,6,7,8)]

*Changing date and time format
mydata2$Date3 <- strptime(paste(mydata2$Date2,mydata2$Time), format="%Y-%m-%d %H:%M:%S")
mydata2$Date_Time <- as.POSIXlt(mydata2$Date3)
mydata2 <- mydata2[-c(10)]
mydata2 <- mydata2[,c(1,2,10,3,4,5,6,7,8,9)]

*Creating subset
sub <- subset(mydata2, mydata2$Date2=="2007-02-01"|mydata2$Date2=="2007-02-02")

*Plot 2
png(file="plot2.png")
plot(sub$Date_Time, sub$Active, xlab="", ylab="Global Active Power (kilowatts)", type="l")
axTicks(side=1)
axis(side=1, at=c(1170304800,1170400000, 1170479200), labels=FALSE, lwd=2, lwd.ticks=0)
axis(side=2,lwd=2, lwd.ticks=1)
dev.off()

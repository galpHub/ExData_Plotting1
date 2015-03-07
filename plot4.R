## This function displays plot4 on a new screen graphical device as well as saves
## a copy of the plot in png format in the current directory.
plot4 <- function(filename){
	data<- readFile(filename)
	plotGraphs(data,4)

	## Saves the plot on the currently active graphical device to a png file
	## named "plot4.png" with a width and height of 480 pixels
	png(filename = "plot4.png", width = 480, height = 480, units = "px")
}



## This function contains all the plotting commands for the four plots: plots1,
## plots2, plots3 and plots4. 
## 'n'	Integer taking values from 1 to 4, determines which plots it 
##		produces.
## 'data'	data.frame containing the data to be used in plotting. Intended to
##		take as input the output of readFile("data_file.txt")
##
## Note: Each time this function is called it creates a screen graphical device
## on which to draw the desired plot, this is done by calling dev.new()

plotGraphs <- function(data,n){

	if(n==1){
	## FIRST PLOT. Histogram of Global Active Power (kilowatts)
	dev.new()
	with(data,hist(as.numeric(Global_active_power),xlab = "Global Active Power (kilowatts)",col = "red", main = "Global Active Power") )
	
	}else if(n==2){
	## SECOND PLOT. Plot of Global Active Power (kilowats) vs. Time of the week
	dev.new()
	with(data, plot(datesToTime,Global_active_power,xaxt='n',type="l",ylab ="Global Active Power (kilowatts)"))
	axis(1, c(0,.5,1), c("Thu","Fri","Sat"))
	
	}else if(n==3){
	## THIRD PLOT. Plot of the various Energy sub_metering variables vs. Time of the Week
	dev.new()
	with(data, plot(datesToTime, Sub_metering_1, xaxt='n',type="l",ylab ="Energy sub metering"))
	with(data, points(datesToTime, Sub_metering_2,type="l",col="red" ) )
	with(data, points(datesToTime, Sub_metering_3,type="l",col="blue") )
	axis(1, c(0,.5,1), c("Thu","Fri","Sat"))
	legend("topright",lty=c(1,1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
	
	}else if(n==4){
	## FOURTH PLOT. Four plots of various electricity consumption metrics vs. Time of the Week.
	dev.new()
	par(mfrow = c(2,2))

	## Plot in (1,1)
	with(data, plot(datesToTime,Global_active_power,xaxt='n',type="l",ylab ="Global Active Power (kilowatts)"))
	axis(1, c(0,.5,1), c("Thu","Fri","Sat"))

	## Plot in (1,2)
	with(data, plot(datesToTime,Voltage,xaxt='n',type="l",ylab ="Voltage",xlab="datetime"))

	## Plot in (2,1)
	with(data, plot(datesToTime, Sub_metering_1, xaxt='n',type="l",ylab ="Energy sub metering"))
	with(data, points(datesToTime, Sub_metering_2,type="l",col="red" ) )
	with(data, points(datesToTime, Sub_metering_3,type="l",col="blue") )
	axis(1, c(0,.5,1), c("Thu","Fri","Sat"))
	legend("topright",bty='n',lty=c(1,1,1),col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
	
	## Plot in (2,2)
	with(data, plot(datesToTime,Global_active_power,xaxt='n',type="l",xlab="datetime"))
	axis(1, c(0,.5,1), c("Thu","Fri","Sat"))
	}
}





readFile <- function(filename){
	## Reads the file as a character vector to avoid increasing size
	textLines <- readLines(filename)

	## Extract variable names from first line
	variableNames <- strsplit(textLines[1],split =";")[[1]]

	## Finds regular expressions for the first and second days.
	## Note that the expressions for Day1 and Day2 in grep look
	## only for expressions that are x/y/2007 where x is 1 or 01
	## and y is either 2 or 02
	Day1 <- "^0?1/0?2/2007"
	Day2 <- "^0?2/0?2/2007"
	indicesDay1 <- grep(Day1,textLines)
	indicesDay2 <- grep(Day2,textLines)
	indices <- c(indicesDay1,indicesDay2)

	## Takes the subset of the lines which are matches for Day1 or
	## Day2
	textLines <- textLines[indices]
	
	## Constructs a list of lists, each sublist containing the 
	## measurement in each of the relevant lines of the source file
	## which is separated by ";" (no quotations)
	data <- sapply(textLines,function(x) strsplit(x,split = ";") )

	## Transforming the list into a dataframe with variables as rows and
	## immediately transposing to have variables as columns. Variable names
	## are assigned at the end to the columns.
	data <- as.data.frame(data)
	row.names(data)<- variableNames
	data <- as.data.frame(t(data))
	row.names(data) <- 1:dim(data)[1]
	## Numeric variables that have been coerced into factor variables are converted
	## back to numeric variables by passing first through character format.
	for(j in 3:9){
		data[,j] <- as.numeric(as.character(data[,j]))
	}

	data[,"datesToTime"] <- sapply(1:dim(data)[1],function(j) dateToTimeFunction(data[j,1:2]))
	return(data)
}




## The function takes as input a data frame with 1 row and 2 columns containing 
## the date and time of a particular measurement, and outputs a numeric variable
## between 0 and 1 that quantifies at what fraction of the 48 hours a measurement
## was made, e.g. .5 indicates midnight of the first day.

dateToTimeFunction <- function(date_timePair){
	Day1 <- "1/2/2007"

	## Makes a character vector of length 2 containing the date and time of the
	## measurement.
	characterVector <- sapply(date_timePair, function(v) as.character(v))
	
	## Total number of seconds in 1 day, out of the two.
	half_totalSeconds <- 24*3600
	
	## Numeric vector containing the number of seconds in an c("hour","minute","second")
	secs_hoursMinutesSecs <- c(3600,60,1) 

	## Splits the string H:M:S (i.e. hours:minutes:seconds) along the ":" symbol and
	## converts the values of H,M,S to numeric. 
	timeVector <- sapply(strsplit(characterVector[2],split=":"), function(v) as.numeric(v))

	## If the value of time is missing, the time vector will not have length 3.
	if(length(timeVector)<3){
		return(NA)
	}

	## Computes the fraction of time out of 2 days that has transpired up 
	## until this particular measurement
	((characterVector[1]!=Day1) +  sum(timeVector*secs_hoursMinutesSecs)/half_totalSeconds)/2
}
# this part loads the file and creates a data frame, then there are functions for each plot
# load_files from the website
if (!file.exists("./data")){dir.create("data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
download.file(fileUrl, destfile = "./data/household_power_consumption.zip", method = "curl")

# unzip the file
unzip("./data/household_power_consumption.zip", exdir = "./data")

# load data 
cons5rows <- read.table("./data/household_power_consumption.txt", sep = ";", header = TRUE, nrows = 5, na.strings = "?", stringsAsFactors = FALSE)
classes <- sapply(cons5rows, class)
consumption <- read.table("./data/household_power_consumption.txt", sep = ";", header = TRUE, colClasses = classes, na.strings = "?", stringsAsFactors = FALSE)
consumption$Date = as.Date(consumption$Date, "%d/%m/%Y")
consFeb2007 <- subset(consumption, Date == as.Date("2007-02-01") | Date == as.Date("2007-02-02"))
rm(consumption)
consFeb2007$DateTime = paste(consFeb2007$Date, consFeb2007$Time)
consFeb2007$DateTime = strptime(consFeb2007$DateTime, format = "%Y-%m-%d %H:%M:%S")
print("Consumption file loaded! - data frame name is consFeb2007")


plot_1 <- function(){
    hist(consFeb2007$Global_active_power, xlab = "Global actve power (kilowatts)", ylab = "Frequency", col = "red", main = "Global Active Power")
    dev.copy(png, file = "plot1.png", width = 480, height = 480)
    dev.off()
}

plot_2 <- function(){
    
    with(consFeb2007, plot(DateTime, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)"))
    dev.copy(png, file = "plot2.png", width = 480, height = 480)
    dev.off()
}

plot_3 <- function(x) {
    
    library(reshape2)
    meltedCons <- melt(consFeb2007, id = c("Date","Time"), measure.vars = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
                       variable.name = "Sub_metering_group", value.name = "Sub_metering_value", variable.factor = TRUE, value.factor = FALSE)
    
    meltedCons$DateTime = paste(meltedCons$Date, consFeb2007$Time)
    meltedCons$DateTime = strptime(meltedCons$DateTime, format = "%Y-%m-%d %H:%M:%S")
    
    plot(meltedCons$DateTime, meltedCons$Sub_metering_value, type = "n", xlab = "", ylab = "Energy sub metering")
    with(meltedCons, points(DateTime[Sub_metering_group=="Sub_metering_1"], Sub_metering_value[Sub_metering_group=="Sub_metering_1"], type = "l"))
    with(meltedCons, points(DateTime[Sub_metering_group=="Sub_metering_2"], Sub_metering_value[Sub_metering_group=="Sub_metering_2"], type = "l", col = "red"))
    with(meltedCons, points(DateTime[Sub_metering_group=="Sub_metering_3"], Sub_metering_value[Sub_metering_group=="Sub_metering_3"], type = "l", col = "blue"))
    legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), text.width = 80000, y.intersp = 2)
    
    dev.copy(png, file = "plot3.png", width = 480, height = 480)
    dev.off()
}

plot_4 <- function(x) {
    plot_2()
    plot_3()
    
    with(consFeb2007, plot(DateTime, Voltage, type = "l", xlab = "datetime", ylab = "Voltage"))
    with(consFeb2007, plot(DateTime, Global_reactive_power, type = "l", xlab = "datetime"))
    
    dev.copy(png, file = "plot4.png", width = 480, height = 480)
    dev.off()
    
}


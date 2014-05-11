

getData<-function(fileName, regExpr){
        
        
        fileHandle <- file(fileName,"rt");
        
        data <- readLines(fileHandle,n=1)               #read the file header  
        
        
        while(TRUE) {
                
                
                theLines = readLines(fileHandle,n=100)  # read in chunks of 100 lines
                rowIndex <- grep(regExpr, theLines)     # search for date pattern 
                data<-c(data, theLines[rowIndex])       # add the matching lines
                
                
                if(length(theLines)!=100) {
                        break                           #end of file
                        
                }
                
        }
        
        close(fileHandle)
        
        out <- textConnection(data,"rt") 
        
        getData <- read.csv(out, sep=";",header=TRUE)
        
        
        
}



makePlot4_a<-function(data){
        
        
        
        d<-data
        
        d$Date <-as.Date(data$Date, "%d/%m/%Y")
        
        x <- paste(d$Date, d$Time)
        
        d$dt <- as.POSIXlt(x)
        
        
        plot(d$dt, d$Global_active_power,  type="l", xlab="", ylab="Global Active Power")
        
        
        
}

makePlot4_b<-function(data){
        
        
        
        d<-data
        
        d$Date <-as.Date(data$Date, "%d/%m/%Y")
        
        x <- paste(d$Date, d$Time)
        
        d$dt <- as.POSIXlt(x)
        
        
        plot(d$dt, d$Voltage,  type="l", xlab="datetime", ylab="Voltage")
        
        
        
}


makePlot4_c<-function(data){
        
        d<-data 
        
        d<-getData("household_power_consumption.txt", "^[12]/2/2007")
        
        d$Date <-as.Date(d$Date, "%d/%m/%Y")
        
        x <- paste(d$Date, d$Time)
        
        d$dt <- as.POSIXlt(x)
        
        
        plot(d$dt, data$Sub_metering_1,  type="l", ylab="Energy sub metering", col="black", xlab="", axes=FALSE)
        
        axis(1, at=c(as.numeric(min(d$dt)), as.numeric(min(d$dt)+86400),  as.numeric(min(d$dt)+2*86400)),     labels=c("Thu", "Fri", "Sat"))          
        axis(2, at=seq(0,30,by=10))
        
        
        lines(d$dt, d$Sub_metering_2, col="red")
        lines(d$dt, d$Sub_metering_3, col="blue")
        
        legend("topright", c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),  lwd=c(2.5,2.5,2.5),col=c("black","red","blue")) 
        
        box()
        
}

makePlot4_d<-function(data){
        
        
        
        d<-data
        
        d$Date <-as.Date(data$Date, "%d/%m/%Y")
        
        x <- paste(d$Date, d$Time)
        
        d$dt <- as.POSIXlt(x)
        
        
        plot(d$dt, d$Global_reactive_power,  type="l", xlab="datetime", ylab="Global_reactive_power")
        
        
        
}

plot4<-function(){
        
        data<-getData("household_power_consumption.txt", "^[12]/2/2007")
        
        png(filename="plot4.png", width = 480, height = 480)
        par(mfrow=c(2,2))
        makePlot4_a(data)
        makePlot4_b(data)
        makePlot4_c(data)
        makePlot4_d(data)
        dev.off()
        
        
}


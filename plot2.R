

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


makePlot2<-function(data){
        
        
        
        d<-data
        
        d$Date <-as.Date(data$Date, "%d/%m/%Y")
        
        x <- paste(d$Date, d$Time)
        
        d$dt <- as.POSIXlt(x)
        
        
        plot(d$dt, d$Global_active_power,  type="l", xlab="", ylab="Global Active Power (kilowatts)")
        
        
        
}



plot2<-function(){
        
        data<-getData("household_power_consumption.txt", "^[12]/2/2007")
        
        
        png(filename="plot2.png", width = 480, height = 480)
        makePlot2(data)
        dev.off()
        
}




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


makePlot1<-function(data){
        
        
        d<-data
        hist(d$Global_active_power, main="Global Active Power", col="red",xlab="Global Active Power (kilowatts)")
        
}



plot1<-function(){
        
        data<-getData("household_power_consumption.txt", "^[12]/2/2007")
        
        png(filename="plot1.png", width = 480, height = 480)
        makePlot1(data)
        dev.off()
        
}









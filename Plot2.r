{
## Plot 2 for Project for Week 1 of Coursera's "Exploratory Data Analysis" - Run Chart
  ## Instructions:
  ## Data downloaded from https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
  ## We will only be using data from the dates 2007-02-01 and 2007-02-02
  ## Our overall goal here is simply to examine how household energy usage varies over a 2-day period in February, 2007. 
  ## Your task is to reconstruct the following plots below, all of which were constructed using the base plotting system.
  ##  For each plot you should
  ##  •  Construct the plot and save it to a PNG file with a width of 480 pixels and a height of 480 pixels.
  ##  •  Name each of the plot files as plot1.png, plot2.png, etc.
  ##  •  Create a separate R code file (plot1.R, plot2.R, etc.) that constructs the corresponding plot, i.e. code in plot1.R constructs the plot1.png plot. 
  ##    Your code file should include code for reading the data so that the plot can be fully reproduced. You should also include the code that creates the PNG file.
  ##  •	Add the PNG file and R code file to your git repository
  ## When you are finished with the assignment, push your git repository to GitHub so that the GitHub version of your repository is up to date. 
  ## There should be four PNG files and four R code files.
  
  ## setwd("C:/Data/Coursera/4 ExploritoryDataAnalysis/Week1")  
  
  ##  Package to check for required package, install it if missing, and load it    
  packages<-function(x){
    x<-as.character(match.call()[[2]])
    if (!require(x,character.only=TRUE)){
      install.packages(pkgs=x,repos="http://cran.r-project.org")
      require(x,character.only=TRUE)
    }
  }

  if (!exists("ds1")){
 
    ## Get the zip file if it doesn't exist in working directory
    zipurl <- "http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
    destfile <- "exdata-data-household_power_consumption.zip"
    workfile <- "./household_power_consumption.txt"
    
    ## if workfile doesn't exist in working directory, get it from the zip file
    if (!file.exists(workfile)){
      # download the zip file if it isn't in the working directory
      
      packages(utils) ## used for the unzip function
      if (!file.exists(destfile)) {
        download.file(zipurl, destfile)
        unzip(destfile, overwrite = TRUE)
      }
    }
    else {
      # Unzip the archive in working directory
      unzip(destfile, overwrite = TRUE)
    }
   # read first few lines to get classes of fields 
    tab5rows <- read.table("household_power_consumption.txt", header = TRUE, sep=";", nrows = 5, na.strings="?")
    classes <- sapply(tab5rows, class)
    ds1 <- read.table("household_power_consumption.txt", header = TRUE, sep=";", colClasses = classes, nrows = 2075260, na.strings="?")
    dsNames <- names(ds1)
  
    # add new column with both date and time   
    ds1<-cbind(ds1,paste(ds1[,1],ds1[,2], sep = " "))  
    names(ds1)<-c(dsNames,"DateTimeText")   ## adds field name to new column
    ## change first column to date format
    ds1[,1] <- as.Date(as.character(ds1$Date), "%d/%m/%Y")
  }
  ds2 <-subset(ds1,ds1$Date>=as.Date("2007-02-01","%Y-%m-%d") & ds1$Date<=as.Date("2007-02-02","%Y-%m-%d"))
  rm(tab5rows)
  #rm(ds1) -- clear memory of unneeded data frame  ##decided to leave it in in case plots are run consecutively
  
  # add converted data/time field and day of week field
  dsNames <- names(ds2)
  ds2<-cbind(ds2,strptime(as.character(ds2[,10]), "%d/%m/%Y %H:%M:%S"))
  names(ds2)<-c(dsNames,"DateTimePOSIX")
  #count plot points for Thursday
  ThuCt<-NROW(subset(ds2,weekdays(ds2[,1],abbreviate=TRUE)=="Thu")) #how many rows from Thursday
  #count plot points for Friday
  FriCt<-NROW(subset(ds2,weekdays(ds2[,1],abbreviate=TRUE)=="Fri")) #how many rows from Friday

  packages(graphics) 
 
  # plots power against the index vector (each measurement gets a point)
  plot(x=as.numeric(ds2$Global_active_power), type = "l",xaxt="n", xlim = c(0,ThuCt+FriCt), xaxs="i", ylab = "Global Active Power (kilowatts)", xlab="")
  # plots tic marks and labels at appropriate point on axis 
  axis(side=1,at=seq(0,ThuCt+FriCt,ThuCt),labels=c("Thu","Fri","Sat"))
  
  # save plots to working directory
  png(filename="./plot2.png",height=480, width=480,bg="white")
  # plots power against the index vector
  plot(x=as.numeric(ds2$Global_active_power), type = "l",xaxt="n", xlim = c(0,ThuCt+FriCt), xaxs="i", ylab = "Global Active Power (kilowatts)", xlab="")
  # plots tic marks and labels at appropriate point on axis 
  axis(side=1,at=seq(0,ThuCt+FriCt,ThuCt),labels=c("Thu","Fri","Sat"))
  
  dev.off()
}


#axis(side=2, at=seq(0,6, 2), labels=seq(0,6,2)) #don't need
#y = ds2$Dayofwk,axes=FALSE, ann=FALSE,  #don't need
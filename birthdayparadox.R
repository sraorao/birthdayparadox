 # download and unzip http://www.nber.org/data/vital-statistics-natality-data.html  
 # 2013  
 download.file(url = "http://www.nber.org/natality/2013/natl2013.csv.zip", destfile = "./R programming assignments/births/natl2013.csv.zip")  
 unzip(zipfile = "./R programming assignments/births/natl2013.csv.zip", exdir = "./R programming assignments/births")  
 births2013 <- fread(input = "./R programming assignments/births/natl2013.csv") # ~ 2.5 GB file  
 # 1988  
 download.file(url = "http://www.nber.org/natality/1988/natl1988.csv.zip", destfile = "./R programming assignments/births/natl1988.csv.zip")  
 unzip(zipfile = "./R programming assignments/births/natl1988.csv.zip", exdir = "./R programming assignments/births")  
 births1988 <- fread(input = "./R programming assignments/births/natl1988.csv", select = c("birmon", "birday")) # ~ 1.26 GB  
 # generate useful vectors  
 daysinmonth <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  
 daysinmonthleap <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)  
 # explore  
 birthsbymonth2013 <- table(births2013$dob_mm) # births by month  
 png(filename = "./R programming assignments/births/birthsbymonth2013_norm.png", width = 500, height = 500)  
 plot(birthsbymonth2013/daysinmonth, ylim = c(0,12000), type = "l") # normalised to no. of days in each month  
 dev.off()  
 table(births2013$dob_wk) # births by week day  
 table(births2013$dob_tt) # births by time  
 birthsbymonth1988 <- table(births1988$birmon) # births by month  
 png(filename = "./R programming assignments/births/birthsbymonth1988.png", width = 500, height = 500)  
 plot(birthsbymonth1988, type = "l") # raw  
 dev.off()  
 png(filename = "./R programming assignments/births/birthsbymonth1988_norm.png", width = 500, height = 500)  
 plot(birthsbymonth1988/daysinmonthleap, ylim = c(0,12000), type = "l") # normalised to no. of days in each month (leap year)  
 dev.off()  
 birthsbydayofmonth1988 <- table(births1988$birday) # births by day of month  
 plot(birthsbydayofmonth1988) # births by day of month  
 birthsbydate1988 <- table(births1988$ddmm)  
 plot(birthsbydate1988)  
 # add columns to data  
 births1988[,ddmm:=paste(birday, birmon, sep = "-")] # new column with day and month combined  
 # The Birthday Problem  
 rowsin1988 <- nrow(births1988) # store number of rows to avoid calculating in each iteration of for loop  
 bdayproblem <- data.table("samplesize" = integer(0), "duplicates" = integer(0)) # empty data atable  
 # function returns list of samplesize and boolean of presence of 1 or more duplicates (0 = no duplicates, 1 = at least 1 duplicate)  
 calcduplicates <- function(data = births1988, samplesize = 70, numberofrows = rowsin1988){  
  return(list(samplesize, as.numeric(sum(duplicated(data[sample(numberofrows, samplesize), 3, with = FALSE])) > 0)))  
 }  
 # for loop  
 for (g in 2:100) { # in a group of g people  
  for (n in 1:1000) { # perform random sampling n times  
   bdayproblem <- rbind(bdayproblem, calcduplicates(samplesize = g))  
  }  
 }  
 # summarise data  
 samplesizevsprobability <- tapply(X = bdayproblem$duplicates, INDEX = bdayproblem$samplesize, mean) # calculate mean of the "duplicates" column  
 samplesizevsprobability <- data.table(samplesize = as.numeric(names(samplesizevsprobability)), probability = samplesizevsprobability) # convert to dataframe  
 # plot  
 png(filename = "./R programming assignments/births/alldata_probability.png", width = 1550, height = 1000)  
 ggplot(data = samplesizevsprobability, aes(x = samplesize, y = probability)) + geom_line(color = "darkblue", size = 2) + geom_smooth(color = "black") + geom_vline(aes(xintercept = 23), linetype = 2) + theme(axis.text = element_text(size=20), axis.title = element_text(size=20, face = "bold"))# plot data  
 dev.off()  

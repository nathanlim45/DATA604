data1 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_1995_2000.csv")
data2 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2001_2005.csv")
data3 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2006_2010.csv")
data4 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2011_2015.csv")

data_19952015 <- rbind(data1, data2, data3, data4)

head(data_19952015)

data_2016 <- read.csv(file="https://raw.githubusercontent.com/nathanlim45/DATA604/master/climate_data/jfk_2016_Oct16.csv")
head(data_2016)

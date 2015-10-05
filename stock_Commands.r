IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble =  read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

#Convert the date information to a date object
IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")

#Find the number of observations
str(IBM)
str(GE)

#Find the earliest/latest date in a data set. Look at highest/lowest values
summary(IBM$Date)

#Find the standard deviation of stock price for Proctor and Gamlbe
sd(ProcterGamble$StockPrice)

#Plot the data to find a pattern, add a line, add a color
plot(CocaCola$Date, CocaCola$StockPrice, type="l")
lines(ProcterGamble$Date,ProcterGamble$StockPrice col="red")

#Combine the data in a graph
plot(CocaCola$Date, CocaCola$StockPrice, type="l",col="red")
lines(ProcterGamble$Date,ProcterGamble$StockPrice, col="blue")

#draws a vertical line at the date, and lwd makes the line thicker(line width)
abline(v=as.Date(c("2000-03-01")),lwd=2)

#look at the stock prices from 1995 to 2005 for coke, which are values 301-402. YLim limits the Y axis, starting from 0 and ending at 210.

plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],type="l",col="red",ylim=c(0,210))

lines(ProcterGamble$Date[301:432],ProcterGamble$StockPrice[301:432],col = "blue")

lines(IBM$Date[301:432],IBM$StockPrice[301:432],col = "green")

lines(GE$Date[301:432],GE$StockPrice[301:432],col = "purple")

lines(Boeing$Date[301:432],Boeing$StockPrice[301:432],col = "orange")
abline(v=as.Date(c("2000-03-01")),lwd=2)

abline(v=as.Date(c("1997-09-01")),lwd=2)
abline(v=as.Date(c("1997-11-01")),lwd=2)

#Find average stock price for IBM
mean(IBM$StockPrice)

#compare monthly averages of stock prices
tapply(IBM$StockPrice,months(IBM$Date),mean)
tapply(GE$StockPrice,months(GE$Date),mean)
tapply(CocaCola$StockPrice,months(CocaCola$Date),mean)

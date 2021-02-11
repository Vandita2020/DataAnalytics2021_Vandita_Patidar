#Creating data frames

v1 <- 1:10
v2 <- letters[1:10]
df <- data.frame(v1,v2)
df

days <- c("Mon","Tue","Wed","Thur","Fri","Sat","Sun")
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
RPI_weather_week <- data.frame(days,temp,snowed)
RPI_weather_week

empty.dataframe <- data.frame() #empty data frame


#Exploring data frame

head(RPI_weather_week)
str(RPI_weather_week) #Structure of data frame
summary(RPI_weather_week)
RPI_weather_week[1,]
RPI_weather_week[,1]
RPI_weather_week[,"days"]
RPI_weather_week[,"temp"]
RPI_weather_week[,"snowed"]
RPI_weather_week[1:5,c('days','temp')]
RPI_weather_week$temp

subset(RPI_weather_week,subset=snowed=='T') #subset

help("order")
sorted.snowed <- order(RPI_weather_week['snowed']) #sorting data in order
sorted.snowed
RPI_weather_week[sorted.snowed,]

sorted.temp <- order(RPI_weather_week['temp']) #sorting data in ascending order
sorted.temp
RPI_weather_week[sorted.temp,]

sorted.temp <- order(RPI_weather_week['temp'],decreasing = TRUE) #sorting data in descending order
#sorted.temp <- order(-RPI_weather_week['temp'])
sorted.temp
RPI_weather_week[sorted.temp,]

#importing data frame
help("write.csv")
write.csv(RPI_weather_week,file="RPI_weather_week.csv")
df2 <- read.csv(file.choose())

#Exercise - getting data in
rd <- read.csv(file.choose())
rd
attach(rd)
continet_order <- order(rd['Continent'])
rd['continent_order',]
summary(rd$Continent)
summary(rd$Area, na.rm=T)    #why not showing 5 number summary?
summary(rd$PopulationPerUnit)
hist(rd$PopulationPerUnit)
plot(rd$Area,rd$PopulationPerUnit, na.rm=TRUE)
hist(rd$Area,rd$PopulationPerUnit, na.rm=TRUE)

#Exercise 1 - Exploring the distribution
EPI_data <- read.csv(file.choose())
View(EPI_data)
attach(EPI_data)
fix(EPI_data)
help("fix")

help("is.na")
tf <- is.na(EPI) #to filter NA values 
E <- EPI[!tf]
View(E)
E

summary(EPI)
fivenum(EPI)
stem(EPI) #stem plot
help("stem")
hist(EPI)
help("hist")
hist(EPI, seq(30,95,1)) #against frequency
hist(EPI, seq(30,95,1), prob=T) #against density
lines(density(EPI,na.rm=TRUE,bw=1)) #plotted with density only
lines(density(EPI,na.rm=TRUE,bw='SJ'))
rug(EPI)

#Exercise 1 - fitting a distribution

help(ecdf) #empirical cumulative density function
plot(ecdf(EPI),do.points=TRUE,verticals = FALSE)
plot(ecdf(EPI),do.points=FALSE,verticals = TRUE)

help("par")
par(pty='s') #type of plot region(s-square plot or m-maximal plot)
qqnorm(EPI) #QQplot
qqline(EPI) #line at 45degree

help("ppoints")
x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='QQ plot for tdsn')
qqline(x)

summary(DALY)
plot(ecdf(DALY),do.points=FALSE,verticals=TRUE)
par(pty='s')
qqnorm(DALY)
qqline(DALY)
qqplot(qt(ppoints(100),df=5),x)
qqline(x)

par(pty='m')
boxplot(EPI,DALY)
qqplot(EPI,DALY)

boxplot(AIR_H,AIR_E)
boxplot(WATER_H,WATER_E)

#Exercise 2- Filtering

t <- is.na(EPI)
EPI1 <- EPI[!t]
EPI1

E <- EPI[!Landlock]
E_land <- EPI[!is.na(E)]
summary(E_land)
fivenum(E_land)
hist(E_land)
hist(E_land,seq(30,95,1),prob=TRUE)
lines(density(E_land, na.rm=TRUE, bw="SJ"))
qqnorm(E_land)
qqline(E_land)
qqplot(qt(ppoints(250),df=5),x)
qqline(x)
stem(E_land)
plot(ecdf(E_land),vertical=TRUE,do.points=FALSE)

W <- EPI[!No_surface_water]
No_water <- EPI[!is.na(W)]
No_water
summary(No_water)
hist(No_water)


summary(EPI_regions)
View(EPI_regions)
EPI_regions
EPI_South_Asia <- EPI['South_Asia']
View(EPI_South_Asia)
help(distribution)
View(EPI)

#GPW3_GRUMP

GPW <- read.csv(file.choose())
GPW
View(GPW)

attach(GPW)
summary(PopulationPerUnit)
fivenum(PopulationPerUnit)
hist(PopulationPerUnit)
#stem(PopulationPerUnit)
help("seq")
hist(PopulationPerUnit, seq(), prob=T)
lines(density(PopulationPerUnit,na.rm=T, bw = "SJ"))
rug(PopulationPerUnit)
plot(ecdf(PopulationPerUnit), do.points=T, verticals = T)
qqnorm(PopulationPerUnit)
qqline(PopulationPerUnit)
qqplot(qt(ppoints(550), df=5), PopulationPerUnit)
qqline(PopulationPerUnit)
fivenum(PopulationPerUnit)
pop <- GPW['PopulationPerUnit']
p <- pop<119
View(p)
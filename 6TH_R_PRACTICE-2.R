
avocado <- read.csv("C:/Users/Prajakta Sable/Downloads/avocado_prices_20211214.csv")
avocado

View(avocado)
head(avocado)
colnames(avocado)
str(avocado)
summary(avocado)
glimpse(avocado) # IT'S COME UNDER THE DPLYR PACKAGE

Average_price <- avocado %>% select(Date,AveragePrice,type, region)


library(plyr)
library(ggplot2)
library(dplyr)

count(avocado, 'region')
c <- table(count(avocado, 'AveragePrice'))

#SMALL AVOCADOES
count(avocado$X4046)
mean(avocado$X4046)
median(avocado$X4046)

#MEDIUM AVOCADO
count(avocado$X4225)
mean(avocado$X4225)
median(avocado$X4225)

#LARGE AVOCADO
count(avocado$X4770)
mean(avocado$X4770)
median(avocado$X4770)

#AVERAGE PRICE OF AVOCADO
mean(avocado$AveragePrice)
median(avocado$AveragePrice)
freq <- tapply(avocado$AveragePrice,avocado$AveragePrice,length)
as.numeric(names(freq)[which.max(freq)]) #MODE OF PP

#HISTOGRAM
hist(avocado$AveragePrice,xlab="Average Price",main ="Frequency of Average Price")
hist(avocado$X4046,xlab="SMALL AVOCADO",main ="Frequency of SMALL AVOCADO")
hist(avocado$X4225,xlab="MEDIUM AVOCADO",main ="Frequency of MEDIUM AVOCADO")
hist(avocado$X4770,xlab="LARGE AVOCADO",main ="Frequency of LARGE AVOCADO")

table(avocado$region)
table(avocado$type)
summary(avocado$AveragePrice)
summary(avocado$Total.Volume)


#Statistically lumping of price and product type
aggregate (avocado$AveragePrice ~ avocado$type, FUN = mean)
aggregate (avocado$AveragePrice ~ avocado$type, FUN = max)
aggregate (avocado$AveragePrice ~ avocado$type, FUN = min)

#Statistically lumping of price and product type
aggregate (avocado$AveragePrice ~ avocado$region, FUN = mean)
aggregate (avocado$AveragePrice ~ avocado$region, FUN = max)
aggregate (avocado$AveragePrice ~ avocado$region, FUN = min)

#Price vs Year
aggregate (avocado$AveragePrice ~ avocado$year, FUN = mean)
aggregate (avocado$AveragePrice ~ avocado$year, FUN = max)
aggregate (avocado$AveragePrice ~ avocado$year, FUN = min)


#co-relation of average ppp & total volume
cor(avocado$Total.Volume,avocado$AveragePrice)

# Price vs Region

table(avocado$region)
table(avocado$type)
summary(avocado$AveragePrice)
summary(avocado$Total.Volume)


#TOTAL VOLUMS VS AVERAGE PP
plot( avocado$Total.Volume~ avocado$AveragePrice, avocado)
regr <- lm(avocado$Total.Volume ~ avocado$AveragePrice, avocado)
abline(regr, col='BLUE')

coefficients(regr)

#TOTAL VOLUMS VS SMALL BAGS , MEDIUM BAGS
plot(avocado$Total.Volume~ avocado$X4046, avocado)
regr <- lm(avocado$Total.Volume ~avocado$X4046)
abline(regr, col("BLUE"))

#TOTAL VOLUMS VS MEDIUM BAGS
plot(avocado$Total.Volume~ avocado$X4225, avocado)
regr <- lm(avocado$Total.Volume ~avocado$X4225)
abline(regr, col("BLUE"))

plot(avocado$Total.Volume~ avocado$X4770, avocado)
regr <- lm(avocado$Total.Volume ~avocado$X4770)
abline(regr, col("BLUE"))

conventional <- ifelse(avocado$type == 'conventional', 1, 0) # HAVING 18249 PRODUCTS.
organic <- ifelse(avocado$type == 'organic', 1, 0) #HAVING 259 PRODUCTS.

#CREATED REGRESSION MODEL ON DUMMY VARIABLES .
avocado_reg <- data.frame(Date = avocado$Date, X = avocado$X, AveragePrice = avocado$AveragePrice, Total.Volume= avocado$Total.Volume
                     , X4046= avocado$X4046, X4225 = avocado$X4225, X4770 = avocado$X4770, Total.Bags= avocado$Total.Bags,
                     Small.Bags= avocado$Small.Bags, XLarge.Bags = avocado$Large.Bags, Large.Bags= avocado$Large.Bags,
                     year= avocado$year, region=avocado$region, type= avocado$type, conventional= conventional, organic= organic)

model <- lm(Total.Volume ~ X4046 + X4225 + X4770, data= avocado_reg)
summary(model)


#SCATTER PLOTS
#Create a scatterplot with multiple regression lines fot TOTAL VOLUMS VS X4046

plot(avocado$Total.Volume ~ avocado$X4046, pch = 19, col = "gray52")
# Underlying model
#lines(seq(0, 1, 0.05), 2 + 3 * seq(0, 1, 0.05)^2, col = "2", lwd = 3, lty = 2)
# Linear fit
abline(lm(avocado$X4046 ~ avocado$Total.Volume), col = "orange", lwd = 3)
# Smooth fit
#lines(lowess(avocado$Total.Volume, avocado$X4046), col = "blue", lwd = 3)
# Legend
legend("topleft", legend = c("Theoretical", "Linear", "Smooth"),
       lwd = 3, lty = c(2, 1, 1), col = c("red", "orange", "blue"))

#2.Create a scatterplot with multiple regression lines fot TOTAL VOLUMS VS X4225
plot(avocado$Total.Volume ~ avocado$X4225, pch = 19, col = "gray58")

abline(lm(avocado$X4225 ~ avocado$Total.Volume), col = "Yellow", lwd = 3)



#3.Create a scatterplot with multiple regression lines fot TOTAL VOLUMS VS X4225

plot(avocado$Total.Volume ~ avocado$X4770, pch = 19, col = "gray58")

# Linear fit
abline(lm(avocado$X4770 ~ avocado$Total.Volume), col = "Black", lwd = 3)





#LR
conmod <- avocado %>%
  filter(type=="conventional") %>%
  mutate(Volume = log(`Total.Volume`)) %>%
  lm(AveragePrice ~ Volume, data = .)

orgmod <- avocado %>%
  filter(type=="organic") %>%
  mutate(Volume = log(`Total.Volume`)) %>%
  lm(AveragePrice ~ Volume, data = .)

summ(conmod, orgmod)
library(gtsummary)
tbl_regression(conmod, exponentiate = F)
tbl_regression(orgmod, exponentiate = F)

#MULTIPLE RL
plot(avocado$AveragePrice~ avocado$Total.Volume ,col = "gray52")+
  abline(lm(avocado$Total.Volume ~ avocado$AveragePrice), col = "orange", lwd = 3)+
  lines(lowess(avocado$AveragePrice, avocado$Total.Volume), col = "blue", lwd = 3)+
  legend("topleft", legend = c("Theoretical", "Linear", "Smooth"),
         lwd = 3, lty = c(2, 1, 1), col = c("red", "orange", "blue"))

#FINDING THE CO-RELATION
cor(avocado$AveragePrice,avocado$Total.Volume)
cor(avocado$AveragePrice,avocado$X4046)
cor(avocado$AveragePrice, avocado$X4225)
cor(avocado$AveragePrice, avocado$X4770)
cor(avocado$AveragePrice, avocado$Total.Bags)
cor(avocado$AveragePrice, avocado$Small.Bags)
cor(avocado$AveragePrice, avocado$Large.Bags)
cor(avocado$AveragePrice, avocado$year)
cor(avocado$AveragePrice, organic)
cor(avocado$AveragePrice, conventional)

#HIGHLY C0-RELATED
cor(avocado$Total.Volume, avocado$AveragePrice)
cor(avocado$Total.Volume, avocado$X4046)
cor(avocado$Total.Volume, avocado$X4225)
cor(avocado$Total.Volume, avocado$X4770)
cor(avocado$Total.Volume, avocado$Total.Bags)
cor(avocado$Total.Volume, avocado$Small.Bags)
cor(avocado$Total.Volume, avocado$Large.Bags)
cor(avocado$Total.Volume, avocado$year)
cor(avocado$Total.Volume, organic)
cor(avocado$Total.Volume, conventional)
cor(avocado$Total.Volume, avocado$XLarge.Bags) #not highly c0-trlated

cor(avocado$year, avocado$AveragePrice)
cor(avocado$year, avocado$Total.Volume)
cor(avocado$year, avocado$X4046)
cor(avocado$year, avocado$X4225)
cor(avocado$year, avocado$X4770)
cor(avocado$year, avocado$Total.Bags)
cor(avocado$year, organic)
cor(avocado$year, conventional)

cor(avocado$X4046, avocado$AveragePrice) 
cor(organic , avocado$X4046)
cor(conventional, avocado$X4046)
cor(organic, avocado$X4225)
cor(conventional, avocado$X4225)
cor(organic, avocado$X4770)
cor(conventional, avocado$X4770)





MR <- lm(avocado$AveragePrice ~ avocado$X4046+ avocado$X4225+avocado$X4770, data = avocado)
summary(MR)
summary(MR)$coefficient


# Dummy variables for the region
Albany <- ifelse(avocado$region== "Albany", 1,0)
Atlanta <- ifelse(avocado$region=="Atlanta", 1,0)
BaltimoreWashington <- ifelse(avocado$region== "BaltimoreWashington", 1,2)
Boise <- ifelse(avocado$region== "Boise", 2,3)
Boston <- ifelse(avocado$region== "Boston", 3,4)
BuffaloRochester <- ifelse(avocado$region== "BuffaloRochester", 4,5)
California <- ifelse(avocado$region== "California", 5,6)
Charlotte <- ifelse(avocado$region== "Charlotte", 6,7)
Chicago <- ifelse(avocado$region== "Chicago", 7,8)
CincinnatiDayton <- ifelse(avocado$region== "CincinnatiDayton", 8,9)
Columbus <- ifelse(avocado$region== "Columbus", 9,10)
DallasFtWorth <- ifelse(avocado$region== "DallasFtWorth", 10,11)
Denver <- ifelse(avocado$region== "Denver", 11,12)
Detroit <- ifelse(avocado$region== "Detroit", 12,13)
GrandRapids <- ifelse(avocado$region== "GrandRapids", 13,14)
GreatLakes <- ifelse(avocado$region== "GreatLakes", 14,15)
HarrisburgScranton <- ifelse(avocado$region== "HarrisburgScranton", 15,16)
HartfordSpringfield <- ifelse(avocado$region== "HartfordSpringfield", 16,17)
Houston <- ifelse(avocado$region== "Houston", 17,18)
Indianapolis <- ifelse(avocado$region== "Indianapolis", 18,19)
Jacksonville <- ifelse(avocado$region== "Jacksonville", 19,20)
LasVegas <- ifelse(avocado$region== "LasVegas", 20,21)
LosAngeles <- ifelse(avocado$region== "LosAngeles", 21,22)
Louisville <- ifelse(avocado$region== "Louisville", 22,23)
MiamiFtLauderdale <- ifelse(avocado$region== "MiamiFtLauderdale", 23,24)
Orlando <- ifelse(avocado$region== "Orlando", 24,25)
Philadelphia <- ifelse(avocado$region== "Philadelphia", 25,26)
PhoenixTucson <- ifelse(avocado$region== "PhoenixTucson", 26,27)
Pittsburgh <- ifelse(avocado$region== "Pittsburgh", 27,28)
Plains <- ifelse(avocado$region== "Plains", 28,29)
Portland <- ifelse(avocado$region== "Portland", 29,30)
RaleighGreensboro <- ifelse(avocado$region== "RaleighGreensboro", 30,31)
RichmondNorfolk <- ifelse(avocado$region== "RichmondNorfolk", 31,32)
Roanoke <- ifelse(avocado$region== "Roanoke", 32,33)
Sacramento <- ifelse(avocado$region== "Sacramento", 33,34)
SanDiego <- ifelse(avocado$region== "SanDiego", 34,35)
SanFrancisco <- ifelse(avocado$region== "SanFrancisco", 35,36)
Seattle <- ifelse(avocado$region== "Seattle", 36,37)
SouthCarolina <- ifelse(avocado$region== "SouthCarolina", 37,38)
SouthCentral <- ifelse(avocado$region== "SouthCentral", 38,39)
Southeast <- ifelse(avocado$region== "Southeast", 39,40)
Spokane <- ifelse(avocado$region== "Spokane", 40,41)
StLouis <- ifelse(avocado$region== "StLouis", 41,42) 
Syracuse <- ifelse(avocado$region== "Syracuse", 42,43)
Tampa <- ifelse(avocado$region== "Tampa", 43,44)
TotalUS <- ifelse(avocado$region== "TotalUS", 44,45)
West <- ifelse(avocado$region== "West", 45,46)
WestTexNewMexico <- ifelse(avocado$region== "WestTexNewMexico", 46,47)
avocado_reg1 <- data.frame(Date = avocado$Date, X = avocado$X, AveragePrice = avocado$AveragePrice, Total.Volume= avocado$Total.Volume
                          , X4046= avocado$X4046, X4225 = avocado$X4225, X4770 = avocado$X4770, Total.Bags= avocado$Total.Bags,
                          Small.Bags= avocado$Small.Bags, XLarge.Bags = avocado$Large.Bags, Large.Bags= avocado$Large.Bags,
                          year= avocado$year, region=avocado$region, type= avocado$type, conventional= conventional, organic= organic,
                          Albany = Albany, Atlanta=Atlanta, BaltimoreWashington=BaltimoreWashington,Boise=Boise,Boston=Boston,
                          BuffaloRochester=BuffaloRochester,California=California,Charlotte=Charlotte,Chicago=Chicago,CincinnatiDayton=CincinnatiDayton, HartfordSpringfield=HartfordSpringfield,
                          Houston=Houston,Indianapolis=Indianapolis,Jacksonville=Jacksonville,LasVegas=LasVegas,LosAngeles=LosAngeles,
                          Columbus=Columbus,DallasFtWorth=DallasFtWorth,Denver=Denver,Detroit=Detroit, GrandRapids=GrandRapids,GreatLakes=GreatLakes,HarrisburgScranton=HarrisburgScranton,
                          Louisville=Louisville, MiamiFtLauderdale=MiamiFtLauderdale,Orlando=Orlando,Philadelphia=Philadelphia,PhoenixTucson=PhoenixTucson, Pittsburgh=Pittsburgh,
                          Plains=Plains, Portland=Portland,RaleighGreensboro=RaleighGreensboro,RichmondNorfolk=RichmondNorfolk,Roanoke=Roanoke,Sacramento=Sacramento,SanDiego=SanDiego, SanFrancisco=SanFrancisco,
                          Seattle=Seattle,SouthCarolina=SouthCarolina,SouthCentral=SouthCentral,Southeast=Southeast,Spokane=Spokane,
                          StLouis=StLouis,Syracuse=Syracuse, Tampa=Tampa,TotalUS=TotalUS,West=West,WestTexNewMexico=WestTexNewMexico)


install.packages('fastDummies')
library('fastDummies')
dataf <- dummy_cols(avocado, select_columns = 'region')

#create separate regression lines for each subset
#THIS IS ORGANIC VS ECAH INDEPENDENT VARIABLES
plot(organic ~ avocado$AveragePrice, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$AveragePrice), col = "Yellow", lwd = 3)

plot(organic ~ avocado$Total.Volume, pch = 19, col = "gray58") #ORGANIC VS Total volume
abline(lm(organic ~ avocado$Total.Volume), col = "Yellow", lwd = 3)

plot(organic ~ avocado$X4225, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$X4225), col = "Yellow", lwd = 3)

plot(organic ~ avocado$X4046, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$X4046), col = "Yellow", lwd = 3)

plot(organic ~ avocado$X4770, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$X4770), col = "Yellow", lwd = 3)

plot(organic ~ avocado$Total.Bags, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$Total.Bags), col = "Yellow", lwd = 3)

plot(organic ~ avocado$Small.Bags, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$Small.Bags), col = "Yellow", lwd = 3)

plot(organic ~ avocado$Large.Bags, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$Large.Bags), col = "Yellow", lwd = 3)

plot(organic ~ avocado$year, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(organic ~ avocado$year), col = "Yellow", lwd = 3)

#2.
plot(conventional ~ avocado$AveragePrice, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$AveragePrice), col = "Yellow", lwd = 3)

plot(conventional ~ avocado$Total.Volume, pch = 19, col = "gray58") #ORGANIC VS Total volume
abline(lm(conventional ~ avocado$Total.Volume), col = "Blue", lwd = 3)

plot(conventional ~ avocado$X4225, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$X4225), col = "Red", lwd = 3)

plot(conventional ~ avocado$X4046, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$X4046), col = "Green", lwd = 3)

plot(conventional ~ avocado$X4770, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$X4770), col = "Pink", lwd = 3)j

plot(conventional ~ avocado$Total.Bags, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$Total.Bags), col = "Yellow", lwd = 3)

plot(conventional ~ avocado$Small.Bags, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$Small.Bags), col = "Yellow", lwd = 3)

plot(conventional ~ avocado$Large.Bags, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$Large.Bags), col = "Yellow", lwd = 3)

plot(conventional ~ avocado$year, pch = 19, col = "gray58") #ORGANIC VS AVERAPGE PP
abline(lm(conventional ~ avocado$year), col = "Yellow", lwd = 3)



#subsetting data by regions
avocado_region <- avocado %>% filter(region %in% c("California", "GreatLakes", "Midsouth", "Northeast", "Plains", "SouthCentral", "Southeast",  "West"))

# subsetting data by cities
avocado_city <- avocado %>% filter(!(region %in% c("California", "GreatLakes", "Midsouth", "Northeast", "Plains", "SouthCentral", "Southeast",  "West", "TotalUS")))

# subsetting data by Total US
avocado11 <- avocado %>% filter(region == "Total")

#kable(table(avocado$year))

avocado$Date = as.Date(avocado$Date)
avocado$month  = factor(months(avocado$Date), levels = month.name)

options(repr.plot.width= 7, repr.plot.height=5)
ggplot(avocado, aes(avocado$type, avocado$AveragePrice))+
  geom_boxplot(aes(colour = avocado$year))+
  labs(colour = "Year", x = "Type", y ="Average Price", title = "Box Plot - Average price of avocado per year by avocado type")

grouped = avocado %>% 
  group_by(year, month, type) %>% 
  select(year, month, type,AveragePrice) %>%
  summarise(averagePrice = mean(AveragePrice))

options(repr.plot.width= 12, repr.plot.height=5)
ggplot(data=grouped,aes(x=month, y=averagePrice, colour=year,group = year)) +
  labs(colour = "Year", x = "Month", y ="Average Price", title = "Line Plot - Average monthly prices of avocado by avocado type for each year")+
  geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~grouped$type)

avocadoCodes = avocado %>%
  mutate(Others = Total.Volume-X4046-X4225-X4770) %>%
  gather("Avocado Type", "Volume", c(X4046:X4770,Others)) %>%
  select (-c(Total.Volume,Total.Bags,Small.Bags,Large.Bags,XLarge.Bags))

avocadoCodes$TotalPrice = avocadoCodes$AveragePrice*avocadoCodes$Volume

#Variation in total volume (total no. of avocado sold) and Net Price (Average Price x Volume) by Avocado Codes (PLUs) by Month
grouped_avocadoCode = avocadoCodes %>% 
  select(year, month, type, `Avocado Type`, TotalPrice, Volume) %>%
  group_by(year, month,type, `Avocado Type`) %>%
  summarise(TotalPrice = sum(TotalPrice)/1000000, Volume = sum(Volume)/1000000)


#lm(y ~ x1+x2+x3...,data)
lm(formula = Total.Volume ~ X4046 + X4225 + X4770, data = avocado)

#MULTIPLE REGRESSION LINES FOR BAGS
install.packages("predict3d")
require(predict3d)
require(rgl)

my.regression <- lm(Total.Volume ~ Small.Bags*Large.Bags*XLarge.Bags, data = avocado)
summary(my.regression)
ggPredict(my.regression,interactive = TRUE,show.point=FALSE,se=TRUE,xpos=0.5)

ggPredict(my.regression, facet.modx = TRUE,add.modx.values = FALSE,xpos=0.5, show.point = FALSE)

#MULTIPLE REGRESSION LINES AVOCADOES
my.regression1<- lm(Total.Volume ~ X4046*X4225*X4770, data = avocado)
summary(my.regression1)

ggPredict(my.regression1,interactive = TRUE,show.point=FALSE,se=TRUE,xpos=0.5)
ggPredict(my.regression1, facet.modx = TRUE,add.modx.values = FALSE,xpos=0.5, show.point = FALSE)


ggplot(avocado, aes(Total.Volume, AveragePrice, col=X4225, lty=X4770)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)

Y = 1.760+(1.45)*x1+(1.37)*x2+(2.57)*x3

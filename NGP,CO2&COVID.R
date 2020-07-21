###########ASSIGNMENT-4##################
#YOU CAN CHECK CODE BY LEVELS ADDED FOR QUESTION NUMBERS AND SEGMENTS IN QUESTIONS

#SET DIRECTORY
setwd("C:/Users/RV/Desktop/Stats Assignment RV")
wd <- "C:/Users/RV/Desktop/Stats Assignment RV"

# MAKE A DIRECTORY FOR THE SOLUTIONS
dir.create("CONFIRMED_MAPS")
tmpfolder1 <- 'CONFIRMED_MAPS'

dir.create("quest1")
tmpfolder2 <- 'quest1'

dir.create("quest2")
tmpfolder3 <- 'quest2'

dir.create("quest3")
tmpfolder4 <- 'quest3'


##########DOWNLOAD CONFIRMED CASES########
library(readr)
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv"
CONFIRM <- read_csv(url(urlfile))
urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Deaths_archived_0325.csv"
DEATH <- read_csv(url(urlfile))

COVID1 <- CONFIRM[CONFIRM$`Country/Region`=='US',]
COVID2 <- DEATH[DEATH$`Country/Region`=='US',]
CV1 <- COVID1[5:ncol(COVID1)]
CV2 <- COVID2[5:ncol(COVID2)]
CV1<-replace(CV1,CV1==0,1)
COVID3 <- (CV2/CV1)*100
COVID3 <- cbind(COVID1[,c(1:2)],COVID3[,c(1:ncol(COVID3))])



# manipulate data to find new cases per day
mydata1 <- COVID1
mydatadifference11 <- COVID1[5:(ncol(mydata1)-1)]
mydatadifference12 <- COVID1[6:ncol(mydata1)]
difference1 <- mydatadifference12-mydatadifference11
#need to add back state info
difference1 <- cbind(COVID1[,c(1:2)],difference1[,c(1:ncol(difference1))])

# change new cases into percentage change
percentdiff11 <- difference1[3:(ncol(difference1)-1)]
percentdiff12 <- difference1[4:ncol(difference1)]

X<-percentdiff11
X<-replace(X,X==0,1)

percentdif1 <- ((percentdiff12 - percentdiff11)/X)*100
percentdif1 <- cbind(COVID1[,c(1:2)],percentdif1[,c(1:ncol(percentdif1))])

# manipulate data to find new cases per day
mydata2 <- COVID2
mydatadifference21 <- COVID2[5:(ncol(mydata2)-1)]
mydatadifference22 <- COVID2[6:ncol(mydata2)]
difference2 <- mydatadifference22-mydatadifference21
#need to add back state info
difference2 <- cbind(COVID2[,c(1:2)],difference2[,c(1:ncol(difference2))])

# change new cases into percentage change
percentdiff21 <- difference2[3:(ncol(difference2)-1)]
percentdiff22 <- difference2[4:ncol(difference2)]

Y<-percentdiff21
Y<-replace(Y,Y==0,1)

percentdif2 <- ((percentdiff22 - percentdiff21)/Y)*100
percentdif2 <- cbind(COVID2[,c(1:2)],percentdif2[,c(1:ncol(percentdif2))])

#########DATASET ON NATURAL GAS PRICE########
#install.packages("jsonlite", repos="https://cran.rstudio.com/")
library("jsonlite")

json_file <- 'https://datahub.io/core/natural-gas/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv' & json_data$resources$datahub$derivedFrom[i]=="daily"){
    path_to_file = json_data$resources$path[i]
    NGP <- read.csv(url(path_to_file))
    print(NGP)
  }
}

library(lubridate)
NGP$Y=year(NGP$Date)
NGP$D=day(NGP$Date)
NGP$M=month(NGP$Date)
NGP = NGP[NGP$Y==2020,]


NGP$D=ifelse(NGP$M==2,NGP$D+31,
             ifelse(NGP$M==3,NGP$D+60,
                    ifelse(NGP$M==4,NGP$D+91,NGP$D)))
P=22:(ncol(COVID1)+18)
P=as.data.frame(P)
price=merge(P,NGP,by.x='P',by.y='D',all.x=T)
price<-price[-c(2,4,5)]
price=as.data.frame(price)

for(i in 1:nrow(price))
{ 
  if(is.na(price$Price[i])==T){price$Price[i]=price$Price[i-1]}
  print(is.na(price$Price[i]))
} 
price<-t(price)
price=as.data.frame(price)

# manipulate data to find new cases per day
ngdata <- price
ngdatadifference1 <- price[1:(ncol(ngdata)-1)]
ngdatadifference2 <- price[2:ncol(ngdata)]
ngdifference <- ngdatadifference2-ngdatadifference1
ngdifference<-replace(ngdifference,is.na(ngdifference),0)

A<-ngdatadifference1[-c(1)]
A<-replace(A,A==0,1)
A=as.data.frame(A)

# change new cases into percentage change
ngpercentdiff1 <- ngdifference[1:(ncol(ngdifference)-1)]
ngpercentdiff2 <- ngdifference[2:ncol(ngdifference)]

ngpercentdif <- ((ngpercentdiff2 - ngpercentdiff1)/A)*100
ngpercentdif<-replace(ngpercentdif,is.na(ngpercentdif),0)


########DATASET ON GLOBAL CO2 PPM DAILY########
json_file <- 'https://datahub.io/core/co2-ppm-daily/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    CO2 <- read.csv(url(path_to_file))
    print(CO2)
  }
}

CO2$Y=year(CO2$date)
CO2$M=month(CO2$date)
CO2$D=day(CO2$date)
CO2 = CO2[CO2$Y==2020,]

CO2$D=ifelse(CO2$M==2,CO2$D+31,
             ifelse(CO2$M==3,CO2$D+60,
                    ifelse(CO2$M==4,CO2$D+91,CO2$D)))
CO21=22:(ncol(COVID1)+18)
CO21=as.data.frame(CO21)
PPM=merge(CO21,CO2,by.x='CO21',by.y='D',all.x=T)

PPM<-PPM[-c(2,4,5)]
PPM=as.data.frame(PPM)
PPM$value[40]
for(i in 22:nrow(PPM)+22)
{ 
if(is.na(PPM$value[i])==T){PPM$value[i]=PPM$value[i-1]}
print(is.na(PPM$value[i]))
} 
PPM<-t(PPM)
PPM=as.data.frame(PPM)

# manipulate data to find new cases per day
COdata <- PPM
COdatadifference1 <- PPM[1:(ncol(COdata)-1)]
COdatadifference2 <- PPM[2:ncol(COdata)]
COdifference <- COdatadifference2-COdatadifference1
COdifference<-replace(COdifference,is.na(COdifference),0)

B<-COdatadifference1[-c(1)]
B<-replace(B,B==0,1)
B=as.data.frame(B)

# change new cases into percentage change
COpercentdiff1 <- COdifference[1:(ncol(COdifference)-1)]
COpercentdiff2 <- COdifference[2:ncol(COdifference)]

COpercentdif <- ((COpercentdiff2 - COpercentdiff1)/B)*100
COpercentdif<-replace(COpercentdif,is.na(COpercentdif),0)


######SUMMARY ON IMPORTANT DATA#####

#GROWTH RATE OF CONFIRMED CASES
summary(difference1)
summary(percentdif1)
#MORTALITY RATE OF DEATH CASES
summary(difference2)
summary(percentdif2)
#CO2 PPM LEVEL RATE
summary(PPM)
summary(COdifference)
summary(COpercentdif)
#NATURAL GAS PRICE DECREASE RATE
summary(price)
summary(ngdifference)
summary(ngpercentdif)
#MORTALITY RATE
summary(COVID3)


######READ USA MAPDATA#####
library(rgdal)
USA <- readOGR(paste0(wd,"/tl_2019_us_state"),
               "tl_2019_us_state")

plot(USA, col='skyblue')

######DOWNLOAD AND SUBSET STATES OF INTERESTS
lower48 <- "https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv"
lower48 <- read_csv(url(lower48))

#DROPPING AK, HI, DC
lower48 <- lower48[lower48$Abbreviation!="HI",]
lower48 <- lower48[lower48$Abbreviation!="AK",]
lower48 <- lower48[lower48$Abbreviation!="DC",]
USA@data$NAME
#MERGE .CSV TO SHAPE FILE
USA <- merge(x=USA,
             y=lower48,
             by.x="NAME",
             by.y="State")

#DROP MISSING DATA
USA <- USA[!is.na(USA@data$Abbreviation),]

#PLOTTING USA MAP
plot(USA, col='chocolate',
     main="My First Map")

#MAP WITH REGION COLORS
plot(USA, col=USA@data$REGION)
#MAP WITH DIVISION COLORS
plot(USA, col=USA@data$DIVISION)




#####MAIN QUESTION IN ASSIGNMENT 4######
library(ggplot2)
library(tmap)
library(RColorBrewer)

#MERGE WITH COVID DATA
Corona1<- merge(x=lower48,
                y=difference1,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA1 <- merge(USA,
              Corona1,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona2<- merge(x=lower48,
                y=percentdif1,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# MERGE INTO SHAPE FILE
USA2 <- merge(USA,
                 Corona2,
                 by.x="NAME",
                 by.y='State',
                 all.x=T)

map1 <- tm_shape(USA1)+
  tm_fill("3/9/20", #DATE
          breaks = c(-Inf,-50,-25,-10,-5,0,5,10,25,50,Inf),
          style="fixed",
          colorNA="white")+
  tm_borders()
map1



map2 <- tm_shape(USA2)+
        tm_fill("3/17/20",     #DATE
         breaks = c(-Inf,-8000,-1000,-500,-100,-50,-25,-10,-5,0,5,10,25,50,100,500,1000,8000,Inf),
        style="fixed",
        colorNA="white")+
       tm_borders()
map2

#####MAPS ON NEW REPORTED CONFIRMED CASES PER DAY#####
x <- USA1@data

a <- 75
for(a in 18:ncol(USA1)){
     map <-   tm_shape(USA1)+
    tm_fill(names(USA1@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-10,-5,0,5,10,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("NEW REPORTED CONFIRMED CASES PER DAY")
  
  tmpName <- gsub("/", "-", names(USA1)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder1,"/",
                   tmpName,".png"))
  
}
warnings()

#####MAPS ON PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY#####
y <- USA2@data

a <- 75
for(a in 18:ncol(USA2)){
  map <-   tm_shape(USA2)+
    tm_fill(names(USA2@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-500,-100,-50,-25,-10,-5,0,5,10,25,50,100,500,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY")
  
  tmpName <- gsub("/", "-", names(USA2)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder1,"/"," CUMATIVE Percent",
                   tmpName,".png"))
  
}

#####QUESTION 1: WHAT IS MORTALITY RATE IN LOWER 48 USA STATES?######

#MERGE WITH COVID DATA
Corona3<- merge(x=lower48,
                y=difference2,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA3 <- merge(USA,
              Corona3,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona4<- merge(x=lower48,
                y=percentdif2,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# MERGE INTO SHAPE FILE
USA4 <- merge(USA,
              Corona4,
              by.x="NAME",
              by.y='State',
              all.x=T)

#MERGE WITH COVID DATA
Corona5<- merge(x=lower48,
                y=COVID3,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# MERGE INTO SHAPE FILE
USA5 <- merge(USA,
              Corona5,
              by.x="NAME",
              by.y='State',
              all.x=T)

map3 <- tm_shape(USA3)+
  tm_fill("3/18/20", #DATE
          breaks = c(-Inf,-8000,-1000,-500,-100,-50,-25,-10,-5,0,5,10,25,50,100,500,1000,8000,Inf),
          style="fixed",
          colorNA="white")+
  tm_borders()
map3



map4 <- tm_shape(USA4)+
  tm_fill("3/7/20", #DATE
          breaks = c(-Inf,-8000,-1000,-500,-100,-50,-25,-10,-5,0,5,10,25,50,100,500,1000,8000,Inf),
          style="fixed",
          colorNA="white")+
  tm_borders()
map4

map5 <- tm_shape(USA5)+
  tm_fill("3/19/20",
          style="fixed",
          breaks=c(-Inf,-1.5 ,-0.15, -0.00025, 0.00025, 0.15,1.5, Inf),
          colorNA="white")+
  tm_borders()
map5


#####MAPS ON NEW REPORTED DEATHS PER DAY#####
x <- USA3@data

a <- 75
for(a in 18:ncol(USA3)){
  map <-   tm_shape(USA3)+
    tm_fill(names(USA3@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-500,-100,-50,-25,-10,-5,0,5,10,25,50,100,500,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("NEW REPORTED CONFIRMED CASES PER DAY")
  
  tmpName <- gsub("/", "-", names(USA3)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder2,"/","NEW REPORTED DEATHS ",
                   tmpName,".png"))
  
}

#####MAPS ON PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY#####
y <- USA4@data

a <- 75
for(a in 18:ncol(USA4)){
  map <-   tm_shape(USA4)+
    tm_fill(names(USA4@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-500,-100,-50,-25,-10,-5,0,5,10,25,50,100,500,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("PERCENTAGE CHANGE PER DAY FROM PREVIOUS DAY")
  
  tmpName <- gsub("/", "-", names(USA4)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder2,"/","DEATH RATE ",
                   tmpName,".png"))
  
}
#####MAPS ON MORTALITY RATE ON DAILY BASIS#####
y <- USA5@data

a <- 75
for(a in 18:ncol(USA5)){
  map <-   tm_shape(USA5)+
    tm_fill(names(USA5@data)[a], #DATE
            breaks = c(-Inf,-8000,-1000,-500,-100,-50,-25,-10,-5,0,5,10,25,50,100,500,1000,8000,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("MORTALITY RATE ON DAILY BASIS")
  
  tmpName <- gsub("/", "-", names(USA4)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder2,"/","MORTALITY RATE ",
                   tmpName,".png"))
  
}

#####QUESTION 2: WHAT IS THE EFFECT OF GLOBAL CO2 PPM LEVEL WITH COVID-19?######


a <- 1
for(a in 1:nrow(percentdif1)){
  #NAMES FOR PLOTS
  tmpdata.State <- percentdif1[a,1]
  tmpdata.Country <- percentdif1[a,2]
  # MISSING VALUES
  if(is.na(tmpdata.State)){
    #NAMING WITH COUNTRY
    tmpdata.Loc <- tmpdata.Country
  } else{ #PASTING STATE WITH COUNTRY
    tmpdata.Loc <- paste0(tmpdata.State,
                          "_",
                          tmpdata.Country)
  }
  # ROW DATA
  tmpdata <-  percentdif1[a, 3:(ncol(percentdif1)-1)]
  tmpdata1 <- difference1[a, 3:(ncol(difference1)-1)]
  tmpdata2 <- COpercentdif*100
  tmpdata2[1,]=c(1:ncol(COpercentdif))

  
  # take the transpose
  tmpdata <- t(tmpdata)
  tmpdata1 <-t(tmpdata1)
  tmpdata2 <- t(tmpdata2)
  
  
  #SPECIAL CHARACTER
  tmpdata.Loc.clean <- gsub("[[:punct:]]", # PUNCTUATION REMOVAL
                            "", 
                            tmpdata.Loc) 
  
  # NAMING THE PLOT
  pltName <- paste0(wd,"/",tmpfolder3,"/",tmpdata.Loc.clean, ".png")
  
  #SAVING FILE NAME
  png(pltName) 
  
  #PLOT
  par(mfrow=c(1,2))
  {plot(tmpdata,
       main=tmpdata.Loc,
       pch=19,
       col='red',
       xlab="Days Since Start",
       ylab="COVID-19 Growth rate")
   par(new=T)
  plot(tmpdata2,
       pch=19,
       col='darkgreen',axes=F,
       xlab="", ylab="")
      mtext("PPM LEVELS",side=4,line=1)
  axis(4)
  legend('topright',
         pch=15,
         col=c("green",'red'),
         c("CO2","COVID-19"),
         bty='n') 
    lines(tmpdata2,
        col='green')
    lines(tmpdata,
          col='darkred')}
 #density plot of COnfirmed cases
    hist(tmpdata1,
         main=tmpdata.Loc,
         xlab="COVID CONFIRMED CASES GROWTH  ON A DAY",
         ylab="COUNTS/FREQUENCY",
         col='pink')
         lines(density(tmpdata1),
          col="violet")
         
    
  dev.off() #TURN OFF THE PLOT AND SAVE IT 
}

#####QUESTION 3: WHAT IS THE EFFECT OF NATURAL GAS PRICE WITH COVID-19 AND CO2 PPM LEVELS?######

#LET'S PREPARE SOME DATA TO PERFORM OUR MARKET BASKET ANALYSIS
#positive confirmed cases growth rate
#positive CO2 growth and decrease of price rates is all we care
#let's just take only rates of the data we have

n=ncol(percentdif1)
probconf<-percentdif1[,3:ncol(percentdif1)]
probconf<-ifelse(probconf>0,1,0)
M<-as.data.frame(table(probconf))
probconf<-as.data.frame(probconf)

COpercentdif<-as.data.frame(COpercentdif)
COprob<-COpercentdif[2,1:(n-2)]
COprob<-ifelse(COprob>0,1,0)
N<-as.data.frame(table(COprob))
COprob<-as.data.frame(COprob)

NGprob<-ngpercentdif[2,1:(n-2)]
NGprob<-ifelse(NGprob<0,1,0) #I changed 1 and 0 to my advantage.
O<-as.data.frame(table(NGprob))
NGprob<-as.data.frame(NGprob)

i=as.numeric(ncol(probconf))
j=as.numeric(nrow(probconf))
ncol(COprob)
ncol(NGprob)#assured same columns

#RESPONSE=DECREASE OF RATES IN NATURAL GAS PRICE
####FOR TOTAL LOWER 48 STATES USA#####
#Expected confidence=NGPrice low observations/total observations
#Support=Union of low NGPrice,high CO2 level and high Confirmed growth rate/total observations
#Confidence= Union of low NGPrice,high CO2 level and high Confirmed growth rate/NGPrice low observations
#lift=Confidence/Expected Confidence

######EXPECTED CONFIDENCE#######
r=0
for(m in 2:(nrow(probconf)))
{
  for(n in 1:(ncol(probconf)))
  {
    print(m)
    print(n)
    print(NGprob[1,n])
    print(probconf[m,n])
    r=if(probconf[m,n]==1 && NGprob[1,n]==1){r+1} else{r}
    print(r)
  }
}
total=i*j #totalobservations
r
total
Expected=r/total
Expected=Expected*100
Expected
#THIS 'Expected' IS THE PERCENTAGE OF MY EXPECTED CONFIDENCE FOR LOWER USA

#######CONFIDENCE######
s=0
for(m in 2:(nrow(probconf)))
{
for(n in 1:(ncol(probconf)))
  {
  print(probconf[m,n])
  print(COprob[1,n])
  s=if(probconf[m,n]==1 && COprob[1,n]==1){s+1} 
     else{s}
  print(s)
  
  }
}

s

U<-ifelse(COprob==1 & NGprob==1,1,0) #made a union here with CO2 and NGdata easy right
c=0
for(m in 2:(nrow(probconf)))
{
  for(n in 1:(ncol(probconf)))
  {
    print(probconf[m,n])
    print(U[1,n])
    c=if(probconf[m,n]==1 && U[1,n]==1){c+1}
    else{c}
    print(c)
    
  }
}
c

Confidence=c/s
Confidence=Confidence*100
Confidence

#THIS 'Confidence' IS THE PERCENTAGE OF MY CONFIDENCE FOR LOWER USA

########SUPPORT########

Support=c/total
Support=Support*100
Support
#THIS 'Support' IS THE PERCENTAGE OF MY SUPPORT FOR LOWER USA


#######OVERALL USA LIFT######
TOTALLIFT=Confidence/Expected
TOTALLIFT
Expected
Confidence
Support

#########ROW WISE LIFT########
#######EXPECTED CONFIDENCE########


lifttable<-COVID1[,c(1:2)]

total=ncol(probconf)
#Intersection of NG price and CO2 PPM level
nc=0
NC<-(1:nrow(probconf))
for(m in 2:(nrow(probconf)))
{
  for(n in 1:(ncol(probconf)))
  {
    a=probconf[m,n]==1 && NGprob[1,n]==1
    a<-replace(a,is.na(a),F)
    nc=if(a){nc+1}
    else{nc}
  }
  NC[m]<-nc
  nc=0 
}


lifttable<-cbind(lifttable,NC)
t=(ncol(probconf)/100)
ExpectedConfidence<-NC/t
lifttable<-cbind(lifttable,ExpectedConfidence)

#########CONFIDENCE(CONDITIONAL PROBABILITY)###########

cc=0
CC<-(1:nrow(probconf))
for(m in 2:(nrow(probconf)))
{
  for(n in 1:(ncol(probconf)))
  {
    cc=if(probconf[m,n]==1 && COprob[1,n]==1){cc+1}
    else{cc}
  }
  CC[m]<-cc
  cc=0 
}

lifttable<-cbind(lifttable,CC)

uc=0
UC<-(1:nrow(probconf))
for(m in 2:(nrow(probconf)))
{
  for(n in 1:(ncol(probconf)))
  {
    uc=if(probconf[m,n]==1 && U[1,n]==1){uc+1}
    else{uc}
  }
  UC[m]<-uc
  uc=0 
}

lifttable<-cbind(lifttable,UC)
CONFIDENCE<-ifelse(CC==0,0,UC/CC)
lifttable<-cbind(lifttable,CONFIDENCE)

########SUPPORT(JOINT PROBABILITY)##########

SUPPORT<-ifelse(UC==0,0,UC/t)
lifttable<-cbind(lifttable,SUPPORT)


##########LIFT###########
LIFT<-ifelse(ExpectedConfidence==0,0,CONFIDENCE/ExpectedConfidence)
lifttable<-cbind(lifttable,LIFT)


png(filename= paste0(wd,"/",tmpfolder4,"/","LIFT CURVE", ".png"))
plot(lifttable$LIFT,
     pch=19,
     main="LIFT CURVE by STATE",
     xlab="STATE",
     ylab="LIFT")
lines(lifttable$LIFT,col="red")
#we can see that the lift is more in 1-50 ROWS of USA and 225-250 ROWS of USA

dev.off() 
lifttable[nrow(lifttable)+1,]<-c("TOTAL","TOTAL",r,Expected,s,Support,c,Confidence,TOTALLIFT)

png(filename= paste0(wd,"/",tmpfolder4,"/","NATURAL GAS PRICE DAILY",".png"))
plot(x=NGP$Price,y=NGP$D,
     col=NGP$M,
     pch=19,
     xlab="NATURAL GAS PRICE",
     ylab="DAY")
legend('topright',
       pch=15,
       col=c("green",'red',"black"),
       c("JAN", "FEB","MARCH"),
       bty='n') 
dev.off() 

png(filename= paste0(wd,"/",tmpfolder4,"/","CO2 PPM LEVEL DAILY",".png"))
plot(x=CO2$value,y=CO2$D,
     col=CO2$M,
     pch=19,
     xlab="CO2 PPM VALUE",
     ylab="DAY")
legend('bottomleft',
       pch=15,
       col=c("green",'red',"black","blue"),
       c("JAN", "FEB","MARCH","APRIL"),
       bty='n') 
dev.off() 

lifttable$NC=as.numeric(lifttable$NC)
lifttable$CC=as.numeric(lifttable$CC)
lifttable$UC=as.numeric(lifttable$UC)
lifttable$ExpectedConfidence=as.numeric(lifttable$ExpectedConfidence)
lifttable$SUPPORT=as.numeric(lifttable$SUPPORT)
lifttable$CONFIDENCE=as.numeric(lifttable$CONFIDENCE)
lifttable$LIFT=as.numeric(lifttable$LIFT)

lift1<-lifttable[-c(250),]
#MERGE WITH COVID DATA
Corona6<- merge(x=lower48,
                y=lift1,
                by.x="State",
                by.y="Province/State",
                all.x=T)
# now let's merge COVID onto the 'USA' shapefile
USA6 <- merge(USA,
              Corona6,
              by.x="NAME",
              by.y='State',
              all.x=T)

y <- USA6@data

a <- 75
for(a in 18:ncol(USA6)){
  map <-   tm_shape(USA6)+
    tm_fill(names(USA6@data)[a], #DATE
            breaks = c(-Inf,-100,-1,-0.6,-0.2,0,0.2,0.6,1,100,Inf),
            style="fixed",
            colorNA="white")+
    tm_borders()+
    tm_layout("LIFT STATS")
  
  tmpName <- gsub("/", "-", names(USA6)[a])
  print(map)
  tmap_save(map,
            paste0(wd,"/",tmpfolder4,"/",
                   tmpName,".png"))
  
}


############ANSWERS TO QUESTIONS/EXPLANATION#######################
########MAIN QUESTON#######
#SOLUTION:
#We can see the graphs are drawn for confirmed cases growth per day and cumulative percentage
#and cumulative are set in different folder in Confirmed Maps
#Changes are made in the Breaks and the maps are showing variance on cases daily

#####QUESTION 1: WHAT IS MORTALITY RATE IN LOWER 48 USA STATES?######
#SOLUTION:
#We can see that the death cases growth per day and the cumulative growth of deaths are drawn
#We didn't notice any major death in 1000s
#Changes are made in the Breaks and the maps are showing variance on cases daily
#We know that Confirm Cases/Death cases% =Mortality Rate
# maps are drawn for mortality rate also
#Changes are senn on daily basis

#####QUESTION 2: WHAT IS THE EFFECT OF GLOBAL CO2 PPM LEVEL WITH COVID-19?######
#SOLUTION:
#We can see that graphs are drwan for the COVID CUMULATIVE PERCENTAGE and CO2 PPM LEVEL
#We noticed the COVID-19 CASES CUMulative growth percent has increased 
#when there is a growth in ppm level sometimes as in Honolulu  and Hawai as the CO2 level was more the COVID-19 cases increased
#The growth was just before or after the peak of cases as we notice
# Sometimes we also notice the cases after the highest peak of PPM level growth 
#only few CA has the growth before the PPM level growth indicating that the cases started there
#or there was an external factor effecting
#This prediction might be more realistic if there was state specific data 
#The histogram and density plot is added for the total confirmed cases
#You can see that increase of frequency of how many cases are admitted in a day 
# frequency of 1 cases or 2 cases confirmed in COVID-19 in State wise growth like that
#the density line doesn't show up but there is a violet density line with the histogram


#####QUESTION 3: WHAT IS THE EFFECT OF NATURAL GAS PRICE WITH COVID-19 AND CO2 PPM LEVELS?######
#SOLUTION:
# chose the Outcome as Price of Natural Gas Price growth rate
#preprocessed data and imputed 0s in growth differences and cumualtive growth rate 
# binned the data to 1s and 0s 
# took nested 'for' loops and if else conditions to get the probability
# made a table for lift on different state 
#Found union of NGPrice and CO2level with ifelse statement
#found expected confidence, support,confidence,lift 
#Expected confidence=NGPrice low observations/total observations
#Support=Union of low NGPrice,high CO2 level and high Confirmed growth rate/total observations
#Confidence= Union of low NGPrice,high CO2 level and high Confirmed growth rate/NGPrice low observations
#lift=Confidence/Expected Confidence
#Generated a graph showing individual lifts for state
#This way we can see which state has more lift through number 
#and say the prices can be set higher here as the cases are increasing
#Like that we can let people can relative cost ro risk and stay at home 
#the CO2 level is also taken into account let us know that more vehicle use
# This can be effective with realtime data of CO2 and Petrol or diesel price
#Summary of lift table:
summary(lifttable)
#The maxstats are for total lower USA data if we see
#but you can find in graph for more lift data as total was added afterwards in graph
#SEEING GRAPH WE CAN SEE GREATER LIFT IN 100 to 200 states  we can see the details in lift table 
# ADDED MAPS FOR ALL THE LIFT STATS LIKE YOU CAN SEE NOW WHICH STATE HAS MORE LIFT ON MAP! 
#AWESOME 

##########END OF CODE############


library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(mongolite)
library(reshape)
library(ggplot2)
library(chron)
library(Rserve)
library(Boruta)
library(Amelia)
library(randomForest)
library(Metrics)
library(ggthemes)
#READ DAT FROM FILE
gold<-read.csv('goldValues.csv')
death<-read.csv('deathValues.csv')
objects<-read.csv('objValues.csv')
bans<-read.csv('banValues.csv')
main <- read.csv('_LeagueofLegends.csv')
#DATA checking
glimpse(gold)
glimpse(death)
glimpse(objects)
glimpse(bans)
glimpse(main)
#Data NA Value checking

(any(is.na(gold)))
(any(is.na(death)))
(any(is.na(objects)))
(any(is.na(bans)))
(any(is.na(main)))
#handle NA Value
# check and handle gold NA Value 
sum(is.na(gold))
colSums(is.na(gold))
is.na(gold)

# check and handle death NA Value 
sum(is.na(death))
colSums(is.na(death))
is.na(death)

# check and handle objects NA Value 
sum(is.na(objects))
colSums(is.na(objects))
is.na(objects)


# should not deltete na datacolSums(is.na(death))
# newDeathData <- na.omit(death)
#check modifty value

glimpse(main)
names(main)[names(main) == "MatchHistory"] <- "MatchId"
uniqueMatch=unique(main$MatchId)
View(uniqueMatch)
nrow(main)

#data wranglering

red <- vector("list",3760)
blue<- vector("list",3760)

for(i in 1:length(main$goldredTop)){
  y <- gsub("\\[|\\]", "", main$goldredTop[i])
  y <- gsub(" ","",y)
  numbers <- strsplit(y,',')
  numbers <- unlist(numbers)
  red[[i]] <- as.numeric(numbers)
}
#for blue
for(i in 1:length(main$goldblueTop)){
  y <- gsub("\\[|\\]", "", main$goldblueTop[i])
  y <- gsub(" ","",y)
  numbers <- strsplit(y,',')
  numbers <- unlist(numbers)
  blue[[i]] <- as.numeric(numbers)
}

mean(red[[2]] - blue[[2]])

#Create functions to calculate the average gold difference, given a role. Size is hard-coded for now.
RoleGoldDiff <- function(role){
  result<-numeric(length = 3760)
  red <- vector("list",3760)
  blue<- vector("list",3760)
  if(role == "top"){
    
    
    for(i in 1:length(main$goldredTop)){
      y <- gsub("\\[|\\]", "", main$goldredTop[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    #for blue
    for(i in 1:length(main$goldblueTop)){
      y <- gsub("\\[|\\]", "", main$goldblueTop[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
    
  }
  else if(role == "jun"){
    
    #for red, function can be better abstracted but I don't have time.
    for(i in 1:length(main$goldredJungle)){
      y <- gsub("\\[|\\]", "", main$goldredJungle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    #for blue
    for(i in 1:length(main$goldblueJungle)){
      y <- gsub("\\[|\\]", "", main$goldblueJungle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
    
    
  }
  else if(role == "mid"){
    
    #for red, function can be better abstracted but I don't have time.
    for(i in 1:length(main$goldredMiddle)){
      y <- gsub("\\[|\\]", "", main$goldredMiddle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    #for blue
    for(i in 1:length(main$goldblueMiddle)){
      y <- gsub("\\[|\\]", "", main$goldblueMiddle[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
    
    
  }
  else if(role == "adc"){
    
    #for red, function can be better abstracted but I don't have time.
    for(i in 1:length(main$goldredADC)){
      y <- gsub("\\[|\\]", "", main$goldredADC[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    #for blue
    for(i in 1:length(main$goldblueADC)){
      y <- gsub("\\[|\\]", "", main$goldblueADC[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
    
    
  }
  else{
    
 
    for(i in 1:length(main$goldredSupport)){
      y <- gsub("\\[|\\]", "", main$goldredSupport[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      red[[i]] <- as.numeric(numbers)
    }
    #for blue
    for(i in 1:length(main$goldblueSupport)){
      y <- gsub("\\[|\\]", "", main$goldblueSupport[i])
      y <- gsub(" ","",y)
      numbers <- strsplit(y,',')
      numbers <- unlist(numbers)
      blue[[i]] <- as.numeric(numbers)
    }
    
    
  }
  for(i in 1:length(result)){
    result[i] <- mean(red[[i]] - blue[[i]])
  }    
  result
}


main$TopAvg <- RoleGoldDiff(role = "top")
main$JungleAvg <- RoleGoldDiff(role = "jun")
main$MidAvg <- RoleGoldDiff(role = "mid")
main$ADCAvg <- RoleGoldDiff(role = "adc")
main$SupportAvg <- RoleGoldDiff(role= "support")
main[1,]
View(main)
main[3760,]


games <- data.frame(winner=ifelse(main$bResult==1,"Blue","Red"),
                    TopAvg = main$TopAvg,
                    JungleAvg= main$JungleAvg,
                    MidAvg = main$MidAvg,
                    ADCAvg = main$ADCAvg,
                    SupportAvg= main$SupportAvg)

head(games)
View(games)

#add a column for each role, which will have a value for the team winner for each role
games$TopWinningTeam <- ifelse(games$TopAvg > 0,"Red","Blue")
games$JungWinningTeam <- ifelse(games$JungleAvg > 0,"Red","Blue")
games$MidWinningTeam <- ifelse(games$MidAvg > 0,"Red","Blue")
games$ADCWinningTeam <- ifelse(games$ADCAvg > 0,"Red","Blue")
games$SupWinningTeam <- ifelse(games$SupportAvg > 0,"Red","Blue")

games$TopAvg <- abs(games$TopAvg)
games$JungleAvg <- abs(games$JungleAvg)
games$MidAvg <- abs(games$MidAvg)
games$ADCAvg <- abs(games$ADCAvg)
games$SupportAvg <- abs(games$SupportAvg)

Carries <- vector("character",length = nrow(games))
CarryTeams <- vector("character",length = nrow(games))

for(i in 1:nrow(games))
{
  TopValue <- games$TopAvg[i]
  JungValue <- games$JungleAvg[i]
  MidValue <- games$MidAvg[i]
  ADCValue <- games$ADCAvg[i]
  SupValue <- games$SupportAvg[i]
  
  RoleValues <- c(TopValue,JungValue,MidValue,ADCValue,SupValue)
  
  CarryGold <- max(RoleValues)
  
  if(CarryGold == TopValue )
  {
    Carries[i] <- "Top"
    CarryTeams[i] <- games$TopWinningTeam[i]
  }
  else if(CarryGold == JungValue){
    Carries[i] <- "Jungle"
    CarryTeams[i] <- games$JungWinningTeam[i]
  }
  
  else if(CarryGold == MidValue){
    Carries[i] <- "Mid"
    CarryTeams[i] <- games$MidWinningTeam[i]
  }
  
  else if(CarryGold == ADCValue){
    Carries[i] <- "ADC"
    CarryTeams[i] <- games$ADCWinningTeam[i]
  }
  else{
    #Case for support (yeah, like that's gonna happen :^) )
    Carries[i] <- "Support"
    CarryTeams[i] <- games$SupWinningTeam[i]
  }
}

games$Carry <- Carries
games$TeamOfCarry <- CarryTeams
head(games)
View(games)
#data visualizations
ggplot(data = games, aes(x = Carry, fill = Carry)) + geom_bar(stat="count") + 
  ggtitle("Count of carrying per role")

WinningDF <- data.frame(Winner=games$winner,CarryTeam=games$TeamOfCarry,Carry=games$Carry)


WinningDF$CarryWin <- ifelse(WinningDF$Winner == WinningDF$CarryTeam,1,0)

TopCarryWins <- nrow(WinningDF[WinningDF$Carry=="Top" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Top",])
JunCarryWins <- nrow(WinningDF[WinningDF$Carry=="Jungle" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Jungle",])
MidCarryWins <- nrow(WinningDF[WinningDF$Carry=="Mid" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Mid",])
ADCCarryWins <- nrow(WinningDF[WinningDF$Carry=="ADC" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="ADC",])
SupCarryWins <- nrow(WinningDF[WinningDF$Carry=="Support" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Support",])

#Create a data frame with the winrate of each role, when that specific role is the carry of the game.
CarryRoleWins <- data.frame(Role = c("Top","Jungle","Mid","ADC","Support"),
                            Winrate= c(TopCarryWins,JunCarryWins,MidCarryWins,ADCCarryWins,SupCarryWins))

ggplot(data=CarryRoleWins, aes(x=Role,y=Winrate,fill=Role)) + geom_bar(stat="identity")
CarryRoleWins
#deeper analysis
WinningDF <- data.frame(Winner=games$winner,CarryTeam=games$TeamOfCarry,Carry=games$Carry)


WinningDF$CarryWin <- ifelse(WinningDF$Winner == WinningDF$CarryTeam,1,0)

TopCarryWins <- nrow(WinningDF[WinningDF$Carry=="Top" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Top",])
JunCarryWins <- nrow(WinningDF[WinningDF$Carry=="Jungle" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Jungle",])
MidCarryWins <- nrow(WinningDF[WinningDF$Carry=="Mid" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Mid",])
ADCCarryWins <- nrow(WinningDF[WinningDF$Carry=="ADC" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="ADC",])
SupCarryWins <- nrow(WinningDF[WinningDF$Carry=="Support" & WinningDF$CarryWin==1,])/nrow(WinningDF[WinningDF$Carry=="Support",])

#Create a data frame with the winrate of each role, when that specific role is the carry of the game.
CarryRoleWins <- data.frame(Role = c("Top","Jungle","Mid","ADC","Support"),
                            Winrate= c(TopCarryWins,JunCarryWins,MidCarryWins,ADCCarryWins,SupCarryWins))

ggplot(data=CarryRoleWins, aes(x=Role,y=Winrate,fill=Role)) + geom_bar(stat="identity")
CarryRoleWins

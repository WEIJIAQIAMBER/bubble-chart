library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(readr)
library(matrixStats)
library(RColorBrewer)

bans<-read.csv('banValues.csv')
main<- read.csv('_LeagueofLegends.csv')
sum(is.na(bans))



View(bans)

banGame <- data.frame(
                    MatchHistory = main$MatchHistory,
                    redTeam= main$redTeamTag,
                    blueTeam = main$blueTeamTag,
                    year=main$Year
                   )
View(banGame)
banTotal <- merge(bans,banGame, by="MatchHistory")
write.csv(banTotal,file = "banTotal.csv")
View(banTotal)

bansBlue<-banTotal %>% filter_all(any_vars(. %in% c('Blue')))
names(bansBlue)[names(bansBlue) == "blueTeam"] <- "teamName"
bansRed<-banTotal %>% filter_all(any_vars(. %in% c('Red')))
names(bansRed)[names(bansRed) == "redTeam"] <- "teamName"
View(bansBlue)
View(bansRed)

write.csv(bansBlue, file = "bansBlue.csv")
write.csv(bansRed, file = "bansRed.csv")
#uniqueID=unique(banTotal$MatchHistory)
#View(uniqueID)

bTotal <- read.csv("bTotal.csv")
View(bTotal)

SKT <- bTotal %>% filter_all(any_vars(. %in% c('SKT')))
write.csv(SKT, file = "SKTbans.csv")
View(SKT)
write.csv(SKT, file = "SKTbans.csv")
Champions <- data.frame(
  blueTeamTag = main$blueTeamTag,
  blueTopChamp = main$blueTopChamp,
  blueMiddleChamp = main$blueMiddleChamp,
  blueJungleChamp = main$blueJungleChamp,
  blueADCChamp = main$blueADCChamp,
  blueSupportChamp = main$blueSupportChamp,
  redTeamTag = main$redTeamTag,
  redTopChamp = main$redTopChamp,
  redMiddleChamp = main$redMiddleChamp,
  redJungleChamp = main$redJungleChamp,
  redADCChamp = main$redADCChamp,
  redSupportChamp = main$redSupportChamp,
  Year=main$Year
)
ChampionSKT <- Champions %>% filter_all(any_vars(. %in% c('SKT')))

View(ChampionSKT)
write.csv(ChampionSKT,file = "SKTpicks.csv")

banTotal2015 <- banTotal %>% filter_all(any_vars(. %in% c('2015')))
View(banTotal2015)
pick2016 <- main %>% filter_all(any_vars(. %in% c('2016')))
View(pick2016)
write.csv(pick2016, file = "pick2016.csv")
View(pick2016win)
View(banTotalBlue)
Champion2016 <- data.frame(
  blueTopChamp = pick2016$blueTopChamp,
  blueMiddleChamp = pick2016$blueMiddleChamp,
  blueJungleChamp = pick2016$blueJungleChamp,
  blueADCChamp = pick2016$blueADCChamp,
  blueSupportChamp = pick2016$blueSupportChamp,
  redTopChamp = pick2016$redTopChamp,
  redMiddleChamp = pick2016$redMiddleChamp,
  redJungleChamp = pick2016$redJungleChamp,
  redADCChamp = pick2016$redADCChamp,
  redSupportChamp = pick2016$redSupportChamp
)

write.csv(Champion2016, file = "Champion2016.csv")

View(Champion2016)


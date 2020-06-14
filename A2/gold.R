library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(matrixStats)
library(RColorBrewer)

global<-read.csv('LeagueofLegends.csv',sep=',')
any(is.na(global))

gold<-read.csv('goldValues.csv',sep=',')
death<-read.csv('deathValues.csv',sep=',')
objects<-read.csv('objValues.csv',sep=',')
bans<-read.csv('banValues.csv',sep=',')
#Gold difference vs. time played
game_id<-1
View(game_id)
game1<-data.frame(t(gold[game_id,3:83]))
View(game1)

colnames(game1)<-'gold_diff'
features<-rownames(game1)
rownames(game1)<-1:nrow(game1)
game1$time<-rep(1:nrow(game1))
game1$MatchHistory<-rep(global$MatchHistory[1],nrow(game1))
res<-data.frame(merge(game1, global %>% select(MatchHistory,League,Season,Year,blueTeamTag,bResult, redTeamTag,rResult,gamelength),by='MatchHistory',y.all=T))
res$blueTeam<-ifelse(res$bResult==1,"BLUE:W ; RED:L","BLUE:L ; RED:W")
View(res)
res %>% na.omit() %>% 
  ggplot(aes(x=time,y=gold_diff,shape=blueTeam,fill=ifelse(gold_diff>0,"blue","red"))) + 
  geom_histogram(stat='identity',size=1) + 
  scale_fill_manual(name="Team",values=c(blue="#3B9AB2",red="#F21A00")) + 
  scale_shape_manual(name='Result',values = c(19)) + 
  theme(legend.position='none') + ggtitle(res$blueTeam) + 
  xlab('minutes') + ylab('Gold difference btw Blue and Red team')



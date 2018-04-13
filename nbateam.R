

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(grid)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(ggthemes)
library(bubbles)
library(ggrepel)
library(gridExtra)


nba17 <- fread('/Users/xiaoweicheng/Desktop/2017nba.csv')
nba18 <- fread('/Users/xiaoweicheng/Desktop/2018nba.csv')

#make region
nba18east <- nba18 %>% filter(Team %in% c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Charlotte Hornets","Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Miami Heat","Milwaukee Bucks","New York Knicks","Orlando Magic","Philadelphia 76ers","Toronto Raptors","Washington Wizards")) %>% mutate(region = "east")
nba18west <- nba18 %>% filter(!Team %in% c("Atlanta Hawks","Boston Celtics","Brooklyn Nets","Charlotte Hornets","Chicago Bulls","Cleveland Cavaliers","Detroit Pistons","Indiana Pacers","Miami Heat","Milwaukee Bucks","New York Knicks","Orlando Magic","Philadelphia 76ers","Toronto Raptors","Washington Wizards")) %>% mutate(region = "west")
nba18 <- rbind(nba18east,nba18west)

#2018
#PTS
#Regino PTS vs. Wining %
ptsregion <- ggplot(nba18,aes(PTS,WP))+
  geom_point(aes(colour=region))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  geom_label_repel(aes(PTS, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Each Team PTS vs. Wining %
ptsteam<- ggplot(nba18,aes(PTS,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_label_repel(aes(PTS, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
pts60 <- ggplot(nba18, aes(x=PTS, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Relationship between PTS and Wining %
ptsreg <- ggplot(nba18, aes(x=PTS, y=WP)) +
  geom_point()+
  geom_text(label=nba18$Team)+
  geom_smooth(method = lm,se=FALSE)+
  theme_economist()
##################################################################
#REBOUND
#Checking Rebound top 5 team
mean(nba18$REB)
rebs <- nba18$Team [order (nba18$REB, decreasing = TRUE)]
rebstop5<- ggplot (data = subset (nba18, Team %in% rebs [1 : 5]), 
                   aes (REB, Team)) +
  geom_point (aes (color = factor (region)), size = 4)
#Regino REB vs. Wining %
rebregion <- ggplot(nba18,aes(REB,WP))+
  geom_point(aes(colour=region))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  geom_label_repel(aes(REB, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Each Team REB vs. Wining %
regteam <- ggplot(nba18,aes(REB,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_label_repel(aes(REB, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
reg60 <- ggplot(nba18, aes(x=REB, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Relation ship between REB and Wining %
regreg <- ggplot(nba18, aes(x=REB, y=WP)) +
  geom_point()+
  geom_text(label=nba18$Team)+
  geom_smooth(method = lm,se=FALSE)+
  theme_economist()
######################################################################
#Assistant
#Checking Rebound top 5 team
mean(nba18$AST)
asts <- nba18$Team [order (nba18$AST, decreasing = TRUE)]
aststop5<- ggplot (data = subset (nba18, Team %in% asts [1 : 5]), 
        aes (AST, Team)) +
        geom_point (aes (color = factor (region)), size = 4)
#Regino AST vs. Wining %
astregion <- ggplot(nba18,aes(AST,WP))+
  geom_point(aes(colour=region))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  geom_label_repel(aes(AST, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Each Team AST vs. Wining %
astteam <- ggplot(nba18,aes(AST,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_label_repel(aes(AST, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
ast60 <- ggplot(nba18, aes(x=AST, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Relation ship between AST and Wining %
astreg <- ggplot(nba18, aes(x=AST, y=WP)) +
  geom_point()+
  geom_text(label=nba18$Team)+
  geom_smooth(method = lm,se=FALSE)+
  theme_economist()
#############################################
#Steal
#Checking Steal top 5 team
mean(nba18$STL)
stls <- nba18$Team [order (nba18$STL, decreasing = TRUE)]
stlstop5<- ggplot (data = subset (nba18, Team %in% stls [1 : 5]), 
                   aes (STL, Team)) +
  geom_point (aes (color = factor (region)), size = 4)
#Regino STL vs. Wining %
stlregion <- ggplot(nba18,aes(STL,WP))+
  geom_point(aes(colour=region))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  geom_label_repel(aes(STL, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Each Team STL vs. Wining %
stlteam <- ggplot(nba18,aes(STL,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_label_repel(aes(STL, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
stl60 <- ggplot(nba18, aes(x=STL, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Relation ship between STL and Wining %
stlreg <- ggplot(nba18, aes(x=STL, y=WP)) +
  geom_point()+
  geom_text(label=nba18$Team)+
  geom_smooth(method = lm,se=FALSE)+
  theme_economist()
##########################################################
#Block
#Checking Block top 5 team
mean(nba18$BLK)
blks <- nba18$Team [order (nba18$BLK, decreasing = TRUE)]
blkstop5<- ggplot (data = subset (nba18, Team %in% blks [1 : 5]), 
                   aes (BLK, Team)) +
  geom_point (aes (color = factor (region)), size = 4)
#Regino BLK vs. Wining %
blkregion <- ggplot(nba18,aes(BLK,WP))+
  geom_point(aes(colour=region))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  geom_label_repel(aes(BLK, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Each Team BLK vs. Wining %
blkteam <- ggplot(nba18,aes(BLK,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_label_repel(aes(BLK, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
blk60 <- ggplot(nba18, aes(x=BLK, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Relation ship between BLK and Wining %
blkreg <- ggplot(nba18, aes(x=BLK, y=WP)) +
  geom_point()+
  geom_text(label=nba18$Team)+
  geom_smooth(method = lm,se=FALSE)+
  theme_economist()
###############################################
#Turnover 
#Checking TOV top 5 team
mean(nba18$TOV)
tovs <- nba18$Team [order (nba18$TOV, decreasing = TRUE)]
tovstop5<- ggplot (data = subset (nba18, Team %in% tovs [1 : 5]), 
                   aes (TOV, Team)) +
  geom_point (aes (color = factor (region)), size = 4)
#Regino TOV vs. Wining %
tovregion <- ggplot(nba18,aes(TOV,WP))+
  geom_point(aes(colour=region))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  geom_label_repel(aes(TOV, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Each Team TOV vs. Wining %
tovteam <- ggplot(nba18,aes(TOV,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_label_repel(aes(TOV, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
tov60 <- ggplot(nba18, aes(x=TOV, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Relation ship between TOV and Wining %
tovreg <- ggplot(nba18, aes(x=TOV, y=WP)) +
  geom_point()+
  geom_text(label=nba18$Team)+
  geom_smooth(method = lm,se=FALSE)+
  theme_economist()
################################################
#Filed Goal%, 3P%,Free Throw %
FGP <- ggplot(nba18, aes(y = Team, x = FGP)) + geom_point(colour="blue",size=2,stat = "identity")
TRPP <- ggplot(nba18, aes(y = Team, x = TRPP)) + geom_point(colour="blue",size=2,stat = "identity")
FTP <- ggplot(nba18, aes(y = Team, x = FTP)) + geom_point(colour="blue",size=2,stat = "identity")
grid.arrange(FGP, TRPP,FTP, ncol = 3)
#Each Team FG% vs. Wining %
fgpteam <- ggplot(nba18,aes(FGP,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_label_repel(aes(FGP, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
fgp60 <- ggplot(nba18, aes(x=FGP, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Each Team 3p% vs. Wining %
trppteam <- ggplot(nba18,aes(TRPP,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_label_repel(aes(TRPP, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
trpp60 <- ggplot(nba18, aes(x=TRPP, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Each Team FT% vs. Wining %
ftpteam <- ggplot(nba18,aes(FTP,WP))+
  geom_point()+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  geom_hline(yintercept=0.5, linetype="dashed", color = "red") +
  geom_label_repel(aes(FTP, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#Highlight Team with more than 60% winging rate
ftp60 <- ggplot(nba18, aes(x=FTP, y=WP,fill=factor(Team))) +
  geom_point(size=3,shape=23)+
  geom_text(aes(label=ifelse(WP>.6,as.character(Team),'')),hjust=1,vjust=0)+
  theme_economist()
#Comparison
grid.arrange(fgpteam,trppteam,ftpteam, ncol = 3)
grid.arrange(fgp60,trpp60,ftp60, ncol = 3)

#MLR
lm1 <- lm(nba18$WP ~ nba18$MIN + nba18$PTS + nba18$FGP + nba18$FTP + nba18$TRPP + nba18$REB + nba18$AST + nba18$TOV + nba18$STL + nba18$BLK)
summary(lm)

lm2 <- lm(nba18$WP ~ nba18$PT + nba18$TRPP + nba18$REB + nba18$AST + nba18$STL)
summary(lm_reduce)

lm3 <- lm(nba18$WP ~ nba18$PT + nba18$TRPP + nba18$AST + nba18$STL)
summary(lm_rereduce)












ggplot(nba18, aes(x= PTS, y = W)) + 
  geom_point(color = "blue", size = 3) +
  geom_label_repel(aes(PTS, W, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50') +
  theme_classic(base_size = 12) +
  ggtitle("NBA 2017-2018 Season")


ggplot(nba18, aes(x= PTS, y = WP)) + 
  geom_point(color = "blue", size = 3) +
  geom_label_repel(aes(PTS, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50') +
  theme_classic(base_size = 12) + 
  ggtitle("NBA 2016-2017 Season")


ggplot(nba18,aes(PTS,WP))+
  geom_point(aes(colour=Team))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  #geom_label_repel(aes(PTS, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
  geom_text(aes(label=ifelse(WP>0.5,as.character(Team),'')),hjust=1,vjust=0)


ggplot(nba18,aes(PTS,WP))+
  geom_point(aes(colour=region))+
  ggtitle("Scatter Plot")+
  theme_economist(base_size=10)+
  guides(colour=guide_legend(title=NULL)) +
  geom_label_repel(aes(PTS, WP, label = Team),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50')
#geom_text(aes(label=ifelse(WP>0.5,as.character(Team),'')),hjust=1,vjust=0)



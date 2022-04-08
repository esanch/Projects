# Assignment: Final Project
# Name: Earl, Elizabeth
# Date: 2021-03-03
library(Hmisc)
library(tidyr)
setwd("C:/Users/Eearl/OneDrive/Documents/School/530")

arrests_df <- read.csv("ArrestsPremierLocationOffence2013-14Table9.csv")
clean_df <-arrests_df[ -c(5:10, 13:15) ]
par(mar=c(1,1,1,1))
home<- ggplot(clean_df, aes(clean_df$Premier_League_team_supported, clean_df$Arrests_at_Home_matches)) +
  geom_col()  +scale_x_discrete(guide = guide_axis(n.dodge=3))+ labs(title = "Home Arrests by Team",
                    x = "Team", y = "Arrests")
away <- ggplot(clean_df, aes(clean_df$Premier_League_team_supported, clean_df$Arrests_at_Away_matches)) +
  geom_col()  +scale_x_discrete(guide = guide_axis(n.dodge=3))+ labs(title = "Away Arrests by Team",
                                                                     x = "Team", y = "Arrests")
alcohol <- ggplot(clean_df, aes(clean_df$Premier_League_team_supported, clean_df$Alcohol_Offences)) +
  geom_col()  +scale_x_discrete(guide = guide_axis(n.dodge=3))+ labs(title = "Alcohol Arrests by Team",
                                                                     x = "Team", y = "Arrests")
ticket <- ggplot(clean_df, aes(clean_df$Premier_League_team_supported, clean_df$Ticket_Touting)) +
  geom_col()  +scale_x_discrete(guide = guide_axis(n.dodge=3))+ labs(title = "Ticket Touting Arrests by Team",
                                                                     x = "Team", y = "Arrests")
property <- ggplot(clean_df, aes(clean_df$Premier_League_team_supported, clean_df$Offences_against_Property)) +
  geom_col()  +scale_x_discrete(guide = guide_axis(n.dodge=3))+ labs(title = "Property Offences Arrests by Team", x = "Team", y = "Arrests")



homeData <- data.frame(clean_df[2])
homeVec<-homeData$Arrests_at_Home_matches
homeMean <- mean(homeVec)
homeMode <- Mode(homeVec)

awayData <- data.frame(clean_df[3])
awayVec<-awayData$Arrests_at_Away_matches
awayMean <- mean(awayVec)
awayMode <- Mode(awayVec)

alcoholData <- data.frame(clean_df[4])
alcoholVec<-alcoholData$Alcohol_Offences
alcoholMean <- mean(alcoholVec)
alcoholMode <- Mode(alcoholVec)

ticketData <- data.frame(clean_df[5])
ticketVec<-ticketData$Ticket_Touting
ticketMean <- mean(ticketVec)
ticketMode <- Mode(ticketVec)

propertyData <- data.frame(clean_df[6])
propertyVec<-propertyData$Offences_against_Property
propertyMean <- mean(propertyVec)
propertyMode <- Mode(propertyVec)

s<- spread(clean_df, Premier_League_team_supported, Premier_League_team_supported)


dev.off()
normDis<- cumsum(clean_df$Arrests_at_Home_matches)
normDis
#plot(clean_df$Total_Arrests,normDis, ylab="Home Arrests",col=5,xlab="Total Arrests", main="Normal Distribution")
curve(dnorm(x,m=10,sd=2),from=0,to=normDis,main="Normal distribution",xlab="Home Arrests", col=7)

dev.off()
plot(clean_df$Total_Arrests,clean_df$Arrests_at_Home_matches,type="h",main="PMF" , ylab="Home Arrests",col=2,xlab="Total Arrests")
> points(DD,PP,col=2);abline(h=0,col=3)

plot(ecdf(clean_df$Arrests_at_Home_matches),xlab="Home Arrests",col=4, main="CDF")

ggplot(clean_df, aes(x=clean_df$Arrests_at_Home_matches, y= clean_df$Arrests_at_Away_matches,colour = "")) + geom_point() +scale_x_discrete(guide = guide_axis(n.dodge=3))+ labs(title = "Home v. Away Arrests",x = "Home Arrests", y = "Away Arrests")
plot( clean_df$Arrests_at_Home_matches ~ clean_df$Alcohol_Offences , pch = 19, col = "pink", main = "Home v. Alcohol Arrests",ylab = "Home Arrests", xlab = "Alcohol Arrests")
plot(clean_df$Arrests_at_Away_matches ~clean_df$Alcohol_Offences , pch = 19, col = "green", main = "Away v. Alcohol Arrests",ylab = "Away Arrests", xlab = "Alcohol Arrests")

normProperty<- cumsum(clean_df$Offences_against_Property)
t.test(clean_df$Offences_against_Property, normProperty, var.equal = TRUE)

model <- lm(clean_df$Total_Arrests ~ clean_df$Arrests_at_Home_matches + clean_df$Arrests_at_Away_matches + clean_df$Alcohol_Offences + clean_df$Ticket_Touting + clean_df$Offences_against_Property , data=clean_df)
summary(model)

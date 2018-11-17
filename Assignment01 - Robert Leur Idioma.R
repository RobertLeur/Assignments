WHO = read.csv('WHO.csv')
D. Find the country with the lowest literacy rate.

literacy_min<-min(WHO$LiteracyRate,na.rm = TRUE)
lowest_literacy<-subset(WHO,LiteracyRate==literacy_min)
print (lowest_literacy$Country)


E. Find the richest country in Europe based on GNI

Europe <- subset(WHO, Region == 'Europe')
GNI_Europe_Max <- max(Europe$GNI,na.rm = TRUE)
GNI_Europe_Max_Row <- subset(Europe, GNI == GNI_Europe_Max)
print(GNI_Europe_Max_Row$Country)

F. The mean life expectancy of countries in Africa

Africa <- subset(WHO, Region == 'Africa')
mean_life_expectancy_africa <- mean(Africa$LifeExpectancy, na.rm = TRUE)
mean_life_expectancy_africa


G. Find the number of countries with a population over 10M

Countries_10M <- subset(WHO, Population > 10000)
dim(Countries_10M)[1]


H. Find names of the country (top 5) in the Americas with the highest child mortality rate

Americas <- subset(WHO, Region == 'Americas')
Childmortality_dec_order <- order(Americas$ChildMortality, decreasing = TRUE)
Americas_Childmortality_Order <- Americas[Childmortality_dec_order,]
top5_Childmortality_head <- head(Americas_Childmortality_Order,5)
top5_Childmortality_index <- Americas_Childmortality_Order[1:5,]
top5_Childmortality_index$Country

2. The NBA Historical Performance Dataset

library('readxl')
historical_nba <- read_excel('Historical NBA Performance.xlsx')

2.a Find the year the Chicago Bulls has its highest winning percentage

bulls <- subset(historical_nba, Team == 'Bulls')
bulls_highest_winpct <- max(bulls$`Winning Percentage`)
bulls_row_highest_winpct <- subset(bulls, `Winning Percentage` == bulls_highest_winpct)
bulls_row_highest_winpct$Year

2.b Find teams with an even win-loss record (i.e. the teams whose recorded win pct for the year are 0.500).

teams_even <- subset(historical_nba, `Winning Percentage` == 0.5)
teams_even


3. The season stats dataset

season_stats <- read.csv('Seasons_Stats.csv')
head(season_stats)

3a. Find the player with the highest three point attempt rates

season_stats_combined <- season_stats %>%
  select(Year, Player, X3PA, X2PA) %>%
  filter(Year >1979) %>%
  group_by(Year, Player) %>%
  summarize(X3PA = sum(X3PA), X2PA = sum(X2PA)) %>%
  mutate(X3PAr = X3PA / (X3PA+X2PA))

subset(season_stats_combined, X3PAr == max(season_stats_combined$X3PAr, na.rm = TRUE))

3.b Find the players with the highest free throw rate in season

season_stats_combined_ftr <- season_stats %>%
  select(Year, Player, FTA, FGA) %>%
  group_by(Year, Player) %>%
  summarize(FTA= sum(FTA), FGA = sum(FGA)) %>%
  mutate(FTr = FTA/FGA)

season_stats_combined_ftr <- season_stats_combined_ftr[!is.infinite(season_stats_combined_ftr$FTr),]


subset(season_stats_combined_ftr, FTr == max(season_stats_combined_ftr$FTr, na.rm = TRUE))

3.c Find the year LeBron James recorded his highest number of points

KingJames <- subset(season_stats, Player == 'LeBron James')
KingJames_max_pts <- max(lbj$PTS, na.rm = TRUE)
subset(KingJames, PTS == KingJames_max_pts)$Year

3.d 

MJ23 <- subset(season_stats, Player == 'Michael Jordan*')
subset(MJ23, PTS == max(MJ23$PTS))$Year

3.e

kobe <- subset(season_stats, Player =='Kobe Bryant')
subset(kobe, MP == min(kobe$MP))$PER


4. The National Universities Rankings

universities <-read.csv('National Universities Rankings.csv')

4.a

print(universities[which.max(universities$Undergrad.Enrollment),]$Name)

4.b

top_10 <- universities[order(universities$Rank),][1:10,]

top_10$tuition_no_dollar <- gsub(pattern = "\\$|\\,",replacement = "", top_10$Tuition.and.fees)
mean(as.numeric(top_10$tuition_no_dollar))

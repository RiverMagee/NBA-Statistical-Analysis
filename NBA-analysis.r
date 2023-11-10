# Data Analytics (CS301) 

################################################

# Names: 
print("The names of this group are the following.")
print("River Magee")
print("Luke Lacaria")

################################################

print("The name of this group is the following.")
print("Young Data Specialists")

rm(list = ls()) # clear out the variables
# from memory to make a clean execution of the code.

# If you want to remove all previous plots
# and clear the console, run the following two lines.
graphics.off() # clear out all plots from previous work.

cat("\014") # clear the console

################################################

# Instructions: Complete this code using
# RStudio by following the steps in README.md.
# Note: your code must be able to
# execute without errors. Please be sure
# that comments are commended, and code
# is coded correctly without typographical errors.

# Load the required Libraries
library(dplyr)
library(tidyverse)

##### Begin by loading your csv file -> Path Name
fname <- "/Users/20riverm/data_analytics_301/final-project-young-data-specialists/data/NBA_data - 3-point-data.csv"
###### Begin by loading your csv file -> Select the data manually
###### Use file: NBA_data - 3-point-data.csv
fname <- file.choose()
# Create the dataset
NBAdata <- read.csv(fname, header = TRUE, sep = ",")

# View the data set.

View(NBAdata)

# Edits the data set as needed.
# Removes commas from the data to prepare them to convert to numeric values.
NBAdata$TOTAL.EARNINGS <- gsub(",", "", NBAdata$TOTAL.EARNINGS)

#Converts data type from characters to numeric.
NBAdata$TOTAL.EARNINGS <- as.numeric(NBAdata$TOTAL.EARNINGS)
NBAdata$Yrs <- as.numeric(NBAdata$Yrs)
NBAdata$ALL.STAR <- as.numeric(NBAdata$ALL.STAR)

# Create a new column that averages the earnings out over the career period.
NBAdata$avg.Earnings <- NBAdata$TOTAL.EARNINGS / NBAdata$Yrs

--------------------------------------
#Question #1: How accurate are draft picks in predicting career success, measured by All-Star Appearances?
  
# Isolates the players to only #1 Draft Picks.
Num1Draft <- NBAdata %>% filter(Pk == "1")

# Sorts the data set by All_Star column in descending order
Num1Draft_sorted <- Num1Draft[order(-Num1Draft$ALL.STAR),]

# Creates a plot to visualize the number of #1 draft picks and the amount of all star selections they have received.
ggplot(data = Num1Draft_sorted) +
  geom_point(mapping = aes(x = reorder(Player, -ALL.STAR), y = ALL.STAR)) +
  ylab("All Star Selections") + xlab("Name of Player")

# Different visualizations depending on the data type.
NBAdata$Pk <- as.character(NBAdata$Pk)
NBAdata$Pk <- as.numeric(NBAdata$Pk)

ggplot(data = NBAdata) +
  geom_point(mapping = aes(x = Pk, y = ALL.STAR)) +
  ylab("All Star Selections") + xlab("Pick Number")

# Calculating percentage likely hood of becoming an All Star at some point in career.
Pick <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

# Percent chance of becoming an All star once in career. 
# Calculated by dividing the number of All_Stars by the total number of seasons for the specified draft pick.
AllStarChance <- c(42, 21, 42, 21, 26, 15, 15, 5, 21, 15)

# Percent Chance of becoming an All Star 5x in career.
# Calculated by dividing the number of 5x All_Stars by the total number of seasons for the specified draft pick.
AllStarChanceX5 <- c(26, 10.5, 21, 10.5, 10.5, 5.26, 5.26, 0, 5.26, 5.26)

# Creates a data frame using the percent chance of being an all star once calculations.
Chance1xAllStar <- data.frame(Pick = Pick, AllStarChance = AllStarChance)

# Graph the percentage chance of making All-Star 5X.
ggplot(data = Chance1xAllStar, aes(x = Pick, y = AllStarChance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ylab("% Chance of Becoming All-Star Once") + 
  xlab("Pick Number") +
  scale_x_continuous(limits = c(0, 10), breaks = 0:10)

# Creates a data frame using the percent chance of being a 5x all star calculations.
Chance5xAllStar <- data.frame(Pick = Pick, AllStarChance = AllStarChanceX5)

# Graph the percentage chance of making All-Star once.
ggplot(data = Chance5xAllStar, aes(x = Pick, y = AllStarChance)) +
  geom_bar(stat = "identity", fill = "pink") +
  ylab("% Chance of Becoming All-Star 5x") + xlab("Pick Number")

# Runs a linear regression model for each of the percent chances.
modPick <- lm(Pick ~ AllStarChance)
modPickX5 <- lm(Pick ~ AllStarChanceX5)

# Gives the summary of information for each regression model.
summary(modPick)
summary(modPickX5)

--------------------------------------------------------------------
#Question #2: How does career shooting efficiency impact all-star appearances and or relative salary in the league?

# Creates a data subset including players that have played over 1800 minutes in their career.
# This effectively weeds out players that may be altering the data with inaccuracies due to limited game experience.
MinutesPerYear <- NBAdata %>% filter(MP/Yrs > 1800)  
  
# Create a plot that maps 3pt percentage and avg.earnings.
ggplot(data = MinutesPerYear) +
  geom_point(mapping = aes(x = avg.Earnings, y = FG., color = "2 Pointers")) +
  geom_point(mapping = aes(x = avg.Earnings, y = X3P., color = "3 Pointers")) +
  ylab("Shooting Percentage") + xlab("Avg Earnings Per Year")

# Create a plot that maps 3pt percentage and avg.earnings.
ggplot(data = MinutesPerYear) +
  geom_point(mapping = aes(x = ALL.STAR, y = FG., color = "2 Pointers")) +
  geom_point(mapping = aes(x = ALL.STAR, y = X3P., color = "3 Pointers")) +
  ylab("Shooting Percentage") + xlab("All Star Appearances")

# Calculates the mean averages for each shooting percentage and their corresponding all-star values.
# Initialize an empty list to store the average shooting percentage data.
results <- list()

# Loop through each value of ALL.STAR and calculate the mean of X3P
for (i in 0:18) {
  mean_X3P <- mean(NBAdata$X3P.[NBAdata$ALL.STAR == i], na.rm = TRUE)
  mean_FG <- mean(NBAdata$FG.[NBAdata$ALL.STAR == i], na.rm = TRUE)
  results[[i+1]] <- c(i, mean_X3P, mean_FG)
}

# Convert the list to a data frame
meanShooting <- data.frame(do.call(rbind, results))

# Set column names
colnames(meanShooting) <- c("ALL.STAR", "Mean_X3P", "Mean_FG")

# View the resulting data frame
View(meanShooting)

# Creates a plot for the averages of shooting and all-star selections.
# The following plot is used to describe 2-point field goal efficiency.
ggplot(data = meanShooting) +
  geom_point(mapping = aes(x = ALL.STAR, y = Mean_FG, color = "blue")) +
  geom_smooth(mapping = aes(x = ALL.STAR, y = Mean_FG, color = "blue"), se = FALSE) +
  ylab("Shooting Percentage") + xlab("All Star Appearances")

# The following plot is used to describe 3-point field goal efficiency.
ggplot(data = meanShooting) +
  geom_point(mapping = aes(x = ALL.STAR, y = Mean_X3P, color = "red")) +
  geom_smooth(mapping = aes(x = ALL.STAR, y = Mean_X3P, color = "red"), se = FALSE) +
  ylab("Shooting Percentage") + xlab("All Star Appearances")

# Runs linear regression models for each of the plots.
mod3pt <- lm(data = meanShooting, ALL.STAR ~ Mean_X3P)
modFG <- lm(data = meanShooting, ALL.STAR ~ Mean_FG)

# Summarizes the information regarding the regression models.
summary(mod3pt)
summary(modFG)

# Creates plots for shooting efficiency and win-share percentages.
# The following plot is used to describe 2-point field goal efficiency.
ggplot(data = NBAdata) +
  geom_point(mapping = aes(x = WS.48, y = FG.), color = "blue") +
  geom_smooth(mapping = aes(x = WS.48, y = FG.), color = "blue") +
  ylab("2PT Shooting Percentage") + xlab("Win Shares per 48 Minutes")

# The following plot is used to describe 3-point field goal efficiency.
ggplot(data = NBAdata) +
  geom_point(mapping = aes(x = WS.48, y = X3P.), color = "red") +
  geom_smooth(mapping = aes(x = WS.48, y = X3P.), color = "red") +
  ylab("3PT Shooting Percentage") + xlab("Win Shares per 48 Minutes")

# Runs linear regression models for each of the plots.
modWin3pt <- lm(data = NBAdata, WS.48 ~ X3P.)
modWinFG <- lm(data = NBAdata, WS.48 ~ FG.)

# Summarizes the information regarding the regression models.
summary(modWin3pt)
summary(modWinFG)

-----------------------------------------------------------
# Question #3: In what ways do assist and rebound totals contribute to individual player success?

# Creates plots for assist totals and win-share percentages.
ggplot(data = NBAdata) +
  geom_point(mapping = aes(x = WS.48, y = AST.1), color = "purple") +
  geom_smooth(mapping = aes(x = WS.48, y = AST.1), color = "purple") +
  ylab("Assists Per Game") + xlab("Win Shares per 48 Minutes") 

# Creates plots for rebound totals and win-share percentages.
ggplot(data = NBAdata) +
  geom_point(mapping = aes(x = WS.48, y = TRB.1), color = "orange") +
  geom_smooth(mapping = aes(x = WS.48, y = TRB.1), color = "orange") +
  ylab("Rebounds Per Game") + xlab("Win Shares per 48 Minutes") 
  
# Runs linear regression models for each of the plots.
modWinAst <- lm(data = NBAdata, WS.48 ~ AST.1)
modWinTrb <- lm(data = NBAdata, WS.48 ~ TRB.1)

# Summarizes the information regarding the regression models.
summary(modWinAst)
summary(modWinTrb)
  
-----------------------------------------------------------
#Question #4: Which colleges / teams generally select the best players?

# Get the unique list colleges
colleges <- unique(NBAdata$College)

# Retrieves the number of occurrences each college has in the entire data set.
college_counts <- table(NBAdata$College)

# Turns this data into a data.frame to analysis.
college_counts_df <- data.frame(college_counts)

# Removes all picks that aren't associated with college, having empty values.
college_counts_df <- college_counts_df[college_counts_df$Var1 != "", ]

# Arrange them in order from highest to lowest.
college_counts_df <- arrange(college_counts_df, desc(Freq))

# Creates a subset of the top 10 colleges with the most draft picks out of the data set.
college_counts_df_10 <- head(college_counts_df, 10)

# Loop through each value of Colleges and calculate the mean of Win-share percentages for each college.
results <- list()
for (i in 1:length(colleges)) {
  mean_win_share <- mean(NBAdata$WS.48[NBAdata$College == colleges[i]], na.rm = TRUE)
  results[[i]] <- c(colleges[i], mean_win_share)
}

# Combine results into a data frame for analysis. 
meanWinShare <- data.frame(do.call(rbind, results))

# Rename the columns.
colnames(meanWinShare) <- c("College", "Mean_WS.48")

# Rearrange the colleges in descending order, from highest mean win-share to lowest.
meanWinShare <- arrange(meanWinShare, desc(Mean_WS.48))

# Convert Win-Share to a numeric data type.
meanWinShare$Mean_WS.48 <- as.numeric(meanWinShare$Mean_WS.48)

# View the finalized data frame.
View(meanWinShare)

# Filter the top 10 colleges for NBA players in the meanWinShare Data set.
top_10_Colleges <- c("Duke", "Kentucky", "Arizona", "UConn", "Kansas", "Texas", "Florida", "Washington", "Georgetown", "Indiana")

# Creates a new data set and then arranges the data in descending order.
# This is a subset that only includes the values of the top 10 colleges of the data set.
meanWinShare_Top10 <- meanWinShare[meanWinShare$College %in% top_10_Colleges, ]
meanWinShare_Top10 <- arrange(meanWinShare_Top10, desc(Mean_WS.48))

###
# GRAPH SECTION

# Creates a plot for colleges, and the amount of top 10 picks they have generated.
ggplot(data = college_counts_df_10, mapping = aes(x = reorder(Var1, - Freq), y = Freq)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  xlab("Colleges (Top 10)") + ylab("Amount of Top 10 Selections")

# Creates plots for colleges and their respective mean win share values.
ggplot(data = meanWinShare_Top10, mapping = aes(x = reorder(College, - Mean_WS.48), y = Mean_WS.48)) +
  geom_bar(stat = "identity", fill = "pink") +
  xlab("Colleges (Top 10)") + ylab("Win Share Per 48 Minutes")
  
-------------------------------------------------
# Programming GraveYard (All of this code is now irrelevent as it pretains to my analysis.)

# Create a plot that maps minutes played, points per game, and avg.earnings.
ggplot(data = NBAdata) +
  geom_point(mapping = aes(x = avg.Earnings, y = PTS.1, color = "Points Per Game")) +
  # geom_point(mapping = aes(x = avg.Earnings, y = MP, color = "Minutes Played")) +
  ylab("Points & Minutes Played") + xlab("Avg Earnings Per Year")

# Create a plot to visualize all-star selections compared to draft picks.
ggplot(data = NBAdata) +
  geom_point(mapping = aes(x = Pk, y = ALL.STAR, color = "Players")) +
  ylab("All Star Selections") + xlab("Draft Number")

# Runs multi-linear regression model.
modPick <- lm(data = NBAdata, ALL.STAR ~ Pk)

summary(modPick)

--------------------------
# Programming GraveYard
teams <- unique(NBAdata$Tm)
team_counts <- table(NBAdata$Tm)


# Loop through each value of Colleges and calculate the mean of Win-share percentage.
results <- list()
for (i in 1:length(teams)) {
  mean_win_share <- mean(NBAdata$WS.48[NBAdata$Tm == teams[i]], na.rm = TRUE)
  results[[i]] <- c(teams[i], mean_win_share)
}

# Combine results into a data frame
meanWinShare <- data.frame(do.call(rbind, results))

# Rename the columns.
colnames(meanWinShare) <- c("Team", "Mean_WS.48")

# Rearrange the colleges in descending order.
meanWinShare <- arrange(meanWinShare, desc(Mean_WS.48))

# Convert Win-Share to a numeric data type.
meanWinShare$Mean_WS.48 <- as.numeric(meanWinShare$Mean_WS.48)

# View the resulting data frame
View(meanWinShare)

# Subset only the top ten colleges.
meanWinShare_Top10 <- head(meanWinShare, 10)

################################################

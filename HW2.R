#Nicholas Taubert, HW2
#Github: cubone8055

#1. Run the following lines and study how they work. Then state what they do and output for us.

df1=data.frame(Name=c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
                      'Richards','George','Ema','Samantha','Catherine'),
               State=c('Alaska','California','Texas','North Carolina','California','Texas',
                       'Alaska','Texas','North Carolina','Alaska','California','Texas'),
               Sales=c(14,24,31,12,13,7,9,31,18,16,18,14))
#This long line creates a table called df1 that has 3 columns (Name, State, and Sales) and
#Populates these columns with the values listed.


aggregate(df1$Sales, by=list(df1$State), FUN=sum)
#Expected output:
# Group.1  x
# 1         Alaska 39
# 2     California 55
# 3 North Carolina 30
# 4          Texas 83

#This line takes the SUM of all sales for each state (adds the values in the
#Sales column for each row that have the same value in the State column)

library(dplyr)
df1 %>% group_by(State) %>% summarise(sum_sales = sum(Sales))

#Expected output:
# A tibble: 4 x 2
# State          sum_sales
# <fct>              <dbl>
# 1 Alaska                39
# 2 California            55
# 3 North Carolina        30
# 4 Texas                 83

#These lines first download the required library, then groups df1 by state and
#creates a column called sum_sales that has, as the name implies, rows with the
#sum of sales that share a state value. This processes is, in function, the same
#as the previous command.


#2. Use R to read the WorldCupMatches.csv from the DATA folder on Google Drive. Then perform the followings
Matches=read.csv('C:\\Users\\nickm\\Downloads\\WorldCupMatches.csv', header=T)

#(a) Find the size of the data frame. How many rows, how many columns?
dim(Matches)

#(b) Use summary function to report the statistical summary of your data.
summary(Matches)

#(c) Find how many unique locations olympics were held at.
length(unique(Matches$City))

#(d) Find the average attendance.
mean(Matches$Attendance)
#if you wanted to not get NA, omit the NA:
mean(na.omit(Matches$Attendance))

#(e) For each Home Team, what is the total number of goals scored? (Hint: Please refer to question 1)
aggregate(Matches$Home.Team.Goals, by=list(Matches$Home.Team.Name), FUN=sum)

#(f) What is the average number of attendees for each year? Is there a trend or pattern in the data in that sense?
aggregate(Matches$Attendance, by=list(Matches$Year), mean)
#Output:
# Group.1        x
# 1     1930 32808.28
# 2     1934 21352.94
# 3     1938 20872.22
# 4     1950 47511.18
# 5     1954 29561.81
# 6     1958 23423.14
# 7     1962 27911.62
# 8     1966 48847.97
# 9     1970 50124.22
# 10    1974 49098.76
# 11    1978 40678.71
# 12    1982 40571.60
# 13    1986 46039.06
# 14    1990 48388.75
# 15    1994 68991.12
# 16    1998 43517.19
# 17    2002 42268.70
# 18    2006 52491.23
# 19    2010 49669.62
# 20    2014       NA
#Based on this, we see a trend of average attendance increasing directly as year increases.

#3. Use R to read the metabolites.csv from the DATA folder on Google Drive. Then perform the followings
Metabolites=read.csv('C:\\Users\\nickm\\Downloads\\metabolite.csv', header=T)

#(a) Find how many Alzheimers patients there are in the data set. (Hint: Please refer to question 1)
length(which(Metabolites$Label=='Alzheimer'))

#(b) Determine the number of missing values for each column. (Hint: is.na( ) )
sum(is.na(Metabolites))

#(c) Remove the rows which has missing value for the Dopamine column and assign the result to a new data frame
#(Hint: is.na( ) )
NewMetabolites = Metabolites[is.na(Metabolites$Dopamine)==FALSE, ]
head(NewMetabolites)

#(d) In the new data frame, replace the missing values in the c4-OH-Pro column with the median value of the same
# column. (Hint: there is median( ) function.)
NewMetabolites[is.na(NewMetabolites$c4.OH.Pro)] <- median(NewMetabolites$c4.OH.Pro)
head(NewMetabolites)

#(e) (Optional) Drop columns which have more than 25% missing values. (Hint: when you slice your data frame, you
#can use -c(.., ..., ...) where ... represent one column name)
## Remove columns with more than 50% NA
NewMetabolites[, which(colMeans(!is.na(NewMetabolites)) > 0.25)]
head(NewMetabolites)


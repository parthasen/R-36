setwd("/Users/sung/Desktop/STA141/HW6")

library(DBI)
library(RSQLite)
library(ggplot2)
library(reshape2)
drv = dbDriver("SQLite")
con = dbConnect(drv, "baseball-archive-2011.sqlite")
dbListTables(con)

rs = dbSendQuery(con, "SELECT * FROM Batting;")
class(rs)
#dbGetInfo(rs)
tmp = fetch(rs, 100)
dbClearResult(rs)

dbGetQuery(con, "SELECT * FROM Batting WHERE yearID == 2004 ;")
dbGetQuery(con, "SELECT min(yearID), max(yearID) FROM Salaries;")
dbGetQuery(con, "SELECT max(salary) FROM Salaries WHERE yearID == 2012 ;")
maxSalByYear = dbGetQuery(con, "SELECT max(salary), yearID FROM Salaries GROUP BY yearID;")
#####


#1
#.schema Batting


#5

win_world = dbGetQuery(con, "SELECT yearID, teamID, W, WSWin FROM Teams WHERE yearID >= 1903 AND yearID != 1904 AND yearID != 1994;
")
world = ifelse(win_world$WSWin == "Y", 1, 0)
world[is.na(world)] = 0
win = win_world$W
cor(win, world)

win_world[101:300,]


model<- glm(world ~ win, family=binomial("logit"))
summary(model)

#7
team_salary = dbGetQuery(con, "SELECT yearID, teamID, SUM(salary) FROM Salaries GROUP BY teamID, yearID;
")
names(team_salary)[3] = "Salary"
names(team_salary)
ggplot(team_salary, aes(x =yearID, y = Salary, colour = teamID)) + geom_line()

#8
info = dbGetQuery(con, "SELECT yearID, teamID, SUM(salary) FROM Salaries GROUP BY teamID, yearID;")
names(info)[3] = "Salary"
info 
diff(info$Salary)/ x[-length(info$Salary)]
tapply(info$Salary, info$yearID, function(x) sum(diff(x)/x[-length(x)]))

#9
content = "SELECT A.lgID, SUM(A.salary) salary, B.divID FROM Salaries A, Teams B GROUP BY A.teamID ORDER BY B.divID;"
league = dbGetQuery(con, content)
league
AL = league[league$lgID =="AL",2:3]
NL = league[league$lgID =="NL",2:3]
ggplot(AL, aes(x = teamID, y = salary, fill = teamID)) + geom_bar() + ggtitle("American League")
ggplot(NL, aes(x = teamID, y = salary, fill = teamID)) + geom_bar() + ggtitle("National League")



barplot(league[league$lgID =="AL",2:3])
league[league$lgID =="AL",2:3]
barplot(league[,2],league[,3])
par(mfrow = c(3, 3))
sapply(1:9, function(x) 
  plot(p.byCounty[[x]][,1], p.byCounty[[x]][,2], xlim = c(0, 5000), 
       ylim = c(0,3000000), col = cols_2[p.byCounty[[x]][,3]], 
       xlab = "Building Square feet", ylab = "Price", main = paste(county_name[x])
  )
)

#10
HR = dbGetQuery(con, "SELECT yearID, SUM(HR) FROM Teams GROUP BY yearID;")
HR40 = dbGetQuery(con, "SELECT yearID, COUNT(HR) FROM Batting WHERE HR >=40 GROUP BY yearID;")
HR
plot(HR[,1], HR[,2], type="l", col="red", xlab = "Year", ylab = "Total Home Run", main = "HR in Each Year of MLB")
points(HR[136, 1], HR[136, 2], col = "blue", cex = 1.0)
text(HR[120, 1], HR[137, 2], labels="Joint Drug Prevention and\n Treatment Program Start", cex = .5)

plot(HR40[, 1], HR40[, 2], type="l", col="red", xlab = "Year", ylab = "Number of Home Run more than 40", main = "HRs(more than 40) in Each Year of MLB")
points(HR40[73, 1], HR40[73, 2], col = "blue", cex = 1.0)
text(HR40[60, 1], HR40[73, 2], labels="Joint Drug Prevention and\n Treatment Program Start", cex = .5)
HR40



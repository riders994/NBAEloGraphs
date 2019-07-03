library(ggplot2)
library(tidyr)

OLD_ELOS_FILE = "weekly_elo_2017.csv"
CURR_ELOS_FILE = "weekly_elo.csv"

old_elos = read.csv(OLD_ELOS_FILE)
current_elos = read.csv(CURR_ELOS_FILE)

week = dim(current_elos)[2] - 2

names(current_elos)[names(current_elos) == "X"] <- "Players"
names(old_elos)[names(old_elos) == "X"] <- "Players"

names_string_old = c('Rohan', 'John', 'Ben', 'Guillem', 'Andrew', 'Neil', 'Ravi', 'Alison', 'Chris', 'Sahil')
names_factor_old = c(1:10)
names_string_curr = c('Rohan', 'John', 'Ben', 'Guillem', 'Alex', 'Neil', 'Ravi', 'Alison', 'Chris', 'Sahil')
names_factor_curr = c(1:4, 11, 6:10)
name_levs = c(names_string_old, 'Alex')
old_elos$Players = names_factor_old
current_elos$Players = names_factor_curr

week_levs = c()
week_factor = c()
curr_week_fac = c()
curr_week_levs = c()
for (i in (1:(week + 25))){
  week_factor = c(week_factor, rep(i, 10))
  week_levs = c(week_levs, paste('week_', i - 1, sep=''))
  if (i >= 25){
    curr_week_fac = c(curr_week_fac, rep(i, 10))
    curr_week_levs = c(curr_week_levs, paste('week_', i - 25, sep=''))
  }
}
curr_week_fac = factor(curr_week_fac)
week_factor = factor(week_factor)
levels(week_factor) = week_levs
levels(curr_week_fac) = curr_week_levs

long_old_elos = old_elos %>% gather(week, elo, -c(Players))
long_curr_elos = current_elos %>% gather(week, elo, -c(Players))

Long_Elos = rbind(long_old_elos, long_curr_elos)
Long_Elos$week = week_factor
Long_Elos$Players = factor(Long_Elos$Players)
levels(Long_Elos$Players) = name_levs

long_curr_elos$Players[long_curr_elos$Players == 11] = 5
long_curr_elos$Players = factor(long_curr_elos$Players)
long_curr_elos$week = curr_week_fac
levels(long_curr_elos$Players) = names_string_curr

ggplot(Long_Elos, aes(x=week, y=elo, group=Players, color=Players)) + geom_line()
ggplot(long_curr_elos, aes(x=week, y=elo, group=Players, color=Players)) + geom_line()


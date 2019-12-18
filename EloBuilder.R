  library(ggplot2)
  library(tidyr)
  
  CURR_ELOS_FILE = "weekly_elo.csv"
  
  current_elos = read.csv(CURR_ELOS_FILE)
  
  d = dim(current_elos)
  week = d[2] - 2
  
  names(current_elos)[names(current_elos) == "X"] <- "Players"
  
  names_string_curr = c('Rohan', 'John', 'Ben', 'Guillem', 'Alex', 'Neil', 'Ravi', 'Alison', 'Chris', 'Sahil')
  
  current_elos$Players = as.factor(current_elos$Players)
  
  week_levs = c()
  week_factor = c()
  curr_week_fac = c()
  curr_week_levs = c()
  for (i in (1:(week + 1))){
    week_factor = c(week_factor, rep(i, d[1]))
    week_levs = c(week_levs, paste('week_', i - 1, sep=''))
  }
  
  week_factor = factor(week_factor)
  levels(week_factor) = week_levs
  
  Long_Elos = current_elos %>% gather(week, elo, -c(Players))
  Long_Elos$week = week_factor
  
  ggplot(Long_Elos, aes(x=week, y=elo, group=Players, color=Players)) + geom_line()
  

library(ggplot2)
library(tidyr)

build_elo_graph = function(csv_file, output_png) {
  current_elos = read.csv(csv_file)

  d = dim(current_elos)
  week = d[2] - 2

  names(current_elos)[names(current_elos) == "X"] <- "Players"

  current_elos$Players = as.factor(current_elos$Players)

  week_levs = c()
  week_factor = c()
  for (i in (1:(week + 1))){
    week_factor = c(week_factor, rep(i, d[1]))
    week_levs = c(week_levs, paste('week_', i - 1, sep=''))
  }

  week_factor = factor(week_factor)
  levels(week_factor) = week_levs

  Long_Elos = current_elos %>% gather(week, elo, -c(Players))
  Long_Elos$week = week_factor

  ggplot(Long_Elos, aes(x=week, y=elo, group=Players, color=Players, symbols=Players)) + geom_line(linewidth = 2) + geom_point()

  ggsave(sprintf(output_png, as.character(length(levels(week_factor)) - 1)), device = 'png', width = 16, height = 9)
}

CURR_ELOS_FILE = "2025_season_elo.csv"
DYN_ELOS_FILE = "dynasty_elo.csv"

CURR_ELOS_PNG = "2025_season_week_%s.png"
DYN_ELOS_PNG = "dynasty_week_%s.png"

if (sys.nframe() == 0) {  # only when run directly, not when sourced
  args = commandArgs(trailingOnly = TRUE)
  if (length(args) >= 2) {
    build_elo_graph(args[1], args[2])
  } else {
    build_elo_graph(CURR_ELOS_FILE, CURR_ELOS_PNG)
    build_elo_graph(DYN_ELOS_FILE, DYN_ELOS_PNG)
  }
}
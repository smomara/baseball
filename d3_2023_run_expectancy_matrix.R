library(baseballr)
library(tidyverse)

df <- data.frame()

load_ncaa_baseball_teams() %>%
  filter(division == 3 & year == 2023) -> teams

for (i in 328:nrow(teams)){
  schedule <- ncaa_schedule_info(team_id = teams$team_id[i], year = 2023)
  if (nrow(schedule) == 0) {
    next
  }
  for (j in 1:nrow(schedule)) {
    progress <- (i-1) / nrow(teams) + 1 / nrow(teams) * j / nrow(schedule)
    print(paste(round(progress * 100, 2), "%"))
    if (is.na(schedule$game_info_url[j]) | schedule$contest_id[j] %in% df$contest_id) {
      next
    }
    game_pbp <- ncaa_pbp(schedule$game_info_url[j])
    game_pbp$contest_id <- schedule$contest_id[j]
    df <- bind_rows(df, game_pbp)
  }
}

write.csv(df, "pbp.csv", row.names = FALSE)

df <- df[complete.cases(df$game_pbp_id), ] %>%
  select(-contest_id)

parsed <- ncaa_parse(df)
matrix <- get_expected_runs_matrix(parsed$base_cd_before, parsed$outs_before, parsed$runs_roi)

write.csv(matrix, "matrix.csv")

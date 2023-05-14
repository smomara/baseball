library(baseballr)
library(tidyverse)

ncaa_fip_constant <- function(division, year) {
  if (is.null(division)) {
    cli::cli_abort("Enter valid division (1:3)")
  }
  if (is.null(year)) {
    cli::cli_abort("Enter valid years as a number (YYYY)")
  }
  ncaa_team_lookup <- baseballr::load_ncaa_baseball_teams() %>%
    filter(division == {{division}} & year == {{year}})
  
  df <- data.frame()
  for (i in 1:nrow(ncaa_team_lookup)) {
    stats <- suppressWarnings(ncaa_team_player_stats(team_id = ncaa_team_lookup$team_id[i], year = year, type = "pitching"))
    if (nrow(stats) > 0) {
      df <- bind_rows(df, stats %>%
                        filter(player_name == "Totals") %>%
                        select(team_name, ER, IP, `HR-A`, BB, HB, SO) %>%
                        mutate(`EXTRA-OUTS` = (IP %% 1) * 10,
                                      OUTS = floor(IP) * 3 + `EXTRA-OUTS`)) %>%
        select(-IP, -`EXTRA-OUTS`)
    }
  }
  
  dfa = janitor::adorn_totals(df, where = "row", name = as.character(division))[nrow(df) + 1, ] %>%
    rename("division" = "team_name") %>%
    mutate(lgERA = as.numeric(ER) / (as.numeric(OUTS) / 3) * 9,
           cFIP = lgERA - ((13 * `HR-A` + 3 * (BB + HB) - 2 * SO) / (OUTS / 3)))
  
  return(dfa$cFIP)
}

rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memory and report the memory usage.
nflreadr::.clear_cache()

library(tidyverse)
library(rvest)
library(janitor)
library(zoo)
library(DescTools)

setwd("~/NFL")

#HFA
HFA <- read.csv("Homefield/Automate/HFA.csv")

#Team Grade
games <- data.frame()
seasons <- 2021:2023
progressr::with_progress({
  
  games <- nflfastR::load_pbp(seasons) |>
    filter(qb_kneel != 1) |>
    group_by(game_id) |>
    mutate(#EPA
      epa_tho = sum(epa[home_team==posteam], na.rm = TRUE),
      epa_tao = sum(epa[away_team==posteam], na.rm = TRUE)) |>
    ungroup() |>
    select(season, week, game_date, season_type, game_id, home_team, away_team, epa_tho, epa_tao) |>
    distinct()
})

home <- games |>
  rename(team1 = home_team, team2 = away_team, epa_off = epa_tho, epa_def = epa_tao)

away <- games |>
  rename(team1 = away_team, team2 = home_team, epa_off = epa_tao, epa_def = epa_tho) 

epa <- home |>
  rbind(away) |>
  arrange(season, week) |>
  group_by(team1) |>
  mutate(#Rolling EPA numbers
    current_off = pracma::movavg(epa_off, n = 16, type = "e"),
    current_def = pracma::movavg(epa_def, n = 16, type = "e"),
    current_epa = current_off - current_def
  ) |>
  ungroup() |>
  mutate(team_grade = (current_epa - mean(current_epa))/sd(current_epa),
         game_date = as.Date(game_date)) |>
  # filter(week!=2) |>
  group_by(team1) |>
  slice(n() - 2, n() - 1, n()) |>  ungroup() |>
  filter(week!=17) |>
  mutate(season = ifelse(season==2022, 2023, 2023),
         week = ifelse(week==1, 2, 
                       ifelse(week==2, 3, 1)),
         team_grade = ifelse(week==1, team_grade/3, team_grade)) |>
  select(season, week, team = team1, team_grade)

#QB Data
elo_total <- readr::read_csv("https://raw.githubusercontent.com/greerreNFL/nfeloqb/main/qb_elos.csv") |>
  filter(season %in% c(2021:2023))

qb1 <- elo_total |>
  select(date, season, team1, team2, qb1_value_pre, qb2_value_pre) |>
  mutate(team1 = ifelse(team1=="LAR", "LA",
                        ifelse(team1=="OAK", "LV",
                               ifelse(team1=="WSH", 'WAS', team1))),
         team2 = ifelse(team2=="LAR", "LA",
                        ifelse(team2=="OAK", "LV",
                               ifelse(team2=="WSH", 'WAS', team2)))) 

qb2 <- qb1 |>
  rename(team1 = team2, team2 = team1, qb1_value_pre = qb2_value_pre, qb2_value_pre = qb1_value_pre)

qb0 <- qb1 |>
  rbind(qb2) |>
  mutate(qb_grade = (qb1_value_pre - mean(qb1_value_pre))/sd(qb1_value_pre)) |>
  filter(date > "2023-09-06") |>
  mutate(week = ifelse(date > "2023-09-20", 3, 
                       ifelse(date > "2023-09-13", 2, 1))) |>
  select(season, week, team = team1, qb_grade)

#Health Data
health21_22 <- read.csv("Homefield/Automate/health21_22.csv")

health23 <- read.csv("Homefield/Automate/current_SIC.csv") |>
  select(-c(drop))

health <- health21_22 |>
  rbind(health23) |>
  mutate(health_grade = (SIC - mean(SIC))/sd(SIC)) |>
  arrange(season, week) |>
  group_by(team) |>
  slice(n() - 2, n() - 1, n()) |>
  ungroup() |>
  select(season, week, team, health_grade)


szn23 <- epa |>
  left_join(qb0, by = c("season", "week", "team")) |>
  left_join(health, by = c("season", "week", "team")) |>
  inner_join(HFA, by = c("team")) |>
  mutate(rating = 4.1*team_grade + 1.04*qb_grade + 1.55*health_grade)

#Predictions
szn <- szn23 |>
  select(season, week, team, b_home, rating)
  
schedule <- nflreadr::load_schedules(seasons = 2023)
schedule0 <- schedule |>
  filter(week %in% c(1:3)) |>
  select(season, week, home_team, home_score, away_team, away_score, spread_line, home_spread_odds, away_spread_odds) |>
  left_join(szn, by = c("season", "week", "home_team" = "team")) |>
  rename(home_rating = rating, HFA = b_home) |>
  left_join(szn, by = c("season", "week", "away_team" = "team")) |>
  rename(away_rating = rating) |>
  select(-c(b_home)) |>
  arrange(season, week) |>
  mutate(pd = home_score - away_score,
         win_prob = 1/(1 + 10^(-spread_line/16)),
         home_update = ifelse(pd>0, 0.8 * (1 - win_prob), 
                              ifelse(pd==0, 0.8 * (0.5 - win_prob), 0.8 * (0 - win_prob))),
         away_update = ifelse(pd<0, 0.8 * (1 - win_prob), 
                              ifelse(pd==0, 0.8 * (0.5 - win_prob), 0.8 * (0 - win_prob))),
         home_mov = ifelse(pd>0, log(pd + 1)/25, 
                           ifelse(pd==0, 0, -1 * log((-1*pd) + 1)/25)),
         away_mov = ifelse(pd<0, log((-1 * pd) + 1)/25, 
                           ifelse(pd==0, 0, -1 * log(pd + 1)/25)),
         HNR = home_update + home_mov,
         HNR = ifelse(is.na(HNR), 0, HNR),
         ANR = away_update + away_mov,
         ANR = ifelse(is.na(ANR), 0, ANR)) |>
  # filter(week!=3) |>
  select(season, week, home_team, home_score, away_team, away_score, spread_line, home_rating, away_rating, 
         HFA, pd, home_update, away_update, home_mov, away_mov, HNR, ANR) |>
  distinct()

home0 <- schedule0 |>
  select(season, week, team = home_team, rating = home_rating, new_rating = HNR)

away0 <- schedule0 |>
  select(season, week, team = away_team, rating = away_rating, new_rating = ANR)

#projection for week 3
szn0 <- szn |>
  filter(week==3)

wk0 <- home0 |>
  rbind(away0) |>
  #change this for start time on updating, rating for week 2 reflects week one's performance
  mutate(new_rating = ifelse(week == 2, rating + new_rating, new_rating)) |>
  arrange(week) |>
  group_by(team) |>
  mutate(adj_rating = cumsum(new_rating)) |>
  ungroup() |>
  #projection for week 3, current week - 1
  filter(week == 2) |>
  select(team, adj_rating) |>
  left_join(szn0, by = c("team"))

schedule1 <- schedule |>
  #projection for week 3
  filter(week == 3) |>
  select(season, week, home_team, home_score, away_team, away_score, spread_line, home_spread_odds, away_spread_odds) |>
  left_join(wk0, by = c("home_team" = "team")) |>
  rename(home_rating = rating, home_rating2 = adj_rating, HFA = b_home) |>
  left_join(wk0, by = c("away_team" = "team")) |>
  select(-c(b_home)) |>
  rename(away_rating = rating, away_rating2 = adj_rating) |>
  distinct() |>
  select(season, week, home_team, home_score, away_team, away_score, spread_line, home_rating, away_rating, 
         home_rating2, away_rating2, HFA) |>
  mutate(win_prob = 1/(1 + 10^(-spread_line/16)),
         point_dif = home_score - away_score,
         predict = home_rating - away_rating + HFA,
         predict = plyr::round_any(predict, 0.1),
         predict2 = home_rating2 - away_rating2 + HFA,
         predict2 = plyr::round_any(predict2, 0.1),
         dif = predict - spread_line,
         dif2 = predict2 - spread_line,
         ER_outcome = ifelse(predict>spread_line & spread_line>0, "FAVORITE", 
                            ifelse(predict>spread_line & spread_line<0, "UNDERDOG", 
                                   ifelse(predict<spread_line & spread_line>0, "UNDERDOG", "FAVORITE"))),
         EU_outcome = ifelse(predict2>spread_line & spread_line>0, "FAVORITE", 
                             ifelse(predict2>spread_line & spread_line<0, "UNDERDOG", 
                                    ifelse(predict2<spread_line & spread_line>0, "UNDERDOG", "FAVORITE"))),
         spread_line = -1 * spread_line,
         predict = -1 * predict,
         predict2 = -1 * predict2) |>
  select(home_team, away_team, point_dif, spread_line, rating_dif = predict, home_rating, away_rating, ER_outcome,
         home_update = home_rating2, away_update = away_rating2, update_dif = predict2, EU_outcome)


#Week 2 Predictions
szn1 <- szn23 |>
  select(season, week, team, rating) |>
  arrange(-rating) |>
  group_by(week) |>
  mutate(rank = as.numeric(row_number())) |>
  ungroup() |>
  arrange(week) |>
  group_by(team) |>
  mutate(dif_rank = lag(rank) - rank) |>
  ungroup() |>
  filter(week==3) |>
  select(rank, dif_rank, team, rating)

nfl1 <- szn1 |>
  slice(1:16) |>
  mutate(ID = row_number())

nfl2 <- szn1 |>
  slice(17:32) |>
  rename(rank2 = rank, dif_rank2 = dif_rank, team2 = team, rating2 = rating) |>
  mutate(ID = row_number())

wk1 <- merge(nfl1, nfl2, by = c('ID'))

rm(nfl1, nfl2)

wk1 <- wk1 |>
  select(-c(ID))

library(gt)
library(gtExtras)
library(nflplotR)

wk1 |>
  gt::gt() |>
  tab_header(title = md("**NFL Team Ratings going into Week 3**"),
             subtitle = "Team, quarterback and health factors are considered") |>
  cols_label(
    rank = md(""),
    team = md("**Team**"),
    dif_rank = md("**Change**"),
    rating = md("**Overall**"),
    rank2 = md(""),
    team2 = md("**Team**"),
    dif_rank2 = md("**Change**"),
    rating2 = md("**Overall**")
  ) |>
  fmt_number(columns = c(rating, rating2), decimals = 2) |>
  tab_style(style = cell_text(weight = "bold"),locations = cells_body(columns = c(rank, team, dif_rank, rating, rank2, team2, dif_rank2, rating2))) |> 
  cols_align(align = "center", columns = c(rank, team, dif_rank, rating, rank2, team2, dif_rank2, rating2)) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"), 
            locations = cells_title(groups = "title")) |>
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size="small"),
            locations = list(cells_column_labels(everything()))) |>
  tab_style(style = cell_text(align = "center", size = "small"), locations = cells_body()) |>
  tab_style(style = cell_text(font = c(google_font(name = "Times"),
                                       default_fonts())), locations = cells_body(columns = everything())) |>
  nflplotR::gt_nfl_wordmarks(columns = gt::starts_with("team")) |>
  cols_width(c(team, team2) ~ px(175)) |>
  cols_width(c(rank, rank2) ~ px(45)) |>
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) |>
  tab_options(data_row.padding = px(0.5)) |>
  gt::data_color(columns = c(rating, rating2),
                 colors = scales::col_numeric(palette = viridis::viridis(10, direction = -1, option ="D"),
                                              domain = c(10, -10)), alpha = 0.8) |>
  gt_fa_rank_change(column = dif_rank, font_color = "match") |>
  gt_fa_rank_change(column = dif_rank2, font_color = "match") |>
  tab_source_note(source_note = md("**Data**: @SICscore & @FiveThirtyEight | **Table**: @PattonAnalytics")) |>
  tab_options(data_row.padding = px(0.5), source_notes.font.size = 10) |>
  gtsave(filename = "Homefield/wk3_rating.png")
  
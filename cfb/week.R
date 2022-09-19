aispoRts::ncaaf_fei_data
aispoRts::ncaaf_game_info
aispoRts::ncaaf_game_stats
aispoRts::ncaaf_opp_adj
aispoRts::ncaaf_preseason_stats

library(cfbfastR)
library(cfb4th)
library(cfbplotR)
library(tidymodels)
#devtools::install_github("Puntalytics/puntr")
library(puntr)
library(tidyverse)

seasons <- 2014:2022

# ncaaf_game_info ----------
load_cfb_games <- function(season) {
  # Helper function
  double_games <- function(g) {
    g1 <- g %>%
      select(sim, game_type, week, away_team, home_team, result) %>%
      rename(team = away_team, opp = home_team) %>%
      mutate(result = -1 * result)
    g2 <- g %>%
      select(sim, game_type, week, away_team, home_team, result) %>%
      rename(team = home_team, opp = away_team)
    g <- bind_rows(g1, g2) %>%
      mutate(outcome = case_when(
        result > 0 ~ 1,
        result < 0 ~ 0,
        result == 0 ~ 0.5,
        TRUE ~ NA_real_
      ))
    return(g)
  }

  # Betting Lines
  bet_df <-  cfbfastR::cfbd_betting_lines(year = season)

  # factor to keep consensus lines
  bet_df$provider <- factor(bet_df$provider,
                                 levels = c(
                                   "consensus",
                                   "teamrankings",
                                   "numberfire",
                                   "Caesars",
                                   "Caesars (Pennsylvania)",
                                   "William Hill (New Jersey)",
                                   "SugarHouse",
                                   "Bovada"
                                 )
  )
  bet_df$over_under <- as.numeric(bet_df$over_under)

  # get totals
  totals <- bet_df %>%
    arrange(game_id, -over_under) %>%
    select(game_id, over_under) %>%
    filter(!is.na(over_under)) %>%
    distinct(game_id, .keep_all = TRUE)
  # remove duplicates
  bet_df <- bet_df %>%
    select(-over_under) %>%
    arrange(game_id, provider) %>%
    distinct(game_id, .keep_all = TRUE) %>%
    left_join(totals, by = "game_id")

  # Game Info
  orig_df <- cfbfastR::cfbd_game_info(year = season) %>%
    mutate(game_type = case_when(season_type == "regular" ~ "REG",
                                 TRUE ~ "BOWL"),
           location = ifelse(neutral_site,"Neutral","Home"),
           result = home_points - away_points,
           start_date = (lubridate::as_datetime(start_date)-7*60*60)
           )
  rest_df <- orig_df %>%
    mutate(sim = game_id) %>%
    double_games() %>%
    left_join(orig_df %>% select(game_id,start_date,season),
              by = c("sim" = "game_id")) %>%
    mutate(start_date = as.Date(start_date)) %>%
    arrange(start_date) %>%
    group_by(season, team) %>%
    mutate(rest = as.numeric(start_date) - lag(as.numeric(start_date)),
           rest = ifelse(is.na(rest),7,rest)) %>%
    ungroup() %>%
    select(season,game_id = sim, team, rest)
  orig_df %>%
    as_tibble() %>%
    left_join(rest_df %>% rename(home_rest = rest),
              by = c("season","game_id","home_team" = "team")) %>%
    left_join(rest_df %>% rename(away_rest = rest),
              by = c("season","game_id","away_team" = "team")) %>%
    mutate(
      fcs_h = ifelse(home_team %in% cfbplotR::valid_team_names("FBS"),0,1),

      fcs_a = ifelse(away_team %in% cfbplotR::valid_team_names("FBS"),0,1)
    ) %>%
    # filter(fcs_h + fcs_a == 0)
    mutate(home_team = ifelse(fcs_h==1,"FCS",home_team),
           home_rest = ifelse(fcs_h==1, 7,home_rest),
           away_team = ifelse(fcs_a==1,"FCS",away_team),
           away_rest = ifelse(fcs_a==1, 7,away_rest)) %>%
    filter(!(away_team == "FCS" & home_team == "FCS")) |>
    left_join(bet_df |>
                select(-start_date,-home_team,-home_conference,-away_team,-away_conference),
              by = c("game_id","season","week","season_type"))
}

game_info_df <- map_df(seasons,load_cfb_games)

game_info_df_clean <- game_info_df |>
  transmute(
    season,
    date = lubridate::as_date(start_date),
    day = lubridate::wday(start_date,label = TRUE,abbr = TRUE),
    time = format(start_date, format = "%H:%M:%S"),
    week,
    home_team,
    away_team,
    spread = as.numeric(spread),
    over_under,
    game_id,
    season_type,
    neutral_site = as.numeric(neutral_site),
    conference_game = as.numeric(conference_game),
    venue_id,
    venue,
    home_id,
    home_conference,
    home_pregame_elo,
    home_postgame_elo,
    away_id,
    away_conference,
    away_pregame_elo,
    away_postgame_elo,
    highlights,
    notes,
    home_rest,
    away_rest,
    start_date,
    home_score,
    away_score,
    provider,
    formatted_spread,
    #spread_open,
    #over_under_open,
    #home_moneyline,
    #away_moneyline,
    spread_home_pos = -1*spread,
    result = home_score - away_score,
    home_bet = case_when(
      home_score+spread > away_score ~ "Win",
      home_score+spread < away_score ~ "Lose",
      TRUE ~ "Push"
    )

  )

saveRDS(game_info_df_clean,"cfb/data/ncaaf_game_info.RDS")

# ncaaf_game_stats

game_stats_df <- game_info_df_clean %>%
  select(
    season,
    week,
    season_type,
    home_team,
    away_team,
    game_id
  ) %>%
  pivot_longer(
    home_team:away_team,
    names_transform = function(x)str_remove(x,"_team"),
    names_to = "location",
    values_to = "team"
  )
game_stats_df <- game_stats_df %>%
  left_join(
    game_stats_df %>% transmute(game_id, season, week, season_type, opponent = team),
    by = c("season", "week", "season_type", "game_id")
  ) %>%
  filter(team != opponent)



# Deal with pbp data
pbp <- load_4th_pbp(seasons) %>%
  mutate(season = year)


##############################
##     Roll up Functions    ## --------------------------------------------------------------------------
##############################

roll_up_prep <- function(pbp) {
  pbp %>%
    mutate(
      abs_diff = abs(score_diff),
      garbage = case_when(
        period == 1 & abs_diff > 43 ~ 1,
        period == 2 & abs_diff > 37 ~ 1,
        period == 3 & abs_diff > 27 ~ 1,
        period == 4 & abs_diff > 22 ~ 1,
        TRUE ~ 0
      ),
      success = case_when(
        down == 1 & yards_gained > .5 * distance ~ 1,
        down == 2 & yards_gained > .7 * distance ~ 1,
        (down == 3 | down == 4) & yards_gained >= distance ~ 1,
        TRUE ~ 0
      ),
      success_pass = ifelse(pass == 1, success, NA),
      success_rush = ifelse(rush == 1, success, NA),
      EPA_pass = ifelse(pass == 1, EPA, NA),
      EPA_rush = ifelse(rush == 1, EPA, NA),
      tfl = ifelse(yards_gained < 0, 1, 0),
      fumb = fumble_vec,
      intercep = ifelse(str_detect(play_type, "Inte"), 1, 0),
      pbreak = ifelse(str_detect(play_text, "broke"), 1, 0),
      yards_gained = as.double(yards_gained),
      first_down = firstD_by_yards
    )
}

roll_up <- function(pbp) {
  pbp  %>%
    group_by(drive_id, .add = TRUE) %>%
    mutate(
      drive_td = ifelse(row_number() == 1, ifelse(drive_result == "TD", 1, 0), NA),
      drive = ifelse(row_number() == 1, 1, NA),
      available_yards = ifelse(row_number() == 1, first(drive_start_yards_to_goal), NA),
      final_yards = ifelse(row_number() == 1, first(drive_end_yards_to_goal), NA),
      drive_yards = available_yards - final_yards
    ) %>%
    ungroup(drive_id) %>%
    summarize(
      week = first(week),
      n = n(),
      EPA_tot = sum(EPA, na.rm = TRUE),
      EPApp = sum(EPA, na.rm = TRUE) / n(),
      EPA_rush_tot = sum(EPA_rush, na.rm = TRUE),
      EPA_pass_tot = sum(EPA_pass, na.rm = TRUE),
      EPApp_rush = mean(EPA_rush, na.rm = TRUE),
      EPApp_pass = mean(EPA_pass, na.rm = TRUE),
      explosiveness = mean(ifelse(success == 1, EPA, NA), na.rm = TRUE),
      success = sum(success),
      sr = success / n,
      success_pass = sum(success_pass, na.rm = TRUE),
      n_pass = sum(pass),
      sr_pass = success_pass / n_pass,
      success_rush = sum(success_rush, na.rm = TRUE),
      n_rush = sum(rush),
      sr_rush = success_rush / n_rush,
      db_havoc = (sum(intercep) + sum(pbreak)) / sum(pass),
      f7_havoc = (sum(tfl) + sum(fumb)) / sum(rush),
      havoc = (sum(intercep) + sum(pbreak) + sum(tfl) + sum(fumb)) /
        (sum(pass) + sum(rush)),
      havoc_tot = sum(intercep) + sum(pbreak) + sum(tfl) + sum(fumb),
      n_drive = n_distinct(drive_id),
      tds = sum(td_play, na.rm = TRUE),
      td_rate = tds / n_drive,
      to_rate = sum(turnover) / n_drive,
      first_downs = sum(first_down),
      first_down_rate = first_downs / n(),
      yards = sum(yards_gained),
      ypp = yards / n(),
      avg_distance = mean(distance, na.rm = TRUE),
      available_yards = sum(available_yards, na.rm = TRUE),
      drive_yards = sum(drive_yards, na.rm = TRUE),
      available_yard_perc = drive_yards / available_yards,
      penalty = sum(penalty_flag, na.rm = TRUE),
      # ALL PENALTYS, NEEDS TO BE FIXED
      penalty_rate = penalty / n,
      garbage_rate = first(garbage_perc),
      n_garbage = first(n_garbage),
      exp_EPA_tot = mean(if_else(EPA > 0, EPA, NA_real_), na.rm = TRUE)
      #game_id = first(game_id)
    )
  # select(week,n,n_pass,n_rush,
  #        # Per Play Rates
  #        EPA_tot,EPA_pass_tot,EPA_rush_tot,
  #        exp_EPA_tot,
  #        success,success_pass,success_rush,
  #        havoc_tot,yards,penalty,
  #        # Per Drive Rates
  #        n_drive, tds, first_downs,
  #        available_yards,drive_yards,
  #        # Need to get fancy to get garbage rate
  #        n_garbage)
}


## Rolling it on up
game_roll_up <- pbp %>%
  filter(kickoff_play != 1) %>%
  roll_up_prep() %>%
  group_by(game_id, offense_play, defense_play, season, week) %>%
  filter(rush + pass == 1) %>%
  mutate(garbage_perc = mean(garbage, na.rm = TRUE),
         n_garbage = sum(garbage, na.rm = TRUE)) %>%
  filter(garbage == 0) %>%
  roll_up() %>%
  rename("offense" = offense_play,
         "defense" = defense_play) %>%
  # pivot_longer(c("offense","defense"),
  #              values_to = "team",
  #              names_to = "posession") %>%
  select(game_id,season,offense,defense,#posession,
         season,week,everything())

##############################
##      Special Teams       ## --------------------------------------------------------------------------
##############################

# devtools::install_github("Puntalytics/puntr")
library(puntr)

# Use puntr package to calculate advanced punt stats
punt_df <- pbp %>%
  mutate(season = year) %>%
  filter(punt == 1) %>%
  puntr::college_to_pro(power_five = FALSE) %>%
  puntr::calculate_all()

# Summarize punt stats by game
#   SHARP_RERUN_tot will be averaged by n_punts and estimates punter strength
#   epa_punts will be averaged by n_punts and estimates overall punting unit strength
punt_game_summary <- punt_df %>%
  group_by(game_id, offense_play, defense_play) %>%
  summarize(
    n_punts = n(),
    SHARP_RERUN_tot = sum(SHARP_RERUN, na.rm = TRUE),
    epa_punts = sum(EPA, na.rm = TRUE)
  )


# FIELD GOALS

# fg_model <- readRDS(url("https://raw.githubusercontent.com/Kazink36/cfb_fourth_down/master/data/fg_model.RDS"))

fg_df <- pbp %>%
  filter(str_detect(play_type,"Field Goal")) %>%
  # Filter for field goal attempts
  #filter(!is.na(fg_make_prob),fg_make_prob != 0) %>%
  # Add a flag for made field goals
  transmute(game_id,offense_play,defense_play,fg_make_prob,fg_make = if_else(stringr::str_detect(play_type,"Good"),1,0)) %>%
  group_by(game_id, offense_play, defense_play) %>%
  summarize(n_fg_att = n(),
            n_fg_make = sum(fg_make),
            exp_fg_make = sum(fg_make_prob))

# Create a single special teams data frame to more easily join with final data
special_teams_df <- punt_game_summary %>%
  full_join(fg_df, by = c("game_id","offense_play","defense_play")) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  ungroup()


game_stats_df_combined <- game_stats_df %>%
  left_join(game_roll_up, by = c("game_id","team" = "offense","opponent" = "defense","season","week")) %>%
  left_join(game_roll_up, by = c("game_id","team" = "defense","opponent" = "offense","season","week"),
            suffix = c("_off", "_def")) %>%
  left_join(special_teams_df, by = c("game_id","team" = "offense_play","opponent" = "defense_play")) %>%
  left_join(special_teams_df, by = c("game_id","team" = "offense_play","opponent" = "defense_play"),
            suffix = c("_off", "_def"))

games_stats_df_pre <- game_stats_df_combined |>
  group_by(season,team) |>
  summarize(across(where(is.numeric),function(x){sum(x,na.rm = TRUE)/n()})) |>
  ungroup() |>
  mutate(week = 0,season = season + 1)

game_stats_df_final <- bind_rows(
    game_stats_df_combined,
    games_stats_df_pre
  ) |>
  arrange(
    season,week
  )

saveRDS(game_stats_df_final, "cfb/data/ncaaf_game_stats.RDS")


preseason_df <- game_stats_df %>%
  distinct(season,team)

preseason_rollup_base <- pbp %>%
  filter(kickoff_play != 1) %>%
  roll_up_prep() %>%
  group_by(season,offense_play) %>%
  filter(rush + pass == 1) %>%
  mutate(garbage_perc = mean(garbage, na.rm = TRUE),
         n_garbage = sum(garbage, na.rm = TRUE)) %>%
  filter(garbage == 0) %>%
  roll_up()

preseason_rollup <- preseason_rollup_base %>%
  ungroup() %>%
  transmute(season = season+1,
            offense_play, EPApp, EPApp_pass, EPApp_rush, explosiveness,
            sr, sr_rush, sr_pass, rush_rate = n_rush/(n_rush+n_pass),
            havoc, td_rate,to_rate,first_down_rate, ypp,
            available_yard_perc, penalty_rate, garbage_rate) %>%
  rename_with(function(x){glue::glue("last_season_{x}")},.cols = 3:18)

preseason_rollup_def_base <- pbp %>%
  filter(kickoff_play != 1) %>%
  roll_up_prep() %>%
  group_by(season,defense_play) %>%
  filter(rush + pass == 1) %>%
  mutate(garbage_perc = mean(garbage, na.rm = TRUE),
         n_garbage = sum(garbage, na.rm = TRUE)) %>%
  filter(garbage == 0) %>%
  roll_up()

preseason_rollup_def <- preseason_rollup_def_base %>%
  ungroup() %>%
  transmute(season = season+1,
            defense_play, EPApp, EPApp_pass, EPApp_rush, explosiveness,
            sr, sr_rush, sr_pass, rush_rate = n_rush/(n_rush+n_pass),
            havoc, td_rate,to_rate,first_down_rate, ypp,
            available_yard_perc, penalty_rate, garbage_rate) %>%
  rename_with(function(x){glue::glue("last_season_{x}")},.cols = 3:length(.))


##############################
##  Returning Talent(YoY)   ## --------------------------------------------------------------------------
##############################

#seasons <- 2014:2021 # Not updated for 2022 season yet

# Pulls returning usage data from cfbfastR
player_returning_usage <- purrr::map_df(seasons, function(x) {
  cfbfastR::cfbd_player_returning(x) %>%
    select(season,team,contains("usage"))
})

# Pulls 247 Talent Ratings (Not available for 2014)
team_talent <- purrr::map_df(seasons,function(x) {
  Sys.sleep(1)
  cfbfastR::cfbd_team_talent(x)
}
)

preseason_df_final <- preseason_df %>%
  left_join(preseason_rollup, by = c("season","team" = "offense_play")) %>%
  left_join(preseason_rollup_def, by = c("season","team" = "defense_play"), suffix = c("_offense","_defense")) %>%
  left_join(player_returning_usage, by = c("season","team")) %>%
  left_join(team_talent, by = c("season" = "year", "team" = "school"))

saveRDS(preseason_df_final,"cfb/data/ncaaf_preseason_stats.RDS")

# Opponent Adjustment ------

# Columns to be adjusted per drive
col_drive <- c("tds_off","first_downs_off")
# Columns to be adjusted per play
col_play <- c("EPA_tot_off","success_off",#"havoc_tot",
              "yards_off"#,"penalty",
              #"n_garbage_off"
)
# Columns to be adjusted per success (just explosiveness)
col_success <- c("exp_EPA_tot_off")
# Columns to be adjusted per run
col_rush <- c("EPA_rush_tot_off","success_rush_off")
# Columns to be adjusted per pass
col_pass <- c("EPA_pass_tot_off","success_pass_off")
# Columns to be adjusted per punt
col_punt <- c(#"SHARP_RERUN_tot",
  "epa_punts_off")
# Columns to be adjusted per fg
#col_fg <- c("n_fg_make","exp_fg_make")



data <- game_stats_df_final %>%
  filter(week != 0) |>
  select(season, week, season_type, game_id, team, opponent,location,
         all_of(col_drive),all_of(col_play),
         all_of(col_success),all_of(col_rush),
         all_of(col_pass),all_of(col_punt),n_drive_off
         ,n_off,success_off,n_rush_off,n_pass_off,n_punts_off) %>%
  mutate(
    across(all_of(col_drive),
           function(x){
             x/n_drive_off
           }
    ),
    across(all_of(col_play),
           function(x){
             x/n_off
           }
    ),
    across(all_of(col_success),
           function(x){
             x/success_off
           }
    ),
    across(all_of(col_rush),
           function(x){
             x/n_rush_off
           }
    ),
    across(all_of(col_pass),
           function(x){
             x/n_pass_off
           }
    ),
    across(all_of(col_punt),
           function(x){
             x/n_punts_off
           }
    )#,
    # across(all_of(col_fg),
    #        function(x){
    #          x/n_fg_att
    #        }
    # )
  ) %>%
  select(-c(n_drive_off,n_off,success_off,n_rush_off,n_pass_off,n_punts_off)) %>%
  filter(team %in% valid_team_names("FBS"),opponent %in% valid_team_names("FBS"), !is.na(tds_off))
df <- data %>%
  rename(offense = team,
         defense = opponent) %>%
  pivot_longer(tds_off:length(data)) %>%
  #select(-game_id) %>%
  mutate(value = ifelse(is.na(value),0,value),
         week = ifelse(season_type != "regular",max(week)+1, week))

df_nested <- df %>%
  group_by(season) %>%
  #filter(name == "EPA_tot_off") %>% #temp
  mutate(true_week = week,
         temp_week = max(week)+1-week,
         temp_week2 = temp_week) %>%
  select(-season_type) %>%
  uncount(temp_week2) %>%
  group_by(season,week,offense,name,game_id) %>%
  mutate(week = true_week + row_number()-1) %>%
  ungroup() %>%
  select(-true_week,-temp_week,-game_id) %>%
  nest(data = c(offense, defense, location, value))

test <- df_nested %>% slice(3) %>% pull(data) %>% .[[1]]

opp_adj_recipe <- recipe(value ~ ., data = test) %>%
  step_dummy(all_predictors(),one_hot = TRUE) %>%
  step_impute_knn(all_predictors())


#Penalty from tuning 2021 data
opp_adj_model <- linear_reg(penalty = 0.153, mixture = 0) %>%
  set_engine("glmnet")

opp_adj_workflow <- workflow() %>%
  add_recipe(opp_adj_recipe) %>%
  add_model(opp_adj_model)

opp_adj_function <- function(data) {
  opp_adj_workflow %>%
    parsnip::fit(data) %>%
    broom::tidy()
}

future::plan(future::multisession(workers = 4))
tictoc::tic()
new_stats_df <- df_nested %>%
  mutate(model = furrr::future_map(data,opp_adj_function,.options = furrr::furrr_options(globals = "opp_adj_workflow",packages = c("tidymodels"),seed = TRUE)))
tictoc::toc()

opp_adj <- new_stats_df %>% select(-data) %>%
  unnest(model) %>%
  group_by(season,week,name) %>%
  mutate(estimate = case_when(row_number()> 1 ~ estimate + first(estimate),
                              TRUE ~ estimate)) %>%
  slice(2:n()) %>%
  filter(!term %in% c("location_home","location_away")) %>%
  mutate(team = str_remove(term,"offense_|defense_"),
         team = str_replace_all(team,"\\."," "),
         posession = ifelse(str_detect(term,"offense_"),"offense","defense"),
         team = case_when(team == "Miami  OH " ~ "Miami (OH)",
                          team == "Texas A M" ~ "Texas A&M",
                          team == "Hawai i" ~ "Hawai'i",
                          TRUE ~ team)) %>%
  select(-term,-penalty) %>%
  pivot_wider(names_from = posession,values_from = estimate) %>%
  pivot_wider(names_from = name, values_from = offense:defense) %>%
  rename_with(function(x)str_remove(x,"_off") %>% str_remove("_tot")) %>%
  ungroup()

opp_adj_pre <- opp_adj |>
  group_by(season,team) |>
  slice(n()) |>
  ungroup() |>
  mutate(week = 0,season = season+1)

opp_adj_final <-
  bind_rows(
    opp_adj,
    opp_adj_pre
  ) |>
  arrange(season,week)
saveRDS(opp_adj,"cfb/data/ncaaf_opp_adj.RDS")


# FEI
library(rvest)

fei_game_scraper <- function(season){

  cfbfastR_game_info <- cfbfastR::cfbd_game_info(year = season,season_type = "both")

  url <- glue::glue("https://www.bcftoys.com/{season}-gr")
  raw <- rvest::read_html(url)

  table <- raw %>% html_table() %>% .[[1]]

  names(table) <-  c("week","opponent","result","final_score",
                     "non_garbage_score","DROP_1","game_rating","game_rating_percentile","game_rating_rank",
                     "DROP_2","offense_rating","offense_rating_percentile","offense_rating_rank",
                     "DROP_3","defense_rating","defense_rating_percentile","defense_rating_rank",
                     "DROP_4","special_teams_rating","special_teams_rating_percentile","special_teams_rating_rank")
  clean_table <- table %>%
    select(-starts_with("DROP")) %>%
    filter(!is.na(result)) %>%
    mutate(title_row = case_when(str_detect(opponent,"Game Rating") ~ 1,
                                 TRUE ~ 0)) %>%
    mutate(team_index = cumsum(title_row)) %>%
    #filter(title_row == 0) %>%
    group_by(team_index) %>%
    mutate(team = str_extract(first(week),"^.*(?= 2)"),
           season = season) %>%
    filter(title_row == 0, opponent != "Opponent") %>%
    ungroup() %>%
    select(-team_index,-title_row) %>%
    mutate(
      team = cfbplotR::clean_school_names(team),
      opponent = cfbplotR::clean_school_names(opponent),
      team = case_when(team == "UL Monroe" ~ "Louisiana Monroe",
                       team == "UTSA" ~ "UT San Antonio",
                       TRUE ~ team
      ),
      opponent = case_when(opponent == "UL Monroe" ~ "Louisiana Monroe",
                           opponent == "UTSA" ~ "UT San Antonio",
                           TRUE ~ opponent
      ),
      week = case_when(week == "C" & season >= 2009 ~ "14",
                       week == "C" ~ as.character(max(cfbfastR_game_info$week)),
                       week == "P" ~ "B",
                       week == "11" & season == 2020 & (team %in% c("California","UCLA")) ~ "12",
                       week == "0" ~ "1",
                       TRUE ~ week)) %>%
    select(season,team,everything())

  for_return <- cfbfastR_game_info %>% #filter(season_type != "regular") %>% select(home_team,away_team)
    mutate(result = home_points-away_points,sim = game_id,week= as.character(week),
           week = case_when(season_type == "postseason" ~ "B",
                            TRUE ~ week)) %>%
    nflseedR:::double_games() %>%
    rename(game_id = sim) %>%
    select(game_id,week,team,opp) %>%
    right_join(clean_table, by = c("week","team","opp" = "opponent"))



  return(for_return)

}

fei_split_scraper <- function(season){

  cfbfastR_game_info <- cfbfastR::cfbd_game_info(year = season,season_type = "both")

  url <- glue::glue("https://www.bcftoys.com/{season}-gs")
  raw <- rvest::read_html(url)

  table <- raw %>% html_table() %>% .[[1]]

  names(table) <-  c("week","opponent","result","final_score","non_garbage_final_score","DROP_0","offensive_drive_efficiency",
                     "defenseive_drive_efficiency","net_drive_efficiency","DROP_1","offense_points_per_drive",
                     "defense_points_per_drive","net_points_per_drive","DROP_2","offensive_available_yards_percentage",
                     "defensive_available_yards_percentage","net_available_yards_percentage","DROP_3",
                     "offensive_yards_per_play","defensive_yards_per_play","net_yards_per_play")
  clean_table <- table %>%
    select(-starts_with("DROP")) %>%
    filter(!is.na(result)) %>%
    mutate(title_row = case_when((result %in% c("R","L","W")) ~ 0,
                                 TRUE ~ 1)) %>%
    mutate(team_index = cumsum(title_row)) %>%
    #filter(title_row == 0) %>%
    group_by(team_index) %>%
    mutate(team = str_extract(first(week),"^.*(?= 2)"),
           season = season) %>%
    filter(title_row == 0, opponent != "Opponent") %>%
    ungroup() %>%
    select(-team_index,-title_row) %>%
    mutate(
      team = cfbplotR::clean_school_names(team),
      opponent = cfbplotR::clean_school_names(opponent),
      team = case_when(team == "UL Monroe" ~ "Louisiana Monroe",
                       team == "UTSA" ~ "UT San Antonio",
                       TRUE ~ team
      ),
      opponent = case_when(opponent == "UL Monroe" ~ "Louisiana Monroe",
                           opponent == "UTSA" ~ "UT San Antonio",
                           TRUE ~ opponent
      ),
      week = case_when(week == "C" & season >= 2009 ~ "14",
                       week == "C" ~ as.character(max(cfbfastR_game_info$week)),
                       week == "P" ~ "B",
                       week == "11" & season == 2020 & (team %in% c("California","UCLA")) ~ "12",
                       week == "0" ~ "1",
                       TRUE ~ week)) %>%
    select(season,team,everything())

  for_return <- cfbfastR_game_info %>% #filter(season_type != "regular") %>% select(home_team,away_team)
    mutate(result = home_points-away_points,sim = game_id,week= as.character(week),
           week = case_when(season_type == "postseason" ~ "B",
                            TRUE ~ week)) %>%
    nflseedR:::double_games() %>%
    rename(game_id = sim) %>%
    select(week,team,opp) %>% #dropped game_id
    right_join(clean_table, by = c("week","team","opp" = "opponent"))



  return(for_return)

}

fei_data <- map_df(2022,
                   function(x){
                     cli::cli_alert_info("scraping season {x}")
                     temp <- fei_game_scraper(x)
                     Sys.sleep(5)
                     temp2 <- fei_split_scraper(x)
                     Sys.sleep(5)
                     for_return <- left_join(temp,temp2,by = c("season","team","week","opp","result","final_score"))
                     return(for_return)
                   })

fei_data_clean <- fei_data |>
  mutate(week = parse_number(week)) |>
  mutate(across(game_rating:net_points_per_drive,parse_number))

fei_data_final <- ncaaf_fei_data |> # Already has pre 2022
  bind_rows(fei_data_clean) |>
  mutate(across(offensive_available_yards_percentage:net_yards_per_play,parse_number))



saveRDS(fei_data_final, "cfb/data/ncaaf_fei_data.RDS")

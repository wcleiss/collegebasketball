#This file contains the monte carlo simulations for a college basektball game.
#It simulates in two ways - one by using individual statistics, and one uses each team's statistics as a whole
#The indivudal mode is more complex as it calculates a different floor lineup for both teams each possession based on probability of each player being on the floor.

library(rvest)
library(readxl)
library(openxlsx)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggalt)
library(ggplot2)
library(scales)
library(formattable)
library(reactable)
library(htmltools)

#This Function Gathers the Basic Monte Carlo Data Needed

mc_tableset <- function(xxxx) {

  mc_away_roster <<- ind_all_adj_frame %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_roster <<- ind_all_adj_frame %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  mc_games_thresh <<- .3
  mc_mins_thresh <<- .1
  
  nat_avg_mx <<- as.matrix(nat_avgs)
  
  nat_2p3p <<- nat_avg_mx[4] / nat_avg_mx[6]
  nat_2pratio <<- nat_avg_mx[4] / (nat_avg_mx[4] + nat_avg_mx[6])
  nat_3pratio <<- nat_avg_mx[6] / (nat_avg_mx[4] + nat_avg_mx[6])
  advantage_mx <<- as.matrix(advantage_frame[1:3, 2:21])
  away_d2ratio <<- mc_away_teamstats[18] / (mc_away_teamstats[18] + mc_away_teamstats[20])
  home_d2ratio <<- mc_home_teamstats[18] / (mc_home_teamstats[18] + mc_home_teamstats[20])
  away_d3ratio <<- 1 - away_d2ratio
  home_d3ratio <<- 1 - home_d2ratio
  
  home_d2_vsna <<- home_d2ratio / nat_2pratio
  away_d2_vsna <<- away_d2ratio / nat_2pratio
  home_d3_vsna <<- home_d3ratio / nat_3pratio
  away_d3_vsna <<- away_d3ratio / nat_3pratio
  
  home_dto_pct <<- mc_home_teamstats[29]
  away_dto_pct <<- mc_away_teamstats[29]
  
  home_d2p3p_vsna <<- (mc_home_teamstats[18] / mc_home_teamstats[20]) / nat_2p3p
  away_d2p3p_vsna <<- (mc_away_teamstats[18] / mc_away_teamstats[20]) / nat_2p3p
  
  away_dast_vsna <<- mc_away_teamstats[26] / nat_avg_mx[13]
  away_dblk_vsna <<- mc_away_teamstats[28] / nat_avg_mx[15]
  home_dast_vsna <<- mc_home_teamstats[26] / nat_avg_mx[13]
  home_dblk_vsna <<- mc_home_teamstats[28] / nat_avg_mx[15]
  
  away_season_tempo <<- mc_away_teamstats[35]
  home_season_tempo <<- mc_home_teamstats[35]
  nat_tempo <<- nat_avg_mx[1]
  
  away_tempo_vsna <<- away_season_tempo - nat_tempo
  home_tempo_vsna <<- home_season_tempo - nat_tempo
  vsna_sums <<- away_tempo_vsna + home_tempo_vsna
  
  exp_tempo <<- (nat_tempo + vsna_sums) * xxxx
  ft_flank <<- (1/.475) - 2
  tempo_integer <<- as.integer(exp_tempo)
  tempo_flank <<- exp_tempo - tempo_integer
  
  nat_avg_orb <<- nat_avg_mx[10]
  nat_avg_drb <<- nat_avg_mx[11]
  nat_avg_ast <<- nat_avg_mx[13]
  nat_avg_stl <<- nat_avg_mx[14]
  nat_avg_blk <<- nat_avg_mx[15]
  nat_avg_tov <<- nat_avg_mx[16]
  nat_avg_pfl <<- nat_avg_mx[17]
  nat_avg_sft <<- nat_avg_mx[19]
  
  away_adv_tov <<- advantage_mx[2, 17] 
  away_adv_pfl <<- advantage_mx[2, 18]
  away_adv_orb <<- advantage_mx[2, 11]
  away_adv_drb <<- advantage_mx[2, 12] 
  away_adv_ast <<- advantage_mx[2, 14] 
  away_adv_blk <<- advantage_mx[2, 16] 
  away_adv_stl <<- advantage_mx[2, 15] 
  away_adv_sft <<- advantage_mx[2, 20] 
  
  home_adv_tov <<- advantage_mx[1, 17] 
  home_adv_pfl <<- advantage_mx[1, 18]
  home_adv_orb <<- advantage_mx[1, 11]
  home_adv_drb <<- advantage_mx[1, 12] 
  home_adv_ast <<- advantage_mx[1, 14] 
  home_adv_blk <<- advantage_mx[1, 16] 
  home_adv_stl <<- advantage_mx[1, 15] 
  home_adv_sft <<- advantage_mx[1, 20] 
  
  neut_adv_tov <<- advantage_mx[3, 17] 
  neut_adv_pfl <<- advantage_mx[3, 18]
  neut_adv_orb <<- advantage_mx[3, 11]
  neut_adv_drb <<- advantage_mx[3, 12] 
  neut_adv_ast <<- advantage_mx[3, 14] 
  neut_adv_blk <<- advantage_mx[3, 16] 
  neut_adv_stl <<- advantage_mx[3, 15] 
  neut_adv_sft <<- advantage_mx[3, 20] 
  
}

#Determine Which Players will Compose Roster and Generate State Frames

mc_injury <- function(xxxx) {
  
  name_fix <<- as.matrix(lastgame_rosters[, 19])
  name_fix2 <<- sapply(name_fix, name_switch)
  lastgame_rosters2 <<- lastgame_rosters
  lastgame_rosters2[, 19] <<- name_fix2
  #lastgame_rosters2[, 19] <<- trim.all(lastgame_rosters2[, 19])
  
  injury_rep <<- read_csv("college-basketball-injury-report.csv")
  injury_join <<- lastgame_rosters2 %>%
    inner_join(injury_rep, by = c("PLAYER_NAME" = "Player")) %>%
    select(PLAYER_NAME, PLAYER_ID, TEAM_NAME, TEAM_ID, Status, Mins) %>%
    filter(Status == "Out" | Status == "Out For Season")
  
}

mc_exclude <- function(xxxx) {
  
  away_exclude <<- mc_incexc %>%
    filter(TEAM_ID == mc_away_id) %>%
    filter(EXC == 1)
  
  if (nrow(away_exclude) > 0) {
    
     a <- 1
     g <- nrow(away_exclude)
     
     for (a in a:g) {
      
       exc_id <<- as.numeric(away_exclude[a, 2])
       exc_place <<- which(mc_away_lineup$PLAYER_ID == exc_id)
       if (length(exc_place) > 0) {
         print(paste("EXCLUDING:", exc_id, mc_away_id, sep = " "))
         exc_place <<- -1 * exc_place
         mc_away_lineup <<- mc_away_lineup[exc_place, ]
         print(mc_away_lineup)
       }
    }
  }
  
  home_exclude <<- mc_incexc %>%
    filter(TEAM_ID == mc_home_id) %>%
    filter(EXC == 1)
  
  if (nrow(home_exclude) > 0) {
    
    a <- 1
    g <- nrow(home_exclude)
    
    for (a in a:g) {
      
      exc_id <<- as.numeric(home_exclude[a, 2])
      exc_place <<- which(mc_home_lineup$PLAYER_ID == exc_id)
      if (length(exc_place) > 0) {
        print(paste("EXCLUDING:", exc_id, mc_home_id, sep = " "))
        exc_place <<- -1 * exc_place
        mc_home_lineup <<- mc_home_lineup[exc_place, ]
        print(mc_home_lineup)
      }
    }
  }
}

mc_roster_select <- function(xxxx) {
  
  mc_away_lineup <<- lastgame_rosters %>%
    filter(TEAM_ID == mc_away_id) %>%
    select(PLAYER_ID) %>%
    inner_join(mc_away_roster, by = c("PLAYER_ID"))
  
  mc_away_lineup <<- mc_away_lineup[, c(2, 1, 3:31)]
  
  mc_home_lineup <<- lastgame_rosters %>%
    filter(TEAM_ID == mc_home_id) %>%
    select(PLAYER_ID) %>%
    inner_join(mc_home_roster, by = c("PLAYER_ID"))
  
  mc_home_lineup <<- mc_home_lineup[, c(2, 1, 3:31)]
  
  mc_away_minslast <<- lastgame_rosters %>%
    filter(TEAM_ID == mc_away_id) %>%
    select(PLAYER_ID, Mins)
  
  a <- 1
  g <- nrow(mc_away_lineup)
  
  for (a in a:g) {
    
    p_id <- as.numeric(mc_away_lineup[a, 2])
    
    b <- 1
    h <- nrow(mc_away_minslast)
    
    for (b in b:h) {
      
      m_id <- as.numeric(mc_away_minslast[b, 1])
      
      if (m_id == p_id) { 
        
        mins_use <- as.numeric(mc_away_minslast[b, 2])
        mc_away_lineup[a, 6] <<- mins_use
          
      }
    }
  }
  
  
  mc_home_minslast <<- lastgame_rosters %>%
    filter(TEAM_ID == mc_home_id) %>%
    select(PLAYER_ID, Mins)
  
  a <- 1
  g <- nrow(mc_home_lineup)
  
  for (a in a:g) {
    
    p_id <- as.numeric(mc_home_lineup[a, 2])
    
    b <- 1
    h <- nrow(mc_home_minslast)
    
    for (b in b:h) {
      
      m_id <- as.numeric(mc_home_minslast[b, 1])
      
      if (m_id == p_id) { 
        
        mins_use <- as.numeric(mc_home_minslast[b, 2])
        mc_home_lineup[a, 6] <<- mins_use
        
      }
    }
  }
  
  mc_exclude(1)
  
  mc_away_stats <<- as.matrix(mc_away_lineup[, c(2, 6:31)])
  mc_home_stats <<- as.matrix(mc_home_lineup[, c(2, 6:31)])
  
  mc_away_min_sums <<- sum(mc_away_lineup$pMIN)
  mc_home_min_sums <<- sum(mc_home_lineup$pMIN)
  
  mc_away_share_frame <<- mc_away_lineup$pMIN / mc_away_min_sums
  mc_home_share_frame <<- mc_home_lineup$pMIN / mc_home_min_sums
  
  mc_away_size <<- length(mc_away_share_frame)
  mc_home_size <<- length(mc_home_share_frame)
  
  #
  
  mc_away_share_round <<- round(mc_away_share_frame * 10000, 0)
  mc_home_share_round <<- round(mc_home_share_frame * 10000, 0)
  
  mc_away_shake <<- matrix(0, nrow = 10000, ncol = 1)
  mc_home_shake <<- matrix(0, nrow = 10000, ncol = 1)
  
  a <- 1
  g <- length(mc_away_share_round)
  ct <- 0
  beg <<- 1
  while (a <= g) {
    shake_val <<- mc_away_share_round[a]
    ct <- shake_val + ct
    if (ct > 10000) { shake_val <<- shake_val - (ct - 10000) }
    shake_id <<- as.numeric(mc_away_lineup$PLAYER_ID[a])
    mc_away_shake[beg:ct] <<- a
    beg <<- beg + shake_val
    a <- a + 1
  }
  
  a <- 1
  g <- length(mc_home_share_round)
  ct <- 0
  beg <<- 1
  while (a <= g) {
    shake_val <<- mc_home_share_round[a]
    ct <- shake_val + ct
    if (ct > 10000) { shake_val <<- shake_val - (ct - 10000) }
    shake_id <<- as.numeric(mc_home_lineup$PLAYER_ID[a])
    mc_home_shake[beg:ct] <<- a
    beg <<- beg + shake_val
    a <- a + 1
  }
}

mc_shoot_pcts <- function(xxxx) {
  
  #Shooting Percentages
  
  away_shootpct_frame <<- mc_away_stats[, c(7, 9, 10)]
  away_shootpct_exp <<- away_shootpct_frame[, 1:3]
  away_shootpct_han <<- away_shootpct_frame[, 1:3]
  home_2pm_vsna <<- mc_home_teamstats[19] / nat_avg_mx[5]
  home_3pm_vsna <<- mc_home_teamstats[21] / nat_avg_mx[7]
  away_shootpct_exp[, 1] <<- (away_shootpct_frame[, 1] * home_2pm_vsna)
  away_shootpct_exp[, 2] <<- (away_shootpct_frame[, 2] * home_3pm_vsna)
  
  home_shootpct_frame <<- mc_home_stats[, c(7, 9, 10)]
  home_shootpct_exp <<- home_shootpct_frame[, 1:3]
  home_shootpct_han <<- home_shootpct_frame[, 1:3]
  away_2pm_vsna <<- mc_away_teamstats[19] / nat_avg_mx[5]
  away_3pm_vsna <<- mc_away_teamstats[21] / nat_avg_mx[7]
  home_shootpct_exp[, 1] <<- (home_shootpct_frame[, 1] * away_2pm_vsna)
  home_shootpct_exp[, 2] <<- (home_shootpct_frame[, 2] * away_3pm_vsna)
  
  if (mc_loc == "A" | mc_loc == "H") {
    
    away_shootpct_han[, 1] <<- away_shootpct_exp[, 1] * advantage_mx[2, 6]
    away_shootpct_han[, 2] <<- away_shootpct_exp[, 2] * advantage_mx[2, 8]
    away_shootpct_han[, 3] <<- away_shootpct_exp[, 3] * advantage_mx[2, 9]
    
    home_shootpct_han[, 1] <<- home_shootpct_exp[, 1] * advantage_mx[1, 6]
    home_shootpct_han[, 2] <<- home_shootpct_exp[, 2] * advantage_mx[1, 8]
    home_shootpct_han[, 3] <<- home_shootpct_exp[, 3] * advantage_mx[1, 9]
    
  }
  
  else {
    
    away_shootpct_han[, 1] <<- away_shootpct_exp[, 1] * advantage_mx[3, 6]
    away_shootpct_han[, 2] <<- away_shootpct_exp[, 2] * advantage_mx[3, 8]
    away_shootpct_han[, 3] <<- away_shootpct_exp[, 3] * advantage_mx[3, 9]
    
    home_shootpct_han[, 1] <<- home_shootpct_exp[, 1] * advantage_mx[3, 6]
    home_shootpct_han[, 2] <<- home_shootpct_exp[, 2] * advantage_mx[3, 8]
    home_shootpct_han[, 3] <<- home_shootpct_exp[, 3] * advantage_mx[3, 9]
    
  }
}

#Sample to Determine 5 Floor Players

shake_time <- function(x) {
  
  a <- 1
  g <- x
  for (a in a:g) {
    mc_floor_shake_2(1) 
  }
}

mc_floor_shake_2 <- function(xxxx) {
  
  away_floor_vector <<- sample(c(1:mc_away_size), size = 5, prob = mc_away_share_frame, replace = FALSE)
  home_floor_vector <<- sample(c(1:mc_home_size), size = 5, prob = mc_home_share_frame, replace = FALSE)

  away_floor_stats <<- mc_away_stats[away_floor_vector, ]
  home_floor_stats <<- mc_home_stats[home_floor_vector, ]
  
  away_sums_tov <<- sum(away_floor_stats[, 18])
  away_sums_stl <<- sum(away_floor_stats[, 16])
  away_sums_pfl <<- sum(away_floor_stats[, 19])
  away_sums_orb <<- sum(away_floor_stats[, 12])
  away_sums_drb <<- sum(away_floor_stats[, 13])
  away_sums_ast <<- sum(away_floor_stats[, 15])
  away_sums_blk <<- sum(away_floor_stats[, 17])
  away_sums_sft <<- sum(away_floor_stats[, 27])
  away_sums_sr2 <<- sum(away_floor_stats[, 6])
  away_sums_sr3 <<- sum(away_floor_stats[, 8])
  
  home_sums_tov <<- sum(home_floor_stats[, 18])
  home_sums_stl <<- sum(home_floor_stats[, 16])
  home_sums_pfl <<- sum(home_floor_stats[, 19])
  home_sums_orb <<- sum(home_floor_stats[, 12])
  home_sums_drb <<- sum(home_floor_stats[, 13])
  home_sums_ast <<- sum(home_floor_stats[, 15])
  home_sums_blk <<- sum(home_floor_stats[, 17])
  home_sums_sft <<- sum(home_floor_stats[, 27])
  home_sums_sr2 <<- sum(home_floor_stats[, 6])
  home_sums_sr3 <<- sum(home_floor_stats[, 8])
  
  away_sr2_prob <<- away_floor_stats[, 6] / away_sums_sr2
  away_sr3_prob <<- away_floor_stats[, 8] / away_sums_sr3
  away_orb_prob <<- away_floor_stats[, 12] / away_sums_orb
  away_drb_prob <<- away_floor_stats[, 13] / away_sums_drb
  away_ast_prob <<- away_floor_stats[, 15] / away_sums_ast
  away_stl_prob <<- away_floor_stats[, 16] / away_sums_stl
  away_blk_prob <<- away_floor_stats[, 17] / away_sums_blk
  away_tov_prob <<- away_floor_stats[, 18] / away_sums_tov
  away_pfl_prob <<- away_floor_stats[, 19] / away_sums_pfl
  away_sft_prob <<- away_floor_stats[, 27] / away_sums_sft
  
  home_sr2_prob <<- home_floor_stats[, 6] / home_sums_sr2
  home_sr3_prob <<- home_floor_stats[, 8] / home_sums_sr3
  home_orb_prob <<- home_floor_stats[, 12] / home_sums_orb
  home_drb_prob <<- home_floor_stats[, 13] / home_sums_drb
  home_ast_prob <<- home_floor_stats[, 15] / home_sums_ast
  home_stl_prob <<- home_floor_stats[, 16] / home_sums_stl
  home_blk_prob <<- home_floor_stats[, 17] / home_sums_blk
  home_tov_prob <<- home_floor_stats[, 18] / home_sums_tov
  home_pfl_prob <<- home_floor_stats[, 19] / home_sums_pfl
  home_sft_prob <<- home_floor_stats[, 27] / home_sums_sft
  
  if (away_sums_sr2 == 0) { away_sr2_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_sr3 == 0) { 
    away_sr3_prob <<- c(.2, .2, .2, .2, .2) 
    away_sums_sr3 <<- 0.01
  }
  if (away_sums_orb == 0) { away_orb_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_drb == 0) { away_drb_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_ast == 0) { away_ast_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_stl == 0) { away_stl_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_blk == 0) { away_blk_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_tov == 0) { away_tov_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_pfl == 0) { away_pfl_prob <<- c(.2, .2, .2, .2, .2) }
  if (away_sums_sft == 0) { away_sft_prob <<- c(.2, .2, .2, .2, .2) }
  
  if (home_sums_sr2 == 0) { home_sr2_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_sr3 == 0) { 
    home_sr3_prob <<- c(.2, .2, .2, .2, .2) 
    home_sums_sr3 <<- 0.01
  }
  if (home_sums_orb == 0) { home_orb_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_drb == 0) { home_drb_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_ast == 0) { home_ast_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_stl == 0) { home_stl_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_blk == 0) { home_blk_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_tov == 0) { home_tov_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_pfl == 0) { home_pfl_prob <<- c(.2, .2, .2, .2, .2) }
  if (home_sums_sft == 0) { home_sft_prob <<- c(.2, .2, .2, .2, .2) }
  
}

mc_exp_generate <- function(xxxx) {
  
  #Per Possession Stats
  
  away_tov_vsna <<- away_sums_tov / nat_avg_tov
  away_stl_vsna <<- away_dto_pct / nat_avg_tov
  away_pfl_vsna <<- away_sums_pfl / nat_avg_pfl
  away_orb_vsna <<- away_sums_orb / nat_avg_orb
  away_drb_vsna <<- away_sums_drb / nat_avg_drb
  away_ast_vsna <<- away_sums_ast / nat_avg_ast
  away_blk_vsna <<- away_sums_blk / nat_avg_blk
  away_sft_vsna <<- away_sums_sft / nat_avg_sft
  away_oorb_vsna <<- (1 - away_sums_drb) / nat_avg_orb
  away_odrb_vsna <<- (1 - away_sums_orb) / nat_avg_drb
  away_2pratio <<- away_sums_sr2 / (away_sums_sr2 + away_sums_sr3)
  away_3pratio <<- 1 - away_2pratio
  
  
  home_tov_vsna <<- home_sums_tov / nat_avg_tov
  home_stl_vsna <<- home_dto_pct / nat_avg_tov
  home_pfl_vsna <<- home_sums_pfl / nat_avg_pfl
  home_orb_vsna <<- home_sums_orb / nat_avg_orb
  home_drb_vsna <<- home_sums_drb / nat_avg_drb
  home_ast_vsna <<- home_sums_ast / nat_avg_ast
  home_blk_vsna <<- home_sums_blk / nat_avg_blk
  home_sft_vsna <<- home_sums_sft / nat_avg_sft
  home_oorb_vsna <<- (1 - home_sums_drb) / nat_avg_orb
  home_odrb_vsna <<- (1 - home_sums_orb) / nat_avg_drb
  home_2pratio <<- home_sums_sr2 / (home_sums_sr2 + home_sums_sr3)
  home_3pratio <<- 1 - home_2pratio
  
  if (away_tov_vsna == 0) { away_tov_vsna <<- .2 }
  if (away_pfl_vsna == 0) { away_pfl_vsna <<- .2 }
  if (away_orb_vsna == 0) { away_orb_vsna <<- .2 }
  if (away_drb_vsna == 0) { away_drb_vsna <<- .2 }
  if (away_ast_vsna == 0) { away_ast_vsna <<- .2 }
  if (away_blk_vsna == 0) { away_blk_vsna <<- .2 }
  if (away_sft_vsna == 0) { away_sft_vsna <<- .2 }
  
  if (home_tov_vsna == 0) { home_tov_vsna <<- .2 }
  if (home_pfl_vsna == 0) { home_pfl_vsna <<- .2 }
  if (home_orb_vsna == 0) { home_orb_vsna <<- .2 }
  if (home_drb_vsna == 0) { home_drb_vsna <<- .2 }
  if (home_ast_vsna == 0) { home_ast_vsna <<- .2 }
  if (home_blk_vsna == 0) { home_blk_vsna <<- .2 }
  if (home_sft_vsna == 0) { home_sft_vsna <<- .2 }

  
  away_exp_sr2 <<- (away_2pratio + home_d2ratio) / 2
  away_exp_sr3 <<- (away_3pratio + home_d3ratio) / 2
  home_exp_sr2 <<- (home_2pratio + away_d2ratio) / 2
  home_exp_sr3 <<- (home_3pratio + away_d3ratio) / 2
  
  away_exp_tov <<- (away_sums_tov * home_stl_vsna)
  away_exp_pfl <<- (away_sums_pfl * home_sft_vsna)
  away_exp_orb <<- (away_sums_orb / home_drb_vsna)
  away_exp_drb <<- (away_sums_drb / home_orb_vsna)
  away_exp_ast <<- (away_sums_ast * home_dast_vsna)
  away_exp_blk <<- (away_sums_blk * home_dblk_vsna)
  away_exp_stl <<- (away_sums_stl * home_tov_vsna)
  away_exp_sft <<- (away_sums_sft * home_pfl_vsna)
  
  home_exp_tov <<- (home_sums_tov * away_stl_vsna)
  home_exp_pfl <<- (home_sums_pfl * away_sft_vsna)
  home_exp_orb <<- (home_sums_orb / away_drb_vsna)
  home_exp_drb <<- (home_sums_drb / away_orb_vsna)
  home_exp_ast <<- (home_sums_ast * away_dast_vsna)
  home_exp_blk <<- (home_sums_blk * away_dblk_vsna)
  home_exp_stl <<- (home_sums_stl * away_tov_vsna)
  home_exp_sft <<- (home_sums_sft * away_pfl_vsna)

  if (mc_loc == "A" | mc_loc == "H") {
    
    away_exp_tov_han <<- away_exp_tov * away_adv_tov
    away_exp_pfl_han <<- away_exp_pfl * away_adv_pfl
    away_exp_orb_han <<- away_exp_orb * away_adv_orb
    away_exp_drb_han <<- away_exp_drb * away_adv_drb
    away_exp_ast_han <<- away_exp_ast * away_adv_ast
    away_exp_blk_han <<- away_exp_blk * away_adv_blk
    away_exp_stl_han <<- away_exp_stl * away_adv_stl
    away_exp_sft_han <<- away_exp_sft * away_adv_sft
    
    home_exp_tov_han <<- home_exp_tov * home_adv_tov
    home_exp_pfl_han <<- home_exp_pfl * home_adv_pfl
    home_exp_orb_han <<- home_exp_orb * home_adv_orb
    home_exp_drb_han <<- home_exp_drb * home_adv_drb
    home_exp_ast_han <<- home_exp_ast * home_adv_ast
    home_exp_blk_han <<- home_exp_blk * home_adv_blk
    home_exp_stl_han <<- home_exp_stl * home_adv_stl
    home_exp_sft_han <<- home_exp_sft * home_adv_sft
    
  }
  
  else {
    
    away_exp_tov_han <<- away_exp_tov * neut_adv_tov
    away_exp_pfl_han <<- away_exp_pfl * neut_adv_pfl
    away_exp_orb_han <<- away_exp_orb * neut_adv_orb
    away_exp_drb_han <<- away_exp_drb * neut_adv_drb
    away_exp_ast_han <<- away_exp_ast * neut_adv_ast
    away_exp_blk_han <<- away_exp_blk * neut_adv_blk
    away_exp_stl_han <<- away_exp_stl * neut_adv_stl
    away_exp_sft_han <<- away_exp_sft * neut_adv_sft
    
    home_exp_tov_han <<- home_exp_tov * neut_adv_tov
    home_exp_pfl_han <<- home_exp_pfl * neut_adv_pfl
    home_exp_orb_han <<- home_exp_orb * neut_adv_orb
    home_exp_drb_han <<- home_exp_drb * neut_adv_drb
    home_exp_ast_han <<- home_exp_ast * neut_adv_ast
    home_exp_blk_han <<- home_exp_blk * neut_adv_blk
    home_exp_stl_han <<- home_exp_stl * neut_adv_stl
    home_exp_sft_han <<- home_exp_sft * neut_adv_sft 
    
  }
  
  
  away_exp_orb_han <<- away_exp_orb_han / (away_exp_orb_han + home_exp_drb_han)
  away_exp_drb_han <<- away_exp_drb_han / (away_exp_drb_han + home_exp_orb_han)
  home_exp_orb_han <<- home_exp_orb_han / (home_exp_orb_han + away_exp_drb_han)
  home_exp_drb_han <<- home_exp_drb_han / (home_exp_drb_han + away_exp_orb_han)
  
  away_shooting_frame <<- away_shootpct_han[away_floor_vector, ]
  home_shooting_frame <<- home_shootpct_han[home_floor_vector, ]
  
}

mc_waterfall <- function(xxxx) {
  
  exp_away_tov_poss <<- exp_tempo * away_exp_tov_han
  exp_away_pfl_ct <<- exp_tempo * away_exp_pfl_han * 2
  
  exp_home_tov_poss <<- exp_tempo * home_exp_tov_han
  exp_home_pfl_ct <<- exp_tempo * home_exp_pfl_han * 2
  
  exp_away_fta_ct <<- exp_tempo * away_exp_sft_han
  exp_home_fta_ct <<- exp_tempo * home_exp_sft_han
  
  exp_away_fta_poss <<- exp_away_fta_ct * .475
  exp_home_fta_poss <<- exp_home_fta_ct * .475
  
  exp_away_fga_poss <<- exp_tempo - exp_away_tov_poss - exp_away_fta_poss
  exp_home_fga_poss <<- exp_tempo - exp_home_tov_poss - exp_home_fta_poss
  
  exp_away_fga_pct <<- exp_away_fga_poss / exp_tempo
  exp_away_fta_pct <<- exp_away_fta_poss / exp_tempo
  exp_away_tov_pct <<- exp_away_tov_poss / exp_tempo
  exp_away_dist <<- c(exp_away_fga_pct, exp_away_fga_pct + exp_away_fta_pct, 1)
  
  exp_home_fga_pct <<- exp_home_fga_poss / exp_tempo
  exp_home_fta_pct <<- exp_home_fta_poss / exp_tempo
  exp_home_tov_pct <<- exp_home_tov_poss / exp_tempo
  exp_home_dist <<- c(exp_home_fga_pct, exp_home_fga_pct + exp_home_fta_pct, 1)
  
  away_2pm_cong <<- sum(away_shooting_frame[, 1] * away_sr2_prob)
  away_3pm_cong <<- sum(away_shooting_frame[, 2] * away_sr3_prob)
  exp_away_2pa_pct <<- exp_away_fga_pct * away_exp_sr2
  exp_away_3pa_pct <<- exp_away_fga_pct * away_exp_sr3
  exp_away_2pm_pct <<- exp_away_2pa_pct * away_2pm_cong
  exp_away_3pm_pct <<- exp_away_3pa_pct * away_3pm_cong
  away_make_x <<- (exp_away_2pm_pct + exp_away_3pm_pct)
  away_miss_x <<- ((exp_away_2pa_pct - exp_away_2pm_pct) + (exp_away_3pa_pct - exp_away_3pm_pct))
  
  home_2pm_cong <<- sum(home_shooting_frame[, 1] * home_sr2_prob)
  home_3pm_cong <<- sum(home_shooting_frame[, 2] * home_sr3_prob)
  exp_home_2pa_pct <<- exp_home_fga_pct * home_exp_sr2
  exp_home_3pa_pct <<- exp_home_fga_pct * home_exp_sr3
  exp_home_2pm_pct <<- exp_home_2pa_pct * home_2pm_cong
  exp_home_3pm_pct <<- exp_home_3pa_pct * home_3pm_cong
  home_make_x <<- (exp_home_2pm_pct + exp_home_3pm_pct)
  home_miss_x <<- ((exp_home_2pa_pct - exp_home_2pm_pct) + (exp_home_3pa_pct - exp_home_3pm_pct))
  
  away_assist_x <<- away_exp_ast_han / away_make_x
  away_block_x <<- away_exp_blk_han / home_miss_x
  home_assist_x <<- home_exp_ast_han / home_make_x
  home_block_x <<- home_exp_blk_han / away_miss_x
  away_steal_x <<- away_exp_stl_han / home_exp_tov_han
  home_steal_x <<- home_exp_stl_han / away_exp_tov_han
  
  
  away_pos_dist1 <<- exp_away_dist[1]
  away_pos_dist2 <<- exp_away_dist[2]
  away_pos_dist3 <<- exp_away_dist[3]
  
  home_pos_dist1 <<- exp_home_dist[1]
  home_pos_dist2 <<- exp_home_dist[2]
  home_pos_dist3 <<- exp_home_dist[3]
}

mc_possession_away <- function(xxxx) {
  
  #Away Possession
  
  r <<- runif(1)
  if (r <= away_pos_dist1) { pos_result <<- 1 }
  else if (r > away_pos_dist1 & r <= away_pos_dist2) { pos_result <<- 2 }
  else { pos_result <<- 3 }
  
  if (pos_result == 1) {
    
    r <<- runif(1)
    #2 Point FGA
    
    if (r <= away_exp_sr2) { 
      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 
        
        #Determine Who the 2 Point Shooter Is
        mc_shooter <<- sample(c(1:5), size = 1, prob = away_sr2_prob)
        
        
        mc_stat_add("A", mc_shooter, 5, 1)
        mc_stat_add("A", mc_shooter, 8, 1)
        #Retrieve 2 Point Shooting Percentage
        mc_shooter_pct <<- away_shooting_frame[mc_shooter, 1]
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 
          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_away_score <<- mc_away_score + 2
          mc_stat_add("A", mc_shooter, 4, 1)
          mc_stat_add("A", mc_shooter, 7, 1)
          mc_stat_add("A", mc_shooter, 24, 2)
          shot_pos_end <<- 1
          
          #Determine if there was an assist
          r <<- runif(1)
          if (r <= away_assist_x) {
            #There Was an Assist, Determine Assisting Player
            
            mc_assister <<- sample(c(1:5), size = 1, prob = away_ast_prob)
            mc_stat_add("A", mc_assister, 19, 1)
            
          }
        }
        else {
          #Shot is No Good, Determine if Offensive Rebound
          #Determine if there was a block
          
          
          r <<- runif(1)
          if (r <= home_block_x) {
            
            #There Was a Block, Determine Blocking Player  
            
            mc_blocker <<- sample(c(1:5), size = 1, prob = home_blk_prob)
            mc_stat_add("H", mc_blocker, 21, 1)
          
          }
          
          r <<- runif(1)
          if (r <= away_exp_orb_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = away_orb_prob)
            mc_stat_add("A", mc_rebounder, 16, 1)
            mc_stat_add("A", mc_rebounder, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = home_drb_prob)

            mc_stat_add("H", mc_rebounder, 17, 1)
            mc_stat_add("H", mc_rebounder, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
    }
    
    #3 Point FGA
    else {

      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 
        
        #Determine Who the 3 Point Shooter Is
        mc_shooter <<- sample(c(1:5), size = 1, prob = away_sr3_prob)

        mc_stat_add("A", mc_shooter, 5, 1)
        mc_stat_add("A", mc_shooter, 11, 1)
        
        #Retrieve 3 Point Shooting Percentage
        mc_shooter_pct <<- away_shooting_frame[mc_shooter, 2]
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 

          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_away_score <<- mc_away_score + 3
          mc_stat_add("A", mc_shooter, 4, 1)
          mc_stat_add("A", mc_shooter, 10, 1)
          mc_stat_add("A", mc_shooter, 24, 3)
          shot_pos_end <<- 1
          
          #Determine if there was an assist
          r <<- runif(1)
          if (r <= away_assist_x) {
            #There Was an Assist, Determine Assisting Player
            
            mc_assister <<- sample(c(1:5), size = 1, prob = away_ast_prob)

            mc_stat_add("A", mc_assister, 19, 1)
            
          }
        }
        else {

          #Determine if there was a block
          
          r <<- runif(1)
          if (r <= home_block_x) {
            
            #There Was a Block, Determine Blocking Player  
            
            mc_blocker <<- sample(c(1:5), size = 1, prob = home_blk_prob)

            mc_stat_add("H", mc_blocker, 21, 1)
            
          }
          
          #Shot is No Good, Determine if Offensive Rebound
          
          r <<- runif(1)
          if (r <= away_exp_orb_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = away_orb_prob)

            mc_stat_add("A", mc_rebounder, 16, 1)
            mc_stat_add("A", mc_rebounder, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = home_drb_prob)

            mc_stat_add("H", mc_rebounder, 17, 1)
            mc_stat_add("H", mc_rebounder, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
      #GREEDY GRANNY
    }
  }
  
  if (pos_result == 2) { 
    #Free Throw Possession
    
    #Determine if 2 or 3 shots
    r <<- runif(1)
    if (r <= ft_flank) { no_ft_shots <<- 3 }
    else { no_ft_shots <<- 2 }
    
    d <- 1
    j <- no_ft_shots
    for (d in d:j) {
        
      #Determine Free Throw Shooter
      mc_shooter <<- sample(c(1:5), size = 1, prob = away_sft_prob)
        
      mc_shooter_pct <<- away_shooting_frame[mc_shooter, 3]
      mc_stat_add("A", mc_shooter, 14, 1)
      #Determine if Free Throw is Good
        
      r <<- runif(1)
      if (r <= mc_shooter_pct) { 
        #Free Throw Is Good

        mc_away_score <<- mc_away_score + 1
        mc_stat_add("A", mc_shooter, 24, 1)
        mc_stat_add("A", mc_shooter, 13, 1)
      }
      else {
        #Free Throw Is No Good - Determine Defensive Rebound if Last Shot in Cycle

        if (d == j) {
        
          mc_rebounder <<- sample(c(1:5), size = 1, prob = home_drb_prob)

          #mc_stat_add("H", mc_rebounder, 17, 1)
          #mc_stat_add("H", mc_rebounder, 18, 1)
          
        }
      }
    }
  }
  
  if (pos_result == 3) {
    #Turnover
    
    #Determine Who Turned the Ball Over
    mc_sloppy <<- sample(c(1:5), size = 1, prob = away_tov_prob)

    mc_stat_add("A", mc_sloppy, 22, 1)
    
    #Determine if There Was a Steal
    r <<- runif(1)
    if (r <= home_steal_x) {
      
      mc_greedy <<- sample(c(1:5), size = 1, prob = home_stl_prob)

      mc_stat_add("H", mc_greedy, 20, 1) 
      
    }
  }
  #Determine if There Was A Foul on Away Team
  
  r <<- runif(1)
  
  if (r <= away_exp_pfl_han) {
    #Determine Who Foul Was On
    
    mc_eartha_brute <<- sample(c(1:5), size = 1, prob = away_pfl_prob)

    mc_stat_add("A", mc_eartha_brute, 23, 1) 
    
  }
  
  #Determine if There Was A Foul on Home Team
  
  r <<- runif(1)
  
  if (r <= home_exp_pfl_han) {
    #Determine Who Foul Was On
    
    mc_eartha_brute <<- sample(c(1:5), size = 1, prob = home_pfl_prob)

    mc_stat_add("H", mc_eartha_brute, 23, 1) 
    
  }
}

mc_possession_home <- function(xxxx) {
  
  #Away Possession
  
  r <<- runif(1)
  if (r <= home_pos_dist1) { pos_result <<- 1 }
  else if (r > home_pos_dist1 & r <= home_pos_dist2) { pos_result <<- 2 }
  else { pos_result <<- 3 }

  
  if (pos_result == 1) {
    
    r <<- runif(1)
    #2 Point FGA
    
    if (r <= home_exp_sr2) { 
      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 
        
        #Determine Who the 2 Point Shooter Is
        mc_shooter <<- sample(c(1:5), size = 1, prob = home_sr2_prob)
        
        mc_stat_add("H", mc_shooter, 5, 1)
        mc_stat_add("H", mc_shooter, 8, 1)
        #Retrieve 2 Point Shooting Percentage
        mc_shooter_pct <<- home_shooting_frame[mc_shooter, 1]
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 
          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_home_score <<- mc_home_score + 2
          mc_stat_add("H", mc_shooter, 4, 1)
          mc_stat_add("H", mc_shooter, 7, 1)
          mc_stat_add("H", mc_shooter, 24, 2)
          shot_pos_end <<- 1
          
          #Determine if there was an assist
          r <<- runif(1)
          if (r <= home_assist_x) {
            #There Was an Assist, Determine Assisting Player
            
            mc_assister <<- sample(c(1:5), size = 1, prob = home_ast_prob)

            mc_stat_add("H", mc_assister, 19, 1)
            
          }
        }
        else {
          #Shot is No Good, Determine if Offensive Rebound
          #Determine if there was a block
          
          r <<- runif(1)
          if (r <= away_block_x) {
            
            #There Was a Block, Determine Blocking Player  
            
            mc_blocker <<- sample(c(1:5), size = 1, prob = away_blk_prob)
            mc_stat_add("A", mc_blocker, 21, 1)
            
          }
          
          r <<- runif(1)
          if (r <= home_exp_orb_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = home_orb_prob)
            
            mc_stat_add("H", mc_rebounder, 16, 1)
            mc_stat_add("H", mc_rebounder, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = away_drb_prob)
            mc_stat_add("A", mc_rebounder, 17, 1)
            mc_stat_add("A", mc_rebounder, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
    }
    
    #3 Point FGA
    else {
      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 
        
        #Determine Who the 3 Point Shooter Is
        mc_shooter <<- sample(c(1:5), size = 1, prob = home_sr3_prob)
        mc_stat_add("H", mc_shooter, 5, 1)
        mc_stat_add("H", mc_shooter, 11, 1)
        
        #Retrieve 3 Point Shooting Percentage
        mc_shooter_pct <<- home_shooting_frame[mc_shooter, 2]
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 
          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_home_score <<- mc_home_score + 3
          mc_stat_add("H", mc_shooter, 4, 1)
          mc_stat_add("H", mc_shooter, 10, 1)
          mc_stat_add("H", mc_shooter, 24, 3)
          shot_pos_end <<- 1
          
          #Determine if there was an assist
          r <<- runif(1)
          if (r <= home_assist_x) {
            #There Was an Assist, Determine Assisting Player
            
            mc_assister <<- sample(c(1:5), size = 1, prob = home_ast_prob)
            mc_stat_add("H", mc_assister, 19, 1)
            
          }
        }
        else {
          #Determine if there was a block
          
          r <<- runif(1)
          if (r <= away_block_x) {
            
            #There Was a Block, Determine Blocking Player  
            
            mc_blocker <<- sample(c(1:5), size = 1, prob = away_blk_prob)
            mc_stat_add("A", mc_blocker, 21, 1)
            
          }
          
          #Shot is No Good, Determine if Offensive Rebound
          
          r <<- runif(1)
          if (r <= home_exp_orb_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = home_orb_prob)
            mc_stat_add("H", mc_rebounder, 16, 1)
            mc_stat_add("H", mc_rebounder, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
            
            mc_rebounder <<- sample(c(1:5), size = 1, prob = away_drb_prob)
            mc_stat_add("A", mc_rebounder, 17, 1)
            mc_stat_add("A", mc_rebounder, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
    }
  }
  
  if (pos_result == 2) { 
    #Free Throw Possession
    
    #Determine if 2 or 3 shots
    r <<- runif(1)
    if (r <= ft_flank) { no_ft_shots <<- 3 }
    else { no_ft_shots <<- 2 }
    
    d <- 1
    j <- no_ft_shots
    for (d in d:j) {
      
      #Determine Free Throw Shooter
      mc_shooter <<- sample(c(1:5), size = 1, prob = home_sft_prob)
      
      mc_shooter_pct <<- home_shooting_frame[mc_shooter, 3]
      mc_stat_add("H", mc_shooter, 14, 1)
      #Determine if Free Throw is Good
      
      r <<- runif(1)
      if (r <= mc_shooter_pct) { 
        #Free Throw Is Good
        mc_home_score <<- mc_home_score + 1
        mc_stat_add("H", mc_shooter, 24, 1)
        mc_stat_add("H", mc_shooter, 13, 1)
      }
      else {
        #Free Throw Is No Good - Determine Defensive Rebound if Last Shot in Cycle
        if (d == j) {
          
          mc_rebounder <<- sample(c(1:5), size = 1, prob = away_drb_prob)
          #mc_stat_add("A", mc_rebounder, 17, 1)
          #mc_stat_add("A", mc_rebounder, 18, 1)
          
        }
      }
    }
  }
  
  if (pos_result == 3) {
    #Turnover
    
    #Determine Who Turned the Ball Over
    mc_sloppy <<- sample(c(1:5), size = 1, prob = home_tov_prob)
    mc_stat_add("H", mc_sloppy, 22, 1)
    
    #Determine if There Was a Steal
    r <<- runif(1)
    if (r <= away_steal_x) {
      
      mc_greedy <<- sample(c(1:5), size = 1, prob = away_stl_prob)
      mc_stat_add("A", mc_greedy, 20, 1) 
      
    }
  }
  #Determine if There Was A Foul on Away Team
  
  r <<- runif(1)
  
  if (r <= away_exp_pfl_han) {
    #Determine Who Foul Was On
    
    mc_eartha_brute <<- sample(c(1:5), size = 1, prob = away_pfl_prob)

    mc_stat_add("A", mc_eartha_brute, 23, 1) 
    
  }
  
  #Determine if There Was A Foul on Home Team
  
  r <<- runif(1)
  
  if (r <= home_exp_pfl_han) {
    #Determine Who Foul Was On
    
    mc_eartha_brute <<- sample(c(1:5), size = 1, prob = home_pfl_prob)

    mc_stat_add("H", mc_eartha_brute, 23, 1) 
    
  }
  mc_stat_add("A", 1, 2, 1) 
  mc_stat_add("A", 2, 2, 1) 
  mc_stat_add("A", 3, 2, 1) 
  mc_stat_add("A", 4, 2, 1) 
  mc_stat_add("A", 5, 2, 1) 
  
  mc_stat_add("H", 1, 2, 1) 
  mc_stat_add("H", 2, 2, 1) 
  mc_stat_add("H", 3, 2, 1) 
  mc_stat_add("H", 4, 2, 1) 
  mc_stat_add("H", 5, 2, 1) 
}


mc_stat_add <- function(ha, player_slot, stat_type, stat_value) {
  
  #1 ID #2 POS #3 POS PCT #4 FGM #5 FGA #6 FG% #7 2PM #8 2PA #9 2P% #10 3PM #11 3PA #12 3P% #13 FTM
  #14 FTA #15 FT% #16 ORB #17 DRB #18 TRB #19 AST #20 STL #21 BLK #22 TOV #23 PFL #24 PTS
  
  if (ha == "A") {
     
    box_score_line <<- away_floor_vector[player_slot]
    prev_stat <<- mc_away_boxscore[box_score_line, stat_type]
    mc_away_boxscore[box_score_line, stat_type] <<- prev_stat + stat_value
    
  }
  else {
    
    box_score_line <<- home_floor_vector[player_slot]
    prev_stat <<- mc_home_boxscore[box_score_line, stat_type]
    mc_home_boxscore[box_score_line, stat_type] <<- prev_stat + stat_value
    
  }
}

mc_monte_carlo_do <- function(trials) {

  mc_tableset(1)
  mc_roster_select(1)
  mc_shoot_pcts(1)
  
  mc_away_boxscore_tot <<- matrix(0, nrow = nrow(mc_away_stats), ncol = 24)
  colnames(mc_away_boxscore_tot) <- c("ID", "POS", "POSPCT", "FGM", "FGA", "FGPCT", "2PM", "2PA", "2PPCT", "3PM", "3PA", "3PPCT",
                                  "FTM", "FTA", "FTPCT", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PFL", "PTS")

  mc_away_boxscore_tot[, 1] <- mc_away_stats[, 1]
  mc_away_boxscore_blank <<- mc_away_boxscore_tot
  
  mc_home_boxscore_tot <<- matrix(0, nrow = nrow(mc_home_stats), ncol = 24)
  colnames(mc_home_boxscore_tot) <- c("ID", "POS", "POSPCT", "FGM", "FGA", "FGPCT", "2PM", "2PA", "2PPCT", "3PM", "3PA", "3PPCT",
                                  "FTM", "FTA", "FTPCT", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PFL", "PTS")
  
  mc_home_boxscore_tot[, 1] <- mc_home_stats[, 1]
  mc_home_boxscore_blank <<- mc_home_boxscore_tot
  
  mc_margin_slotter <<- rep(0, trials)
  mc_total_slotter <<- rep(0, trials)
  mc_win_slotter <<- rep(0, trials)
  mc_ascore_slotter <<- rep(0, trials)
  mc_hscore_slotter <<- rep(0, trials)
  
  mc_awayscore_tot <<- 0
  mc_homescore_tot <<- 0
  mc_tempo_tot <<- 0
  mc_trials <<- trials
  orb_test <<- 0
  
  mc_cycle_do(trials)
  
}

mc_cycle_do <- function(trials) {
  
  a <- 0
  g <- trials
  ot_switch <<- 0
  ot_possessions_tot <<- 0
  
  mc_away_score <<- 0
  mc_home_score <<- 0
  
  mc_away_boxscore <<- mc_away_boxscore_blank
  mc_home_boxscore <<- mc_home_boxscore_blank
  
  if (r <= tempo_flank) { game_possessions <<- tempo_integer + 1 }
  else { game_possessions <<- tempo_integer }
  original_possessions <<- game_possessions
  
  while (a < g) {
    
    
    r <<- runif(1)
    
    if (ot_switch == 1) { game_possessions <<- overtime_possessions }
    b <- 1
    for (b in b:game_possessions) {
      
      mc_floor_shake_2(1)
      mc_exp_generate(1)
      mc_waterfall(1)
      mc_possession_away(1)
      mc_possession_home(1)
      pos_rem_full <<- game_possessions - b 
    }
    
    if (mc_away_score != mc_home_score) { 
      a <- a + 1 
      ot_switch <<- 0
      mc_away_boxscore_tot <<- mc_away_boxscore_tot + mc_away_boxscore
      mc_home_boxscore_tot <<- mc_home_boxscore_tot + mc_home_boxscore
      mc_awayscore_tot <<- mc_away_score + mc_awayscore_tot
      mc_homescore_tot <<- mc_home_score + mc_homescore_tot
      mc_tempo_tot <<- mc_tempo_tot + original_possessions + ot_possessions_tot
      ot_possessions_tot <<- 0
      mc_margin_slotter[a] <<- mc_away_score - mc_home_score
      mc_total_slotter[a] <<- (mc_away_score + mc_home_score)
      mc_ascore_slotter[a] <<- mc_away_score
      mc_hscore_slotter[a] <<- mc_home_score
      if (mc_away_score > mc_home_score) { mc_win_slotter[a] <<- 1 }
      
      #FOR CHAMP GAME ONLY - REMOVE FOR NEXT YEAR UNLESS PLAYING DFS
      if (a == 1) {
        mc_away_boxscore_list <<- list(mc_away_boxscore)
        mc_home_boxscore_list <<- list(mc_home_boxscore)
      }
      else {
        mc_away_boxscore_list[[a]] <<- mc_away_boxscore
        mc_home_boxscore_list[[a]] <<- mc_home_boxscore
      }
      #REMOVE oR UPDATE FOR 2021-2022
 
      mc_away_score <<- 0
      mc_home_score <<- 0
      
      mc_away_boxscore <<- mc_away_boxscore_blank
      mc_home_boxscore <<- mc_home_boxscore_blank
      
      if (r <= tempo_flank) { game_possessions <<- tempo_integer + 1 }
      else { game_possessions <<- tempo_integer }
      original_possessions <<- game_possessions
    }
    else { 
      ot_switch <<- 1
      overtime_possessions <<- game_possessions * .125
      ot_possessions_tot <<- overtime_possessions + ot_possessions_tot
    }
  }
  mc_finish(1)
}

mc_finish <- function(xxxx) {

  avg_awayscore <<- mc_awayscore_tot / mc_trials
  avg_homescore <<- mc_homescore_tot / mc_trials
  mc_home_boxscore_avg <<- mc_home_boxscore_tot / mc_trials
  mc_away_boxscore_avg <<- mc_away_boxscore_tot / mc_trials
  avg_tempo <<- mc_tempo_tot / mc_trials
  
  val_pos <<- nat_avgs$oe_do
  lg_drb <<- nat_avgs$drb_do
  lg_ftp <<- nat_avgs$ftpct_do
  lg_orb <<- nat_avgs$orb_do
  
  home_mvp_vector <<- ((mc_home_boxscore_avg[, 24] * 1) + 
                      ((mc_home_boxscore_avg[, 5] - mc_home_boxscore_avg[, 4]) * val_pos * lg_drb * -1) +
                      ((mc_home_boxscore_avg[, 14] - mc_home_boxscore_avg[, 13]) * lg_ftp * -1) +
                      (mc_home_boxscore_avg[, 16] * val_pos) + 
                      (mc_home_boxscore_avg[, 17] * val_pos) +
                      (mc_home_boxscore_avg[, 19] * .66 * val_pos) +
                      (mc_home_boxscore_avg[, 20] * val_pos) +
                      (mc_home_boxscore_avg[, 21] * lg_orb) +
                      (mc_home_boxscore_avg[, 22] * val_pos * -1) +
                      (mc_home_boxscore_avg[, 23] * -1 * .95)) / avg_tempo
  
  home_mvp_score <<- max(home_mvp_vector)
  home_mvp <<- which(home_mvp_vector == home_mvp_score)

  
  away_mvp_vector <<- ((mc_away_boxscore_avg[, 24] * 1) + 
                         ((mc_away_boxscore_avg[, 5] - mc_away_boxscore_avg[, 4]) * val_pos * lg_drb * -1) +
                         ((mc_away_boxscore_avg[, 14] - mc_away_boxscore_avg[, 13]) * lg_ftp * -1) +
                         (mc_away_boxscore_avg[, 16] * val_pos) + 
                         (mc_away_boxscore_avg[, 17] * val_pos) +
                         (mc_away_boxscore_avg[, 19] * .66 * val_pos) +
                         (mc_away_boxscore_avg[, 20] * val_pos) +
                         (mc_away_boxscore_avg[, 21] * lg_orb) +
                         (mc_away_boxscore_avg[, 22] * val_pos * -1) +
                         (mc_away_boxscore_avg[, 23] * -1 * .95)) / avg_tempo
  
  away_mvp_score <<- max(away_mvp_vector)
  away_mvp <<- which(away_mvp_vector == away_mvp_score)
  
  if (away_mvp_score > home_mvp_score) {
    
    game_mvp_score <<- away_mvp_score
    game_mvp <<- away_mvp
    game_mvp_pts <<- mc_away_boxscore_avg[away_mvp, 24]
    game_mvp_reb <<- mc_away_boxscore_avg[away_mvp, 18]
    game_mvp_ast <<- mc_away_boxscore_avg[away_mvp, 19]
    game_mvp_id <<- mc_away_boxscore_avg[away_mvp, 1]
    game_mvp_team <<- mc_away_id
    
  }
  else {
    
    game_mvp_score <<- home_mvp_score
    game_mvp <<- home_mvp
    game_mvp_pts <<- mc_home_boxscore_avg[home_mvp, 24]
    game_mvp_reb <<- mc_home_boxscore_avg[home_mvp, 18]
    game_mvp_ast <<- mc_home_boxscore_avg[home_mvp, 19]
    game_mvp_id <<- mc_home_boxscore_avg[home_mvp, 1]
    game_mvp_team <<- mc_home_id
    
  }
  
  away_lineup_short <<- mc_away_lineup[, c(1:2, 4)]
  away_dfs_frame <<- as.data.frame(mc_away_boxscore_avg)
  away_dfs_frame <<- away_dfs_frame %>%
    left_join(away_lineup_short, by = c("ID" = "PLAYER_ID")) %>%
    mutate(DFS = PTS + (`3PM` * 0.5) + (TRB * 1.25) + (AST * 1.5) + (STL * 2) +
           (BLK * 2) + (TOV * -0.5)) %>%
    select(ID, PLAYER_NAME, TEAM_NAME, DFS)
  
  if (is.null(dfs_frame) == TRUE) { 
    dfs_frame <<- away_dfs_frame 
    
    home_de_avg <<- sum(mc_home_boxscore_avg[, 24]) / avg_tempo
    home_d3p_avg <<- sum(mc_away_boxscore_avg[, 10]) / sum(mc_away_boxscore_avg[, 11])
    home_trb_avg <<- sum(mc_home_boxscore_avg[, 18]) / (sum(mc_home_boxscore_avg[, 18]) + sum(mc_away_boxscore_avg[, 18]))
    home_dast_avg <<- sum(mc_away_boxscore_avg[, 19]) / avg_tempo
    home_tov_avg <<- sum(mc_home_boxscore_avg[, 22]) / avg_tempo
    home_dblk_avg <<- sum(mc_away_boxscore_avg[, 21]) / avg_tempo
    home_stl_avg <<- sum(mc_away_boxscore_avg[, 22]) / avg_tempo
    
    dfs_whole_frame <<- as.data.frame(mc_away_boxscore_avg) %>%
      left_join(away_lineup_short, by = c("ID" = "PLAYER_ID")) %>%
      mutate(DFS = PTS + (`3PM` * 0.5) + (TRB * 1.25) + (AST * 1.5) + (STL * 2) +
               (BLK * 2) + (TOV * -0.5)) %>%
      select(ID, PLAYER_NAME, TEAM_NAME, DFS, PTS, `3PM`, TRB, AST, STL, BLK, TOV)
    
    dfs_whole_frame[, 12] <<- home_de_avg
    dfs_whole_frame[, 13] <<- home_d3p_avg
    dfs_whole_frame[, 14] <<- home_trb_avg
    dfs_whole_frame[, 15] <<- home_dast_avg
    dfs_whole_frame[, 16] <<- home_tov_avg
    dfs_whole_frame[, 17] <<- home_dblk_avg
    dfs_whole_frame[, 18] <<- home_stl_avg
    dfs_whole_frame[, 19] <<- avg_tempo
    
  }
  else { 
    dfs_frame <<- rbind(dfs_frame, away_dfs_frame)
    
    home_de_avg <<- sum(mc_home_boxscore_avg[, 24]) / avg_tempo
    home_d3p_avg <<- sum(mc_away_boxscore_avg[, 10]) / sum(mc_away_boxscore_avg[, 11])
    home_trb_avg <<- sum(mc_home_boxscore_avg[, 18]) / (sum(mc_home_boxscore_avg[, 18]) + sum(mc_away_boxscore_avg[, 18]))
    home_dast_avg <<- sum(mc_away_boxscore_avg[, 19]) / avg_tempo
    home_tov_avg <<- sum(mc_home_boxscore_avg[, 22]) / avg_tempo
    home_dblk_avg <<- sum(mc_away_boxscore_avg[, 21]) / avg_tempo
    home_stl_avg <<- sum(mc_away_boxscore_avg[, 22]) / avg_tempo
    
    dfs_whole_frame2 <<- as.data.frame(mc_away_boxscore_avg) %>%
      left_join(away_lineup_short, by = c("ID" = "PLAYER_ID")) %>%
      mutate(DFS = PTS + (`3PM` * 0.5) + (TRB * 1.25) + (AST * 1.5) + (STL * 2) +
               (BLK * 2) + (TOV * -0.5)) %>%
      select(ID, PLAYER_NAME, TEAM_NAME, DFS, PTS, `3PM`, TRB, AST, STL, BLK, TOV)
    
    dfs_whole_frame2[, 12] <<- home_de_avg
    dfs_whole_frame2[, 13] <<- home_d3p_avg
    dfs_whole_frame2[, 14] <<- home_trb_avg
    dfs_whole_frame2[, 15] <<- home_dast_avg
    dfs_whole_frame2[, 16] <<- home_tov_avg
    dfs_whole_frame2[, 17] <<- home_dblk_avg
    dfs_whole_frame2[, 18] <<- home_stl_avg
    dfs_whole_frame2[, 19] <<- avg_tempo
    
    dfs_whole_frame <<- rbind(dfs_whole_frame, dfs_whole_frame2)
  }
  
  home_lineup_short <<- mc_home_lineup[, c(1:2, 4)]
  home_dfs_frame <<- as.data.frame(mc_home_boxscore_avg)
  home_dfs_frame <<- home_dfs_frame %>%
    left_join(home_lineup_short, by = c("ID" = "PLAYER_ID")) %>%
    mutate(DFS = PTS + (`3PM` * 0.5) + (TRB * 1.25) + (AST * 1.5) + (STL * 2) +
             (BLK * 2) + (TOV * -0.5)) %>%
    select(ID, PLAYER_NAME, TEAM_NAME, DFS)
  
  if (is.null(dfs_frame) == TRUE) { 
    dfs_frame <<- home_dfs_frame 
    
    home_de_avg <<- sum(mc_away_boxscore_avg[, 24]) / avg_tempo
    home_d3p_avg <<- sum(mc_home_boxscore_avg[, 10]) / sum(mc_home_boxscore_avg[, 11])
    home_trb_avg <<- sum(mc_away_boxscore_avg[, 18]) / (sum(mc_home_boxscore_avg[, 18]) + sum(mc_away_boxscore_avg[, 18]))
    home_dast_avg <<- sum(mc_home_boxscore_avg[, 19]) / avg_tempo
    home_tov_avg <<- sum(mc_away_boxscore_avg[, 22]) / avg_tempo
    home_dblk_avg <<- sum(mc_home_boxscore_avg[, 21]) / avg_tempo
    home_stl_avg <<- sum(mc_home_boxscore_avg[, 22]) / avg_tempo
    
    dfs_whole_frame <<- as.data.frame(mc_home_boxscore_avg) %>%
      left_join(home_lineup_short, by = c("ID" = "PLAYER_ID")) %>%
      mutate(DFS = PTS + (`3PM` * 0.5) + (TRB * 1.25) + (AST * 1.5) + (STL * 2) +
               (BLK * 2) + (TOV * -0.5)) %>%
      select(ID, PLAYER_NAME, TEAM_NAME, DFS, PTS, `3PM`, TRB, AST, STL, BLK, TOV)
    
    dfs_whole_frame[, 12] <<- home_de_avg
    dfs_whole_frame[, 13] <<- home_d3p_avg
    dfs_whole_frame[, 14] <<- home_trb_avg
    dfs_whole_frame[, 15] <<- home_dast_avg
    dfs_whole_frame[, 16] <<- home_tov_avg
    dfs_whole_frame[, 17] <<- home_dblk_avg
    dfs_whole_frame[, 18] <<- home_stl_avg
    dfs_whole_frame[, 19] <<- avg_tempo
    
    
  }
  else { 
    dfs_frame <<- rbind(dfs_frame, home_dfs_frame) 
    
    home_de_avg <<- sum(mc_away_boxscore_avg[, 24]) / avg_tempo
    home_d3p_avg <<- sum(mc_home_boxscore_avg[, 10]) / sum(mc_home_boxscore_avg[, 11])
    home_trb_avg <<- sum(mc_away_boxscore_avg[, 18]) / (sum(mc_home_boxscore_avg[, 18]) + sum(mc_away_boxscore_avg[, 18]))
    home_dast_avg <<- sum(mc_home_boxscore_avg[, 19]) / avg_tempo
    home_tov_avg <<- sum(mc_away_boxscore_avg[, 22]) / avg_tempo
    home_dblk_avg <<- sum(mc_home_boxscore_avg[, 21]) / avg_tempo
    home_stl_avg <<- sum(mc_home_boxscore_avg[, 22]) / avg_tempo
    
    dfs_whole_frame2 <<- as.data.frame(mc_home_boxscore_avg) %>%
      left_join(home_lineup_short, by = c("ID" = "PLAYER_ID")) %>%
      mutate(DFS = PTS + (`3PM` * 0.5) + (TRB * 1.25) + (AST * 1.5) + (STL * 2) +
               (BLK * 2) + (TOV * -0.5)) %>%
      select(ID, PLAYER_NAME, TEAM_NAME, DFS, PTS, `3PM`, TRB, AST, STL, BLK, TOV)
    
    dfs_whole_frame2[, 12] <<- home_de_avg
    dfs_whole_frame2[, 13] <<- home_d3p_avg
    dfs_whole_frame2[, 14] <<- home_trb_avg
    dfs_whole_frame2[, 15] <<- home_dast_avg
    dfs_whole_frame2[, 16] <<- home_tov_avg
    dfs_whole_frame2[, 17] <<- home_dblk_avg
    dfs_whole_frame2[, 18] <<- home_stl_avg
    dfs_whole_frame2[, 19] <<- avg_tempo
    
    dfs_whole_frame <<- rbind(dfs_whole_frame, dfs_whole_frame2)
  }
}

mc_time_test <- function(xxxx) {
  
  a <- 1
  g <- xxxx
  game_possessions2 <<- 69
  for (a in a:g) {
    
    b <- 1
    for (b in b:game_possessions2) {
      
      mc_floor_shake_2(1)
      mc_exp_generate(1)
      mc_waterfall(1)
      mc_possession_away(1)
      
    }
    
    b <- 1
    for (b in b:game_possessions2) {
      
      mc_floor_shake_2(1)
      mc_exp_generate(1)
      mc_waterfall(1)
      mc_possession_home(1)
      
    }
  }
}

mc_time_test2 <- function(xxxx) {
  
  a <- 1
  g <- xxxx
  for (a in a:g) {
    mc_floor_shake_2(1)
  }
}

mc_diagnostics <- function(xxxx) {
  
  mc_diag_away <<- as.data.frame(mc_away_boxscore_avg)
  mc_diag_home <<- as.data.frame(mc_home_boxscore_avg)
  
  mc_abox_sums <<- colSums(mc_diag_away)
  mc_hbox_sums <<- colSums(mc_diag_home)
  
  game_pos <<- mc_abox_sums[2] / 5
  
  mc_diag_away[, 3] <<- mc_diag_away[, 2] / game_pos
  mc_diag_away[, 6] <<- mc_diag_away[, 4] / mc_diag_away[, 5]
  mc_diag_away[, 9] <<- mc_diag_away[, 7] / mc_diag_away[, 8]
  mc_diag_away[, 12] <<- mc_diag_away[, 10] / mc_diag_away[, 11]
  mc_diag_away[, 15] <<- mc_diag_away[, 13] / mc_diag_away[, 14]
  mc_diag_away[, 25] <<- mc_diag_away[, 24] / game_pos
  mc_diag_away[, 26] <<- mc_diag_away[, 22] / game_pos
  
  mc_diag_home[, 3] <<- mc_diag_home[, 2] / game_pos
  mc_diag_home[, 6] <<- mc_diag_home[, 4] / mc_diag_home[, 5]
  mc_diag_home[, 9] <<- mc_diag_home[, 7] / mc_diag_home[, 8]
  mc_diag_home[, 12] <<- mc_diag_home[, 10] / mc_diag_home[, 11]
  mc_diag_home[, 15] <<- mc_diag_home[, 13] / mc_diag_home[, 14]
  mc_diag_home[, 25] <<- mc_diag_home[, 24] / game_pos
  mc_diag_home[, 26] <<- mc_diag_home[, 22] / game_pos
  
  mc_diag_a_orb <<- sum(mc_diag_away[, 16])
  mc_diag_a_drb <<- sum(mc_diag_away[, 17])
  mc_diag_a_trb <<- sum(mc_diag_away[, 18])
  
  mc_diag_h_orb <<- sum(mc_diag_home[, 16])
  mc_diag_h_drb <<- sum(mc_diag_home[, 17])
  mc_diag_h_trb <<- sum(mc_diag_home[, 18])
  
}

#-------------------
#TEAM MONTE CARLO
#-------------------

mt_tableset <- function(xxxx) {
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  nat_avg_mx <<- as.matrix(nat_avgs)
  
  away_season_tempo <<- mc_away_teamstats[35]
  home_season_tempo <<- mc_home_teamstats[35]
  nat_tempo <<- nat_avg_mx[1]
  
  away_tempo_vsna <<- away_season_tempo - nat_tempo
  home_tempo_vsna <<- home_season_tempo - nat_tempo
  vsna_sums <<- away_tempo_vsna + home_tempo_vsna
  
  exp_tempo <<- (nat_tempo + vsna_sums) * xxxx
  ft_flank <<- (1/.475) - 2
  tempo_integer <<- as.integer(exp_tempo)
  tempo_flank <<- exp_tempo - tempo_integer
  
  away_sr2 <<- as.numeric(mc_away_teamstats[1, 3])
  away_sm2 <<- as.numeric(mc_away_teamstats[1, 4])
  away_sr3 <<- as.numeric(mc_away_teamstats[1, 5])
  away_sm3 <<- as.numeric(mc_away_teamstats[1, 6])
  away_orb <<- as.numeric(mc_away_teamstats[1, 8])
  away_drb <<- as.numeric(mc_away_teamstats[1, 9])
  away_tov <<- as.numeric(mc_away_teamstats[1, 14])
  away_pfl <<- as.numeric(mc_away_teamstats[1, 15])
  away_dsr2 <<- as.numeric(mc_away_teamstats[1, 18])
  away_dsm2 <<- as.numeric(mc_away_teamstats[1, 19])
  away_dsr3 <<- as.numeric(mc_away_teamstats[1, 20])
  away_dsm3 <<- as.numeric(mc_away_teamstats[1, 21])
  away_dorb <<- as.numeric(mc_away_teamstats[1, 23])
  away_ddrb <<- as.numeric(mc_away_teamstats[1, 24])
  away_dtov <<- as.numeric(mc_away_teamstats[1, 29])
  away_dpfl <<- as.numeric(mc_away_teamstats[1, 30])
  away_srft <<- as.numeric(mc_away_teamstats[1, 33])
  away_dsrft <<- as.numeric(mc_away_teamstats[1, 34])
  away_ftpct <<- as.numeric(mc_away_teamstats[1, 36])
  
  home_sr2 <<- as.numeric(mc_home_teamstats[1, 3])
  home_sm2 <<- as.numeric(mc_home_teamstats[1, 4])
  home_sr3 <<- as.numeric(mc_home_teamstats[1, 5])
  home_sm3 <<- as.numeric(mc_home_teamstats[1, 6])
  home_orb <<- as.numeric(mc_home_teamstats[1, 8])
  home_drb <<- as.numeric(mc_home_teamstats[1, 9])
  home_tov <<- as.numeric(mc_home_teamstats[1, 14])
  home_pfl <<- as.numeric(mc_home_teamstats[1, 15])
  home_dsr2 <<- as.numeric(mc_home_teamstats[1, 18])
  home_dsm2 <<- as.numeric(mc_home_teamstats[1, 19])
  home_dsr3 <<- as.numeric(mc_home_teamstats[1, 20])
  home_dsm3 <<- as.numeric(mc_home_teamstats[1, 21])
  home_dorb <<- as.numeric(mc_home_teamstats[1, 23])
  home_ddrb <<- as.numeric(mc_home_teamstats[1, 24])
  home_dtov <<- as.numeric(mc_home_teamstats[1, 29])
  home_dpfl <<- as.numeric(mc_home_teamstats[1, 30])
  home_srft <<- as.numeric(mc_home_teamstats[1, 33])
  home_dsrft <<- as.numeric(mc_home_teamstats[1, 34])
  home_ftpct <<- as.numeric(mc_home_teamstats[1, 36])
  
  nat_sr2 <<- as.numeric(nat_avg_mx[1, 4])
  nat_sm2 <<- as.numeric(nat_avg_mx[1, 5])
  nat_sr3 <<- as.numeric(nat_avg_mx[1, 6])
  nat_sm3 <<- as.numeric(nat_avg_mx[1, 7])
  nat_orb <<- as.numeric(nat_avg_mx[1, 10])
  nat_drb <<- as.numeric(nat_avg_mx[1, 11])
  nat_tov <<- as.numeric(nat_avg_mx[1, 16])
  nat_pfl <<- as.numeric(nat_avg_mx[1, 17])
  nat_srft <<- as.numeric(nat_avg_mx[1, 19])
  
  away_sr2_vsna <<- away_sr2 - nat_sr2
  away_sm2_vsna <<- away_sm2 / nat_sm2
  away_sr3_vsna <<- away_sr3 - nat_sr3
  away_sm3_vsna <<- away_sm3 / nat_sm3
  away_orb_vsna <<- away_orb / nat_orb
  away_drb_vsna <<- away_drb / nat_drb
  away_tov_vsna <<- away_tov / nat_tov
  away_pfl_vsna <<- away_pfl / nat_pfl
  away_srft_vsna <<- away_srft / nat_srft
  away_dsr2_vsna <<- away_dsr2 - nat_sr2
  away_dsm2_vsna <<- away_dsm2 / nat_sm2
  away_dsr3_vsna <<- away_dsr3 - nat_sr3
  away_dsm3_vsna <<- away_dsm3 / nat_sm3
  away_dorb_vsna <<- away_dorb / nat_orb
  away_ddrb_vsna <<- away_ddrb / nat_drb
  away_dtov_vsna <<- away_dtov / nat_tov
  away_dpfl_vsna <<- away_dpfl / nat_pfl
  away_dsrft_vsna <<- away_dsrft / nat_srft
  
  home_sr2_vsna <<- home_sr2 - nat_sr2
  home_sm2_vsna <<- home_sm2 / nat_sm2
  home_sr3_vsna <<- home_sr3 - nat_sr3
  home_sm3_vsna <<- home_sm3 / nat_sm3
  home_orb_vsna <<- home_orb / nat_orb
  home_drb_vsna <<- home_drb / nat_drb
  home_tov_vsna <<- home_tov / nat_tov
  home_pfl_vsna <<- home_pfl / nat_pfl
  home_srft_vsna <<- home_srft / nat_srft
  home_dsr2_vsna <<- home_dsr2 - nat_sr2
  home_dsm2_vsna <<- home_dsm2 / nat_sm2
  home_dsr3_vsna <<- home_dsr3 - nat_sr3
  home_dsm3_vsna <<- home_dsm3 / nat_sm3
  home_dorb_vsna <<- home_dorb / nat_orb
  home_ddrb_vsna <<- home_ddrb / nat_drb
  home_dtov_vsna <<- home_dtov / nat_tov
  home_dpfl_vsna <<- home_dpfl / nat_pfl
  home_dsrft_vsna <<- home_dsrft / nat_srft
  
  away_sr2_exp <<- (away_sr2 + home_dsr2) / 2
  away_sr3_exp <<- (away_sr3 + home_dsr3) / 2
  home_sr2_exp <<- (home_sr2 + away_dsr2) / 2
  home_sr3_exp <<- (home_sr3 + away_dsr3) / 2
  
  away_sm2_exp <<- away_sm2 * home_dsm2_vsna
  away_sm3_exp <<- away_sm3 * home_dsm3_vsna
  away_orb_exp <<- away_orb / home_drb_vsna
  away_drb_exp <<- away_drb / home_orb_vsna
  away_tov_exp <<- away_tov * home_dtov_vsna
  away_pfl_exp <<- away_pfl * home_dpfl_vsna
  away_srft_exp <<- away_srft * home_dsrft_vsna
  
  home_sm2_exp <<- home_sm2 * away_dsm2_vsna
  home_sm3_exp <<- home_sm3 * away_dsm3_vsna
  home_orb_exp <<- home_orb / away_drb_vsna
  home_drb_exp <<- home_drb / away_orb_vsna
  home_tov_exp <<- home_tov * away_dtov_vsna
  home_pfl_exp <<- home_pfl * away_dpfl_vsna
  home_srft_exp <<- home_srft * away_dsrft_vsna
  
  advantage_mx <<- as.matrix(advantage_frame[1:3, 2:21])
  
  if (mc_loc == "A" | mc_loc == "H") {
    
    away_sm2_exp_han <<- away_sm2_exp * as.numeric(advantage_mx[2, 6])
    away_sm3_exp_han <<- away_sm3_exp * as.numeric(advantage_mx[2, 8])
    away_orb_exp_han <<- away_orb_exp * as.numeric(advantage_mx[2, 11]) 
    away_drb_exp_han <<- away_drb_exp * as.numeric(advantage_mx[2, 12]) 
    away_tov_exp_han <<- away_tov_exp * as.numeric(advantage_mx[2, 17]) 
    away_pfl_exp_han <<- away_pfl_exp * as.numeric(advantage_mx[2, 18])
    away_srft_exp_han <<- away_srft_exp * as.numeric(advantage_mx[2, 20]) 
    away_ftpct_exp_han <<- away_ftpct * as.numeric(advantage_mx[2, 9])
    
    home_sm2_exp_han <<- home_sm2_exp * as.numeric(advantage_mx[1, 6])
    home_sm3_exp_han <<- home_sm3_exp * as.numeric(advantage_mx[1, 8])
    home_orb_exp_han <<- home_orb_exp * as.numeric(advantage_mx[1, 11]) 
    home_drb_exp_han <<- home_drb_exp * as.numeric(advantage_mx[1, 12]) 
    home_tov_exp_han <<- home_tov_exp * as.numeric(advantage_mx[1, 17]) 
    home_pfl_exp_han <<- home_pfl_exp * as.numeric(advantage_mx[1, 18]) 
    home_srft_exp_han <<- home_srft_exp * as.numeric(advantage_mx[1, 20]) 
    home_ftpct_exp_han <<- home_ftpct * as.numeric(advantage_mx[1, 9])
    
  }
  else {
    
    away_sm2_exp_han <<- away_sm2_exp * as.numeric(advantage_mx[3, 6])
    away_sm3_exp_han <<- away_sm3_exp * as.numeric(advantage_mx[3, 8]) 
    away_orb_exp_han <<- away_orb_exp * as.numeric(advantage_mx[3, 11]) 
    away_drb_exp_han <<- away_drb_exp * as.numeric(advantage_mx[3, 12]) 
    away_tov_exp_han <<- away_tov_exp * as.numeric(advantage_mx[3, 17]) 
    away_pfl_exp_han <<- away_pfl_exp * as.numeric(advantage_mx[3, 18]) 
    away_srft_exp_han <<- away_srft_exp * as.numeric(advantage_mx[3, 20]) 
    away_ftpct_exp_han <<- away_ftpct * as.numeric(advantage_mx[3, 9])
    
    home_sm2_exp_han <<- home_sm2_exp * as.numeric(advantage_mx[3, 6])
    home_sm3_exp_han <<- home_sm3_exp * as.numeric(advantage_mx[3, 8]) 
    home_orb_exp_han <<- home_orb_exp * as.numeric(advantage_mx[3, 11]) 
    home_drb_exp_han <<- home_drb_exp * as.numeric(advantage_mx[3, 12]) 
    home_tov_exp_han <<- home_tov_exp * as.numeric(advantage_mx[3, 17]) 
    home_pfl_exp_han <<- home_pfl_exp * as.numeric(advantage_mx[3, 18]) 
    home_srft_exp_han <<- home_srft_exp * as.numeric(advantage_mx[3, 20]) 
    home_ftpct_exp_han <<- home_ftpct * as.numeric(advantage_mx[3, 9])
    
  }
  
  away_sr2_exp <<- away_sr2_exp / (away_sr2_exp + away_sr3_exp)
  away_sr3_exp <<- 1 - away_sr2_exp
  home_sr2_exp <<- home_sr2_exp / (home_sr2_exp + home_sr3_exp)
  home_sr3_exp <<- 1 - home_sr2_exp
  
  away_orb_exp_han <<- away_orb_exp_han / (away_orb_exp_han + home_drb_exp_han)
  away_drb_exp_han <<- away_drb_exp_han / (away_drb_exp_han + home_orb_exp_han)
  home_orb_exp_han <<- 1 - away_drb_exp_han
  home_drb_exp_han <<- 1 - away_orb_exp_han
  
  
}

mt_waterfall <- function(xxxx) {
  
  exp_away_tov_poss <<- exp_tempo * away_tov_exp_han
  exp_away_pfl_ct <<- exp_tempo * away_pfl_exp_han * 2
  
  exp_home_tov_poss <<- exp_tempo * home_tov_exp_han
  exp_home_pfl_ct <<- exp_tempo * home_pfl_exp_han * 2
  
  exp_away_fta_ct <<- exp_tempo * away_srft_exp_han
  exp_home_fta_ct <<- exp_tempo * home_srft_exp_han
  
  exp_away_fta_poss <<- exp_away_fta_ct * .475
  exp_home_fta_poss <<- exp_home_fta_ct * .475
  
  exp_away_fga_poss <<- exp_tempo - exp_away_tov_poss - exp_away_fta_poss
  exp_home_fga_poss <<- exp_tempo - exp_home_tov_poss - exp_home_fta_poss
  
  exp_away_fga_pct <<- exp_away_fga_poss / exp_tempo
  exp_away_fta_pct <<- exp_away_fta_poss / exp_tempo
  exp_away_tov_pct <<- exp_away_tov_poss / exp_tempo
  exp_away_dist <<- c(exp_away_fga_pct, exp_away_fga_pct + exp_away_fta_pct, 1)
  
  exp_home_fga_pct <<- exp_home_fga_poss / exp_tempo
  exp_home_fta_pct <<- exp_home_fta_poss / exp_tempo
  exp_home_tov_pct <<- exp_home_tov_poss / exp_tempo
  exp_home_dist <<- c(exp_home_fga_pct, exp_home_fga_pct + exp_home_fta_pct, 1)
  
  away_2pm_cong <<- away_sm2_exp_han
  away_3pm_cong <<- away_sm3_exp_han
  exp_away_2pa_pct <<- exp_away_fga_pct * away_sr2_exp
  exp_away_3pa_pct <<- exp_away_fga_pct * away_sr3_exp
  exp_away_2pm_pct <<- exp_away_2pa_pct * away_2pm_cong
  exp_away_3pm_pct <<- exp_away_3pa_pct * away_3pm_cong
  away_make_x <<- (exp_away_2pm_pct + exp_away_3pm_pct)
  away_miss_x <<- ((exp_away_2pa_pct - exp_away_2pm_pct) + (exp_away_3pa_pct - exp_away_3pm_pct))
  
  home_2pm_cong <<- home_sm2_exp_han
  home_3pm_cong <<- home_sm3_exp_han
  exp_home_2pa_pct <<- exp_home_fga_pct * home_sr2_exp
  exp_home_3pa_pct <<- exp_home_fga_pct * home_sr3_exp
  exp_home_2pm_pct <<- exp_home_2pa_pct * home_2pm_cong
  exp_home_3pm_pct <<- exp_home_3pa_pct * home_3pm_cong
  home_make_x <<- (exp_home_2pm_pct + exp_home_3pm_pct)
  home_miss_x <<- ((exp_home_2pa_pct - exp_home_2pm_pct) + (exp_home_3pa_pct - exp_home_3pm_pct))

  away_pos_dist1 <<- exp_away_dist[1]
  away_pos_dist2 <<- exp_away_dist[2]
  away_pos_dist3 <<- exp_away_dist[3]
  
  home_pos_dist1 <<- exp_home_dist[1]
  home_pos_dist2 <<- exp_home_dist[2]
  home_pos_dist3 <<- exp_home_dist[3]
}

mt_possession_away <- function(xxxx) {
  
  #Away Possession
  
  r <<- runif(1)
  if (r <= away_pos_dist1) { pos_result <<- 1 }
  else if (r > away_pos_dist1 & r <= away_pos_dist2) { pos_result <<- 2 }
  else { pos_result <<- 3 }
  
  if (pos_result == 1) {
    
    r <<- runif(1)
    #2 Point FGA
    
    if (r <= away_sr2_exp) { 
      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 

        mt_stat_add("A", 1, 5, 1)
        mt_stat_add("A", 1, 8, 1)
        #Retrieve 2 Point Shooting Percentage
        mc_shooter_pct <<- away_sm2_exp_han
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 
          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_away_score <<- mc_away_score + 2
          mt_stat_add("A", 1, 4, 1)
          mt_stat_add("A", 1, 7, 1)
          mt_stat_add("A", 1, 24, 2)
          shot_pos_end <<- 1
          
        }
        else {
          #Shot is No Good, Determine if Offensive Rebound

          r <<- runif(1)
          if (r <= away_orb_exp_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mt_stat_add("A", 1, 16, 1)
            mt_stat_add("A", 1, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
   
            mt_stat_add("H", 1, 17, 1)
            mt_stat_add("H", 1, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
    }
    
    #3 Point FGA
    else {
      
      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 
        
        #Determine Who the 3 Point Shooter Is
        
        mt_stat_add("A", 1, 5, 1)
        mt_stat_add("A", 1, 11, 1)
        
        #Retrieve 3 Point Shooting Percentage
        mc_shooter_pct <<- away_sm3_exp_han
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 
          
          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_away_score <<- mc_away_score + 3
          mt_stat_add("A", 1, 4, 1)
          mt_stat_add("A", 1, 10, 1)
          mt_stat_add("A", 1, 24, 3)
          shot_pos_end <<- 1
          
        }
        else {
  
          #Shot is No Good, Determine if Offensive Rebound
          
          r <<- runif(1)
          if (r <= away_orb_exp_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mt_stat_add("A", 1, 16, 1)
            mt_stat_add("A", 1, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
            
            mt_stat_add("H", 1, 17, 1)
            mt_stat_add("H", 1, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
      #GREEDY GRANNY
    }
  }
  
  if (pos_result == 2) { 
    #Free Throw Possession
    
    #Determine if 2 or 3 shots
    r <<- runif(1)
    if (r <= ft_flank) { no_ft_shots <<- 3 }
    else { no_ft_shots <<- 2 }
    
    d <- 1
    j <- no_ft_shots
    for (d in d:j) {
      
      #Determine Free Throw Shooter
      
      mc_shooter_pct <<- away_ftpct_exp_han
      mt_stat_add("A", 1, 14, 1)
      #Determine if Free Throw is Good
      
      r <<- runif(1)
      if (r <= mc_shooter_pct) { 
        #Free Throw Is Good
        
        mc_away_score <<- mc_away_score + 1
        mt_stat_add("A", 1, 24, 1)
        mt_stat_add("A", 1, 13, 1)
      }
      else {
        #Free Throw Is No Good - Determine Defensive Rebound if Last Shot in Cycle
        
        if (d == j) {
          
          #mc_rebounder <<- sample(c(1:5), size = 1, prob = home_drb_prob)
          
          #mt_stat_add("H", mc_rebounder, 17, 1)
          #mt_stat_add("H", mc_rebounder, 18, 1)
          
        }
      }
    }
  }
  
  if (pos_result == 3) {
    #Turnover
    
    #Determine Who Turned the Ball Over
    
    mt_stat_add("A", 1, 22, 1)
    
  }
  #Determine if There Was A Foul on Away Team
  
  r <<- runif(1)
  
  if (r <= away_pfl_exp_han) {
    
    mt_stat_add("A", 1, 23, 1) 
    
  }
  
  #Determine if There Was A Foul on Home Team
  
  r <<- runif(1)
  
  if (r <= home_pfl_exp_han) {
    
    mt_stat_add("H", 1, 23, 1) 
    
  }
}

mt_possession_home <- function(xxxx) {
  
  #Away Possession
  
  r <<- runif(1)
  if (r <= home_pos_dist1) { pos_result <<- 1 }
  else if (r > home_pos_dist1 & r <= home_pos_dist2) { pos_result <<- 2 }
  else { pos_result <<- 3 }
  
  if (pos_result == 1) {
    
    r <<- runif(1)
    #2 Point FGA
    
    if (r <= home_sr2_exp) { 
      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 
        
        mt_stat_add("H", 1, 5, 1)
        mt_stat_add("H", 1, 8, 1)
        #Retrieve 2 Point Shooting Percentage
        mc_shooter_pct <<- home_sm2_exp_han
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 
          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_home_score <<- mc_home_score + 2
          mt_stat_add("H", 1, 4, 1)
          mt_stat_add("H", 1, 7, 1)
          mt_stat_add("H", 1, 24, 2)
          shot_pos_end <<- 1
          
        }
        else {
          #Shot is No Good, Determine if Offensive Rebound
          
          r <<- runif(1)
          if (r <= home_orb_exp_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mt_stat_add("H", 1, 16, 1)
            mt_stat_add("H", 1, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
            
            mt_stat_add("A", 1, 17, 1)
            mt_stat_add("A", 1, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
    }
    
    #3 Point FGA
    else {
      
      shot_pos_end <<- 0
      #Loop Until Shot is Made or Missed and Rebounded by Defense
      while (shot_pos_end <= 0) { 
        
        #Determine Who the 3 Point Shooter Is
        
        mt_stat_add("H", 1, 5, 1)
        mt_stat_add("H", 1, 11, 1)
        
        #Retrieve 3 Point Shooting Percentage
        mc_shooter_pct <<- home_sm3_exp_han
        
        #Determine if Shot is Good
        r <<- runif(1)
        if (r <= mc_shooter_pct) { 
          
          #Shot is Good, Add 2 Points to Away Team and End Possession
          mc_home_score <<- mc_home_score + 3
          mt_stat_add("H", 1, 4, 1)
          mt_stat_add("H", 1, 10, 1)
          mt_stat_add("H", 1, 24, 3)
          shot_pos_end <<- 1
          
        }
        else {
          
          #Shot is No Good, Determine if Offensive Rebound
          
          r <<- runif(1)
          if (r <= home_orb_exp_han) {
            
            #Offensive Rebound, Determine Who Rebounded, Start from Top
            
            mt_stat_add("H", 1, 16, 1)
            mt_stat_add("H", 1, 18, 1)
            
          }
          else {
            #Defensive Rebound, Determine Who Rebounded, End Possession
            
            mt_stat_add("A", 1, 17, 1)
            mt_stat_add("A", 1, 18, 1)
            
            shot_pos_end <<- 1
            
          }
        }
      }
      #GREEDY GRANNY
    }
  }
  
  if (pos_result == 2) { 
    #Free Throw Possession
    
    #Determine if 2 or 3 shots
    r <<- runif(1)
    if (r <= ft_flank) { no_ft_shots <<- 3 }
    else { no_ft_shots <<- 2 }
    
    d <- 1
    j <- no_ft_shots
    for (d in d:j) {
      
      #Determine Free Throw Shooter
      
      mc_shooter_pct <<- home_ftpct_exp_han
      mt_stat_add("H", 1, 14, 1)
      #Determine if Free Throw is Good
      
      r <<- runif(1)
      if (r <= mc_shooter_pct) { 
        #Free Throw Is Good
        
        mc_home_score <<- mc_home_score + 1
        mt_stat_add("H", 1, 24, 1)
        mt_stat_add("H", 1, 13, 1)
      }
      else {
        #Free Throw Is No Good - Determine Defensive Rebound if Last Shot in Cycle
        
        if (d == j) {
          
          #mc_rebounder <<- sample(c(1:5), size = 1, prob = home_drb_prob)
          
          #mt_stat_add("H", mc_rebounder, 17, 1)
          #mt_stat_add("H", mc_rebounder, 18, 1)
          
        }
      }
    }
  }
  
  if (pos_result == 3) {
    #Turnover
    
    #Determine Who Turned the Ball Over
    
    mt_stat_add("H", 1, 22, 1)
    
  }
  #Determine if There Was A Foul on Away Team
  
  r <<- runif(1)
  
  if (r <= home_pfl_exp_han) {
    
    mt_stat_add("H", 1, 23, 1) 
    
  }
  
  #Determine if There Was A Foul on Home Team
  
  r <<- runif(1)
  
  if (r <= away_pfl_exp_han) {
    
    mt_stat_add("A", 1, 23, 1) 
    
  }
}

mt_stat_add <- function(ha, player_slot, stat_type, stat_value) {
  
  #1 ID #2 POS #3 POS PCT #4 FGM #5 FGA #6 FG% #7 2PM #8 2PA #9 2P% #10 3PM #11 3PA #12 3P% #13 FTM
  #14 FTA #15 FT% #16 ORB #17 DRB #18 TRB #19 AST #20 STL #21 BLK #22 TOV #23 PFL #24 PTS
  
  if (ha == "A") {
    
    box_score_line <<- 1
    prev_stat <<- mt_away_boxscore[box_score_line, stat_type]
    mt_away_boxscore[box_score_line, stat_type] <<- prev_stat + stat_value
    
  }
  else {
    
    box_score_line <<- 1
    prev_stat <<- mt_home_boxscore[box_score_line, stat_type]
    mt_home_boxscore[box_score_line, stat_type] <<- prev_stat + stat_value
    
  }
}

mt_monte_carlo_do <- function(trials) {
  
  mt_tableset(1)
  
  mt_away_boxscore_tot <<- matrix(0, nrow = nrow(mc_away_stats), ncol = 24)
  colnames(mt_away_boxscore_tot) <- c("ID", "POS", "POSPCT", "FGM", "FGA", "FGPCT", "2PM", "2PA", "2PPCT", "3PM", "3PA", "3PPCT",
                                      "FTM", "FTA", "FTPCT", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PFL", "PTS")
  
  mt_away_boxscore_tot[, 1] <- mc_away_stats[, 1]
  mt_away_boxscore_blank <<- mt_away_boxscore_tot
  
  mt_home_boxscore_tot <<- matrix(0, nrow = nrow(mc_home_stats), ncol = 24)
  colnames(mt_home_boxscore_tot) <- c("ID", "POS", "POSPCT", "FGM", "FGA", "FGPCT", "2PM", "2PA", "2PPCT", "3PM", "3PA", "3PPCT",
                                      "FTM", "FTA", "FTPCT", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PFL", "PTS")
  
  mt_home_boxscore_tot[, 1] <- mc_home_stats[, 1]
  mt_home_boxscore_blank <<- mt_home_boxscore_tot
  
  mt_margin_slotter <<- rep(0, trials)
  mt_total_slotter <<- rep(0, trials)
  mt_win_slotter <<- rep(0, trials)
  mt_ascore_slotter <<- rep(0, trials)
  mt_hscore_slotter <<- rep(0, trials)
  
  mt_awayscore_tot <<- 0
  mt_homescore_tot <<- 0
  mt_tempo_tot <<- 0
  mc_trials <<- trials
  
  mt_cycle_do(trials)
  
}

mt_cycle_do <- function(trials) {
  
  a <- 0
  g <- trials
  ot_switch <<- 0
  ot_possessions_tot <<- 0
  
  mc_away_score <<- 0
  mc_home_score <<- 0
  
  mt_away_boxscore <<- mt_away_boxscore_blank
  mt_home_boxscore <<- mt_home_boxscore_blank
  
  if (r <= tempo_flank) { game_possessions <<- tempo_integer + 1 }
  else { game_possessions <<- tempo_integer }
  original_possessions <<- game_possessions
  
  while (a < g) {
    
    
    r <<- runif(1)
    
    if (ot_switch == 1) { game_possessions <<- overtime_possessions }
    #mc_pos_rem_amx <<- NULL
    #mc_pos_rem_tmx <<- NULL
    b <- 1
    for (b in b:game_possessions) {
      
      mt_waterfall(1)
      mt_possession_away(1)
      mt_possession_home(1)
      pos_rem_full <<- game_possessions - b 
    }
    
    if (mc_away_score != mc_home_score) { 
      a <- a + 1 
      ot_switch <<- 0
      mt_away_boxscore_tot <<- mt_away_boxscore_tot + mt_away_boxscore
      mt_home_boxscore_tot <<- mt_home_boxscore_tot + mt_home_boxscore
      mt_awayscore_tot <<- mc_away_score + mt_awayscore_tot
      mt_homescore_tot <<- mc_home_score + mt_homescore_tot
      mt_tempo_tot <<- mt_tempo_tot + original_possessions + ot_possessions_tot
      ot_possessions_tot <<- 0
      mt_margin_slotter[a] <<- mc_away_score - mc_home_score
      mt_total_slotter[a] <<- (mc_away_score + mc_home_score)
      mt_ascore_slotter[a] <<- mc_away_score
      mt_hscore_slotter[a] <<- mc_home_score
      if (mc_away_score > mc_home_score) { mt_win_slotter[a] <<- 1 }

      mc_away_score <<- 0
      mc_home_score <<- 0
      
      mt_away_boxscore <<- mt_away_boxscore_blank
      mt_home_boxscore <<- mt_home_boxscore_blank
      
      if (r <= tempo_flank) { game_possessions <<- tempo_integer + 1 }
      else { game_possessions <<- tempo_integer }
      original_possessions <<- game_possessions
    }
    else { 
      ot_switch <<- 1
      overtime_possessions <<- game_possessions * .125
      ot_possessions_tot <<- overtime_possessions + ot_possessions_tot
    }
  }
  mt_finish(1)
}

mt_finish <- function(xxxx) {
  
  mt_avg_awayscore <<- mt_awayscore_tot / mc_trials
  mt_avg_homescore <<- mt_homescore_tot / mc_trials
  mt_home_boxscore_avg <<- mt_home_boxscore_tot / mc_trials
  mt_away_boxscore_avg <<- mt_away_boxscore_tot / mc_trials
  mt_avg_tempo <<- mt_tempo_tot / mc_trials

}

#--------------------
#LIVE MONTE CARLO
#--------------------

mc_secsleft <<- function(h, m, s) {
  
  mc_time <<- 2400
  if (h > 1) { mc_time <<- mc_time - 1200 }
  mins_rem <<- 19 - m
  secs_rem <<- 60 - s
  return(mc_time - (mins_rem * 60) - (secs_rem))
  
}

mc_live <- function(c_gid, a_cscore, h_cscore, c_half, c_mins, c_secs) {
  
  stage_spread <<- as.numeric(formatted_wpt[c_gid, 4]) * -1
  stage_aml <<- as.numeric(formatted_wpt[c_gid, 6])
  stage_hml <<- as.numeric(formatted_wpt[c_gid, 7])
  stage_total <<- as.numeric(formatted_wpt[c_gid, 5])
  stage_loc <<- as.character(formatted_wpt[c_gid, 8])
  if (stage_loc == "@") { stage_loc <<- "H" }
  else { stage_loc <<- "N" }
  stage_aname <<- as.character(formatted_wpt[c_gid, 11])
  stage_hname <<- as.character(formatted_wpt[c_gid, 12])
  stage_aid <<- as.numeric(names_file[which(names_file[, 1] == stage_aname), 3])
  stage_hid <<- as.numeric(names_file[which(names_file[, 1] == stage_hname), 3])
  
  mc_away_id <<- stage_aid
  mc_home_id <<- stage_hid
  mc_loc <<- stage_loc
  
  secs_left <<- mc_secsleft(c_half, c_mins, c_secs)
  pct_rem <<- secs_left / 2400
  
  print(paste("LIVE Staging:", stage_loc, stage_aname, stage_hname, pct_rem, "Remaining", sep = " "))
  
  mc_live_do(num_trials, a_cscore, h_cscore, pct_rem)
  
  print(paste("Result:", stage_aname, avg_awayscore, "-", stage_hname, avg_homescore, sep = " "))
  
  avg_margin <<- avg_awayscore - avg_homescore
  avg_total <<- avg_awayscore + avg_homescore
  cover_vector <<- mc_margin_slotter - stage_spread
  a_cover_pct <<- sum(cover_vector > 0) / num_trials
  h_cover_pct <<- sum(cover_vector < 0) / num_trials
  p_cover_pct <<- sum(cover_vector == 0) / num_trials
  
  true_a_cover_pct <<- (a_cover_pct + (p_cover_pct / 2))
  true_h_cover_pct <<- (h_cover_pct + (p_cover_pct / 2))
  
  over_pct <<- sum(mc_total_slotter > stage_total) / num_trials
  under_pct <<- sum(mc_total_slotter < stage_total) / num_trials
  totpush_pct <<- sum(mc_total_slotter == stage_total) / num_trials
  
  true_over_pct <<- (over_pct + (totpush_pct / 2))
  true_under_pct <<- (under_pct + (totpush_pct / 2))
  
  a_win_pct <<- sum(mc_win_slotter == 1) / num_trials
  h_win_pct <<- sum(mc_win_slotter == 0) / num_trials
  
  a_ml_conv <<- moneyline_conversion(stage_aml)
  h_ml_conv <<- moneyline_conversion(stage_hml)
  
  live_df <<- data.frame(avg_awayscore, avg_homescore)
  live_df[2, ] <<- c(avg_margin, avg_total)
  live_df[3, ] <<- c(a_win_pct, h_win_pct)
  live_df[4, ] <<- c(a_cover_pct, h_cover_pct)
  live_df[5, ] <<- c(over_pct, under_pct)
  
  colnames(live_df) <<- c("AWAY", "HOME")
  
  write_csv(live_df, "livebetting.csv")

} 

mc_live_do <- function(trials, la_score, lh_score, pctrem) {
  
  mc_tableset(pctrem)
  mc_roster_select(1)
  mc_shoot_pcts(1)
  
  mc_away_boxscore_tot <<- matrix(0, nrow = nrow(mc_away_stats), ncol = 24)
  colnames(mc_away_boxscore_tot) <- c("ID", "POS", "POSPCT", "FGM", "FGA", "FGPCT", "2PM", "2PA", "2PPCT", "3PM", "3PA", "3PPCT",
                                      "FTM", "FTA", "FTPCT", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PFL", "PTS")
  
  mc_away_boxscore_tot[, 1] <- mc_away_stats[, 1]
  mc_away_boxscore_blank <<- mc_away_boxscore_tot
  
  mc_home_boxscore_tot <<- matrix(0, nrow = nrow(mc_home_stats), ncol = 24)
  colnames(mc_home_boxscore_tot) <- c("ID", "POS", "POSPCT", "FGM", "FGA", "FGPCT", "2PM", "2PA", "2PPCT", "3PM", "3PA", "3PPCT",
                                      "FTM", "FTA", "FTPCT", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TOV", "PFL", "PTS")
  
  mc_home_boxscore_tot[, 1] <- mc_home_stats[, 1]
  mc_home_boxscore_blank <<- mc_home_boxscore_tot
  
  mc_margin_slotter <<- rep(0, trials)
  mc_total_slotter <<- rep(0, trials)
  mc_win_slotter <<- rep(0, trials)
  mc_ascore_slotter <<- rep(0, trials)
  mc_hscore_slotter <<- rep(0, trials)
  
  mc_awayscore_tot <<- 0
  mc_homescore_tot <<- 0
  mc_tempo_tot <<- 0
  mc_trials <<- trials
  orb_test <<- 0
  
  mc_cycle_do_live(trials, la_score, lh_score)
  
}

mc_cycle_do_live <- function(trials, la_score, lh_score) {
  
  a <- 0
  g <- trials
  ot_switch <<- 0
  ot_possessions_tot <<- 0
  
  mc_away_score <<- la_score
  mc_home_score <<- lh_score
  
  mc_away_boxscore <<- mc_away_boxscore_blank
  mc_home_boxscore <<- mc_home_boxscore_blank
  
  if (r <= tempo_flank) { game_possessions <<- tempo_integer + 1 }
  else { game_possessions <<- tempo_integer }
  original_possessions <<- game_possessions
  
  while (a < g) {
    
    
    r <<- runif(1)
    
    if (ot_switch == 1) { game_possessions <<- overtime_possessions }
    #mc_pos_rem_amx <<- NULL
    #mc_pos_rem_tmx <<- NULL
    b <- 1
    for (b in b:game_possessions) {
      
      mc_floor_shake_2(1)
      mc_exp_generate(1)
      mc_waterfall(1)
      mc_possession_away(1)
      mc_possession_home(1)
      pos_rem_full <<- game_possessions - b 
      #if (is.null(mc_pos_rem_amx) == TRUE) { mc_pos_rem_amx <<- matrix(c(pos_rem_full, (mc_away_score - mc_home_score), 0, 0), nrow = 1, ncol = 4) }
      #else { 
      #mc_pos_rem_amx3 <<- matrix(c(pos_rem_full, (mc_away_score - mc_home_score), 0, 0), nrow = 1, ncol = 4) 
      #mc_pos_rem_amx <<- rbind(mc_pos_rem_amx, mc_pos_rem_amx3) 
      #}
      #if (is.null(mc_pos_rem_tmx) == TRUE) { mc_pos_rem_tmx <<- matrix(c(pos_rem_full, (mc_home_score + mc_away_score), 0), nrow = 1, ncol = 3) }
      #else { 
      #mc_pos_rem_tmx3 <<- matrix(c(pos_rem_full, (mc_home_score + mc_away_score), 0), 0, nrow = 1, ncol = 3) 
      #mc_pos_rem_tmx <<- rbind(mc_pos_rem_tmx, mc_pos_rem_tmx3) 
      #}
    }
    
    if (mc_away_score != mc_home_score) { 
      a <- a + 1 
      ot_switch <<- 0
      mc_away_boxscore_tot <<- mc_away_boxscore_tot + mc_away_boxscore
      mc_home_boxscore_tot <<- mc_home_boxscore_tot + mc_home_boxscore
      mc_awayscore_tot <<- mc_away_score + mc_awayscore_tot
      mc_homescore_tot <<- mc_home_score + mc_homescore_tot
      mc_tempo_tot <<- mc_tempo_tot + original_possessions + ot_possessions_tot
      ot_possessions_tot <<- 0
      mc_margin_slotter[a] <<- mc_away_score - mc_home_score
      mc_total_slotter[a] <<- (mc_away_score + mc_home_score)
      mc_ascore_slotter[a] <<- mc_away_score
      mc_hscore_slotter[a] <<- mc_home_score
      if (mc_away_score > mc_home_score) { mc_win_slotter[a] <<- 1 }
      
      
      #if ((mc_away_score - stage_spread) > mc_home_score) { mc_pos_rem_amx[, 3] <<- 1 }
      #else if ((mc_away_score - stage_spread) < mc_home_score) { mc_pos_rem_amx[, 3] <<- 0 }
      #else { mc_pos_rem_amx[, 3] <<- .5 }
      
      #if (mc_away_score > mc_home_score) { mc_pos_rem_amx[, 4] <<- 1 }
      #else { mc_pos_rem_amx[, 4] <<- 0 }
      
      #if ((mc_away_score + mc_home_score) > stage_total) { mc_pos_rem_tmx[, 3] <<- 1 }
      #else if ((mc_away_score + mc_home_score) < stage_total) { mc_pos_rem_tmx[, 3] <<- 0 }
      #else { mc_pos_rem_tmx[, 3] <<- .5 }
      
      #if (a == 1) { 
      #mc_master_amx <<- mc_pos_rem_amx 
      #mc_master_tmx <<- mc_pos_rem_tmx
      #}
      #else {
      #mc_master_amx <<- rbind(mc_master_amx, mc_pos_rem_amx)
      #mc_master_tmx <<- rbind(mc_master_tmx, mc_pos_rem_tmx)
      #}
      
      mc_away_score <<- la_score
      mc_home_score <<- lh_score
      
      mc_away_boxscore <<- mc_away_boxscore_blank
      mc_home_boxscore <<- mc_home_boxscore_blank
      
      if (r <= tempo_flank) { game_possessions <<- tempo_integer + 1 }
      else { game_possessions <<- tempo_integer }
      original_possessions <<- game_possessions
    }
    else { 
      ot_switch <<- 1
      overtime_possessions <<- game_possessions * .125
      ot_possessions_tot <<- overtime_possessions + ot_possessions_tot
    }
  }
  mc_finish_live(1)
}

mc_finish_live <- function(xxxx) {
  
  avg_awayscore <<- mc_awayscore_tot / mc_trials
  avg_homescore <<- mc_homescore_tot / mc_trials
  mc_home_boxscore_avg <<- mc_home_boxscore_tot / mc_trials
  mc_away_boxscore_avg <<- mc_away_boxscore_tot / mc_trials
  avg_tempo <<- mc_tempo_tot / mc_trials

}

mc_live_check <- function(xxxx) {
  
  mc_live_updater <<- read_csv("livedata.csv", col_names = FALSE)
  
  c_check <<- as.numeric(colSums((mc_live_updater == mc_live_use)))
  print(c_check)
  if (c_check < 6) { 
    
    mc_live_use <<- mc_live_updater
    p1 <- as.numeric(mc_live_use[1, 1])
    p2 <- as.numeric(mc_live_use[2, 1])
    p3 <- as.numeric(mc_live_use[3, 1])
    p4 <- as.numeric(mc_live_use[4, 1])
    p5 <- as.numeric(mc_live_use[5, 1])
    p6 <- as.numeric(mc_live_use[6, 1])
    mc_live(p1, p2, p3, p4, p5, p6)
    beep()
    
  }
}

#-----------------------------
#SIMPLE
#-----------------------------

simple_predict <- function(xxxx) {
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  sim_away_oe <<- as.numeric(mc_away_teamstats[1, 1])
  sim_away_de <<- as.numeric(mc_away_teamstats[1, 16])
  sim_away_te <<- as.numeric(mc_away_teamstats[1, 35])
  
  sim_home_oe <<- as.numeric(mc_home_teamstats[1, 1])
  sim_home_de <<- as.numeric(mc_home_teamstats[1, 16])
  sim_home_te <<- as.numeric(mc_home_teamstats[1, 35])
  
  nat_tempo <<- nat_avg_mx[1]
  nat_oe <<- nat_avg_mx[2]
  
  away_tempo_vsna <<- sim_away_te - nat_tempo
  home_tempo_vsna <<- sim_home_te - nat_tempo
  vsna_sums <<- away_tempo_vsna + home_tempo_vsna
  
  if (xxxx == 2) { exp_tempo <<- (avg_tempo + mt_avg_tempo) / 2 }
  else { exp_tempo <<- (nat_tempo + vsna_sums) }
  
  away_oe_vsna <<- sim_away_oe - nat_oe
  away_de_vsna <<- sim_away_de - nat_oe
  home_oe_vsna <<- sim_home_oe - nat_oe
  home_de_vsna <<- sim_home_de - nat_oe
  
  away_oe_vsnasums <<- away_oe_vsna + home_de_vsna
  home_oe_vsnasums <<- home_oe_vsna + away_de_vsna
  
  away_proj_oe <<- nat_oe + away_oe_vsnasums
  home_proj_oe <<- nat_oe + home_oe_vsnasums
  
  if (mc_loc == "A" | mc_loc == "H") {
    
    away_proj_oe <<- away_proj_oe * as.numeric(advantage_mx[2, 3])
    home_proj_oe <<- home_proj_oe * as.numeric(advantage_mx[1, 3])
    
  }
  
  away_simp_score <<- away_proj_oe * exp_tempo
  home_simp_score <<- home_proj_oe * exp_tempo
  
  away_simp_winpct <<- ((away_proj_oe ^ 10.25)) / ((away_proj_oe ^ 10.25) + (home_proj_oe ^ 10.25))
  home_simp_winpct <<- 1 - away_simp_winpct
}

#-----------------------------
#BLENDER
#-----------------------------

blender_predict_old <- function(xxxx)  {
  
  blender_control <<- 0.585
  blender_wontrol <<- 0.02
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  mc_away_teamstats_cn <<- colnames(mc_away_teamstats)
  a <- 1
  g <- length(mc_away_teamstats_cn)
  
  for (a in a:g) {
    
    colname_tmp <- mc_away_teamstats_cn[a]
    colname_paste <- paste(colname_tmp, ".x", sep = "")
    if (a == 1) { colname_vec <- colname_paste }
    else { colname_vec <- c(colname_vec, colname_paste) }
    
  }
  
  colname_vec_a <- colname_vec
  
  mc_home_teamstats_cn <<- colnames(mc_home_teamstats)
  a <- 1
  g <- length(mc_home_teamstats_cn)
  
  for (a in a:g) {
    
    colname_tmp <- mc_home_teamstats_cn[a]
    colname_paste <- paste(colname_tmp, ".y", sep = "")
    if (a == 1) { colname_vec <- colname_paste }
    else { colname_vec <- c(colname_vec, colname_paste) }
    
  }
  
  colname_vec_h <- colname_vec
  
  mc_away_teamstats2 <<- mc_away_teamstats
  mc_home_teamstats2 <<- mc_home_teamstats
  
  colnames(mc_away_teamstats2) <<- colname_vec_a
  colnames(mc_home_teamstats2) <<- colname_vec_h
  
  bln_input <<- as.data.frame(cbind(mc_away_teamstats2, mc_home_teamstats2))
  
  if (mc_loc == "A" | mc_loc == "H") { 
    
    blender_awin_pct <<- (as.numeric(predict(winpct_ah_fit, newdata = bln_input, type = "response"))) + blender_wontrol
    blender_hwin_pct <<- 1 - blender_awin_pct
    
    blender_proj_marg <<- as.numeric(predict(marg_ah_fit, newdata = bln_input, type = "response")) + (blender_control * 2)
    blender_proj_ascore <<- as.numeric(predict(score_aa_fit, newdata = bln_input, type = "response")) + blender_control
    blender_proj_hscore <<- as.numeric(predict(score_ah_fit, newdata = bln_input, type = "response")) - blender_control
    blender_proj_total <<- as.numeric(predict(total_ah_fit, newdata = bln_input, type = "response"))
    
    
  }
  else {
    blender_awin_pct <<- as.numeric(predict(winpct_na_fit, newdata = bln_input, type = "response"))
    blender_hwin_pct <<- 1 - blender_awin_pct
    
    blender_proj_marg <<- as.numeric(predict(marg_nn_fit, newdata = bln_input, type = "response"))
    blender_proj_ascore <<- as.numeric(predict(score_na_fit, newdata = bln_input, type = "response"))
    blender_proj_hscore <<- as.numeric(predict(score_nh_fit, newdata = bln_input, type = "response"))
    blender_proj_total <<- as.numeric(predict(total_nn_fit, newdata = bln_input, type = "response"))
    
  }
}


blender_predict <- function(xxxx)  {
  
  blender_control <<- 2.36
  blender_wontrol <<- .933
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  mc_away_teamstats_cn <<- colnames(mc_away_teamstats)
  a <- 1
  g <- length(mc_away_teamstats_cn)
  
  for (a in a:g) {
    
    colname_tmp <- mc_away_teamstats_cn[a]
    colname_paste <- paste(colname_tmp, ".x", sep = "")
    if (a == 1) { colname_vec <- colname_paste }
    else { colname_vec <- c(colname_vec, colname_paste) }
    
  }
  
  colname_vec_a <- colname_vec
  
  mc_home_teamstats_cn <<- colnames(mc_home_teamstats)
  a <- 1
  g <- length(mc_home_teamstats_cn)
  
  for (a in a:g) {
    
    colname_tmp <- mc_home_teamstats_cn[a]
    colname_paste <- paste(colname_tmp, ".y", sep = "")
    if (a == 1) { colname_vec <- colname_paste }
    else { colname_vec <- c(colname_vec, colname_paste) }
    
  }
  
  colname_vec_h <- colname_vec
  
  mc_away_teamstats2 <<- mc_away_teamstats
  mc_home_teamstats2 <<- mc_home_teamstats
  
  colnames(mc_away_teamstats2) <<- colname_vec_a
  colnames(mc_home_teamstats2) <<- colname_vec_h
  
  bln_input <<- as.data.frame(cbind(mc_away_teamstats2, mc_home_teamstats2))

  blender_proj_marg <<- as.numeric(predict(blender_ats_use, newdata = bln_input, type = "response"))
  blender_awin_pct <<- as.numeric(predict(blender_ml_use, newdata = bln_input, type = "response"))
  blender_hwin_pct <<- 1 - blender_awin_pct
  blender_proj_total <<- as.numeric(predict(blender_total_use, newdata = bln_input, type = "response"))
  
  print("BLEND")
  #print(blender_awin_pct)
  
  if (mc_loc == "A" | mc_loc == "H") { 
    
    blender_proj_marg <<- blender_proj_marg - blender_control
    if (blender_awin_pct > blender_hwin_pct) {
      blender_hwin_pct <<- blender_hwin_pct * ((1 - blender_wontrol) + 1)
      blender_awin_pct <<- 1 - blender_hwin_pct
    }
    else {
      blender_awin_pct <<- blender_awin_pct * blender_wontrol
      blender_hwin_pct <<- 1 - blender_awin_pct
    }
  }
  #print(blender_awin_pct)
  blender_proj_ascore <<- (blender_proj_total / 2) + (blender_proj_marg / 2)
  blender_proj_hscore <<- (blender_proj_total / 2) - (blender_proj_marg / 2)
}



rf_predict <- function(xxxx)  {
  
  blender_control <<- 2.36
  blender_wontrol <<- .933
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  mc_away_teamstats_cn <<- colnames(mc_away_teamstats)
  a <- 1
  g <- length(mc_away_teamstats_cn)
  
  for (a in a:g) {
    
    colname_tmp <- mc_away_teamstats_cn[a]
    colname_paste <- paste(colname_tmp, ".x", sep = "")
    if (a == 1) { colname_vec <- colname_paste }
    else { colname_vec <- c(colname_vec, colname_paste) }
    
  }
  
  colname_vec_a <- colname_vec
  
  mc_home_teamstats_cn <<- colnames(mc_home_teamstats)
  a <- 1
  g <- length(mc_home_teamstats_cn)
  
  for (a in a:g) {
    
    colname_tmp <- mc_home_teamstats_cn[a]
    colname_paste <- paste(colname_tmp, ".y", sep = "")
    if (a == 1) { colname_vec <- colname_paste }
    else { colname_vec <- c(colname_vec, colname_paste) }
    
  }
  
  colname_vec_h <- colname_vec
  
  mc_away_teamstats2 <<- mc_away_teamstats
  mc_home_teamstats2 <<- mc_home_teamstats
  
  colnames(mc_away_teamstats2) <<- colname_vec_a
  colnames(mc_home_teamstats2) <<- colname_vec_h
  
  bln_input <<- as.data.frame(cbind(mc_away_teamstats2, mc_home_teamstats2))
  
  rf_proj_marg <<- as.numeric(predict(zz_ats_use, newdata = bln_input))
  rf_awin_pct <<- predict(zzz_ml_use, newdata = bln_input, type = "prob")
  rf_awin_pct <<- rf_awin_pct[2]
  rf_hwin_pct <<- 1 - rf_awin_pct
  rf_proj_total <<- as.numeric(predict(zzz_total_use, newdata = bln_input))
  #print(rf_awin_pct)
  
  if (mc_loc == "A" | mc_loc == "H") { 
    
    rf_proj_marg <<- rf_proj_marg - blender_control
    if (rf_awin_pct > rf_hwin_pct) {
      rf_hwin_pct <<- rf_hwin_pct * ((1 - blender_wontrol) + 1)
      rf_awin_pct <<- 1 - rf_hwin_pct
    }
    else {
      rf_awin_pct <<- rf_awin_pct * blender_wontrol
      rf_hwin_pct <<- 1 - rf_awin_pct
    }
  }
 # print(rf_awin_pct)

  rf_proj_ascore <<- (rf_proj_total / 2) + (rf_proj_marg / 2)
  rf_proj_hscore <<- (rf_proj_total / 2) - (rf_proj_marg / 2)
}

#-----------------------------
#ATM
#-----------------------------


atm_fingerprint <- function(xxxx) {
  
  atm_control <<- 0.585
  atm_wontrol <<- 0.02
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  atm_away_oe <<- as.numeric(mc_away_teamstats[1, 1])
  atm_away_de <<- as.numeric(mc_away_teamstats[1, 16])
  atm_away_te <<- as.numeric(mc_away_teamstats[1, 35])
  
  atm_home_oe <<- as.numeric(mc_home_teamstats[1, 1])
  atm_home_de <<- as.numeric(mc_home_teamstats[1, 16])
  atm_home_te <<- as.numeric(mc_home_teamstats[1, 35])
  
  atm_away_efg <<- as.numeric(mc_away_teamstats[1, 2])
  atm_away_orb <<- as.numeric(mc_away_teamstats[1, 8])
  atm_away_tov <<- as.numeric(mc_away_teamstats[1, 14])
  atm_away_srft <<- as.numeric(mc_away_teamstats[1, 33])
  atm_away_defg <<- as.numeric(mc_away_teamstats[1, 17])
  atm_away_dorb <<- as.numeric(mc_away_teamstats[1, 23])
  atm_away_dtov <<- as.numeric(mc_away_teamstats[1, 29])
  atm_away_dsrft <<- as.numeric(mc_away_teamstats[1, 34])
  
  atm_home_efg <<- as.numeric(mc_home_teamstats[1, 2])
  atm_home_orb <<- as.numeric(mc_home_teamstats[1, 8])
  atm_home_tov <<- as.numeric(mc_home_teamstats[1, 14])
  atm_home_srft <<- as.numeric(mc_home_teamstats[1, 33])
  atm_home_defg <<- as.numeric(mc_home_teamstats[1, 17])
  atm_home_dorb <<- as.numeric(mc_home_teamstats[1, 23])
  atm_home_dtov <<- as.numeric(mc_home_teamstats[1, 29])
  atm_home_dsrft <<- as.numeric(mc_home_teamstats[1, 34])
  
  if (mc_loc == "A" | mc_loc == "H") {
    
     atm_frame_use <<- master_reg_atm_amx
     atm_drame_use <<- master_reg_atm_adx
     
     #Tier 1
     
     atm_frame_tier1 <<- atm_frame_use[, c(5, 12, 19, 22, 29, 36)]
     atm_sdevs_tier1 <<- master_reg_sd[c(5, 12, 19, 22, 29, 36)]
     atm_vec_tier1 <<- c(atm_away_oe, atm_away_de, atm_away_te, atm_home_oe, atm_home_de, atm_home_te)
     a <- 1
     g <- ncol(atm_frame_tier1)
     
     for (a in a:g) {
      
       atm_ss <<- (abs(atm_frame_tier1[, a] - atm_vec_tier1[a])) / atm_sdevs_tier1[a]
       if (a == 1) { atm_tier1_comps <<- atm_ss }
       else { atm_tier1_comps <<- cbind(atm_tier1_comps, atm_ss) }
       
       
     }
     
     rm_tmp <- rowMeans(atm_tier1_comps)
     atm_tier1_comps <<- cbind(atm_tier1_comps, rm_tmp)
     atm_tier1_w <- which(atm_tier1_comps[, 7] <= .6)
     if (length(atm_tier1_w) < 10) {
       
       tail_atm1 <- head(sort(atm_tier1_comps[, 7]), 10)
       tail_atm1 <- tail_atm1[10]
       atm_tier1_w <- which(atm_tier1_comps[, 7] <= tail_atm1)
       
     }
     atm_tier1_filt <<- atm_frame_use[atm_tier1_w, ]
     atm_tier1_dilt <<- atm_drame_use[atm_tier1_w, ]
     
     #Tier 2
     
     atm_frame_tier2 <<- atm_tier1_filt[, c(6, 7, 10, 11, 13, 15, 17, 18, 23, 24, 27, 28, 30, 32, 34, 35)]
     atm_sdevs_tier2 <<- master_reg_sd[c(6, 7, 10, 11, 13, 15, 17, 18, 23, 24, 27, 28, 30, 32, 34, 35)]
     atm_vec_tier2 <<- c(atm_away_efg, atm_away_orb, atm_away_tov, atm_away_srft, atm_away_defg, atm_away_dorb, atm_away_dtov, atm_away_dsrft,
                         atm_home_efg, atm_home_orb, atm_home_tov, atm_home_srft, atm_home_defg, atm_home_dorb, atm_home_dtov, atm_home_dsrft)
     
     a <- 1
     g <- ncol(atm_frame_tier2)
     
     for (a in a:g) {
       
       atm_ss <<- (abs(atm_frame_tier2[, a] - atm_vec_tier2[a])) / atm_sdevs_tier2[a]
       if (a == 1) { atm_tier2_comps <<- atm_ss }
       else { atm_tier2_comps <<- cbind(atm_tier2_comps, atm_ss) }

     }
     
     rm_tmp <- rowMeans(atm_tier2_comps)
     atm_tier2_comps <<- cbind(atm_tier2_comps, rm_tmp)
     atm_tier2_w <- which(atm_tier2_comps[, 17] < .75)
     
     if (length(atm_tier2_w) < 5) {
       
       tail_atm <- head(sort(atm_tier2_comps[, 17]), 5)
       tail_atm <- tail_atm[5]
       atm_tier2_w <- which(atm_tier2_comps[, 17] <= tail_atm)
       
     }
     else { 
       tail_atm <- .75
      
    }
     atm_tier2_wr <- atm_tier2_comps[atm_tier2_w, 17]
     atm_tier2_filt <<- atm_tier1_filt[atm_tier2_w, ]
     atm_tier2_filt <<- cbind(atm_tier2_filt, atm_tier2_wr)
     atm_tier2_dilt <<- atm_tier1_dilt[atm_tier2_w, ]
     
     tier_2_weight <<- (8 - (tail_atm - (tail_atm - atm_tier2_filt[, 37])) * (7 / tail_atm))
     
     atm_tier2_filt <<- cbind(atm_tier2_filt, tier_2_weight)
     
     atm_results_frame1 <- atm_tier2_filt[, 1] * tier_2_weight
     atm_results_frame2 <- atm_tier2_filt[, 2] * tier_2_weight
     atm_results_frame3 <- atm_tier2_filt[, 4] * tier_2_weight
     
     atm_proj_ascore <<- (sum(atm_results_frame1) / sum(tier_2_weight)) + atm_control
     atm_proj_hscore <<- (sum(atm_results_frame2) / sum(tier_2_weight)) - atm_control
     atm_proj_tscore <<- sum(atm_results_frame3) / sum(tier_2_weight)
     
     atm_tier2_win <<- ifelse(atm_tier2_filt[, 1] > atm_tier2_filt[, 2], 1, 0)
     atm_results_frame4 <<- atm_tier2_win * tier_2_weight
     
     atm_proj_awinpct <<- (sum(atm_results_frame4) / sum(tier_2_weight)) + atm_wontrol
     if (atm_proj_awinpct > 1) { atm_proj_awinpct <<- 1 }
     atm_proj_hwinpct <<- 1 - atm_proj_awinpct
     
     
     
  }
  
  else {
    
    atm_frame_use <<- master_reg_atm_nmx
    atm_drame_use <<- master_reg_atm_ndx
    
    #Tier 1
    
    atm_frame_tier1 <<- atm_frame_use[, c(5, 12, 19, 22, 29, 36)]
    atm_sdevs_tier1 <<- master_reg_sd[c(5, 12, 19, 22, 29, 36)]
    atm_vec_tier1 <<- c(atm_away_oe, atm_away_de, atm_away_te, atm_home_oe, atm_home_de, atm_home_te)
    a <- 1
    g <- ncol(atm_frame_tier1)
    
    for (a in a:g) {
      
      atm_ss <<- (abs(atm_frame_tier1[, a] - atm_vec_tier1[a])) / atm_sdevs_tier1[a]
      if (a == 1) { atm_tier1_comps <<- atm_ss }
      else { atm_tier1_comps <<- cbind(atm_tier1_comps, atm_ss) }
      
      
    }
    
    rm_tmp <- rowMeans(atm_tier1_comps)
    atm_tier1_comps <<- cbind(atm_tier1_comps, rm_tmp)
    atm_tier1_w <- which(atm_tier1_comps[, 7] <= .6)
    if (length(atm_tier1_w) < 10) {
      
      tail_atm1 <- head(sort(atm_tier1_comps[, 7]), 10)
      tail_atm1 <- tail_atm1[10]
      atm_tier1_w <- which(atm_tier1_comps[, 7] <= tail_atm1)
      
    }
    atm_tier1_filt <<- atm_frame_use[atm_tier1_w, ]
    atm_tier1_dilt <<- atm_drame_use[atm_tier1_w, ]
    
    #Tier 2
    
    atm_frame_tier2 <<- atm_tier1_filt[, c(6, 7, 10, 11, 13, 15, 17, 18, 23, 24, 27, 28, 30, 32, 34, 35)]
    atm_sdevs_tier2 <<- master_reg_sd[c(6, 7, 10, 11, 13, 15, 17, 18, 23, 24, 27, 28, 30, 32, 34, 35)]
    atm_vec_tier2 <<- c(atm_away_efg, atm_away_orb, atm_away_tov, atm_away_srft, atm_away_defg, atm_away_dorb, atm_away_dtov, atm_away_dsrft,
                        atm_home_efg, atm_home_orb, atm_home_tov, atm_home_srft, atm_home_defg, atm_home_dorb, atm_home_dtov, atm_home_dsrft)
    
    a <- 1
    g <- ncol(atm_frame_tier2)
    
    for (a in a:g) {
      
      atm_ss <<- (abs(atm_frame_tier2[, a] - atm_vec_tier2[a])) / atm_sdevs_tier2[a]
      if (a == 1) { atm_tier2_comps <<- atm_ss }
      else { atm_tier2_comps <<- cbind(atm_tier2_comps, atm_ss) }
      
      
    }
    
    rm_tmp <- rowMeans(atm_tier2_comps)
    atm_tier2_comps <<- cbind(atm_tier2_comps, rm_tmp)
    atm_tier2_w <- which(atm_tier2_comps[, 17] < .75)
    
    if (length(atm_tier2_w) < 5) {
      
      tail_atm <- head(sort(atm_tier2_comps[, 17]), 5)
      tail_atm <- tail_atm[5]
      atm_tier2_w <- which(atm_tier2_comps[, 17] <= tail_atm)
      
    }
    else { 
      tail_atm <- .75
      
    }
    
    atm_tier2_wr <- atm_tier2_comps[atm_tier2_w, 17]
    atm_tier2_filt <<- atm_tier1_filt[atm_tier2_w, ]
    atm_tier2_filt <<- cbind(atm_tier2_filt, atm_tier2_wr)
    atm_tier2_dilt <<- atm_tier1_dilt[atm_tier2_w, ]
    
    a_dilt <<- which(atm_tier2_dilt[, 2] == "A")
    h_dilt <<- which(atm_tier2_dilt[, 2] == "H")
    
    a <- 1
    g <- length(a_dilt)
    
    for (a in a:g) {
      
      b <- a_dilt[a]
      atm_tier2_filt[b, 1] <<- atm_tier2_filt[b, 1] * 1.02
      atm_tier2_filt[b, 2] <<- atm_tier2_filt[b, 2] * .98
      
    }
    
    a <- 1
    g <- length(h_dilt)
    
    for (a in a:g) {
      
      b <- h_dilt[a]
      atm_tier2_filt[b, 1] <<- atm_tier2_filt[b, 1] * .98
      atm_tier2_filt[b, 2] <<- atm_tier2_filt[b, 2] * 1.02
      
    }
    
    tier_2_weight <<- (8 - (tail_atm - (tail_atm - atm_tier2_filt[, 37])) * (7 / tail_atm))
    
    atm_tier2_filt <<- cbind(atm_tier2_filt, tier_2_weight)
    
    atm_results_frame1 <- atm_tier2_filt[, 1] * tier_2_weight
    atm_results_frame2 <- atm_tier2_filt[, 2] * tier_2_weight
    atm_results_frame3 <- atm_tier2_filt[, 4] * tier_2_weight
    
    atm_proj_ascore <<- sum(atm_results_frame1) / sum(tier_2_weight)
    atm_proj_hscore <<- sum(atm_results_frame2) / sum(tier_2_weight)
    atm_proj_tscore <<- sum(atm_results_frame3) / sum(tier_2_weight)
    
    atm_tier2_win <<- ifelse(atm_tier2_filt[, 1] > atm_tier2_filt[, 2], 1, 0)
    atm_results_frame4 <<- atm_tier2_win * tier_2_weight
    
    atm_proj_awinpct <<- sum(atm_results_frame4) / sum(tier_2_weight)
    if (atm_proj_awinpct > 1) { atm_proj_awinpct <<- 1 }
    atm_proj_hwinpct <<- 1 - atm_proj_awinpct
    
    
  }
}



beep <- function(n = 3){
  for(i in seq(n)){
    system("rundll32 user32.dll,MessageBeep MB_ICONASTERISK")
    Sys.sleep(.5)
  }
}

run_check <- function (xxxx) { 
  .id <<- tcl("after", 3000, run_check) # after 1000 ms execute run() again
  mc_live_check(1)   # replace with your code
}

run_stop <- function(xxxx) {
  tcl("after", "cancel", .id)
}


#-----------------NCAA TOURNEY------------------------

#ROUND ROBIN PROJ

round_robin <- function(x) {
  
  rrtable <- read_csv('hypomatch.csv')
  
  a <- 1
  g <- nrow(rrtable)
  c <- 1
  
  for (a in a:g) {
    
    tma <- as.character(rrtable[a, 1])
    tmb <- as.character(rrtable[a, 2])
    tmaid <- as.numeric(names_csv[which(names_csv$`Ken Pom` == tma), 3])
    tmbid <- as.numeric(names_csv[which(names_csv$`Ken Pom` == tmb), 3])
    print(c(a, g, tma, tmb))
    mc_loc <<- "N"
    mc_away_id <<- tmaid
    mc_home_id <<- tmbid
    blender_predict(1)
    rf_predict(1)
    simple_predict(1)
    
    avg_awinpct <- away_simp_winpct
    avg_hwinpct <- home_simp_winpct
    avg_ascore <- (blender_proj_ascore + rf_proj_ascore) / 2
    avg_hscore <- (blender_proj_hscore + rf_proj_hscore) / 2
    avg_total <- (blender_proj_total + rf_proj_total) / 2
    

    rfa_out <- data.frame(tmaid, tma, tmbid, tmb, blender_proj_ascore, blender_proj_total,
                           rf_proj_ascore, rf_proj_total, avg_awinpct, avg_ascore, avg_total,
                          blender_proj_hscore, rf_proj_hscore, avg_hwinpct, avg_hscore)
    
    rfh_out <- data.frame(tmbid, tmb, tmaid, tma, blender_proj_hscore, blender_proj_total,
                          rf_proj_hscore, rf_proj_total, avg_hwinpct, avg_hscore, avg_total,
                          blender_proj_ascore, rf_proj_ascore, avg_awinpct, avg_ascore)
    
    colnames(rfa_out) <- c("TEAMID", "TEAM", "OPPID", "OPP", "BLNDSCORE", "BLNDTOTAL", "RFSCORE",
                           "RFTOTAL", "AVGWINPCT", "AVGSCORE", "AVGTOTAL", "OBLNDSCORE", "ORFSCORE", "OWINPCT", "OAVGSCORE")
    
    colnames(rfh_out) <- c("TEAMID", "TEAM", "OPPID", "OPP" , "BLNDSCORE", "BLNDTOTAL", "RFSCORE",
                           "RFTOTAL", "AVGWINPCT", "AVGSCORE", "AVGTOTAL", "OBLNDSCORE", "ORFSCORE", "OWINPCT", "OAVGSCORE")
    
    if (c == 1) { 
      rf_out <- rbind(rfa_out, rfh_out)
      c <- c + 1
    }
    else {
      
      rf_out2 <- rbind(rfa_out, rfh_out)
      rf_out <- rbind(rf_out, rf_out2)
      
    }
  }
  
  View(rf_out)
  write_csv(rf_out, "ncaarrtable.csv")
}

bracket_mc <- function(x) {
  
  bracket_form <- read_csv('bracketst.csv')
  bracket_robin <- read_csv('ncaarrtable.csv')
  bracket_output <- matrix(0, nrow = 68, ncol = 8)
  conf_read <- read_csv('NCAACONF.csv')
  
  info_matrix <- as.matrix(conf_read[, 1])
  trial_matrix <- matrix(0, nrow = 68, ncol = x)
  wins_matrix <- trial_matrix
  sweet16_matrix <- trial_matrix
  champ_matrix <- trial_matrix
  final_four_matrix <- trial_matrix
  
  View(info_matrix)

  y <- 1
  
  for (y in y:x) {
    
    bracket_form2 <- bracket_form
    bracket_output2 <- matrix(0, nrow = 68, ncol = 8)
    print(y)
    
    a <- 1
    g <- nrow(bracket_form) - 1
    
    for (a in a:g) {
      
      round_ <- as.numeric(bracket_form[a, 2])
      team1 <- as.character(bracket_form[a, 3])
      team2 <- as.character(bracket_form[a, 4])
      feed_ <- as.numeric(bracket_form[a, 5])
      slot_ <- as.numeric(bracket_form[a, 6]) + 2
      seed1 <- as.character(bracket_form[a, 7])
      seed2 <- as.character(bracket_form[a, 8])
      
      mx_slot1 <- which(info_matrix[, 1] == team1)
      mx_slot2 <- which(info_matrix[, 1] == team2)
      
      robin_which <- which(bracket_robin$TEAM == team1 & bracket_robin$OPP == team2)
      team1_winpct <- as.numeric(bracket_robin[robin_which, 9])
      
      r <- runif(1)
      
      if (r <= team1_winpct) {
        
        bracket_form[feed_, slot_] <- team1
        bracket_form[feed_, (slot_ + 4)] <- seed1
        bracket_form[a, 9] <- team2
        bracket_form[a, 10] <- seed2
        
        wins_matrix[mx_slot1, y] <- wins_matrix[mx_slot1, y] + 1
        if (round_ == 3) { sweet16_matrix[mx_slot1, y] <- 1 }
        if (round_ == 5) { final_four_matrix[mx_slot1, y] <- 1 }
        if (round_ == 7) { champ_matrix[mx_slot1, y] <- 1 }
        
      }
      
      else {
        
        bracket_form[feed_, slot_] <- team2
        bracket_form[feed_, (slot_ + 4)] <- seed2
        bracket_form[a, 9] <- team1
        bracket_form[a, 10] <- seed1
        
        wins_matrix[mx_slot2, y] <- wins_matrix[mx_slot2, y] + 1
        if (round_ == 3) { sweet16_matrix[mx_slot2, y] <- 1 }
        if (round_ == 5) { final_four_matrix[mx_slot2, y] <- 1 }
        if (round_ == 7) { champ_matrix[mx_slot2, y] <- 1 }
        
      }
    }
    
    bracket_form[(g + 1), 9] <- as.character(bracket_form[(g + 1), 3])
    bracket_form[(g + 1), 10] <- as.character(bracket_form[(g + 1), 7])
    bracket_peak <- bracket_form %>%
      group_by(V9) %>%
      summarize(PEAK = max(ROUND)) %>%
      arrange(V9)
    
    bracket_peak2 <- as.matrix(bracket_peak[, 2])
    
    a <- 1
    g <- nrow(bracket_form)
    
    for (a in a:g) {
      
      pk <- as.numeric(bracket_peak2[a, 1])
      whole <- rep(0, 8)
      half1 <- rep(1, pk)
      whole[1:pk] <- half1
      bracket_output2[a, ] <- whole
  
    }
    
    bracket_output <- bracket_output + bracket_output2
  }
  bracket_output <- bracket_output / x
  bracket_helper <- bracket_form[, 9:10] %>%
    arrange(V9)
  
  bracket_output <- as.data.frame(bracket_output)
  bracket_output <- cbind(bracket_helper, bracket_output)
  
  a <- 1
  g <- nrow(bracket_output)
  
  for (a in a:g) {
    
    st_ <- bracket_output[a, 2]
    split_ <- strsplit(st_, split = "i")
    reg_ <- as.numeric(split_[[1]][1])
    seed_ <- as.numeric(split_[[1]][2])
    bracket_output[a, 11] <- reg_
    bracket_output[a, 12] <- seed_
    
  }

  colnames(bracket_output) <- c("TEAM", "SEEDE", "FIRSTFOUR", "R64", "R32", "S16", "E8", "F4", "RUNNERUP", "CHAMP", "REGION", "SEED")
  bracket_output <- bracket_output[, c(-2, -3)]
  bracket_output <- bracket_output[, c(9:10, 1:8)]
  write_csv(bracket_output, "bracketoutput.csv")
  champ_matrix <- cbind(conf_read, as.data.frame(champ_matrix))
  wins_matrix <- cbind(conf_read, as.data.frame(wins_matrix))
  final_four_matrix <- cbind(conf_read, as.data.frame(final_four_matrix))
  sweet16_matrix <- cbind(conf_read, as.data.frame(sweet16_matrix))
  write_csv(champ_matrix, 'champ_matrix.csv')
  write_csv(wins_matrix, 'wins_matrix.csv')
  write_csv(final_four_matrix, 'final_four_matrix.csv')
  write_csv(sweet16_matrix, 'sweet16_matrix.csv')
} 


bracket_mc_16 <- function(x) {
  
  bracket_form <- read_csv('bracketst16.csv')
  bracket_robin <- read_csv('ncaarrtable.csv')
  bracket_output <- matrix(0, nrow = 16, ncol = 5)
  
  y <- 1
  
  for (y in y:x) {
    
    bracket_form2 <- bracket_form
    bracket_output2 <- matrix(0, nrow = 16, ncol = 5)
    print(y)
    
    a <- 1
    g <- nrow(bracket_form) - 1
    
    for (a in a:g) {
      
      round_ <- as.numeric(bracket_form[a, 2])
      team1 <- as.character(bracket_form[a, 3])
      team2 <- as.character(bracket_form[a, 4])
      feed_ <- as.numeric(bracket_form[a, 5])
      slot_ <- as.numeric(bracket_form[a, 6]) + 2
      seed1 <- as.character(bracket_form[a, 7])
      seed2 <- as.character(bracket_form[a, 8])
      
      robin_which <- which(bracket_robin$TEAM == team1 & bracket_robin$OPP == team2)
      team1_winpct <- as.numeric(bracket_robin[robin_which, 9])
      
      r <- runif(1)
      
      if (r <= team1_winpct) {
        
        bracket_form[feed_, slot_] <- team1
        bracket_form[feed_, (slot_ + 4)] <- seed1
        bracket_form[a, 9] <- team2
        bracket_form[a, 10] <- seed2
        
      }
      
      else {
        
        bracket_form[feed_, slot_] <- team2
        bracket_form[feed_, (slot_ + 4)] <- seed2
        bracket_form[a, 9] <- team1
        bracket_form[a, 10] <- seed1
        
      }
    }
    
    bracket_form[(g + 1), 9] <- as.character(bracket_form[(g + 1), 3])
    bracket_form[(g + 1), 10] <- as.character(bracket_form[(g + 1), 7])
    bracket_peak <- bracket_form %>%
      group_by(V9) %>%
      summarize(PEAK = max(ROUND)) %>%
      arrange(V9)
    
    bracket_peak2 <- as.matrix(bracket_peak[, 2])
    
    a <- 1
    g <- nrow(bracket_form)
    
    for (a in a:g) {
      
      pk <- as.numeric(bracket_peak2[a, 1])
      whole <- rep(0, 5)
      half1 <- rep(1, pk)
      whole[1:pk] <- half1
      bracket_output2[a, ] <- whole
      
    }
    
    bracket_output <- bracket_output + bracket_output2
  }
  bracket_output <- bracket_output / x
  bracket_helper <- bracket_form[, 9:10] %>%
    arrange(V9)
  
  bracket_output <- as.data.frame(bracket_output)
  bracket_output <- cbind(bracket_helper, bracket_output)
  
  a <- 1
  g <- nrow(bracket_output)
  
  for (a in a:g) {
    
    st_ <- bracket_output[a, 2]
    split_ <- strsplit(st_, split = "i")
    reg_ <- as.numeric(split_[[1]][1])
    seed_ <- as.numeric(split_[[1]][2])
    bracket_output[a, 8] <- reg_
    bracket_output[a, 9] <- seed_
    
  }
  
  colnames(bracket_output) <- c("TEAM", "SEEDE", "S16", "E8", "F4", "RUNNERUP", "CHAMP", "REGION", "SEED")
  #bracket_output <- bracket_output[, c(-2, -3)]
  bracket_output <- bracket_output[, c(8:9, 1:7)]
  write_csv(bracket_output, "bracketoutput.csv")
} 

ncaa_meaningful <- function(x) {
  
  bracket_robin <- read_csv('ncaarrtable.csv') %>%
    group_by(TEAM) %>%
    summarize(ct = n()) %>%
    select(TEAM) %>%
    ungroup() %>%
    inner_join(final_stats_frame, by = c("TEAM" = "TEAM_NAME"))

  bracket_robin <- bracket_robin[, c(1, 40:83, 90:99, 104:113)]

  write_csv(bracket_robin, "ncaameaningful.csv")
}

mc_margin_histo <- function(x) {
  
  
  
  p <- ggplot(mc_margin_bind, aes(x=V1)) + 
    geom_histogram(binwidth = 1, aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  p
}

mt_margin_histo <- function(x) {
  p <- ggplot(mt_margin_bind, aes(x=V1)) + 
    geom_histogram(binwidth = 1, aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  p
}

conf_wins_histo <- function(conf, tot) {
  
  z_file <- read_csv("wins_matrix.csv")
  
  z_filt <- z_file %>%
    filter(CONF == conf)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  z_upper <- length(which(z_gather$Wins > tot))
  z_lower <- length(which(z_gather$Wins < tot))
  print(paste("Over: ", z_upper / (z_upper + z_lower)))
  print(paste("Under: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=Wins)) + 
    geom_histogram(binwidth = 1, aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  
  p
  
}

team_wins_histo <- function(team, tot) {
  
  z_file <- read_csv("wins_matrix.csv")
  
  z_filt <- z_file %>%
    filter(TEAM == team)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  z_upper <- length(which(z_gather$Wins > tot))
  z_lower <- length(which(z_gather$Wins < tot))
  print(paste("Over: ", z_upper / (z_upper + z_lower)))
  print(paste("Under: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=Wins)) + 
    geom_histogram(binwidth = 1, aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  
  p
  
}

team_sweet16_histo <- function(team) {
  
  z_file <- read_csv("sweet16_matrix.csv")
  
  z_filt <- z_file %>%
    filter(TEAM == team)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  z_upper <- length(which(z_gather$Wins > 0))
  z_lower <- length(which(z_gather$Wins < 1))
  print(paste("Yes: ", z_upper / (z_upper + z_lower)))
  print(paste("No: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=Wins)) + geom_bar(position='dodge')
  
  p
  
}

seed_wins_histo <- function(seeder, tot) {
  
  z_file <- read_csv("wins_matrix.csv")
  
  z_filt <- z_file %>%
    filter(SEED == seeder)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  z_upper <- length(which(z_gather$Wins > tot))
  z_lower <- length(which(z_gather$Wins < tot))
  print(paste("Over: ", z_upper / (z_upper + z_lower)))
  print(paste("Under: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=Wins)) + 
    geom_histogram(binwidth = 1, aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  
  p
  
}

large_seed_histo <- function(seeder) {
  
  z_file <- read_csv("final_four_matrix.csv")
  
  z_gather <- gather(z_file, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    filter(Wins > 0) %>%
    group_by(Trial) %>%
    summarize(MAXSEED = max(SEED))
  
  z_upper <- length(which(z_gather$MAXSEED > seeder))
  z_lower <- length(which(z_gather$MAXSEED < seeder))
  print(paste("Over: ", z_upper / (z_upper + z_lower)))
  print(paste("Under: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=MAXSEED)) + 
    geom_histogram(binwidth = 1, aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  
  p
  
}

conf_champ_histo <- function(conf) {
  
  z_file <- read_csv("champ_matrix.csv")
  
  z_filt <- z_file %>%
    filter(CONF == conf)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  z_upper <- length(which(z_gather$Wins > 0))
  z_lower <- length(which(z_gather$Wins < 1))
  print(paste("Yes: ", z_upper / (z_upper + z_lower)))
  print(paste("No: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=Wins)) + geom_bar(position='dodge')
  
  p
  
}

seed_champ_histo <- function(conf) {
  
  z_file <- read_csv("champ_matrix.csv")
  
  z_filt <- z_file %>%
    filter(SEED == conf)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  z_upper <- length(which(z_gather$Wins > 0))
  z_lower <- length(which(z_gather$Wins < 1))
  print(paste("Yes: ", z_upper / (z_upper + z_lower)))
  print(paste("No: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=Wins)) + geom_bar(position='dodge')
  
  p
  
}

seed_final4_histo <- function(conf, tot) {
  
  z_file <- read_csv("final_four_matrix.csv")
  
  z_filt <- z_file %>%
    filter(SEED == conf)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  z_upper <- length(which(z_gather$Wins > tot))
  z_lower <- length(which(z_gather$Wins < tot))
  print(paste("Over: ", z_upper / (z_upper + z_lower)))
  print(paste("Under: ", z_lower / (z_upper + z_lower)))
  
  p <- ggplot(z_gather, aes(x=Wins)) + 
    geom_histogram(binwidth = 1, aes(y=..density..), colour="black", fill="white")+
    geom_density(alpha=.2, fill="#FF6666") 
  
  p
}


head_to_head_histo <- function(team1, team2) {
  
  z_file <- read_csv("wins_matrix.csv")
  
  z_filt <- z_file %>%
    filter(TEAM == team1)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  team1_gather <- z_gather
  
  z_filt <- z_file %>%
    filter(TEAM == team2)
  
  z_gather <- gather(z_filt, Trial, Wins, -TEAM, -CONF, -SEED) %>%
    group_by(Trial) %>%
    summarize(Wins = sum(Wins))
  
  team2_gather <- z_gather
  
  gather_join <- team1_gather %>%
    inner_join(team2_gather, by = c("Trial")) %>%
    mutate(TEAM1EDGE = ifelse(Wins.x > Wins.y, 1, 0)) %>%
    mutate(TEAM2EDGE = ifelse(Wins.y > Wins.x, 1, 0)) %>%
    mutate(DIF = Wins.x - Wins.y) %>%
    arrange(DIF)
  
  team1edgesum <- sum(gather_join$TEAM1EDGE)
  team2edgesum <- sum(gather_join$TEAM2EDGE)
  
  print(paste(team1, "Edge: ", team1edgesum / (team1edgesum + team2edgesum)))
  print(paste(team2, "Edge: ", team2edgesum / (team1edgesum + team2edgesum)))
  
  View(gather_join)
  
  #p <- ggplot(data = gather_join,
              #aes(x = reorder(Trial, DIF), y = DIF,
                  #fill = factor(TEAM1EDGE)))+
    #geom_bar(stat = "identity")
  
  #p
}

#A = Alternate Point Spread
prop_swap <- function(t, n) {
  
  if (t == "A") {
    
    a <- 1
    g <- length(mc_away_boxscore_list)
    
    a_cov_ct <- 0
    h_cov_ct <- 0
    
    for (a in a:g) {
      
      abox_frame <- mc_away_boxscore_list[[a]]
      hbox_frame <- mc_home_boxscore_list[[a]]
      
      apts_sum <- sum(abox_frame[, 24])
      hpts_sum <- sum(hbox_frame[, 24])
      
      marg_sum <- (apts_sum + n) - hpts_sum
      print(marg_sum)
      
      if (marg_sum > 0) { a_cov_ct <- a_cov_ct + 1 }
      if (marg_sum < 0) { h_cov_ct <- h_cov_ct + 1 }
      
    }
    print(a_cov_ct / g)
    print(h_cov_ct / g)
    
  }
  
  if (t == "TM3") {
    
    a <- 1
    g <- length(mc_away_boxscore_list)
    
    a_cov_ct <- 0
    h_cov_ct <- 0
    
    for (a in a:g) {
      
      abox_frame <- mc_away_boxscore_list[[a]]
      hbox_frame <- mc_home_boxscore_list[[a]]
      
      apts_sum <- sum(abox_frame[, 10])
      hpts_sum <- sum(hbox_frame[, 10])
      
      marg_sum <- apts_sum + hpts_sum
      print(marg_sum)
      
      if (marg_sum > n) { a_cov_ct <- a_cov_ct + 1 }
      if (marg_sum < n) { h_cov_ct <- h_cov_ct + 1 }
      
    }
    print(a_cov_ct / g)
    print(h_cov_ct / g)
    
  }
  
  if (t == "T") {
    
    a <- 1
    g <- length(mc_away_boxscore_list)
    
    a_cov_ct <- 0
    h_cov_ct <- 0
    
    for (a in a:g) {
      
      abox_frame <- mc_away_boxscore_list[[a]]
      hbox_frame <- mc_home_boxscore_list[[a]]
      
      apts_sum <- sum(abox_frame[, 24])
      hpts_sum <- sum(hbox_frame[, 24])
      
      marg_sum <- apts_sum + hpts_sum
      marg_sum2 <- n + 9
      
      if (marg_sum >= n & marg_sum <= marg_sum2) { a_cov_ct <- a_cov_ct + 1 }
      else{ h_cov_ct <- h_cov_ct + 1 }
      
    }
    print(a_cov_ct / g)
    print(h_cov_ct / g)
    
  }
  
  if (t == "A3") {
    
    a <- 1
    g <- length(mc_away_boxscore_list)
    
    a_cov_ct <- 0
    h_cov_ct <- 0
    
    for (a in a:g) {
      
      abox_frame <- mc_away_boxscore_list[[a]]
      
      apts_sum <- sum(abox_frame[, 10])
      print(apts_sum)
      
      if (apts_sum > n) { a_cov_ct <- a_cov_ct + 1 }
      else{ h_cov_ct <- h_cov_ct + 1 }
      
    }
    print(a_cov_ct / g)
    print(h_cov_ct / g)
    
  }
  
  if (t == "H3") {
    
    a <- 1
    g <- length(mc_away_boxscore_list)
    
    a_cov_ct <- 0
    h_cov_ct <- 0
    
    for (a in a:g) {
      
      abox_frame <- mc_home_boxscore_list[[a]]
      
      apts_sum <- sum(abox_frame[, 10])
      print(apts_sum)
      
      if (apts_sum > n) { a_cov_ct <- a_cov_ct + 1 }
      else{ h_cov_ct <- h_cov_ct + 1 }
      
    }
    print(a_cov_ct / g)
    print(h_cov_ct / g)
    
  }
}

player_swap <- function(l, i, t, n) {
  
  a <- 1
  g <- length(mc_away_boxscore_list)
  
  o_ct <- 0
  u_ct <- 0
  avg_ct <- 0
  
  for (a in a:g) {
    
    if (t == "PTS") { stat_loc <- 24 }
    if (t == "REB") { stat_loc <- 18 }
    if (t == "AST") { stat_loc <- 19 }
    if (t == "FT") { stat_loc <- 13 }
    if (t == "P3") { stat_loc <- 10 }
    
    if (l == "A") {
      box_list <- mc_away_boxscore_list[[a]]
    }
    else {
      box_list <- mc_home_boxscore_list[[a]]
    }
    
    if (t == "PRA") { stat_line <- sum(box_list[which(box_list[, 1] == i), c(24, 18, 19)]) }
    else { stat_line <- as.numeric(box_list[which(box_list[, 1] == i), stat_loc]) }
    
    
    if (stat_line > n) { o_ct <- o_ct + 1 }
    if (stat_line < n) { u_ct <- u_ct + 1 }
    avg_ct <- avg_ct + stat_line
    
  }
  o_cov <- o_ct / (o_ct + u_ct)
  u_cov <- u_ct / (o_ct + u_ct)
  a_cov <- (avg_ct / g)
  return(c(o_cov, u_cov, a_cov))
}

player_frame_do <- function(x) {
  
  player_frame <- read_csv('playerprop.csv')
  player_out <- player_frame
  a <- 1
  g <- nrow(player_frame)
  
  print(player_frame)
  
  for (a in a:g) {
    
    l <- as.character(player_frame[a, 1])
    i <- as.numeric(player_frame[a, 3])
    t <- as.character(player_frame[a, 4])
    n <- as.numeric(player_frame[a, 5])
    o <- as.numeric(player_frame[a, 6])
    u <- as.numeric(player_frame[a, 7])
    
    o_c <- moneyline_conversion(o)
    u_c <- moneyline_conversion(u)
    
    print(a)
    
    z <- player_swap(l, i, t, n)
    
    player_out[a, 8] <- z[1]
    player_out[a, 9] <- z[2]
    player_out[a, 10] <- z[3]
    player_out[a, 11] <- z[1] - o_c
    player_out[a, 12] <- z[2] - u_c
    
  }
  write_csv(player_out, 'playerout.csv')
}

#This file runs each day's games through various prediction methods and structures the outputs into a file titled formatted_wpt.
#The prediction methods used are 2 monte carlo simulation based on individaul or team stats, a linear/logistic regression(known as the Blender), a kMeans Clustering Approach
  #known as the ATM, a and random forest known as the Simple Model.
#The 2nd half of the script contains a lot of train-test-split approaches for hyper parameter tuning of machine learning methods to translate raw model outputs into
  #more actionable approaches. Specifically random forest and logistic regression methods are used to decode raw outputs for 2nd half and pre game wagers.


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
library(randomForest)
library(caret)

roaro_connorson <- function(m, d, y, ml_bot, ml_top) {
  
  bet_value_5(1)
  master_key_stripper_set(1)
  #master_key_stripper(m, d, y) 
  #master_key_stripper2(m, d, y)
  website_picks5(1)
  #pre_key_tree(m, d, y, ml_bot, ml_top)
  website_picks5_conv(m, d, y)

}

odds_load <- function(xxxx) {
  
  master_names_cbb <<- read_csv("names.csv")
  daily_odds <<- read_excel("CBB Odds.xlsm")
  
  ngames_frame <<- team_game_by_game_raw %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(Games = n())
  
  a <- 1
  g <- nrow(daily_odds)
  strike_vector <<- NULL
  
  for (a in a:g) {
   
    strike_use <<- 0
    aid_use <<- as.numeric(daily_odds[a, 8])
    hid_use <<- as.numeric(daily_odds[a, 9])
    
    aid_games <<- which(ngames_frame$TEAM_ID == aid_use)
    if (length(aid_games) == 0) { 
      strike_use <<- 1 
      a_games <<- 0
    }
    else { 
      a_games <<- as.numeric(ngames_frame[aid_games, 3])
      if (a_games < 3) { strike_use <<- 1 }
    }
    
    hid_games <<- which(ngames_frame$TEAM_ID == hid_use)
    if (length(hid_games) == 0) { 
      strike_use <<- 1 
      h_games <<- 0
    }
    else { 
      h_games <<- as.numeric(ngames_frame[hid_games, 3])
      if (h_games < 3) { strike_use <<- 1 }
    }
    
    if (strike_use == 1) { 
      if (is.null(strike_vector) == TRUE) { strike_vector <<- a }
      else { strike_vector <<- c(strike_vector, a) }
    }
  }
  strike_vector <<- -1 * strike_vector
  
  if (length(strike_vector) > 0) { daily_odds_strike <<- daily_odds[strike_vector, ] }
  else { daily_odds_strike <<- daily_odds }
  stage_output_frame5 <<- daily_odds_strike
}

stage_do_fix <- function(s, e) {
  
  a <- 1
  g <- nrow(stage_output_frame5)
  for (a in a:g) {
    
    stage_spread <<- as.numeric(stage_output_frame5[a, 1])
    stage_aml <<- as.numeric(stage_output_frame5[a, 2])
    stage_hml <<- as.numeric(stage_output_frame5[a, 3])
    stage_total <<- as.numeric(stage_output_frame5[a, 4])
    stage_loc <<- as.character(stage_output_frame5[a, 5])
    stage_aname <<- as.character(stage_output_frame5[a, 6])
    stage_hname <<- as.character(stage_output_frame5[a, 7])
    stage_aid <<- as.numeric(stage_output_frame5[a, 8])
    stage_hid <<- as.numeric(stage_output_frame5[a, 9])
    
    mc_away_id <<- stage_aid
    mc_home_id <<- stage_hid
    mc_loc <<- stage_loc
    mc_tableset(1)
    
    a_ml_conv <<- moneyline_conversion(stage_aml)
    h_ml_conv <<- moneyline_conversion(stage_hml)
    
    print(paste("Staging:", a, "/", g, stage_loc, stage_aname, stage_hname, mc_away_id, mc_home_id, sep = " "))
    
    blender_predict(1)
    rf_predict(1)

    
    blnd_avg_margin <<- blender_proj_marg
    blnd_avg_total <<- blender_proj_total
    blnd_avg_ascore <<- blender_proj_ascore
    blnd_avg_hscore <<- blender_proj_hscore
    blnd_a_win_pct <<- blender_awin_pct
    blnd_h_win_pct <<- blender_hwin_pct
    
    sim_avg_margin <<- rf_proj_marg
    sim_avg_total <<- rf_proj_total
    sim_avg_ascore <<- rf_proj_ascore
    sim_avg_hscore <<- rf_proj_hscore
    sim_a_win_pct <<- rf_awin_pct
    sim_h_win_pct <<- rf_hwin_pct
    
    print(paste(stage_aname, stage_hname,
                "BLND:", blnd_avg_ascore, blnd_avg_hscore, 
                "SIMP:", sim_avg_ascore, sim_avg_hscore, sep = " "))
    
    blnd_value_amarg <<- blnd_avg_margin - stage_spread
    blnd_value_hmarg <<- stage_spread - blnd_avg_margin
    blnd_value_over <<- blnd_avg_total - stage_total
    blnd_value_under <<- stage_total - blnd_avg_total
    blnd_value_aml <<- blnd_a_win_pct - a_ml_conv
    blnd_value_hml <<- blnd_h_win_pct - h_ml_conv
    
    sim_value_amarg <<- sim_avg_margin - stage_spread
    sim_value_hmarg <<- stage_spread - sim_avg_margin
    sim_value_over <<- sim_avg_total - stage_total
    sim_value_under <<- stage_total - sim_avg_total
    sim_value_aml <<- sim_a_win_pct - a_ml_conv
    sim_value_hml <<- sim_h_win_pct - h_ml_conv
    
    stage_output_frame5[a, 35] <<- blnd_avg_margin
    stage_output_frame5[a, 36] <<- blnd_avg_ascore
    stage_output_frame5[a, 37] <<- blnd_avg_hscore
    stage_output_frame5[a, 38] <<- blnd_avg_total
    stage_output_frame5[a, 39] <<- blnd_a_win_pct
    stage_output_frame5[a, 40] <<- blnd_h_win_pct
    stage_output_frame5[a, 41] <<- blnd_value_amarg
    stage_output_frame5[a, 42] <<- blnd_value_hmarg
    stage_output_frame5[a, 43] <<- blnd_value_aml
    stage_output_frame5[a, 44] <<- blnd_value_hml
    stage_output_frame5[a, 45] <<- blnd_value_over
    stage_output_frame5[a, 46] <<- blnd_value_under
    
    stage_output_frame5[a, 59] <<- sim_avg_margin
    stage_output_frame5[a, 60] <<- sim_avg_ascore
    stage_output_frame5[a, 61] <<- sim_avg_hscore
    stage_output_frame5[a, 62] <<- sim_avg_total
    stage_output_frame5[a, 63] <<- sim_a_win_pct
    stage_output_frame5[a, 64] <<- sim_h_win_pct
    stage_output_frame5[a, 65] <<- sim_value_amarg
    stage_output_frame5[a, 66] <<- sim_value_hmarg
    stage_output_frame5[a, 67] <<- sim_value_aml
    stage_output_frame5[a, 68] <<- sim_value_hml
    stage_output_frame5[a, 69] <<- sim_value_over
    stage_output_frame5[a, 70] <<- sim_value_under
    
    
  }
}
  
stage_do_5 <- function(s, e) {
  
  mc_incexc <<- read_excel("INCEXC.xlsx")
  daily_odds <<- daily_odds_strike
  num_trials <<- 1000
  if (s == 0 & e == 0) { 
    odds_load(1)
    a <- 1
    g <- nrow(stage_output_frame5)
  }
  else {
    a <- s
    g <- e
  }
  n_ct <<- 0
  
  dfs_frame <<- NULL
  dfs_whole_frame <<- NULL
  
  for (a in a:g) {
    
    stage_spread <<- as.numeric(stage_output_frame5[a, 1])
    stage_aml <<- as.numeric(stage_output_frame5[a, 2])
    stage_hml <<- as.numeric(stage_output_frame5[a, 3])
    stage_total <<- as.numeric(stage_output_frame5[a, 4])
    stage_loc <<- as.character(stage_output_frame5[a, 5])
    stage_aname <<- as.character(stage_output_frame5[a, 6])
    stage_hname <<- as.character(stage_output_frame5[a, 7])
    stage_aid <<- as.numeric(stage_output_frame5[a, 8])
    stage_hid <<- as.numeric(stage_output_frame5[a, 9])
    
    mc_away_id <<- stage_aid
    mc_home_id <<- stage_hid
    mc_loc <<- stage_loc
    
    
    a_ml_conv <<- moneyline_conversion(stage_aml)
    h_ml_conv <<- moneyline_conversion(stage_hml)
    
    print(paste("Staging:", a, "/", g, stage_loc, stage_aname, stage_hname, sep = " "))
    
    mc_monte_carlo_do(num_trials)
    mt_monte_carlo_do(num_trials)
    
    #Margin Frames for Props
    
    mc_slot_z <- as.data.frame(matrix(abs(mc_margin_slotter), nrow = num_trials, ncol = 1))
    mt_slot_z <- as.data.frame(matrix(abs(mt_margin_slotter), nrow = num_trials, ncol = 1))
    
    if (a == 1) { 
      mc_margin_bind <<- data.frame(quantile(mc_slot_z[, 1])[2], quantile(mc_slot_z[, 1])[4])
      mt_margin_bind <<- data.frame(quantile(mt_slot_z[, 1])[2], quantile(mt_slot_z[, 1])[4])
      }
    else { 
      mc_margin_bind <<- rbind(mc_margin_bind, data.frame(quantile(mc_slot_z[, 1])[2], quantile(mc_slot_z[, 1])[4]))
      mt_margin_bind <<- rbind(mt_margin_bind, data.frame(quantile(mt_slot_z[, 1])[2], quantile(mt_slot_z[, 1])[4]))
    }
    
    #End Margin Frames
    
    blender_predict(1)
    rf_predict(1)
    atm_fingerprint(1)
    simple_predict(2)
    
    #Indy MC Data
    
    indy_avg_margin <<- avg_awayscore - avg_homescore
    indy_avg_total <<- avg_awayscore + avg_homescore
    indy_avg_ascore <<- avg_awayscore
    indy_avg_hscore <<- avg_homescore
    indy_a_win_pct <<- sum(mc_win_slotter == 1) / num_trials
    indy_h_win_pct <<- sum(mc_win_slotter == 0) / num_trials
    
    #Team MC Data
    
    team_avg_margin <<- mt_avg_awayscore - mt_avg_homescore
    team_avg_total <<- mt_avg_awayscore + mt_avg_homescore
    team_avg_ascore <<- mt_avg_awayscore
    team_avg_hscore <<- mt_avg_homescore
    team_a_win_pct <<- sum(mt_win_slotter == 1) / num_trials
    team_h_win_pct <<- sum(mt_win_slotter == 0) / num_trials
    
    #Blender Data
    
    blnd_avg_margin <<- blender_proj_marg
    blnd_avg_total <<- blender_proj_total
    blnd_avg_ascore <<- blender_proj_ascore
    blnd_avg_hscore <<- blender_proj_hscore
    blnd_a_win_pct <<- blender_awin_pct
    blnd_h_win_pct <<- blender_hwin_pct
    
    #ATM Data
    
    atm_avg_margin <<- atm_proj_ascore - atm_proj_hscore
    atm_avg_total <<- atm_proj_tscore
    atm_avg_ascore <<- atm_proj_ascore
    atm_avg_hscore <<- atm_proj_hscore
    atm_a_win_pct <<- atm_proj_awinpct
    atm_h_win_pct <<- atm_proj_hwinpct
    
    #Simple Data
    
    sim_avg_margin <<- rf_proj_marg
    sim_avg_total <<- rf_proj_total
    sim_avg_ascore <<- rf_proj_ascore
    sim_avg_hscore <<- rf_proj_hscore
    sim_a_win_pct <<- rf_awin_pct
    sim_h_win_pct <<- rf_hwin_pct
    
    #Combo Data
    
    comb_avg_margin <<- mean(c(indy_avg_margin, team_avg_margin, blnd_avg_margin, sim_avg_margin, atm_avg_margin))
    comb_avg_total <<- mean(c(indy_avg_total, team_avg_total, blnd_avg_total, sim_avg_total, atm_avg_total))
    comb_avg_ascore <<- mean(c(indy_avg_ascore, team_avg_ascore, blnd_avg_ascore, sim_avg_ascore, atm_avg_ascore))
    comb_avg_hscore <<- mean(c(indy_avg_hscore, team_avg_hscore, blnd_avg_hscore, sim_avg_hscore, atm_avg_hscore))
    comb_a_win_pct <<- mean(c(indy_a_win_pct, team_a_win_pct, blnd_a_win_pct, sim_a_win_pct, atm_a_win_pct))
    comb_h_win_pct <<- mean(c(indy_h_win_pct, team_h_win_pct, blnd_h_win_pct, sim_h_win_pct, atm_h_win_pct))
    
    print(paste(stage_aname, stage_hname, "INDY:", indy_avg_ascore, indy_avg_hscore, "-", "TEAM:", team_avg_ascore, team_avg_hscore, "-",
                "BLND:", blnd_avg_ascore, blnd_avg_hscore, "-", "ATM:", atm_avg_ascore, atm_avg_hscore, "-",
                "SIMP:", sim_avg_ascore, sim_avg_hscore, "-", "COMB:", comb_avg_ascore, comb_avg_hscore, sep = " "))
    
    #Values
    
    indy_value_amarg <<- indy_avg_margin - stage_spread
    indy_value_hmarg <<- stage_spread - indy_avg_margin
    indy_value_over <<- indy_avg_total - stage_total
    indy_value_under <<- stage_total - indy_avg_total
    indy_value_aml <<- indy_a_win_pct - a_ml_conv
    indy_value_hml <<- indy_h_win_pct - h_ml_conv
    
    team_value_amarg <<- team_avg_margin - stage_spread
    team_value_hmarg <<- stage_spread - team_avg_margin
    team_value_over <<- team_avg_total - stage_total
    team_value_under <<- stage_total - team_avg_total
    team_value_aml <<- team_a_win_pct - a_ml_conv
    team_value_hml <<- team_h_win_pct - h_ml_conv
    
    blnd_value_amarg <<- blnd_avg_margin - stage_spread
    blnd_value_hmarg <<- stage_spread - blnd_avg_margin
    blnd_value_over <<- blnd_avg_total - stage_total
    blnd_value_under <<- stage_total - blnd_avg_total
    blnd_value_aml <<- blnd_a_win_pct - a_ml_conv
    blnd_value_hml <<- blnd_h_win_pct - h_ml_conv
    
    atm_value_amarg <<- atm_avg_margin - stage_spread
    atm_value_hmarg <<- stage_spread - atm_avg_margin
    atm_value_over <<- atm_avg_total - stage_total
    atm_value_under <<- stage_total - atm_avg_total
    atm_value_aml <<- atm_a_win_pct - a_ml_conv
    atm_value_hml <<- atm_h_win_pct - h_ml_conv
    
    sim_value_amarg <<- sim_avg_margin - stage_spread
    sim_value_hmarg <<- stage_spread - sim_avg_margin
    sim_value_over <<- sim_avg_total - stage_total
    sim_value_under <<- stage_total - sim_avg_total
    sim_value_aml <<- sim_a_win_pct - a_ml_conv
    sim_value_hml <<- sim_h_win_pct - h_ml_conv
    
    comb_value_amarg <<- comb_avg_margin - stage_spread
    comb_value_hmarg <<- stage_spread - comb_avg_margin
    comb_value_over <<- comb_avg_total - stage_total
    comb_value_under <<- stage_total - comb_avg_total
    comb_value_aml <<- comb_a_win_pct - a_ml_conv
    comb_value_hml <<- comb_h_win_pct - h_ml_conv
    
    #Add to Frame
    
    stage_output_frame5[a, 10] <<- avg_tempo
    
    stage_output_frame5[a, 11] <<- indy_avg_margin
    stage_output_frame5[a, 12] <<- indy_avg_ascore
    stage_output_frame5[a, 13] <<- indy_avg_hscore
    stage_output_frame5[a, 14] <<- indy_avg_total
    stage_output_frame5[a, 15] <<- indy_a_win_pct
    stage_output_frame5[a, 16] <<- indy_h_win_pct
    stage_output_frame5[a, 17] <<- indy_value_amarg
    stage_output_frame5[a, 18] <<- indy_value_hmarg
    stage_output_frame5[a, 19] <<- indy_value_aml
    stage_output_frame5[a, 20] <<- indy_value_hml
    stage_output_frame5[a, 21] <<- indy_value_over
    stage_output_frame5[a, 22] <<- indy_value_under
    
    stage_output_frame5[a, 23] <<- team_avg_margin
    stage_output_frame5[a, 24] <<- team_avg_ascore
    stage_output_frame5[a, 25] <<- team_avg_hscore
    stage_output_frame5[a, 26] <<- team_avg_total
    stage_output_frame5[a, 27] <<- team_a_win_pct
    stage_output_frame5[a, 28] <<- team_h_win_pct
    stage_output_frame5[a, 29] <<- team_value_amarg
    stage_output_frame5[a, 30] <<- team_value_hmarg
    stage_output_frame5[a, 31] <<- team_value_aml
    stage_output_frame5[a, 32] <<- team_value_hml
    stage_output_frame5[a, 33] <<- team_value_over
    stage_output_frame5[a, 34] <<- team_value_under
    
    stage_output_frame5[a, 35] <<- blnd_avg_margin
    stage_output_frame5[a, 36] <<- blnd_avg_ascore
    stage_output_frame5[a, 37] <<- blnd_avg_hscore
    stage_output_frame5[a, 38] <<- blnd_avg_total
    stage_output_frame5[a, 39] <<- blnd_a_win_pct
    stage_output_frame5[a, 40] <<- blnd_h_win_pct
    stage_output_frame5[a, 41] <<- blnd_value_amarg
    stage_output_frame5[a, 42] <<- blnd_value_hmarg
    stage_output_frame5[a, 43] <<- blnd_value_aml
    stage_output_frame5[a, 44] <<- blnd_value_hml
    stage_output_frame5[a, 45] <<- blnd_value_over
    stage_output_frame5[a, 46] <<- blnd_value_under
    
    stage_output_frame5[a, 47] <<- atm_avg_margin
    stage_output_frame5[a, 48] <<- atm_avg_ascore
    stage_output_frame5[a, 49] <<- atm_avg_hscore
    stage_output_frame5[a, 50] <<- atm_avg_total
    stage_output_frame5[a, 51] <<- atm_a_win_pct
    stage_output_frame5[a, 52] <<- atm_h_win_pct
    stage_output_frame5[a, 53] <<- atm_value_amarg
    stage_output_frame5[a, 54] <<- atm_value_hmarg
    stage_output_frame5[a, 55] <<- atm_value_aml
    stage_output_frame5[a, 56] <<- atm_value_hml
    stage_output_frame5[a, 57] <<- atm_value_over
    stage_output_frame5[a, 58] <<- atm_value_under
    
    stage_output_frame5[a, 59] <<- sim_avg_margin
    stage_output_frame5[a, 60] <<- sim_avg_ascore
    stage_output_frame5[a, 61] <<- sim_avg_hscore
    stage_output_frame5[a, 62] <<- sim_avg_total
    stage_output_frame5[a, 63] <<- sim_a_win_pct
    stage_output_frame5[a, 64] <<- sim_h_win_pct
    stage_output_frame5[a, 65] <<- sim_value_amarg
    stage_output_frame5[a, 66] <<- sim_value_hmarg
    stage_output_frame5[a, 67] <<- sim_value_aml
    stage_output_frame5[a, 68] <<- sim_value_hml
    stage_output_frame5[a, 69] <<- sim_value_over
    stage_output_frame5[a, 70] <<- sim_value_under
    
    stage_output_frame5[a, 71] <<- comb_avg_margin
    stage_output_frame5[a, 72] <<- comb_avg_ascore
    stage_output_frame5[a, 73] <<- comb_avg_hscore
    stage_output_frame5[a, 74] <<- comb_avg_total
    stage_output_frame5[a, 75] <<- comb_a_win_pct
    stage_output_frame5[a, 76] <<- comb_h_win_pct
    stage_output_frame5[a, 77] <<- comb_value_amarg
    stage_output_frame5[a, 78] <<- comb_value_hmarg
    stage_output_frame5[a, 79] <<- comb_value_aml
    stage_output_frame5[a, 80] <<- comb_value_hml
    stage_output_frame5[a, 81] <<- comb_value_over
    stage_output_frame5[a, 82] <<- comb_value_under
    
    stage_output_frame5[a, 83] <<- game_mvp_id
    stage_output_frame5[a, 84] <<- game_mvp_team
    stage_output_frame5[a, 85] <<- game_mvp_score
    stage_output_frame5[a, 86] <<- game_mvp_pts
    stage_output_frame5[a, 87] <<- game_mvp_reb
    stage_output_frame5[a, 88] <<- game_mvp_ast
    
  }
  print("Complete")
  dfs_name_fix(1)
  colnames(dfs_whole_frame)[12:19] <<- c("oDE", "o3PD", "oTRB", "oDAST", "oTOV", "oDBLK", "oSTL", "PACE")
  colnames(stage_output_frame5)[10:88] <<- c("AVGTEMPO", "IMARG", "IASCORE", "IHSCORE", "ITOTAL", "IAWIN", "IHWIN",
                                            "IAMARGV", "IHMARGV", "IAMLV", "IHMLV", "IOVERV", "IUNDERV", 
                                            "TMARG", "TASCORE", "THSCORE", "TTOTAL", "TAWIN", "THWIN",
                                            "TAMARGV", "THMARGV", "TAMLV", "THMLV", "TOVERV", "TUNDERV", 
                                            "BMARG", "BASCORE", "BHSCORE", "BTOTAL", "BAWIN", "BHWIN",
                                            "BAMARGV", "BHMARGV", "BAMLV", "BHMLV", "BOVERV", "BUNDERV", 
                                            "AMARG", "AASCORE", "AHSCORE", "ATOTAL", "AAWIN", "AHWIN",
                                            "AAMARGV", "AHMARGV", "AAMLV", "AHMLV", "AOVERV", "AUNDERV", 
                                            "SMARG", "SASCORE", "SHSCORE", "STOTAL", "SAWIN", "SHWIN",
                                            "SAMARGV", "SHMARGV", "SAMLV", "SHMLV", "SOVERV", "SUNDERV",
                                            "CMARG", "CASCORE", "CHSCORE", "CTOTAL", "CAWIN", "CHWIN",
                                            "CAMARGV", "CHMARGV", "CAMLV", "CHMLV", "COVERV", "CUNDERV", 
                                            "MVPID", "MVPTEAM", "MVPSCORE", "MVPPTS", "MVPREB", "MVPAST")
  write_csv(stage_output_frame5, "stage_output_frame_run1.csv")
  
}  

bet_value_5 <- function(xxxx) {
  
  a <- 1
  g <- nrow(stage_output_frame5)
  bv_vec <- c("INDY", "TEAM", "BLND", "ATM", "SIM", "COMB")
  ct_all <- 0
  
  for (a in a:g) {
    
    bv_spread <- as.numeric(stage_output_frame5[a, 1])
    bv_aml <- as.numeric(stage_output_frame5[a, 2])
    bv_hml <- as.numeric(stage_output_frame5[a, 3])
    bv_total <- as.numeric(stage_output_frame5[a, 4])
    bv_loc <- as.character(stage_output_frame5[a, 5])
    bv_ateam <- as.character(stage_output_frame5[a, 6])
    bv_hteam <- as.character(stage_output_frame5[a, 7])
    
    if (bv_loc == "H") { 
      bv_loc_a <- "A" 
      bv_loc_h <- "H"
    }
    else {
      bv_loc_a <- "N"
      bv_loc_h <- "N"
    }
    
    bv_list <- c(17:22, 29:34, 41:46, 53:58, 65:70, 77:82)
    b <- 1
    h <- length(bv_list)
    ct <- 1
    ct2 <- 1
    
    for (b in b:h) {
      
       ct_all <- ct_all + 1
       bb <- bv_list[b]
       bv_v <- as.numeric(stage_output_frame5[a, bb])
       bv_type2 <- bv_vec[ct2]
       
       if (ct == 1) { 
         bv_type1 <- "SPREAD"
         bv_line <- data.frame(bv_type2, bv_type1, bv_loc_a, bv_ateam, bv_hteam, (-1 * bv_spread), bv_v, stringsAsFactors = FALSE)
       }
       if (ct == 2) { 
         bv_type1 <- "SPREAD"
         bv_line <- data.frame(bv_type2, bv_type1, bv_loc_h, bv_hteam, bv_ateam, bv_spread, bv_v, stringsAsFactors = FALSE)
       }
       if (ct == 3) { 
         bv_type1 <- "ML"
         bv_line <- data.frame(bv_type2, bv_type1, bv_loc_a, bv_ateam, bv_hteam, bv_aml, bv_v, stringsAsFactors = FALSE)
       }
       if (ct == 4) { 
         bv_type1 <- "ML"
         bv_line <- data.frame(bv_type2, bv_type1, bv_loc_h, bv_hteam, bv_ateam, bv_hml, bv_v, stringsAsFactors = FALSE)
       }
       if (ct == 5) { 
         bv_type1 <- "OVER"
         bv_line <- data.frame(bv_type2, bv_type1, "T", bv_ateam, bv_hteam, bv_total, bv_v, stringsAsFactors = FALSE)
       }
       if (ct == 6) { 
         bv_type1 <- "UNDER"
         bv_line <- data.frame(bv_type2, bv_type1, "T", bv_ateam, bv_hteam, bv_total, bv_v, stringsAsFactors = FALSE)
       }
       
       colnames(bv_line) <- c("MTYPE", "BTYPE", "LOC", "TEAM", "OPP", "LINE", "VALUE")
       if (ct_all == 1) { bet_value_frame5 <<- bv_line }
       else { bet_value_frame5 <<- rbind(bet_value_frame5, bv_line) }
      
       ct <- ct + 1
       if (ct > 6) { 
         ct <- 1 
         ct2 <- ct2 + 1
       }
    }
  }
  write_csv(bet_value_frame5, "betvalueframe5tmp.csv")
}

website_picks5 <- function(xxxx) {

    website_ranker <<- master_team_adj_frame_wt %>%
      mutate(ED = OE - DE) %>%
      arrange(desc(ED))
    
    bv_list <- c(17:22, 29:34, 41:46, 53:58, 65:70, 77:82)

    b <- 1
    h <- length(bv_list)
    ct <- 1
    ct2 <- 1
    
    stage_output_frame_picks <<- stage_output_frame5
      
    for (b in b:h) {
      
      a <- 1
      g <- nrow(stage_output_frame_picks)
      bb <- bv_list[b]
      cc <- (ncol(stage_output_frame_picks) + 1)
      
      stage_output_frame_picks[, cc] <<- 0
      print(colnames(stage_output_frame_picks[bb]))
      
      for (a in a:g) {
        
        bv_val <- as.numeric(stage_output_frame_picks[a, bb])
        print(bv_val)
        print(a)
        
        if (is.nan(bv_val) == TRUE) {
          
          
          
        }  
        if (bv_val > 0) { stage_output_frame_picks[a, cc] <<- "X" }
        
    }
  }
  colnames(stage_output_frame_picks)[89:124] <<- c("IPAATS", "IPHATS", "IPAML", "IPHML", "IPOVR", "IPUND",
                                                   "TPAATS", "TPHATS", "TPAML", "TPHML", "TPOVR", "TPUND",
                                                   "BPAATS", "BPHATS", "BPAML", "BPHML", "BPOVR", "BPUND",
                                                   "APAATS", "APHATS", "APAML", "APHML", "APOVR", "APUND",
                                                   "SPAATS", "SPHATS", "SPAML", "SPHML", "SPOVR", "SPUND",
                                                   "CPAATS", "CPHATS", "CPAML", "CPHML", "CPOVR", "CPUND")
}

website_picks5_conv <- function(m, d, y) {
  
  a <- 1
  g <- nrow(stage_output_frame_picks)
  sofp_pmx <<- matrix("-", nrow = nrow(stage_output_frame_picks), ncol = 18)
  colnames(sofp_pmx) <<- c("IATSPLAY", "IMLPLAY", "IOUPLAY", "TATSPLAY", "TMLPLAY", "TOUPLAY", "BATSPLAY", "BMLPLAY", "BOUPLAY",
                           "AATSPLAY", "AMLPLAY", "AOUPLAY", "SATSPLAY", "SMLPLAY", "SOUPLAY", "CATSPLAY", "CMLPLAY", "COUPLAY")
  
  sofp_date <- matrix(c(m, d, y), nrow = nrow(stage_output_frame_picks), ncol = 3)
  bb <- ncol(stage_output_frame_picks) * -1
  #stage_output_frame_picks <<- stage_output_frame_picks[bb, ]
  
  for (a in a:g) {
    
     sofp_stub <- stage_output_frame_picks[a, c(1:10)]
     sofp_scores <- round(stage_output_frame_picks[a, c(12:13, 24:25, 36:37, 48:49, 60:61, 72:73)], digits = 1)
     sofp_winpcts <- stage_output_frame_picks[a, c(15:16, 27:28, 39:40, 51:52, 63:64, 75:76)]
     sofp_winpcts <- round(sofp_winpcts * 100, digits = 1)
     sofp_mvps <- stage_output_frame_picks[a, c(83:88)]
     
     sofp_mvp_team <<- as.numeric(sofp_mvps[1, 2])
     sofp_mvp_id <<- as.numeric(sofp_mvps[1, 1])
     mvp_score_use <<- round(as.numeric(sofp_mvps[1, 3]), 1) * 100
     mvp_pts_use <<- round(as.numeric(sofp_mvps[1, 4]), 1)
     mvp_reb_use <<- round(as.numeric(sofp_mvps[1, 5]), 1)
     mvp_ast_use <<- round(as.numeric(sofp_mvps[1, 6]), 1)
     
     mvp_name <- as.character(ind_all_adj_frame[which(ind_all_adj_frame$PLAYER_ID == sofp_mvp_id), 1])
     abb_mvp <- as.character(master_names_cbb[which(master_names_cbb$NCAA == sofp_mvp_team), 7])
     
     mvp_split <<- 'Jones, Horace'
     mvp_firstname <<- trim.all(mvp_split[[1]][2])
     mvp_lastname <<- mvp_split[[1]][1]
     
     mvp_name_use <<- paste(mvp_firstname, mvp_lastname, sep = " ")
     
     mvp_listout <<- paste(abb_mvp, " - ", mvp_name_use, ", ", mvp_score_use, " pScore - ", mvp_pts_use, " pts, ", mvp_reb_use, " reb, ",
                           mvp_ast_use, " ast", sep = "")
     
     sofp_mvps[1, 7:9] <- c('Travis', 'Travis', 'Travis')

     b <- 89
     h <- 124
     ct <- 1
     ct2 <- 1
     ct3 <- 1
     sofp_vec <- c("A", "H", "A", "H", "O", "U")
     

     for (b in b:h) {
       sofp_pick <- stage_output_frame_picks[a, b]
       if (sofp_pick == "X") {
         
         sofp_play <- sofp_vec[ct]
         sofp_pmx[a, ct3] <<- sofp_play
         
       }
       ct <- ct + 1
       ct2 <- ct2 + 1
       if (ct > 6) { ct <- 1 }
       if (ct2 > 2) { 
         ct2 <- 1 
         ct3 <- ct3 + 1
        }
     }
     
     if (a == 1) { formatted_wpt5 <<- data.frame(sofp_stub, sofp_scores, sofp_winpcts, sofp_mvps[1, 9], stringsAsFactors = FALSE) }
     else { 
       formatted_wpt52 <<- data.frame(sofp_stub, sofp_scores, sofp_winpcts, sofp_mvps[1, 9], stringsAsFactors = FALSE) 
       formatted_wpt5 <<- rbind(formatted_wpt5, formatted_wpt52)
    }
  }
  formatted_wpt5 <<- cbind(sofp_date, formatted_wpt5, sofp_pmx)
  formatted_wpt5[, 1] <<- m
  formatted_wpt5[, 2] <<- d
  formatted_wpt5[, 3] <<- y
  formatted_wpt5[, 38] <<- stage_output_frame_picks[, 38]
  colnames(formatted_wpt5)[1:3] <<- c("Month", "Day", "Year")
  #colnames(formatted_wpt5)[38] <<- c("PreMVP")
  colnames(formatted_wpt5)[38] <<- c("BLNDTOTAL")
  write_csv(formatted_wpt5, "formatted_wpt5_pre.csv")
}


#------------------------------
#PRE KEY DECISION TREES
#------------------------------

pre_key_step_loop <- function(ml_bot, ml_top) {
  
  pre_key_step(ml_bot, ml_top)
  pre_key_step_comb(ml_bot, ml_top)
  
  print(summary(ats_log_kit_nocomb))
  print(summary(ats_log_kit_comb))
  print(summary(over_log_kit_nocomb))
  print(summary(over_log_kit_comb))
  print(summary(under_log_kit_nocomb))
  print(summary(under_log_kit_comb))
  print(summary(ml_log_kit_nocomb))
  print(summary(ml_log_kit_comb))

  
  #NOCOMB, #NOCOMB, #NOCOMB, #NOCOMB
  
}

#Stepwise Regression to Determine Inputs
pre_key_step_use <- function(ml_bot, ml_top) {
  
  key_tree_ats <<- master_df_lineup_ats %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_over <<- master_df_lineup_tot %>%
    filter(BTYPE == "OVER") %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_under <<- master_df_lineup_tot %>%
    filter(BTYPE == "UNDER") %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_ats$RESFAC <<- as.factor(key_tree_ats$RESFAC)
  key_tree_over$RESFAC <<- as.factor(key_tree_over$RESFAC)
  key_tree_under$RESFAC <<- as.factor(key_tree_under$RESFAC)
  
  pre_key_ml_set(ml_bot, ml_top)
  
  print("ATS")
  ats_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_ats, family = "binomial"), direction='backward')
  print("OVER")
  #over_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_over, family = "binomial"), direction='backward')
  print("UNDER")
  #under_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_under, family = "binomial"), direction='backward')
  print("ML")
  ml_log_kit <<- step(glm(ATSRES ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_ml, family = "binomial"), direction='backward')
  
  #ats_log_kit <<- glm(RESFAC ~ BLND + SIM + COMB, data = key_tree_ats, family = "binomial")
  over_log_kit <<- glm(RESFAC ~ BLND + COMB, data = key_tree_over, family = "binomial")
  under_log_kit <<- glm(RESFAC ~ BLND + COMB, data = key_tree_under, family = "binomial")
  
  ats_log_vals <- length(ats_log_kit$coefficients)
  a <- 2
  g <- ats_log_vals
  for (a in a:g) {
    ats_log_name <- names(ats_log_kit$coefficients)[a]
    if (a == 2) { ats_log_form <- paste("RESFAC ~", ats_log_name) }
    else { ats_log_form <- paste(ats_log_form, "+", ats_log_name) }
  }
  ats_log_form_use <<- formula(ats_log_form)
  ats_rf_kit <<- randomForest(ats_log_form_use, data = key_tree_ats)
  
  over_log_vals <- length(over_log_kit$coefficients)
  a <- 2
  g <- over_log_vals
  for (a in a:g) {
    over_log_name <- names(over_log_kit$coefficients)[a]
    if (a == 2) { over_log_form <- paste("RESFAC ~", over_log_name) }
    else { over_log_form <- paste(over_log_form, "+", over_log_name) }
  }
  over_log_form_use <<- formula(over_log_form)
  over_rf_kit <<- randomForest(over_log_form_use, data = key_tree_over)
  
  under_log_vals <- length(under_log_kit$coefficients)
  a <- 2
  g <- under_log_vals
  for (a in a:g) {
    under_log_name <- names(under_log_kit$coefficients)[a]
    if (a == 2) { under_log_form <- paste("RESFAC ~", under_log_name) }
    else { under_log_form <- paste(under_log_form, "+", under_log_name) }
  }
  under_log_form_use <<- formula(under_log_form)
  under_rf_kit <<- randomForest(under_log_form_use, data = key_tree_under)
  
  ml_log_vals <- length(ml_log_kit$coefficients)
  a <- 2
  g <- ml_log_vals
  for (a in a:g) {
    ml_log_name <- names(ml_log_kit$coefficients)[a]
    if (a == 2) { ml_log_form <- paste("ATSRES ~", ml_log_name) }
    else { ml_log_form <- paste(ml_log_form, "+", ml_log_name) }
  }
  ml_log_form_use <<- formula(ml_log_form)
  ml_rf_kit <<- randomForest(ml_log_form_use, data = key_tree_ml)
  
}

pre_key_step <- function(ml_bot, ml_top) {
  
  key_tree_ats <<- master_df_lineup_ats %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_over <<- master_df_lineup_tot %>%
    filter(BTYPE == "OVER") %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_under <<- master_df_lineup_tot %>%
    filter(BTYPE == "UNDER") %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_ats$RESFAC <<- as.factor(key_tree_ats$RESFAC)
  key_tree_over$RESFAC <<- as.factor(key_tree_over$RESFAC)
  key_tree_under$RESFAC <<- as.factor(key_tree_under$RESFAC)
  
  pre_key_ml_set(ml_bot, ml_top)
  
  print("------ATS NO COMB")
  ats_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_ats, family = "binomial"), direction='backward')
  print("------OVER NO COMB")
  over_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_over, family = "binomial"), direction='backward')
  print("------UNDER NO COMB")
  under_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_under, family = "binomial"), direction='backward')
  print("------ML NO COMB")
  ml_log_kit <<- step(glm(ATSRES ~ INDY + TEAMC + BLND + ATM + SIM, data = key_tree_ml, family = "binomial"), direction='backward')
  
  
  ats_log_kit_nocomb <<- ats_log_kit
  over_log_kit_nocomb <<- over_log_kit
  under_log_kit_nocomb <<- under_log_kit
  ml_log_kit_nocomb <<- ml_log_kit
  
  ats_log_vals <- length(ats_log_kit$coefficients)
  a <- 2
  g <- ats_log_vals
  for (a in a:g) {
    ats_log_name <- names(ats_log_kit$coefficients)[a]
    if (a == 2) { ats_log_form <- paste("RESFAC ~", ats_log_name) }
    else { ats_log_form <- paste(ats_log_form, "+", ats_log_name) }
  }
  ats_log_form_use <<- formula(ats_log_form)
  ats_rf_kit <<- randomForest(ats_log_form_use, data = key_tree_ats)
  
  over_log_vals <- length(over_log_kit$coefficients)
  a <- 2
  g <- over_log_vals
  for (a in a:g) {
    over_log_name <- names(over_log_kit$coefficients)[a]
    if (a == 2) { over_log_form <- paste("RESFAC ~", over_log_name) }
    else { over_log_form <- paste(over_log_form, "+", over_log_name) }
  }
  over_log_form_use <<- formula(over_log_form)
  over_rf_kit <<- randomForest(over_log_form_use, data = key_tree_over)
  
  under_log_vals <- length(under_log_kit$coefficients)
  a <- 2
  g <- under_log_vals
  for (a in a:g) {
    under_log_name <- names(under_log_kit$coefficients)[a]
    if (a == 2) { under_log_form <- paste("RESFAC ~", under_log_name) }
    else { under_log_form <- paste(under_log_form, "+", under_log_name) }
  }
  under_log_form_use <<- formula(under_log_form)
  under_rf_kit <<- randomForest(under_log_form_use, data = key_tree_under)
  
  ml_log_vals <- length(ml_log_kit$coefficients)
  a <- 2
  g <- ml_log_vals
  for (a in a:g) {
    ml_log_name <- names(ml_log_kit$coefficients)[a]
    if (a == 2) { ml_log_form <- paste("ATSRES ~", ml_log_name) }
    else { ml_log_form <- paste(ml_log_form, "+", ml_log_name) }
  }
  ml_log_form_use <<- formula(ml_log_form)
  ml_rf_kit <<- randomForest(ml_log_form_use, data = key_tree_ml)
  
}

pre_key_step_comb <- function(ml_bot, ml_top) {
  
  key_tree_ats <<- master_df_lineup_ats %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_over <<- master_df_lineup_tot %>%
    filter(BTYPE == "OVER") %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_under <<- master_df_lineup_tot %>%
    filter(BTYPE == "UNDER") %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_ats$RESFAC <<- as.factor(key_tree_ats$RESFAC)
  key_tree_over$RESFAC <<- as.factor(key_tree_over$RESFAC)
  key_tree_under$RESFAC <<- as.factor(key_tree_under$RESFAC)
  
  pre_key_ml_set(ml_bot, ml_top)
  
  print("------ATS COMB")
  ats_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM + COMB, data = key_tree_ats, family = "binomial"), direction='backward')
  print("------OVER COMB")
  over_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM + COMB, data = key_tree_over, family = "binomial"), direction='backward')
  print("------UNDER COMB")
  under_log_kit <<- step(glm(RESFAC ~ INDY + TEAMC + BLND + ATM + SIM + COMB, data = key_tree_under, family = "binomial"), direction='backward')
  print("------ML COMB")
  ml_log_kit <<- step(glm(ATSRES ~ INDY + TEAMC + BLND + ATM + SIM + COMB, data = key_tree_ml, family = "binomial"), direction='backward')
  
  ats_log_kit_comb <<- ats_log_kit
  over_log_kit_comb <<- over_log_kit
  under_log_kit_comb <<- under_log_kit
  ml_log_kit_comb <<- ml_log_kit
  
  ats_log_vals <- length(ats_log_kit$coefficients)
  a <- 2
  g <- ats_log_vals
  for (a in a:g) {
    ats_log_name <- names(ats_log_kit$coefficients)[a]
    if (a == 2) { ats_log_form <- paste("RESFAC ~", ats_log_name) }
    else { ats_log_form <- paste(ats_log_form, "+", ats_log_name) }
  }
  ats_log_form_use <<- formula(ats_log_form)
  ats_rf_kit <<- randomForest(ats_log_form_use, data = key_tree_ats)
  
  over_log_vals <- length(over_log_kit$coefficients)
  a <- 2
  g <- over_log_vals
  for (a in a:g) {
    over_log_name <- names(over_log_kit$coefficients)[a]
    if (a == 2) { over_log_form <- paste("RESFAC ~", over_log_name) }
    else { over_log_form <- paste(over_log_form, "+", over_log_name) }
  }
  over_log_form_use <<- formula(over_log_form)
  over_rf_kit <<- randomForest(over_log_form_use, data = key_tree_over)
  
  under_log_vals <- length(under_log_kit$coefficients)
  a <- 2
  g <- under_log_vals
  for (a in a:g) {
    under_log_name <- names(under_log_kit$coefficients)[a]
    if (a == 2) { under_log_form <- paste("RESFAC ~", under_log_name) }
    else { under_log_form <- paste(under_log_form, "+", under_log_name) }
  }
  under_log_form_use <<- formula(under_log_form)
  under_rf_kit <<- randomForest(under_log_form_use, data = key_tree_under)
  
  ml_log_vals <- length(ml_log_kit$coefficients)
  a <- 2
  g <- ml_log_vals
  for (a in a:g) {
    ml_log_name <- names(ml_log_kit$coefficients)[a]
    if (a == 2) { ml_log_form <- paste("ATSRES ~", ml_log_name) }
    else { ml_log_form <- paste(ml_log_form, "+", ml_log_name) }
  }
  ml_log_form_use <<- formula(ml_log_form)
  ml_rf_kit <<- randomForest(ml_log_form_use, data = key_tree_ml)
  
}


#Set ML Tree Table

pre_key_ml_range <- function(ml_bot, ml_top) {
  
  a <- 1
  g <- nrow(master_df_lineup_ml)
  
  for (a in a:g) {
    
    mlres <- as.numeric(master_df_lineup_ml$RES[a])
    mlline <- as.numeric(master_df_lineup_ml$LINE[a])
    mlconv <- moneyline_conversion(mlline)
    if (mlres > 0) { master_df_lineup_ml[a, 13] <<- 1 }
    else { master_df_lineup_ml[a, 13] <<- 0 }
    
    master_df_lineup_ml[a, 14] <<- mlconv
    master_df_lineup_ml[a, 15] <<- mlconv + as.numeric(master_df_lineup_ml[a, 5])
    master_df_lineup_ml[a, 16] <<- mlconv + as.numeric(master_df_lineup_ml[a, 6])
    master_df_lineup_ml[a, 17] <<- mlconv + as.numeric(master_df_lineup_ml[a, 7])
    master_df_lineup_ml[a, 18] <<- mlconv + as.numeric(master_df_lineup_ml[a, 8])
    master_df_lineup_ml[a, 19] <<- mlconv + as.numeric(master_df_lineup_ml[a, 9])
    master_df_lineup_ml[a, 20] <<- mlconv + as.numeric(master_df_lineup_ml[a, 10])
    
    
  }
  
  master_df_lineup_ml$V13 <<- as.factor(master_df_lineup_ml$V13)
  key_tree_ml <<- master_df_lineup_ml[, c(1:4, 11:20)] %>%
    filter(LINE >= ml_bot & LINE <= ml_top)
  
  colnames(key_tree_ml)[7:14] <<- c("ATSRES", "MLCONV", "INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  ml_range_find <- key_tree_ml %>% group_by(LINE) %>% summarize(RES = sum(RES))
  ml_cumsum <- cumsum(ml_range_find$RES)
  ml_range_find <- cbind(ml_range_find, ml_cumsum)
  ggg <- ggplot(ml_range_find, aes(x = LINE, y = ml_cumsum)) + geom_point()
  
  View(ml_range_find)
  
  ggg
  
}

pre_key_ml_set <- function(ml_bot, ml_top) {
  
  a <- 1
  g <- nrow(master_df_lineup_ml)
  
  for (a in a:g) {
  
    mlres <- as.numeric(master_df_lineup_ml$RES[a])
    mlline <- as.numeric(master_df_lineup_ml$LINE[a])
    mlconv <- moneyline_conversion(mlline)
    if (mlres > 0) { master_df_lineup_ml[a, 13] <<- 1 }
    else { master_df_lineup_ml[a, 13] <<- 0 }
    
    master_df_lineup_ml[a, 14] <<- mlconv
    master_df_lineup_ml[a, 15] <<- mlconv + as.numeric(master_df_lineup_ml[a, 5])
    master_df_lineup_ml[a, 16] <<- mlconv + as.numeric(master_df_lineup_ml[a, 6])
    master_df_lineup_ml[a, 17] <<- mlconv + as.numeric(master_df_lineup_ml[a, 7])
    master_df_lineup_ml[a, 18] <<- mlconv + as.numeric(master_df_lineup_ml[a, 8])
    master_df_lineup_ml[a, 19] <<- mlconv + as.numeric(master_df_lineup_ml[a, 9])
    master_df_lineup_ml[a, 20] <<- mlconv + as.numeric(master_df_lineup_ml[a, 10])
    
    
  }
  
  master_df_lineup_ml$V13 <<- as.factor(master_df_lineup_ml$V13)
  key_tree_ml <<- master_df_lineup_ml[, c(1:4, 11:20)] %>%
    filter(LINE >= ml_bot & LINE <= ml_top)
  
  colnames(key_tree_ml)[7:14] <<- c("RESFAC", "MLCONV", "INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  a <- 1
  g <- nrow(master_st_lineup_ml)
  master_st_lineup_ml2 <<- master_st_lineup_ml
  
  for (a in a:g) {
    
    mlline <- as.numeric(master_st_lineup_ml2$LINE[a])
    mlconv <- moneyline_conversion(mlline)
    master_st_lineup_ml2[a, 11] <<- mlconv
    master_st_lineup_ml2[a, 12] <<- mlconv + as.numeric(master_st_lineup_ml2[a, 5])
    master_st_lineup_ml2[a, 13] <<- mlconv + as.numeric(master_st_lineup_ml2[a, 6])
    master_st_lineup_ml2[a, 14] <<- mlconv + as.numeric(master_st_lineup_ml2[a, 7])
    master_st_lineup_ml2[a, 15] <<- mlconv + as.numeric(master_st_lineup_ml2[a, 8])
    master_st_lineup_ml2[a, 16] <<- mlconv + as.numeric(master_st_lineup_ml2[a, 9])
    master_st_lineup_ml2[a, 17] <<- mlconv + as.numeric(master_st_lineup_ml2[a, 10])
    
  }
  
  master_st_lineup_ml2 <<- master_st_lineup_ml2[, c(1:4, 11:17)]
  colnames(master_st_lineup_ml2)[5:11] <<- c("MLCONV", "INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  master_st_lineup_ml2 <<- master_st_lineup_ml2 %>%
    mutate(PAYOUT = 1 * ((1 - MLCONV) / MLCONV))
  
}

pre_key_tree <- function(m, d, y, ml_bot, ml_top) {
  
  pre_key_ml_set(ml_bot, ml_top)
  
  #ATS Logistic and Random Forest
  
  rf_train <- sample(c(1:nrow(key_tree_ats)), round(nrow(key_tree_ats) * 1, 0))
  rf_train <- key_tree_ats[rf_train, ]
  
  ats_indy_log_kit <<- glm(RESFAC ~ INDY, data = rf_train, family = "binomial")
  ats_indy_rf_kit <<- randomForest(RESFAC ~ INDY, data = rf_train)
  ats_team_log_kit <<- glm(RESFAC ~ TEAMC, data = rf_train, family = "binomial")
  ats_team_rf_kit <<- randomForest(RESFAC ~ TEAMC, data = rf_train)
  ats_blnd_log_kit <<- glm(RESFAC ~ BLND, data = rf_train, family = "binomial")
  ats_blnd_rf_kit <<- randomForest(RESFAC ~ BLND, data = rf_train)
  ats_atm_log_kit <<- glm(RESFAC ~ ATM, data = rf_train, family = "binomial")
  ats_atm_rf_kit <<- randomForest(RESFAC ~ ATM, data = rf_train)
  ats_sim_log_kit <<- glm(RESFAC ~ SIM, data = rf_train, family = "binomial")
  ats_sim_rf_kit <<- randomForest(RESFAC ~ SIM, data = rf_train)
  ats_comb_log_kit <<- glm(RESFAC ~ COMB, data = rf_train, family = "binomial")
  ats_comb_rf_kit <<- randomForest(RESFAC ~ COMB, data = rf_train)
  
  #Over Logistic and Random Forest
  
  key_tree_over$RESFAC <<- as.factor(key_tree_over$RESFAC)
  rf_train <- sample(c(1:nrow(key_tree_over)), round(nrow(key_tree_over) * 1, 0))
  rf_train <- key_tree_over[rf_train, ]

  over_indy_log_kit <<- glm(RESFAC ~ INDY, data = rf_train, family = "binomial")
  over_indy_rf_kit <<- randomForest(RESFAC ~ INDY, data = rf_train)
  over_team_log_kit <<- glm(RESFAC ~ TEAMC, data = rf_train, family = "binomial")
  over_team_rf_kit <<- randomForest(RESFAC ~ TEAMC, data = rf_train)
  over_blnd_log_kit <<- glm(RESFAC ~ BLND, data = rf_train, family = "binomial")
  over_blnd_rf_kit <<- randomForest(RESFAC ~ BLND, data = rf_train)
  over_atm_log_kit <<- glm(RESFAC ~ ATM, data = rf_train, family = "binomial")
  over_atm_rf_kit <<- randomForest(RESFAC ~ ATM, data = rf_train)
  over_sim_log_kit <<- glm(RESFAC ~ SIM, data = rf_train, family = "binomial")
  over_sim_rf_kit <<- randomForest(RESFAC ~ SIM, data = rf_train)
  over_comb_log_kit <<- glm(RESFAC ~ COMB, data = rf_train, family = "binomial")
  over_comb_rf_kit <<- randomForest(RESFAC ~ COMB, data = rf_train)
  
  #Under Logistic and Random Forest
  
  key_tree_under$RESFAC <<- as.factor(key_tree_under$RESFAC)
  rf_train <- sample(c(1:nrow(key_tree_under)), round(nrow(key_tree_under) * 1, 0))
  rf_train <- key_tree_under[rf_train, ]
  
  under_indy_log_kit <<- glm(RESFAC ~ INDY, data = rf_train, family = "binomial")
  under_indy_rf_kit <<- randomForest(RESFAC ~ INDY, data = rf_train)
  under_team_log_kit <<- glm(RESFAC ~ TEAMC, data = rf_train, family = "binomial")
  under_team_rf_kit <<- randomForest(RESFAC ~ TEAMC, data = rf_train)
  under_blnd_log_kit <<- glm(RESFAC ~ BLND, data = rf_train, family = "binomial")
  under_blnd_rf_kit <<- randomForest(RESFAC ~ BLND, data = rf_train)
  under_atm_log_kit <<- glm(RESFAC ~ ATM, data = rf_train, family = "binomial")
  under_atm_rf_kit <<- randomForest(RESFAC ~ ATM, data = rf_train)
  under_sim_log_kit <<- glm(RESFAC ~ SIM, data = rf_train, family = "binomial")
  under_sim_rf_kit <<- randomForest(RESFAC ~ SIM, data = rf_train)
  under_comb_log_kit <<- glm(RESFAC ~ COMB, data = rf_train, family = "binomial")
  under_comb_rf_kit <<- randomForest(RESFAC ~ COMB, data = rf_train)
  
  #ML Logistic and Random Forest
  
  rf_train <- sample(c(1:nrow(key_tree_ml)), round(nrow(key_tree_ml) * 1, 0))
  rf_train <- key_tree_ml[rf_train, ]
  
  ml_indy_log_kit <<- glm(RESFAC ~ INDY, data = rf_train, family = "binomial")
  ml_indy_rf_kit <<- randomForest(RESFAC ~ INDY, data = rf_train)
  ml_team_log_kit <<- glm(RESFAC ~ TEAMC, data = rf_train, family = "binomial")
  ml_team_rf_kit <<- randomForest(RESFAC ~ TEAMC, data = rf_train)
  ml_blnd_log_kit <<- glm(RESFAC ~ BLND, data = rf_train, family = "binomial")
  ml_blnd_rf_kit <<- randomForest(RESFAC ~ BLND, data = rf_train)
  ml_atm_log_kit <<- glm(RESFAC ~ ATM, data = rf_train, family = "binomial")
  ml_atm_rf_kit <<- randomForest(RESFAC ~ ATM, data = rf_train)
  ml_sim_log_kit <<- glm(RESFAC ~ SIM, data = rf_train, family = "binomial")
  ml_sim_rf_kit <<- randomForest(RESFAC ~ SIM, data = rf_train)
  ml_comb_log_kit <<- glm(RESFAC ~ COMB, data = rf_train, family = "binomial")
  ml_comb_rf_kit <<- randomForest(RESFAC ~ COMB, data = rf_train)
  
  #Predictions
  
  testtrain <- master_st_lineup_ats
  ats_log_kit <<- glm(pre_ats_log_form, data = key_tree_ats, family = 'binomial')
  
  form_txt <- as.character(pre_ats_rf_form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  
  ats_rf_kit <<- randomForest(pre_ats_rf_form, data = key_tree_ats, mtry=form_ct)
  live_predict <- predict(ats_log_kit, testtrain, type = "response")
  rf_predict <- predict(ats_rf_kit, testtrain, type = "prob")
  ats_log_out <<- data.frame(testtrain, live_predict, rf_predict)
  
  ats_sin_lpredict <- predict(ats_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(ats_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "INDY", "ATS")
  
  ats_sin_lpredict <- predict(ats_team_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(ats_team_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "TEAMC", "ATS")
  
  ats_sin_lpredict <- predict(ats_blnd_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(ats_blnd_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "BLND", "ATS")
  
  ats_sin_lpredict <- predict(ats_atm_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(ats_atm_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "ATM", "ATS")
  
  ats_sin_lpredict <- predict(ats_sim_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(ats_sim_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "SIM", "ATS")
  
  ats_sin_lpredict <- predict(ats_comb_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(ats_comb_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "COMB", "ATS")
  
  
  testtrain <- master_st_lineup_tot %>% filter(BTYPE == "OVER")
  total_log_kit <<- glm(pre_total_log_form, data = key_tree_tot, family = 'binomial')
  
  form_txt <- as.character(pre_total_rf_form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  
  total_rf_kit <<- randomForest(pre_total_rf_form, data = key_tree_tot, mtry=form_ct)
  live_predict <- predict(total_log_kit, testtrain, type = "response")
  rf_predict <- predict(total_rf_kit, testtrain, type = "prob")
  total_log_out <<- data.frame(testtrain, live_predict, rf_predict)
  
  ats_sin_lpredict <- predict(over_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(over_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "INDY", "OVER")
  
  ats_sin_lpredict <- predict(over_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(over_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "TEAMC", "OVER")
  
  ats_sin_lpredict <- predict(over_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(over_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "BLND", "OVER")
  
  ats_sin_lpredict <- predict(over_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(over_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "ATM", "OVER")
  
  ats_sin_lpredict <- predict(over_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(over_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "SIM", "OVER")
  
  ats_sin_lpredict <- predict(over_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(over_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "COMB", "OVER")
  
  ats_sin_lpredict <- predict(under_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(under_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "INDY", "UNDER")
  
  ats_sin_lpredict <- predict(under_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(under_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "TEAMC", "UNDER")
  
  ats_sin_lpredict <- predict(under_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(under_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "BLND", "UNDER")
  
  ats_sin_lpredict <- predict(under_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(under_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "ATM", "UNDER")
  
  ats_sin_lpredict <- predict(under_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(under_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "SIM", "UNDER")
  
  ats_sin_lpredict <- predict(under_indy_log_kit, testtrain, type = "response")
  ats_sin_rpredict <- predict(under_indy_rf_kit, testtrain, type = "prob")
  single_log_out <- data.frame(testtrain, ats_sin_lpredict, ats_sin_rpredict)
  pre_key_frame(single_log_out, "COMB", "UNDER")
  
  testtrain <- master_st_lineup_ml2 %>%
    filter(LINE >= ml_bot & LINE <= ml_top)
  ml_log_kit <<- glm(pre_ml_log_form, data = key_tree_ml, family = 'binomial')
  
  form_txt <- as.character(pre_ml_rf_form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  
  ml_rf_kit <<- randomForest(pre_ml_rf_form, data = key_tree_ml, mtry=form_ct)
  live_predict <- predict(ml_log_kit, testtrain, type = "response")
  rf_predict <- predict(ml_rf_kit, testtrain, type = "prob")
  ml_log_out <<- data.frame(testtrain, live_predict, rf_predict) %>%
    mutate(LOGEXPVAL = (PAYOUT * live_predict) + (-1 * (1 - live_predict))) %>%
    mutate(RFEXPVAL = (PAYOUT * X1) + (-1 * X0))
}

#Function to Slot "X" into stage_output_frame5 picks

pre_key_frame <- function(x_df, mtype, btype) {
  
  if (btype == "ATS") {
    a <- 1
    g <- nrow(x_df)
    c <- 1
    
    
    while (a <= g) {
      
      loc_use_a <- as.character(x_df[a, 1])
      log_use_a <- as.numeric(x_df[a, 11])
      rf_use_a <- as.numeric(x_df[a, 13])
      team_use_a <- as.character(x_df[a, 2])
      
      loc_use_h <- as.character(x_df[a + 1, 1])
      log_use_h <- as.numeric(x_df[a + 1, 11])
      rf_use_h <- as.numeric(x_df[a + 1, 13])
      team_use_h <- as.character(x_df[a + 1, 2])
      
      slot_use_a <- pre_key_slot(btype, loc_use_a, mtype)
      slot_use_h <- pre_key_slot(btype, loc_use_h, mtype)
      
      a_pct_comb <- (log_use_a + rf_use_a) / 2
      h_pct_comb <- (log_use_h + rf_use_h) / 2
      
      if (a_pct_comb > h_pct_comb) {
        
        a_mark_spot <- "X"
        h_mark_spot <- "0"
        
      }
      
      else {
        
        a_mark_spot <- "0"
        h_mark_spot <- "X"
        
      }
      
      stage_output_frame_picks[c, slot_use_a] <<- a_mark_spot
      stage_output_frame_picks[c, slot_use_h] <<- h_mark_spot
      
      c <- c + 1
      a <- a + 2
      
    }
  }
  else {
    
    a <- 1
    g <- nrow(x_df)
    
    while (a <= g) {
      
      loc_use_a <- as.character(x_df[a, 1])
      log_use_a <- as.numeric(x_df[a, 11])
      rf_use_a <- as.numeric(x_df[a, 13])
      team_use_a <- as.character(x_df[a, 2])
      
      slot_use_a <- pre_key_slot(btype, loc_use_a, mtype)
      
      a_pct_comb <- (log_use_a + rf_use_a) / 2
      h_pct_comb <- ((1 - log_use_a) + (1 - rf_use_a)) / 2
      
      if (a_pct_comb > h_pct_comb) {
        
        a_mark_spot <- "X"
        
      }
      
      else {
        
        a_mark_spot <- "0"
        
      }
      
      stage_output_frame_picks[c, slot_use_a] <<- a_mark_spot
      
      a <- a + 1
      
    }
  }
}

pre_key_slot <- function(type1, loc1, mtype1) {
  
  if (type1 == "ATS") {
    
    if (loc1 == "A") {

      if (mtype1 == "INDY") { key_slot <- 89 }
      if (mtype1 == "TEAMC") { key_slot <- 95 }
      if (mtype1 == "BLND") { key_slot <- 101 }
      if (mtype1 == "ATM") { key_slot <- 107 }
      if (mtype1 == "SIM") { key_slot <- 113 }
      if (mtype1 == "COMB") { key_slot <- 119 }
      
    }
    
    else {
      
      if (mtype1 == "INDY") { key_slot <- 90 }
      if (mtype1 == "TEAMC") { key_slot <- 96 }
      if (mtype1 == "BLND") { key_slot <- 102 }
      if (mtype1 == "ATM") { key_slot <- 108 }
      if (mtype1 == "SIM") { key_slot <- 114 }
      if (mtype1 == "COMB") { key_slot <- 120 }
      
    }
  }
  
  if (loc1 == "OVER") {
    
    if (mtype1 == "INDY") { key_slot <- 93 }
    if (mtype1 == "TEAMC") { key_slot <- 99 }
    if (mtype1 == "BLND") { key_slot <- 105 }
    if (mtype1 == "ATM") { key_slot <- 111 }
    if (mtype1 == "SIM") { key_slot <- 117 }
    if (mtype1 == "COMB") { key_slot <- 123 }
    
  }
  
  if (loc1 == "UNDER") {
    
    if (mtype1 == "INDY") { key_slot <- 94 }
    if (mtype1 == "TEAMC") { key_slot <- 100 }
    if (mtype1 == "BLND") { key_slot <- 106 }
    if (mtype1 == "ATM") { key_slot <- 112 }
    if (mtype1 == "SIM") { key_slot <- 118 }
    if (mtype1 == "COMB") { key_slot <- 124 }
    
  }
  
  return(key_slot)
}

#Selects Plays Based on Logistic/Random Forest Decision Trees

pre_key_plays2 <- function(m, d, y) {
  
  #-----------ML----------------
  
  log_lo <- tree_ml_matrix[1, 1]
  log_hi <- tree_ml_matrix[1, 2]
  rf_lo <- tree_ml_matrix[1, 3]
  rf_hi <- tree_ml_matrix[1, 4]
  cmb_lo <- tree_ml_matrix[1, 5]
  cmb_hi <- tree_ml_matrix[1, 6]
  log_pct <- tree_ml_matrix[1, 7]
  rf_pct <- tree_ml_matrix[1, 8]
  comb_pct <- tree_ml_matrix[1, 9]
  semi_pct <- tree_ml_matrix[1, 10]
  log_mean <- tree_ml_matrix[1, 11]
  log_sd <- tree_ml_matrix[1, 12]
  rf_mean <- tree_ml_matrix[1, 13]
  rf_sd <- tree_ml_matrix[1, 14]
  
  ml_log_out2 <<- ml_log_out
  
  ml_log_out2[, 18] <<- (ml_log_out2$LOGEXPVAL - log_mean) / log_sd
  ml_log_out2[, 19] <<- (ml_log_out2$RFEXPVAL - rf_mean) / rf_sd
  ml_log_out2[, 20] <<- ((ml_log_out2[, 18] + ml_log_out2[, 19]) / 2)
  
  #LOG ONLY
  
  all_log_ml <<- ml_log_out2 %>%
    filter(LOGEXPVAL >= log_lo & LOGEXPVAL <= log_hi) %>%
    mutate(EDGE = log_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #RF ONLY
  
  all_rf_ml <<- ml_log_out2 %>%
    filter(RFEXPVAL >= rf_lo & RFEXPVAL <= rf_hi) %>%
    mutate(EDGE = rf_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #COMB ONLY
  
  all_comb_ml <<- ml_log_out2 %>%
    filter(V20 >= cmb_lo & V20 <= cmb_hi) %>%
    mutate(EDGE = comb_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #SEMI ONLY
  
  all_semi_ml <<- ml_log_out2 %>%
    filter(RFEXPVAL >= rf_lo & RFEXPVAL <= rf_hi) %>%
    filter(LOGEXPVAL >= log_lo & LOGEXPVAL <= log_hi) %>%
    mutate(EDGE = semi_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  all_ml_bind <<- rbind(all_log_ml, all_rf_ml, all_comb_ml, all_semi_ml) %>%
    group_by(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE)) %>%
    ungroup()
  
  all_ml_bind[, 1] <<- "ML"
  colnames(all_ml_bind)[1] <<- "BTYPE"
  
  #ATS
  
  log_lo <- tree_ats_matrix[1, 1]
  log_hi <- tree_ats_matrix[1, 2]
  rf_lo <- tree_ats_matrix[1, 3]
  rf_hi <- tree_ats_matrix[1, 4]
  cmb_lo <- tree_ats_matrix[1, 5]
  cmb_hi <- tree_ats_matrix[1, 6]
  log_pct <- tree_ats_matrix[1, 7]
  rf_pct <- tree_ats_matrix[1, 8]
  comb_pct <- tree_ats_matrix[1, 9]
  semi_pct <- tree_ats_matrix[1, 10]
  log_mean <- tree_ats_matrix[1, 11]
  log_sd <- tree_ats_matrix[1, 12]
  rf_mean <- tree_ats_matrix[1, 13]
  rf_sd <- tree_ats_matrix[1, 14]
  
  ats_log_out2 <<- ats_log_out %>%
    mutate(LOGPLAY = ifelse(live_predict >= .5, 1, 0)) %>%
    mutate(LOGPCT = ifelse(live_predict >= .5, live_predict, 1 - live_predict)) %>%
    mutate(RFPLAY = ifelse(X1 > X0, 1, 0)) %>%
    mutate(RFPCT = ifelse(X1 > X0, X1, X0))
  
  ats_log_out2[, 18] <<- (ats_log_out2$LOGPCT - log_mean) / log_sd
  ats_log_out2[, 19] <<- (ats_log_out2$RFPCT - rf_mean) / rf_sd
  ats_log_out2[, 20] <<- ((ats_log_out2[, 18] + ats_log_out2[, 19]) / 2)

  #ATS LOG
  
  all_log_ats <<- ats_log_out2 %>%
    filter(LOGPCT >= log_lo & LOGPCT <= log_hi) %>%
    mutate(EDGE = log_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, LOGPLAY, LOGPCT, EDGE)
  
  a <- 1
  g <- nrow(all_log_ats)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_log_ats$LOGPLAY[a] == 0) {
        
        tm <- all_log_ats$TEAM[a]
        op <- all_log_ats$OPP[a]
        ln <- all_log_ats$LINE[a]
        all_log_ats$TEAM[a] <<- op
        all_log_ats$OPP[a] <<- tm
        all_log_ats$LINE[a] <<- ln * -1
        all_log_ats[a, 5:10] <<- all_log_ats[a, 5:10] * -1
        
      }
    }
  }
  
  all_log_ats <<- all_log_ats %>%
    select(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE)) %>%
    mutate(`BET TYPE` = "ATS") %>%
    select(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #ATS RF
  
  all_rf_ats <<- ats_log_out2 %>%
    filter(RFPCT >= rf_lo & RFPCT <= rf_hi) %>%
    mutate(EDGE = rf_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, RFPLAY, RFPCT, EDGE)
  
  a <- 1
  g <- nrow(all_rf_ats)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_rf_ats$RFPLAY[a] == 0) {
        
        tm <- all_rf_ats$TEAM[a]
        op <- all_rf_ats$OPP[a]
        ln <- all_rf_ats$LINE[a]
        all_rf_ats$TEAM[a] <<- op
        all_rf_ats$OPP[a] <<- tm
        all_rf_ats$LINE[a] <<- ln * -1
        all_rf_ats[a, 5:10] <<- all_rf_ats[a, 5:10] * -1
        
      }
    }
  }
  
  all_rf_ats <<- all_rf_ats %>%
    select(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE)) %>%
    mutate(`BET TYPE` = "ATS") %>%
    select(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #COMBO RF
  
  all_comb_ats <<- ats_log_out2 %>%
    filter(V20 >= cmb_lo & V20 <= cmb_hi) %>%
    filter(RFPLAY == LOGPLAY) %>%
    mutate(EDGE = comb_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, RFPLAY, V20, EDGE)
  
  a <- 1
  g <- nrow(all_comb_ats)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_comb_ats$RFPLAY[a] == 0) {
        
        tm <- all_comb_ats$TEAM[a]
        op <- all_comb_ats$OPP[a]
        ln <- all_comb_ats$LINE[a]
        all_comb_ats$TEAM[a] <<- op
        all_comb_ats$OPP[a] <<- tm
        all_comb_ats$LINE[a] <<- ln * -1
        all_comb_ats[a, 5:10] <<- all_comb_ats[a, 5:10] * -1
        
      }
    }
  }
  
  all_comb_ats <<- all_comb_ats %>%
    select(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE)) %>%
    mutate(`BET TYPE` = "ATS") %>%
    select(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #ATS SEMI
  
  all_semi_ats <<- ats_log_out2 %>%
    filter(RFPCT >= rf_lo & RFPCT <= rf_hi) %>%
    filter(LOGPCT >= log_lo & LOGPCT <= log_hi) %>%
    filter(RFPLAY == LOGPLAY) %>%
    mutate(EDGE = semi_pct) %>%
    select(LOC, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, RFPLAY, RFPCT, EDGE)
  
  a <- 1
  g <- nrow(all_semi_ats)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_semi_ats$RFPLAY[a] == 0 & g > 0) {
        
        tm <- all_semi_ats$TEAM[a]
        op <- all_semi_ats$OPP[a]
        ln <- all_semi_ats$LINE[a]
        all_semi_ats$TEAM[a] <<- op
        all_semi_ats$OPP[a] <<- tm
        all_semi_ats$LINE[a] <<- ln * -1
        all_semi_ats[a, 5:10] <<- all_semi_ats[a, 5:10] * -1
        
      }
    }
  }
  
  all_semi_ats <<- all_semi_ats %>%
    select(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE)) %>%
    mutate(`BET TYPE` = "ATS") %>%
    select(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  all_ats_bind <<- rbind(all_log_ats, all_rf_ats, all_comb_ats, all_semi_ats) %>%
    mutate(BTYPE = "ATS") %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE))
  
  
  #-----------TOTALS----------------
  
  log_lo <- tree_total_matrix[1, 1]
  log_hi <- tree_total_matrix[1, 2]
  rf_lo <- tree_total_matrix[1, 3]
  rf_hi <- tree_total_matrix[1, 4]
  cmb_lo <- tree_total_matrix[1, 5]
  cmb_hi <- tree_total_matrix[1, 6]
  log_pct <- tree_total_matrix[1, 7]
  rf_pct <- tree_total_matrix[1, 8]
  comb_pct <- tree_total_matrix[1, 9]
  semi_pct <- tree_total_matrix[1, 10]
  log_mean <- tree_total_matrix[1, 11]
  log_sd <- tree_total_matrix[1, 12]
  rf_mean <- tree_total_matrix[1, 13]
  rf_sd <- tree_total_matrix[1, 14]
  
  total_log_out2 <<- total_log_out %>%
    mutate(LOGPLAY = ifelse(live_predict >= .5, 1, 0)) %>%
    mutate(LOGPCT = ifelse(live_predict >= .5, live_predict, 1 - live_predict)) %>%
    mutate(RFPLAY = ifelse(X1 > X0, 1, 0)) %>%
    mutate(RFPCT = ifelse(X1 > X0, X1, X0))
  
  total_log_out2[, 18] <<- (total_log_out2$LOGPCT - log_mean) / log_sd
  total_log_out2[, 19] <<- (total_log_out2$RFPCT - rf_mean) / rf_sd
  total_log_out2[, 20] <<- ((total_log_out2[, 18] + total_log_out2[, 19]) / 2)
  
  #total LOG
  
  all_log_total <<- total_log_out2 %>%
    filter(LOGPCT >= log_lo & LOGPCT <= log_hi) %>%
    mutate(EDGE = log_pct) %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, LOGPLAY, LOGPCT, EDGE)
  
  a <- 1
  g <- nrow(all_log_total)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_log_total$LOGPLAY[a] == 1) { all_log_total$BTYPE[a] <<- "UNDER" }
    }
  }
  
  all_log_total <<- all_log_total %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE))
  
  #total RF
  
  all_rf_total <<- total_log_out2 %>%
    filter(RFPCT >= rf_lo & RFPCT <= rf_hi) %>%
    mutate(EDGE = rf_pct) %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, RFPLAY, RFPCT, EDGE)
  
  a <- 1
  g <- nrow(all_rf_total)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_rf_total$RFPLAY[a] == 1) { all_rf_total$BTYPE[a] <<- "UNDER" }
    }
  }
  
  all_rf_total <<- all_rf_total %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE))
  
  #total COMB
  
  all_comb_total <<- total_log_out2 %>%
    filter(V20 >= cmb_lo & V20 <= cmb_hi) %>%
    filter(RFPLAY == LOGPLAY) %>%
    mutate(EDGE = comb_pct) %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, RFPLAY, V20, EDGE)
  
  a <- 1
  g <- nrow(all_comb_total)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_comb_total$RFPLAY[a] == 1) { all_comb_total$BTYPE[a] <<- "UNDER" }
    }
  }

  all_comb_total <<- all_comb_total %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE))
  
  #total SEMI
  
  all_semi_total <<- total_log_out2 %>%
    filter(RFPLAY == LOGPLAY) %>%
    filter(RFPCT >= rf_lo & RFPCT <= rf_hi) %>%
    filter(LOGPCT >= log_lo & LOGPCT <= log_hi) %>%
    mutate(EDGE = semi_pct) %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, LOGPCT, RFPCT, RFPLAY, EDGE)
  
  a <- 1
  g <- nrow(all_semi_total)
  if (g > 0) {
    for (a in a:g) {
      
      if (all_semi_total$RFPLAY[a] == 1) { all_semi_total$BTYPE[a] <<- "UNDER" }
    }
  }
  all_semi_total <<- all_semi_total %>%
    select(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE))
  
  all_total_bind <<- rbind(all_log_total, all_rf_total, all_comb_total, all_semi_total) %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE))
  
  all_ats_bind <<- all_ats_bind %>%
    ungroup()
  
  all_total_bind <<- all_total_bind %>%
    ungroup()
  
  whole_plays_bind <<- rbind(all_ats_bind, all_total_bind, all_ml_bind)
  wb_picks <- whole_plays_bind %>%
    mutate(MONTH = m) %>%
    mutate(DAY = d) %>%
    mutate(YEAR = y)
  
  write_csv(wb_picks, "Todays Plays.csv")
  write_csv(whole_plays_bind, "Todays Plays Multi.csv")
}
  


pre_key_plays <- function(m, d, y) {
  
  #ATS
  
  a <- 1
  g <- length(which(tree_ats_matrix[, 5] != 0))
  ct <- 0
  
  for (a in a:g) {
    
    log_lo <- tree_ats_matrix[a, 1]
    log_hi <- tree_ats_matrix[a, 2]
    rf_lo <- tree_ats_matrix[a, 3]
    rf_hi <- tree_ats_matrix[a, 4]
    log_pct <- tree_ats_matrix[a, 5]
    rf_pct <- tree_ats_matrix[a, 6]
    comb_pct <- tree_ats_matrix[a, 7]
    
    all_log_ats <<- ats_log_out %>%
      filter(live_predict >= log_lo & live_predict <= log_hi) %>%
      mutate(EDGE = log_pct)
    
    all_rf_ats <<- ats_log_out %>%
      filter(X1 >= rf_lo & X1 <= rf_hi) %>%
      mutate(EDGE = rf_pct)
    
    all_comb_ats <<- ats_log_out %>%
      filter(live_predict >= log_lo & live_predict <= log_hi) %>%
      filter(X1 >= rf_lo & X1 <= rf_hi) %>%
      mutate(EDGE = comb_pct)
    
    if (ct == 0) { 
      all_log_ats_whole <<- rbind(all_log_ats, all_rf_ats, all_comb_ats)
      ct <- ct + 1
    }
    else {
      all_log_ats_whole2 <<- rbind(all_log_ats, all_rf_ats, all_comb_ats)
      all_log_ats_whole <<- rbind(all_log_ats_whole, all_log_ats_whole2)
    }
  }
  
  all_log_cut_ats_bind <<- all_log_ats_whole %>%
    mutate(`BET TYPE` = "ATS")
  
  colnames(all_log_cut_ats_bind)[1] <<- "BTYPE"
  
  #OVER
  
  a <- 1
  g <- length(which(tree_over_matrix[, 5] != 0))
  ct <- 0
  
  for (a in a:g) {
    
    log_lo <- tree_over_matrix[a, 1]
    log_hi <- tree_over_matrix[a, 2]
    rf_lo <- tree_over_matrix[a, 3]
    rf_hi <- tree_over_matrix[a, 4]
    log_pct <- tree_over_matrix[a, 5]
    rf_pct <- tree_over_matrix[a, 6]
    comb_pct <- tree_over_matrix[a, 7]
    
    all_log_over <<- over_log_out %>%
      filter(live_predict >= log_lo & live_predict <= log_hi) %>%
      mutate(EDGE = log_pct)
    
    all_rf_over <<- over_log_out %>%
      filter(X1 >= rf_lo & X1 <= rf_hi) %>%
      mutate(EDGE = rf_pct)
    
    all_comb_over <<- over_log_out %>%
      filter(live_predict >= log_lo & live_predict <= log_hi) %>%
      filter(X1 >= rf_lo & X1 <= rf_hi) %>%
      mutate(EDGE = comb_pct)
    
    if (ct == 0) { 
      all_log_over_whole <<- rbind(all_log_over, all_rf_over, all_comb_over)
      ct <- ct + 1
    }
    else {
      all_log_over_whole2 <<- rbind(all_log_over, all_rf_over, all_comb_over)
      all_log_over_whole <<- rbind(all_log_over_whole, all_log_over_whole2)
    }
  }
  
  all_log_cut_over_bind <<- all_log_over_whole %>%
    mutate(`BET TYPE` = "OVER")
  
  #UNDER
  
  a <- 1
  g <- length(which(tree_under_matrix[, 5] != 0))
  ct <- 0
  
  for (a in a:g) {
    
    log_lo <- tree_under_matrix[a, 1]
    log_hi <- tree_under_matrix[a, 2]
    rf_lo <- tree_under_matrix[a, 3]
    rf_hi <- tree_under_matrix[a, 4]
    log_pct <- tree_under_matrix[a, 5]
    rf_pct <- tree_under_matrix[a, 6]
    comb_pct <- tree_under_matrix[a, 7]
    
    all_log_under <<- under_log_out %>%
      filter(live_predict >= log_lo & live_predict <= log_hi) %>%
      mutate(EDGE = log_pct)
    
    all_rf_under <<- under_log_out %>%
      filter(X1 >= rf_lo & X1 <= rf_hi) %>%
      mutate(EDGE = rf_pct)
    
    all_comb_under <<- under_log_out %>%
      filter(live_predict >= log_lo & live_predict <= log_hi) %>%
      filter(X1 >= rf_lo & X1 <= rf_hi) %>%
      mutate(EDGE = comb_pct)
    
    if (ct == 0) { 
      all_log_under_whole <<- rbind(all_log_under, all_rf_under, all_comb_under)
      ct <- ct + 1
    }
    else {
      all_log_under_whole2 <<- rbind(all_log_under, all_rf_under, all_comb_under)
      all_log_under_whole <<- rbind(all_log_under_whole, all_log_under_whole2)
    }
  }
  
  all_log_cut_under_bind <<- all_log_under_whole %>%
    mutate(`BET TYPE` = "UNDER")
  
  all_log_cut <<- rbind(all_log_cut_ats_bind, all_log_cut_over_bind, all_log_cut_under_bind)
  
  all_log_cut <<- all_log_cut %>%
    group_by(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE) %>%
    summarize(ct = n())
  
  web_plays <- all_log_cut[, -12] %>%
    group_by(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(EDGE = max(EDGE))
  
  web_plays_m <- web_plays
  web_plays_d <- web_plays
  web_plays_m[, 12:13] <- 1
  
  write_csv(web_plays_m, "Todays Plays Multi.csv")
  
  web_plays_d[, 11] <- m
  web_plays_d[, 12] <- d
  web_plays_d[, 13] <- y
  colnames(web_plays_d)[11:13] <- c("MONTH", "DAY", "YEAR")
  
  #write_csv(web_plays_d, "Todays Plays.csv")
}
  
  
 

#------------------------------------------
#LIVE PLAYS
#------------------------------------------

livelog_step_loop <- function(xxxx) {
  
  livelog_step(1)
  livelog_step_comb(1)
  print(summary(ats_fit_2h_nocomb))
  print(summary(ats_fit_2h_comb))
  print(summary(over_fit_2h_nocomb))
  print(summary(over_fit_2h_comb))
  print(summary(under_fit_2h_nocomb))
  print(summary(under_fit_2h_comb))

  #NOCOMB, #NOCOMB, #NOCOMB
  
}

livelog_file_prepare <- function(xxxx) {
  
  live_log <<- read_csv("livelog.csv")
  
  res_log <- team_game_by_game_raw[, c(3, 5, 10)]
  res_log <- res_log %>%
    left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
    select(`Ken Pom`, DATE, PTS) %>%
    mutate(DATE2 = mdy(DATE)) %>%
    select(`Ken Pom`, DATE2, PTS) %>%
    mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
    select(-DATE2)
  
  live_log_join <- live_log %>%
    inner_join(res_log, by = c("AWAY" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year")) %>%
    inner_join(res_log, by = c("HOME" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year"))
  
  ats_log_2h <<- live_log_join %>%
    filter(BETTYPE == "ATS") %>%
    mutate(MARGIN = ifelse(AORH == "A", PTS.x - PTS.y, PTS.y - PTS.x)) %>%
    mutate(ATSMARG = MARGIN + LIVELINE) %>%
    mutate(RES = ifelse(MARGIN + LIVELINE == 0, 0, ifelse(MARGIN + LIVELINE > 0, 1, -1.1))) %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  ats_log_2h$RESFAC <<- as.factor(ats_log_2h$RESFAC)
  
  over_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "O") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSMARG = GAMETOTAL - LIVELINE) %>%
    mutate(RES = ifelse(ATSMARG == 0, 0, ifelse(ATSMARG > 0, 1, -1.1))) %>%
    mutate(RESFAC = ifelse(RES > 0, 0, 1))
  
  over_log_2h$RESFAC <<- as.factor(over_log_2h$RESFAC)
  
  under_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "U") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSMARG = LIVELINE - GAMETOTAL) %>%
    mutate(RES = ifelse(ATSMARG == 0, 0, ifelse(ATSMARG > 0, 1, -1.1))) %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  under_log_2h$RESFAC <<- as.factor(under_log_2h$RESFAC)
  
  colnames(ats_log_2h)[11:16] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  colnames(over_log_2h)[11:16] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  colnames(under_log_2h)[11:16] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  colnames(ats_log_2h)[6:7] <<- c("TEAM", "OPP")
  colnames(over_log_2h)[6:7] <<- c("TEAM", "OPP")
  colnames(under_log_2h)[6:7] <<- c("TEAM", "OPP")

}

livelog_step_use <- function(xxxx) {
  
  live_log <<- read_csv("livelog.csv")
  
  res_log <- team_game_by_game_raw[, c(3, 5, 10)]
  res_log <- res_log %>%
    left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
    select(`Ken Pom`, DATE, PTS) %>%
    mutate(DATE2 = mdy(DATE)) %>%
    select(`Ken Pom`, DATE2, PTS) %>%
    mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
    select(-DATE2)
  
  live_log_join <- live_log %>%
    inner_join(res_log, by = c("AWAY" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year")) %>%
    inner_join(res_log, by = c("HOME" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year"))
  
  ats_log_2h <<- live_log_join %>%
    filter(BETTYPE == "ATS") %>%
    mutate(MARGIN = ifelse(AORH == "A", PTS.x - PTS.y, PTS.y - PTS.x)) %>%
    mutate(ATSMARG = MARGIN + LIVELINE) %>%
    mutate(ATSRES = ifelse((MARGIN + LIVELINE) > 0, 1, 0))
  
  ats_log_2h$ATSRES <<- as.factor(ats_log_2h$ATSRES)
  
  print("hi")
  
  over_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "O") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSRES = ifelse(GAMETOTAL > LIVELINE, 1, 0))
  
  over_log_2h$ATSRES <<- as.factor(over_log_2h$ATSRES)
  
  under_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "U") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSRES = ifelse(GAMETOTAL < LIVELINE, 1, 0))
  
  under_log_2h$ATSRES <<- as.factor(under_log_2h$ATSRES)
  
  ats_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = ats_log_2h, family = "binomial"), direction='backward')
  over_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = over_log_2h, family = "binomial"), direction='backward')
  under_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = under_log_2h, family = "binomial"), direction='backward')
  
  ats_log_vals <- length(ats_log_fit_2h$coefficients)
  a <- 2
  g <- ats_log_vals
  for (a in a:g) {
    ats_log_name <- names(ats_log_fit_2h$coefficients)[a]
    if (a == 2) { ats_log_form <- paste("ATSRES ~", ats_log_name) }
    else { ats_log_form <- paste(ats_log_form, "+", ats_log_name) }
  }
  ats_log_form_2h <<- formula(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG)
  ats_rf_fit_2h <<- randomForest(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = ats_log_2h)
  
  over_log_vals <- length(over_log_fit_2h$coefficients)
  a <- 2
  g <- over_log_vals
  for (a in a:g) {
    over_log_name <- names(over_log_fit_2h$coefficients)[a]
    if (a == 2) { over_log_form <- paste("ATSRES ~", over_log_name) }
    else { over_log_form <- paste(over_log_form, "+", over_log_name) }
  }
  over_log_form_2h <<- formula(over_log_form)
  over_rf_fit_2h <<- randomForest(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = over_log_2h)
  
  under_log_vals <- length(under_log_fit_2h$coefficients)
  a <- 2
  g <- under_log_vals
  for (a in a:g) {
    under_log_name <- names(under_log_fit_2h$coefficients)[a]
    if (a == 2) { under_log_form <- paste("ATSRES ~", under_log_name) }
    else { under_log_form <- paste(under_log_form, "+", under_log_name) }
  }
  under_log_form_2h <<- formula(under_log_form)
  under_rf_fit_2h <<- randomForest(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = under_log_2h)
  
}

livelog_step <- function(xxxx) {
  
  live_log <<- read_csv("livelog.csv")
  
  res_log <- team_game_by_game_raw[, c(3, 5, 10)]
  res_log <- res_log %>%
    left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
    select(`Ken Pom`, DATE, PTS) %>%
    mutate(DATE2 = mdy(DATE)) %>%
    select(`Ken Pom`, DATE2, PTS) %>%
    mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
    select(-DATE2)
  
  live_log_join <- live_log %>%
    inner_join(res_log, by = c("AWAY" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year")) %>%
    inner_join(res_log, by = c("HOME" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year"))
  
  ats_log_2h <<- live_log_join %>%
    filter(BETTYPE == "ATS") %>%
    mutate(MARGIN = ifelse(AORH == "A", PTS.x - PTS.y, PTS.y - PTS.x)) %>%
    mutate(ATSMARG = MARGIN + LIVELINE) %>%
    mutate(ATSRES = ifelse((MARGIN + LIVELINE) > 0, 1, 0))
  
  ats_log_2h$ATSRES <<- as.factor(ats_log_2h$ATSRES)
  
  over_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "O") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSRES = ifelse(GAMETOTAL > LIVELINE, 1, 0))
  
  over_log_2h$ATSRES <<- as.factor(over_log_2h$ATSRES)
  
  under_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "U") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSRES = ifelse(GAMETOTAL < LIVELINE, 1, 0))
  
  under_log_2h$ATSRES <<- as.factor(under_log_2h$ATSRES)
  
  print("ATS NOCOMB")
  ats_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = ats_log_2h, family = "binomial"), direction='backward')
  print("OVER NOCOMB")
  over_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = over_log_2h, family = "binomial"), direction='backward')
  print("UNDER NOCOMB")
  under_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG, data = under_log_2h, family = "binomial"), direction='backward')
  
  ats_fit_2h_nocomb <<- ats_log_fit_2h
  over_fit_2h_nocomb <<- over_log_fit_2h
  under_fit_2h_nocomb <<- under_log_fit_2h
  
  ats_log_vals <- length(ats_log_fit_2h$coefficients)
  a <- 2
  g <- ats_log_vals
  for (a in a:g) {
    ats_log_name <- names(ats_log_fit_2h$coefficients)[a]
    if (a == 2) { ats_log_form <- paste("ATSRES ~", ats_log_name) }
    else { ats_log_form <- paste(ats_log_form, "+", ats_log_name) }
  }
  ats_log_form_2h <<- formula(ats_log_form)
  ats_rf_fit_2h <<- randomForest(ats_log_form_2h, data = ats_log_2h)
  
  over_log_vals <- length(over_log_fit_2h$coefficients)
  a <- 2
  g <- over_log_vals
  for (a in a:g) {
    over_log_name <- names(over_log_fit_2h$coefficients)[a]
    if (a == 2) { over_log_form <- paste("ATSRES ~", over_log_name) }
    else { over_log_form <- paste(over_log_form, "+", over_log_name) }
  }
  over_log_form_2h <<- formula(over_log_form)
  over_rf_fit_2h <<- randomForest(over_log_form_2h, data = over_log_2h)
  
  under_log_vals <- length(under_log_fit_2h$coefficients)
  a <- 2
  g <- under_log_vals
  for (a in a:g) {
    under_log_name <- names(under_log_fit_2h$coefficients)[a]
    if (a == 2) { under_log_form <- paste("ATSRES ~", under_log_name) }
    else { under_log_form <- paste(under_log_form, "+", under_log_name) }
  }
  under_log_form_2h <<- formula(under_log_form)
  under_rf_fit_2h <<- randomForest(under_log_form_2h, data = under_log_2h)
  
}

livelog_step_comb <- function(xxxx) {
  
  live_log <<- read_csv("livelog.csv")
  
  res_log <- team_game_by_game_raw[, c(3, 5, 10)]
  res_log <- res_log %>%
    left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
    select(`Ken Pom`, DATE, PTS) %>%
    mutate(DATE2 = mdy(DATE)) %>%
    select(`Ken Pom`, DATE2, PTS) %>%
    mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
    select(-DATE2)
  
  live_log_join <- live_log %>%
    inner_join(res_log, by = c("AWAY" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year")) %>%
    inner_join(res_log, by = c("HOME" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year"))
  
  ats_log_2h <<- live_log_join %>%
    filter(BETTYPE == "ATS") %>%
    mutate(MARGIN = ifelse(AORH == "A", PTS.x - PTS.y, PTS.y - PTS.x)) %>%
    mutate(ATSMARG = MARGIN + LIVELINE) %>%
    mutate(ATSRES = ifelse((MARGIN + LIVELINE) > 0, 1, 0))
  
  ats_log_2h$ATSRES <<- as.factor(ats_log_2h$ATSRES)
  
  print("hi")
  
  over_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "O") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSRES = ifelse(GAMETOTAL > LIVELINE, 1, 0))
  
  over_log_2h$ATSRES <<- as.factor(over_log_2h$ATSRES)
  
  under_log_2h <<- live_log_join %>%
    filter(BETTYPE == "TOTAL" & AORH == "U") %>%
    mutate(GAMETOTAL = PTS.x + PTS.y) %>%
    mutate(ATSRES = ifelse(GAMETOTAL < LIVELINE, 1, 0))
  
  under_log_2h$ATSRES <<- as.factor(under_log_2h$ATSRES)
  
  print("ATS COMB")
  ats_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG + CATSMARG, data = ats_log_2h, family = "binomial"), direction='backward')
  print("OVER COMB")
  over_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG + CATSMARG, data = over_log_2h, family = "binomial"), direction='backward')
  print("UNDER COMB")
  under_log_fit_2h <<- step(glm(ATSRES ~ IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG + CATSMARG, data = under_log_2h, family = "binomial"), direction='backward')
  
  ats_fit_2h_comb <<- ats_log_fit_2h
  over_fit_2h_comb <<- over_log_fit_2h
  under_fit_2h_comb <<- under_log_fit_2h
  
  ats_log_vals <- length(ats_log_fit_2h$coefficients)
  a <- 2
  g <- ats_log_vals
  for (a in a:g) {
    ats_log_name <- names(ats_log_fit_2h$coefficients)[a]
    if (a == 2) { ats_log_form <- paste("ATSRES ~", ats_log_name) }
    else { ats_log_form <- paste(ats_log_form, "+", ats_log_name) }
  }
  ats_log_form_2h <<- formula(ats_log_form)
  ats_rf_fit_2h <<- randomForest(ats_log_form_2h, data = ats_log_2h)
  
  over_log_vals <- length(over_log_fit_2h$coefficients)
  a <- 2
  g <- over_log_vals
  for (a in a:g) {
    over_log_name <- names(over_log_fit_2h$coefficients)[a]
    if (a == 2) { over_log_form <- paste("ATSRES ~", over_log_name) }
    else { over_log_form <- paste(over_log_form, "+", over_log_name) }
  }
  over_log_form_2h <<- formula(over_log_form)
  over_rf_fit_2h <<- randomForest(over_log_form_2h, data = over_log_2h)
  
  under_log_vals <- length(under_log_fit_2h$coefficients)
  a <- 2
  g <- under_log_vals
  for (a in a:g) {
    under_log_name <- names(under_log_fit_2h$coefficients)[a]
    if (a == 2) { under_log_form <- paste("ATSRES ~", under_log_name) }
    else { under_log_form <- paste(under_log_form, "+", under_log_name) }
  }
  under_log_form_2h <<- formula(under_log_form)
  under_rf_fit_2h <<- randomForest(under_log_form_2h, data = under_log_2h)
  
}
  
live_odds <- function(xxxx) {
  
  ats_log_fit_2h <<- glm(live_ats_log_form, data = live_tree_ats, family = 'binomial')
  form_txt <- as.character(live_ats_rf_form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  ats_rf_fit_2h <<- randomForest(live_ats_rf_form, data = live_tree_ats, mtry=form_ct)
  
  tot_log_fit_2h <<- glm(live_total_log_form, data = live_tree_tot, family = 'binomial')
  form_txt <- as.character(live_total_rf_form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  tot_rf_fit_2h <<- randomForest(live_total_rf_form, data = live_tree_tot, mtry=form_ct)
  
  live_data <<- read_csv("livedata.csv")
  colnames(live_data)[11:16] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  print(live_data)
  
  
  ats_live_data <<- live_data %>%
    filter(BETTYPE == "ATS")
  
  over_live_data <<- live_data %>%
    filter(AORH == "O")

  if (nrow(ats_live_data) > 0) { 
    ats_log_predict <<- predict(ats_log_fit_2h, ats_live_data, type = "response")
    ats_rf_predict <<- predict(ats_rf_fit_2h, ats_live_data, type = "prob")
    ats_log_out <<- data.frame(ats_live_data, ats_log_predict, ats_rf_predict)
    
    colnames(ats_log_out)[17:19] <<- c("LOGODDS", "RF0", "RF1")
  }
  
  if (nrow(over_live_data) > 0) {
    over_log_predict <<- predict(tot_log_fit_2h, over_live_data, type = "response")
    over_rf_predict <<- predict(tot_rf_fit_2h, over_live_data, type = "prob")
    tot_log_out <<- data.frame(over_live_data, over_log_predict, over_rf_predict)
    tot_log_out <<- tot_log_out %>%
      mutate(OVERODDS = 1 - over_log_predict) %>%
      mutate(LOGPICK = ifelse(OVERODDS > over_log_predict, 0, 1)) %>%
      mutate(RFPICK = ifelse(X0 > X1, 0, 1)) %>%
      mutate(ACTLOGODDS = ifelse(OVERODDS > over_log_predict, 1 - over_log_predict, over_log_predict)) %>%
      mutate(ACTRFODDS = ifelse(X0 > X1, X0, X1))
    
    colnames(tot_log_out)[17:19] <<- c("UNDERODDS", "RFOVER", "RFUNDER")
  }

  #ATS
  
  log_lo <- tree_2h_ats_matrix[1, 1]
  log_hi <- tree_2h_ats_matrix[1, 2]
  rf_lo <- tree_2h_ats_matrix[1, 3]
  rf_hi <- tree_2h_ats_matrix[1, 4]
  cmb_lo <- tree_2h_ats_matrix[1, 5]
  cmb_hi <- tree_2h_ats_matrix[1, 6]
  log_pct <- tree_2h_ats_matrix[1, 7]
  rf_pct <- tree_2h_ats_matrix[1, 8]
  comb_pct <- tree_2h_ats_matrix[1, 9]
  semi_pct <- tree_2h_ats_matrix[1, 10]
  log_mean <- tree_2h_ats_matrix[1, 11]
  log_sd <- tree_2h_ats_matrix[1, 12]
  rf_mean <- tree_2h_ats_matrix[1, 13]
  rf_sd <- tree_2h_ats_matrix[1, 14]

  
  ats_log_out2 <<- ats_log_out
  
  ats_log_out2[, 20] <<- (ats_log_out2$LOGODDS- log_mean) / log_sd
  ats_log_out2[, 21] <<- (ats_log_out2$RF1 - rf_mean) / rf_sd
  ats_log_out2[, 22] <<- ((ats_log_out2[, 20] + ats_log_out2[, 21]) / 2)
  
  #ATS LOG ONLY
  
  all_log_ats <<- ats_log_out2 %>%
    filter(LOGODDS >= log_lo & LOGODDS <= log_hi) %>%
    mutate(EDGE = log_pct) %>%
    select(AORH, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #ATS RF ONLY
  
  all_rf_ats <<- ats_log_out2 %>%
    filter(RF1 >= rf_lo & RF1 <= rf_hi) %>%
    mutate(EDGE = rf_pct) %>%
    select(AORH, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #ATS COMB ONLY
  
  all_comb_ats <<- ats_log_out2 %>%
    filter(V22 >= cmb_lo & V22 <= cmb_hi) %>%
    mutate(EDGE = comb_pct) %>%
    select(AORH, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  #ATS SEMI ONLY
  
  all_semi_ats <<- ats_log_out2 %>%
    filter(LOGODDS >= log_lo & LOGODDS <= log_hi) %>%
    filter(RF1 >= rf_lo & RF1 <= rf_hi) %>%
    mutate(EDGE = semi_pct) %>%
    select(AORH, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  all_ats_bind <<- rbind(all_log_ats, all_rf_ats, all_comb_ats, all_semi_ats) %>%
    group_by(AORH, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(CT = n(), MAXEDGE = max(EDGE)) %>%
    ungroup()
  
  #TOTAL
  
  log_lo <- tree_2h_total_matrix[1, 1]
  log_hi <- tree_2h_total_matrix[1, 2]
  rf_lo <- tree_2h_total_matrix[1, 3]
  rf_hi <- tree_2h_total_matrix[1, 4]
  cmb_lo <- tree_2h_total_matrix[1, 5]
  cmb_hi <- tree_2h_total_matrix[1, 6]
  log_pct <- tree_2h_total_matrix[1, 7]
  rf_pct <- tree_2h_total_matrix[1, 8]
  comb_pct <- tree_2h_total_matrix[1, 9]
  semi_pct <- tree_2h_total_matrix[1, 10]
  log_mean <- tree_2h_total_matrix[1, 11]
  log_sd <- tree_2h_total_matrix[1, 12]
  rf_mean <- tree_2h_total_matrix[1, 13]
  rf_sd <- tree_2h_total_matrix[1, 14]
  
  tot_log_out2 <<- tot_log_out
  
  tot_log_out2[, 25] <<- (tot_log_out2$ACTLOGODDS- log_mean) / log_sd
  tot_log_out2[, 26] <<- (tot_log_out2$ACTRFODDS - rf_mean) / rf_sd
  tot_log_out2[, 27] <<- ((tot_log_out2[, 25] + tot_log_out2[, 26]) / 2)
  
  #ATS LOG ONLY
  
  all_log_tot <<- tot_log_out2 %>%
    filter(ACTLOGODDS >= log_lo & ACTLOGODDS <= log_hi) %>%
    mutate(EDGE = log_pct) %>%
    select(LOGPICK, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  colnames(all_log_tot)[1] <<- "RFPICK"
  
  all_rf_tot <<- tot_log_out2 %>%
    filter(ACTRFODDS >= rf_lo & ACTRFODDS <= rf_hi) %>%
    mutate(EDGE = rf_pct) %>%
    select(RFPICK, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  all_comb_tot <<- tot_log_out2 %>%
    filter(RFPICK == LOGPICK) %>%
    filter(V27 >= cmb_lo & V27 <= cmb_hi) %>%
    mutate(EDGE = comb_pct) %>%
    select(RFPICK, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  all_semi_tot <<- tot_log_out2 %>%
    filter(RFPICK == LOGPICK) %>%
    filter(ACTLOGODDS >= log_lo & ACTLOGODDS <= log_hi) %>%
    filter(ACTRFODDS >= rf_lo & ACTRFODDS <= rf_hi) %>%
    mutate(EDGE = semi_pct) %>%
    select(RFPICK, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB, EDGE)
  
  all_tot_bind <<- rbind(all_log_tot, all_rf_tot, all_comb_tot, all_semi_tot) %>%
    mutate(AORH = ifelse(RFPICK == 1, "U", "O")) %>%
    group_by(AORH, AWAY, HOME, LIVELINE, CURMARG, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(CT = n(), MAXEDGE = max(EDGE)) %>%
    ungroup()
  
  all_live_bind <<- rbind(all_ats_bind, all_tot_bind)
  View(all_live_bind)
}
  
  

line_redshift <- function(type_use, line_no, inc_no) {
  
  if (type_use == "ATS") {
      
    master_st_lineup_ats[line_no, 4] <<- master_st_lineup_ats[line_no, 4] + inc_no
    master_st_lineup_ats[line_no, 5] <<- master_st_lineup_ats[line_no, 5] + inc_no
    master_st_lineup_ats[line_no, 6] <<- master_st_lineup_ats[line_no, 6] + inc_no
    master_st_lineup_ats[line_no, 7] <<- master_st_lineup_ats[line_no, 7] + inc_no
    master_st_lineup_ats[line_no, 8] <<- master_st_lineup_ats[line_no, 8] + inc_no
    master_st_lineup_ats[line_no, 9] <<- master_st_lineup_ats[line_no, 9] + inc_no
    master_st_lineup_ats[line_no, 10] <<- master_st_lineup_ats[line_no, 10] + inc_no
    
    
    master_st_lineup_ats[line_no + 1, 4] <<- master_st_lineup_ats[line_no + 1, 4] - inc_no
    master_st_lineup_ats[line_no + 1, 5] <<- master_st_lineup_ats[line_no + 1, 5] - inc_no
    master_st_lineup_ats[line_no + 1, 6] <<- master_st_lineup_ats[line_no + 1, 6] - inc_no
    master_st_lineup_ats[line_no + 1, 7] <<- master_st_lineup_ats[line_no + 1, 7] - inc_no
    master_st_lineup_ats[line_no + 1, 8] <<- master_st_lineup_ats[line_no + 1, 8] - inc_no
    master_st_lineup_ats[line_no + 1, 9] <<- master_st_lineup_ats[line_no + 1, 9] - inc_no
    master_st_lineup_ats[line_no + 1, 10] <<- master_st_lineup_ats[line_no + 1, 10] - inc_no
    
    testtrain <- master_st_lineup_ats
    live_predict <- predict(ats_log_kit, testtrain, type = "response")
    rf_predict <- predict(ats_rf_kit, testtrain, type = "prob")
    ats_log_out <<- data.frame(testtrain, live_predict, rf_predict)
    
    print(tree_ats)
    
    View(ats_log_out)
      
  }
  
  if (type_use == "OVER") {
    
    master_st_lineup_tot[line_no, 4] <<- master_st_lineup_tot[line_no, 4] + inc_no
    master_st_lineup_tot[line_no, 5] <<- master_st_lineup_tot[line_no, 5] + inc_no
    master_st_lineup_tot[line_no, 6] <<- master_st_lineup_tot[line_no, 6] + inc_no
    master_st_lineup_tot[line_no, 7] <<- master_st_lineup_tot[line_no, 7] + inc_no
    master_st_lineup_tot[line_no, 8] <<- master_st_lineup_tot[line_no, 8] + inc_no
    master_st_lineup_tot[line_no, 9] <<- master_st_lineup_tot[line_no, 9] + inc_no
    master_st_lineup_tot[line_no, 10] <<- master_st_lineup_tot[line_no, 10] + inc_no
    
    
    master_st_lineup_tot[line_no + 1, 4] <<- master_st_lineup_tot[line_no + 1, 4] - inc_no
    master_st_lineup_tot[line_no + 1, 5] <<- master_st_lineup_tot[line_no + 1, 5] - inc_no
    master_st_lineup_tot[line_no + 1, 6] <<- master_st_lineup_tot[line_no + 1, 6] - inc_no
    master_st_lineup_tot[line_no + 1, 7] <<- master_st_lineup_tot[line_no + 1, 7] - inc_no
    master_st_lineup_tot[line_no + 1, 8] <<- master_st_lineup_tot[line_no + 1, 8] - inc_no
    master_st_lineup_tot[line_no + 1, 9] <<- master_st_lineup_tot[line_no + 1, 9] - inc_no
    master_st_lineup_tot[line_no + 1, 10] <<- master_st_lineup_tot[line_no + 1, 10] - inc_no
    
    testtrain <- master_st_lineup_tot %>%
      filter(BTYPE == "OVER")
    
    live_predict <- predict(over_log_kit, testtrain, type = "response")
    rf_predict <- predict(over_rf_kit, testtrain, type = "prob")
    over_log_out <<- data.frame(testtrain, live_predict, rf_predict)
    
    print(tree_over)
    
    View(over_log_out)
    
  }
  
  if (type_use == "ML") {
    
    master_st_lineup_ml[line_no, 5] <<- master_st_lineup_ml[line_no, 5] + inc_no
    master_st_lineup_ml[line_no, 6] <<- master_st_lineup_ml[line_no, 6] + inc_no
    master_st_lineup_ml[line_no, 7] <<- master_st_lineup_ml[line_no, 7] + inc_no
    master_st_lineup_ml[line_no, 8] <<- master_st_lineup_ml[line_no, 8] + inc_no
    master_st_lineup_ml[line_no, 9] <<- master_st_lineup_ml[line_no, 9] + inc_no
    master_st_lineup_ml[line_no, 10] <<- master_st_lineup_ml[line_no, 10] + inc_no
    
    
    master_st_lineup_ml[line_no + 1, 5] <<- master_st_lineup_ml[line_no + 1, 5] - inc_no
    master_st_lineup_ml[line_no + 1, 6] <<- master_st_lineup_ml[line_no + 1, 6] - inc_no
    master_st_lineup_ml[line_no + 1, 7] <<- master_st_lineup_ml[line_no + 1, 7] - inc_no
    master_st_lineup_ml[line_no + 1, 8] <<- master_st_lineup_ml[line_no + 1, 8] - inc_no
    master_st_lineup_ml[line_no + 1, 9] <<- master_st_lineup_ml[line_no + 1, 9] - inc_no
    master_st_lineup_ml[line_no + 1, 10] <<- master_st_lineup_ml[line_no + 1, 10] - inc_no
    
    testtrain <- master_st_lineup_ml

    live_predict <- predict(ml_log_kit, testtrain, type = "response")
    rf_predict <- predict(ml_rf_kit, testtrain, type = "prob")
    over_log_out <<- data.frame(testtrain, live_predict, rf_predict)
    
    print(tree_ml)
    
    View(ml_log_out)
    
  }
}

livestack_over_mc <- function(trials, m, cutt) {
  
  a <- 1
  for (a in a:trials) {
    
    print(a)
    livestack_mc_over(0)
    
    if (a == 1) { pre_key_master_df <<- over_log_out_2h[, 20:23] }
    else { pre_key_master_df <<- rbind(pre_key_master_df, over_log_out_2h[, 20:23]) }
    
  }
  
  #livestack_analyze_over(trials, m, cutt)
}

livestack_analyze_over <- function(trials, m, cutt) {
  
  min_lp <- min(pre_key_master_df[, 2])
  max_lp <- max(pre_key_master_df[, 2])
  min_rf <- min(pre_key_master_df[, 4])
  max_rf <- max(pre_key_master_df[, 4])
  
  min_lp <- m
  min_rf <- m
  
  a <- min_lp
  g <- max_lp
  i <- (max_lp - min_lp) / cutt
  c <- 0
  #a <- 0
  #g <- 0
  
  while (a <= g) {
    
    b <- min_lp
    h <- max_rf
    j <- (max_rf - min_rf) / cutt
    #b <- 0
    #h <- 0
    
    log_df <- pre_key_master_df %>%
      filter(live_predict >= a) %>%
      group_by(ATSRES) %>%
      summarize(ct = n())
    
    log_w <- as.numeric(log_df[2, 2])
    log_l <- as.numeric(log_df[1, 2])
    log_p <- log_w / (log_w + log_l)
    log_u <- log_w - (log_l * 1.1)
    log_i <- log_p * log_u
    log_c <- log_w + log_l
    
    while (b <= h) {
      
      c <- c + 1
      
      rf_df <- pre_key_master_df %>%
        filter(X1 >= b) %>%
        group_by(ATSRES) %>%
        summarize(ct = n())
      
      rf_w <- as.numeric(rf_df[2, 2])
      rf_l <- as.numeric(rf_df[1, 2])
      rf_p <- rf_w / (rf_w + rf_l)
      rf_u <- rf_w - (rf_l * 1.1)
      rf_i <- rf_p * rf_u
      rf_c <- rf_w + rf_l
      
      comb_df <- pre_key_master_df %>%
        filter(live_predict >= a & X1 >= b) %>%
        group_by(ATSRES) %>%
        summarize(ct = n())
      
      comb_w <- as.numeric(comb_df[2, 2])
      comb_l <- as.numeric(comb_df[1, 2])
      comb_p <- comb_w / (comb_w + comb_l)
      comb_u <- comb_w - (comb_l * 1.1)
      comb_i <- comb_p * comb_u
      comb_c <- comb_w + comb_l
      
      if (c == 1) { tmp_mx <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) }
      else { 
        tmp_mx2 <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) 
        tmp_mx <- rbind(tmp_mx, tmp_mx2)
      }
      
      b <- b + j
    }
    
    a <- a + i
  }
  tmp_mx[, c(4, 6, 8, 10, 12, 14)] <- tmp_mx[, c(4, 6, 8, 10, 12, 14)] / trials
  
  colnames(tmp_mx) <- c("LOGVAL", "RFVAL", "LOGPCT", "LOGUNT", "LOGIDX", "LOGCNT", "RFPCT", "RFUNT", "RFIDX", "RFCNT", "CMBPCT", "CMBUNT", "CMBIDX", "CMBCNT")
  tmp_mx <- as.data.frame(tmp_mx)
  
  top_log <- tmp_mx %>%
    arrange(desc(LOGIDX))
  
  top_log_val <- as.numeric(top_log[1, 1])
  top_log_idx <- as.numeric(top_log[1, 5])
  top_log_pct <- as.numeric(top_log[1, 3])
  top_log_cnt <- as.numeric(top_log[1, 6])
  
  top_rf <- tmp_mx %>%
    arrange(desc(RFIDX))
  
  top_rf_val <- as.numeric(top_rf[1, 2])
  top_rf_idx <- as.numeric(top_rf[1, 9])
  top_rf_pct <- as.numeric(top_rf[1, 7])
  top_rf_cnt <- as.numeric(top_rf[1, 10])
  
  top_comb <- tmp_mx %>%
    arrange(desc(CMBIDX))
  
  top_comb_log_val <- as.numeric(top_comb[1, 1])
  top_comb_rf_val <- as.numeric(top_comb[1, 2])
  top_comb_idx <- as.numeric(top_comb[1, 13])
  top_comb_pct <- as.numeric(top_comb[1, 11])
  top_comb_cnt <- as.numeric(top_comb[1, 14])
  
  top_log_merge <- tmp_mx %>%
    filter(LOGVAL >= top_log_val) %>%
    arrange(desc(RFIDX))
  
  top_lm_rf_val <- as.numeric(top_log_merge[1, 2])
  top_lm_idx <- as.numeric(top_log_merge[1, 13])
  top_lm_pct <- as.numeric(top_log_merge[1, 11])
  top_lm_cnt <- as.numeric(top_log_merge[1, 14])
  
  top_rf_merge <- tmp_mx %>%
    filter(RFVAL >= top_rf_val) %>%
    arrange(desc(LOGIDX))
  
  top_rm_log_val <- as.numeric(top_rf_merge[1, 1])
  top_rm_idx <- as.numeric(top_rf_merge[1, 13])
  top_rm_pct <- as.numeric(top_rf_merge[1, 11])
  top_rm_cnt <- as.numeric(top_rf_merge[1, 14])
  
  output_mx <<- matrix(0, nrow = 5, ncol = 6)
  
  output_mx[1, 2:6] <<- c(top_log_idx, top_log_pct, top_log_cnt, top_log_val, min_rf)
  output_mx[2, 2:6] <<- c(top_rf_idx, top_rf_pct, top_rf_cnt, min_lp, top_rf_val)
  output_mx[3, 2:6] <<- c(top_comb_idx, top_comb_pct, top_comb_cnt, top_comb_log_val, top_comb_rf_val)
  output_mx[4, 2:6] <<- c(top_lm_idx, top_lm_pct, top_lm_cnt, top_log_val, top_lm_rf_val)
  output_mx[5, 2:6] <<- c(top_rm_idx, top_rm_pct, top_rm_cnt, top_rm_log_val, top_rf_val)
  
  output_mx <<- as.data.frame(output_mx)
  
  output_mx[1, 1] <<- "LOG ONLY" 
  output_mx[2, 1] <<- "RF ONLY" 
  output_mx[3, 1] <<- "COMB ONLY" 
  output_mx[4, 1] <<- "LOG MERGE" 
  output_mx[5, 1] <<- "RF MERGE" 
  
  colnames(output_mx) <- c("Type", "Index", "Pct", 'Count', "Log Val", "RF Val")
  
  View(output_mx)
  
}

livestack_mc_over <- function(cccc) {
  
  if (cccc == 1) { 
    
    live_log <<- read_csv("livelog.csv")
    
    res_log <- team_game_by_game_raw[, c(3, 5, 10)]
    res_log <- res_log %>%
      left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
      select(`Ken Pom`, DATE, PTS) %>%
      mutate(DATE2 = mdy(DATE)) %>%
      select(`Ken Pom`, DATE2, PTS) %>%
      mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
      select(-DATE2)
    
    live_log_join <- live_log %>%
      inner_join(res_log, by = c("AWAY" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year")) %>%
      inner_join(res_log, by = c("HOME" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year"))
    
    over_log_2h <<- live_log_join %>%
      filter(BETTYPE == "TOTAL" & AORH == "O") %>%
      mutate(GAMETOTAL = PTS.x + PTS.y) %>%
      mutate(ATSRES = ifelse(GAMETOTAL > LIVELINE, 1, 0))
    
    over_log_2h$ATSRES <<- as.factor(over_log_2h$ATSRES)
  }
  
  #ATS Logistic and Random Forest
  
  rf_train <- sample(c(1:nrow(over_log_2h)), round(nrow(over_log_2h) * .75, 0))
  rf_test <- -1 * rf_train
  
  rf_train <- over_log_2h[rf_train, ]
  rf_test <- over_log_2h[rf_test, ]
  
  #IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG + CATSMARG
  over_log_fit_2h <<- glm(over_log_form_2h, data = rf_train, family = "binomial")
  over_rf_fit_2h <<- randomForest(over_log_form_2h, data = rf_train)
  
  live_predict <- predict(over_log_fit_2h, rf_test, type = "response")
  rf_predict <<- predict(over_rf_fit_2h, rf_test, type = "prob")
  over_log_out_2h <<- data.frame(rf_test, live_predict, rf_predict)
}



livestack_under_mc <- function(trials, m, cutt) {
  
  a <- 1
  for (a in a:trials) {
    
    print(a)
    livestack_mc_under(0)
    
    if (a == 1) { pre_key_master_df <<- under_log_out_2h[, 20:23] }
    else { pre_key_master_df <<- rbind(pre_key_master_df, under_log_out_2h[, 20:23]) }
    
  }
  
  #livestack_analyze_under(trials, m, cutt)
}

livestack_analyze_under <- function(trials, m, cutt) {
  
  min_lp <- min(pre_key_master_df[, 2])
  max_lp <- max(pre_key_master_df[, 2])
  min_rf <- min(pre_key_master_df[, 4])
  max_rf <- max(pre_key_master_df[, 4])
  
  min_lp <- m
  min_rf <- m
  
  a <- m
  g <- max_lp
  i <- (max_lp - min_lp) / cutt
  c <- 0
  #a <- 0
  #g <- 0
  
  while (a <= g) {
    
    b <- m
    h <- max_rf
    j <- (max_rf - min_rf) / cutt
    #b <- 0
    #h <- 0
    
    log_df <- pre_key_master_df %>%
      filter(live_predict >= a) %>%
      group_by(ATSRES) %>%
      summarize(ct = n())
    
    log_w <- as.numeric(log_df[2, 2])
    log_l <- as.numeric(log_df[1, 2])
    log_p <- log_w / (log_w + log_l)
    log_u <- log_w - (log_l * 1.1)
    log_i <- log_p * log_u
    log_c <- log_w + log_l
    
    while (b <= h) {
      
      c <- c + 1
      
      rf_df <- pre_key_master_df %>%
        filter(X1 >= b) %>%
        group_by(ATSRES) %>%
        summarize(ct = n())
      
      rf_w <- as.numeric(rf_df[2, 2])
      rf_l <- as.numeric(rf_df[1, 2])
      rf_p <- rf_w / (rf_w + rf_l)
      rf_u <- rf_w - (rf_l * 1.1)
      rf_i <- rf_p * rf_u
      rf_c <- rf_w + rf_l
      
      comb_df <- pre_key_master_df %>%
        filter(live_predict >= a & X1 >= b) %>%
        group_by(ATSRES) %>%
        summarize(ct = n())
      
      comb_w <- as.numeric(comb_df[2, 2])
      comb_l <- as.numeric(comb_df[1, 2])
      comb_p <- comb_w / (comb_w + comb_l)
      comb_u <- comb_w - (comb_l * 1.1)
      comb_i <- comb_p * comb_u
      comb_c <- comb_w + comb_l
      
      if (c == 1) { tmp_mx <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) }
      else { 
        tmp_mx2 <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) 
        tmp_mx <- rbind(tmp_mx, tmp_mx2)
      }
      
      b <- b + j
    }
    
    a <- a + i
  }
  tmp_mx[, c(4, 6, 8, 10, 12, 14)] <- tmp_mx[, c(4, 6, 8, 10, 12, 14)] / trials
  
  colnames(tmp_mx) <- c("LOGVAL", "RFVAL", "LOGPCT", "LOGUNT", "LOGIDX", "LOGCNT", "RFPCT", "RFUNT", "RFIDX", "RFCNT", "CMBPCT", "CMBUNT", "CMBIDX", "CMBCNT")
  tmp_mx <- as.data.frame(tmp_mx)
  
  top_log <- tmp_mx %>%
    arrange(desc(LOGIDX))
  
  top_log_val <- as.numeric(top_log[1, 1])
  top_log_idx <- as.numeric(top_log[1, 5])
  top_log_pct <- as.numeric(top_log[1, 3])
  top_log_cnt <- as.numeric(top_log[1, 6])
  
  top_rf <- tmp_mx %>%
    arrange(desc(RFIDX))
  
  top_rf_val <- as.numeric(top_rf[1, 2])
  top_rf_idx <- as.numeric(top_rf[1, 9])
  top_rf_pct <- as.numeric(top_rf[1, 7])
  top_rf_cnt <- as.numeric(top_rf[1, 10])
  
  top_comb <- tmp_mx %>%
    arrange(desc(CMBIDX))
  
  top_comb_log_val <- as.numeric(top_comb[1, 1])
  top_comb_rf_val <- as.numeric(top_comb[1, 2])
  top_comb_idx <- as.numeric(top_comb[1, 13])
  top_comb_pct <- as.numeric(top_comb[1, 11])
  top_comb_cnt <- as.numeric(top_comb[1, 14])
  
  top_log_merge <- tmp_mx %>%
    filter(LOGVAL >= top_log_val) %>%
    arrange(desc(RFIDX))
  
  top_lm_rf_val <- as.numeric(top_log_merge[1, 2])
  top_lm_idx <- as.numeric(top_log_merge[1, 13])
  top_lm_pct <- as.numeric(top_log_merge[1, 11])
  top_lm_cnt <- as.numeric(top_log_merge[1, 14])
  
  top_rf_merge <- tmp_mx %>%
    filter(RFVAL >= top_rf_val) %>%
    arrange(desc(LOGIDX))
  
  top_rm_log_val <- as.numeric(top_rf_merge[1, 1])
  top_rm_idx <- as.numeric(top_rf_merge[1, 13])
  top_rm_pct <- as.numeric(top_rf_merge[1, 11])
  top_rm_cnt <- as.numeric(top_rf_merge[1, 14])
  
  output_mx <<- matrix(0, nrow = 5, ncol = 6)
  
  output_mx[1, 2:6] <<- c(top_log_idx, top_log_pct, top_log_cnt, top_log_val, min_rf)
  output_mx[2, 2:6] <<- c(top_rf_idx, top_rf_pct, top_rf_cnt, min_lp, top_rf_val)
  output_mx[3, 2:6] <<- c(top_comb_idx, top_comb_pct, top_comb_cnt, top_comb_log_val, top_comb_rf_val)
  output_mx[4, 2:6] <<- c(top_lm_idx, top_lm_pct, top_lm_cnt, top_log_val, top_lm_rf_val)
  output_mx[5, 2:6] <<- c(top_rm_idx, top_rm_pct, top_rm_cnt, top_rm_log_val, top_rf_val)
  
  output_mx <<- as.data.frame(output_mx)
  
  output_mx[1, 1] <<- "LOG ONLY" 
  output_mx[2, 1] <<- "RF ONLY" 
  output_mx[3, 1] <<- "COMB ONLY" 
  output_mx[4, 1] <<- "LOG MERGE" 
  output_mx[5, 1] <<- "RF MERGE" 
  
  colnames(output_mx) <- c("Type", "Index", "Pct", 'Count', "Log Val", "RF Val")
  
  View(output_mx)
}

livestack_mc_under <- function(cccc) {
  
  if (cccc == 1) { 
    
    live_log <<- read_csv("livelog.csv")
    
    res_log <- team_game_by_game_raw[, c(3, 5, 10)]
    res_log <- res_log %>%
      left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
      select(`Ken Pom`, DATE, PTS) %>%
      mutate(DATE2 = mdy(DATE)) %>%
      select(`Ken Pom`, DATE2, PTS) %>%
      mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
      select(-DATE2)
    
    live_log_join <- live_log %>%
      inner_join(res_log, by = c("AWAY" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year")) %>%
      inner_join(res_log, by = c("HOME" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year"))
    
    under_log_2h <<- live_log_join %>%
      filter(BETTYPE == "TOTAL" & AORH == "O") %>%
      mutate(GAMETOTAL = PTS.x + PTS.y) %>%
      mutate(ATSRES = ifelse(GAMETOTAL > LIVELINE, 1, 0))
    
    under_log_2h$ATSRES <<- as.factor(under_log_2h$ATSRES)
  }
  
  #ATS Logistic and Random Forest
  
  rf_train <- sample(c(1:nrow(under_log_2h)), round(nrow(under_log_2h) * .75, 0))
  rf_test <- -1 * rf_train
  
  rf_train <- under_log_2h[rf_train, ]
  rf_test <- under_log_2h[rf_test, ]
  
  #IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG + CATSMARG
  under_log_fit_2h <<- glm(under_log_form_2h, data = rf_train, family = "binomial")
  under_rf_fit_2h <<- randomForest(under_log_form_2h, data = rf_train)
  
  live_predict <- predict(under_log_fit_2h, rf_test, type = "response")
  rf_predict <<- predict(under_rf_fit_2h, rf_test, type = "prob")
  under_log_out_2h <<- data.frame(rf_test, live_predict, rf_predict)
}



livestack_ats_mc <- function(trials, m, cutt) {
  
  a <- 1
  for (a in a:trials) {
    
    print(a)
    livestack_mc_ats(0)
    
    if (a == 1) { pre_key_master_df <<- ats_log_out_2h[, 21:24] }
    else { pre_key_master_df <<- rbind(pre_key_master_df, ats_log_out_2h[, 21:24]) }
    
  }
  
  #livestack_analyze_ats(trials, m, cutt)
}

livestack_analyze_ats <- function(trials, m, cutt) {
  
  min_lp <- min(pre_key_master_df[, 2])
  max_lp <- max(pre_key_master_df[, 2])
  min_rf <- min(pre_key_master_df[, 4])
  max_rf <- max(pre_key_master_df[, 4])
  
  min_lp <- m
  min_rf <- m
  
  a <- min_lp
  g <- max_lp
  i <- (max_lp - min_lp) / cutt
  c <- 0
  #a <- 0
  #g <- 0
  
  while (a <= g) {
    
    b <- min_rf
    h <- max_rf
    j <- (max_rf - min_rf) / cutt
    #b <- 0
    #h <- 0
    
    log_df <- pre_key_master_df %>%
      filter(live_predict >= a) %>%
      group_by(ATSRES) %>%
      summarize(ct = n())
    
    log_w <- as.numeric(log_df[2, 2])
    log_l <- as.numeric(log_df[1, 2])
    log_p <- log_w / (log_w + log_l)
    log_u <- log_w - (log_l * 1.1)
    log_i <- log_p * log_u
    log_c <- log_w + log_l
    
    while (b <= h) {
      
      c <- c + 1

      rf_df <- pre_key_master_df %>%
        filter(X1 >= b) %>%
        group_by(ATSRES) %>%
        summarize(ct = n())
      
      rf_w <- as.numeric(rf_df[2, 2])
      rf_l <- as.numeric(rf_df[1, 2])
      rf_p <- rf_w / (rf_w + rf_l)
      rf_u <- rf_w - (rf_l * 1.1)
      rf_i <- rf_p * rf_u
      rf_c <- rf_w + rf_l
      
      comb_df <- pre_key_master_df %>%
        filter(live_predict >= a & X1 >= b) %>%
        group_by(ATSRES) %>%
        summarize(ct = n())
      
      comb_w <- as.numeric(comb_df[2, 2])
      comb_l <- as.numeric(comb_df[1, 2])
      comb_p <- comb_w / (comb_w + comb_l)
      comb_u <- comb_w - (comb_l * 1.1)
      comb_i <- comb_p * comb_u
      comb_c <- comb_w + comb_l
      
      if (c == 1) { tmp_mx <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) }
      else { 
        tmp_mx2 <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) 
        tmp_mx <- rbind(tmp_mx, tmp_mx2)
      }
      
      b <- b + j
    }
    
    a <- a + i
  }
  tmp_mx[, c(4, 6, 8, 10, 12, 14)] <- tmp_mx[, c(4, 6, 8, 10, 12, 14)] / trials
  
  colnames(tmp_mx) <- c("LOGVAL", "RFVAL", "LOGPCT", "LOGUNT", "LOGIDX", "LOGCNT", "RFPCT", "RFUNT", "RFIDX", "RFCNT", "CMBPCT", "CMBUNT", "CMBIDX", "CMBCNT")
  tmp_mx <- as.data.frame(tmp_mx)
  
  top_log <- tmp_mx %>%
    arrange(desc(LOGIDX))
  
  top_log_val <- as.numeric(top_log[1, 1])
  top_log_idx <- as.numeric(top_log[1, 5])
  top_log_pct <- as.numeric(top_log[1, 3])
  top_log_cnt <- as.numeric(top_log[1, 6])
  
  top_rf <- tmp_mx %>%
    arrange(desc(RFIDX))
  
  top_rf_val <- as.numeric(top_rf[1, 2])
  top_rf_idx <- as.numeric(top_rf[1, 9])
  top_rf_pct <- as.numeric(top_rf[1, 7])
  top_rf_cnt <- as.numeric(top_rf[1, 10])
  
  top_comb <- tmp_mx %>%
    arrange(desc(CMBIDX))
  
  top_comb_log_val <- as.numeric(top_comb[1, 1])
  top_comb_rf_val <- as.numeric(top_comb[1, 2])
  top_comb_idx <- as.numeric(top_comb[1, 13])
  top_comb_pct <- as.numeric(top_comb[1, 11])
  top_comb_cnt <- as.numeric(top_comb[1, 14])
  
  top_log_merge <- tmp_mx %>%
    filter(LOGVAL >= top_log_val) %>%
    arrange(desc(RFIDX))
  
  top_lm_rf_val <- as.numeric(top_log_merge[1, 2])
  top_lm_idx <- as.numeric(top_log_merge[1, 13])
  top_lm_pct <- as.numeric(top_log_merge[1, 11])
  top_lm_cnt <- as.numeric(top_log_merge[1, 14])
  
  top_rf_merge <- tmp_mx %>%
    filter(RFVAL >= top_rf_val) %>%
    arrange(desc(LOGIDX))
  
  top_rm_log_val <- as.numeric(top_rf_merge[1, 1])
  top_rm_idx <- as.numeric(top_rf_merge[1, 13])
  top_rm_pct <- as.numeric(top_rf_merge[1, 11])
  top_rm_cnt <- as.numeric(top_rf_merge[1, 14])
  
  output_mx <<- matrix(0, nrow = 5, ncol = 6)
  
  output_mx[1, 2:6] <<- c(top_log_idx, top_log_pct, top_log_cnt, top_log_val, min_rf)
  output_mx[2, 2:6] <<- c(top_rf_idx, top_rf_pct, top_rf_cnt, min_lp, top_rf_val)
  output_mx[3, 2:6] <<- c(top_comb_idx, top_comb_pct, top_comb_cnt, top_comb_log_val, top_comb_rf_val)
  output_mx[4, 2:6] <<- c(top_lm_idx, top_lm_pct, top_lm_cnt, top_log_val, top_lm_rf_val)
  output_mx[5, 2:6] <<- c(top_rm_idx, top_rm_pct, top_rm_cnt, top_rm_log_val, top_rf_val)
  
  output_mx <<- as.data.frame(output_mx)
  
  output_mx[1, 1] <<- "LOG ONLY" 
  output_mx[2, 1] <<- "RF ONLY" 
  output_mx[3, 1] <<- "COMB ONLY" 
  output_mx[4, 1] <<- "LOG MERGE" 
  output_mx[5, 1] <<- "RF MERGE" 
  
  colnames(output_mx) <- c("Type", "Index", "Pct", 'Count', "Log Val", "RF Val")
  
  View(output_mx)
}

livestack_mc_ats <- function(cccc) {
  
  if (cccc == 1) { 
    
    live_log <<- read_csv("livelog.csv")
    
    res_log <- team_game_by_game_raw[, c(3, 5, 10)]
    res_log <- res_log %>%
      left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
      select(`Ken Pom`, DATE, PTS) %>%
      mutate(DATE2 = mdy(DATE)) %>%
      select(`Ken Pom`, DATE2, PTS) %>%
      mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
      select(-DATE2)
    
    live_log_join <- live_log %>%
      inner_join(res_log, by = c("AWAY" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year")) %>%
      inner_join(res_log, by = c("HOME" = "Ken Pom", "MONTH" = "Month", "DAY" = "Day", "YEAR" = "Year"))
    
    ats_log_2h <<- live_log_join %>%
      filter(BETTYPE == "TOTAL" & AORH == "O") %>%
      mutate(GAMETOTAL = PTS.x + PTS.y) %>%
      mutate(ATSRES = ifelse(GAMETOTAL > LIVELINE, 1, 0))
    
    ats_log_2h$ATSRES <<- as.factor(ats_log_2h$ATSRES)
  }
  
  #ATS Logistic and Random Forest
  
  rf_train <- sample(c(1:nrow(ats_log_2h)), round(nrow(ats_log_2h) * .75, 0))
  rf_test <- -1 * rf_train
  
  rf_train <- ats_log_2h[rf_train, ]
  rf_test <- ats_log_2h[rf_test, ]
  
  #IATSMARG + TATSMARG + BATSMARG + AATSMARG + SATSMARG + CATSMARG
  ats_log_fit_2h <<- glm(ats_log_form_2h, data = rf_train, family = "binomial")
  ats_rf_fit_2h <<- randomForest(ats_log_form_2h, data = rf_train)
  
  live_predict <- predict(ats_log_fit_2h, rf_test, type = "response")
  rf_predict <<- predict(ats_rf_fit_2h, rf_test, type = "prob")
  ats_log_out_2h <<- data.frame(rf_test, live_predict, rf_predict)
}

#-------------------------
#PRESTACK
#-------------------------

prestack_ats_mc <- function(trials) {
  
  set.seed(b_seed)
  
  a <- 1
  for (a in a:trials) {
    
    print(a)
    
    rf_train9 <- sample(c(1:nrow(key_tree_ats)), round(nrow(key_tree_ats) * .75, 0))
    rf_test <- -1 * rf_train9
    rf_train9 <- key_tree_ats[rf_train9, ]
    rf_test <- key_tree_ats[rf_test, ]
    
    
    ats_log9_kit <<- glm(pre_ats_log_form, data = rf_train9, family = "binomial")
    ats_rf9_kit <<- randomForest(pre_ats_rf_form, data = rf_train9)
    live_predict <- predict(ats_log9_kit, rf_test, type = "response")
    rf_predict <<- predict(ats_rf9_kit, rf_test, type = "prob")
    ats_log_out9 <<- data.frame(rf_test, live_predict, rf_predict) %>%
      filter(RES != 0)
    
    if (a == 1) { pre_key_master_df <<- ats_log_out9[, 12:15] }
    else { pre_key_master_df <<- rbind(pre_key_master_df, ats_log_out9[, 12:15]) }
    
  }
  
  #prestack_analyze_ats(trials, m, cutt)
}




prestack_over_mc <- function(trials, m, cutt) {
  
  a <- 1
  for (a in a:trials) {
    
    print(a)
    prestack_mc_over(0)
    
    if (a == 1) { pre_key_master_df <<- over_log_out9[, 12:15] }
    else { pre_key_master_df <<- rbind(pre_key_master_df, over_log_out9[, 12:15]) }
    
  }
  
  #prestack_analyze_over(trials, m, cutt)
}

prestack_analyze_over <- function(trials, m, cutt) {
  
  min_lp <- min(pre_key_master_df[, 2])
  max_lp <- max(pre_key_master_df[, 2])
  min_rf <- min(pre_key_master_df[, 4])
  max_rf <- max(pre_key_master_df[, 4])
  
  min_rf <- m
  min_lp <- m
  
  a <- min_lp
  g <- max_lp
  i <- (max_lp - min_lp) / cutt
  c <- 0
  #a <- 0
  #g <- 0
  
  while (a <= g) {
    
    b <- min_rf
    h <- max_rf
    j <- (max_rf - min_rf) / cutt
    #b <- 0
    #h <- 0
    
    log_df <- pre_key_master_df %>%
      filter(live_predict >= a) %>%
      group_by(RESFAC) %>%
      summarize(ct = n())
    
    log_w <- as.numeric(log_df[2, 2])
    log_l <- as.numeric(log_df[1, 2])
    log_p <- log_w / (log_w + log_l)
    log_u <- log_w - (log_l * 1.1)
    log_i <- log_p * log_u
    log_c <- log_w + log_l
    
    while (b <= h) {
      
      c <- c + 1
      
      rf_df <- pre_key_master_df %>%
        filter(X1 >= b) %>%
        group_by(RESFAC) %>%
        summarize(ct = n())
      
      rf_w <- as.numeric(rf_df[2, 2])
      rf_l <- as.numeric(rf_df[1, 2])
      rf_p <- rf_w / (rf_w + rf_l)
      rf_u <- rf_w - (rf_l * 1.1)
      rf_i <- rf_p * rf_u
      rf_c <- rf_w + rf_l
      
      comb_df <- pre_key_master_df %>%
        filter(live_predict >= a & X1 >= b) %>%
        group_by(RESFAC) %>%
        summarize(ct = n())
      
      comb_w <- as.numeric(comb_df[2, 2])
      comb_l <- as.numeric(comb_df[1, 2])
      comb_p <- comb_w / (comb_w + comb_l)
      comb_u <- comb_w - (comb_l * 1.1)
      comb_i <- comb_p * comb_u
      comb_c <- comb_w + comb_l
      
      if (c == 1) { tmp_mx <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) }
      else { 
        tmp_mx2 <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) 
        tmp_mx <- rbind(tmp_mx, tmp_mx2)
      }
      
      b <- b + j
    }
    
    a <- a + i
  }
  tmp_mx[, c(4, 6, 8, 10, 12, 14)] <- tmp_mx[, c(4, 6, 8, 10, 12, 14)] / trials
  
  colnames(tmp_mx) <- c("LOGVAL", "RFVAL", "LOGPCT", "LOGUNT", "LOGIDX", "LOGCNT", "RFPCT", "RFUNT", "RFIDX", "RFCNT", "CMBPCT", "CMBUNT", "CMBIDX", "CMBCNT")
  tmp_mx <- as.data.frame(tmp_mx)
  top_log <- tmp_mx %>%
    arrange(desc(LOGIDX))
  
  top_log_val <- as.numeric(top_log[1, 1])
  top_log_idx <- as.numeric(top_log[1, 5])
  top_log_pct <- as.numeric(top_log[1, 3])
  top_log_cnt <- as.numeric(top_log[1, 6])
  
  top_rf <- tmp_mx %>%
    arrange(desc(RFIDX))
  
  top_rf_val <- as.numeric(top_rf[1, 2])
  top_rf_idx <- as.numeric(top_rf[1, 9])
  top_rf_pct <- as.numeric(top_rf[1, 7])
  top_rf_cnt <- as.numeric(top_rf[1, 10])
  
  top_comb <- tmp_mx %>%
    arrange(desc(CMBIDX))
  
  top_comb_log_val <- as.numeric(top_comb[1, 1])
  top_comb_rf_val <- as.numeric(top_comb[1, 2])
  top_comb_idx <- as.numeric(top_comb[1, 13])
  top_comb_pct <- as.numeric(top_comb[1, 11])
  top_comb_cnt <- as.numeric(top_comb[1, 14])
  
  top_log_merge <- tmp_mx %>%
    filter(LOGVAL >= top_log_val) %>%
    arrange(desc(RFIDX))
  
  top_lm_rf_val <- as.numeric(top_log_merge[1, 2])
  top_lm_idx <- as.numeric(top_log_merge[1, 13])
  top_lm_pct <- as.numeric(top_log_merge[1, 11])
  top_lm_cnt <- as.numeric(top_log_merge[1, 14])
  
  top_rf_merge <- tmp_mx %>%
    filter(RFVAL >= top_rf_val) %>%
    arrange(desc(LOGIDX))
  
  top_rm_log_val <- as.numeric(top_rf_merge[1, 1])
  top_rm_idx <- as.numeric(top_rf_merge[1, 13])
  top_rm_pct <- as.numeric(top_rf_merge[1, 11])
  top_rm_cnt <- as.numeric(top_rf_merge[1, 14])
  
  output_mx <<- matrix(0, nrow = 5, ncol = 6)
  
  output_mx[1, 2:6] <<- c(top_log_idx, top_log_pct, top_log_cnt, top_log_val, min_rf)
  output_mx[2, 2:6] <<- c(top_rf_idx, top_rf_pct, top_rf_cnt, min_lp, top_rf_val)
  output_mx[3, 2:6] <<- c(top_comb_idx, top_comb_pct, top_comb_cnt, top_comb_log_val, top_comb_rf_val)
  output_mx[4, 2:6] <<- c(top_lm_idx, top_lm_pct, top_lm_cnt, top_log_val, top_lm_rf_val)
  output_mx[5, 2:6] <<- c(top_rm_idx, top_rm_pct, top_rm_cnt, top_rm_log_val, top_rf_val)
  
  output_mx <<- as.data.frame(output_mx)
  
  output_mx[1, 1] <<- "LOG ONLY" 
  output_mx[2, 1] <<- "RF ONLY" 
  output_mx[3, 1] <<- "COMB ONLY" 
  output_mx[4, 1] <<- "LOG MERGE" 
  output_mx[5, 1] <<- "RF MERGE" 
  
  colnames(output_mx) <- c("Type", "Index", "Pct", 'Count', "Log Val", "RF Val")
  
  View(output_mx)
}

prestack_mc_over <- function(xxxx) {
  
  #over Logistic and Random Forest
  
  rf_train9 <- sample(c(1:nrow(key_tree_over)), round(nrow(key_tree_over) * .9, 0))
  rf_test <- -1 * rf_train9
  rf_train9 <- key_tree_over[rf_train9, ]
  rf_test <- key_tree_over[rf_test, ]
  
  
  over_log9_kit <<- glm(over_log_form_use, data = rf_train9, family = "binomial")
  over_rf9_kit <<- randomForest(over_log_form_use, data = rf_train9)
  live_predict <- predict(over_log9_kit, rf_test, type = "response")
  rf_predict <<- predict(over_rf9_kit, rf_test, type = "prob")
  over_log_out9 <<- data.frame(rf_test, live_predict, rf_predict)
  
}


prestack_under_mc <- function(trials, m, cutt) {
  
  a <- 1
  for (a in a:trials) {
    
    print(a)
    prestack_mc_under(0)
    
    if (a == 1) { pre_key_master_df <<- under_log_out9[, 12:15] }
    else { pre_key_master_df <<- rbind(pre_key_master_df, under_log_out9[, 12:15]) }
    
  }
  
  #prestack_analyze_under(trials, m, cutt)
}

prestack_analyze_under <- function(trials, m, cutt) {
  
  min_lp <- min(pre_key_master_df[, 2])
  max_lp <- max(pre_key_master_df[, 2])
  min_rf <- min(pre_key_master_df[, 4])
  max_rf <- max(pre_key_master_df[, 4])
  
  min_lp <- m
  min_rf <- m
  
  a <- m
  g <- max_lp
  i <- (max_lp - min_lp) / 40
  c <- 0
  #a <- 0
  #g <- 0
  
  while (a <= g) {
    
    b <- m
    h <- max_rf
    j <- (max_rf - min_rf) / 40
    #b <- 0
    #h <- 0
    
    log_df <- pre_key_master_df %>%
      filter(live_predict >= a) %>%
      group_by(RESFAC) %>%
      summarize(ct = n())
    
    log_w <- as.numeric(log_df[2, 2])
    log_l <- as.numeric(log_df[1, 2])
    log_p <- log_w / (log_w + log_l)
    log_u <- log_w - (log_l * 1.1)
    log_i <- log_p * log_u
    log_c <- log_w + log_l
    
    while (b <= h) {
      
      c <- c + 1
      
      rf_df <- pre_key_master_df %>%
        filter(X1 >= b) %>%
        group_by(RESFAC) %>%
        summarize(ct = n())
      
      rf_w <- as.numeric(rf_df[2, 2])
      rf_l <- as.numeric(rf_df[1, 2])
      rf_p <- rf_w / (rf_w + rf_l)
      rf_u <- rf_w - (rf_l * 1.1)
      rf_i <- rf_p * rf_u
      rf_c <- rf_w + rf_l
      
      comb_df <- pre_key_master_df %>%
        filter(live_predict >= a & X1 >= b) %>%
        group_by(RESFAC) %>%
        summarize(ct = n())
      
      comb_w <- as.numeric(comb_df[2, 2])
      comb_l <- as.numeric(comb_df[1, 2])
      comb_p <- comb_w / (comb_w + comb_l)
      comb_u <- comb_w - (comb_l * 1.1)
      comb_i <- comb_p * comb_u
      comb_c <- comb_w + comb_l
      
      if (c == 1) { tmp_mx <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) }
      else { 
        tmp_mx2 <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) 
        tmp_mx <- rbind(tmp_mx, tmp_mx2)
      }
      
      b <- b + j
    }
    
    a <- a + i
  }
  tmp_mx[, c(4, 6, 8, 10, 12, 14)] <- tmp_mx[, c(4, 6, 8, 10, 12, 14)] / trials
  
  colnames(tmp_mx) <- c("LOGVAL", "RFVAL", "LOGPCT", "LOGUNT", "LOGIDX", "LOGCNT", "RFPCT", "RFUNT", "RFIDX", "RFCNT", "CMBPCT", "CMBUNT", "CMBIDX", "CMBCNT")
  tmp_mx <- as.data.frame(tmp_mx)
  View(tmp_mx)
  top_log <- tmp_mx %>%
    arrange(desc(LOGIDX))
  
  top_log_val <- as.numeric(top_log[1, 1])
  top_log_idx <- as.numeric(top_log[1, 5])
  top_log_pct <- as.numeric(top_log[1, 3])
  top_log_cnt <- as.numeric(top_log[1, 6])
  
  top_rf <- tmp_mx %>%
    arrange(desc(RFIDX))
  
  top_rf_val <- as.numeric(top_rf[1, 2])
  top_rf_idx <- as.numeric(top_rf[1, 9])
  top_rf_pct <- as.numeric(top_rf[1, 7])
  top_rf_cnt <- as.numeric(top_rf[1, 10])
  
  top_comb <- tmp_mx %>%
    arrange(desc(CMBIDX))
  
  top_comb_log_val <- as.numeric(top_comb[1, 1])
  top_comb_rf_val <- as.numeric(top_comb[1, 2])
  top_comb_idx <- as.numeric(top_comb[1, 13])
  top_comb_pct <- as.numeric(top_comb[1, 11])
  top_comb_cnt <- as.numeric(top_comb[1, 14])
  
  top_log_merge <- tmp_mx %>%
    filter(LOGVAL >= top_log_val) %>%
    arrange(desc(RFIDX))
  
  top_lm_rf_val <- as.numeric(top_log_merge[1, 2])
  top_lm_idx <- as.numeric(top_log_merge[1, 13])
  top_lm_pct <- as.numeric(top_log_merge[1, 11])
  top_lm_cnt <- as.numeric(top_log_merge[1, 14])
  
  top_rf_merge <- tmp_mx %>%
    filter(RFVAL >= top_rf_val) %>%
    arrange(desc(LOGIDX))
  
  top_rm_log_val <- as.numeric(top_rf_merge[1, 1])
  top_rm_idx <- as.numeric(top_rf_merge[1, 13])
  top_rm_pct <- as.numeric(top_rf_merge[1, 11])
  top_rm_cnt <- as.numeric(top_rf_merge[1, 14])
  
  output_mx <<- matrix(0, nrow = 5, ncol = 6)
  
  output_mx[1, 2:6] <<- c(top_log_idx, top_log_pct, top_log_cnt, top_log_val, min_rf)
  output_mx[2, 2:6] <<- c(top_rf_idx, top_rf_pct, top_rf_cnt, min_lp, top_rf_val)
  output_mx[3, 2:6] <<- c(top_comb_idx, top_comb_pct, top_comb_cnt, top_comb_log_val, top_comb_rf_val)
  output_mx[4, 2:6] <<- c(top_lm_idx, top_lm_pct, top_lm_cnt, top_log_val, top_lm_rf_val)
  output_mx[5, 2:6] <<- c(top_rm_idx, top_rm_pct, top_rm_cnt, top_rm_log_val, top_rf_val)
  
  output_mx <<- as.data.frame(output_mx)
  
  output_mx[1, 1] <<- "LOG ONLY" 
  output_mx[2, 1] <<- "RF ONLY" 
  output_mx[3, 1] <<- "COMB ONLY" 
  output_mx[4, 1] <<- "LOG MERGE" 
  output_mx[5, 1] <<- "RF MERGE" 
  
  colnames(output_mx) <- c("Type", "Index", "Pct", 'Count', "Log Val", "RF Val")
  
  View(output_mx)
}

prestack_mc_under <- function(xxxx) {
  
  #under Logistic and Random Forest
  
  rf_train9 <- sample(c(1:nrow(key_tree_under)), round(nrow(key_tree_under) * .9, 0))
  rf_test <- -1 * rf_train9
  rf_train9 <- key_tree_under[rf_train9, ]
  rf_test <- key_tree_under[rf_test, ]
  
  
  under_log9_kit <<- glm(under_log_form_use, data = rf_train9, family = "binomial")
  under_rf9_kit <<- randomForest(under_log_form_use, data = rf_train9)
  live_predict <- predict(under_log9_kit, rf_test, type = "response")
  rf_predict <<- predict(under_rf9_kit, rf_test, type = "prob")
  under_log_out9 <<- data.frame(rf_test, live_predict, rf_predict)
  
}


prestack_ml_mc <- function(trials, m, cutt, ml_bot, ml_top) {
  
  tree_log_mlb <<- ml_bot
  tree_log_mlt <<- ml_top
  pre_key_ml_set(ml_bot, ml_top)
  
  a <- 1
  for (a in a:trials) {
    
    print(a)
    prestack_mc_ml(0)
    
    if (a == 1) { pre_key_master_df <<- ml_log_out9[, c(5:6, 8, 15:20)] }
    else { pre_key_master_df <<- rbind(pre_key_master_df, ml_log_out9[, c(5:6, 8, 15:20)]) }
    
  }
  
  prestack_analyze_ml(trials, m, cutt, ml_bot, ml_top)
}

prestack_analyze_ml <- function(trials, m, cutt, ml_bot, ml_top) {

  max_lp <- max(pre_key_master_df[, 8])
  max_rf <- max(pre_key_master_df[, 9])
  
  min_rf <- m
  min_lp <- m
  
  a <- min_lp
  g <- max_lp
  i <- (max_lp - min_lp) / cutt
  c <- 0
  #a <- 0
  #g <- 0
  
  while (a <= g) {
    
    b <- min_rf
    h <- max_rf
    j <- (max_rf - min_rf) / cutt
    #b <- 0
    #h <- 0
    
    log_df <- pre_key_master_df %>%
      filter(LOGEXP >= a)
    
    log_w <- length(which(log_df$RES > 0))
    log_l <- length(which(log_df$RES < 0))
    log_p <- log_w / (log_w + log_l)
    log_u <- sum(log_df$RES)
    log_c <- log_w + log_l
    log_i <- log_u / log_c
    
    while (b <= h) {
      
      c <- c + 1
      
      rf_df <- pre_key_master_df %>%
        filter(RFEXP >= b)
      
      rf_w <- length(which(rf_df$RES > 0))
      rf_l <- length(which(rf_df$RES < 0))
      rf_p <- rf_w / (rf_w + rf_l)
      rf_u <- sum(rf_df$RES)
      rf_c <- rf_w + rf_l
      rf_i <- rf_u / rf_c
      
      comb_df <- pre_key_master_df %>%
        filter(LOGEXP >= a & RFEXP >= b)
      
      comb_w <- length(which(comb_df$RES > 0))
      comb_l <- length(which(comb_df$RES < 0))
      comb_p <- comb_w / (comb_w + comb_l)
      comb_u <- sum(comb_df$RES)
      comb_c <- comb_w + comb_l
      comb_i <- comb_u / rf_c
      
      if (c == 1) { tmp_mx <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) }
      else { 
        tmp_mx2 <- matrix(c(a, b, log_p, log_u, log_i, log_c, rf_p, rf_u, rf_i, rf_c, comb_p, comb_u, comb_i, comb_c), nrow = 1, ncol = 14) 
        tmp_mx <- rbind(tmp_mx, tmp_mx2)
      }
      
      b <- b + j
    }
    
    a <- a + i
  }
  tmp_mx[, c(4, 6, 8, 10, 12, 14)] <- tmp_mx[, c(4, 6, 8, 10, 12, 14)] / trials
  
  colnames(tmp_mx) <- c("LOGVAL", "RFVAL", "LOGPCT", "LOGUNT", "LOGIDX", "LOGCNT", "RFPCT", "RFUNT", "RFIDX", "RFCNT", "CMBPCT", "CMBUNT", "CMBIDX", "CMBCNT")
  tmp_mx <- as.data.frame(tmp_mx)
  
  top_log <- tmp_mx %>%
    arrange(desc(LOGUNT))
  
  top_log_val <- as.numeric(top_log[1, 1])
  top_log_idx <- as.numeric(top_log[1, 5])
  top_log_pct <- as.numeric(top_log[1, 3])
  top_log_cnt <- as.numeric(top_log[1, 6])
  top_log_unt <- as.numeric(top_log[1, 4])
  
  top_rf <- tmp_mx %>%
    arrange(desc(RFUNT))
  
  top_rf_val <- as.numeric(top_rf[1, 2])
  top_rf_idx <- as.numeric(top_rf[1, 9])
  top_rf_pct <- as.numeric(top_rf[1, 7])
  top_rf_cnt <- as.numeric(top_rf[1, 10])
  top_rf_unt <- as.numeric(top_rf[1, 8])
  
  top_comb <- tmp_mx %>%
    arrange(desc(CMBUNT))
  
  top_comb_log_val <- as.numeric(top_comb[1, 1])
  top_comb_rf_val <- as.numeric(top_comb[1, 2])
  top_comb_idx <- as.numeric(top_comb[1, 13])
  top_comb_pct <- as.numeric(top_comb[1, 11])
  top_comb_cnt <- as.numeric(top_comb[1, 14])
  top_comb_unt <- as.numeric(top_comb[1, 12])
  
  top_log_merge <- tmp_mx %>%
    filter(LOGVAL >= top_log_val) %>%
    arrange(desc(RFUNT))
  
  top_lm_rf_val <- as.numeric(top_log_merge[1, 2])
  top_lm_idx <- as.numeric(top_log_merge[1, 13])
  top_lm_pct <- as.numeric(top_log_merge[1, 11])
  top_lm_cnt <- as.numeric(top_log_merge[1, 14])
  top_lm_unt <- as.numeric(top_comb[1, 12])
  
  top_rf_merge <- tmp_mx %>%
    filter(RFVAL >= top_rf_val) %>%
    arrange(desc(LOGUNT))
  
  top_rm_log_val <- as.numeric(top_rf_merge[1, 1])
  top_rm_idx <- as.numeric(top_rf_merge[1, 13])
  top_rm_pct <- as.numeric(top_rf_merge[1, 11])
  top_rm_cnt <- as.numeric(top_rf_merge[1, 14])
  top_rm_unt <- as.numeric(top_comb[1, 12])
  
  output_mx <<- matrix(0, nrow = 5, ncol = 6)
  
  output_mx[1, 2:6] <<- c(top_log_idx, top_log_unt, top_log_cnt, top_log_val, min_rf)
  output_mx[2, 2:6] <<- c(top_rf_idx, top_rf_unt, top_rf_cnt, min_lp, top_rf_val)
  output_mx[3, 2:6] <<- c(top_comb_idx, top_comb_unt, top_comb_cnt, top_comb_log_val, top_comb_rf_val)
  output_mx[4, 2:6] <<- c(top_lm_idx, top_lm_unt, top_lm_cnt, top_log_val, top_lm_rf_val)
  output_mx[5, 2:6] <<- c(top_rm_idx, top_rm_unt, top_rm_cnt, top_rm_log_val, top_rf_val)
  
  output_mx <<- as.data.frame(output_mx)
  
  output_mx[1, 1] <<- "LOG ONLY" 
  output_mx[2, 1] <<- "RF ONLY" 
  output_mx[3, 1] <<- "COMB ONLY" 
  output_mx[4, 1] <<- "LOG MERGE" 
  output_mx[5, 1] <<- "RF MERGE" 
  
  colnames(output_mx) <- c("Type", "Index", "Unit", 'Count', "Log Val", "RF Val")
  
  View(tmp_mx)
  View(output_mx)
}

prestack_mc_ml <- function(xxxx) {
  
  #ml Logistic and Random Forest
  
  rf_train9 <- sample(c(1:nrow(key_tree_ml)), round(nrow(key_tree_ml) * .9, 0))
  rf_test <- -1 * rf_train9
  rf_train9 <- key_tree_ml[rf_train9, ]
  rf_test <- key_tree_ml[rf_test, ]
  
  
  ml_log9_kit <<- glm(ml_log_form_use, data = rf_train9, family = "binomial")
  ml_rf9_kit <<- randomForest(ml_log_form_use, data = rf_train9)
  live_predict <- predict(ml_log9_kit, rf_test, type = "response")
  rf_predict <<- predict(ml_rf9_kit, rf_test, type = "prob")
  ml_log_out9 <<- data.frame(rf_test, live_predict, rf_predict)
  ml_log_out9 <<- ml_log_out9 %>%
    mutate(PAYOUT = 1 * ((1 - MLCONV) / MLCONV)) %>%
    mutate(LOGEXP = (PAYOUT * live_predict) + (-1 * (1 - live_predict))) %>%
    mutate(RFEXP = (PAYOUT * X1) + (-1 * X0))
  
}

livestack_scree <- function(xmin, xmax, xinc) {
  a <- xmin
  g <- xmax
  ct <- 0
  
  #Log Only
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
    filter(live_predict >= 0)
  
  scree_table <- scree_table %>% arrange(live_predict)
  scree_table[, 6] <- cumsum(scree_table$ACT)
  scree_table <- scree_table %>% arrange(desc(live_predict))
  scree_table[, 7] <- cumsum(scree_table$ACT)
  
  lo_scree_right <- as.numeric(scree_table[which.max(scree_table$V6), 2][1])
  lo_scree_left <- as.numeric(scree_table[which.max(scree_table$V7), 2][1])
  
  filtered_scree <- scree_table %>%
    filter(live_predict >= lo_scree_left & live_predict <= lo_scree_right) 
  
  filtered_table <- table(filtered_scree$ATSRES)
  
  lo_win <- as.numeric(filtered_table[2])
  lo_loss <- as.numeric(filtered_table[1])
  lo_pct <- lo_win / (lo_win + lo_loss)
  lo_unit <- lo_win - (1.1 * lo_loss)
  lo_indx <- lo_unit * lo_pct 
  lo_ctpct <- (lo_win + lo_loss) / nrow(scree_table)
  

  #RF Only
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
    filter(X1 >= 0)
  
  scree_table <- scree_table %>% arrange(X1)
  scree_table[, 6] <- cumsum(scree_table$ACT)
  scree_table <- scree_table %>% arrange(desc(X1))
  scree_table[, 7] <- cumsum(scree_table$ACT)
  
  ro_scree_right <- as.numeric(scree_table[which.max(scree_table$V6), 4][1])
  ro_scree_left <- as.numeric(scree_table[which.max(scree_table$V7), 4][1])

  filtered_scree <- scree_table %>%
    filter(X1 >= ro_scree_left & X1 <= ro_scree_right) 
  
  filtered_table <- table(filtered_scree$ATSRES)
  
  ro_win <- as.numeric(filtered_table[2])
  ro_loss <- as.numeric(filtered_table[1])
  ro_pct <- ro_win / (ro_win + ro_loss)
  ro_unit <- ro_win - (1.1 * ro_loss)
  ro_indx <- ro_unit * ro_pct 
  ro_ctpct <- (ro_win + ro_loss) / nrow(scree_table)
  
  
  while (a <= g) {
    
    scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
      filter(live_predict >= a) %>%
      filter(X1 >= a)
    
    scree_table <- scree_table %>% arrange(live_predict)
    scree_table[, 6] <- cumsum(scree_table$ACT)
    scree_table <- scree_table %>% arrange(desc(live_predict))
    scree_table[, 7] <- cumsum(scree_table$ACT)
    
    scree_table <- scree_table %>% arrange(X1)
    scree_table[, 8] <- cumsum(scree_table$ACT)
    scree_table <- scree_table %>% arrange(desc(X1))
    scree_table[, 9] <- cumsum(scree_table$ACT)
    
    log_scree_right <- as.numeric(scree_table[which.max(scree_table$V6), 2][1])
    log_scree_left <- as.numeric(scree_table[which.max(scree_table$V7), 2][1])
    
    rf_scree_right <- as.numeric(scree_table[which.max(scree_table$V8), 4][1])
    rf_scree_left <- as.numeric(scree_table[which.max(scree_table$V9), 4][1])
    
    #print(log_scree_left)
    #print(log_scree_right)
    
    #print(rf_scree_left)
    #print(rf_scree_right)
    
    #gg <<- ggplot(scree_table, aes(x = live_predict, y = V6)) + geom_point() + geom_point(aes(x = live_predict, y = V7))
    #rr <<- ggplot(scree_table, aes(x = X1, y = V8)) + geom_point() + geom_point(aes(x = X1, y = V9))
    
    filtered_scree <- scree_table %>%
      filter(live_predict >= log_scree_left & live_predict <= log_scree_right) %>%
      filter(X1 >= rf_scree_left & X1 <= rf_scree_right)
    
    #View(filtered_scree)
    
    filtered_table <- table(filtered_scree$ATSRES)
    
    ft_win <- as.numeric(filtered_table[2])
    ft_loss <- as.numeric(filtered_table[1])
    ft_pct <- ft_win / (ft_win + ft_loss)
    ft_unit <- ft_win - (1.1 * ft_loss)
    ft_indx <- ft_unit * ft_pct 
    ft_ctpct <- (ft_win + ft_loss) / nrow(scree_table)

    if (ct == 0) { scree_output <<- matrix(c(a, ft_win, ft_loss, ft_pct, ft_unit, ft_indx, ft_ctpct, 
                                             log_scree_left, log_scree_right, rf_scree_left, rf_scree_right,
                                             lo_pct, lo_unit, lo_ctpct, lo_scree_left, lo_scree_right,
                                             ro_pct, ro_unit, ro_ctpct, ro_scree_left, ro_scree_right), nrow = 1, ncol = 21) }
    
    else {
      
      scree_output2 <- matrix(c(a, ft_win, ft_loss, ft_pct, ft_unit, ft_indx, ft_ctpct, 
                                log_scree_left, log_scree_right, rf_scree_left, rf_scree_right,
                                lo_pct, lo_unit, lo_ctpct, lo_scree_left, lo_scree_right,
                                ro_pct, ro_unit, ro_ctpct, ro_scree_left, ro_scree_right), nrow = 1, ncol = 21)
                              
      scree_output <<- rbind(scree_output, scree_output2)
      
    }
    
    #print(paste(lo_win, lo_loss, lo_pct, lo_unit, lo_indx, lo_ctpct, ro_win, ro_loss, ro_pct, ro_unit, ro_indx, nrow(scree_output)))
    
    
    ct <- ct + 1
    a <- a + xinc
  }
  
  colnames(scree_output) <- c("VAL", "W", "L", "PCT", "UNIT", "INDEX", "CTPCT", "LOGLO", "LOGHI", "RFLO", "RFHI",
                              "LONLY-PCT", "LONLY-UNT", "LONLY-CTP", "LONLY-L", "LONLY-R",
                              "RONLY-PCT", "RONLY-UNT", "RONLY-CTP", "RONLY-L", "RONLY-R")
  
  scree_output_final <<- data.frame(scree_output)
  
  scree_output_final <<- scree_output_final %>%
    arrange(desc(INDEX))
  
  View(scree_output_final)
}

live_scree_set <- function(typ, lin) {
  
  if (typ == "ATS") {
    
    if (lin == -1) {
      
      tree_2h_ats_log_lo <<- 10
      tree_2h_ats_log_hi <<- 10
      tree_2h_ats_rf_lo <<- 10
      tree_2h_ats_rf_hi <<- 10
      
    }
    else {
      
    tree_2h_ats_log_lo <<- scree_output_final$LOGLO[lin]
    tree_2h_ats_log_hi <<- scree_output_final$LOGHI[lin]
    tree_2h_ats_rf_lo <<- scree_output_final$RFLO[lin]
    tree_2h_ats_rf_hi <<- scree_output_final$RFHI[lin]
    
    }
  }
  
  if (typ == "OVER") {
    
    if (lin == -1) {
      
      tree_2h_over_log_lo <<- 10
      tree_2h_over_log_hi <<- 10
      tree_2h_over_rf_lo <<- 10
      tree_2h_over_rf_hi <<- 10
      
    }
    else {
      
      tree_2h_over_log_lo <<- scree_output_final$LOGLO[lin]
      tree_2h_over_log_hi <<- scree_output_final$LOGHI[lin]
      tree_2h_over_rf_lo <<- scree_output_final$RFLO[lin]
      tree_2h_over_rf_hi <<- scree_output_final$RFHI[lin]
      
    }

  }
  
  if (typ == "UNDER") {
    
    if (lin == -1) {
      
      tree_2h_under_log_lo <<- 10
      tree_2h_under_log_hi <<- 10
      tree_2h_under_rf_lo <<- 10
      tree_2h_under_rf_hi <<- 10
      
    }
    
    else {
    
      tree_2h_under_log_lo <<- scree_output_final$LOGLO[lin]
      tree_2h_under_log_hi <<- scree_output_final$LOGHI[lin]
      tree_2h_under_rf_lo <<- scree_output_final$RFLO[lin]
      tree_2h_under_rf_hi <<- scree_output_final$RFHI[lin]
      
    }
  }
}

prestack_scree <- function(xmin, xmax, xinc) {
  a <- xmin
  g <- xmax
  ct <- 0
  
  #Log Only
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(live_predict >= 0)
  
  scree_table <- scree_table %>% arrange(live_predict)
  scree_table[, 6] <- cumsum(scree_table$ACT)
  scree_table <- scree_table %>% arrange(desc(live_predict))
  scree_table[, 7] <- cumsum(scree_table$ACT)
  
  lo_scree_right <- as.numeric(scree_table[which.max(scree_table$V6), 2][1])
  lo_scree_left <- as.numeric(scree_table[which.max(scree_table$V7), 2][1])
  
  filtered_scree <- scree_table %>%
    filter(live_predict >= lo_scree_left & live_predict <= lo_scree_right) 
  
  filtered_table <- table(filtered_scree$RESFAC)
  
  lo_win <- as.numeric(filtered_table[2])
  lo_loss <- as.numeric(filtered_table[1])
  lo_pct <- lo_win / (lo_win + lo_loss)
  lo_unit <- lo_win - (1.1 * lo_loss)
  lo_indx <- lo_unit * lo_pct 
  lo_ctpct <- (lo_win + lo_loss) / nrow(scree_table)
  
  
  #RF Only
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(X1 >= 0)
  
  scree_table <- scree_table %>% arrange(X1)
  scree_table[, 6] <- cumsum(scree_table$ACT)
  scree_table <- scree_table %>% arrange(desc(X1))
  scree_table[, 7] <- cumsum(scree_table$ACT)
  
  ro_scree_right <- as.numeric(scree_table[which.max(scree_table$V6), 4][1])
  ro_scree_left <- as.numeric(scree_table[which.max(scree_table$V7), 4][1])
  
  filtered_scree <- scree_table %>%
    filter(X1 >= ro_scree_left & X1 <= ro_scree_right) 
  
  filtered_table <- table(filtered_scree$RESFAC)
  
  ro_win <- as.numeric(filtered_table[2])
  ro_loss <- as.numeric(filtered_table[1])
  ro_pct <- ro_win / (ro_win + ro_loss)
  ro_unit <- ro_win - (1.1 * ro_loss)
  ro_indx <- ro_unit * ro_pct 
  ro_ctpct <- (ro_win + ro_loss) / nrow(scree_table)
  
  
  while (a <= g) {
    
    scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
      filter(live_predict >= a) %>%
      filter(X1 >= a)
    
    scree_table <- scree_table %>% arrange(live_predict)
    scree_table[, 6] <- cumsum(scree_table$ACT)
    scree_table <- scree_table %>% arrange(desc(live_predict))
    scree_table[, 7] <- cumsum(scree_table$ACT)
    
    scree_table <- scree_table %>% arrange(X1)
    scree_table[, 8] <- cumsum(scree_table$ACT)
    scree_table <- scree_table %>% arrange(desc(X1))
    scree_table[, 9] <- cumsum(scree_table$ACT)
    
    log_scree_right <- as.numeric(scree_table[which.max(scree_table$V6), 2][1])
    log_scree_left <- as.numeric(scree_table[which.max(scree_table$V7), 2][1])
    
    rf_scree_right <- as.numeric(scree_table[which.max(scree_table$V8), 4][1])
    rf_scree_left <- as.numeric(scree_table[which.max(scree_table$V9), 4][1])
    
    #print(log_scree_left)
    #print(log_scree_right)
    
    #print(rf_scree_left)
    #print(rf_scree_right)
    
    gg <<- ggplot(scree_table, aes(x = live_predict, y = V6)) + geom_point() + geom_point(aes(x = live_predict, y = V7))
    rr <<- ggplot(scree_table, aes(x = X1, y = V8)) + geom_point() + geom_point(aes(x = X1, y = V9))
    
    View(scree_table)
    
    filtered_scree <- scree_table %>%
      filter(live_predict >= log_scree_left & live_predict <= log_scree_right) %>%
      filter(X1 >= rf_scree_left & X1 <= rf_scree_right)
    
    
    #View(filtered_scree)
    
    filtered_table <- table(filtered_scree$RESFAC)
    
    ft_win <- as.numeric(filtered_table[2])
    ft_loss <- as.numeric(filtered_table[1])
    ft_pct <- ft_win / (ft_win + ft_loss)
    ft_unit <- ft_win - (1.1 * ft_loss)
    ft_indx <- ft_unit * ft_pct 
    ft_ctpct <- (ft_win + ft_loss) / nrow(scree_table)
    
    if (ct == 0) { scree_output <<- matrix(c(a, ft_win, ft_loss, ft_pct, ft_unit, ft_indx, ft_ctpct, 
                                             log_scree_left, log_scree_right, rf_scree_left, rf_scree_right,
                                             lo_pct, lo_unit, lo_ctpct, lo_scree_left, lo_scree_right,
                                             ro_pct, ro_unit, ro_ctpct, ro_scree_left, ro_scree_right), nrow = 1, ncol = 21) }
    
    else {
      
      scree_output2 <- matrix(c(a, ft_win, ft_loss, ft_pct, ft_unit, ft_indx, ft_ctpct, 
                                log_scree_left, log_scree_right, rf_scree_left, rf_scree_right,
                                lo_pct, lo_unit, lo_ctpct, lo_scree_left, lo_scree_right,
                                ro_pct, ro_unit, ro_ctpct, ro_scree_left, ro_scree_right), nrow = 1, ncol = 21)
      
      scree_output <<- rbind(scree_output, scree_output2)
      
    }
    
    #print(paste(lo_win, lo_loss, lo_pct, lo_unit, lo_indx, lo_ctpct, ro_win, ro_loss, ro_pct, ro_unit, ro_indx, nrow(scree_output)))
    
    
    ct <- ct + 1
    a <- a + xinc
  }
  
  colnames(scree_output) <- c("VAL", "W", "L", "PCT", "UNIT", "INDEX", "CTPCT", "LOGLO", "LOGHI", "RFLO", "RFHI",
                              "LONLY-PCT", "LONLY-UNT", "LONLY-CTP", "LONLY-L", "LONLY-R",
                              "RONLY-PCT", "RONLY-UNT", "RONLY-CTP", "RONLY-L", "RONLY-R")
  
  scree_output_final <<- data.frame(scree_output)
  
  scree_output_final <<- scree_output_final %>%
    arrange(desc(INDEX))
  
  View(scree_output_final)
}

pre_scree_set <- function(typ, lin) {
  
  if (typ == "ATS") {
    
    if (lin == -1) {
      
      tree_ats_log_lo <<- 10
      tree_ats_log_hi <<- 10
      tree_ats_rf_lo <<- 10
      tree_ats_rf_hi <<- 10
      
    }
    else {
      
      tree_ats_log_lo <<- scree_output_final$LOGLO[lin]
      tree_ats_log_hi <<- scree_output_final$LOGHI[lin]
      tree_ats_rf_lo <<- scree_output_final$RFLO[lin]
      tree_ats_rf_hi <<- scree_output_final$RFHI[lin]
      
    }
  }
  
  if (typ == "OVER") {
    
    if (lin == -1) {
      
      tree_over_log_lo <<- 10
      tree_over_log_hi <<- 10
      tree_over_rf_lo <<- 10
      tree_over_rf_hi <<- 10
      
    }
    else {
      
      tree_over_log_lo <<- scree_output_final$LOGLO[lin]
      tree_over_log_hi <<- scree_output_final$LOGHI[lin]
      tree_over_rf_lo <<- scree_output_final$RFLO[lin]
      tree_over_rf_hi <<- scree_output_final$RFHI[lin]
      
    }
    
  }
  
  if (typ == "UNDER") {
    
    if (lin == -1) {
      
      tree_under_log_lo <<- 10
      tree_under_log_hi <<- 10
      tree_under_rf_lo <<- 10
      tree_under_rf_hi <<- 10
      
    }
    
    else {
      
      tree_under_log_lo <<- scree_output_final$LOGLO[lin]
      tree_under_log_hi <<- scree_output_final$LOGHI[lin]
      tree_under_rf_lo <<- scree_output_final$RFLO[lin]
      tree_under_rf_hi <<- scree_output_final$RFHI[lin]
      
    }
  }
}

prestack_scree2 <- function(xmin, xmax, xinc) {
  
  a <- xmin
  g <- xmax
  ct <- 0
  
  #LOG ONLY
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(live_predict >= 0)
  
  scree_table <- scree_table %>% arrange(live_predict)
  scree_table[, 6] <- cumsum(scree_table$ACT)
  scree_table <- scree_table %>% arrange(desc(live_predict))
  scree_table[, 7] <- cumsum(scree_table$ACT)
  scree_table[, 8] <- abs(scree_table[, 6] - scree_table[, 7])
  
  scree_max <- max(scree_table$V6)
  scree_min <- min(scree_table$V6)
  scree_maxdifw <- which.max(scree_table$V8)[1]
  
  scree_md_v6 <- scree_table$V6[scree_maxdifw]
  
  if (scree_md_v6 == scree_max) { 
    scree_chop <- scree_table[scree_maxdifw:nrow(scree_table), ]
    scree_lo_bot <- scree_chop$live_predict[which.min(scree_chop$V6)[1]]
    scree_lo_top <- scree_chop$live_predict[which.max(scree_chop$V6)[1]]
  }
  else {
    scree_chop <- scree_table[1:scree_maxdifw, ]
    scree_lo_bot <- scree_chop$live_predict[which.min(scree_chop$V6)[1]]
    scree_lo_top <- scree_chop$live_predict[which.max(scree_chop$V6)[1]]
  }
  
  filtered_scree <- scree_table %>%
    filter(live_predict >= scree_lo_bot & live_predict <= scree_lo_top)
  
  #View(filtered_scree)
  
  filtered_table <- table(filtered_scree$RESFAC)
  
  lo_win <- as.numeric(filtered_table[2])
  lo_loss <- as.numeric(filtered_table[1])
  lo_pct <- lo_win / (lo_win + lo_loss)
  lo_unit <- lo_win - (1.1 * lo_loss)
  lo_indx <- lo_unit * lo_pct 
  lo_ctpct <- (lo_win + lo_loss) / nrow(scree_table)
  
  #RF ONLY
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(X1 >= 0)
  
  scree_table <- scree_table %>% arrange(X1)
  scree_table[, 6] <- cumsum(scree_table$ACT)
  scree_table <- scree_table %>% arrange(desc(X1))
  scree_table[, 7] <- cumsum(scree_table$ACT)
  scree_table[, 8] <- abs(scree_table[, 6] - scree_table[, 7])
  
  scree_max <- max(scree_table$V6)
  scree_min <- min(scree_table$V6)
  scree_maxdifw <- which.max(scree_table$V8)[1]
  
  scree_md_v6 <- scree_table$V6[scree_maxdifw]
  
  if (scree_md_v6 == scree_max) { 
    scree_chop <- scree_table[scree_maxdifw:nrow(scree_table), ]
    scree_ro_bot <- scree_chop$X1[which.min(scree_chop$V6)[1]]
    scree_ro_top <- scree_chop$X1[which.max(scree_chop$V6)[1]]
  }
  else {
    scree_chop <- scree_table[1:scree_maxdifw, ]
    scree_ro_bot <- scree_chop$X1[which.min(scree_chop$V6)[1]]
    scree_ro_top <- scree_chop$X1[which.max(scree_chop$V6)[1]]
  }
  
  #rr <<- ggplot(scree_table, aes(x = X1, y = V6)) + geom_point() + geom_point(aes(x = X1, y = V7), col="blue")
  
  filtered_scree <- scree_table %>%
    filter(X1 >= scree_ro_bot & X1 <= scree_ro_top)
  
  #View(filtered_scree)
  
  filtered_table <- table(filtered_scree$RESFAC)
  
  ro_win <- as.numeric(filtered_table[2])
  ro_loss <- as.numeric(filtered_table[1])
  ro_pct <- ro_win / (ro_win + ro_loss)
  ro_unit <- ro_win - (1.1 * ro_loss)
  ro_indx <- ro_unit * ro_pct 
  ro_ctpct <- (ro_win + ro_loss) / nrow(scree_table)
  
  
  while (a <= g) {
    
    scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
      filter(live_predict >= a) %>%
      filter(X1 >= a)
    
    #LOG
    
    scree_table <- scree_table %>% arrange(live_predict)
    scree_table[, 6] <- cumsum(scree_table$ACT)
    scree_table <- scree_table %>% arrange(desc(live_predict))
    scree_table[, 7] <- cumsum(scree_table$ACT)
    scree_table[, 8] <- abs(scree_table[, 6] - scree_table[, 7])
    
    scree_max <- max(scree_table$V6)
    scree_min <- min(scree_table$V6)
    scree_maxdifw <- which.max(scree_table$V8)[1]
    
    scree_md_v6 <- scree_table$V6[scree_maxdifw]
    
    if (scree_md_v6 == scree_max) { 
      scree_chop <- scree_table[scree_maxdifw:nrow(scree_table), ]
      scree_log_bot <- scree_chop$live_predict[which.min(scree_chop$V6)[1]]
      scree_log_top <- scree_chop$live_predict[which.max(scree_chop$V6)[1]]
    }
    else {
      scree_chop <- scree_table[1:scree_maxdifw, ]
      scree_log_bot <- scree_chop$live_predict[which.min(scree_chop$V6)[1]]
      scree_log_top <- scree_chop$live_predict[which.max(scree_chop$V6)[1]]
    }
    
    #print(scree_log_bot)
    #print(scree_log_top)
    
    #RF
    
    scree_table <- scree_table %>% arrange(X1)
    scree_table[, 9] <- cumsum(scree_table$ACT)
    scree_table <- scree_table %>% arrange(desc(X1))
    scree_table[, 10] <- cumsum(scree_table$ACT)
    scree_table[, 11] <- abs(scree_table[, 9] - scree_table[, 10])
    
    #View(scree_table)
    rr <<- ggplot(scree_table, aes(x = live_predict, y = V6)) + geom_point() + geom_point(aes(x = live_predict, y = V7), col="blue")
    #rr <<- ggplot(scree_table, aes(x = X1, y = V11)) + geom_point()
    
    scree_max <- max(scree_table$V9)
    scree_min <- min(scree_table$V9)
    scree_maxdifw <- which.max(scree_table$V11)[1]
    
    scree_md_v6 <- scree_table$V9[scree_maxdifw]
    
    if (scree_md_v6 == scree_max) { 
      scree_chop <- scree_table[scree_maxdifw:nrow(scree_table), ]
      scree_rf_bot <- scree_chop$X1[which.min(scree_chop$V9)[1]]
      scree_rf_top <- scree_chop$X1[which.max(scree_chop$V9)[1]]
    }
    else {
      scree_chop <- scree_table[1:scree_maxdifw, ]
      scree_rf_bot <- scree_chop$X1[which.min(scree_chop$V9)[1]]
      scree_rf_top <- scree_chop$X1[which.max(scree_chop$V9)[1]]
    }
    
    #print(scree_rf_bot)
    #print(scree_rf_top)
    
    filtered_scree <- scree_table %>%
      filter(live_predict >= scree_log_bot & live_predict <= scree_log_top) %>%
      filter(X1 >= scree_rf_bot & X1 <= scree_rf_top)
    
    #View(filtered_scree)
    
    filtered_table <- table(filtered_scree$RESFAC)
    
    ft_win <- as.numeric(filtered_table[2])
    ft_loss <- as.numeric(filtered_table[1])
    ft_pct <- ft_win / (ft_win + ft_loss)
    ft_unit <- ft_win - (1.1 * ft_loss)
    ft_indx <- ft_unit * ft_pct 
    ft_ctpct <- (ft_win + ft_loss) / nrow(scree_table)
    
    if (ct == 0) { scree_output <<- matrix(c(a, ft_win, ft_loss, ft_pct, ft_unit, ft_indx, ft_ctpct, 
                                             scree_log_bot, scree_log_top, scree_rf_bot, scree_rf_top,
                                             lo_pct, lo_unit, lo_ctpct, scree_lo_bot, scree_lo_top,
                                             ro_pct, ro_unit, ro_ctpct, scree_ro_bot, scree_ro_top), nrow = 1, ncol = 21) }
    
    else {
      
      scree_output2 <- matrix(c(a, ft_win, ft_loss, ft_pct, ft_unit, ft_indx, ft_ctpct, 
                                scree_log_bot, scree_log_top, scree_rf_bot, scree_rf_top,
                                lo_pct, lo_unit, lo_ctpct, scree_lo_bot, scree_lo_top,
                                ro_pct, ro_unit, ro_ctpct, scree_ro_bot, scree_ro_top), nrow = 1, ncol = 21)
      
      scree_output <<- rbind(scree_output, scree_output2)
      
    }
    
    ct <- ct + 1
    a <- a + xinc
    
  }
  
  colnames(scree_output) <- c("VAL", "W", "L", "PCT", "UNIT", "INDEX", "CTPCT", "LOGLO", "LOGHI", "RFLO", "RFHI",
                              "LONLY-PCT", "LONLY-UNT", "LONLY-CTP", "LONLY-L", "LONLY-R",
                              "RONLY-PCT", "RONLY-UNT", "RONLY-CTP", "RONLY-L", "RONLY-R")
  
  scree_output_final <<- data.frame(scree_output)
  
  scree_output_final <<- scree_output_final %>%
    arrange(desc(INDEX))
  
  View(scree_output_final)
}

prestack_scree4 <- function(lowx, highx, lowr, highr, typ, no) {
  
  #Log Only
  
  scree_table_log <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(live_predict >= lowx & live_predict <= highx)
  
  scree_table_log <- scree_table_log %>% arrange(live_predict)
  scree_table_log[, 6] <- cumsum(scree_table_log$ACT)
  
  s_min <- min(scree_table_log$V6)
  s_max <- max(scree_table_log$V6)
  w_min <- which.min(scree_table_log$V6)[1]
  w_max <- which.max(scree_table_log$V6)[1]
  x_min <- scree_table_log$live_predict[w_min]
  x_max <- scree_table_log$live_predict[w_max]
  
  print(c(x_min, x_max))
  
  ll <<- ggplot(scree_table_log, aes(x = live_predict, y = V6)) + geom_point()
  
  #RF Only
  
  scree_table_rf <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(X1 >= lowr & X1 <= highr)
  
  scree_table_rf <- scree_table_rf %>% arrange(X1)
  scree_table_rf[, 6] <- cumsum(scree_table_rf$ACT)
  
  s_min <- min(scree_table_rf$V6)
  s_max <- max(scree_table_rf$V6)
  w_min <- which.min(scree_table_rf$V6)[1]
  w_max <- which.max(scree_table_rf$V6)[1]
  x_min <- scree_table_rf$X1[w_min]
  x_max <- scree_table_rf$X1[w_max]
  
  print(c(x_min, x_max))
  
  rr <<- ggplot(scree_table_rf, aes(x = X1, y = V6)) + geom_point()
  
  #Combo?
  
  scree_table_rf <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(X1 >= lowr & live_predict >= lowx) %>%
    filter(X1 <= highr & live_predict <= highx)
  
  scree_table_rf <- scree_table_rf %>% arrange(live_predict)
  scree_table_rf[, 6] <- cumsum(scree_table_rf$ACT)
  scree_table_rf <- scree_table_rf %>% arrange(X1)
  scree_table_rf[, 7] <- cumsum(scree_table_rf$ACT)
  
  cc <<- ggplot(scree_table_rf, aes(x = live_predict, y = V6)) + geom_point() + geom_point(aes(x = X1, y = V7), col = "blue")
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(live_predict >= lowx & live_predict <= highx) 
  
  w_ <- as.numeric(format(round(table(scree_table$RESFAC)[2], 0), nsmall = 0))
  l_ <- as.numeric(format(round(table(scree_table$RESFAC)[1], 0), nsmall = 0))
  u_ <- w_ - (l_ * -1.1)
  pl_ <- w_ / (w_ + l_)
  print(c(w_, l_, u_, pl_, ((w_ + l_) / nrow(pre_key_master_df))))
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(X1 >= lowr & X1 <= highr) 
  
  w_ <- as.numeric(format(round(table(scree_table$RESFAC)[2], 0), nsmall = 0))
  l_ <- as.numeric(format(round(table(scree_table$RESFAC)[1], 0), nsmall = 0))
  u_ <- w_ - (l_ * -1.1)
  pr_ <- w_ / (w_ + l_)
  print(c(w_, l_, u_, pr_, ((w_ + l_) / nrow(pre_key_master_df))))
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    filter(X1 >= lowr & X1 <= highr) %>%
    filter(live_predict >= lowx & live_predict <= highx)
  
  w_ <- as.numeric(format(round(table(scree_table$RESFAC)[2], 0), nsmall = 0))
  l_ <- as.numeric(format(round(table(scree_table$RESFAC)[1], 0), nsmall = 0))
  u_ <- w_ - (l_ * -1.1)
  pc_ <- w_ / (w_ + l_)
  print(c(w_, l_, u_, pc_, ((w_ + l_) / nrow(pre_key_master_df))))
  
  if (typ == "ATS") {
    
    tree_ats_matrix[no, 1] <<- lowx
    tree_ats_matrix[no, 2] <<- highx
    tree_ats_matrix[no, 3] <<- lowr
    tree_ats_matrix[no, 4] <<- highr
    tree_ats_matrix[no, 5] <<- pl_
    tree_ats_matrix[no, 6] <<- pr_
    tree_ats_matrix[no, 7] <<- pc_
    
  }
  
  if (typ == "OVER") {
    
    tree_over_matrix[no, 1] <<- lowx
    tree_over_matrix[no, 2] <<- highx
    tree_over_matrix[no, 3] <<- lowr
    tree_over_matrix[no, 4] <<- highr
    tree_over_matrix[no, 5] <<- pl_
    tree_over_matrix[no, 6] <<- pr_
    tree_over_matrix[no, 7] <<- pc_
    
  }
  
  if (typ == "UNDER") {
    
    tree_under_matrix[no, 1] <<- lowx
    tree_under_matrix[no, 2] <<- highx
    tree_under_matrix[no, 3] <<- lowr
    tree_under_matrix[no, 4] <<- highr
    tree_under_matrix[no, 5] <<- pl_
    tree_under_matrix[no, 6] <<- pr_
    tree_under_matrix[no, 7] <<- pc_
    
  }

}

prestack_scree5 <- function(lowx, highx, lowr, highr, typ, no) {
  
  #Log Only
  
  scree_table_log <- pre_key_master_df %>% mutate(ACT = ifelse(RESFAC == 1, 1, -1.1)) %>%
    mutate(LOGPRED = ifelse(live_predict > .5, 1, 0)) %>%
    mutate(LOGCONF = ifelse(live_predict > .5, live_predict, 1 - live_predict)) %>%
    mutate(LOGRES = ifelse(LOGPRED == RESFAC, 1, -1.1)) %>%
    filter(LOGCONF >= lowx & LOGCONF <= highx)
  
  scree_table_log <- scree_table_log %>% arrange(LOGCONF)
  scree_table_log[, 9] <- cumsum(scree_table_log$ACT)
  
  s_min <- min(scree_table_log$V9)
  s_max <- max(scree_table_log$V9)
  w_min <- which.min(scree_table_log$V9)[1]
  w_max <- which.max(scree_table_log$V9)[1]
  x_min <- scree_table_log$LOGCONF[w_min]
  x_max <- scree_table_log$LOGCONF[w_max]
  
  print(c(x_min, x_max))
  
  ll <<- ggplot(scree_table_log, aes(x = LOGCONF, y = V9)) + geom_point()
  
  #RF Only
  
  scree_table_rf <- pre_key_master_df %>%
    mutate(RFPRED = ifelse(X1 > .5, 1, 0)) %>%
    mutate(RFCONF = ifelse(X1 > .5, X1, X0)) %>%
    mutate(RFRES = ifelse(RFPRED == RESFAC, 1, -1.1)) %>%
    filter(RFCONF >= lowr & RFCONF <= highr)
  
  scree_table_rf <- scree_table_rf %>% arrange(RFCONF)
  scree_table_rf[, 8] <- cumsum(scree_table_rf$RFRES)
  
  View(scree_table_log)
  View(scree_table_rf)
  
  s_min <- min(scree_table_rf$V8)
  s_max <- max(scree_table_rf$V8)
  w_min <- which.min(scree_table_rf$V8)[1]
  w_max <- which.max(scree_table_rf$V8)[1]
  x_min <- scree_table_rf$RFCONF[w_min]
  x_max <- scree_table_rf$RFCONF[w_max]
  
  print(c(x_min, x_max))
  
  rr <<- ggplot(scree_table_rf, aes(x = RFCONF, y = V8)) + geom_point()
  
}

livestack_scree4 <- function(lowx, highx, lowr, highr, typ, no) {
  
  #Log Only
  
  scree_table_log <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
    filter(live_predict >= lowx & live_predict <= highx)
  
  scree_table_log <- scree_table_log %>% arrange(live_predict)
  scree_table_log[, 6] <- cumsum(scree_table_log$ACT)
  
  s_min <- min(scree_table_log$V6)
  s_max <- max(scree_table_log$V6)
  w_min <- which.min(scree_table_log$V6)[1]
  w_max <- which.max(scree_table_log$V6)[1]
  x_min <- scree_table_log$live_predict[w_min]
  x_max <- scree_table_log$live_predict[w_max]
  
  print(c(x_min, x_max))
  
  ll <<- ggplot(scree_table_log, aes(x = live_predict, y = V6)) + geom_point()
  
  #RF Only
  
  scree_table_rf <- pre_key_master_df %>%
    mutate(RFPRED = ifelse(X1 > .5, 1, 0)) %>%
    mutate(RFCONF = ifelse(X1 > .5, X1, X0)) %>%
    mutate(RFRES = ifelse(RFPRED == ATSRES, 1, -1.1))
  
  scree_table_rf <- scree_table_rf %>% arrange(RFCONF)
  scree_table_rf[, 8] <- cumsum(scree_table_rf$RFRES)
  
  View(scree_table_rf)
  
  s_min <- min(scree_table_rf$V8)
  s_max <- max(scree_table_rf$V8)
  w_min <- which.min(scree_table_rf$V8)[1]
  w_max <- which.max(scree_table_rf$V8)[1]
  x_min <- scree_table_rf$RFCONF[w_min]
  x_max <- scree_table_rf$RFCONF[w_max]
  
  print(c(x_min, x_max))
  
  rr <<- ggplot(scree_table_rf, aes(x = X1, y = V8)) + geom_point()
}
  
  
  
  #Combo?
  
  scree_table_rf <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
    filter(X1 >= lowr & live_predict >= lowx) %>%
    filter(X1 <= highr & live_predict <= highx)
  
  scree_table_rf <- scree_table_rf %>% arrange(live_predict)
  scree_table_rf[, 6] <- cumsum(scree_table_rf$ACT)
  scree_table_rf <- scree_table_rf %>% arrange(X1)
  scree_table_rf[, 7] <- cumsum(scree_table_rf$ACT)
  
  cc <<- ggplot(scree_table_rf, aes(x = live_predict, y = V6)) + geom_point() + geom_point(aes(x = X1, y = V7), col = "blue")
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
    filter(live_predict >= lowx & live_predict <= highx) 
  
  w_ <- as.numeric(format(round(table(scree_table$ATSRES)[2], 0), nsmall = 0))
  l_ <- as.numeric(format(round(table(scree_table$ATSRES)[1], 0), nsmall = 0))
  u_ <- w_ + (l_ * -1.1)
  pl_ <- w_ / (w_ + l_)
  print(c(w_, l_, u_, pl_, ((w_ + l_) / nrow(pre_key_master_df))))
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
    filter(X1 >= lowr & X1 <= highr) 
  
  w_ <- as.numeric(format(round(table(scree_table$ATSRES)[2], 0), nsmall = 0))
  l_ <- as.numeric(format(round(table(scree_table$ATSRES)[1], 0), nsmall = 0))
  u_ <- w_ + (l_ * -1.1)
  pr_ <- w_ / (w_ + l_)
  print(c(w_, l_, u_, pr_, ((w_ + l_) / nrow(pre_key_master_df))))
  
  scree_table <- pre_key_master_df %>% mutate(ACT = ifelse(ATSRES == 1, 1, -1.1)) %>%
    filter(X1 >= lowr & X1 <= highr) %>%
    filter(live_predict >= lowx & live_predict <= highx)
  
  w_ <- as.numeric(format(round(table(scree_table$ATSRES)[2], 0), nsmall = 0))
  l_ <- as.numeric(format(round(table(scree_table$ATSRES)[1], 0), nsmall = 0))
  u_ <- w_ + (l_ * -1.1)
  pc_ <- w_ / (w_ + l_)
  print(c(w_, l_, u_, pc_, ((w_ + l_) / nrow(pre_key_master_df))))
  
  if (typ == "ATS") {
    
    tree_2h_ats_matrix[no, 1] <<- lowx
    tree_2h_ats_matrix[no, 2] <<- highx
    tree_2h_ats_matrix[no, 3] <<- lowr
    tree_2h_ats_matrix[no, 4] <<- highr
    tree_2h_ats_matrix[no, 5] <<- pl_
    tree_2h_ats_matrix[no, 6] <<- pr_
    tree_2h_ats_matrix[no, 7] <<- pc_
    
  }
  
  if (typ == "OVER") {
    
    tree_2h_over_matrix[no, 1] <<- lowx
    tree_2h_over_matrix[no, 2] <<- highx
    tree_2h_over_matrix[no, 3] <<- lowr
    tree_2h_over_matrix[no, 4] <<- highr
    tree_2h_over_matrix[no, 5] <<- pl_
    tree_2h_over_matrix[no, 6] <<- pr_
    tree_2h_over_matrix[no, 7] <<- pc_
    
  }
  
  if (typ == "UNDER") {
    
    tree_2h_under_matrix[no, 1] <<- lowx
    tree_2h_under_matrix[no, 2] <<- highx
    tree_2h_under_matrix[no, 3] <<- lowr
    tree_2h_under_matrix[no, 4] <<- highr
    tree_2h_under_matrix[no, 5] <<- pl_
    tree_2h_under_matrix[no, 6] <<- pr_
    tree_2h_under_matrix[no, 7] <<- pc_
    
  }
}

tree_matrix_reset <- function(x) {
  
  tree_ats_matrix <<- matrix(0, nrow = x, ncol = 14)
  tree_total_matrix <<- matrix(0, nrow = x, ncol = 14)
  tree_ml_matrix <<- matrix(0, nrow = x, ncol = 14)
  tree_2h_ats_matrix <<- matrix(0, nrow = x, ncol = 14)
  tree_2h_total_matrix <<- matrix(0, nrow = x, ncol = 14)
  
}


#---------------------------------------
#LAST STAND
#---------------------------------------

model_select_loop <- function(trials) {
  
  print("ATS")
  model_fit_juggle("ATS", "PRE", trials)
  print("OVER")
  model_fit_juggle("OVER", "PRE", trials)
  print("UNDER")
  model_fit_juggle("UNDER", "PRE", trials)
  
}

model_fit_prep <- function(x) {
  
  val_corr <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  print("ATS Load")
  
  key_tree_ats <<- master_df_lineup_ats %>%
    filter(RES != 0) %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0))
  
  key_tree_ats$RESFAC <<- as.factor(key_tree_ats$RESFAC)
  
  key_tree_over <<- master_df_lineup_tot %>%
    filter(BTYPE == "OVER") %>%
    filter(RES != 0) %>%
    mutate(RESFAC = ifelse(RES > 0, 0, 1)) %>%
    mutate(TOTPLAY = 0) %>%
    mutate(TOTRES = ifelse(RESFAC == 0, 1, 0)) %>%
    mutate(TOTUNIT = ifelse(RESFAC == 0, 1, -1.1))
  
  key_tree_under <<- master_df_lineup_tot %>%
    filter(BTYPE == "UNDER") %>%
    filter(RES != 0) %>%
    mutate(RESFAC = ifelse(RES > 0, 1, 0)) %>%
    mutate(TOTPLAY = 1) %>%
    mutate(TOTRES = ifelse(RESFAC == 1, 1, 0)) %>%
    mutate(TOTUNIT = ifelse(RESFAC == 1, 1, -1.1))
  
  #key_tree_under[, 5:10] <<- key_tree_under[, 5:10] * -1
  
  print("Total Load")
  
  key_tree_tot <<- rbind(key_tree_over, key_tree_under) %>%
    filter(RES != 0)
  
  key_tree_tot$RESFAC <<- as.factor(key_tree_tot$RESFAC)
  
  ktt_over <- length(which(key_tree_tot$RESFAC == 0))
  ktt_undr <- length(which(key_tree_tot$RESFAC == 1))
  
  ktt_u_sample <- key_tree_tot %>%
    filter(RESFAC == 1)
  
  ktt_o_sample <- key_tree_tot %>%
    filter(RESFAC == 0)
  
  if (ktt_undr > ktt_over) { 
    ktt_u_sample <- ktt_u_sample[sample(c(1:ktt_undr), ktt_over), ]
    key_tree_tot <<- rbind(ktt_u_sample, ktt_o_sample)
  }
  else if (ktt_undr < ktt_over) { 
    ktt_o_sample <- ktt_o_sample[sample(c(1:ktt_over), ktt_undr), ]
    key_tree_tot <<- rbind(ktt_u_sample, ktt_o_sample)
  }
  else { key_tree_tot <<- key_tree_tot }
  
  key_tree_tot <<- key_tree_tot %>%
    select(TOTPLAY, TEAM, OPP, DAY, INDY, TEAMC, BLND, ATM, SIM, COMB, RES, RESFAC) %>%
    filter(TOTPLAY == 0)
  
  #Prepare Live Log Frames
  
  print("Live ATS Load")
  
  livelog_file_prepare(1)
  
  live_tree_ats <<- ats_log_2h %>%
    filter(RES != 0) %>%
    select(AORH, TEAM, OPP, LIVELINE, INDY, TEAMC, BLND, ATM, SIM, COMB, RES, RESFAC)
  
  print("Live Total Load")
  
  live_tree_tot <<- rbind(over_log_2h, under_log_2h) %>%
    filter(RES != 0)
  
  ktt_over <- length(which(live_tree_tot$RESFAC == 0))
  ktt_undr <- length(which(live_tree_tot$RESFAC == 1))
  
  ktt_u_sample <- live_tree_tot %>%
    filter(RESFAC == 1)
  
  ktt_o_sample <- live_tree_tot %>%
    filter(RESFAC == 0)
  
  if (ktt_undr > ktt_over) { 
    ktt_u_sample <- ktt_u_sample[sample(c(1:ktt_undr), ktt_over), ]
    live_tree_tot <<- rbind(ktt_u_sample, ktt_o_sample)
  }
  else if (ktt_undr < ktt_over) { 
    ktt_o_sample <- ktt_o_sample[sample(c(1:ktt_over), ktt_undr), ]
    live_tree_tot <<- rbind(ktt_u_sample, ktt_o_sample)
  }
  else { live_tree_tot <<- live_tree_tot }
  
  live_tree_tot <<- live_tree_tot %>%
    select(AORH, TEAM, OPP, LIVELINE, INDY, TEAMC, BLND, ATM, SIM, COMB, RES, RESFAC) %>%
    filter(AORH == "O")
  
}

model_ml_juggle <- function(trials, ml_bot, ml_top) {
  
  pre_key_ml_set(ml_bot, ml_top)
  
  b_seed <<- sample(c(1:100), 1)
  
  set.seed(b_seed)
  
  key_tree_ml <<- key_tree_ml %>%
    select(TEAM, OPP, DAY, RES, LINE, RESFAC, MLCONV, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    mutate(PAYOUT =  1 * ((1 - MLCONV) / MLCONV))
  
  colnames(key_tree_ml)[6] <<- "RESFAC"
  key_tree_ml$RESFAC <<- as.factor(key_tree_ml$RESFAC)
  df <- key_tree_ml
  
  r = "RESFAC"
  
  cmb <- read_csv("keycombomx.csv")
  fill_mx_log <- cmb
  a <- 1
  g <- nrow(cmb)
  g <- 1

  for (a in a:g) {
    
    b <- 2
    h <- as.numeric(cmb[a, 1]) + 1
    
    for (b in b:h) {
      
      f = as.character(cmb[a, b])
      
      if (b == 2) { 
        form <- paste(r, " ~ LINE + ", f, sep = "") 
        hold_out <- which(val_corr == f) + 4
      }
      else { 
        form <- paste(form, " + ", f, sep = "") 
        hold_out[(b - 1)] <<- which(val_corr == f) + 4
      }
    }
    
    form <- formula(form)
    form_txt <- as.character(form)[3]
    form_ct <- str_count(form_txt, "\\+") + 1
    print(c(form_txt, form_ct))
    
    #LOG MONTE CARLO LOOP
    set.seed(b_seed)
    b <- 1
    h <- trials
    for (b in b:h) {
      
      rf_train <- sample(c(1:nrow(df)), round(nrow(df) * .7, 0))
      rf_test <- -1 * rf_train
      rf_train <- df[rf_train, ]
      rf_test <- df[rf_test, ]
      
      m_log_fit <- glm(form, data = rf_train, family = "binomial")
      live_predict <- predict(m_log_fit, rf_test, type = "response")
      if (b == 1) {
        pred_log <- data.frame(rf_test, live_predict)
      }
      else {
        pred_log2 <- data.frame(rf_test, live_predict)
        pred_log <- rbind(pred_log, pred_log2)
      }
    }
    
    p_log <- pred_log %>%
      mutate(LOGVSML = live_predict - MLCONV) %>%
      mutate(EXPVAL2 = (live_predict * PAYOUT) + ((1 - live_predict) * - 1)) %>%
      filter(EXPVAL2 > 0) %>%
      arrange(EXPVAL2)
    
    p_log[, 18] <- cumsum(p_log$RES)
    
    p_w <- length(which(p_log$RESFAC == 1))
    p_l <- length(which(p_log$RESFAC == 0))
    p_p <- p_w / (p_w + p_l)
    p_max <- max(p_log$V18)
    fill_mx_log[a, 8] <- p_w
    fill_mx_log[a, 9] <- p_l
    fill_mx_log[a, 10] <- p_p
    fill_mx_log[a, 11] <- p_max
    
    #RANDOM FOREST MONTE CARLO LOOP
    set.seed(b_seed)
    b <- 1
    h <- 1
    for (b in b:h) {
      
      
      rf_train <- sample(c(1:nrow(df)), round(nrow(df) * 1, 0))
      rf_test <- sample(c(1:nrow(df)), round(nrow(df) * .1, 0))
      rf_train <- df[rf_train, ]
      rf_test <- df[rf_test, ]
      
      m_log_fit <- randomForest(form, data = rf_train, mtry = form_ct, ntree=600)
      rf_predict <<- predict(m_log_fit, rf_test, type = "prob")
      if (b == 1) {
        pred_log <- data.frame(rf_test, rf_predict)
      }
      else {
        pred_log2 <- data.frame(rf_test, rf_predict)
        pred_log <- rbind(pred_log, pred_log2)
      }
    }
    err_rate <- m_log_fit$err.rate[600,1]
    p_log <- pred_log %>%
      mutate(RFVSML = X1 - MLCONV) %>%
      mutate(RFVAL2 = (X1 * PAYOUT) + ((1 - X1) * - 1)) %>%
      filter(RFVAL2 > 0) %>%
      mutate(ERRATE = err_rate) %>%
      arrange(RFVAL2)
    
    p_log[, 20] <- cumsum(p_log$RES)
    
    p_w <- length(which(p_log$RESFAC == 1))
    p_l <- length(which(p_log$RESFAC == 0))
    p_p <- p_w / (p_w + p_l)
    p_max <- max(p_log$V20)
    fill_mx_log[a, 12] <- p_w
    fill_mx_log[a, 13] <- p_l
    fill_mx_log[a, 14] <- p_p
    fill_mx_log[a, 15] <- p_max
    fill_mx_log[a, 16] <- as.numeric(p_log$ERRATE[1])
    
  }
  
  fill_mx_log[, 17] <- c(1:nrow(fill_mx_log))
  colnames(fill_mx_log)[8:17] <- c("LOGW", "LOGL", "LOGP", "LOGU", "RFW", "RFL", "RFP", "RFU", "RFERR", "NO")
  fill_mx_log <<- fill_mx_log
  top_model_log <- which.max(fill_mx_log$LOGU)[1]
  top_model_rf <- which.max(fill_mx_log$RFU)[1]

  model_select(top_model_log, "ML", "PRE", "LOG")
  model_select(top_model_rf, "ML", "PRE", "RF")
}

model_fit_juggle <- function(typ, kind, trials) {
  
  b_seed <<- sample(c(1:100), 1)
  
  cmb <- read_csv("keycombomx.csv")
  fill_mx_log <- cmb
  a <- 1
  g <- nrow(cmb)
  #g <- 11
  
  r <- "RESFAC"
  
  for (a in a:g) {
    
    b <- 2
    h <- as.numeric(cmb[a, 1]) + 1
    
    for (b in b:h) {
      
      f = as.character(cmb[a, b])
      
      if (b == 2) { 
        form <- paste(r, " ~ ", f, sep = "")
        hold_out <<- which(val_corr == f) + 4
      }
      else { 
        form <- paste(form, " + ", f, sep = "")
        hold_out[(b - 1)] <<- which(val_corr == f) + 4
      }
      
    }
    
    form <- formula(form)
    print(form)
    
    #LOG
    
    p_log <- model_fit_send(typ, kind, form, trials)
    
    p_log2 <- p_log %>%
      filter(RES != 0) %>%
      arrange(REALPCT)
    
    p_log2[, 18] <- cumsum(p_log2$CONFRES)
    
    p_w <- length(which(p_log2$CONF == 1))
    p_l <- length(which(p_log2$CONF == 0))
    p_p <- p_w / (p_w + p_l)
    p_max <- max(p_log2$V18)
    fill_mx_log[a, 8] <- p_w
    fill_mx_log[a, 9] <- p_l
    fill_mx_log[a, 10] <- p_p
    fill_mx_log[a, 11] <- p_max

    #RF
    
    r_log <- rf_fit_send(typ, kind, form, 1)
    r_log2 <- r_log %>%
      filter(RES != 0) %>%
      arrange(REALPCT)
    
    r_log2[, 20] <- cumsum(r_log2$CONFRES)

    r_w <- length(which(r_log2$CONF == 1))
    r_l <- length(which(r_log2$CONF == 0))
    r_p <- r_w / (r_w + r_l)
    r_max <- max(r_log2$V20)
    fill_mx_log[a, 12] <- r_w
    fill_mx_log[a, 13] <- r_l
    fill_mx_log[a, 14] <- r_p
    fill_mx_log[a, 15] <- r_max
    fill_mx_log[a, 16] <- as.numeric(r_log2$ERRATE[1])
    
  }
  View(p_log2)
  View(r_log2)
  fill_mx_log[, 17] <- c(1:nrow(fill_mx_log))
  colnames(fill_mx_log)[8:17] <- c("LOGW", "LOGL", "LOGP", "LOGU", "RFW", "RFL", "RFP", "RFU", "RFERR", "NO")
  fill_mx_log <<- fill_mx_log
  top_model_log <- which.max(fill_mx_log$LOGU)[1]
  top_model_rf <- which.min(fill_mx_log$RFERR)[1]
  
  model_select(top_model_log, typ, kind, "LOG")
  model_select(top_model_rf, typ, kind, "RF")
}

model_select <- function(no, typ, kind, split) {
  
  r <- "RESFAC"
  
  a <- 2
  g <- as.numeric(fill_mx_log[no, 1]) + 1
  
  for (a in a:g) {
    
    f = as.character(fill_mx_log[no, a])
    
    if (a == 2) { 
      if (typ == "ML") { form <- paste(r, " ~ LINE + ", f, sep = "") }
      else { form <- paste(r, " ~ ", f, sep = "") }
    }
    else { form <- paste(form, " + ", f, sep = "") }
    
  }
  
  form <- formula(form)
  print(c("Model Selection:", split))
  print(form)
  
  if (typ == "ML") { 
    if (split == "LOG") { pre_ml_log_form <<- form }
    else { pre_ml_rf_form <<- form }
  }
  if (typ == "ATS") {
    if (kind == "PRE") { 
      if (split == "LOG") { pre_ats_log_form <<- form }
      else { pre_ats_rf_form <<- form }
    }
    if (kind == "LIVE") {
      if (split == "LOG") { live_ats_log_form <<- form }
      else { live_ats_rf_form <<- form }
    }
  }
  if (typ == "TOTAL") {
    if (kind == "PRE") { 
      if (split == "LOG") { pre_total_log_form <<- form }
      else { pre_total_rf_form <<- form }
    }
    if (kind == "LIVE") { 
      if (split == "LOG") { live_total_log_form <<- form }
      else { live_total_rf_form <<- form }
    }
  }
}

model_scree <- function(xlo, xhi, rlo, rhi, clo, chi, typ, kind) {
  
  #LOG ONLY
  log_only <- pred_log %>%
    select(live_predict, LOGPICK, LOGPCT, LOGRES, LOGUNIT) %>%
    filter(LOGPCT >= xlo, LOGPCT <= xhi) %>%
    arrange(LOGPCT)
  
  log_only[, 6] <- cumsum(log_only$LOGUNIT)
  
  l_max <- round(log_only[which.max(log_only$V6)[1], 3], 4)
  l_min <- round(log_only[which.min(log_only$V6)[1], 3], 4)
  l_win <- length(which(log_only$LOGUNIT > 0))
  l_loss <- length(which(log_only$LOGUNIT < 0))
  l_pct <- round(l_win / (l_win + l_loss),4)
  l_tot <- l_win + l_loss
  l_unit <- round(l_win + (l_loss * -1.1), 4)
  l_carve <- round(l_tot / nrow(pred_log), 4)

  print(paste("LOG - MIN:", l_min, "MAX:", l_max, "PCT:", l_pct, "WIN:", l_win, "LOSS:", l_loss, "UNIT:", l_unit, "CARVE:", l_carve))
  
  ll <<- ggplot(log_only, aes(x = LOGPCT, y = V6)) + geom_point()
  
  #RF ONLY
  rf_only <- pred_log %>%
    select(X1, RFPICK, RFPCT, RFRES, RFUNIT) %>%
    filter(RFPCT >= rlo, RFPCT <= rhi) %>%
    arrange(RFPCT)
  
  rf_only[, 6] <- cumsum(rf_only$RFUNIT)
  
  r_max <- round(rf_only[which.max(rf_only$V6)[1], 3], 4)
  r_min <- round(rf_only[which.min(rf_only$V6)[1], 3], 4)
  r_win <- length(which(rf_only$RFUNIT > 0))
  r_loss <- length(which(rf_only$RFUNIT < 0))
  r_pct <- round(r_win / (r_win + r_loss),4)
  r_tot <- r_win + r_loss
  r_unit <- round(r_win + (r_loss * -1.1), 4)
  r_carve <- round(r_tot / nrow(pred_log), 4)
  
  print(paste("RFO - MIN:", r_min, "MAX:", r_max, "PCT:", r_pct, "WIN:", r_win, "LOSS:", r_loss, "UNIT:", r_unit, "CARVE:", r_carve))

  rr <<- ggplot(rf_only, aes(x = RFPCT, y = V6)) + geom_point()
  
  #COMBINED
  log_sd <- sd(pred_log$live_predict)
  rf_sd <- sd(pred_log$X1)
  log_mean <- mean(pred_log$live_predict)
  rf_mean <- mean(pred_log$X1)
  
  comb_only <- pred_log %>%
    select(live_predict, X1, LOGPCT, LOGRES, LOGUNIT, RFPCT, RFRES, RFUNIT) %>%
    filter(LOGRES == RFRES)
  
  comb_only[, 9] <- (comb_only$LOGPCT - log_mean) / log_sd
  comb_only[, 10] <- (comb_only$RFPCT - rf_mean) / rf_sd
  comb_only[, 11] <- ((comb_only[, 9] + comb_only[, 10]) / 2)
  
  colnames(comb_only)[9:11] <- c("LOGSCALE", "RFSCALE", "COMBSCALE")

  comb_only <- data.frame(comb_only) %>%
    arrange(COMBSCALE) %>%
    filter(COMBSCALE >= clo & COMBSCALE <= chi)
  
  comb_only[, 12] <- cumsum(comb_only$RFUNIT)
  
  c_max <- round(comb_only[which.max(comb_only$V12)[1], 11], 4)
  c_min <- round(comb_only[which.min(comb_only$V12)[1], 11], 4)
  c_win <- length(which(comb_only$RFUNIT > 0))
  c_loss <- length(which(comb_only$RFUNIT < 0))
  c_pct <- round(c_win / (c_win + c_loss),4)
  c_tot <- c_win + c_loss
  c_unit <- round(c_win + (c_loss * -1.1), 4)
  c_carve <- round(c_tot / nrow(pred_log), 4)
  
  print(paste("CMB - MIN:", c_min, "MAX:", c_max, "PCT:", c_pct, "WIN:", c_win, "LOSS:", c_loss, "UNIT:", c_unit, "CARVE:", c_carve))
  
  
  cc <<- ggplot(comb_only, aes(x = COMBSCALE, y = V12)) + geom_point()
  
  #SEMI COMBINED
  semi_only <- pred_log %>%
    select(live_predict, X1, LOGPCT, LOGRES, LOGUNIT, RFPCT, RFRES, RFUNIT) %>%
    filter(LOGRES == RFRES) %>%
    filter(LOGPCT >= xlo, LOGPCT <= xhi) %>%
    filter(RFPCT >= rlo, RFPCT <= rhi)
  
  s_win <- length(which(semi_only$RFUNIT > 0))
  s_loss <- length(which(semi_only$RFUNIT < 0))
  s_pct <- round(s_win / (s_win + s_loss),4)
  s_tot <- s_win + s_loss
  s_unit <- round(s_win + (s_loss * -1.1), 4)
  s_carve <- round(s_tot / nrow(pred_log), 4)
  
  print(paste("SEM - PCT:", s_pct, "WIN:", s_win, "LOSS:", s_loss, "UNIT:", s_unit, "CARVE:", s_carve))
  if (typ == "ATS") {
    if (kind == "PRE") { tree_ats_matrix[1, ] <<- c(xlo, xhi, rlo, rhi, clo, chi, l_pct, r_pct, c_pct, s_pct, log_mean, log_sd, rf_mean, rf_sd) }
    if (kind == "LIVE") { tree_2h_ats_matrix[1, ] <<- c(xlo, xhi, rlo, rhi, clo, chi, l_pct, r_pct, c_pct, s_pct, log_mean, log_sd, rf_mean, rf_sd) }
    }
  if (typ == "TOTAL") {
    if (kind == "PRE") { tree_total_matrix[1, ] <<- c(xlo, xhi, rlo, rhi, clo, chi, l_pct, r_pct, c_pct, s_pct, log_mean, log_sd, rf_mean, rf_sd) }
    if (kind == "LIVE") { tree_2h_total_matrix[1, ] <<- c(xlo, xhi, rlo, rhi, clo, chi, l_pct, r_pct, c_pct, s_pct, log_mean, log_sd, rf_mean, rf_sd) }
  }
}

model_scree_ml <- function(xlo, xhi, rlo, rhi, clo, chi, typ) {
  
  #LOG ONLY
  log_only <- pred_log %>%
    filter(LOGEXPVAL >= xlo, LOGEXPVAL <= xhi) %>%
    arrange(LOGEXPVAL)
  
  log_only[, 13] <- cumsum(log_only$RES)

  l_max <- round(log_only[which.max(log_only$V13)[1], 11], 4)
  l_min <- round(log_only[which.min(log_only$V13)[1], 11], 4)
  l_win <- length(which(log_only$RES > 0))
  l_loss <- length(which(log_only$RES < 0))
  l_pct <- round(l_win / (l_win + l_loss),4)
  l_tot <- l_win + l_loss
  l_unit <- round(sum(log_only$RES), 4)
  l_carve <- round(l_tot / nrow(pred_log), 4)
  l_roi <- round(l_unit / l_tot, 4)
  
  print(paste("LOG - MIN:", l_min, "MAX:", l_max, "PCT:", l_pct, "WIN:", l_win, "LOSS:", l_loss, "UNIT:", l_unit, "CARVE:", l_carve, "ROI:", l_roi))
  
  ll <<- ggplot(log_only, aes(x = LOGEXPVAL, y = V13)) + geom_point()
  
  #RF ONLY
  rf_only <- pred_log %>%
    filter(RFEXPVAL >= rlo, RFEXPVAL <= rhi) %>%
    arrange(RFEXPVAL)
  
  rf_only[, 13] <- cumsum(rf_only$RES)
  
  r_max <- round(rf_only[which.max(rf_only$V13)[1], 12], 4)
  r_min <- round(rf_only[which.min(rf_only$V13)[1], 12], 4)
  r_win <- length(which(rf_only$RES > 0))
  r_loss <- length(which(rf_only$RES < 0))
  r_pct <- round(r_win / (r_win + r_loss),4)
  r_tot <- r_win + r_loss
  r_unit <- round(sum(rf_only$RES),4)
  r_carve <- round(r_tot / nrow(pred_log), 4)
  r_roi <- round(r_unit / r_tot, 4)
  
  print(paste("RFO - MIN:", r_min, "MAX:", r_max, "PCT:", r_pct, "WIN:", r_win, "LOSS:", r_loss, "UNIT:", r_unit, "CARVE:", r_carve, "ROI:", r_roi))
  rr <<- ggplot(rf_only, aes(x = RFEXPVAL, y = V13)) + geom_point()
  
  #COMBINED
  
  log_sd <- sd(pred_log$LOGEXPVAL)
  rf_sd <- sd(pred_log$RFEXPVAL)
  log_mean <- mean(pred_log$RFEXPVAL)
  rf_mean <- mean(pred_log$LOGEXPVAL)
  
  comb_only <- pred_log %>%
    filter(LOGEXPVAL > 0 & RFEXPVAL > 0)
  
  
  comb_only[, 13] <- (comb_only$LOGEXPVAL - log_mean) / log_sd
  comb_only[, 14] <- (comb_only$RFEXPVAL - rf_mean) / rf_sd
  comb_only[, 15] <- ((comb_only[, 13] + comb_only[, 14]) / 2)
  
  colnames(comb_only)[13:15] <- c("LOGSCALE", "RFSCALE", "COMBSCALE")
  
  View(comb_only)
  
  comb_only <- data.frame(comb_only) %>%
    arrange(COMBSCALE) %>%
    filter(COMBSCALE >= clo & COMBSCALE <= chi)
  
  comb_only[, 16] <- cumsum(comb_only$RES)
  
  c_max <- round(comb_only[which.max(comb_only$V16)[1], 15], 4)
  c_min <- round(comb_only[which.min(comb_only$V16)[1], 15], 4)
  c_win <- length(which(comb_only$RES > 0))
  c_loss <- length(which(comb_only$RES < 0))
  c_pct <- round(c_win / (c_win + c_loss),4)
  c_tot <- c_win + c_loss
  c_unit <- round(sum(comb_only$RES), 4)
  c_carve <- round(c_tot / nrow(pred_log), 4)
  c_roi <- round(c_unit / c_tot, 4)
  
  print(paste("CMB - MIN:", c_min, "MAX:", c_max, "PCT:", c_pct, "WIN:", c_win, "LOSS:", c_loss, "UNIT:", c_unit, "CARVE:", c_carve, "ROI:", c_roi))
  
  
  cc <<- ggplot(comb_only, aes(x = COMBSCALE, y = V16)) + geom_point()
  
  #SEMI COMBINED
  semi_only <- pred_log  %>%
    filter(LOGEXPVAL > 0 & RFEXPVAL > 0) %>%
    filter(LOGEXPVAL >= xlo, LOGEXPVAL <= xhi) %>%
    filter(RFEXPVAL >= rlo, RFEXPVAL <= rhi)
  
  s_win <- length(which(semi_only$RES > 0))
  s_loss <- length(which(semi_only$RES < 0))
  s_pct <- round(s_win / (s_win + s_loss),4)
  s_tot <- s_win + s_loss
  s_unit <- round(sum(semi_only$RES), 4)
  s_carve <- round(s_tot / nrow(pred_log), 4)
  s_roi <- round(s_unit / s_tot, 4)
  
  print(paste("SEM - PCT:", s_pct, "WIN:", s_win, "LOSS:", s_loss, "UNIT:", s_unit, "CARVE:", s_carve, "ROI:", s_roi))
  tree_ml_matrix[1, ] <<- c(xlo, xhi, rlo, rhi, clo, chi, l_roi, r_roi, c_roi, s_roi, log_mean, log_sd, rf_mean, rf_sd)
}

model_ml_send <- function(ml_bot, ml_top, trials) {
  
  pre_key_ml_set(ml_bot, ml_top)
  
  key_tree_ml <<- key_tree_ml %>%
    select(TEAM, OPP, DAY, RES, LINE, RESFAC, MLCONV, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    mutate(PAYOUT =  1 * ((1 - MLCONV) / MLCONV))
  
  b_seed <<- sample(c(1:100), 1)
  df <- key_tree_ml
  log_form <- pre_ml_log_form
  rf_form <- pre_ml_rf_form
  form_txt <- as.character(rf_form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1

  set.seed(b_seed)
  
  a <- 1
  g <- trials
  for (a in a:g) {
    
    print(a)
    rf_train <- sample(c(1:nrow(df)), round(nrow(df) * .7, 0))
    rf_test <- -1 * rf_train
    rf_train <- df[rf_train, ]
    rf_test <- df[rf_test, ]
    
    m_log_fit <- glm(log_form, data = rf_train, family = "binomial")
    live_predict <- predict(m_log_fit, rf_test, type = "response")
    r_log_fit <- randomForest(rf_form, data = rf_train, mtry=form_ct)
    rf_predict <<- predict(r_log_fit, rf_test, type = "prob")
    
    if (a == 1) {
      pred_log <- data.frame(rf_test, live_predict, rf_predict)
    }
    else {
      pred_log2 <- data.frame(rf_test, live_predict, rf_predict)
      pred_log <- rbind(pred_log, pred_log2)
    }
  }
  pred_log <<- pred_log %>%
    select(TEAM, OPP, LINE, MLCONV, PAYOUT, RES, RESFAC, live_predict, X0, X1) %>%
    mutate(LOGEXPVAL = (live_predict * PAYOUT) + ((1 - live_predict) * -1)) %>%
    mutate(RFEXPVAL = (X1 * PAYOUT) + (X0 * -1))

}

model_scree_send <- function(typ, kind, trials) {
  
  b_seed <<- sample(c(1:100), 1)
  if (typ == "ATS" & kind == "PRE") { 
    df <- key_tree_ats 
    log_form <- pre_ats_log_form
    rf_form <- pre_ats_rf_form
  }
  if (typ == "TOTAL" & kind == "PRE") { 
    df <- key_tree_tot 
    log_form <- pre_total_log_form
    rf_form <- pre_total_rf_form
  }
  if (typ == "ATS" & kind == "LIVE") {
    df <- live_tree_ats 
    log_form <- live_ats_log_form
    rf_form <- live_ats_rf_form
  }
  if (typ == "TOTAL" & kind == "LIVE") {
    df <- live_tree_tot 
    log_form <- live_total_log_form
    rf_form <- live_total_rf_form
  }
  
  form_txt <- as.character(rf_form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  
  set.seed(b_seed)
  
  a <- 1
  g <- trials
  for (a in a:g) {
    
    print(a)
    rf_train <- sample(c(1:nrow(df)), round(nrow(df) * .7, 0))
    rf_test <- -1 * rf_train
    rf_train <- df[rf_train, ]
    rf_test <- df[rf_test, ]
    
    m_log_fit <- glm(log_form, data = rf_train, family = "binomial")
    live_predict <- predict(m_log_fit, rf_test, type = "response")
    r_log_fit <- randomForest(rf_form, data = rf_train, mtry=form_ct)
    rf_predict <<- predict(r_log_fit, rf_test, type = "prob")
    
    if (a == 1) {
      pred_log <- data.frame(rf_test, live_predict, rf_predict)
    }
    else {
      pred_log2 <- data.frame(rf_test, live_predict, rf_predict)
      pred_log <- rbind(pred_log, pred_log2)
    }
  }
  pred_log <<- pred_log %>%
    select(TEAM, OPP, RES, RESFAC, live_predict, X0, X1) %>%
    filter(RES != 0) %>%
    mutate(LOGPICK = ifelse(live_predict >= .5, 1, 0)) %>%
    mutate(LOGPCT = ifelse(live_predict >= .5, live_predict, 1 - live_predict)) %>%
    mutate(LOGRES = ifelse(LOGPICK == RESFAC, 1, 0)) %>%
    mutate(LOGUNIT = ifelse(LOGRES == 1, 1, -1.1)) %>%
    mutate(RFPICK = ifelse(X1 > X0, 1, 0)) %>%
    mutate(RFPCT = ifelse(X1 > X0, X1, X0)) %>%
    mutate(RFRES = ifelse(RFPICK == RESFAC, 1, 0)) %>%
    mutate(RFUNIT = ifelse(RFRES == 1, 1, -1.1))
}

model_fit_send <- function(typ, kind, form, trials) {
  
  if (typ == "ATS" & kind == "PRE") { 
    df <- key_tree_ats[which(rowMeans(subset(key_tree_ats[, hold_out] > 0)) == 1), ]
  }
  if (typ == "TOTAL" & kind == "PRE") { df <- key_tree_tot }
  if (typ == "ATS" & kind == "LIVE") { df <- live_tree_ats }
  if (typ == "TOTAL" & kind == "LIVE") { df <- live_tree_tot }
  
  set.seed(b_seed)
  
  a <- 1
  g <- trials
  for (a in a:g) {
    
    rf_train <- sample(c(1:nrow(df)), round(nrow(df) * .7, 0))
    rf_test <- -1 * rf_train
    rf_train <- df[rf_train, ]
    rf_test <- df[rf_test, ]
    
    m_log_fit <- glm(form, data = rf_train, family = "binomial")
    live_predict <- predict(m_log_fit, rf_test, type = "response")
    if (a == 1) {
      pred_log <- data.frame(rf_test, live_predict)
    }
    else {
      pred_log2 <- data.frame(rf_test, live_predict)
      pred_log <- rbind(pred_log, pred_log2)
    }
  }
  pred_log <- pred_log %>%
    mutate(CLASS = ifelse(live_predict > .5, 1, 0)) %>%
    mutate(CONF = ifelse(CLASS == RESFAC, 1, 0)) %>%
    mutate(CONFRES = ifelse(CONF == 1, 1, -1.1)) %>%
    mutate(REALPCT = ifelse(live_predict > .5, live_predict, 1 - live_predict))
  
  return(pred_log)
}

rf_fit_send <- function(typ, kind, form, trials) {
  
  if (typ == "ATS" & kind == "PRE") { 
    df <- key_tree_ats[which(rowMeans(subset(key_tree_ats[, hold_out] > 0)) == 1), ]
  }
  if (typ == "TOTAL" & kind == "PRE") { df <- key_tree_tot }
  if (typ == "ATS" & kind == "LIVE") { df <- live_tree_ats }
  if (typ == "TOTAL" & kind == "LIVE") { df <- live_tree_tot }
  
  set.seed(b_seed)
  
  form_txt <- as.character(form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  
  a <- 1
  g <- trials
  for (a in a:g) {
    
    rf_train <- sample(c(1:nrow(df)), round(nrow(df) * 1, 0))
    rf_test <- sample(c(1:nrow(df)), round(nrow(df) * .1, 0))
    rf_train <- df[rf_train, ]
    rf_test <- df[rf_test, ]
    
    m_log_fit <- randomForest(form, data = rf_train, mtry = form_ct, ntree=600)
    rf_predict <<- predict(m_log_fit, rf_test, type = "prob")
    if (a == 1) {
      pred_log <- data.frame(rf_test, rf_predict)
    }
    else {
      pred_log2 <- data.frame(rf_test, rf_predict)
      pred_log <- rbind(pred_log, pred_log2)
    }
  }
  err_rate <- m_log_fit$err.rate[m_log_fit$ntree,1]
  pred_log <- pred_log %>%
    mutate(CLASS = ifelse(X1 > X0, 1, 0)) %>%
    mutate(CONF = ifelse(CLASS == RESFAC, 1, 0)) %>%
    mutate(CONFRES = ifelse(CONF == 1, 1, -1.1)) %>%
    mutate(REALPCT = ifelse(X1 > X0, X1, X0)) %>%
    mutate(ERRATE = err_rate)
  return(pred_log)
}



simple_train_test <- function(typ, kind, form, trials) {
  
  if (typ == "ATS" & kind == "PRE") { 
    df <- key_tree_ats[which(rowMeans(subset(key_tree_ats[, hold_out] > 0)) == 1), ]
  }
  
  form_txt <- as.character(form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  df$RESFAC <- as.factor(df$RESFAC)
  
  m_sum_right <- 0
  m_sum_wrong <- 0
  a <- 1
  for (a in a:trials) {
  print(a)
    X_train_sample <- sample(c(1:nrow(df)), round(nrow(df) * .7, 0))
    X_test_sample <- -1 * X_train_sample
    X_train <- df[X_train_sample, ]
    X_test <- df[X_test_sample, ]
    Y_test <- df$RESFAC[X_test_sample]
    #X_fit <- randomForest(form, data=X_train, mtry=form_ct)
    X_fit <- glm(form, data = X_train, family = 'binomial')
    Y_pred <- predict(X_fit, X_test, type = "response")
    Y_table <- data.frame(Y_test, Y_pred)
    if (a == 1) { mass_table <- Y_table }
    else { mass_table <- rbind(mass_table, Y_table) }
  }
  mass_table <- mass_table %>%
    mutate(PICK = ifelse(Y_pred >= .5, 1, 0)) %>%
    mutate(ACTRES = ifelse(PICK == Y_test, 1, -1.1)) %>%
    filter(PICK == 1) %>%
    arrange(desc(Y_pred))
  
  mass_table[, 5] <- cumsum(mass_table$ACTRES)
  
  print(table(mass_table[, c(1, 3)]))
  print(max(mass_table$V5))
  View(mass_table)
  
  mass_table <- mass_table %>%
    arrange(Y_pred)
  mass_table[, 5] <- cumsum(mass_table$ACTRES)
  ggplot(mass_table, aes(x = Y_pred, y = V5)) + geom_point()
  #print(Y_table)
  #print(m_sum_right / (m_sum_right + m_sum_wrong))
}

train_control <- trainControl(method="repeatedcv", number=5, repeats=2)
model <- train(pre_ats_rf_form, data=key_tree_ats, trControl=train_control, method="rf")

rf_thunderdome <- function(typ, kind, x) {
  
  b_seed <<- sample(c(1:100), 1)
  if (typ == "ATS" & kind == "PRE") { rfdata <- key_tree_ats }
  if (typ == "TOTAL" & kind == "PRE") { rfdata <- key_tree_tot }
  if (typ == "ATS" & kind == "LIVE") { rfdata <- live_tree_ats }
  if (typ == "TOTAL" & kind == "LIVE") { rfdata <- live_tree_tot }
  
  r = "RESFAC"
  fill_mx_log <<- fill_mx_log %>%
    arrange(RFERR)
  
  lo <- 99
  
  a <- 1
  for (a in a:x) {
    
    b = 2
    h = as.numeric(fill_mx_log[a, 1]) + 1

    for (b in b:h) {
      
      f = as.character(fill_mx_log[a, b])
      
      if (b == 2) { form <- paste(r, " ~ ", f, sep = "") }
      else { form <- paste(form, " + ", f, sep = "") }
      
    }
    
    form <- formula(form)
    form_txt <- as.character(form)[3]
    form_ct <- str_count(form_txt, "\\+") + 1
    print(c(form_txt, form_ct))
    set.seed(b_seed)
    rf <- randomForest(form, data = rfdata, mtry = form_ct, ntree=4000)
    print(rf$err.rate[1000, 1])
    err <- rf$err.rate[1000, 1]
    if (err < lo) {
      lo <- as.numeric(err)
      lnum <- a
      l_form <- form
    }
    plot(rf$err.rate[,1])

  }
  print(lnum)
  print(l_form)
  if (typ == "ATS" & kind == "PRE") { pre_ats_rf_form <<- l_form }
  if (typ == "TOTAL" & kind == "PRE") { pre_total_rf_form <<- l_form }
  if (typ == "ATS" & kind == "LIVE") { live_ats_rf_form <<- l_form }
  if (typ == "TOTAL" & kind == "LIVE") { live_total_rf_form <<- l_form }
}

ml_thunderdome <- function(x, trials) {
  
  b_seed <<- sample(c(1:100), 1)
  r = "RESFAC"
  fill_mx_log <<- fill_mx_log %>%
    arrange(RFERR)
  
  lo <- -234234
  
  a <- 1
  for (a in a:x) {
    
    b = 2
    h = as.numeric(fill_mx_log[a, 1]) + 1
    
    for (b in b:h) {
      
      f = as.character(fill_mx_log[a, b])
      
      if (b == 2) { form <- paste(r, " ~ LINE + ", f, sep = "") }
      else { form <- paste(form, " + ", f, sep = "") }
      
    }
    
    form <- formula(form)
    form_txt <- as.character(form)[3]
    form_ct <- str_count(form_txt, "\\+") + 1
    print(c(form_txt, form_ct))
    
    zz <- rf_ml_testtrain(form, trials)
    print(zz)
    
    if (zz > lo) {
      lo <- as.numeric(zz)
      lnum <- a
      l_form <- form
    }
  }
  print(lnum)
  print(l_form)
  pre_ml_rf_form <<- l_form
}

rf_ml_testtrain <- function(form, trials) {
  
  
  form_txt <- as.character(form)[3]
  form_ct <- str_count(form_txt, "\\+") + 1
  
  df <- key_tree_ml
  
  set.seed(b_seed)
  
  b <- 1
  h <- trials
  for (b in b:h) {
    
    
    rf_train <- sample(c(1:nrow(df)), round(nrow(df) * .7, 0))
    rf_test <- -1 * rf_train
    rf_train <- df[rf_train, ]
    rf_test <- df[rf_test, ]
    
    m_log_fit <- randomForest(form, data = rf_train, mtry = form_ct, ntree=600)
    rf_predict <<- predict(m_log_fit, rf_test, type = "prob")
    if (b == 1) {
      pred_log <- data.frame(rf_test, rf_predict)
    }
    else {
      pred_log2 <- data.frame(rf_test, rf_predict)
      pred_log <- rbind(pred_log, pred_log2)
    }
  }
  
  p_log <- pred_log %>%
    mutate(RFVSML = X1 - MLCONV) %>%
    mutate(RFVAL2 = (X1 * PAYOUT) + ((1 - X1) * - 1)) %>%
    filter(RFVAL2 > 0) %>%
    arrange(RFVAL2)
  
  p_log[, 19] <- cumsum(p_log$RES)
  plot(p_log[, 19])
  return(max(p_log$V19))
  
}

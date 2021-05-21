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

results_do_all <- function(m, d, y) {
  
  
  box_results_5(m, d, y)
  br_breakdown_5_set(1)
  br_breakdown_5_do(1)
  #Import from betvaluesub5
  bet_value_result5(2)
  #
  bx_keyer_new(0)
  yesterdays_breakdown(m, d, y)
  my_plays_analyze(1)

}

dfs_comb_write <- function(m, d, y) {
  
  dfs_actual_whole_frame <<- dfs_actual_comb %>%
    inner_join(dfs_whole_frame, by = c("PLAYER_ID" = "ID"))
  
  colnames(dfs_actual_whole_frame)[2] <<- "DFSAct"
  colnames(dfs_actual_whole_frame)[5] <<- "DFSProj"
  dfs_actual_whole_frame <<- dfs_actual_whole_frame[, c(1, 3:4, 2, 5:20)]
  dfs_actual_whole_frame[, 21] <<- m
  dfs_actual_whole_frame[, 22] <<- d
  dfs_actual_whole_frame[, 23] <<- y
  colnames(dfs_actual_whole_frame)[21] <<- "Month"
  colnames(dfs_actual_whole_frame)[22] <<- "Day"
  colnames(dfs_actual_whole_frame)[23] <<- "Year"
  
  write_csv(dfs_actual_whole_frame, "dfs_whole_tmp.csv")
  master_dfsw <<- read_csv("dfs_whole.csv")
  
  master_dfsw_chop <<- which(master_dfsw$Month == m & master_dfsw$Day == d & master_dfsw$Year == y)

  if (length(master_dfsw_chop) > 0) {
    master_dfsw_chop <<- -1 * master_dfsw_chop
    master_dfsw_chopped <<- master_dfsw[master_dfsw_chop, ]
    master_dfsw_chopped <<- master_dfsw[master_dfsw_chop, ]
  } else {
    master_dfsw_chopped <<- master_dfsw
  }
  
  master_dfsw_bind <<- rbind(master_dfsw_chopped, dfs_actual_whole_frame)
  
  write_csv(master_dfsw_bind, "dfs_whole.csv")
  
  
}

#---------------------
#BOX RESULTS 5
#---------------------

box_results_5 <- function(m, d, y) {
  
  results_frame <<- read_excel("CBB Odds.xlsm", sheet = "Score Matcher")
  formatted_wpt5_load <<- read_csv("formatted_wpt5_pre.csv")
  formatted_wpt5 <<- formatted_wpt5_load
  
  if (nchar(m) == 1) { mmm <- paste(0, m, sep = "") }
  else { mmm <- m }
  if (nchar(d) == 1) { ddd <- paste(0, d, sep = "") }
  else { ddd <- d }
  print(paste(m, d, y, sep = "-"))
  bx_file <<- paste("box_scores_", mmm, "-", ddd, "-", y, ".csv", sep = "")
  bx_file <<- paste("boxscores\\", bx_file, sep = "")
  bx_csv <<- read_csv(bx_file)
  
  val_pos <<- nat_avgs$oe_do
  lg_drb <<- nat_avgs$drb_do
  lg_ftp <<- nat_avgs$ftpct_do
  lg_orb <<- nat_avgs$orb_do
  top_mvp_score <<- -9999
  top_mvp_no <<- -9999
  
  
  strk_vector <<- NULL
  
  a <- 1
  g <- nrow(formatted_wpt5)
  
  bres_mx <<- matrix(0, nrow = g, ncol = 18)
  
  for (a in a:g) {
    
    a_team <<- as.character(formatted_wpt5$Away[a])
    h_team <<- as.character(formatted_wpt5$Home[a])
    
    a_id <<- as.numeric(formatted_wpt5$AID[a])
    h_id <<- as.numeric(formatted_wpt5$HID[a])
    
    a_box_frame <<- bx_csv %>%
      filter(TEAM_ID == a_id & OPP_ID == h_id)
    
    h_box_frame <<- bx_csv %>%
      filter(TEAM_ID == h_id & OPP_ID == a_id)
    
    a_pts <<- sum(a_box_frame$PTS)
    a_orb <<- sum(a_box_frame$ORB)
    a_tov <<- sum(a_box_frame$TOV)
    a_fga <<- sum(a_box_frame$FGA)
    a_fta <<- sum(a_box_frame$FTA)
    
    h_pts <<- sum(h_box_frame$PTS)
    h_orb <<- sum(h_box_frame$ORB)
    h_tov <<- sum(h_box_frame$TOV)
    h_fga <<- sum(h_box_frame$FGA)
    h_fta <<- sum(h_box_frame$FTA)
    
    lu_ct <<- 2
    
    if (nrow(a_box_frame) <= 0 & nrow(h_box_frame) <= 0) { lu_ct <<- -99 }
    
    if (a_pts == 0) {
      
      lu_apts <<- which(results_frame$AID == a_id)
      if (length(lu_apts) == 0) { lu_ct <<- lu_ct - 1 }
      else { a_pts <<- results_frame[lu_apts, 10] }
      
    }
    
    if (h_pts == 0) {
      
      lu_hpts <<- which(results_frame$HID == h_id)
      if (length(lu_hpts) == 0) { lu_ct <<- lu_ct - 1 }
      else { h_pts <<- results_frame[lu_hpts, 11] }
      
    }
    
    if (lu_ct < 2) {
      
      if (is.null(strk_vector) == TRUE) { strk_vector <<- a }
      else { strk_vector <<- c(strk_vector, a) }
      
    }
    
    
    bx_pos_a <<- (a_fga - a_orb + a_tov + (a_fta * .475)) 
    bx_pos_h <<- (h_fga - h_orb + h_tov + (h_fta * .475)) 
    bx_pos_use <<- (bx_pos_a + bx_pos_h) / 2

    #MVP Select
    
    
    bx_amvp_vector <<- ((a_box_frame[, 10] * 1) + 
                          ((a_box_frame[, 3] - a_box_frame[, 2]) * val_pos * lg_drb * -1) +
                          ((a_box_frame[, 9] - a_box_frame[, 8]) * lg_ftp * -1) +
                          (a_box_frame[, 11] * val_pos) + 
                          (a_box_frame[, 12] * val_pos) +
                          (a_box_frame[, 14] * .66 * val_pos) +
                          (a_box_frame[, 16] * val_pos) +
                          (a_box_frame[, 17] * lg_orb) +
                          (a_box_frame[, 15] * val_pos * -1) +
                          (a_box_frame[, 18] * -1 * .95)) / bx_pos_use
    
    bx_hmvp_vector <<- ((h_box_frame[, 10] * 1) + 
                          ((h_box_frame[, 3] - h_box_frame[, 2]) * val_pos * lg_drb * -1) +
                          ((h_box_frame[, 9] - h_box_frame[, 8]) * lg_ftp * -1) +
                          (h_box_frame[, 11] * val_pos) + 
                          (h_box_frame[, 12] * val_pos) +
                          (h_box_frame[, 14] * .66 * val_pos) +
                          (h_box_frame[, 16] * val_pos) +
                          (h_box_frame[, 17] * lg_orb) +
                          (h_box_frame[, 15] * val_pos * -1) +
                          (h_box_frame[, 18] * -1 * .95)) / bx_pos_use
    
    bx_amvp_comb <<- cbind(a_box_frame, bx_amvp_vector)
    bx_hmvp_comb <<- cbind(h_box_frame, bx_hmvp_vector)
    
    if (a == 1) { bx_mvp3 <<- rbind(bx_amvp_comb, bx_hmvp_comb) }
    else { bx_mvp3 <<- rbind(bx_mvp3, rbind(bx_amvp_comb, bx_hmvp_comb)) }
    
    bx_pos_use2 <<- as.numeric(format(round(bx_pos_use, digits = 1), nsmall = 1))
    
    formatted_wpt5[a, 57] <<- a_pts
    formatted_wpt5[a, 58] <<- h_pts
    formatted_wpt5[a, 59] <<- bx_pos_use2
    formatted_wpt5[a, 60] <<- (a_pts + h_pts)
    
    colnames(bx_amvp_comb)[29] <<- "pSCORE"
    colnames(bx_hmvp_comb)[29] <<- "pSCORE"
    
    bx_tmvp_comb <<- rbind(bx_amvp_comb, bx_hmvp_comb)
    
    bx_tmvp_comb <<- bx_tmvp_comb %>%
      arrange(desc(pSCORE))
    
    pscore_ <<- as.numeric(bx_tmvp_comb[1, 29])
    pts_ <<- as.numeric(bx_tmvp_comb[1, 10])
    reb_ <<- as.numeric(bx_tmvp_comb[1, 13])
    ast_ <<- as.numeric(bx_tmvp_comb[1, 14])
    stl_ <<- as.numeric(bx_tmvp_comb[1, 16])
    blk_ <<- as.numeric(bx_tmvp_comb[1, 17])
    mvpname_ <<- as.character(bx_tmvp_comb[1, 19])
    mvpteam_ <<- as.character(bx_tmvp_comb[1, 21])
    
    abb_mvp <<- as.character(master_names_cbb[which(master_names_cbb$NCAA == mvpteam_), 7])
    mvp_split <<- strsplit(mvpname_, ",")
    mvp_firstname <<- trim.all(mvp_split[[1]][2])
    mvp_lastname <<- mvp_split[[1]][1]
    game_mvp_score_use <<- as.numeric(format(round((pscore_ * 100), digits = 1), nsmall = 1))
    mvp_name_use <<- paste(mvp_firstname, mvp_lastname, sep = " ")
    
    if (is.na(game_mvp_score_use) == TRUE) { mvp_listout <<- "Unknown" }
    
    else { 
      
      mvp_listout <<- paste(abb_mvp, " - ", mvp_name_use, ", ", game_mvp_score_use, " pScore - ", pts_, " pts, ", 
                            reb_, " reb, ",
                            ast_, " ast, ", stl_, " stl, ", blk_, " blk", sep = "")
      
    }
    
    #BET RESULTS

    
    bres_spread <<- as.numeric(formatted_wpt5[a, 4])
    bres_aml <<- as.numeric(formatted_wpt5[a, 5])
    bres_hml <<- as.numeric(formatted_wpt5[a, 6])
    bres_total <<- as.numeric(formatted_wpt5[a, 7])
    
    aats_pts <<- a_pts
    hats_pts <<- bres_spread + h_pts
    tt_pts <<- a_pts + h_pts
    
    if (aats_pts > hats_pts) { bres_ats_winner <<- "A" }
    else if (aats_pts < hats_pts) { bres_ats_winner <<- "H" }
    else { bres_ats_winner <<- "P" }
    
    if (a_pts > h_pts) { bres_ml_winner <<- "A" }
    else { bres_ml_winner <<- "H" }
    
    if (tt_pts > bres_total) { bres_tot_winner <<- "O" }
    else if (tt_pts < bres_total) { bres_tot_winner <<- "U" }
    else { bres_tot_winner <<- "P" }
    
    #ATS BET RESULTS
    
    b <- 1
    h <- 6
    bres_atsvec <<- c(39, 42, 45, 48, 51, 54)
    bres_atsslot <<- c(1, 4, 7, 10, 13, 16)
    
    for (b in b:h) {
      
      bres_atsvec_take <<- as.numeric(bres_atsvec[b])
      bres_atsvec_slot <<- as.numeric(bres_atsslot[b])
      
      bres_play_check <<- as.character(formatted_wpt5[a, bres_atsvec_take])
      
      if (bres_ats_winner == "P" | bres_play_check == "-") { bres_result <<- 0 }
      else {
        if (bres_play_check == bres_ats_winner) { bres_result <<- 1 }
        else { bres_result <<- -1.1 }
      }
      bres_mx[a, bres_atsvec_slot] <<- bres_result
    }
    
    #ML BET RESULTS
    
    b <- 1
    h <- 6
    bres_atsvec <<- c(40, 43, 46, 49, 52, 55)
    bres_atsslot <<- c(2, 5, 8, 11, 14, 17)
    
    for (b in b:h) {
      
      bres_atsvec_take <<- as.numeric(bres_atsvec[b])
      bres_atsvec_slot <<- as.numeric(bres_atsslot[b])
      
      bres_play_check <<- as.character(formatted_wpt5[a, bres_atsvec_take])
      
      if (bres_ml_winner == "P" | bres_play_check == "-") { bres_result <<- 0 }
      else {
        if (bres_play_check == bres_ml_winner) { 
          if (bres_play_check == "A") {
            bres_ml_use <<- as.numeric(formatted_wpt5[a, 5])
            if (bres_ml_use > 0) { bres_result <<- bres_ml_use / 100 }
            else { bres_result <<- 100 / abs(bres_ml_use) }
          }
          else {
            bres_ml_use <<- as.numeric(formatted_wpt5[a, 6])
            if (bres_ml_use > 0) { bres_result <<- bres_ml_use / 100 }
            else { bres_result <<- 100 / abs(bres_ml_use) }
          }
        }
        else { bres_result <<- -1 }
      }
      bres_mx[a, bres_atsvec_slot] <<- bres_result
    }
    
    #TOTAL BET RESULTS
    
    b <- 1
    h <- 6
    bres_atsvec <<- c(41, 44, 47, 50, 53, 56)
    bres_atsslot <<- c(3, 6, 9, 12, 15, 18)
    
    for (b in b:h) {
      
      bres_atsvec_take <<- as.numeric(bres_atsvec[b])
      bres_atsvec_slot <<- as.numeric(bres_atsslot[b])
      
      bres_play_check <<- as.character(formatted_wpt5[a, bres_atsvec_take])
      
      if (bres_tot_winner == "P" | bres_play_check == "-") { bres_result <<- 0 }
      else {
        if (bres_play_check == bres_tot_winner) { bres_result <<- 1 }
        else { bres_result <<- -1.1 }
      }
      bres_mx[a, bres_atsvec_slot] <<- bres_result
    }
    
    
    formatted_wpt5[a, 61] <<- mvp_listout
    
  }
  
  #formatted_wpt[top_mvp_no, 46] <<- 1
  
  
  
  colnames(bx_mvp3)[29] <<- "pScore"
  bx_mvp3 <<- bx_mvp3 %>%
    arrange(desc(pScore))
  
  pscore_1 <<- as.numeric(bx_mvp3[1, 29])
  pts_1 <<- as.numeric(bx_mvp3[1, 10])
  reb_1 <<- as.numeric(bx_mvp3[1, 13])
  ast_1 <<- as.numeric(bx_mvp3[1, 14])
  stl_1 <<- as.numeric(bx_mvp3[1, 16])
  blk_1 <<- as.numeric(bx_mvp3[1, 17])
  mvpname_1 <<- as.character(bx_mvp3[1, 19])
  mvpteam_1 <<- as.character(bx_mvp3[1, 21])
  
  pscore_2 <<- as.numeric(bx_mvp3[2, 29])
  pts_2 <<- as.numeric(bx_mvp3[2, 10])
  reb_2 <<- as.numeric(bx_mvp3[2, 13])
  ast_2 <<- as.numeric(bx_mvp3[2, 14])
  stl_2 <<- as.numeric(bx_mvp3[2, 16])
  blk_2 <<- as.numeric(bx_mvp3[2, 17])
  mvpname_2 <<- as.character(bx_mvp3[2, 19])
  mvpteam_2 <<- as.character(bx_mvp3[2, 21])
  
  
  pscore_3 <<- as.numeric(bx_mvp3[3, 29])
  pts_3 <<- as.numeric(bx_mvp3[3, 10])
  reb_3 <<- as.numeric(bx_mvp3[3, 13])
  ast_3 <<- as.numeric(bx_mvp3[3, 14])
  stl_3 <<- as.numeric(bx_mvp3[3, 16])
  blk_3 <<- as.numeric(bx_mvp3[3, 17])
  mvpname_3 <<- as.character(bx_mvp3[3, 19])
  mvpteam_3 <<- as.character(bx_mvp3[3, 21])
  
  abb_mvp1 <<- as.character(master_names_cbb[which(master_names_cbb$NCAA == mvpteam_1), 1])
  mvp_split <<- strsplit(mvpname_1, ",")
  mvp_firstname <<- trim.all(mvp_split[[1]][2])
  mvp_lastname <<- mvp_split[[1]][1]
  game_mvp_score_use1 <<- as.numeric(format(round((pscore_1 * 100), digits = 1), nsmall = 1))
  mvp_name_use1 <<- paste(mvp_firstname, mvp_lastname, sep = " ")
  
  mvp_listout1 <<- paste(abb_mvp1, " - ", mvp_name_use1, ", ", game_mvp_score_use1, " pScore - ", pts_1, " pts, ", 
                         reb_1, " reb, ",
                         ast_1, " ast, ", stl_1, " stl, ", blk_1, " blk", sep = "")
  
  abb_mvp2 <<- as.character(master_names_cbb[which(master_names_cbb$NCAA == mvpteam_2), 1])
  mvp_split <<- strsplit(mvpname_2, ",")
  mvp_firstname <<- trim.all(mvp_split[[1]][2])
  mvp_lastname <<- mvp_split[[1]][1]
  game_mvp_score_use2 <<- as.numeric(format(round((pscore_2 * 100), digits = 1), nsmall = 1))
  mvp_name_use2 <<- paste(mvp_firstname, mvp_lastname, sep = " ")
  
  mvp_listout2 <<- paste(abb_mvp2, " - ", mvp_name_use2, ", ", game_mvp_score_use2, " pScore - ", pts_2, " pts, ", 
                         reb_2, " reb, ",
                         ast_2, " ast, ", stl_2, " stl, ", blk_2, " blk", sep = "")
  
  
  abb_mvp3 <<- as.character(master_names_cbb[which(master_names_cbb$NCAA == mvpteam_3), 1])
  mvp_split <<- strsplit(mvpname_3, ",")
  mvp_firstname <<- trim.all(mvp_split[[1]][2])
  mvp_lastname <<- mvp_split[[1]][1]
  game_mvp_score_use3 <<- as.numeric(format(round((pscore_3 * 100), digits = 1), nsmall = 1))
  mvp_name_use3 <<- paste(mvp_firstname, mvp_lastname, sep = " ")
  
  mvp_listout3 <<- paste(abb_mvp3, " - ", mvp_name_use3, ", ", game_mvp_score_use3, " pScore - ", pts_3, " pts, ", 
                         reb_3, " reb, ",
                         ast_3, " ast, ", stl_3, " stl, ", blk_3, " blk", sep = "")

  print("HI")
  
  formatted_wpt5[, 62] <<- "NA"
  formatted_wpt5[1, 62] <<- mvp_listout1 
  formatted_wpt5[2, 62] <<- mvp_listout2 
  if (nrow(formatted_wpt5) > 2) { formatted_wpt5[3, 62] <<- mvp_listout3 }
  colnames(formatted_wpt5)[62] <<- "TopMVP"
  
  formatted_wpt5 <<- cbind(formatted_wpt5, bres_mx)
  
  strk_vector <<- strk_vector * -1
  if (length(strk_vector) > 0) { formatted_wpt5 <<- formatted_wpt5[strk_vector, ] }
  
  colnames(formatted_wpt5)[57:61] <<- c("ACTASCORE", "ACTHSCORE", "ACTPOS", "ACTTOTAL", "ACTMVP")
  colnames(formatted_wpt5)[63:80] <<- c("IATSRES", "IMLRES", "ITOTRES", "TATSRES", "TMLRES", "TTOTRES", "BATSRES", "BMLRES", "BTOTRES", 
                                        "AATSRES", "AMLRES", "ATOTRES", "SATSRES", "SMLRES", "STOTRES", "CATSRES", "CMLRES", "CTOTRES")
  
  write_csv(formatted_wpt5, "formatted_wpt_tmp5.csv")
  master_fwpt <<- read_csv("formatted_wpt5.csv")
  
  master_fwpt_chop <<- which(master_fwpt$Month == m & master_fwpt$Day == d & master_fwpt$Year == y)
  master_fwpt_chop <<- -1 * master_fwpt_chop
  master_fwpt_chopped <<- master_fwpt[master_fwpt_chop, ]
  master_fwpt_bind <<- rbind(master_fwpt_chopped, formatted_wpt5)
  
  write_csv(master_fwpt_bind, "formatted_wpt5.csv")
}

br_breakdown_5_set <- function(xxxx) {
  
  bx_break <<- read_csv("formatted_wpt5.csv")
  bx_break <<- bx_break[, c(-13, -38:-62)]
  bx_dates <<- bx_break[ , 1:3] %>%
    group_by(Month, Day, Year) %>%
    summarize(ct = n())
  
  bx_a_mlconv <<- bx_break[, 5]
  bx_h_mlconv <<- bx_break[, 6]
  
  a <- 1
  g <- nrow(bx_a_mlconv)
  
  for (a in a:g) {
    
    aml_tmp <- as.numeric(bx_a_mlconv[a, 1])
    hml_tmp <- as.numeric(bx_h_mlconv[a, 1])
    aml_conv <- moneyline_conversion(aml_tmp)
    hml_conv <- moneyline_conversion(hml_tmp)
    bx_a_mlconv[a, 1] <<- aml_conv * 100
    bx_h_mlconv[a, 1] <<- hml_conv * 100
    
    
  }
  
  bx_a_spread <<- bx_break[, 4] * -1
  bx_h_spread <<- bx_break[, 4]
  bx_t_spread <<- bx_break[, 7]
  
  #Away ATS Value
  
  bx_val_vec <<- c(13, 15, 17, 19, 21, 23)
  b <- 1
  h <- length(bx_val_vec)
  
  for (b in b:h) {
    
    bx_val_use <<- bx_val_vec[b]
    bx_val_opp <<- bx_val_use + 1
    bx_vc_col <<- bx_break[, bx_val_use]
    bx_vc_aats <<- bx_vc_col + bx_a_spread
    
    bx_oc_col <<- bx_break[, bx_val_opp]
    
    bx_val_frame <<- bx_vc_aats - bx_oc_col
    
    bx_break[, ncol(bx_break) + 1] <<- bx_val_frame
    
  }
  
  #Home ATS Value
  
  bx_val_vec <<- c(14, 16, 18, 20, 22, 24)
  b <- 1
  h <- length(bx_val_vec)
  
  for (b in b:h) {
    
    bx_val_use <<- bx_val_vec[b]
    bx_val_opp <<- bx_val_use - 1
    bx_vc_col <<- bx_break[, bx_val_use]
    bx_vc_aats <<- bx_vc_col + bx_h_spread
    
    bx_oc_col <<- bx_break[, bx_val_opp]
    
    bx_val_frame <<- bx_vc_aats - bx_oc_col
    
    bx_break[, ncol(bx_break) + 1] <<- bx_val_frame
    
  }
  
  #Away ML Value
  
  bx_val_vec <<- c(25, 27, 29, 31, 33, 35)
  b <- 1
  h <- length(bx_val_vec)
  
  for (b in b:h) {
    
    bx_val_use <<- bx_val_vec[b]
    bx_vc_col <<- bx_break[, bx_val_use]
    bx_val_frame <<- bx_vc_col - bx_a_mlconv
    
    bx_break[, ncol(bx_break) + 1] <<- bx_val_frame
    
  }
  
  #Home ML Value
  
  bx_val_vec <<- c(26, 28, 30, 32, 34, 36)
  b <- 1
  h <- length(bx_val_vec)
  
  for (b in b:h) {
    
    bx_val_use <<- bx_val_vec[b]
    bx_vc_col <<- bx_break[, bx_val_use]
    bx_val_frame <<- bx_vc_col - bx_h_mlconv
    
    bx_break[, ncol(bx_break) + 1] <<- bx_val_frame
    
  }
  
  #Over Value
  
  bx_val_vec <<- c(13, 15, 17, 19, 21, 23)
  b <- 1
  h <- length(bx_val_vec)
  
  for (b in b:h) {
    
    bx_val_use <<- bx_val_vec[b]
    bx_val_opp <<- bx_val_use + 1
    
    bx_as_col <<- bx_break[, bx_val_use]
    bx_hs_col <<- bx_break[, bx_val_opp]
    
    bx_ts_col <<- bx_as_col + bx_hs_col
    bx_tv_col <<- bx_ts_col - bx_t_spread
    
    bx_break[, ncol(bx_break) + 1] <<- bx_tv_col
    
  }
  
  #Under Value
  
  bx_val_vec <<- c(13, 15, 17, 19, 21, 23)
  b <- 1
  h <- length(bx_val_vec)
  
  for (b in b:h) {
    
    bx_val_use <<- bx_val_vec[b]
    bx_val_opp <<- bx_val_use + 1
    
    bx_as_col <<- bx_break[, bx_val_use]
    bx_hs_col <<- bx_break[, bx_val_opp]
    
    bx_ts_col <<- bx_as_col + bx_hs_col
    bx_tv_col <<- bx_t_spread - bx_ts_col
    
    bx_break[, ncol(bx_break) + 1] <<- bx_tv_col
    
  }
  
  colnames(bx_break)[55:90] <<- c("IATSVAL", "TATSVAL", "BATSVAL", "AATSVAL", "SATSVAL", "CATSVAL",
                                  "IHTSVAL", "THTSVAL", "BHTSVAL", "AHTSVAL", "SHTSVAL", "CHTSVAL",
                                  "IAMLVAL", "TAMLVAL", "BAMLVAL", "AAMLVAL", "SAMLVAL", "CAMLVAL",
                                  "IHMLVAL", "THMLVAL", "BHMLVAL", "AHMLVAL", "SHMLVAL", "CHMLVAL",
                                  "IOVRVAL", "TOVRVAL", "BOVRVAL", "AOVRVAL", "SOVRVAL", "COVRVAL",
                                  "IUNDVAL", "TUNDVAL", "BUNDVAL", "AUNDVAL", "SUNDVAL", "CUNDVAL")
  
  if (nrow(bx_dates) <= 14) { 
    
    bx_break[, 91] <<- 1
    
  } 
  else {
    
    bx_break[, 91] <<- 0
    bx_date_start <- nrow(bx_dates) - 14
    bx_date_end <- nrow(bx_dates)
    bx_dates_14 <<- bx_dates[bx_date_start:bx_date_end, ]
    
    b <- 1
    h <- nrow(bx_dates_14)
    
    for (b in b:h) {
      
      mm_ <- as.numeric(bx_dates_14[b, 1])
      dd_ <- as.numeric(bx_dates_14[b, 2])
      yy_ <- as.numeric(bx_dates_14[b, 3])
      ww_ <- which(bx_break$Month == mm_ & bx_break$Day == dd_ & bx_break$Year == yy_)
      
      bx_break[ww_, 91] <<- 1
      
    }
  }
  colnames(bx_break)[91] <<- "LAST14"
  
  w_inf_i <<- which(bx_break$IMLRES == "Inf")
  if (length(w_inf_i) > 0) { bx_break$IMLRES[w_inf_i] <<- .01 }
  w_inf_i <<- which(bx_break$TMLRES == "Inf")
  if (length(w_inf_i) > 0) { bx_break$TMLRES[w_inf_i] <<- .01 }
  w_inf_i <<- which(bx_break$BMLRES == "Inf")
  if (length(w_inf_i) > 0) { bx_break$BMLRES[w_inf_i] <<- .01 }
  w_inf_i <<- which(bx_break$AMLRES == "Inf")
  if (length(w_inf_i) > 0) { bx_break$AMLRES[w_inf_i] <<- .01 }
  w_inf_i <<- which(bx_break$SMLRES == "Inf")
  if (length(w_inf_i) > 0) { bx_break$SMLRES[w_inf_i] <<- .01 }
  w_inf_i <<- which(bx_break$CMLRES == "Inf")
  if (length(w_inf_i) > 0) { bx_break$CMLRES[w_inf_i] <<- .01 }
}


br_breakdown_5_do <- function(xxx) {
  
  bx_break_nxt <<- bx_break[, c(-4:-36)]
  bx_break_res <<- bx_break_nxt[, c(1:21)]
  
  bx_break_group <<- bx_break_res %>%
    mutate(IATSW = ifelse(IATSRES > 0, 1, 0), IATSL = ifelse(IATSRES < 0, 1, 0), IMLW = ifelse(IMLRES > 0, 1, 0),
           IMLL = ifelse(IMLRES < 0, 1, 0), ITOTW = ifelse(ITOTRES > 0, 1, 0), ITOTL = ifelse(ITOTRES < 0, 1, 0),
           TATSW = ifelse(TATSRES > 0, 1, 0), TATSL = ifelse(TATSRES < 0, 1, 0), TMLW = ifelse(TMLRES > 0, 1, 0),
           TMLL = ifelse(TMLRES < 0, 1, 0), TTOTW = ifelse(TTOTRES > 0, 1, 0), TTOTL = ifelse(TTOTRES < 0, 1, 0),
           BATSW = ifelse(BATSRES > 0, 1, 0), BATSL = ifelse(BATSRES < 0, 1, 0), BMLW = ifelse(BMLRES > 0, 1, 0),
           BMLL = ifelse(BMLRES < 0, 1, 0), BTOTW = ifelse(BTOTRES > 0, 1, 0), BTOTL = ifelse(BTOTRES < 0, 1, 0),
           AATSW = ifelse(AATSRES > 0, 1, 0), AATSL = ifelse(AATSRES < 0, 1, 0), AMLW = ifelse(AMLRES > 0, 1, 0),
           AMLL = ifelse(AMLRES < 0, 1, 0), ATOTW = ifelse(ATOTRES > 0, 1, 0), ATOTL = ifelse(ATOTRES < 0, 1, 0),
           SATSW = ifelse(SATSRES > 0, 1, 0), SATSL = ifelse(SATSRES < 0, 1, 0), SMLW = ifelse(SMLRES > 0, 1, 0),
           SMLL = ifelse(SMLRES < 0, 1, 0), STOTW = ifelse(STOTRES > 0, 1, 0), STOTL = ifelse(STOTRES < 0, 1, 0),
           CATSW = ifelse(CATSRES > 0, 1, 0), CATSL = ifelse(CATSRES < 0, 1, 0), CMLW = ifelse(CMLRES > 0, 1, 0),
           CMLL = ifelse(CMLRES < 0, 1, 0), CTOTW = ifelse(CTOTRES > 0, 1, 0), CTOTL = ifelse(CTOTRES < 0, 1, 0))
  
  bx_break_group <<- bx_break_group %>%
    filter(is.na(IATSRES) == FALSE)
  
    #5 or 6 Agree ATS
    agree_vec <- c(1, 4, 7, 10, 13, 16)
    bx_break_mx <<- as.matrix(bx_break_group[, c(4:21)])
    a <- 1
    g <- nrow(bx_break_mx)
    
    
    for (a in a:g) {
      
      
      b <- 1
      h <- length(agree_vec)
      
      for (b in b:h) {
        
        agree_col <- agree_vec[b]
        agree_res <- as.numeric(bx_break_mx[a, agree_col])
        if (b == 1) { agree_cume <- agree_res }
        else { agree_cume <- c(agree_cume, agree_res) }
        
      }
      if (sum(agree_cume) == 0) { bx_break_group[a, 58:59] <<- 0 }
      
      else {
        
        agree_table <- table(agree_cume)
        agree_t1 <- as.numeric(agree_table[1])
        agree_h1 <- as.numeric(names(agree_table)[1])
        
        agree_t2 <- as.numeric(agree_table[2])
        agree_h2 <- as.numeric(names(agree_table)[2])
        
        if (length(agree_table) == 1) { 
          bx_break_group[a, 58:59] <<- agree_h1
        }
        
        else {
          
          if (agree_t1 < 5 & agree_t2 < 5) { bx_break_group[a ,58:59] <<- 0 }
          else { 
            if (agree_t1 == 5) { 
              bx_break_group[a, 58] <<- agree_h1
              bx_break_group[a, 59] <<- 0
            }
            if (agree_t2 == 5) {
              bx_break_group[a, 58] <<- agree_h2
              bx_break_group[a, 59] <<- 0
            }
          }
        }
      }
    }
    
    #5 or 6 Agree ML
    agree_vec <- c(2, 5, 8, 11, 14, 17)
    bx_break_mx <<- as.matrix(bx_break_group[, c(4:21)])
    a <- 1
    g <- nrow(bx_break_mx)
    
    for (a in a:g) {
      
      
      b <- 1
      h <- length(agree_vec)
      
      for (b in b:h) {
        
        agree_col <- agree_vec[b]
        agree_res <- as.numeric(bx_break_mx[a, agree_col])
        if (b == 1) { agree_cume <- agree_res }
        else { agree_cume <- c(agree_cume, agree_res) }
        
      }
      if (sum(agree_cume) == 0) { bx_break_group[a, 60:61] <<- 0 }
      
      else {
        
        agree_table <- table(agree_cume)
        agree_t1 <- as.numeric(agree_table[1])
        agree_h1 <- as.numeric(names(agree_table)[1])
        
        agree_t2 <- as.numeric(agree_table[2])
        agree_h2 <- as.numeric(names(agree_table)[2])
        
        if (length(agree_table) == 1) { 
          bx_break_group[a, 60:61] <<- agree_h1
        }
        
        else {
          
          if (agree_t1 < 5 & agree_t2 < 5) { bx_break_group[a ,60:61] <<- 0 }
          else { 
            if (agree_t1 == 5) { 
              bx_break_group[a, 60] <<- agree_h1
              bx_break_group[a, 61] <<- 0
            }
            if (agree_t2 == 5) {
              bx_break_group[a, 60] <<- agree_h2
              bx_break_group[a, 61] <<- 0
            }
          }
        }
      }
    }
    
    #5 or 6 Agree TOT
    agree_vec <- c(3, 6, 9, 12, 15, 18)
    bx_break_mx <<- as.matrix(bx_break_group[, c(4:21)])
    a <- 1
    g <- nrow(bx_break_mx)
    
    for (a in a:g) {
      
      
      b <- 1
      h <- length(agree_vec)
      
      for (b in b:h) {
        
        agree_col <- agree_vec[b]
        agree_res <- as.numeric(bx_break_mx[a, agree_col])
        if (b == 1) { agree_cume <- agree_res }
        else { agree_cume <- c(agree_cume, agree_res) }
        
      }
      if (sum(agree_cume) == 0) { bx_break_group[a, 62:63] <<- 0 }
      
      else {
        
        agree_table <- table(agree_cume)
        agree_t1 <- as.numeric(agree_table[1])
        agree_h1 <- as.numeric(names(agree_table)[1])
        
        agree_t2 <- as.numeric(agree_table[2])
        agree_h2 <- as.numeric(names(agree_table)[2])
        
        if (length(agree_table) == 1) { 
          bx_break_group[a, 62:63] <<- agree_h1
        }
        
        else {
          
          if (agree_t1 < 5 & agree_t2 < 5) { bx_break_group[a ,62:63] <<- 0 }
          else { 
            if (agree_t1 == 5) { 
              bx_break_group[a, 62] <<- agree_h1
              bx_break_group[a, 63] <<- 0
            }
            if (agree_t2 == 5) {
              bx_break_group[a, 62] <<- agree_h2
              bx_break_group[a, 63] <<- 0
            }
          }
        }
      }
    }
    
    colnames(bx_break_group)[58:63] <<- c("ATS5", "ATS6", "ML5", "ML6", "TOT5", "TOT6")
  
    bx_break_group <<- bx_break_group %>%
    group_by(Month, Day, Year) %>%
    summarize(IATSUNIT = sum(IATSRES), IATSW = sum(IATSW), IATSL = sum(IATSL),
              TATSUNIT = sum(TATSRES), TATSW = sum(TATSW), TATSL = sum(TATSL),
              BATSUNIT = sum(BATSRES), BATSW = sum(BATSW), BATSL = sum(BATSL),
              AATSUNIT = sum(AATSRES), AATSW = sum(AATSW), AATSL = sum(AATSL),
              SATSUNIT = sum(SATSRES), SATSW = sum(SATSW), SATSL = sum(SATSL),
              CATSUNIT = sum(CATSRES), CATSW = sum(CATSW), CATSL = sum(CATSL),
              IMLUNIT = sum(IMLRES), IMLW = sum(IMLW), IMLL = sum(IMLL),
              TMLUNIT = sum(TMLRES), TMLW = sum(TMLW), TMLL = sum(TMLL),
              BMLUNIT = sum(BMLRES), BMLW = sum(BMLW), BMLL = sum(BMLL),
              AMLUNIT = sum(AMLRES), AMLW = sum(AMLW), AMLL = sum(AMLL),
              SMLUNIT = sum(SMLRES), SMLW = sum(SMLW), SMLL = sum(SMLL),
              CMLUNIT = sum(CMLRES), CMLW = sum(CMLW), CMLL = sum(CMLL),
              ITOTUNIT = sum(ITOTRES), ITOTW = sum(ITOTW), ITOTL = sum(ITOTL),
              TTOTUNIT = sum(TTOTRES), TTOTW = sum(TTOTW), TTOTL = sum(TTOTL),
              BTOTUNIT = sum(BTOTRES), BTOTW = sum(BTOTW), BTOTL = sum(BTOTL),
              ATOTUNIT = sum(ATOTRES), ATOTW = sum(ATOTW), ATOTL = sum(ATOTL),
              STOTUNIT = sum(STOTRES), STOTW = sum(STOTW), STOTL = sum(STOTL),
              CTOTUNIT = sum(CTOTRES), CTOTW = sum(CTOTW), CTOTL = sum(CTOTL),
              ATS5UNIT = sum(ATS5), ATS6UNIT = sum(ATS6), ML5UNIT = sum(ML5),
              ML6UNIT = sum(ML6), TOT5UNIT = sum(TOT5), TOT6UNIT = sum(TOT6))
    
    bx_break_tidy1 <- gather(bx_break_group, "MTYPE", "UNIT", c(IATSUNIT, TATSUNIT, BATSUNIT, AATSUNIT, SATSUNIT, CATSUNIT,
                                                                 IMLUNIT, TMLUNIT, BMLUNIT, AMLUNIT, SMLUNIT, CMLUNIT,
                                                                 ITOTUNIT, TTOTUNIT, BTOTUNIT, ATOTUNIT, STOTUNIT, CTOTUNIT,
                                                                 ATS5UNIT, ATS6UNIT, ML5UNIT, ML6UNIT, TOT5UNIT, TOT6UNIT))
    
    
    bx_break_tidy2 <- gather(bx_break_group, "MTYPE", "UNIT", c(IATSW, TATSW, BATSW, AATSW, SATSW, CATSW,
                                                IMLW, TMLW, BMLW, AMLW, SMLW, CMLW,
                                                ITOTW, TTOTW, BTOTW, ATOTW, STOTW, CTOTW))
    
    
    bx_break_tidy3 <- gather(bx_break_group, "MTYPE", "UNIT", c(IATSL, TATSL, BATSL, AATSL, SATSL, CATSL,
                                                    IMLL, TMLL, BMLL, AMLL, SMLL, CMLL,
                                                    ITOTL, TTOTL, BTOTL, ATOTL, STOTL, CTOTL))
    
    
    
    bx_break_tidy <<- rbind(bx_break_tidy1[, c(1:3, 40:41)], bx_break_tidy2[, c(1:3, 46:47)], bx_break_tidy3[, c(1:3, 46:47)])
    
    bx_break_tidydates <- bx_break_tidy %>%
      group_by(Month, Day, Year) %>%
      summarize(ct = n())

    bx_break_tidy[, 6] <<- 0
    if (nrow(bx_break_tidydates) <= 14) { 
      bx_break_tidy[, 6] <<- 1  
    }
    else {
      r <- nrow(bx_break_tidydates) - 13
      u <- nrow(bx_break_tidydates)
      bx_break_tidydates <- bx_break_tidydates[r:u, ]
      b <- 1
      h <- nrow(bx_break_tidydates)
      for (b in b:h) {
      
        mm_ <- as.numeric(bx_break_tidydates[b, 1]) 
        dd_ <- as.numeric(bx_break_tidydates[b, 2]) 
        yy_ <- as.numeric(bx_break_tidydates[b, 3]) 
        
        ww_ <- which(bx_break_tidy$Month == mm_ & bx_break_tidy$Day == dd_ & bx_break_tidy$Year == yy_)
        bx_break_tidy[ww_, 6] <<- 1 
        
      }
    }
    
    write_csv(bx_break_tidy, "bxbreak5.csv")
  
      
}

bx_keyer <- function(xxxx) {
  
  #Total Key
  
  res_vec <- c(37:54)
  val1_vec <- c(55, 67, 79, 56, 68, 80, 57, 69, 81, 58, 70, 82, 59, 71, 83, 60, 72, 84)
  val2_vec <- c(61, 73, 85, 62, 74, 86, 63, 75, 87, 64, 76, 88, 65, 77, 89, 66, 78, 90)
  nms_vec <- c("IATS", "IML", "ITOT", "TATS", "TML", "TTOT", "BATS", "BML", "BTOT", 
               "AATS", "AML", "ATOT", "SATS", "SML", "STOT", "CATS", "CML", "CTOT")
  
  bx_break_use <- bx_break
  
  a <- 1
  g <- length(res_vec)
  ct <- 0
  
  for (a in a:g) {
    
    res_col <- res_vec[a]
    val1_col <- val1_vec[a]
    val2_col <- val2_vec[a]
    nms_col <- nms_vec[a]
    
    tbl_1 <- bx_break_use[, c(res_col, val1_col)]
    tbl_2 <- bx_break_use[, c(res_col, val2_col)]
    
    colnames(tbl_1) <- c("RES", "VAL")
    colnames(tbl_2) <- c("RES", "VAL")
    
    tbl_r <- rbind(tbl_1, tbl_2)
    tbl_r <- tbl_r %>%
      filter(VAL > 0) %>%
      arrange(desc(VAL))
    
    tbl_r[, 3] <- 0
    tbl_mx <- as.matrix(tbl_r)
    
    b <- 1
    h <- nrow(tbl_mx)
    roll_ <- 0
    
    for (b in b:h) {
      
       roll_ <- roll_ + as.numeric(tbl_mx[b, 1])
       tbl_mx[b, 3] <- roll_
      
    }
    
    key_w <- which.max(tbl_mx[, 3])[[1]]
    key_ <- as.numeric(tbl_mx[key_w, 2])
    key_u <- as.numeric(tbl_mx[key_w, 3])
    
    key_div <- matrix(tbl_mx[1:key_w, ], nrow = key_w, ncol = 3)
    
    key_wins <- length(which(key_div[, 1] > 0))
    key_loss <- length(which(key_div[, 1] < 0))
    
    ct <- ct + 1
    if (ct == 1) { 
      bx_key_frame <- data.frame(0, 0, 0, nms_col, key_, key_u, key_wins, key_loss, stringsAsFactors = FALSE) 
      colnames(bx_key_frame) <- c("Month", "Day", "Year", "Type", "Key", "Unit", "Win", "Loss")
    }
    else { 
      bx_key_frame2 <- data.frame(0, 0, 0, nms_col, key_, key_u, key_wins, key_loss, stringsAsFactors = FALSE)
      colnames(bx_key_frame2) <- c("Month", "Day", "Year", "Type", "Key", "Unit", "Win", "Loss")
      bx_key_frame <- rbind(bx_key_frame, bx_key_frame2)     
    }
  }
  
  #14 Key
  
  res_vec <- c(37:54)
  val1_vec <- c(55, 67, 79, 56, 68, 80, 57, 69, 81, 58, 70, 82, 59, 71, 83, 60, 72, 84)
  val2_vec <- c(61, 73, 85, 62, 74, 86, 63, 75, 87, 64, 76, 88, 65, 77, 89, 66, 78, 90)
  
  bx_break_use <- bx_break %>%
    filter(LAST14 == 1)
  
  a <- 1
  g <- length(res_vec)
  
  for (a in a:g) {
    
    res_col <- res_vec[a]
    val1_col <- val1_vec[a]
    val2_col <- val2_vec[a]
    nms_col <- nms_vec[a]
    
    tbl_1 <- bx_break_use[, c(res_col, val1_col)]
    tbl_2 <- bx_break_use[, c(res_col, val2_col)]
    
    colnames(tbl_1) <- c("RES", "VAL")
    colnames(tbl_2) <- c("RES", "VAL")
    
    tbl_r <- rbind(tbl_1, tbl_2)
    tbl_r <- tbl_r %>%
      filter(VAL > 0) %>%
      arrange(desc(VAL))
    
    tbl_r[, 3] <- 0
    tbl_mx <- as.matrix(tbl_r)
    
    b <- 1
    h <- nrow(tbl_mx)
    roll_ <- 0
    
    for (b in b:h) {
      
      roll_ <- roll_ + as.numeric(tbl_mx[b, 1])
      tbl_mx[b, 3] <- roll_
      
    }
    key_w <- which.max(tbl_mx[, 3])[[1]]
    key_ <- as.numeric(tbl_mx[key_w, 2])
    key_u <- as.numeric(tbl_mx[key_w, 3])
    
    key_div <- matrix(tbl_mx[1:key_w, ], nrow = key_w, ncol = 3)
    
    key_wins <- length(which(key_div[, 1] > 0))
    key_loss <- length(which(key_div[, 1] < 0))
    
    bx_key_frame2 <- data.frame(14, 14, 14, nms_col, key_, key_u, key_wins, key_loss, stringsAsFactors = FALSE)
    colnames(bx_key_frame2) <- c("Month", "Day", "Year", "Type", "Key", "Unit", "Win", "Loss")
    bx_key_frame <- rbind(bx_key_frame, bx_key_frame2)     
    
  }

  #Day Keys
  
  
  res_vec <- c(37:54)
  val1_vec <- c(55, 67, 79, 56, 68, 80, 57, 69, 81, 58, 70, 82, 59, 71, 83, 60, 72, 84)
  val2_vec <- c(61, 73, 85, 62, 74, 86, 63, 75, 87, 64, 76, 88, 65, 77, 89, 66, 78, 90)
  
  bx_dates <- bx_break %>%
    group_by(Month, Day, Year) %>%
    summarize(ct = n())
  
  c <- 1
  i <- nrow(bx_dates)
  
  for (c in c:i) {
    
    a <- 1
    g <- length(res_vec)
    
    a_m <- as.numeric(bx_dates[c, 1])
    a_d <- as.numeric(bx_dates[c, 2])
    a_y <- as.numeric(bx_dates[c, 3])
    
    bx_break_use <- bx_break %>%
      filter(Month == a_m, Day == a_d, Year == a_y)
    
    for (a in a:g) {
      
      res_col <- res_vec[a]
      val1_col <- val1_vec[a]
      val2_col <- val2_vec[a]
      nms_col <- nms_vec[a]
      
      tbl_1 <- bx_break_use[, c(res_col, val1_col)]
      tbl_2 <- bx_break_use[, c(res_col, val2_col)]
      
      colnames(tbl_1) <- c("RES", "VAL")
      colnames(tbl_2) <- c("RES", "VAL")
      
      tbl_r <- rbind(tbl_1, tbl_2)
      tbl_r <- tbl_r %>%
        filter(VAL > 0) %>%
        arrange(desc(VAL))
      
      tbl_r[, 3] <- 0
      tbl_mx <- as.matrix(tbl_r)
      
      b <- 1
      h <- nrow(tbl_mx)
      roll_ <- 0
      
      for (b in b:h) {
        
        roll_ <- roll_ + as.numeric(tbl_mx[b, 1])
        tbl_mx[b, 3] <- roll_
        
      }
      key_w <- which.max(tbl_mx[, 3])[[1]]
      key_ <- as.numeric(tbl_mx[key_w, 2])
      key_u <- as.numeric(tbl_mx[key_w, 3])
      
      key_div <- matrix(tbl_mx[1:key_w, ], nrow = key_w, ncol = 3)
      
      key_wins <- length(which(key_div[, 1] > 0))
      key_loss <- length(which(key_div[, 1] < 0))
      
      bx_key_frame2 <- data.frame(a_m, a_d, a_y, nms_col, key_, key_u, key_wins, key_loss, stringsAsFactors = FALSE)
      colnames(bx_key_frame2) <- c("Month", "Day", "Year", "Type", "Key", "Unit", "Win", "Loss")
      bx_key_frame <- rbind(bx_key_frame, bx_key_frame2)     
      
    }
  }
  
  bx_key_frame_final <<- bx_key_frame
  write_csv(bx_key_frame_final, "bxkey5.csv")
}

bet_value_result5 <- function(xxxx) {
  
  results_clip_a <- results_frame[, c(6, 10, 11)]
  results_clip_h <- results_frame[, c(7, 11, 10)]
  results_clip_t <- results_frame[, c(6, 12)]
  colnames(results_clip_t) <- c("TEAM", "RES")
  
  
  results_clip_a <- results_clip_a %>%
    mutate(RES = ASCORE - HSCORE) %>%
    select(Away, RES)
  
  results_clip_h <- results_clip_h %>%
    mutate(RES = HSCORE - ASCORE) %>%
    select(Home, RES)
  
  colnames(results_clip_a) <- c("TEAM", "RES")
  colnames(results_clip_h) <- c("TEAM", "RES")
  
  results_clip_m <- rbind(results_clip_a, results_clip_h)
  
  bet_value_frame5_use <<- read_csv('betvalueframe5tmp.csv')
  
  spread_bt_5 <- bet_value_frame5_use %>%
    filter(BTYPE == "SPREAD") %>%
    left_join(results_clip_m, by = c("TEAM")) %>%
    mutate(ATSRES = LINE + RES) %>%
    mutate(RESULT = ifelse(ATSRES == 0, 0, ifelse(ATSRES > 0, 1, -1.1))) %>%
    filter(is.na(RES) == FALSE)
  
  ml_bt_5 <- bet_value_frame5_use %>%
    filter(BTYPE == "ML") %>%
    left_join(results_clip_m, by = c("TEAM")) %>%
    mutate(ATSRES = RES) %>%
    mutate(RESULT = ifelse(RES < 0, -1, ifelse(LINE > 0, LINE / 100, 100 / abs(LINE)))) %>%
    filter(is.na(RES) == FALSE)
  
  over_bt_5 <- bet_value_frame5_use %>%
    filter(BTYPE == "OVER") %>%
    left_join(results_clip_t, by = c("TEAM")) %>%
    mutate(ATSRES = RES - LINE) %>%
    mutate(RESULT = ifelse(ATSRES == 0, 0, ifelse(ATSRES > 0, 1, -1.1))) %>%
    filter(is.na(RES) == FALSE)
  
  under_bt_5 <- bet_value_frame5_use %>%
    filter(BTYPE == "UNDER") %>%
    left_join(results_clip_t, by = c("TEAM")) %>%
    mutate(ATSRES = LINE - RES) %>%
    mutate(RESULT = ifelse(ATSRES == 0, 0, ifelse(ATSRES > 0, 1, -1.1))) %>%
    filter(is.na(RES) == FALSE)
  
  master_bv_frame <- rbind(spread_bt_5, ml_bt_5, over_bt_5, under_bt_5)
  write_csv(master_bv_frame, "results5tmp.csv")
  
  if (xxxx == 2) {
    
    tmp_rf <- read_csv("Master Results Frame 5.csv")
    last_day <- as.numeric(tmp_rf[nrow(tmp_rf), 11])
    print(last_day)
    master_bv_frame[, 11] <- (last_day + 1)
    colnames(master_bv_frame)[11] <- "DAY"
    tmp_wr <- rbind(tmp_rf, master_bv_frame)
    write_csv(tmp_wr, "Master Results Frame 5.csv")
    
  }
}

yesterdays_breakdown <- function(m, d, y) {
  
  yd_df <- read_csv("Todays Plays.csv")
  print(yd_df)
  results_clip_a <- results_frame[, c(6, 10, 11)]
  results_clip_h <- results_frame[, c(7, 11, 10)]
  results_clip_t <- results_frame[, c(6, 12)]
  colnames(results_clip_t) <- c("TEAM", "RES")
  
  
  results_clip_a <- results_clip_a %>%
    mutate(RES = ASCORE - HSCORE) %>%
    select(Away, RES)
  
  results_clip_h <- results_clip_h %>%
    mutate(RES = HSCORE - ASCORE) %>%
    select(Home, RES)
  
  colnames(results_clip_a) <- c("TEAM", "RES")
  colnames(results_clip_h) <- c("TEAM", "RES")
  
  results_clip_m <- rbind(results_clip_a, results_clip_h)
  
  spread_yd <- yd_df %>%
    filter(`BET TYPE` == "ATS") %>%
    left_join(results_clip_m, by = c("TEAM")) %>%
    mutate(ATSRES = LINE + RES) %>%
    mutate(RESULT = ifelse(ATSRES == 0, 0, ifelse(ATSRES > 0, 1, -1.1))) %>%
    filter(is.na(RES) == FALSE)
  
  ml_yd <- yd_df %>%
    filter(`BET TYPE` == "ML") %>%
    left_join(results_clip_m, by = c("TEAM")) %>%
    mutate(ATSRES = RES) %>%
    mutate(RESULT = ifelse(RES < 0, -1, ifelse(LINE > 0, LINE / 100, 100 / abs(LINE)))) %>%
    filter(is.na(RES) == FALSE)
  
  over_yd <- yd_df %>%
    filter(`BET TYPE` == "OVER") %>%
    left_join(results_clip_t, by = c("TEAM")) %>%
    mutate(ATSRES = RES - LINE) %>%
    mutate(RESULT = ifelse(ATSRES == 0, 0, ifelse(ATSRES > 0, 1, -1.1))) %>%
    filter(is.na(RES) == FALSE)
  
  under_yd <- yd_df %>%
    filter(`BET TYPE` == "UNDER") %>%
    left_join(results_clip_t, by = c("TEAM")) %>%
    mutate(ATSRES = LINE - RES) %>%
    mutate(RESULT = ifelse(ATSRES == 0, 0, ifelse(ATSRES > 0, 1, -1.1))) %>%
    filter(is.na(RES) == FALSE)
  
  master_yd_frame <- rbind(spread_yd, ml_yd, over_yd, under_yd)
  
  colnames(master_yd_frame)[12] <- "DAY"

  write_csv(master_yd_frame, "Yesterdays Results.csv")
  
  tmp_rf <- read_csv("My Plays DB.csv")
  
  print(tmp_rf)
  print(master_yd_frame)
  
  tmp_w <- which(tmp_rf$MONTH == m & tmp_rf$DAY == d & tmp_rf$YEAR == y)
  tmp_w <- -1 * tmp_w
  tmp_rf <- tmp_rf[tmp_w, ]
  tmp_wr <- rbind(tmp_rf, master_yd_frame)
  write_csv(tmp_wr, "My Plays DB.csv")
  
} 

my_plays_analyze <- function(xxxx) {
  
  my_play_df <- read_csv("My Plays DB.csv") %>%
    mutate(WIN = ifelse(RESULT == 0, 0, ifelse(RESULT > 0, 1, 0))) %>%
    mutate(LOSS = ifelse(RESULT == 0, 0, ifelse(RESULT < 0, 1, 0)))
  
  #ATS
  
  my_ats_res <- my_play_df %>%
    filter(`BET TYPE` == "ATS") %>%
    group_by(MONTH, DAY, YEAR) %>%
    summarize(UNIT = sum(RESULT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS)))

  my_ats_res[, 8] <- "ATS"
  colnames(my_ats_res)[8] <- "TYPE"
  
  if (nrow(my_ats_res) <= 14) { 
    my_ats_14 <- my_ats_res %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_ats_14_plug <- my_ats_res[1, ]
    my_ats_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_ats_14_plug[1, 4] <- my_ats_14$UNIT
    my_ats_14_plug[1, 5] <- my_ats_14$WIN
    my_ats_14_plug[1, 6] <- my_ats_14$LOSS
    my_ats_14_plug[1, 7] <- my_ats_14$PCT
    my_ats_14_plug[1, 8] <- "ATS"
  }
  else {
    
    ats_14s <- nrow(my_ats_res) - 13
    ats_14e <- nrow(my_ats_res)
    my_ats_14 <- my_ats_res[ats_14s:ats_14e, ] %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_ats_14_plug <- my_ats_res[1, ]
    my_ats_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_ats_14_plug[1, 4] <- my_ats_14$UNIT
    my_ats_14_plug[1, 5] <- my_ats_14$WIN
    my_ats_14_plug[1, 6] <- my_ats_14$LOSS
    my_ats_14_plug[1, 7] <- my_ats_14$PCT
    my_ats_14_plug[1, 8] <- "ATS"
    
  }
  
  my_ats_whole <- my_ats_res %>%
    ungroup() %>%
    summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
    select(UNIT, WIN, LOSS, PCT)
  
  my_ats_whole_plug <- my_ats_res[1, ]
  my_ats_whole_plug[1, 1:3] <- c(0 , 0 ,0)
  my_ats_whole_plug[1, 4] <- my_ats_whole$UNIT
  my_ats_whole_plug[1, 5] <- my_ats_whole$WIN
  my_ats_whole_plug[1, 6] <- my_ats_whole$LOSS
  my_ats_whole_plug[1, 7] <- my_ats_whole$PCT
  my_ats_whole_plug[1, 8] <- "ATS"
              
  my_ats_comb <- rbind(my_ats_res, my_ats_14_plug, my_ats_whole_plug)
  
  #Total
  
  my_tot_res <- my_play_df %>%
    filter(`BET TYPE` == "OVER" | `BET TYPE` == "UNDER") %>%
    group_by(MONTH, DAY, YEAR) %>%
    summarize(UNIT = sum(RESULT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS)))
  
  my_tot_res[, 8] <- "TOT"
  colnames(my_tot_res)[8] <- "TYPE"
  
  if (nrow(my_tot_res) <= 14) { 
    my_tot_14 <- my_tot_res %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_tot_14_plug <- my_tot_res[1, ]
    my_tot_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_tot_14_plug[1, 4] <- my_tot_14$UNIT
    my_tot_14_plug[1, 5] <- my_tot_14$WIN
    my_tot_14_plug[1, 6] <- my_tot_14$LOSS
    my_tot_14_plug[1, 7] <- my_tot_14$PCT
    my_tot_14_plug[1, 8] <- "TOT"
  }
  else {
    
    tot_14s <- nrow(my_tot_res) - 13
    tot_14e <- nrow(my_tot_res)
    my_tot_14 <- my_tot_res[tot_14s:tot_14e, ] %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_tot_14_plug <- my_tot_res[1, ]
    my_tot_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_tot_14_plug[1, 4] <- my_tot_14$UNIT
    my_tot_14_plug[1, 5] <- my_tot_14$WIN
    my_tot_14_plug[1, 6] <- my_tot_14$LOSS
    my_tot_14_plug[1, 7] <- my_tot_14$PCT
    my_tot_14_plug[1, 8] <- "TOT"
    
  }
  
  my_tot_whole <- my_tot_res %>%
    ungroup() %>%
    summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
    select(UNIT, WIN, LOSS, PCT)
  
  my_tot_whole_plug <- my_tot_res[1, ]
  my_tot_whole_plug[1, 1:3] <- c(0 , 0 ,0)
  my_tot_whole_plug[1, 4] <- my_tot_whole$UNIT
  my_tot_whole_plug[1, 5] <- my_tot_whole$WIN
  my_tot_whole_plug[1, 6] <- my_tot_whole$LOSS
  my_tot_whole_plug[1, 7] <- my_tot_whole$PCT
  my_tot_whole_plug[1, 8] <- "TOT"
  
  my_tot_comb <- rbind(my_tot_res, my_tot_14_plug, my_tot_whole_plug)
  
  
  #ML
  
  my_ml_res <- my_play_df %>%
    filter(`BET TYPE` == "ML") %>%
    group_by(MONTH, DAY, YEAR) %>%
    summarize(UNIT = sum(RESULT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS)))
  
  my_ml_res[, 8] <- "ML"
  colnames(my_ml_res)[8] <- "TYPE"
  
  if (nrow(my_ml_res) <= 14) { 
    my_ml_14 <- my_ml_res %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_ml_14_plug <- my_ml_res[1, ]
    my_ml_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_ml_14_plug[1, 4] <- my_ml_14$UNIT
    my_ml_14_plug[1, 5] <- my_ml_14$WIN
    my_ml_14_plug[1, 6] <- my_ml_14$LOSS
    my_ml_14_plug[1, 7] <- my_ml_14$PCT
    my_ml_14_plug[1, 8] <- "ML"
  }
  else {
    
    ml_14s <- nrow(my_ml_res) - 13
    ml_14e <- nrow(my_ml_res)
    my_ml_14 <- my_ml_res[ml_14s:ml_14e, ] %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_ml_14_plug <- my_ml_res[1, ]
    my_ml_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_ml_14_plug[1, 4] <- my_ml_14$UNIT
    my_ml_14_plug[1, 5] <- my_ml_14$WIN
    my_ml_14_plug[1, 6] <- my_ml_14$LOSS
    my_ml_14_plug[1, 7] <- my_ml_14$PCT
    my_ml_14_plug[1, 8] <- "ML"
    
  }
  
  my_ml_whole <- my_ml_res %>%
    ungroup() %>%
    summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
    select(UNIT, WIN, LOSS, PCT)
  
  my_ml_whole_plug <- my_ml_res[1, ]
  my_ml_whole_plug[1, 1:3] <- c(0 , 0 ,0)
  my_ml_whole_plug[1, 4] <- my_ml_whole$UNIT
  my_ml_whole_plug[1, 5] <- my_ml_whole$WIN
  my_ml_whole_plug[1, 6] <- my_ml_whole$LOSS
  my_ml_whole_plug[1, 7] <- my_ml_whole$PCT
  my_ml_whole_plug[1, 8] <- "ML"
  
  my_ml_comb <- rbind(my_ml_res, my_ml_14_plug, my_ml_whole_plug)
  
  
  #Overall
  
  my_ovr_res <- my_play_df %>%
    group_by(MONTH, DAY, YEAR) %>%
    summarize(UNIT = sum(RESULT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS)))
  
  my_ovr_res[, 8] <- "OVR"
  colnames(my_ovr_res)[8] <- "TYPE"
  
  if (nrow(my_ovr_res) <= 14) { 
    my_ovr_14 <- my_ovr_res %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_ovr_14_plug <- my_ovr_res[1, ]
    my_ovr_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_ovr_14_plug[1, 4] <- my_ovr_14$UNIT
    my_ovr_14_plug[1, 5] <- my_ovr_14$WIN
    my_ovr_14_plug[1, 6] <- my_ovr_14$LOSS
    my_ovr_14_plug[1, 7] <- my_ovr_14$PCT
    my_ovr_14_plug[1, 8] <- "OVR"
  }
  else {
    
    ovr_14s <- nrow(my_ovr_res) - 13
    ovr_14e <- nrow(my_ovr_res)
    my_ovr_14 <- my_ovr_res[ovr_14s:ovr_14e, ] %>%
      ungroup() %>%
      summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
      select(UNIT, WIN, LOSS, PCT)
    
    my_ovr_14_plug <- my_ovr_res[1, ]
    my_ovr_14_plug[1, 1:3] <- c(14 , 14 ,14)
    my_ovr_14_plug[1, 4] <- my_ovr_14$UNIT
    my_ovr_14_plug[1, 5] <- my_ovr_14$WIN
    my_ovr_14_plug[1, 6] <- my_ovr_14$LOSS
    my_ovr_14_plug[1, 7] <- my_ovr_14$PCT
    my_ovr_14_plug[1, 8] <- "OVR"
    
  }
  
  my_ovr_whole <- my_ovr_res %>%
    ungroup() %>%
    summarize(UNIT = sum(UNIT), WIN = sum(WIN), LOSS = sum(LOSS), PCT = sum(WIN) / (sum(WIN) + sum(LOSS))) %>%
    select(UNIT, WIN, LOSS, PCT)
  
  my_ovr_whole_plug <- my_ovr_res[1, ]
  my_ovr_whole_plug[1, 1:3] <- c(0 , 0 ,0)
  my_ovr_whole_plug[1, 4] <- my_ovr_whole$UNIT
  my_ovr_whole_plug[1, 5] <- my_ovr_whole$WIN
  my_ovr_whole_plug[1, 6] <- my_ovr_whole$LOSS
  my_ovr_whole_plug[1, 7] <- my_ovr_whole$PCT
  my_ovr_whole_plug[1, 8] <- "OVR"
  
  my_ovr_comb <- rbind(my_ovr_res, my_ovr_14_plug, my_ovr_whole_plug)
  
  all_comb <- rbind(my_ovr_comb, my_ats_comb, my_tot_comb, my_ml_comb)
  
  write_csv(all_comb, "My Plays Stats.csv")

}

master_key_finder_set <- function(s, e) {
  
  combo_mx <- read_csv("keycombomx.csv")
  key_df <- read_csv("Master Results Frame 5.csv") %>%
    filter(DAY >= s & DAY <= e)
 
  key_df_lineup <- key_df %>%
    group_by(LOC, TEAM, OPP, DAY) %>%
    summarize(ct = n()) %>%
    select(LOC, TEAM, OPP, DAY)
  
  key_df_tineup <- key_df %>%
    group_by(BTYPE, TEAM, OPP, DAY) %>%
    summarize(ct = n()) %>%
    select(BTYPE, TEAM, OPP, DAY)
  
  indy_df_lineup_ats <- key_df %>%
    filter(MTYPE == "INDY", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  team_df_lineup_ats <- key_df %>%
    filter(MTYPE == "TEAM", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  blnd_df_lineup_ats <- key_df %>%
    filter(MTYPE == "BLND", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  atm_df_lineup_ats <- key_df %>%
    filter(MTYPE == "ATM", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  sim_df_lineup_ats <- key_df %>%
    filter(MTYPE == "SIM", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  comb_df_lineup_ats <- key_df %>%
    filter(MTYPE == "COMB", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  master_df_lineup_ats <- indy_df_lineup_ats %>%
    inner_join(team_df_lineup_ats, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(blnd_df_lineup_ats, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(atm_df_lineup_ats, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(sim_df_lineup_ats, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(comb_df_lineup_ats, by =c ("LOC", "TEAM", "OPP", "DAY"))
  
  master_df_lineup_ats <<- master_df_lineup_ats[, c(1:4, 5, 7, 9, 11, 13, 15, 6)]
  colnames(master_df_lineup_ats)[5:11] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB", "RES")
  
  #Total Master Frame
  
  indy_df_lineup_tot <- key_df %>%
    filter(MTYPE == "INDY") %>%
    filter(BTYPE == "UNDER" | BTYPE == "OVER") %>%
    select(BTYPE, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_tineup, by = c("BTYPE", "TEAM", "OPP", "DAY"))
  
  team_df_lineup_tot <- key_df %>%
    filter(MTYPE == "TEAM") %>%
    filter(BTYPE == "UNDER" | BTYPE == "OVER") %>%
    select(BTYPE, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_tineup, by = c("BTYPE", "TEAM", "OPP", "DAY"))
  
  blnd_df_lineup_tot <- key_df %>%
    filter(MTYPE == "BLND") %>%
    filter(BTYPE == "UNDER" | BTYPE == "OVER") %>%
    select(BTYPE, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_tineup, by = c("BTYPE", "TEAM", "OPP", "DAY"))
  
  atm_df_lineup_tot <- key_df %>%
    filter(MTYPE == "ATM") %>%
    filter(BTYPE == "UNDER" | BTYPE == "OVER") %>%
    select(BTYPE, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_tineup, by = c("BTYPE", "TEAM", "OPP", "DAY"))
  
  sim_df_lineup_tot <- key_df %>%
    filter(MTYPE == "SIM") %>%
    filter(BTYPE == "UNDER" | BTYPE == "OVER") %>%
    select(BTYPE, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_tineup, by = c("BTYPE", "TEAM", "OPP", "DAY"))
  
  comb_df_lineup_tot <- key_df %>%
    filter(MTYPE == "COMB") %>%
    filter(BTYPE == "UNDER" | BTYPE == "OVER") %>%
    select(BTYPE, TEAM, OPP, DAY, VALUE, RESULT) %>%
    inner_join(key_df_tineup, by = c("BTYPE", "TEAM", "OPP", "DAY"))
  
  master_df_lineup_tot <- indy_df_lineup_tot %>%
    inner_join(team_df_lineup_tot, by =c ("BTYPE", "TEAM", "OPP", "DAY")) %>%
    inner_join(blnd_df_lineup_tot, by =c ("BTYPE", "TEAM", "OPP", "DAY")) %>%
    inner_join(atm_df_lineup_tot, by =c ("BTYPE", "TEAM", "OPP", "DAY")) %>%
    inner_join(sim_df_lineup_tot, by =c ("BTYPE", "TEAM", "OPP", "DAY")) %>%
    inner_join(comb_df_lineup_tot, by =c ("BTYPE", "TEAM", "OPP", "DAY"))
  
  master_df_lineup_tot <<- master_df_lineup_tot[, c(1:4, 5, 7, 9, 11, 13, 15, 6)]
  colnames(master_df_lineup_tot)[5:11] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB", "RES")

  #ML MASTER FRAME
  
  indy_df_lineup_ml <- key_df %>%
    filter(MTYPE == "INDY", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT, LINE) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  team_df_lineup_ml <- key_df %>%
    filter(MTYPE == "TEAM", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT, LINE) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  blnd_df_lineup_ml <- key_df %>%
    filter(MTYPE == "BLND", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT, LINE) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  atm_df_lineup_ml <- key_df %>%
    filter(MTYPE == "ATM", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT, LINE) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  sim_df_lineup_ml <- key_df %>%
    filter(MTYPE == "SIM", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT, LINE) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  comb_df_lineup_ml <- key_df %>%
    filter(MTYPE == "COMB", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, DAY, VALUE, RESULT, LINE) %>%
    inner_join(key_df_lineup, by = c("LOC", "TEAM", "OPP", "DAY"))
  
  master_df_lineup_ml <- indy_df_lineup_ml %>%
    inner_join(team_df_lineup_ml, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(blnd_df_lineup_ml, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(atm_df_lineup_ml, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(sim_df_lineup_ml, by =c ("LOC", "TEAM", "OPP", "DAY")) %>%
    inner_join(comb_df_lineup_ml, by =c ("LOC", "TEAM", "OPP", "DAY"))
  
  master_df_lineup_ml <<- master_df_lineup_ml[, c(1:4, 5, 8, 11, 14, 17, 20, 6, 7)]
  colnames(master_df_lineup_ml)[5:12] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB", "RES", "LINE")

}

bx_keyer_new <- function(xxxx) {
  
  keyer_df <- read_csv("Master Results Frame 5.csv")
  keyer_dates <- read_csv("datemaster.csv")
  
  key_max <- max(keyer_df$DAY)
  key_min <- min(keyer_df$DAY)
  
  if (xxxx == 1) {
    a <- key_min
  }
  else {
    a <- key_max
    dt_wh <- which(keyer_dates$No == key_max)
    max_m <- as.numeric(keyer_dates[dt_wh, 1])
    max_d <- as.numeric(keyer_dates[dt_wh, 2])
    max_y <- as.numeric(keyer_dates[dt_wh, 3])
  }
  g <- key_max
  s <- 0
  
  for (a in a:g) {
  
    dt_wh <- which(keyer_dates$No == a)
    act_m <- as.numeric(keyer_dates[dt_wh, 1])
    act_d <- as.numeric(keyer_dates[dt_wh, 2])
    act_y <- as.numeric(keyer_dates[dt_wh, 3])
    
    master_key_finder_set(a, a)
    master_key_finder(0)
    
    b <- 1
    h <- 6
    tvec <- c("I", "T", "B", "A", "S", "C")
    
    for (b in b:h) {
      
      res_w_ats <- as.numeric(combo_mx_slot_ats[b, 14])
      res_l_ats <- as.numeric(combo_mx_slot_ats[b, 15])
      res_u_ats <- as.numeric(combo_mx_slot_ats[b, 16])
      res_k_ats <- as.numeric(combo_mx_slot_ats[b, (7 + b)])
      res_t_ats <- paste(tvec[b], "ATS", sep = "")
      
      res_w_tot <- as.numeric(combo_mx_slot_tot[b, 14])
      res_l_tot <- as.numeric(combo_mx_slot_tot[b, 15])
      res_u_tot <- as.numeric(combo_mx_slot_tot[b, 16])
      res_k_tot <- as.numeric(combo_mx_slot_tot[b, (7 + b)])
      res_t_tot <- paste(tvec[b], "TOT", sep = "")
      
      res_w_ml <- as.numeric(combo_mx_slot_ml[b, 14])
      res_l_ml <- as.numeric(combo_mx_slot_ml[b, 15])
      res_u_ml <- as.numeric(combo_mx_slot_ml[b, 16])
      res_k_ml <- as.numeric(combo_mx_slot_ml[b, (7 + b)])
      res_t_ml <- paste(tvec[b], "ML", sep = "")

      bx_out_ats <- data.frame(act_m, act_d, act_y, res_t_ats, res_k_ats, res_u_ats, res_w_ats, res_l_ats)
      bx_out_tot <- data.frame(act_m, act_d, act_y, res_t_tot, res_k_tot, res_u_tot, res_w_tot, res_l_tot)
      bx_out_ml <- data.frame(act_m, act_d, act_y, res_t_ml, res_k_ml, res_u_ml, res_w_ml, res_l_ml)
      
      colnames(bx_out_ats) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
      colnames(bx_out_tot) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
      colnames(bx_out_ml) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
      
      bx_out_all <- rbind(bx_out_ats, bx_out_tot, bx_out_ml)
      
      if (s == 0) { 
        bx_out_master <- bx_out_all
        s <- s + 1
      }
      else {
        bx_out_master <- rbind(bx_out_master, bx_out_all)
      }
    }
  }
  
  if ((key_max - key_min) <= 14) { 
    bot14 <- 1
    top14 <- key_max
  }
  else {
    bot14 <- key_max - 13
    top14 <- key_max
  }
  
  master_key_finder_set(bot14, top14)
  master_key_finder(0)
  
  act_m <- 14
  act_d <- 14
  act_y <- 14
  
  b <- 1
  h <- 6
  tvec <- c("I", "T", "B", "A", "S", "C")
  
  for (b in b:h) {
    
    res_w_ats <- as.numeric(combo_mx_slot_ats[b, 14])
    res_l_ats <- as.numeric(combo_mx_slot_ats[b, 15])
    res_u_ats <- as.numeric(combo_mx_slot_ats[b, 16])
    res_k_ats <- as.numeric(combo_mx_slot_ats[b, (7 + b)])
    res_t_ats <- paste(tvec[b], "ATS", sep = "")
    
    res_w_tot <- as.numeric(combo_mx_slot_tot[b, 14])
    res_l_tot <- as.numeric(combo_mx_slot_tot[b, 15])
    res_u_tot <- as.numeric(combo_mx_slot_tot[b, 16])
    res_k_tot <- as.numeric(combo_mx_slot_tot[b, (7 + b)])
    res_t_tot <- paste(tvec[b], "TOT", sep = "")
    
    res_w_ml <- as.numeric(combo_mx_slot_ml[b, 14])
    res_l_ml <- as.numeric(combo_mx_slot_ml[b, 15])
    res_u_ml <- as.numeric(combo_mx_slot_ml[b, 16])
    res_k_ml <- as.numeric(combo_mx_slot_ml[b, (7 + b)])
    res_t_ml <- paste(tvec[b], "ML", sep = "")
    
    bx_out_ats <- data.frame(act_m, act_d, act_y, res_t_ats, res_k_ats, res_u_ats, res_w_ats, res_l_ats)
    bx_out_tot <- data.frame(act_m, act_d, act_y, res_t_tot, res_k_tot, res_u_tot, res_w_tot, res_l_tot)
    bx_out_ml <- data.frame(act_m, act_d, act_y, res_t_ml, res_k_ml, res_u_ml, res_w_ml, res_l_ml)
    
    colnames(bx_out_ats) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
    colnames(bx_out_tot) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
    colnames(bx_out_ml) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
    
    bx_out_all <- rbind(bx_out_ats, bx_out_tot, bx_out_ml)
    
    if (s == 0) { 
      bx_out_master <- bx_out_all
      s <- s + 1
    }
    else {
      bx_out_master <- rbind(bx_out_master, bx_out_all)
    }
  }
  
  master_key_finder_set(key_min, key_max)
  master_key_finder(0)
  
  act_m <- 0
  act_d <- 0
  act_y <- 0
  
  b <- 1
  h <- 6
  tvec <- c("I", "T", "B", "A", "S", "C")
  
  for (b in b:h) {
    
    res_w_ats <- as.numeric(combo_mx_slot_ats[b, 14])
    res_l_ats <- as.numeric(combo_mx_slot_ats[b, 15])
    res_u_ats <- as.numeric(combo_mx_slot_ats[b, 16])
    res_k_ats <- as.numeric(combo_mx_slot_ats[b, (7 + b)])
    res_t_ats <- paste(tvec[b], "ATS", sep = "")
    
    res_w_tot <- as.numeric(combo_mx_slot_tot[b, 14])
    res_l_tot <- as.numeric(combo_mx_slot_tot[b, 15])
    res_u_tot <- as.numeric(combo_mx_slot_tot[b, 16])
    res_k_tot <- as.numeric(combo_mx_slot_tot[b, (7 + b)])
    res_t_tot <- paste(tvec[b], "TOT", sep = "")
    
    res_w_ml <- as.numeric(combo_mx_slot_ml[b, 14])
    res_l_ml <- as.numeric(combo_mx_slot_ml[b, 15])
    res_u_ml <- as.numeric(combo_mx_slot_ml[b, 16])
    res_k_ml <- as.numeric(combo_mx_slot_ml[b, (7 + b)])
    res_t_ml <- paste(tvec[b], "ML", sep = "")
    
    bx_out_ats <- data.frame(act_m, act_d, act_y, res_t_ats, res_k_ats, res_u_ats, res_w_ats, res_l_ats)
    bx_out_tot <- data.frame(act_m, act_d, act_y, res_t_tot, res_k_tot, res_u_tot, res_w_tot, res_l_tot)
    bx_out_ml <- data.frame(act_m, act_d, act_y, res_t_ml, res_k_ml, res_u_ml, res_w_ml, res_l_ml)
    
    colnames(bx_out_ats) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
    colnames(bx_out_tot) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
    colnames(bx_out_ml) <- c("Month", "Day",	"Year",	"Type",	"Key", "Unit", "Win",	"Loss")
    
    bx_out_all <- rbind(bx_out_ats, bx_out_tot, bx_out_ml)
    
    if (s == 0) { 
      bx_out_master <- bx_out_all
      s <- s + 1
    }
    else {
      bx_out_master <- rbind(bx_out_master, bx_out_all)
    }
  }
  
  if (xxxx == 1) { write_csv(bx_out_master, "bxkey5.csv") }
  
  else {
    
    bx_out_master1 <- read_csv("bxkey5.csv") %>%
      filter(Month != 0) %>%
      filter(Month != 14)
    
    bx_which <- which(bx_out_master1$Month == max_m & bx_out_master1$Day == max_d & bx_out_master1$Year == max_y)
    bx_which <- bx_which * -1
    bx_out_master1 <- bx_out_master1[bx_which, ]
    
    bx_out_master2 <- rbind(bx_out_master1, bx_out_master)
    write_csv(bx_out_master2, "bxkey5.csv")
    
    
  }
}

master_key_finder <- function(xxxx) {
  
  inf_which <<- which(master_df_lineup_ml[, 11] == "Inf")
  print(inf_which)
  if (length(inf_which) > 0) { 
    inf_which <- inf_which * -1
    master_df_lineup_ml <<- master_df_lineup_ml[inf_which, ]
  }
  
  combo_mx <- read_csv("keycombomx.csv")
  
  #Spread Key Finder

  combo_mx_slot_ats <<- combo_mx
  combo_mx_slot_ats[, 8:19] <<- 0
  combo_mx_slot_ats[19] <<- c(1:nrow(combo_mx))
  combo_ats_mx <- as.matrix(master_df_lineup_ats[, 5:11])
  col_name <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  a <- 1
  g <- nrow(combo_mx)
  
  for (a in a:g) {
    
    combo_ct <- as.numeric(combo_mx[a, 1])
    
    b <- 1
    h <- combo_ct
    
    for (b in b:h) {
      
      combo_name <- as.character(combo_mx[a, (b + 1)])
      combo_che <- which(col_name == combo_name)
      
      if (b == 1) { combo_choice <- combo_che }
      else { combo_choice <- c(combo_choice, combo_che) }
      
      combo_slot <- which(col_name == combo_name)
      
      combo_g0 <- which(combo_ats_mx[, combo_slot] > 0)
      
      
      if (b == 1) { combo_intersect <- combo_g0 }
      else { combo_intersect <- intersect(combo_intersect, combo_g0) }
     
      
    }
    
    combo_subset <- combo_ats_mx[combo_intersect, ]
    if (length(combo_subset) <= 0) {
      print("HI")
    }
    
    #Roll to Find Top Key
    
    
    combo_countdown <- h
    
    
    key_slot <- rep(0, 6)
    
    while (combo_countdown > 0) {
    
      b <- 1
      h <- length(combo_choice)
      
      for (b in b:h) {
        
        x <- combo_choice[b]
        cl <- class(combo_subset)
        if (cl == "numeric") { combo_subset <- matrix(combo_subset, nrow = 1, ncol = 7) }
        combo_ordered <- combo_subset[order(combo_subset[, x], decreasing = TRUE),]
        
        if (nrow(combo_subset) == 1) {
          combo_ordered <- matrix(combo_ordered, nrow = 1, ncol = 7)
        }
        
        pl = class(combo_ordered)
        if (cl == "numeric") { combo_ordered <- matrix(combo_ordered, nrow = 1, ncol = 7) }
        
        c <- 1
        i <- nrow(combo_ordered)
        roll_ <- 0
        
        for (c in c:i) {
          
          roll_ <- roll_ + combo_ordered[c, 7]
          val_ <- combo_ordered[c, x]
          if (c == 1) { 
            roll_vec <- roll_ 
            val_vec <- val_
          }
          else {
            roll_vec <- c(roll_vec, roll_)
            val_vec <- c(val_vec, val_)
          }
        }
        
        peak_roll_w <- which.max(roll_vec)
        peak_roll_w <- peak_roll_w[[1]]
        peak_roll <- roll_vec[peak_roll_w]
        peak_val <- val_vec[peak_roll_w]
        
        if (b == 1) { 
          whole_peak_roll <- peak_roll 
          whole_peak_val <- peak_val
        }
        else {
          whole_peak_roll <- c(whole_peak_roll, peak_roll)
          whole_peak_val <- c(whole_peak_val, peak_val)
        }
      }
      
      whole_peak_roll_w <- which.max(whole_peak_roll)
      whole_peak_roll_w <- whole_peak_roll_w[[1]]
      whole_peak_val_w <- whole_peak_val[whole_peak_roll_w]
      whole_peak_choice <- combo_choice[whole_peak_roll_w]
      
      whole_peak_val_g <- whole_peak_val[whole_peak_roll_w]
      
      combo_subset_w <- which(combo_subset[, whole_peak_choice] >= whole_peak_val_g)
      if (length(combo_subset_w) == 1) {
        combo_subset <- matrix(combo_subset[combo_subset_w, ], nrow = 1, ncol = 7)
      }
      else {
        combo_subset <- combo_subset[combo_subset_w, ]
      }
      combo_countdown <- combo_countdown - 1
      combo_choice_f <- which(combo_choice == whole_peak_choice) * -1
      combo_choice <- combo_choice[combo_choice_f]
      
      key_slot[whole_peak_choice] <- whole_peak_val_g
      
    }
  
    
    combo_wins <- length(which(combo_subset[, 7] > 0))
    combo_loss <- length(which(combo_subset[, 7] < 0))
    combo_unit <- sum(combo_subset[, 7])
    combo_slotter <- 8 + (combo_ct - 1)

    
    combo_mx_slot_ats[a, 8:13] <<- key_slot
    combo_mx_slot_ats[a, 14] <<- combo_wins
    combo_mx_slot_ats[a, 15] <<- combo_loss
    combo_mx_slot_ats[a, 16] <<- combo_unit
    combo_mx_slot_ats[a, 17] <<- combo_unit / (combo_wins + combo_loss)
    combo_mx_slot_ats[a, 18] <<- combo_unit * combo_unit / (combo_wins + combo_loss)
    colnames(combo_mx_slot_ats)[8:18] <<- c("VI", "VT", "VB", "VA", "VS", "VC", "W", "L", "U", "ROI", "INDEX")
    
  }
  
  
  #TotalKey Finder
  print("TOTALS!")
  combo_mx_slot_tot <<- combo_mx
  combo_mx_slot_tot[, 8:19] <<- 0
  combo_mx_slot_tot[19] <<- c(1:nrow(combo_mx))
  combo_tot_mx <- as.matrix(master_df_lineup_tot[, 5:11])
  col_name <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  a <- 1
  g <- nrow(combo_mx)
  
  for (a in a:g) {
    
    combo_ct <- as.numeric(combo_mx[a, 1])
    
    b <- 1
    h <- combo_ct
    
    for (b in b:h) {
      
      combo_name <- as.character(combo_mx[a, (b + 1)])
      combo_che <- which(col_name == combo_name)
      
      if (b == 1) { combo_choice <- combo_che }
      else { combo_choice <- c(combo_choice, combo_che) }
      
      combo_slot <- which(col_name == combo_name)
      
      combo_g0 <- which(combo_tot_mx[, combo_slot] > 0)
      
      
      if (b == 1) { combo_intersect <- combo_g0 }
      else { combo_intersect <- intersect(combo_intersect, combo_g0) }
      
      
    }
    
    combo_subset <- combo_tot_mx[combo_intersect, ]
    if (length(combo_subset) <= 0) {
      print("HI")
    }
    
    #Roll to Find Top Key
    
    
    combo_countdown <- h
    
    
    key_slot <- rep(0, 6)
    
    while (combo_countdown > 0) {
      
      b <- 1
      h <- length(combo_choice)
      
      for (b in b:h) {
        
        x <- combo_choice[b]
        cl <- class(combo_subset)
        if (cl == "numeric") { combo_subset <- matrix(combo_subset, nrow = 1, ncol = 7) }
        combo_ordered <- combo_subset[order(combo_subset[, x], decreasing = TRUE),]
        
        if (nrow(combo_subset) == 1) {
          combo_ordered <- matrix(combo_ordered, nrow = 1, ncol = 7)
        }
        
        pl = class(combo_ordered)
        if (cl == "numeric") { combo_ordered <- matrix(combo_ordered, nrow = 1, ncol = 7) }
        
        c <- 1
        i <- nrow(combo_ordered)
        roll_ <- 0
        
        for (c in c:i) {
          
          roll_ <- roll_ + combo_ordered[c, 7]
          val_ <- combo_ordered[c, x]
          if (c == 1) { 
            roll_vec <- roll_ 
            val_vec <- val_
          }
          else {
            roll_vec <- c(roll_vec, roll_)
            val_vec <- c(val_vec, val_)
          }
        }
        
        peak_roll_w <- which.max(roll_vec)
        peak_roll_w <- peak_roll_w[[1]]
        peak_roll <- roll_vec[peak_roll_w]
        peak_val <- val_vec[peak_roll_w]
        
        if (b == 1) { 
          whole_peak_roll <- peak_roll 
          whole_peak_val <- peak_val
        }
        else {
          whole_peak_roll <- c(whole_peak_roll, peak_roll)
          whole_peak_val <- c(whole_peak_val, peak_val)
        }
      }
      
      whole_peak_roll_w <- which.max(whole_peak_roll)
      whole_peak_roll_w <- whole_peak_roll_w[[1]]
      whole_peak_val_w <- whole_peak_val[whole_peak_roll_w]
      whole_peak_choice <- combo_choice[whole_peak_roll_w]
      
      whole_peak_val_g <- whole_peak_val[whole_peak_roll_w]
      
      combo_subset_w <- which(combo_subset[, whole_peak_choice] >= whole_peak_val_g)
      if (length(combo_subset_w) == 1) {
        combo_subset <- matrix(combo_subset[combo_subset_w, ], nrow = 1, ncol = 7)
      }
      else {
        combo_subset <- combo_subset[combo_subset_w, ]
      }
      combo_countdown <- combo_countdown - 1
      combo_choice_f <- which(combo_choice == whole_peak_choice) * -1
      combo_choice <- combo_choice[combo_choice_f]
      
      key_slot[whole_peak_choice] <- whole_peak_val_g
      
    }
    
    
    combo_wins <- length(which(combo_subset[, 7] > 0))
    combo_loss <- length(which(combo_subset[, 7] < 0))
    combo_unit <- sum(combo_subset[, 7])
    combo_slotter <- 8 + (combo_ct - 1)
    
    
    combo_mx_slot_tot[a, 8:13] <<- key_slot
    combo_mx_slot_tot[a, 14] <<- combo_wins
    combo_mx_slot_tot[a, 15] <<- combo_loss
    combo_mx_slot_tot[a, 16] <<- combo_unit
    combo_mx_slot_tot[a, 17] <<- combo_unit / (combo_wins + combo_loss)
    combo_mx_slot_tot[a, 18] <<- combo_unit * combo_unit / (combo_wins + combo_loss)
    colnames(combo_mx_slot_tot)[8:18] <<- c("VI", "VT", "VB", "VA", "VS", "VC", "W", "L", "U", "ROI", "INDEX")
    
  }
  
  
  #MLKey Finder
  
  print("MONEYLINES!!")
  combo_mx_slot_ml <<- combo_mx
  combo_mx_slot_ml[, 8:18] <<- 0
  combo_mx_slot_ml[, 8:19] <<- 0
  combo_mx_slot_ml[19] <<- c(1:nrow(combo_mx))
  combo_ml_mx <- as.matrix(master_df_lineup_ml[, 5:11])
  col_name <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  a <- 1
  g <- nrow(combo_mx)
  
  for (a in a:g) {
    
    combo_ct <- as.numeric(combo_mx[a, 1])
    
    b <- 1
    h <- combo_ct
    
    for (b in b:h) {
      
      combo_name <- as.character(combo_mx[a, (b + 1)])
      combo_che <- which(col_name == combo_name)
      
      if (b == 1) { combo_choice <- combo_che }
      else { combo_choice <- c(combo_choice, combo_che) }
      
      combo_slot <- which(col_name == combo_name)
      
      combo_g0 <- which(combo_ml_mx[, combo_slot] > 0)
      
      
      if (b == 1) { combo_intersect <- combo_g0 }
      else { combo_intersect <- intersect(combo_intersect, combo_g0) }
      
      
    }
    
    combo_subset <- combo_ml_mx[combo_intersect, ]

    #Roll to Find Top Key
    
    
    combo_countdown <- h
    
    
    key_slot <- rep(0, 6)
    
    while (combo_countdown > 0) {
      
      b <- 1
      h <- length(combo_choice)
      
      for (b in b:h) {
        
        
        x <- combo_choice[b]
        cl <- class(combo_subset)
        if (cl == "numeric") { combo_subset <- matrix(combo_subset, nrow = 1, ncol = 7) }
        combo_ordered <- combo_subset[order(combo_subset[, x], decreasing = TRUE),]
        
        if (nrow(combo_subset) == 1) {
          combo_ordered <- matrix(combo_ordered, nrow = 1, ncol = 7)
        }
        
        #print(combo_ordered)
        
        pl = class(combo_ordered)
        if (cl == "numeric") { combo_ordered <- matrix(combo_ordered, nrow = 1, ncol = 7) }

        c <- 1
        i <- nrow(combo_ordered)
        if (i == 0) { combo_ordered <- matrix(0, nrow = 1, ncol = 7) }
        roll_ <- 0
        
        for (c in c:i) {
          
          roll_ <- roll_ + combo_ordered[c, 7]
          val_ <- combo_ordered[c, x]
          if (c == 1) { 
            roll_vec <- roll_ 
            val_vec <- val_
          }
          else {
            roll_vec <- c(roll_vec, roll_)
            val_vec <- c(val_vec, val_)
          }
        }
        
        peak_roll_w <- which.max(roll_vec)
        peak_roll_w <- peak_roll_w[[1]]
        peak_roll <- roll_vec[peak_roll_w]
        peak_val <- val_vec[peak_roll_w]
        
        if (b == 1) { 
          whole_peak_roll <- peak_roll 
          whole_peak_val <- peak_val
        }
        else {
          whole_peak_roll <- c(whole_peak_roll, peak_roll)
          whole_peak_val <- c(whole_peak_val, peak_val)
        }
      }
      
      whole_peak_roll_w <- which.max(whole_peak_roll)
      whole_peak_roll_w <- whole_peak_roll_w[[1]]
      whole_peak_val_w <- whole_peak_val[whole_peak_roll_w]
      whole_peak_choice <- combo_choice[whole_peak_roll_w]
      
      whole_peak_val_g <- whole_peak_val[whole_peak_roll_w]
      
      combo_subset_w <- which(combo_subset[, whole_peak_choice] >= whole_peak_val_g)
      if (length(combo_subset_w) == 1) {
        combo_subset <- matrix(combo_subset[combo_subset_w, ], nrow = 1, ncol = 7)
      }
      else {
        combo_subset <- combo_subset[combo_subset_w, ]
      }
      combo_countdown <- combo_countdown - 1
      combo_choice_f <- which(combo_choice == whole_peak_choice) * -1
      combo_choice <- combo_choice[combo_choice_f]
      
      key_slot[whole_peak_choice] <- whole_peak_val_g
      
    }
    
    
    combo_wins <- length(which(combo_subset[, 7] > 0))
    combo_loss <- length(which(combo_subset[, 7] < 0))
    combo_unit <- sum(combo_subset[, 7])
    combo_slotter <- 8 + (combo_ct - 1)
    
    
    combo_mx_slot_ml[a, 8:13] <<- key_slot
    combo_mx_slot_ml[a, 14] <<- combo_wins
    combo_mx_slot_ml[a, 15] <<- combo_loss
    combo_mx_slot_ml[a, 16] <<- combo_unit
    combo_mx_slot_ml[a, 17] <<- combo_unit / (combo_wins + combo_loss)
    combo_mx_slot_ml[a, 18] <<- combo_unit * combo_unit / (combo_wins + combo_loss)
    colnames(combo_mx_slot_ml)[8:18] <<- c("VI", "VT", "VB", "VA", "VS", "VC", "W", "L", "U", "ROI", "INDEX")
    
  }
  
  if (xxxx == 1) { 
    View(combo_mx_slot_ats)
    View(combo_mx_slot_tot)
    View(combo_mx_slot_ml)
  }
}

master_key_stripper_set <- function(xxxx) {
  
  key_df <- bet_value_frame5
  
  key_st_lineup <- key_df %>%
    group_by(LOC, TEAM, OPP) %>%
    summarize(ct = n()) %>%
    select(LOC, TEAM, OPP) %>%
    filter(LOC == "A" | LOC == "H" | LOC == "N")
  
  key_st_tineup <- key_df %>%
    filter(BTYPE == "OVER" | BTYPE == "UNDER") %>%
    group_by(BTYPE, TEAM, OPP) %>%
    summarize(ct = n()) %>%
    select(BTYPE, TEAM, OPP)
  
  #ATS
  
  indy_st_lineup_ats <- key_df %>%
    filter(MTYPE == "INDY", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, LINE, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  team_st_lineup_ats <- key_df %>%
    filter(MTYPE == "TEAM", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  blnd_st_lineup_ats <- key_df %>%
    filter(MTYPE == "BLND", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  atm_st_lineup_ats <- key_df %>%
    filter(MTYPE == "ATM", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  sim_st_lineup_ats <- key_df %>%
    filter(MTYPE == "SIM", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  comb_st_lineup_ats <- key_df %>%
    filter(MTYPE == "COMB", BTYPE == "SPREAD") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  master_st_lineup_ats <<- indy_st_lineup_ats %>%
    inner_join(team_st_lineup_ats, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(blnd_st_lineup_ats, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(atm_st_lineup_ats, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(sim_st_lineup_ats, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(comb_st_lineup_ats, by =c ("LOC", "TEAM", "OPP"))
  
  colnames(master_st_lineup_ats)[5:10] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  #Total
  
  indy_st_lineup_tot <- key_df %>%
    filter(MTYPE == "INDY") %>%
    filter(BTYPE == "OVER" | BTYPE == "UNDER") %>%
    select(BTYPE, TEAM, OPP, LINE, VALUE) %>%
    inner_join(key_st_tineup, by = c("BTYPE", "TEAM", "OPP"))
  
  team_st_lineup_tot <- key_df %>%
    filter(MTYPE == "TEAM") %>%
    filter(BTYPE == "OVER" | BTYPE == "UNDER") %>%
    select(BTYPE, TEAM, OPP, VALUE) %>%
    inner_join(key_st_tineup, by = c("BTYPE", "TEAM", "OPP"))
  
  blnd_st_lineup_tot <- key_df %>%
    filter(MTYPE == "BLND") %>%
    filter(BTYPE == "OVER" | BTYPE == "UNDER") %>%
    select(BTYPE, TEAM, OPP, VALUE) %>%
    inner_join(key_st_tineup, by = c("BTYPE", "TEAM", "OPP"))
  
  atm_st_lineup_tot <- key_df %>%
    filter(MTYPE == "ATM") %>%
    filter(BTYPE == "OVER" | BTYPE == "UNDER") %>%
    select(BTYPE, TEAM, OPP, VALUE) %>%
    inner_join(key_st_tineup, by = c("BTYPE", "TEAM", "OPP"))
  
  sim_st_lineup_tot <- key_df %>%
    filter(MTYPE == "SIM") %>%
    filter(BTYPE == "OVER" | BTYPE == "UNDER") %>%
    select(BTYPE, TEAM, OPP, VALUE) %>%
    inner_join(key_st_tineup, by = c("BTYPE", "TEAM", "OPP"))
  
  comb_st_lineup_tot <- key_df %>%
    filter(MTYPE == "COMB") %>%
    filter(BTYPE == "OVER" | BTYPE == "UNDER") %>%
    select(BTYPE, TEAM, OPP, VALUE) %>%
    inner_join(key_st_tineup, by = c("BTYPE", "TEAM", "OPP"))
  
  master_st_lineup_tot <<- indy_st_lineup_tot %>%
    inner_join(team_st_lineup_tot, by =c ("BTYPE", "TEAM", "OPP")) %>%
    inner_join(blnd_st_lineup_tot, by =c ("BTYPE", "TEAM", "OPP")) %>%
    inner_join(atm_st_lineup_tot, by =c ("BTYPE", "TEAM", "OPP")) %>%
    inner_join(sim_st_lineup_tot, by =c ("BTYPE", "TEAM", "OPP")) %>%
    inner_join(comb_st_lineup_tot, by =c ("BTYPE", "TEAM", "OPP"))
  
  colnames(master_st_lineup_tot)[5:10] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  
  #ML
  
  indy_st_lineup_ml <- key_df %>%
    filter(MTYPE == "INDY", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, LINE, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  team_st_lineup_ml <- key_df %>%
    filter(MTYPE == "TEAM", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  blnd_st_lineup_ml <- key_df %>%
    filter(MTYPE == "BLND", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  atm_st_lineup_ml <- key_df %>%
    filter(MTYPE == "ATM", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  sim_st_lineup_ml <- key_df %>%
    filter(MTYPE == "SIM", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  comb_st_lineup_ml <- key_df %>%
    filter(MTYPE == "COMB", BTYPE == "ML") %>%
    select(LOC, TEAM, OPP, VALUE) %>%
    inner_join(key_st_lineup, by = c("LOC", "TEAM", "OPP"))
  
  master_st_lineup_ml <<- indy_st_lineup_ml %>%
    inner_join(team_st_lineup_ml, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(blnd_st_lineup_ml, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(atm_st_lineup_ml, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(sim_st_lineup_ml, by =c ("LOC", "TEAM", "OPP")) %>%
    inner_join(comb_st_lineup_ml, by =c ("LOC", "TEAM", "OPP"))
  
  colnames(master_st_lineup_ml)[5:10] <<- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")

}

new_key_stripper2 <- function(m, d, y) {
  
  blender_ats_key <- ats_key_vector[3]
  rf_ats_key <- ats_key_vector[5]
  indy_ats_key <- ats_key_vector[1]
  team_ats_key <- ats_key_vector[2]
  atm_ats_key <- ats_key_vector[4]
  comb_ats_key <- ats_key_vector[6]
  
  blender_tot_key <- tot_key_vector[3]
  rf_tot_key <- tot_key_vector[5]
  indy_tot_key <- tot_key_vector[1]
  team_tot_key <- tot_key_vector[2]
  atm_tot_key <- tot_key_vector[4]
  comb_tot_key <- tot_key_vector[6]
  
  blender_ml_key <- ml_key_vector[3]
  rf_ml_key <- ml_key_vector[5]
  indy_ml_key <- ml_key_vector[1]
  team_ml_key <- ml_key_vector[2]
  atm_ml_key <- ml_key_vector[4]
  comb_ml_key <- ml_key_vector[6]
  
  ats_strip <- master_st_lineup_ats %>%
    filter(BLND >= blender_ats_key & SIM >= rf_ats_key & INDY >= indy_ats_key & TEAMC >= team_ats_key & ATM >= atm_ats_key & COMB >= comb_ats_key)
  
  tot_strip <- master_st_lineup_tot %>%
    filter(BLND >= blender_tot_key & SIM >= rf_tot_key & INDY >= indy_tot_key & TEAMC >= team_tot_key & ATM >= atm_tot_key & COMB >= comb_tot_key)
  
  print(tot_strip)
  
  colnames(ats_strip)[1] <- "BET TYPE"
  colnames(tot_strip)[1] <- "BET TYPE"
  
  print(ats_strip)
  print(tot_strip)
  
  if (nrow(ats_strip) > 0) { ats_strip[, 1] <- "ATS" }
  
  blender_ml_frame <- master_st_lineup_ml
  
  a <- 1
  g <- nrow(blender_ml_frame)
  for (a in a:g) {
    
    blender_ml_frame[a, 11] <- as.numeric(moneyline_conversion(blender_ml_frame[a, 4]))
    
  }
  
  colnames(blender_ml_frame)[11] <- "MLODDS"
  
  blender_ml_frame <- blender_ml_frame %>%
    mutate(PAYOUT = 1 * ((1 - MLODDS) / MLODDS)) %>%
    mutate(BLNDODDS = MLODDS + BLND) %>%
    mutate(SIMODDS = MLODDS + SIM) %>%
    mutate(INDYODDS = MLODDS + INDY) %>%
    mutate(TEAMCODDS = MLODDS + TEAMC) %>%
    mutate(ATMODDS = MLODDS + ATM) %>%
    mutate(COMBODDS = MLODDS + COMB) %>%
    mutate(BLNDEV = (BLNDODDS * PAYOUT) + (-1 * (1 - BLNDODDS))) %>%
    mutate(SIMEV = (SIMODDS * PAYOUT) + (-1 * (1 - SIMODDS))) %>%
    mutate(INDYEV = (INDYODDS * PAYOUT) + (-1 * (1 - INDYODDS))) %>%
    mutate(TEAMCEV = (TEAMCODDS * PAYOUT) + (-1 * (1 - TEAMCODDS))) %>%
    mutate(ATMEV = (ATMODDS * PAYOUT) + (-1 * (1 - ATMODDS))) %>%
    mutate(COMBEV = (COMBODDS * PAYOUT) + (-1 * (1 - COMBODDS)))
  
  
  ml_strip <- blender_ml_frame %>%
    filter(BLNDEV >= blender_ml_key & SIMEV >= rf_ml_key & INDYEV >= indy_ml_key & TEAMCEV >= team_ml_key & 
             ATMEV >= atm_ml_key & COMBEV >= comb_ml_key) %>%
    filter(LINE >= -400 & LINE <= 950)
  
  print(ml_strip)
  
  ml_strip <- ml_strip[, 1:10]
  
  colnames(ml_strip)[1] <- "BET TYPE"
  
  if (nrow(ml_strip) > 0) { ml_strip[, 1] <- "ML" }
  
  all_out_bind <- rbind(ats_strip, tot_strip, ml_strip)
  
  View(all_out_bind)
  all_out_bind[, 11] <- m
  all_out_bind[, 12] <- d
  all_out_bind[, 13] <- y
  colnames(all_out_bind)[11:13] <- c("MONTH", "DAY", "YEAR")
  write_csv(all_out_bind, "Todays Plays.csv")

  View(all_out_bind)
  
}

new_key_stripper <- function(m, d, y) {
  
  blender_ats_strip <- master_st_lineup_ats %>%
    filter(BLND >= blender_ats_key)
  
  rf_ats_strip <- master_st_lineup_ats %>%
    filter(SIM >= rf_ats_key)
  
  blender_tot_strip <- master_st_lineup_tot %>%
    filter(BLND >= blender_tot_key)
  
  rf_tot_strip <- master_st_lineup_tot %>%
    filter(SIM >= rf_tot_key)
  
  colnames(blender_ats_strip)[1] <- "BTYPE"
  colnames(rf_ats_strip)[1] <- "BTYPE"
  colnames(blender_tot_strip)[1] <- "BTYPE"
  colnames(rf_tot_strip)[1] <- "BTYPE"
  
  print(blender_ats_strip)
  print(rf_ats_strip)
  print(blender_tot_strip)
  print(rf_tot_strip)
  
  if (nrow(blender_ats_strip) > 0) { blender_ats_strip[, 1] <- "ATS" }
  if (nrow(rf_ats_strip) > 0) { rf_ats_strip[, 1] <- "ATS" }

  ats_tot_bind <- rbind(blender_ats_strip, rf_ats_strip, blender_tot_strip, rf_tot_strip)
  
  blender_ml_frame <- master_st_lineup_ml
  
  a <- 1
  g <- nrow(blender_ml_frame)
  for (a in a:g) {
    
    blender_ml_frame[a, 11] <- as.numeric(moneyline_conversion(blender_ml_frame[a, 4]))
    
  }
  
  colnames(blender_ml_frame)[11] <- "MLODDS"
  
  blender_ml_frame <- blender_ml_frame %>%
    mutate(PAYOUT = 1 * ((1 - MLODDS) / MLODDS)) %>%
    mutate(BLNDODDS = MLODDS + BLND) %>%
    mutate(SIMODDS = MLODDS + SIM) %>%
    mutate(BLNDEV = (BLNDODDS * PAYOUT) + (-1 * (1 - BLNDODDS))) %>%
    mutate(SIMEV = (SIMODDS * PAYOUT) + (-1 * (1 - SIMODDS)))
  
  blender_ml_strip <- blender_ml_frame %>%
    filter(BLNDEV >= blender_ml_key)
  
  rf_ml_strip <- blender_ml_frame %>%
    filter(SIMEV >= rf_ml_key)
  
  colnames(blender_ml_strip)[1] <- "BTYPE"
  colnames(rf_ml_strip)[1] <- "BTYPE"
  
  ml_tot_bind <- rbind(blender_ml_strip, rf_ml_strip)
  
  if (nrow(ml_tot_bind) > 0) { ml_tot_bind[, 1] <- "ML" }

  
  ml_tot_bind <- ml_tot_bind[, 1:10] %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(ct = n()) %>%
    ungroup()
  ml_tot_bind <- ml_tot_bind[, 1:10]
  
  ats_tot_bind <- ats_tot_bind[, 1:10] %>%
    group_by(BTYPE, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
    summarize(ct = n()) %>%
    ungroup()
  
  ats_tot_bind <- ats_tot_bind[, 1:10]
  
  all_out_bind <- rbind(ats_tot_bind, ml_tot_bind)
  
  View(all_out_bind)
  all_out_bind[, 11] <- m
  all_out_bind[, 12] <- d
  all_out_bind[, 13] <- y
  colnames(all_out_bind)[11:13] <- c("MONTH", "DAY", "YEAR")
  write_csv(all_out_bind, "Todays Plays.csv")
  

  View(all_out_bind)
  
}

master_key_stripper <- function(m, d, y) {
  
  #ATS
  
  ats_key <- todays_keys[1]
  
  ats_key_line <- combo_mx_slot_ats[ats_key, ]
  
  key_slots <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  key_cols <- c(5, 6, 7, 8, 9, 10)
  
  key_depth <- as.numeric(ats_key_line[1, 1])
  a <- 1
  g <- key_depth
  for (a in a:g) {
    
    m_line <- as.character(ats_key_line[1, a + 1])
    if (m_line == "INDY") { v_line <- as.numeric(ats_key_line[1, 8]) }
    if (m_line == "TEAMC") { v_line <- as.numeric(ats_key_line[1, 9]) }
    if (m_line == "BLND") { v_line <- as.numeric(ats_key_line[1, 10]) }
    if (m_line == "ATM") { v_line <- as.numeric(ats_key_line[1, 11]) }
    if (m_line == "SIM") { v_line <- as.numeric(ats_key_line[1, 12]) }
    if (m_line == "COMB") { v_line <- as.numeric(ats_key_line[1, 13]) }
    
    k_slot <- key_cols[which(key_slots == m_line)]
    
    if (a == 1) { 
      key_slot_vec <- k_slot 
      key_vals_vec <- v_line
    }
    else {
      key_slot_vec <- c(key_slot_vec, k_slot)
      key_vals_vec <- c(key_vals_vec, v_line)
    }
  }
  
  a <- 1
  g <- length(key_slot_vec)
  
  print(key_vals_vec)
  
  for (a in a:g) {
    
    cur_key_slot <- key_slot_vec[a]
    cur_key_val <- key_vals_vec[a]
    
    key_int <- which(master_st_lineup_ats[, cur_key_slot] >= cur_key_val)
    
    if (a == 1) { key_intersect <- key_int }
    else { key_intersect <- intersect(key_intersect, key_int) }
      
  }
  
  ats_plays <<- master_st_lineup_ats[key_intersect, ]
  ats_plays[, 1] <<- "ATS"
  colnames(ats_plays)[1] <<- "BET TYPE"
  
  
  #TOTALS
  
  tot_key <- todays_keys[2]
  
  tot_key_line <- combo_mx_slot_tot[tot_key, ]
  
  key_slots <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  key_cols <- c(5, 6, 7, 8, 9, 10)
  
  key_depth <- as.numeric(tot_key_line[1, 1])
  a <- 1
  g <- key_depth
  for (a in a:g) {
    
    m_line <- as.character(tot_key_line[1, a + 1])
    if (m_line == "INDY") { v_line <- as.numeric(tot_key_line[1, 8]) }
    if (m_line == "TEAMC") { v_line <- as.numeric(tot_key_line[1, 9]) }
    if (m_line == "BLND") { v_line <- as.numeric(tot_key_line[1, 10]) }
    if (m_line == "ATM") { v_line <- as.numeric(tot_key_line[1, 11]) }
    if (m_line == "SIM") { v_line <- as.numeric(tot_key_line[1, 12]) }
    if (m_line == "COMB") { v_line <- as.numeric(tot_key_line[1, 13]) }
    
    k_slot <- key_cols[which(key_slots == m_line)]
    
    if (a == 1) { 
      key_slot_vec <- k_slot 
      key_vals_vec <- v_line
    }
    else {
      key_slot_vec <- c(key_slot_vec, k_slot)
      key_vals_vec <- c(key_vals_vec, v_line)
    }
  }
  
  a <- 1
  g <- length(key_slot_vec)
  
  print(key_vals_vec)
  
  for (a in a:g) {
    
    cur_key_slot <- key_slot_vec[a]
    cur_key_val <- key_vals_vec[a]
    
    key_int <- which(master_st_lineup_tot[, cur_key_slot] >= cur_key_val)
    
    if (a == 1) { key_intersect <- key_int }
    else { key_intersect <- intersect(key_intersect, key_int) }
    
  }
  
  tot_plays <<- master_st_lineup_tot[key_intersect, ]
  colnames(tot_plays)[1] <<- "BET TYPE"
  
  #ML
  
  ml_key <- todays_keys[3]
  
  ml_key_line <- combo_mx_slot_ml[ml_key, ]
  
  key_slots <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  key_cols <- c(5, 6, 7, 8, 9, 10)
  
  key_depth <- as.numeric(ml_key_line[1, 1])
  a <- 1
  g <- key_depth
  for (a in a:g) {
    
    m_line <- as.character(ml_key_line[1, a + 1])
    if (m_line == "INDY") { v_line <- as.numeric(ml_key_line[1, 8]) }
    if (m_line == "TEAMC") { v_line <- as.numeric(ml_key_line[1, 9]) }
    if (m_line == "BLND") { v_line <- as.numeric(ml_key_line[1, 10]) }
    if (m_line == "ATM") { v_line <- as.numeric(ml_key_line[1, 11]) }
    if (m_line == "SIM") { v_line <- as.numeric(ml_key_line[1, 12]) }
    if (m_line == "COMB") { v_line <- as.numeric(ml_key_line[1, 13]) }
    
    k_slot <- key_cols[which(key_slots == m_line)]
    
    if (a == 1) { 
      key_slot_vec <- k_slot 
      key_vals_vec <- v_line
    }
    else {
      key_slot_vec <- c(key_slot_vec, k_slot)
      key_vals_vec <- c(key_vals_vec, v_line)
    }
  }
  
  a <- 1
  g <- length(key_slot_vec)
  
  print(key_vals_vec)
  
  for (a in a:g) {
    
    cur_key_slot <- key_slot_vec[a]
    cur_key_val <- key_vals_vec[a]
    
    key_int <- which(master_st_lineup_ml[, cur_key_slot] >= cur_key_val)
    
    if (a == 1) { key_intersect <- key_int }
    else { key_intersect <- intersect(key_intersect, key_int) }
    
  }
  
  ml_plays <<- master_st_lineup_ml[key_intersect, ]
  if (nrow(ml_plays) > 0) { ml_plays[, 1] <<- "ML" }
  colnames(ml_plays)[1] <<- "BET TYPE"
  
  todays_plays <<- rbind(ats_plays, tot_plays, ml_plays)
  todays_plays[, 11] <<- m
  todays_plays[, 12] <<- d
  todays_plays[, 13] <<- y
  colnames(todays_plays)[11:13] <<- c("MONTH", "DAY", "YEAR")
  write_csv(todays_plays, "Todays Plays.csv")
}

master_key_stripper2 <- function(m, d, y) {
  
  #ATS
  
  ats_key_frame <- combo_mx_slot_ats[ats_keys, ]
  
  key_slots <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  key_cols <- c(5, 6, 7, 8, 9, 10)
  
  z <- 1
  u <- length(ats_keys)
  ct <- 0
  
  for (z in z:u) {
  
    key_depth <- as.numeric(ats_key_frame[z, 1])
    key_index <- as.numeric(ats_key_frame[z, 18])
    a <- 1
    g <- key_depth
    for (a in a:g) {
      
      m_line <- as.character(ats_key_frame[z, a + 1])
      if (m_line == "INDY") { v_line <- as.numeric(ats_key_frame[z, 8]) }
      if (m_line == "TEAMC") { v_line <- as.numeric(ats_key_frame[z, 9]) }
      if (m_line == "BLND") { v_line <- as.numeric(ats_key_frame[z, 10]) }
      if (m_line == "ATM") { v_line <- as.numeric(ats_key_frame[z, 11]) }
      if (m_line == "SIM") { v_line <- as.numeric(ats_key_frame[z, 12]) }
      if (m_line == "COMB") { v_line <- as.numeric(ats_key_frame[z, 13]) }
      
      k_slot <- key_cols[which(key_slots == m_line)]
      
      if (a == 1) { 
        key_slot_vec <- k_slot 
        key_vals_vec <- v_line
      }
      else {
        key_slot_vec <- c(key_slot_vec, k_slot)
        key_vals_vec <- c(key_vals_vec, v_line)
      }
    }
    
    a <- 1
    g <- length(key_slot_vec)
    
    print(key_vals_vec)
    
    for (a in a:g) {
      
      cur_key_slot <- key_slot_vec[a]
      cur_key_val <- key_vals_vec[a]
      
      key_int <- which(master_st_lineup_ats[, cur_key_slot] >= cur_key_val)
      
      if (a == 1) { key_intersect <- key_int }
      else { key_intersect <- intersect(key_intersect, key_int) }
      
    }
    
    ats_plays <<- master_st_lineup_ats[key_intersect, ]
    if (nrow(ats_plays) > 0) { 
      ct <- ct + 1
      ats_plays[, 1] <<- "ATS"
      colnames(ats_plays)[1] <<- "BET TYPE"
      ats_plays[, 11] <<- key_index
      colnames(ats_plays)[11] <<- "INDEX"
      
      if (ct == 1) { whole_ats_plays <<- ats_plays }
      else { whole_ats_plays <<- rbind(ats_plays, whole_ats_plays) }
    }
  }
  
  if (nrow(whole_ats_plays) > 0) {
    whole_ats_plays <<- whole_ats_plays %>%
      group_by(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
      summarize(INDEX = max(INDEX))
  }
  
  
  #Totals
  
  tot_key_frame <- combo_mx_slot_tot[tot_keys, ]
  
  key_slots <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  key_cols <- c(5, 6, 7, 8, 9, 10)
  
  z <- 1
  u <- length(tot_keys)
  ct <- 0
  
  for (z in z:u) {
    
    key_depth <- as.numeric(tot_key_frame[z, 1])
    key_index <- as.numeric(tot_key_frame[z, 18])
    a <- 1
    g <- key_depth
    for (a in a:g) {
      
      m_line <- as.character(tot_key_frame[z, a + 1])
      if (m_line == "INDY") { v_line <- as.numeric(tot_key_frame[z, 8]) }
      if (m_line == "TEAMC") { v_line <- as.numeric(tot_key_frame[z, 9]) }
      if (m_line == "BLND") { v_line <- as.numeric(tot_key_frame[z, 10]) }
      if (m_line == "ATM") { v_line <- as.numeric(tot_key_frame[z, 11]) }
      if (m_line == "SIM") { v_line <- as.numeric(tot_key_frame[z, 12]) }
      if (m_line == "COMB") { v_line <- as.numeric(tot_key_frame[z, 13]) }
      
      k_slot <- key_cols[which(key_slots == m_line)]
      
      if (a == 1) { 
        key_slot_vec <- k_slot 
        key_vals_vec <- v_line
      }
      else {
        key_slot_vec <- c(key_slot_vec, k_slot)
        key_vals_vec <- c(key_vals_vec, v_line)
      }
    }
    
    a <- 1
    g <- length(key_slot_vec)
    
    print(key_vals_vec)
    
    for (a in a:g) {
      
      cur_key_slot <- key_slot_vec[a]
      cur_key_val <- key_vals_vec[a]
      
      key_int <- which(master_st_lineup_tot[, cur_key_slot] >= cur_key_val)
      
      if (a == 1) { key_intersect <- key_int }
      else { key_intersect <- intersect(key_intersect, key_int) }
      
    }
    
    tot_plays <<- master_st_lineup_tot[key_intersect, ]
    if (nrow(tot_plays) > 0) {
      ct <- ct + 1
      colnames(tot_plays)[1] <<- "BET TYPE"
      tot_plays[, 11] <<- key_index
      colnames(tot_plays)[11] <<- "INDEX"
      
      if (ct == 1) { whole_tot_plays <<- tot_plays }
      else { whole_tot_plays <<- rbind(tot_plays, whole_tot_plays) }
    }
  }
  
  if (nrow(whole_tot_plays) > 0) {
    whole_tot_plays <<- whole_tot_plays %>%
      group_by(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
      summarize(INDEX = max(INDEX))
  }
  
  
  #ML
  
  ml_key_frame <- combo_mx_slot_ml[ml_keys, ]
  
  key_slots <- c("INDY", "TEAMC", "BLND", "ATM", "SIM", "COMB")
  key_cols <- c(5, 6, 7, 8, 9, 10)
  
  z <- 1
  u <- length(ml_keys)
  ct <- 0
  
  for (z in z:u) {
    
    key_depth <- as.numeric(ml_key_frame[z, 1])
    key_index <- as.numeric(ml_key_frame[z, 18])
    a <- 1
    g <- key_depth
    for (a in a:g) {
      
      m_line <- as.character(ml_key_frame[z, a + 1])
      if (m_line == "INDY") { v_line <- as.numeric(ml_key_frame[z, 8]) }
      if (m_line == "TEAMC") { v_line <- as.numeric(ml_key_frame[z, 9]) }
      if (m_line == "BLND") { v_line <- as.numeric(ml_key_frame[z, 10]) }
      if (m_line == "ATM") { v_line <- as.numeric(ml_key_frame[z, 11]) }
      if (m_line == "SIM") { v_line <- as.numeric(ml_key_frame[z, 12]) }
      if (m_line == "COMB") { v_line <- as.numeric(ml_key_frame[z, 13]) }
      
      k_slot <- key_cols[which(key_slots == m_line)]
      
      if (a == 1) { 
        key_slot_vec <- k_slot 
        key_vals_vec <- v_line
      }
      else {
        key_slot_vec <- c(key_slot_vec, k_slot)
        key_vals_vec <- c(key_vals_vec, v_line)
      }
    }
    
    a <- 1
    g <- length(key_slot_vec)
    
    print(key_vals_vec)
    
    for (a in a:g) {
      
      cur_key_slot <- key_slot_vec[a]
      cur_key_val <- key_vals_vec[a]
      
      key_int <- which(master_st_lineup_ml[, cur_key_slot] >= cur_key_val)
      
      if (a == 1) { key_intersect <- key_int }
      else { key_intersect <- intersect(key_intersect, key_int) }
      
    }
    
    ml_plays <<- master_st_lineup_ml[key_intersect, ]
    if (nrow(ml_plays) > 0) {
      ct <- ct + 1
      ml_plays[, 1] <<- "ML"
      colnames(ml_plays)[1] <<- "BET TYPE"
      ml_plays[, 11] <<- key_index
      colnames(ml_plays)[11] <<- "INDEX"
      
      if (ct == 1) { whole_ml_plays <<- ml_plays }
      else { whole_ml_plays <<- rbind(ml_plays, whole_ml_plays) }
    }
  }
  
  if (nrow(whole_ml_plays) > 0) {
    whole_ml_plays <<- whole_ml_plays %>%
      group_by(`BET TYPE`, TEAM, OPP, LINE, INDY, TEAMC, BLND, ATM, SIM, COMB) %>%
      summarize(INDEX = max(INDEX))
  }
  
  whole_all_plays <<- rbind(whole_ats_plays, whole_tot_plays, whole_ml_plays)
  unit_floor <- max(whole_all_plays$INDEX)
  whole_all_plays[, 12] <<- whole_all_plays[, 11] / unit_floor
  write_csv(whole_all_plays, "Todays Plays Multi.csv")
}


zzz <- master_df_lineup_ats %>%
  gather(key = "METRIC", value = "VALUE", c(-LOC, -TEAM, -OPP, -DAY, -RES)) %>%
  mutate(MRES = ifelse(VALUE > 0, ifelse(RES >= 0, RES, -1.1), ifelse(RES <= 0, RES, 1))) %>%
  filter(VALUE < 3 & DAY >= 21) %>%
  group_by(METRIC) %>%
  summarize(MRES = sum(MRES), ct = n()) %>%
  mutate(EXP = ct * (-1/11)) %>%
  mutate(VSEXP = MRES - EXP) %>%
  filter(METRIC == "BLND")
  
  
zzz <- ats_log_2h %>%
  select(INDY, TEAMC, BLND, ATM, SIM, COMB, RES) %>%
  gather(key = "METRIC", value = "VALUE", c(-RES)) %>%
  mutate(MRES = ifelse(VALUE > 0, ifelse(RES >= 0, RES, -1.1), ifelse(RES <= 0, RES, 1))) %>%
  filter(VALUE > 2.5) %>%
  group_by(METRIC) %>%
  summarize(MRES = sum(MRES), ct = n())

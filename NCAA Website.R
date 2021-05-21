wbs_sched2 <- function(xxxx) {
  
  raw_sched_frame <<- team_game_by_game_raw[, -27] %>% ungroup()
  adv_sched_frame <<- adv_game_log_join[, c(9:41, 44:46)]
  adj_sched_frame <<- game_log_adj2[, c(3:32, 35:36)]
  tempo_sched_frame <<- tempo_adj2_frame[, 2:3]
  gid_sched_frame <<- team_game_by_game_raw[, 5:6] %>% ungroup()
  
  adj_sched_frame[, c(3, 5, 18, 20)] <<- adv_sched_frame[, c(4, 6, 20, 22)]
  
  colnames(tempo_sched_frame)[2] <<- "Tempo"
  
  all_sched_frame <<- cbind(raw_sched_frame, adv_sched_frame, adj_sched_frame, tempo_sched_frame, gid_sched_frame)
  all_sched_frame <<- all_sched_frame[, c(-112, -114)]
  
  
  a <- 44
  g <- 79
  
  print("HI")
  
  for (a in a:g) {
    
    c <- colnames(all_sched_frame)[a]
    c <- strsplit(c, "\\.")[[1]][1]
    colnames(all_sched_frame)[a] <<- paste("Raw", c, sep = "")
    
  }
  
  a <- 80
  g <- 112
  
  for (a in a:g) {
    
    c <- colnames(all_sched_frame)[a]
    c <- strsplit(c, "\\.")[[1]][1]
    colnames(all_sched_frame)[a] <<- paste("Adj", c, sep = "")
    
  }
  all_sched_frame <<- all_sched_frame[, c(-113)]
  
  all_sched_frame <<- all_sched_frame %>%
    mutate(RGScore = RawOE - RawDE) %>%
    mutate(AGScore = AdjOE - AdjDE)
  
  rank_vector <<- c(80:101, 105, 108:112, 114)
  colnames_vector <<- colnames(all_sched_frame)[rank_vector]
  desc_vector <<- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1)
  
  cur_cols <- ncol(all_sched_frame)
  
  rank_mx <- as.matrix(all_sched_frame[, rank_vector])
  
  a <- 1
  g <- length(rank_vector)
  
  for (a in a:g) {
    
     r_cur <- a
     d_cur <- desc_vector[a]
     
     cur_cols <- cur_cols + 1
     
     if (d_cur == 0) {
       
        loop_rank_vector <- rank(rank_mx[, r_cur])
       
     }
     
     else {
       loop_rank_vector <- rank(rank_mx[, r_cur]) * -1
       loop_rank_vector <- rank(loop_rank_vector)
       
     }
     
     all_sched_frame[, cur_cols] <<- loop_rank_vector
     
     pctile_vector <- 1 - ((loop_rank_vector - 1) / (length(loop_rank_vector) - 1))
     
     cur_colname <- colnames_vector[a]
     colnames(all_sched_frame)[cur_cols] <<- paste(cur_colname, "-RK", sep = "")
     
     cur_cols <- cur_cols + 1
     
     all_sched_frame[, cur_cols] <<- pctile_vector
     colnames(all_sched_frame)[cur_cols] <<- paste(cur_colname, "-PCT", sep = "")

  }
  
  all_sched_frame <<- all_sched_frame %>%
    arrange(`AGScore-RK`)
  
  all_sched_frame$DATE <<- mdy(all_sched_frame$DATE)
  
  names_cut <- names_csv[, c(3, 1)]
  
  all_sched_frame <<- all_sched_frame %>%
    inner_join(names_cut, by = c("TEAM_ID" = "NCAA"))
  
  all_sched_frame <<- all_sched_frame %>%
    inner_join(names_cut, by = c("OPP_ID" = "NCAA"))
  
  all_sched_frame[, 3] <<- all_sched_frame$`Ken Pom.x`
  all_sched_frame[, 4] <<- all_sched_frame$`Ken Pom.y`
  
  all_sched_frame<<- all_sched_frame[, c(-173, -174)]
  
  ranker_frame <<- website_ranker[, c(1, 39)]
  ranker_frame[, 3] <<- c(1:nrow(ranker_frame))
  ranker_frame <<- ranker_frame[, c(1:3)]
  
  all_sched_frame <<- all_sched_frame %>%
    left_join(ranker_frame, by = c("OPP_ID" = "TEAM_ID"))
  
  all_sched_frame <<- all_sched_frame %>%
    left_join(ranker_frame, by = c("TEAM_ID"))
  
  colnames(all_sched_frame)[174] <<- "OppRank"
  colnames(all_sched_frame)[176] <<- "TeamRank"
  
  all_sched_frame<<- all_sched_frame[, c(-173, -175)]

  write_csv(all_sched_frame, "scheddb.csv")
}

wbs_sched <- function(xxxx) {
  
  raw_sched_frame <<- team_game_by_game_raw[, c(1, 3, 2, 5, 7, 4, 10, 28)]
  adv_sched_frame <<- adv_game_log_join[, c(10, 26, 9)]
  adj_sched_frame <<- game_log_adj2[, c(3, 18)]
  tempo_sched_frame <<- tempo_adj2_frame[, 3]
  gid_sched_frame <<- team_game_by_game_raw[, 6]
  
  all_sched_frame <<- cbind(raw_sched_frame, adv_sched_frame, adj_sched_frame, tempo_sched_frame, gid_sched_frame)
  colnames(all_sched_frame)[9:14] <- c("RawOE", "RawDE", "RawTempo", "AdjOE", "AdjDE", "AdjTempo")
  
  all_sched_frame2 <<- all_sched_frame %>%
    mutate(RGScore = RawOE - RawDE) %>%
    mutate(AGScore = AdjOE - AdjDE)
  ranker_frame <<- website_ranker[, c(1, 39)]
  ranker_frame[, 3] <<- c(1:nrow(ranker_frame))
  ranker_frame <<- ranker_frame[, c(1:3)]
  
  all_sched_frame3 <<- all_sched_frame2 %>%
    left_join(ranker_frame, by = c("OPP_ID" = "TEAM_ID"))
  
  print(head(all_sched_frame3))
  
  asf_row <<- nrow(all_sched_frame3)
  
  all_sched_frame_ranks <<- all_sched_frame3
  
  #print(all_sched_frame_ranks)
  
  all_sched_frame_ranks <<- all_sched_frame_ranks %>%
    arrange(desc(AdjOE))
  
  all_sched_frame_ranks[, 20] <<- c(1:asf_row)
  
  all_sched_frame_ranks <<- all_sched_frame_ranks %>%
    arrange(AdjDE)
  
  all_sched_frame_ranks[, 21] <<- c(1:asf_row)
  
  all_sched_frame_ranks <<- all_sched_frame_ranks %>%
    arrange(desc(AdjTempo))
  
  all_sched_frame_ranks[, 22] <<- c(1:asf_row)
  
  all_sched_frame_ranks <<- all_sched_frame_ranks %>%
    arrange(desc(AGScore))
  
  all_sched_frame_ranks[, 23] <<- c(1:asf_row)
  
  
  all_sched_frame_mutate <<- all_sched_frame_ranks
  
  all_sched_frame_mutate <<- all_sched_frame_mutate %>%
    mutate(1 - ((V20 - 1) / (asf_row - 1)))
  
  all_sched_frame_mutate <<- all_sched_frame_mutate %>%
    mutate(1 - ((V21 - 1) / (asf_row - 1)))
  
  all_sched_frame_mutate <<- all_sched_frame_mutate %>%
    mutate(1 - ((V22 - 1) / (asf_row - 1)))
  
  all_sched_frame_mutate <<- all_sched_frame_mutate %>%
    mutate(1 - ((V23 - 1) / (asf_row - 1)))
  
  colnames(all_sched_frame_mutate)[19:27] <<- c("OppRank", "AOERank", "ADERank", "ATempoRank", "AGSRank", "OEPCT", "DEPCT", "TEMPOPCT", "GSPCT")
  
  all_sched_frame_final <<- all_sched_frame_mutate %>%
    left_join(ranker_frame, by = c("TEAM_ID"))

  
  all_sched_frame_final <<- all_sched_frame_final[, c(-18, -28)]
  colnames(all_sched_frame_final)[27] <<- "TeamRank" 
  
  all_sched_frame_final$DATE <<- mdy(all_sched_frame_final$DATE)
  
  names_cut <- names_csv[, c(3, 1)]
  
  all_sched_frame_final <<- all_sched_frame_final %>%
    inner_join(names_cut, by = c("TEAM_ID" = "NCAA"))
  
  all_sched_frame_final <<- all_sched_frame_final %>%
    inner_join(names_cut, by = c("OPP_ID" = "NCAA"))
  
  all_sched_frame_final[, 2] <<- all_sched_frame_final$`Ken Pom.x`
  all_sched_frame_final[, 6] <<- all_sched_frame_final$`Ken Pom.y`
  
  all_sched_frame_final <<- all_sched_frame_final[, c(-28, -29)]
  
  write_csv(all_sched_frame_final, "scheddb.csv")
  
}

wbs_recordcompute <- function(xxxx) {
  
  record_frame <<- team_game_by_game_raw %>%
    ungroup() %>%    
    select(TEAM_ID, TEAM_NAME, PTS, PTSd) %>%
    mutate(WIN = ifelse(PTS > PTSd, 1, 0), LOSS = ifelse(PTS < PTSd, 1, 0)) %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(WIN = sum(WIN), LOSS = sum(LOSS))

  
}

wbs_soscompute <- function(xxxx) {
  
  sos_frame <<- team_game_by_game_raw %>%
    ungroup() %>%
    select(TEAM_ID, TEAM_NAME, OPP_ID, OPP_NAME) %>%
    left_join(master_team_adj_frame_wt, by = c("OPP_ID" = "TEAM_ID")) %>%
    select(TEAM_ID, TEAM_NAME.x, OE, DE) %>%
    mutate(ED = OE - DE) %>%
    group_by(TEAM_ID, TEAM_NAME.x) %>%
    summarize(SOS = mean(ED)) %>%
    arrange(desc(SOS))
  
  nrow_sos <- nrow(sos_frame)
  sos_frame[, 4] <<- c(1:nrow_sos)
  
  colnames(sos_frame)[2] <<- "TEAM_NAME"
  colnames(sos_frame)[4] <<- "SOS-RK"
  

}

wbs_stats <- function(xxxx) {
  
  wbs_recordcompute(1)
  wbs_soscompute(1)
  
  all_stats_frame <- master_team_adj_frame_wt
  raw_stats_frames <<- raw_perpos_frame
  colnames(raw_stats_frames) <<- c("TEAM_ID", "TEAM_NAME", "rTEMPO", "rOE", "rEFG", "rSR2", "rSM2", "rSR3", "rSM3", "rFTPCT", 
                                   "rFTR", "rORB", "rDRB", "rTRB", "rAST", "rSTL", "rBLK", "rTO", "rFOUL", "rDE", "rdEFG", 
                                   "rdSR2", "rdSM2", "rdSR3", "rdSM3", "rdFTPCT", "rdFTR", "rdORB", "rdDRB", "rdTRB", "rdAST", 
                                   "rdSTL", "rdBLK", "rdTO", "rdFOUL", "rFT2FOUL", "rdFT2FOUL", "rSRFT", "rdSRFT")
  
  all_stats_frame <- all_stats_frame %>%
    mutate(ED = OE - DE)
  
  a <- 3
  g <- ncol(all_stats_frame)
  ct <- 0
  
  desc_vec <<- c(16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 34, 36)
  
  for (a in a:g) {
    
    ct <- ct + 1
    
    if (length(which(desc_vec == a)) > 0) { desc_val <- FALSE }
    else { desc_val <- TRUE }
    
    tmp_ranker <<- as.matrix(all_stats_frame[, c(1, a)])
    tmp_ranker <<- tmp_ranker[order(tmp_ranker[, 2], decreasing = desc_val), ]
    tmp_ranker <<- as.data.frame(tmp_ranker)
    tmp_colname <- paste(colnames(tmp_ranker)[2], "RK", sep = '-')
    tmp_ranker[, 3] <<- c(1:nrow(tmp_ranker))
    colnames(tmp_ranker)[3] <<- tmp_colname
    
    if (ct == 1) { all_ranks_frame <<- tmp_ranker }
    else { 
      all_ranks_frame <<- all_ranks_frame %>%
        left_join(tmp_ranker, by = c("TEAM_ID"))
    }
  }
  final_stats_frame <<- raw_stats_frames %>%
    left_join(all_ranks_frame, by = c("TEAM_ID"))
  
  names_cut <- names_csv[, c(3, 1)]
  
  final_stats_frame <<- final_stats_frame %>%
    inner_join(names_cut, by = c("TEAM_ID" = "NCAA"))
  
  final_stats_frame[, 2] <<- final_stats_frame$`Ken Pom`
  final_stats_frame <<- final_stats_frame[, -114]
  
  record_frame <<- record_frame[, -2]
  sos_frame <<- sos_frame[, -2]
  
  final_stats_frame <<- final_stats_frame %>%
    left_join(record_frame, by = c("TEAM_ID"))
  
  final_stats_frame <<- final_stats_frame %>%
    left_join(sos_frame, by = c("TEAM_ID"))
  
  write_csv(final_stats_frame, "tstatsdb.csv")
}

wbs_indy2 <- function(xxxx) {
  
  
  indy_raw_frame <<- ind_totals_raw[, c(3, 5:22, 4)]
  indy_ranker <<- ind_all_adj_frame %>%
    left_join(indy_raw_frame, by = c("PLAYER_ID")) %>%
    mutate(SBTPV = ((pOE * nat_avgs$oe_do) + (pTRB * .4616425) + (pAST * .66) + (pSTL * nat_avgs$oe_do) + (pBLK * nat_avgs$drb_do) + (pTO * -1 * nat_avgs$oe_do) + (pFOUL * -0.95)) * nat_avgs$tempo_do)
  
  indy_ranker$TEAM_ID <<- as.numeric(indy_ranker$TEAM_ID)
  
  
  indy_ranker2 <<- indy_ranker %>%
    mutate(TOTPOS = POS.TGM * ct)
  
  indy_vec <<- c(51, 6, 29, 7, 26, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 31)
  indy_des <<- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1)
  
  indy_stats_mx <<- as.matrix(indy_ranker2[, indy_vec])
  indy_qual_mx <<- as.matrix(indy_ranker2[, c(35, 37, 39, 41, 50, 37, 39, 41, 43)])
  indy_qual_mx[, 6] <<- indy_qual_mx[, 1] / indy_qual_mx[, 5]
  indy_qual_mx[, 7] <<- indy_qual_mx[, 2] / indy_qual_mx[, 5]
  indy_qual_mx[, 8] <<- indy_qual_mx[, 3] / indy_qual_mx[, 5]
  indy_qual_mx[, 9] <<- indy_qual_mx[, 4] / indy_qual_mx[, 5]
  
  
  indy_rank_mx <<- matrix(0, nrow = nrow(indy_ranker2), ncol = 24)
  indy_pct_mx <<- indy_rank_mx
  
  a <- 1
  g <- length(indy_vec)
  ct <- 0
  
  for (a in a:g) {
    
    indy_use <<- indy_vec[a]
    desc_use <<- indy_des[a]
    
     #No Qualifiers
    
    if (indy_use == 6 | indy_use == 29 | indy_use == 7) {
      
      rank_mx <<- rank(rank(indy_stats_mx[, a]) * -1)
      indy_rank_mx[, a] <<- rank_mx
      indy_pct_mx[, a] <<- 1 - ((rank_mx - 1) / (length(rank_mx) - 1))
      
    }
    
    #Min Qualifier Only
    
    if (indy_use == 51 | indy_use == 26 | indy_use == 8 | indy_use == 10 | indy_use == 12 | indy_use == 16 | indy_use == 17 |
        indy_use == 18 | indy_use == 19 | indy_use == 20 | indy_use == 21 | indy_use == 22 | indy_use == 23 | indy_use == 24 |
        indy_use == 25 | indy_use == 31) {
      
      which_mx <- which(indy_stats_mx[, 3] >= .3)
      subset_mx <- subset(indy_stats_mx, indy_stats_mx[, 3] >= .3)
      
      if (desc_use == 1) {
        
        rank_mx <<- rank(rank(subset_mx[, a]) * -1)
        indy_rank_mx[which_mx, a] <<- rank_mx
        
      }
      
      else {
        
        rank_mx <<- rank(subset_mx[, a])
        indy_rank_mx[which_mx, a] <<- rank_mx
        
      }
      indy_pct_mx[which_mx, a] <<- 1 - ((rank_mx - 1) / (length(rank_mx) - 1))
    }
    
    #eFG - Must Average 5 FGA Per Game
    
    if (indy_use == 9) {
      
      which_mx <- which(indy_qual_mx[, 6] >= 5 & indy_stats_mx[, 3] >= .3)
      subset_mx <- indy_stats_mx[which_mx, ]
      rank_mx <<- rank(rank(subset_mx[, a]) * -1)
      indy_rank_mx[which_mx, a] <<- rank_mx
      indy_pct_mx[which_mx, a] <<- 1 - ((rank_mx - 1) / (length(rank_mx) - 1))
      
    }
    
    #2PCT - Must Average 5 2PA Per Game
    
    if (indy_use == 11) {
      
      which_mx <- which(indy_qual_mx[, 7] >= 5 & indy_stats_mx[, 3] >= .3)
      subset_mx <- indy_stats_mx[which_mx, ]
      rank_mx <<- rank(rank(subset_mx[, a]) * -1)
      indy_rank_mx[which_mx, a] <<- rank_mx
      indy_pct_mx[which_mx, a] <<- 1 - ((rank_mx - 1) / (length(rank_mx) - 1))
      
    }
    
    #2PCT - Must Average 3 3PA Per Game
    
    if (indy_use == 13) {
      
      which_mx <- which(indy_qual_mx[, 8] >= 3 & indy_stats_mx[, 3] >= .3)
      subset_mx <- indy_stats_mx[which_mx, ]
      rank_mx <<- rank(rank(subset_mx[, a]) * -1)
      indy_rank_mx[which_mx, a] <<- rank_mx
      indy_pct_mx[which_mx, a] <<- 1 - ((rank_mx - 1) / (length(rank_mx) - 1))
      
    }
    
    #FTPCT- Must Average 3 FTA Per Game
    
    if (indy_use == 14) {
      
      which_mx <- which(indy_qual_mx[, 9] >= 3 & indy_stats_mx[, 3] >= .3)
      subset_mx <- indy_stats_mx[which_mx, ]
      rank_mx <<- rank(rank(subset_mx[, a]) * -1)
      indy_rank_mx[which_mx, a] <<- rank_mx
      indy_pct_mx[which_mx, a] <<- 1 - ((rank_mx - 1) / (length(rank_mx) - 1))
      
    }
    
    #FTR- Must Average 3 FTA Per Game
    
    if (indy_use == 15) {
      
      which_mx <- which(indy_qual_mx[, 9] >= 3 & indy_stats_mx[, 3] >= .3)
      subset_mx <- indy_stats_mx[which_mx, ]
      rank_mx <<- rank(rank(subset_mx[, a]) * -1)
      indy_rank_mx[which_mx, a] <<- rank_mx
      indy_pct_mx[which_mx, a] <<- 1 - ((rank_mx - 1) / (length(rank_mx) - 1))
      
    }
  }
  
  colnames_vec <- colnames(indy_ranker2)[indy_vec]
  
  master_indy <- data.frame(indy_ranker2, indy_rank_mx, indy_pct_mx)
  colnames(master_indy)[53:76] <- paste(colnames_vec, ".RK", sep = "")
  colnames(master_indy)[77:100] <- paste(colnames_vec, ".RKP", sep = "")
  
  names_cut <- names_csv[, c(3, 1)]
  
  final_ind_frame <<- master_indy  %>%
    inner_join(names_cut, by = c("TEAM_ID" = "NCAA"))
  
  final_ind_frame[, 4] <<- final_ind_frame$`Ken Pom`
  
  write_csv(final_ind_frame[ , -101], "istatsdb.csv")
  
}

wbs_indy <- function(xxxx) {
  
  indy_vec <- c(6, 29, 7, 30, 26, 8, 9, 10, 11, 12, 13, 14, 15, 31, 16, 17, 18, 19, 22, 20, 21, 23, 24, 25, 36)
  indy_raw_frame <<- ind_totals_raw[, c(3, 10, 12, 14, 4)]
  a <- 1
  g <- length(indy_vec)
  ct <- 0
  indy_ranker <<- ind_all_adj_frame %>%
    left_join(indy_raw_frame, by = c("PLAYER_ID")) %>%
    mutate(SBTPV = ((pOE * nat_avgs$oe_do) + (pTRB * .4616425) + (pAST * .66) + (pSTL * nat_avgs$oe_do) + (pBLK * nat_avgs$drb_do) + (pTO * -1 * nat_avgs$oe_do) + (pFOUL * -0.95)) * nat_avgs$tempo_do)
  
  indy_ranker$TEAM_ID <<- as.numeric(indy_ranker$TEAM_ID)
  
  
  indy_ranker2 <<- indy_ranker 
 
  for (a in a:g) {
    
    vec_use <<- indy_vec[a]
    inc_dec <- TRUE
    vec_col <<- colnames(indy_ranker2[vec_use])
    vec_rk <<- paste(vec_col, "RK", sep = "-")
    if (vec_use == 22 | vec_use == 25 | vec_use == 23) { inc_dec <- FALSE }
    print(a)
    indy_sorter <<- indy_ranker2[order(indy_ranker2[, vec_use], decreasing = inc_dec), ]
    indy_mx <<- as.matrix(indy_sorter[, c(vec_use, 2, 5, 29, 32:34)])
    
    rkct <- 0
    b <- 1
    h <- nrow(indy_mx)
    newrkcol <<- ncol(indy_ranker) + 1
    
    for (b in b:h) {
      
      if (vec_use == 11) {
        v_check1 <- indy_mx[b, 5]
        v_idcheck <- indy_mx[b, 2]
        if (v_check1 >= 20) {
          rkct <- rkct + 1 
          indy_which <<- which(indy_ranker$PLAYER_ID == v_idcheck)
          indy_ranker[indy_which, newrkcol] <<- rkct
        }
      }
      
      else if (vec_use == 13) {
        v_check1 <- indy_mx[b, 6]
        v_idcheck <- indy_mx[b, 2]
        if (v_check1 >= 20) {
          rkct <- rkct + 1 
          indy_which <<- which(indy_ranker$PLAYER_ID == v_idcheck)
          indy_ranker[indy_which, newrkcol] <<- rkct
        }
        
      }
      else if (vec_use == 14 | vec_use == 15) {
        v_check1 <- indy_mx[b, 7]
        v_idcheck <- indy_mx[b, 2]
        if (v_check1 >= 16) {
          rkct <- rkct + 1 
          indy_which <<- which(indy_ranker$PLAYER_ID == v_idcheck)
          indy_ranker[indy_which, newrkcol] <<- rkct
        }
        
      }
      else {
        
        v_check1 <- indy_mx[b, 3]
        v_check2 <- indy_mx[b, 4]
        v_idcheck <- indy_mx[b, 2]
        if (v_check1 >= 5 & v_check2 >= .2) {
          rkct <- rkct + 1 
          indy_which <<- which(indy_ranker$PLAYER_ID == v_idcheck)
          indy_ranker[indy_which, newrkcol] <<- rkct
        }
      }
    }
    colnames(indy_ranker)[newrkcol] <<- vec_rk
    if (a == 1) { rankcount_vec <<- rkct }
    else { rankcount_vec <<- c(rankcount_vec, rkct) }
    
  }
  
  a <- 1
  g <- 25
  ranks_mx <<- as.matrix(indy_ranker[, 37:61])
  pcts_mx <<- ranks_mx
  
  for (a in a:g) {
    
    print(a)
    rank_count <<- rankcount_vec[a]
    
    b <- 1
    h <- nrow(ranks_mx)
    colnames(pcts_mx)[a] <<- paste(colnames(ranks_mx)[a], "P", sep = "")
    
    for (b in b:h) {
      
      rk_see <<- ranks_mx[b, a]
      if (is.na(rk_see) == FALSE) {
        
        pcts_mx[b, a] <<- 1 - ((rk_see - 1) / (rank_count - 1))
        
      }
    }
  }
  indy_ranker$pORTG <<- round(indy_ranker$pORTG, 3)
  indy_ranker$pUSG <<- round(indy_ranker$pUSG, 3)
  final_ind_frame <<- data.frame(indy_ranker, pcts_mx)
  
  names_cut <- names_csv[, c(3, 1)]
  
  final_ind_frame  <<- final_ind_frame  %>%
    inner_join(names_cut, by = c("TEAM_ID" = "NCAA"))
  
  final_ind_frame[, 4] <<- final_ind_frame$`Ken Pom`
  final_ind_frame <<- final_ind_frame[, -87]
  
  write_csv(final_ind_frame, "istatsdb.csv")
}

website_do_all <- function(xxxx) {
  
  print("---SCHEDULE---")
  wbs_sched2(1)
  print("---TEAM STATS---")
  wbs_stats(1)
  print("---INDY2---")
  wbs_indy2(1)
  print("---INDY GAME---")
  write_csv(advantage_frame, "nstatsdb.csv")
  wbs_indy_game(1)
  print("---TEAM LEADERS---")
  website_team_leaders(1)
  print("---INDY LEADERS---")
  website_ind_leaders(1)
  
}

wbs_indy_game <- function(xxxx) {
  
  a <- 1
  g <- length(ind_game_log_list)
  
  for (a in a:g) {
    
    raw_lister <- ind_game_log_list[[a]]
    adj_lister <- ind_gamelog_adj_list[[a]]
    t_id2 <- as.numeric(ind_game_log_list[[a]][1, 3])
    file_name_csv2 <- paste("teamscores\\team_scores_", t_id2, ".csv", sep = "")
    t_csv_frame2 <- read_csv(file_name_csv2)
    t_csv_frame2 <- t_csv_frame2[, c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19:24, 27)]
    t_csv_frame2[, 1] <- lapply(t_csv_frame2[, 1], as.numeric)
    
    adj_lister[, c(3)] <- as.numeric(adj_lister[, c(3)])
    adj_lister[, c(4)] <- as.numeric(adj_lister[, c(4)])
    adj_lister[, c(7)] <- as.numeric(adj_lister[, c(7)])
    
    comb_lister <- raw_lister %>%
      inner_join(adj_lister, by = c("PLAYER_NAME", "PLAYER_ID", "TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", 
                                    "NO_ID", "LOC", "GAME_MINS", "GAME_POS", "pMIN", "pPOS", 
                                    "pSR2", "pSR3", "pFTPCT"))
    
    if (a == 1) { 
      ind_gamelog_binder <- comb_lister 
      raw_gamelog_binder <- t_csv_frame2
    }
    else { 
      ind_gamelog_binder <- rbind(ind_gamelog_binder, comb_lister) 
      raw_gamelog_binder <- rbind(raw_gamelog_binder, t_csv_frame2)
    }
  }
  
  ind_gamelog_binder <- ind_gamelog_binder %>%
    mutate(RAW2PA = pPOS * pSR2, RAW3PA = pPOS * pSR3, RAWFTA = pPOS * pSRFT.x) %>%
    mutate(SBTPV.x = ((pOE.x * nat_avgs$oe_do) + (pTRB.x * .4616425) + (pAST.x * .66) + (pSTL.x * nat_avgs$oe_do) 
                    + (pBLK.x * nat_avgs$drb_do) + (pTO.x * -1 * nat_avgs$oe_do) + (pFOUL.x * -0.95)) * nat_avgs$tempo_do) %>%
    mutate(SBTPV.y = ((pOE.y * nat_avgs$oe_do) + (pTRB.y * .4616425) + (pAST.y * .66) + (pSTL.y * nat_avgs$oe_do) 
                    + (pBLK.y * nat_avgs$drb_do) + (pTO.y * -1 * nat_avgs$oe_do) + (pFOUL.y * -0.95)) * nat_avgs$tempo_do)


  ind_binder_mx <- as.matrix(ind_gamelog_binder[, c(13:14, 17, 19, 33, 36:50, 58, 53:56)])
  ind_ranker_mx <- ind_binder_mx[, 1:22]
  ind_ranker_mx[, 2:22] <- 0
  
  #Mins Qual
  
  mins_qual <- c(2, 3, 4, 5, 6, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  desc_qual <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0)
  
  a <- 1
  g <- length(mins_qual)
  
  for (a in a:g) {
    
    mq_vec <- mins_qual[a]
    ds_vec <- desc_qual[a]
    cn_vec <- colnames(ind_binder_mx)[mq_vec]
    rk_colname <- paste(cn_vec, "RK", sep = "-")
    
    qualifier_mx <- which(ind_binder_mx[, 1] >= .4)
    cut_mx <- ind_binder_mx[qualifier_mx, mq_vec]
    
    if (ds_vec == 0) { ranks_mx <- rank(-cut_mx, ties.method = "random") }
    else { ranks_mx <- rank(cut_mx, ties.method = "random") }
    
    ind_ranker_mx[qualifier_mx, mq_vec] <- round(ranks_mx, 0)
    colnames(ind_ranker_mx)[mq_vec] <- rk_colname

  }
  
  #eFG% Qual
  mq_vec <- 7
  cn_vec <- colnames(ind_binder_mx)[mq_vec]
  rk_colname <- paste(cn_vec, "RK", sep = "-")
  qualifier_mx <- which((ind_binder_mx[, 23] + ind_binder_mx[, 24]) >= 8)
  cut_mx <- ind_binder_mx[qualifier_mx, mq_vec]
  ranks_mx <- rank(-cut_mx, ties.method = "random") 
  ind_ranker_mx[qualifier_mx, mq_vec] <- round(ranks_mx, 0)
  colnames(ind_ranker_mx)[mq_vec] <- rk_colname
  
  #2PFG% Qual
  mq_vec <- 8
  cn_vec <- colnames(ind_binder_mx)[mq_vec]
  rk_colname <- paste(cn_vec, "RK", sep = "-")
  qualifier_mx <- which(ind_binder_mx[, 23] >= 8)
  cut_mx <- ind_binder_mx[qualifier_mx, mq_vec]
  ranks_mx <- rank(-cut_mx, ties.method = "random") 
  ind_ranker_mx[qualifier_mx, mq_vec] <- round(ranks_mx, 0)
  colnames(ind_ranker_mx)[mq_vec] <- rk_colname
  
  #3PFG% Qual
  mq_vec <- 9
  cn_vec <- colnames(ind_binder_mx)[mq_vec]
  rk_colname <- paste(cn_vec, "RK", sep = "-")
  qualifier_mx <- which(ind_binder_mx[, 24] >= 5)
  cut_mx <- ind_binder_mx[qualifier_mx, mq_vec]
  ranks_mx <- rank(-cut_mx, ties.method = "random") 
  ind_ranker_mx[qualifier_mx, mq_vec] <- round(ranks_mx, 0)
  colnames(ind_ranker_mx)[mq_vec] <- rk_colname
  
  #FTR Qual
  mq_vec <- 10
  cn_vec <- colnames(ind_binder_mx)[mq_vec]
  rk_colname <- paste(cn_vec, "RK", sep = "-")
  qualifier_mx <- which(ind_binder_mx[, 25] >= 6)
  cut_mx <- ind_binder_mx[qualifier_mx, mq_vec]
  ranks_mx <- rank(-cut_mx, ties.method = "random") 
  ind_ranker_mx[qualifier_mx, mq_vec] <- round(ranks_mx, 0)
  colnames(ind_ranker_mx)[mq_vec] <- rk_colname
 
  #Percentiles
  
  ind_pctile_mx <- ind_ranker_mx
  
  a <- 2
  g <- ncol(ind_pctile_mx)
  
  for (a in a:g) {
    
    pct_max <- max(ind_pctile_mx[, a])
    cn_vec <- colnames(ind_pctile_mx)[a]
    rk_colname <- paste(cn_vec, "PCT", sep = "")
    whc_pct <- which(ind_pctile_mx[, a] > 0)
    ind_pctile_mx[whc_pct, a] <- 1 - ((ind_pctile_mx[whc_pct, a] - 1) / (pct_max - 1))
    colnames(ind_pctile_mx)[a] <- rk_colname
    
  }
  
  final_ind_game_frame <- cbind(ind_gamelog_binder, ind_ranker_mx[, 2:22], ind_pctile_mx[, 2:22])
  
  final_ind_game_frame <- final_ind_game_frame %>%
    filter(PLAYER_NAME != "TEAM")
  
  final_ind_game_frame <- final_ind_game_frame %>%
    inner_join(raw_gamelog_binder, by = c("PLAYER_NAME", "PLAYER_ID", "TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", 
                                    "NO_ID"))
  
  colnames(final_ind_game_frame)[110] <- "POINTS"
  
  names_ct <- names_csv[, c(1:4)]
  rep_mx <- final_ind_game_frame[, 5:6] %>%
    inner_join(names_ct, by = c("TEAM_NAME" = "NCAA ID")) %>%
    inner_join(names_ct, by = c("OPP_NAME" = "NCAA ID"))
  
  rep_mx <- rep_mx[, c(3, 6)]
  
  final_ind_game_frame[, 5:6] <- rep_mx

  
  write_csv(final_ind_game_frame, "gstatsdb.csv")
}


#Team Ranker

website_team_leaders <- function(xxxx) {
  
  stat_abb_vector <- c("Tempo", "OE", "eFG", "SR2", "SM2", "SR3", "SM3", "FT", "FTR", "SRFT", "ORB", "AST", "TO", "dFOUL",
                       "DE", "deFG", "dSR2", "dSM2", "dSR3", "dSM3", "dFTR", "dSRFT", "DRB", "TRB", "dAST", "dTO", "STL", "BLK",
                       "FOUL")
  
  round_vector <- c(1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  
  stat_full_vector <- c("Possessions per 40 Minutes", "Offensive Efficiency", "Effective Field Goal %", "2PA per Possession",
                        "2 Point Field Goal %", "3PA per Possession", "3 Point Field Goal %", "Free Throw %",
                        "FT to FGA Ratio", "FTA per Possession", "Offensive Rebounding %", "Assist %",
                        "Turnover %", "Opponent Foul %", "Defensive Efficiency",
                        "Opponent eFG %", "Opponent 2PA / Possession", "Opponent 2P FG %", "Opponent 3PA / Possession",
                        "Opponent 3P FG %", "Opponent FT/FGA Ratio", "Opponent FT / Possession", "Defensive Rebounding %",
                        "Total Rebounding %", "Opponent Assist %", "Opponent Turnover %", "Steal %", "Block %", "Foul %")
  
  a <- 1
  g <- length(stat_abb_vector)
  
  for (a in a:g) {
    
    stat_use <- stat_abb_vector[a]
    stat_user <- paste(stat_use, "-RK", sep = "")
    stat_where <- which(colnames(final_stats_frame) == stat_user)
    one_line <- which.min(final_stats_frame[, stat_where])
    one_team <- final_stats_frame[one_line, 2]
    one_val <- final_stats_frame[one_line, (stat_where - 1)]
    round_vec <- round_vector[a]
    
    if (round_vec == 1) { one_val <- round(one_val, 2) }
    if (round_vec == 2) { one_val <- round(one_val * 100, 1) }
    
    if (a == 1) { 
      team_leaders_frame <<- data.frame(stat_use, stat_full_vector[a], one_val, one_team, NA, NA, stringsAsFactors = FALSE)  
    }
    else {
      team_leaders_frame2 <<- data.frame(stat_use, stat_full_vector[a], one_val, one_team, NA, NA, stringsAsFactors = FALSE)  
      team_leaders_frame <<- rbind(team_leaders_frame, team_leaders_frame2)
    }
  }
  
  #Team Game Leaders
  
  colnames(team_leaders_frame) <<- c("StatAbb", "StatFull", "Val", "Team", "Opp", "Date")
  
  stat_abb_vector <- c("GScore", "Tempo", "Tempo", "OE", "eFG", "SR2", "SM2", "SR3", "SM3", "FTR", "SRFT", "ORB", "AST", "TO", "dFOUL",
                       "DE", "deFG", "dSR2", "dSM2", "dSR3", "dSM3", "dFTR", "dSRFT", "DRB", "TRB", "dAST", "dTO", "STL", "BLK",
                       "FOUL")
  
  stat_full_vector <- c("Game Score", "Most Possessions per 40 Minutes", "Fewest Possessions per 40 Minutes", "Offensive Efficiency", "Effective Field Goal %", "2PA per Possession",
                        "2 Point Field Goal %", "3PA per Possession", "3 Point Field Goal %",
                        "FT to FGA Ratio", "FTA per Possession", "Offensive Rebounding %", "Assist %",
                        "Turnover %", "Opponent Foul %", "Defensive Efficiency",
                        "Opponent eFG %", "Opponent 2PA / Possession", "Opponent 2P FG %", "Opponent 3PA / Possession",
                        "Opponent 3P FG %", "Opponent FT/FGA Ratio", "Opponent FT / Possession", "Defensive Rebounding %",
                        "Total Rebounding %", "Opponent Assist %", "Opponent Turnover %", "Steal %", "Block %", "Foul %")
  
  round_vector <- c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

  high_ball <- c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 0)
  
  raw_sched_frame <<- team_game_by_game_raw[, c(1, 3, 2, 5, 7, 4, 10, 28)] %>% ungroup()
  adj_sched_frame <<- game_log_adj2[, c(3:32, 35:36)]
  tempo_sched_frame <<- tempo_adj2_frame[, 3]
  gid_sched_frame <<- team_game_by_game_raw[, 6] %>% ungroup()
  
  all_sched_frame <<- cbind(raw_sched_frame, adj_sched_frame, tempo_sched_frame, gid_sched_frame)
  colnames(all_sched_frame)[41] <<- "Tempo"
  
  all_sched_frame <<- all_sched_frame %>%
    mutate(GScore = (OE - DE) * 100)
  
  a <- 1
  g <- length(stat_abb_vector)
  
  for (a in a:g) {
    
    stat_user <- stat_abb_vector[a]
    stat_where <- which(colnames(all_sched_frame) == stat_user)
    highball_use <- high_ball[a]
    
    if (highball_use == 1) { 
      one_line <- which.max(all_sched_frame[, stat_where])[1]
    }
    else {
      one_line <- which.min(all_sched_frame[, stat_where])[1]
    }
    
    one_val <- as.numeric(all_sched_frame[one_line, stat_where])
    one_team <- as.character(all_sched_frame[one_line, 2])
    one_opp <- as.character(all_sched_frame[one_line, 6])
    one_team <- names_cut_replace(one_team)
    one_opp <- names_cut_replace(one_opp)
    one_date <- as.character(all_sched_frame[one_line, 4])
    print(paste(stat_user, stat_full_vector[a], stat_where, one_line))
    round_vec <- round_vector[a]
    
    if (round_vec == 1) { one_val <- round(one_val, 2) }
    if (round_vec == 2) { one_val <- round(one_val * 100, 1) }
    
    if (a == 1) { stat_user <- "GSCORE" }
    if (a == 2) { stat_user <- "TempoHi" }
    if (a == 3) { stat_user <- "TempoLo" }
    
    team_leaders_frame2 <<- data.frame(stat_user, stat_full_vector[a], one_val, one_team, one_opp, one_date, stringsAsFactors = FALSE)  
    colnames(team_leaders_frame2) <<- c("StatAbb", "StatFull", "Val", "Team", "Opp", "Date")
    team_leaders_frame <<- rbind(team_leaders_frame, team_leaders_frame2)
    
  }
  write_csv(team_leaders_frame, "teamleaders.csv")
}

names_cut_replace <- function(x) {
  
  a <- 1
  g <- nrow(names_csv)
  
  for (a in a:g) {
    b <- as.character(names_csv[a, 4])
    if (b == x) {
      
      v <- as.character(names_csv[a, 1])
      return(v)
      
    }
  }
}

website_ind_leaders <- function(xxxx) {
  
  final_ind_game_frame <- read_csv("gstatsdb.csv")
  
  stat_abb_vector <- c("SBTPV", "pMIN", "pPOS", "pUSG", "pORTG", "pDRTG", "pOE", "peFG", "pSR2", "pSM2", "pSR3", "pSM3", 
                       "pFTPCT", "pFTR", "pSRFT", "pORB", "pDRB", "pTRB", "pAST", "pTO", "pSTL", "pBLK", "pFOUL")
  
  stat_full_vector <- c("SBT Player Value", "Minutes %", "Possessions / Game", "Usage", "Offensive Rating", "Defensive Rating",
                        "Points per Possession", "Effective Field Goal %", "2 Point Attempts / Possession", "2 Point Field Goal %",
                        "3 Point Attempts / Possession", "3 Point Field Goal %", "Free Throw %", "FT to FGA Ratio",
                        "FTA / Possession", "Offensive Rebounding %", "Defensive Rebounding %", "Total Rebounding %",
                        "Assist %", "Turnover %", "Steal %", "Block %", "Foul %")
  
  round_vector <- c(1, 2, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  
  a <- 1
  g <- length(stat_abb_vector)
  
  for (a in a:g) {
    
    stat_use <- stat_abb_vector[a]
    stat_user <- paste(stat_use, ".RK", sep = "")
    stat_where <- which(colnames(final_ind_frame) == stat_user)

    val_where <- which(colnames(final_ind_frame) == stat_use)
    one_line_x <- which(final_ind_frame[, stat_where] > 0)
    one_line <- which.min(final_ind_frame[one_line_x, stat_where])[1]
    
    one_frame_x <- final_ind_frame[one_line_x, ]
    one_team <- one_frame_x[one_line, 4]
    one_player <- one_frame_x[one_line, 1]
    one_pid <- one_frame_x[one_line, 2]
    one_val <- one_frame_x[one_line, val_where]
    round_vec <- round_vector[a]
    
    
    if (round_vec == 1) { one_val <- round(one_val, 2) }
    if (round_vec == 2) { one_val <- round(one_val * 100, 1) }
    
    #StatAbb, StatFull, Value, Player, Team, Opponent, Date
    
    ppp <- stat_use
    if (substring(ppp, 1, 1) == "p") {
      
      ppp <- substring(ppp, 2, 100)
      
    }
    
    #one_team <- names_cut_replace(one_team)
    
    if (a == 1) { 
      ind_leaders_frame <<- data.frame(ppp, stat_full_vector[a], one_val, one_player, one_team, one_pid, NA, NA, stringsAsFactors = FALSE)  
    }
    else {
      ind_leaders_frame2 <<- data.frame(ppp, stat_full_vector[a], one_val, one_player, one_team, one_pid, NA, NA, stringsAsFactors = FALSE)  
      ind_leaders_frame <<- rbind(ind_leaders_frame, ind_leaders_frame2)
    }
  }
  
  colnames(ind_leaders_frame) <<- c("StatAbb", "StatFull", "Val", "Player", "Team", "PID", "Opp", "Date")
  
  stat_abb_vector <- c("SBTPV.y", "pPOS", "pUSG.x", "pORTG.y", "pDRTG.y", "pOE.y", "peFG.y", "pSR2", "pSM2.y", "pSR3", "pSM3.y", 
                       "pFTR.y", "pSRFT.y", "pORB.y", "pDRB.y", "pTRB.y", "pAST.y", "pSTL.y", "pBLK.y")
  
  stat_abb_vector2 <- c("SBTPV", "pPOS", "pUSG", "pORTG", "pDRTG", "pOE", "peFG", "pSR2", "pSM2", "pSR3", "pSM3", 
                       "pFTR", "pSRFT", "pORB", "pDRB", "pTRB", "pAST", "pSTL", "pBLK")
  
  stat_full_vector <- c("SBT Player Value", "Possessions Played", "Usage", "Offensive Rating", "Defensive Rating",
                        "Points per Possession", "Effective Field Goal %", "2 Point Attempts / Possession", "2 Point Field Goal %",
                        "3 Point Attempts / Possession", "3 Point Field Goal %", "FT to FGA Ratio",
                        "FTA / Possession", "Offensive Rebounding %", "Defensive Rebounding %", "Total Rebounding %",
                        "Assist %", "Steal %", "Block %")
  
  round_vector <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  
  a <- 1
  g <- length(stat_abb_vector)
  
  for (a in a:g) {
    
    stat_use <- stat_abb_vector[a]
    stat_user <- paste(stat_use, "-RK", sep = "")
    stat_where <- which(colnames(final_ind_game_frame) == stat_user)
    val_where <- which(colnames(final_ind_game_frame) == stat_use)
    one_line <- which(final_ind_game_frame[, stat_where] == 1)[1]
    
    one_team <- final_ind_game_frame[one_line, 5]
    one_opp <- final_ind_game_frame[one_line, 6]
    one_date <- final_ind_game_frame[one_line, 7]
    one_player <- final_ind_game_frame[one_line, 1]
    one_pid <- final_ind_game_frame[one_line, 2]
    one_val <- final_ind_game_frame[one_line, val_where]
    round_vec <- round_vector[a]
    
    if (round_vec == 1) { one_val <- round(one_val, 2) }
    if (round_vec == 2) { one_val <- round(one_val * 100, 1) }
  
    
    #StatAbb, StatFull, Value, Player, Team, Opponent, Date
    
    ppp <- stat_abb_vector2[a]
    if (substring(ppp, 1, 1) == "p") {
      
      ppp <- substring(ppp, 2, 100)
      
    }
    
    
    #one_team <- names_cut_replace(one_team)
    #one_opp <- names_cut_replace(one_opp)
    
    
    ind_leaders_frame2 <<- data.frame(ppp, stat_full_vector[a], one_val, one_player, one_team, one_pid, one_opp, one_date, stringsAsFactors = FALSE) 
    colnames(ind_leaders_frame2) <<- c("StatAbb", "StatFull", "Val", "Player", "Team", "PID", "Opp", "Date")
    ind_leaders_frame <<- rbind(ind_leaders_frame, ind_leaders_frame2)
  }
  
  write_csv(ind_leaders_frame, "indleaders.csv")
}

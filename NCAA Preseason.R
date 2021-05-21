#Coach-Tempo Merge

coach_tempo_merge <- function(xxxx) {
  
  year_vector <<- c(2017, 2018, 2019, 2020, 2021)
  
  a <- 1
  g <- 4
  
  for (a in a:g) {
    
    y <- year_vector[a]
    past_team_file <<- read_csv(paste("team_", y, "_stats.csv", sep = ""))
    coach_file <<- read_csv(paste("coaches_", y, ".csv", sep = ""))
    
    print(y)
    
    pt_file_trim <<- past_team_file[, c(1:2, 37)]
    
    pt_file_trim[, 1] <<- lapply(pt_file_trim[, 1], as.numeric)
    coach_file[, 3] <<- lapply(coach_file[, 3], as.numeric)
    
    if (y == 2017) { 
      pt_file_join <<- coach_file %>%
        left_join(pt_file_trim, by = c("NCAA2" = "TEAM_ID"))
      
    }
    else {
      
      pt_file_join <<- coach_file %>%
        left_join(pt_file_trim, by = c("NCAA" = "TEAM_ID"))
      
    }
    
    if (a == 1) {
      
      coach_frame <<- pt_file_join[, c(9, 11)]
      coach_frame[, 3] <<- y
      
    }
    
    else {
      
      coach_frame2 <<- rbind(pt_file_join[, c(9, 11)])
      coach_frame2[, 3] <<- y
      coach_frame <<- rbind(coach_frame, coach_frame2)
      
    }
  }
}

curryear_tempo_coach <- function(xxxx) {
  
  cur_year <- 2021
  coach_file <<- read_csv(paste("coaches_", cur_year, ".csv", sep = ""))
  coach_tempo_fill <<- coach_file[, 3:4]
  nat_avg_use <<- read_csv(paste("advantages_", (cur_year - 1), ".csv", sep = ""))
  
  a <- 1
  g <- nrow(coach_file)
  
  for (a in a:g) {
    
    coach_lu <<- as.character(coach_file[a, 9]) 
    coach_subset <<- coach_frame %>%
      filter(V9 == coach_lu) %>%
      arrange(desc(V3))
    
    if (nrow(coach_subset) > 0) {
      coach_tempo_fill[a, 3] <<- as.numeric(coach_subset[1, 2])
    }
    else {
      coach_tempo_fill[a, 3] <<- as.numeric(nat_avg_use$Tempo[4])
    }
  }
}

#Create Baseline and Change Frames

baseline_create <- function(xxxx) {
  
  year_4 <- 2017
  year_3 <- 2018
  year_2 <- 2019
  year_1 <- 2020
  
  stat_file_1 <<- read_csv("team_2017_stats.csv")
  stat_file_2 <<- read_csv("team_2018_stats.csv")
  stat_file_3 <<- read_csv("team_2019_stats.csv")
  stat_file_4 <<- read_csv("team_2020_stats.csv")
  
  names_file_1 <<- read_csv("old files\\names - 2017.csv")
  names_file_2 <<- read_csv("old files\\names - 2018.csv")
  names_file_3 <<- read_csv("old files\\names - 2019.csv")
  names_file_4 <<- read_csv("old files\\names - 2020.csv")
  
  names_file <<- read_csv("names.csv")
  names_short <<- names_file[, c(8, 3, 4)]
  baseline_frame <<- names_short
  
  baseline_vector <<- c(3:36, 38)
  
  a <- 1
  g <- length(baseline_vector)
  
  for (a in a:g) {
    
    #Year 1
    blv_id <- baseline_vector[a]
    stat_file_1_short <<- stat_file_1[, c(1:2, blv_id)]
    
    id_check <- as.numeric(stat_file_1[1, 1])
    
    if (id_check > 100000) {
      
      n_file_1_short <<- names_file_1[, c(3:4, 8)]
      n_file_1_short[, 1] <<- lapply(n_file_1_short[, 1], as.numeric)
      stat_file_1_join <<- stat_file_1_short %>%
        left_join(n_file_1_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_1_join <<- stat_file_1_join[, c(5, 4, 3)]
      
    }
    
    else { stat_file_1_join <<- stat_file_1_short }
    
    #Year 2
    blv_id <- baseline_vector[a]
    stat_file_2_short <<- stat_file_2[, c(1:2, blv_id)]
    
    id_check <- as.numeric(stat_file_2[1, 1])
    
    if (id_check > 100000) {
      
      n_file_2_short <<- names_file_2[, c(3:4, 8)]
      n_file_2_short[, 1] <<- lapply(n_file_2_short[, 1], as.numeric)
      stat_file_2_join <<- stat_file_2_short %>%
        left_join(n_file_2_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_2_join <<- stat_file_2_join[, c(5, 4, 3)]
      
    }
    
    else { stat_file_2_join <<- stat_file_2_short }
    
    #Year 3
    blv_id <- baseline_vector[a]
    stat_file_3_short <<- stat_file_3[, c(1:2, blv_id)]
    
    id_check <- as.numeric(stat_file_3[1, 1])
    
    if (id_check > 100000) {
      
      n_file_3_short <<- names_file_3[, c(3:4, 8)]
      n_file_3_short[, 1] <<- lapply(n_file_3_short[, 1], as.numeric)
      stat_file_3_join <<- stat_file_3_short %>%
        left_join(n_file_3_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_3_join <<- stat_file_3_join[, c(5, 4, 3)]
      
    }
    
    else { stat_file_3_join <<- stat_file_3_short }
    
    #Year 4
    blv_id <- baseline_vector[a]
    stat_file_4_short <<- stat_file_4[, c(1:2, blv_id)]
    
    id_check <- as.numeric(stat_file_4[1, 1])
    
    if (id_check > 100000) {
      
      n_file_4_short <<- names_file_4[, c(3:4, 8)]
      n_file_4_short[, 1] <<- lapply(n_file_4_short[, 1], as.numeric)
      stat_file_4_join <<- stat_file_4_short %>%
        left_join(n_file_4_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_4_join <<- stat_file_4_join[, c(5, 4, 3)]
      
    }
    
    else { stat_file_4_join <<- stat_file_4_short }
    
    #Change Col Names
    
    colnames(stat_file_1_join) <<- c("ID", "TEAM", "STAT")
    colnames(stat_file_2_join) <<- c("ID", "TEAM", "STAT")
    colnames(stat_file_3_join) <<- c("ID", "TEAM", "STAT")
    colnames(stat_file_4_join) <<- c("ID", "TEAM", "STAT")
    
    stat_file_1_join <<- stat_file_1_join[, c(1, 3)]
    stat_file_2_join <<- stat_file_2_join[, c(1, 3)]
    stat_file_3_join <<- stat_file_3_join[, c(1, 3)]
    stat_file_4_join <<- stat_file_4_join[, c(1, 3)]
    
    #Join Frames
    
    
    
    to_4_frame <<- names_short %>%
      left_join(stat_file_4_join, by = c("NCAA2" = "ID")) %>%
      left_join(stat_file_3_join, by = c("NCAA2" = "ID")) %>%
      left_join(stat_file_2_join, by = c("NCAA2" = "ID")) %>%
      left_join(stat_file_1_join, by = c("NCAA2" = "ID"))
    
    #Calculate Weighted Baseline
    
    b <- 1
    h <- nrow(to_4_frame)
    change_frame <<- NULL
    
    for (b in b:h) {
      
      c <- 4
      i <- 7
      stat_vector <<- NULL
      act_ncaa2 <<- as.numeric(names_short[b, 1])
      act_ncaa <<- as.numeric(names_short[b, 2])
      act_ncaaid <<- as.character(names_short[b, 3])
      
      for (c in c:i) {
        
        stat_use <<- as.numeric(to_4_frame[b, c])
        if ((c + 1) > i) { stat_use_nxt <<- NA }
        else { stat_use_nxt <<- as.numeric(to_4_frame[b, (c + 1)]) }
        
        if (is.na(stat_use) == FALSE & is.na(stat_use_nxt) == FALSE) {
          
          dif_use <<- stat_use - stat_use_nxt
          dif_year <<- year_1 - (c - 4) 
          if (is.null(change_frame) == TRUE) { change_frame <<- data.frame(act_ncaa2, act_ncaa, act_ncaaid, dif_year, dif_use,
                                                                           stringsAsFactors = FALSE) 
          }
          else {
            
            change_frame2 <<- data.frame(act_ncaa2, act_ncaa, act_ncaaid, dif_year, dif_use,
                                         stringsAsFactors = FALSE) 
            change_frame <<- rbind(change_frame, change_frame2)
            
          }
        }
        
        if (is.na(stat_use) == FALSE) {
          
          if (is.null(stat_vector) == TRUE) { stat_vector <<- stat_use }
          else { stat_vector <<- c(stat_vector, stat_use) }
          
        }
      }
      
      if (length(stat_vector) == 0) { weighted_stat <<- -99 }
      if (length(stat_vector) == 1) { weighted_stat <<- stat_vector }
      if (length(stat_vector) == 2) { 
        
        sv_1 <<- stat_vector[1] * 1.5
        sv_2 <<- stat_vector[2] * 1
        weighted_stat <<- (sv_1 + sv_2) / 2.5
        
      }
      if (length(stat_vector) == 3) {
        
        sv_1 <<- stat_vector[1] * 1.67
        sv_2 <<- stat_vector[2] * 1.33
        sv_3 <<- stat_vector[3] * 1
        weighted_stat <<- (sv_1 + sv_2 + sv_3) / 4
        
      }
      if (length(stat_vector) == 4) {
        
        sv_1 <<- stat_vector[1] * 2
        sv_2 <<- stat_vector[2] * 1.67
        sv_3 <<- stat_vector[3] * 1.33
        sv_4 <<- stat_vector[4] * 1
        weighted_stat <<- (sv_1 + sv_2 + sv_3 + sv_4) / 6
        
      }
      baseline_frame[b, (a + 3)] <<- weighted_stat
      
    }
    if (a == 1) { change_frame_list <<- change_frame }
    else { change_frame_list <<- cbind(change_frame_list, change_frame[, 5]) }
  }
  colnames(baseline_frame)[4:38] <<- c("OE", "eFG", "SR2", "SM2", "SR3", "SM3", "FTR", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TO",
                                       "FOUL", "DE", "deFG", "dSR2", "dSM2", "dSR3", "dSM3", "dFTR", "dORB", "dDRB", "dTRB", "dAST", "dSTL",
                                       "dBLK", "dTO", "dFOUL", "FTperFOUL", "dFTperFOUL", "SRFT", "dSRFT", "FT")
  
  colnames(change_frame_list)[5:39] <<- c("OE", "eFG", "SR2", "SM2", "SR3", "SM3", "FTR", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TO",
                                          "FOUL", "DE", "deFG", "dSR2", "dSM2", "dSR3", "dSM3", "dFTR", "dORB", "dDRB", "dTRB", "dAST", "dSTL",
                                          "dBLK", "dTO", "dFOUL", "FTperFOUL", "dFTperFOUL", "SRFT", "dSRFT", "FT")
}

#Create Returning Minutes Frame

return_calculate <- function(xxxx) {
  
  year_4 <- 2017
  year_3 <- 2018
  year_2 <- 2019
  year_1 <- 2020
  
  stat_file_1 <<- read_csv("ind_2017_stats.csv")
  stat_file_2 <<- read_csv("ind_2018_stats.csv")
  stat_file_3 <<- read_csv("ind_2019_stats.csv")
  stat_file_4 <<- read_csv("ind_2020_stats.csv")
  
  names_file_1 <<- read_csv("old files\\names - 2017.csv")
  names_file_2 <<- read_csv("old files\\names - 2018.csv")
  names_file_3 <<- read_csv("old files\\names - 2019.csv")
  names_file_4 <<- read_csv("old files\\names - 2020.csv")
  
  names_file <<- read_csv("names.csv")
  names_short <<- names_file[, c(8, 3, 4)]
  
  baseline_vector <<- 29
  
  a <- 1
  g <- length(baseline_vector)
  
  for (a in a:g) {
    
    #Year 1
    blv_id <- baseline_vector[a]
    stat_file_1_short <<- stat_file_1[, c(1:3, blv_id)]
    
    id_check <- as.numeric(stat_file_1[1, 3])
    
    if (id_check > 100000) {
      
      n_file_1_short <<- names_file_1[, c(3:4, 8)]
      n_file_1_short[, 1] <<- lapply(n_file_1_short[, 1], as.numeric)
      stat_file_1_join <<- stat_file_1_short %>%
        left_join(n_file_1_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_1_join <<- stat_file_1_join[, c(6, 5, 2, 1, 4)]
      
    }
    
    else { stat_file_1_join <<- stat_file_1_short }
    
    #Year 2
    blv_id <- baseline_vector[a]
    stat_file_2_short <<- stat_file_2[, c(1:3, blv_id)]
    
    id_check <- as.numeric(stat_file_2[1, 3])
    
    if (id_check > 100000) {
      
      n_file_2_short <<- names_file_2[, c(3:4, 8)]
      n_file_2_short[, 1] <<- lapply(n_file_2_short[, 1], as.numeric)
      stat_file_2_join <<- stat_file_2_short %>%
        left_join(n_file_2_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_2_join <<- stat_file_2_join[, c(1, 2, 6, 4)]
      
    }
    
    else { stat_file_2_join <<- stat_file_2_short }
    
    #Year 3
    blv_id <- baseline_vector[a]
    stat_file_3_short <<- stat_file_3[, c(1:3, blv_id)]
    
    id_check <- as.numeric(stat_file_3[1, 3])
    
    if (id_check > 100000) {
      
      n_file_3_short <<- names_file_3[, c(3:4, 8)]
      n_file_3_short[, 1] <<- lapply(n_file_3_short[, 1], as.numeric)
      stat_file_3_join <<- stat_file_3_short %>%
        left_join(n_file_3_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_3_join <<- stat_file_3_join[, c(1, 2, 6, 4)]
      
    }
    
    else { stat_file_3_join <<- stat_file_3_short }
    
    #Year 4
    blv_id <- baseline_vector[a]
    stat_file_4_short <<- stat_file_4[, c(1:3, blv_id)]
    
    id_check <- as.numeric(stat_file_4[1, 3])
    
    if (id_check > 100000) {
      
      n_file_4_short <<- names_file_4[, c(3:4, 8)]
      n_file_4_short[, 1] <<- lapply(n_file_4_short[, 1], as.numeric)
      stat_file_4_join <<- stat_file_4_short %>%
        left_join(n_file_4_short, by = c("TEAM_ID" = "NCAA")) 
      
      stat_file_4_join <<- stat_file_4_join[, c(1, 2, 6, 4)]
      
    }
    
    else { stat_file_4_join <<- stat_file_4_short }
    
    colnames(stat_file_1_join) <<- c("NAME", "PID", "NCAA", "STAT")
    colnames(stat_file_2_join) <<- c("NAME", "PID", "NCAA", "STAT")
    colnames(stat_file_3_join) <<- c("NAME", "PID", "NCAA", "STAT")
    colnames(stat_file_4_join) <<- c("NAME", "PID", "NCAA", "STAT")
    
    y1to2_frame <<- stat_file_2_join %>% left_join(stat_file_1_join, by = c("PID"))
    y1_sum <<- y1to2_frame %>% group_by(NCAA.x) %>% summarize(STAT = sum(STAT.x))
    y2_sum <<- y1to2_frame %>% group_by(NCAA.y) %>% summarize(STAT = sum(STAT.y))
    y1to2_mutate <<- y1_sum %>%
      inner_join(y2_sum, by = c("NCAA.x" = "NCAA.y")) %>%
      mutate(RETMIN = STAT.y / STAT.x)
    
    y2to3_frame <<- stat_file_3_join %>% left_join(stat_file_2_join, by = c("PID"))
    y2_sum <<- y2to3_frame %>% group_by(NCAA.x) %>% summarize(STAT = sum(STAT.x))
    y3_sum <<- y2to3_frame %>% group_by(NCAA.y) %>% summarize(STAT = sum(STAT.y))
    y2to3_mutate <<- y2_sum %>%
      inner_join(y3_sum, by = c("NCAA.x" = "NCAA.y")) %>%
      mutate(RETMIN = STAT.y / STAT.x)
    
    y3to4_frame <<- stat_file_4_join %>% left_join(stat_file_3_join, by = c("PID"))
    y3_sum <<- y3to4_frame %>% group_by(NCAA.x) %>% summarize(STAT = sum(STAT.x))
    y4_sum <<- y3to4_frame %>% group_by(NCAA.y) %>% summarize(STAT = sum(STAT.y))
    y3to4_mutate <<- y3_sum %>%
      inner_join(y4_sum, by = c("NCAA.x" = "NCAA.y")) %>%
      mutate(RETMIN = STAT.y / STAT.x)
    
    
  }
}

#Append RETMIN to Change Frame List

retmin_append <- function(xxxx) {
  
  year_2 <- 2018
  year_3 <- 2019
  year_4 <- 2020
  
  a <- 1
  g <- nrow(change_frame_list)
  
  for (a in a:g) {
    
    act_id <<- as.numeric(change_frame_list[a, 1])
    act_year <<- as.numeric(change_frame_list[a, 4])
    
    if (act_year == year_4) {
      
      m_line <- which(y3to4_mutate$NCAA.x == act_id)
      ret_min <<- as.numeric(y3to4_mutate[m_line, 4])
      change_frame_list[a, 40] <<- ret_min
      
    }
    
    if (act_year == year_3) {
      
      m_line <- which(y2to3_mutate$NCAA.x == act_id)
      ret_min <<- as.numeric(y2to3_mutate[m_line, 4])
      change_frame_list[a, 40] <<- ret_min
      
    }
    
    if (act_year == year_2) {
      
      m_line <- which(y1to2_mutate$NCAA.x == act_id)
      ret_min <<- as.numeric(y1to2_mutate[m_line, 4])
      change_frame_list[a, 40] <<- ret_min
      
    }
  }
}

#Create Frame of Last Year's Stats

change_ly_frame <- function(xxxx) {
  
  ly_frame <<- read_csv("team_2020_stats.csv")
  names_frame <<- read_csv("names.csv")
  ly_names <<- read_csv("old files\\names - 2020.csv")
  
  ly_frame_id <<- ly_frame %>%
    inner_join(ly_names, by = c("TEAM_ID" = "NCAA"))
  
  ly_frame_id <<- ly_frame_id[, c(46, 2, 3:36, 38)]
  
}

#Load Frame of Last Year's Stats with Returning Minutes

change_regression <- function(xxxx) {
  
  ly_frame <<- read_csv("2021_retmin.csv")
  colnames(change_frame_list)[40] <<- "RETMIN"
  reg_apply_frame <<- ly_frame
  
  a <- 5
  g <- ncol(change_frame_list) - 1
  
  for (a in a:g) {
    
    reg_use <<- lm(change_frame_list[, a] ~ RETMIN, data = change_frame_list)
    int_use <<- reg_use[[1]][1]
    cof_use <<- reg_use[[1]][2]
    
    b <- 1
    h <- nrow(ly_frame)
    
    for (b in b:h) {
      
      r <- a - 2
      stat_use <<- as.numeric(ly_frame[b, r])
      ret_min_use <<- as.numeric(ly_frame[b, 38])
      reg_calc <<- int_use + (cof_use * ret_min_use)
      stat_apply_use <<- as.numeric(stat_use + reg_calc)
      reg_apply_frame[b, r] <<- stat_apply_use
      
    }
  }
}

#Preseason Fingerprinter

pre_fingerprint_frames <- function(xxxx) {
  
  year_1 <- 2020
  year_2 <- 2019
  year_3 <- 2018
  
  names_file_1 <<- read_csv(paste("old files\\names - ", year_1, ".csv", sep = ""))
  names_file_2 <<- read_csv(paste("old files\\names - ", year_2, ".csv", sep = ""))
  names_file_3 <<- read_csv(paste("old files\\names - ", year_3, ".csv", sep = ""))
  
  ly_frame_short <<- read_csv("2021_retmin.csv")
  ly_frame_short <<- ly_frame_short[, c(1, 2, 3, 4, 9, 10, 16, 18, 19, 24, 25, 31, 38)]
  
  y1_frame_short <<- read_csv(paste("team_", year_1, "_stats.csv", sep = ""))
  y1_frame_short <<- y1_frame_short[, c(1, 2, 3, 4, 9, 10, 16, 18, 19, 24, 25, 31)]
  
  y2_frame_short <<- read_csv(paste("team_", year_2, "_stats.csv", sep = ""))
  y2_frame_short <<- y2_frame_short[, c(1, 2, 3, 4, 9, 10, 16, 18, 19, 24, 25, 31)]
  
  y3_frame_short <<- read_csv(paste("team_", year_3, "_stats.csv", sep = ""))
  y3_frame_short <<- y3_frame_short[, c(1, 2, 3, 4, 9, 10, 16, 18, 19, 24, 25, 31)]
  
  y1_check <<- y1_frame_short[1, 1]
  y2_check <<- y2_frame_short[1, 1]
  y3_check <<- y3_frame_short[1, 1]
  
  if (y1_check > 100000) {
    
    n_file_1_short <<- names_file_1[, c(3:4, 8)]
    n_file_1_short[, 1] <<- lapply(n_file_1_short[, 1], as.numeric)
    y1_frame_join <<- y1_frame_short %>%
      left_join(n_file_1_short, by = c("TEAM_ID" = "NCAA")) 
    
    y1_frame_join <<- y1_frame_join[, c(14, 13, 3:12)]
    
  }
  
  else { y1_frame_join <<- y1_frame_short }
  
  if (y2_check > 100000) {
    
    n_file_2_short <<- names_file_2[, c(3:4, 8)]
    n_file_2_short[, 1] <<- lapply(n_file_2_short[, 1], as.numeric)
    y2_frame_join <<- y2_frame_short %>%
      left_join(n_file_2_short, by = c("TEAM_ID" = "NCAA")) 
    
    y2_frame_join <<- y2_frame_join[, c(14, 13, 3:12)]
    
  }
  
  else { y2_frame_join <<- y2_frame_short }
  
  if (y3_check > 100000) {
    
    n_file_3_short <<- names_file_3[, c(3:4, 8)]
    n_file_3_short[, 1] <<- lapply(n_file_3_short[, 1], as.numeric)
    y3_frame_join <<- y3_frame_short %>%
      left_join(n_file_3_short, by = c("TEAM_ID" = "NCAA")) 
    
    y3_frame_join <<- y3_frame_join[, c(14, 13, 3:12)]
    
  }
  
  else { y3_frame_join <<- y3_frame_short }
  
  y1_frame_mins <<- y1_frame_join %>%
    inner_join(y3to4_mutate, by = c("NCAA2" = "NCAA.x"))
  
  y1_frame_mins[, 13] <<- year_1
  y1_frame_mins <<- y1_frame_mins[, -14]
  
  y2_frame_mins <<- y2_frame_join %>%
    inner_join(y2to3_mutate, by = c("NCAA2" = "NCAA.x"))
  
  y2_frame_mins[, 13] <<- year_2
  y2_frame_mins <<- y2_frame_mins[, -14]
  
  y3_frame_mins <<- y3_frame_join %>%
    inner_join(y1to2_mutate, by = c("NCAA2" = "NCAA.x"))
  
  y3_frame_mins[, 13] <<- year_3
  y3_frame_mins <<- y3_frame_mins[, -14]
  
  master_finger_frame <<- rbind(y1_frame_mins, y2_frame_mins, y3_frame_mins)
  
  oe_std <<- sd(master_finger_frame$OE)
  efg_std <<- sd(master_finger_frame$eFG)
  ftr_std <<- sd(master_finger_frame$FTR)
  orb_std <<- sd(master_finger_frame$ORB)
  tov_std <<- sd(master_finger_frame$TO)
  de_std <<- sd(master_finger_frame$DE)
  defg_std <<- sd(master_finger_frame$deFG)
  dftr_std <<- sd(master_finger_frame$dFTR)
  dorb_std <<- sd(master_finger_frame$dORB)
  dto_std <<- sd(master_finger_frame$dTO)
  min_std <<- sd(master_finger_frame$RETMIN)
  
  oe_mean <<- mean(master_finger_frame$OE)
  efg_mean <<- mean(master_finger_frame$eFG)
  ftr_mean <<- mean(master_finger_frame$FTR)
  orb_mean <<- mean(master_finger_frame$ORB)
  tov_mean <<- mean(master_finger_frame$TO)
  de_mean <<- mean(master_finger_frame$DE)
  defg_mean <<- mean(master_finger_frame$deFG)
  dftr_mean <<- mean(master_finger_frame$dFTR)
  dorb_mean <<- mean(master_finger_frame$dORB)
  dto_mean <<- mean(master_finger_frame$dTO)
  min_mean <<- mean(master_finger_frame$RETMIN)
  
  master_finger_frame[, 3] <<- master_finger_frame[, 3] - oe_mean
  master_finger_frame[, 3] <<- master_finger_frame[, 3] / oe_std
  
  master_finger_frame[, 4] <<- master_finger_frame[, 4] - efg_mean
  master_finger_frame[, 4] <<- master_finger_frame[, 4] / efg_std
  
  master_finger_frame[, 5] <<- master_finger_frame[, 5] - ftr_mean
  master_finger_frame[, 5] <<- master_finger_frame[, 5] / ftr_std
  
  master_finger_frame[, 6] <<- master_finger_frame[, 6] - orb_mean
  master_finger_frame[, 6] <<- master_finger_frame[, 6] / orb_std
  
  master_finger_frame[, 7] <<- master_finger_frame[, 7] - tov_mean
  master_finger_frame[, 7] <<- master_finger_frame[, 7] / tov_std
  
  master_finger_frame[, 8] <<- master_finger_frame[, 8] - de_mean
  master_finger_frame[, 8] <<- master_finger_frame[, 8] / de_std
  
  master_finger_frame[, 9] <<- master_finger_frame[, 9] - defg_mean
  master_finger_frame[, 9] <<- master_finger_frame[, 9] / defg_std
  
  master_finger_frame[, 10] <<- master_finger_frame[, 10] - dftr_mean
  master_finger_frame[, 10] <<- master_finger_frame[, 10] / dftr_std
  
  master_finger_frame[, 11] <<- master_finger_frame[, 11] - dorb_mean
  master_finger_frame[, 11] <<- master_finger_frame[, 11] / dorb_std
  
  master_finger_frame[, 12] <<- master_finger_frame[, 12] - dto_mean
  master_finger_frame[, 12] <<- master_finger_frame[, 12] / dto_std
  
  master_finger_frame[, 14] <<- master_finger_frame[, 14] - min_mean
  master_finger_frame[, 14] <<- master_finger_frame[, 14] / min_std
  
  ly_frame_short[, 3] <<- ly_frame_short[, 3] - oe_mean
  ly_frame_short[, 3] <<- ly_frame_short[, 3] / oe_std
  
  ly_frame_short[, 4] <<- ly_frame_short[, 4] - efg_mean
  ly_frame_short[, 4] <<- ly_frame_short[, 4] / efg_std
  
  ly_frame_short[, 5] <<- ly_frame_short[, 5] - ftr_mean
  ly_frame_short[, 5] <<- ly_frame_short[, 5] / ftr_std
  
  ly_frame_short[, 6] <<- ly_frame_short[, 6] - orb_mean
  ly_frame_short[, 6] <<- ly_frame_short[, 6] / orb_std
  
  ly_frame_short[, 7] <<- ly_frame_short[, 7] - tov_mean
  ly_frame_short[, 7] <<- ly_frame_short[, 7] / tov_std
  
  ly_frame_short[, 8] <<- ly_frame_short[, 8] - de_mean
  ly_frame_short[, 8] <<- ly_frame_short[, 8] / de_std
  
  ly_frame_short[, 9] <<- ly_frame_short[, 9] - defg_mean
  ly_frame_short[, 9] <<- ly_frame_short[, 9] / defg_std
  
  ly_frame_short[, 10] <<- ly_frame_short[, 10] - dftr_mean
  ly_frame_short[, 10] <<- ly_frame_short[, 10] / dftr_std
  
  ly_frame_short[, 11] <<- ly_frame_short[, 11] - dorb_mean
  ly_frame_short[, 11] <<- ly_frame_short[, 11] / dorb_std
  
  ly_frame_short[, 12] <<- ly_frame_short[, 12] - dto_mean
  ly_frame_short[, 12] <<- ly_frame_short[, 12] / dto_std
  
  ly_frame_short[, 13] <<- ly_frame_short[, 13] - min_mean
  ly_frame_short[, 13] <<- ly_frame_short[, 13] / min_std
  
}

pre_fingerprint_find <- function(xxxx) {
  
  thisyear_mx_no <<- ly_frame_short[, 1:13]
  
  pastyear_mx_no <<- master_finger_frame[, c(1:12, 14, 13)]
  
  year_1 <- 2020
  year_2 <- 2019
  year_3 <- 2018
  
  names_file_1 <<- read_csv(paste("old files\\names - ", year_1, ".csv", sep = ""))
  names_file_2 <<- read_csv(paste("old files\\names - ", year_2, ".csv", sep = ""))
  names_file_3 <<- read_csv(paste("old files\\names - ", year_3, ".csv", sep = ""))
  
  y1_frame <<- read_csv(paste("team_", year_1, "_stats.csv", sep = ""))
  
  y2_frame <<- read_csv(paste("team_", year_2, "_stats.csv", sep = ""))
  
  y3_frame <<- read_csv(paste("team_", year_3, "_stats.csv", sep = ""))
  
  y1_check <<- as.numeric(y1_frame[1, 1])
  y2_check <<- as.numeric(y2_frame[1, 1])
  y3_check <<- as.numeric(y3_frame[1, 1])
  
  if (y1_check > 100000) {
    
    n_file_1_short <<- names_file_1[, c(3:4, 8)]
    n_file_1_short[, 1] <<- lapply(n_file_1_short[, 1], as.numeric)
    y1_frame_join <<- y1_frame %>%
      left_join(n_file_1_short, by = c("TEAM_ID" = "NCAA")) 
    
    y1_frame_join <<- y1_frame_join[, c(41, 40, 3:39)]
    
  }
  
  else { y1_frame_join <<- y1_frame }
  
  if (y2_check > 100000) {
    
    n_file_2_short <<- names_file_2[, c(3:4, 8)]
    n_file_2_short[, 1] <<- lapply(n_file_2_short[, 1], as.numeric)
    y2_frame_join <<- y2_frame %>%
      left_join(n_file_2_short, by = c("TEAM_ID" = "NCAA")) 
    
    y2_frame_join <<- y2_frame_join[, c(41, 40, 3:39)]
    
  }
  
  else { y2_frame_join <<- y2_frame }
  
  if (y3_check > 100000) {
    
    n_file_3_short <<- names_file_3[, c(3:4, 8)]
    n_file_3_short[, 1] <<- lapply(n_file_3_short[, 1], as.numeric)
    y3_frame_join <<- y3_frame %>%
      left_join(n_file_3_short, by = c("TEAM_ID" = "NCAA")) 
    
    y3_frame_join <<- y3_frame_join[, c(41, 40, 3:39)]
    
  }
  
  else { y3_frame_join <<- y3_frame }
  
  a <- 1
  g <- nrow(thisyear_mx_id)
  
  for (a in a:g) {
    
    mins_no <<- as.numeric(thisyear_mx_no[a, 13])
    mins_min <<- mins_no - 0.5
    mins_max <<- mins_no + 0.5
    
    finger_mins_frame <<- pastyear_mx_no %>%
      filter(RETMIN >= mins_min & RETMIN <= mins_max)
    
    finger_sub_frame <<- finger_mins_frame
    
    finger_line <<- thisyear_mx_no[a, 3:13]
    
    b <- 1
    h <- nrow(finger_mins_frame)
    
    for (b in b:h) {
      
      comp_line <<- finger_mins_frame[b, 3:13]
      finger_sub_frame[b, 3:13] <<- abs(finger_line - comp_line)
      finger_sub_frame[b, 15] <<- rowSums(finger_sub_frame[b, 3:13]) / 11
      
    }
    
    finger_sub_frame <<- finger_sub_frame %>%
      arrange(V15)
    
    finger_sum_calc <<- finger_sub_frame[1:10, ]
    
    finger_sum_y1 <<- finger_sum_calc %>%
      filter(STAT.x == year_1) %>%
      select(NCAA2, `NCAA ID`) %>%
      left_join(y1_frame_join, by = c("NCAA2", "NCAA ID"))
    
    finger_sum_y2 <<- finger_sum_calc %>%
      filter(STAT.x == year_2) %>%
      select(NCAA2, `NCAA ID`) %>%
      left_join(y2_frame_join, by = c("NCAA2", "NCAA ID"))
    
    finger_sum_y3 <<- finger_sum_calc %>%
      filter(STAT.x == year_3) %>%
      select(NCAA2, `NCAA ID`) %>%
      left_join(y3_frame_join, by = c("NCAA2", "NCAA ID"))
    
    finger_sum_all <<- rbind(finger_sum_y1, finger_sum_y2, finger_sum_y3)
    
    finger_sum_means <<- colMeans(finger_sum_all[, 3:39])
    print(a)
    thisyear_mx_id[a, 3:39] <<- c(finger_sum_means)
  }
  colnames(thisyear_mx_id[3:39]) <<- colnames(y1_frame_join)[3:39]
}

#RSCI Frames

rsci_do <- function(xxxx) {
  
  rsci_frame <<- read_csv("RSCINet.csv")
  
  year_1 <- 2020
  year_2 <- 2019
  year_3 <- 2018
  
  y1_join <<- change_frame_list %>%
    filter(dif_year == year_1)
  
  rsci_frame_y1 <<- rsci_frame %>%
    filter(Year == year_1) %>%
    left_join(y1_join, by = c("NCAA2" = "act_ncaa2"))
  
  y2_join <<- change_frame_list %>%
    filter(dif_year == year_2)
  
  rsci_frame_y2 <<- rsci_frame %>%
    filter(Year == year_2) %>%
    left_join(y2_join, by = c("NCAA2" = "act_ncaa2"))
  
  y3_join <<- change_frame_list %>%
    filter(dif_year == year_3)
  
  rsci_frame_y3 <<- rsci_frame %>%
    filter(Year == year_3) %>%
    left_join(y3_join, by = c("NCAA2" = "act_ncaa2"))
  
  rsci_frame_all <<- rbind(rsci_frame_y1, rsci_frame_y2, rsci_frame_y3)
  
}

master_preseason_create <- function(xxxx) {
  
  fingerprint_frame <<- thisyear_mx_id
  changer_frame <<- reg_apply_frame
  baseliner_frame <<- baseline_frame
  tempoer_frame <<- coach_tempo_fill
  names_csv <<- read_csv("names.csv")
  newcomers_csv <<- read_excel("newcomers.xlsx")
  newcomers_line <<- newcomers_csv[4, c(4:37, 39)]
  
  fingerprint_mx <<- as.matrix(thisyear_mx_id[, c(3:36, 38)])
  changer_mx <<- as.matrix(reg_apply_frame[, c(3:37)])
  baseliner_mx <<- as.matrix(baseline_frame[, c(4:38)])
  
  ultra_pre_mx <<- fingerprint_mx
  ultra_pre_mx[, 1:35] <<- 0
  
  
  
  a <- 1
  g <- nrow(tempoer_frame)
  
  for (a in a:g) {
    
    new_id_use <<- as.numeric(tempoer_frame[a, 1])
    if (new_id_use > 0) {
      old_id_use <<- as.numeric(names_csv[which(names_csv$NCAA == new_id_use), 8])
      
      fingerprint_line <<- which(fingerprint_frame$NCAA2 == old_id_use)
      changer_line <<- which(changer_frame$NCAA2 == old_id_use)
      baseliner_line <<- which(baseliner_frame$NCAA2 == old_id_use)
      
      if (length(fingerprint_line) > 0) {
        
        b <- 1
        h <- 35
        
        for (b in b:h) {
          
          fp_stat <<- fingerprint_mx[fingerprint_line, b]
          ch_stat <<- changer_mx[changer_line, b]
          bs_stat <<- baseliner_mx[baseliner_line, b]
          ul_stat <<- (fp_stat * .2) + (ch_stat * .4) + (bs_stat * .4)
          ultra_pre_mx[a, b] <<- ul_stat
          
        }
      }
    }
    
    if (a <= nrow(fingerprint_frame)) { tempoer_frame[a, 4:38] <<- ultra_pre_mx[a, ] }
    else { tempoer_frame[a, 4:38] <<- newcomers_line }
    
  }
  colnames(tempoer_frame)[4:38] <<- colnames(fingerprint_frame)[c(3:36, 38)]
  colnames(tempoer_frame)[3] <<- "Tempo"
  master_preseason_frame <<- tempoer_frame
}
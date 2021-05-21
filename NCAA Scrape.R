library(rvest)
library(readxl)
library(openxlsx)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggalt)
library(magrittr)
library(ggplot2)
library(scales)
library(htmltools)
library(tcltk)
library(randomForest)
library(class)

#---------------MASS EXPORT OF IMPORTANT DATA-------------



#----------------BOX SCORE ID GRAB------------

#START = First Line to Read from BOXDATES.xlsx
#FINISH = Last Line to Read from BOXDATES.xlsx
#E = 1 to Wipe master_box_ids frame clean, 0 to append to bottom

box_id_cycle <- function(s, f, e) {
  
  names_csv <<- read_csv("names.csv")
  
  box_date_file <<- read_excel('BOXDATES.xlsx') %>%
    filter(PLAYED == 1)
  
  a <- s
  g <- f
  ct <- 0
  
  if (e == 0) {
    master_box_ids <<- read_csv("master_box_ids.csv")
  }
  
  for (a in a:g) {
  
    m_ <- as.character(box_date_file[a, 1])
    d_ <- as.character(box_date_file[a, 2]) 
    y_ <- as.character(box_date_file[a, 3])
    print(paste("Pulling", m_, d_, y_, sep = "-"))
    ct <- ct + 1
    load_game_day_scores(m_, d_, y_)
    grab_box_score_ids(1)
    if (e == 1) {
      if (ct == 1) { master_box_ids <<- box_mx }
      else { master_box_ids <<- rbind(master_box_ids, box_mx) }
    }
    else {
      master_box_ids <<- rbind(master_box_ids[, 1:7], box_mx) 
    }
  }
  write_csv(master_box_ids, "master_box_ids.csv")
  filter_out_boxid2(1)
}

new_year_id_retrieve <- function(xxxx) {
  
  aaa <- master_box_ids %>%
    filter(VID > 0) %>%
    group_by(VID, AWAYNAME) %>%
    summarize(ct = n())
  
  hhh <- master_box_ids %>%
    filter(HID > 0) %>%
    group_by(HID, HOMENAME) %>%
    summarize(ct = n())
  
  colnames(hhh)[1:2] <- c("VID", "AWAYNAME")
  
  yyy <- rbind(aaa, hhh)
  
  yyyy <<- yyy %>%
    group_by(VID, AWAYNAME) %>%
    summarize(ct = n())
  
}

filter_out_boxid2 <- function(xxxx) {
  
  master_box_ids <<- read_csv("master_box_ids.csv")
  
  date_rehab(1)
  
  new_year_id_retrieve(1)
  
  a <- 1
  g <- nrow(master_box_ids)
  
  for (a in a:g) {
    
    vid <- as.character(master_box_ids[a, 3]) 
    hid <- as.character(master_box_ids[a, 4]) 
    vn <- as.character(master_box_ids[a, 6]) 
    hn <- as.character(master_box_ids[a, 7]) 
    
    vid_w <- length(which(names_csv[, 3] == vid))
    hid_w <- length(which(names_csv[, 3] == hid))
    vn_w <- length(which(names_csv[, 4] == vn))
    hn_w <- length(which(names_csv[, 4] == hn))
    s_w <- sum(vid_w, hid_w, vn_w, hn_w)
    
    master_box_ids[a, 8] <<- s_w
    
  }
  
  print(s_w)
  colnames(master_box_ids)[8] <- "V8"
  print("HI2")
  ultra_box_filtered <<- master_box_ids %>%
    filter(V8 > 3)
  
  ultra_sam_houston_a <<- master_box_ids %>%
    filter(AWAYNAME == "Sam Houston St.") %>%
    filter(V8 > 2)
  
  ultra_sam_houston_a[, 6] <<- "Sam Houston"
  
  ultra_sam_houston_h <<- master_box_ids %>%
    filter(HOMENAME == "Sam Houston St.") %>%
    filter(V8 > 2)
  
  ultra_sam_houston_h[, 7] <<- "Sam Houston"
  
  ultra_sam_houston <<- rbind(ultra_sam_houston_a, ultra_sam_houston_h)
  
  ultra_box_filtered <<- rbind(ultra_box_filtered, ultra_sam_houston)
  
  ultra_box_filtered[, 8] <<- c(1:nrow(ultra_box_filtered))
  colnames(ultra_box_filtered)[8] <- "No"
  
  ultra_box_filtered <<- ultra_box_filtered %>%
    group_by(Date, BOXID, VID, HID, LOC, AWAYNAME, HOMENAME) %>%
    summarize(No = mean(No)) %>%
    arrange(No)
  
  date_vector_ <<- ultra_box_filtered %>%
    group_by(Date) %>%
    summarize(ct = mean(No)) %>%
    arrange(ct)
  
  
  aaa <- ultra_box_filtered %>%
    filter(VID > 0) %>%
    group_by(VID, AWAYNAME) %>%
    summarize(ct = n())
  
  hhh <- ultra_box_filtered %>%
    filter(HID > 0) %>%
    group_by(HID, HOMENAME) %>%
    summarize(ct = n())
  
  colnames(hhh)[1:2] <- c("VID", "AWAYNAME")
  
  yyy <- rbind(aaa, hhh)
  
  teamid_box_sum <<- yyy %>%
    group_by(VID, AWAYNAME) %>%
    summarize(ct = n())
}

switcheroo_2018 <- function(xxxx) {
  
  mbtmp <<- data.frame(master_box_ids[, 1:2], as.numeric(master_box_ids[, 4]), as.numeric(master_box_ids[, 3]), master_box_ids[, 5], master_box_ids[, 7],
                  master_box_ids[, 6], stringsAsFactors = FALSE)

  colnames(mbtmp) <<- c("Date", "BOXID", "VID", "HID", "LOC", "AWAYNAME", "HOMENAME")
  
  master_box_ids <<- mbtmp
  
}

filter_out_boxid <- function(xxxx) {
  
  ultra_box_ids <<- master_box_ids %>%
    filter(VID != 0 & HID != 0)
  
  ultra_box_ids[, 8] <<- c(1:nrow(ultra_box_ids))
  colnames(ultra_box_ids)[8] <<- c("No")
  
  vid_box_sum2 <<- ultra_box_ids %>%
    group_by(VID, AWAYNAME) %>%
    summarize(ct = n()) %>%
    filter(ct > 7)
  
  hid_box_sum2 <<- ultra_box_ids %>%
    group_by(HID, HOMENAME) %>%
    summarize(ct = n()) %>%
    filter(ct > 7)
  
  colnames(vid_box_sum2)[1:2] <<- c("TEAMID", "TEAMNAME")
  colnames(hid_box_sum2)[1:2] <<- c("TEAMID", "TEAMNAME")
  
  teamid_box_sum_v <<- data.frame(vid_box_sum2)
  teamid_box_sum_h <<- data.frame(hid_box_sum2)
  
  teamid_box_sum <<- rbind(teamid_box_sum_v, teamid_box_sum_h)
  teamid_box_sum_id <<- teamid_box_sum
  teamid_box_sum <<- teamid_box_sum %>%
    group_by(TEAMID) %>%
    summarize(ct = n())
  
  vid_box_mx <<- as.matrix(vid_box_sum2)
  ultra_mx <<- as.matrix(ultra_box_ids[, 3:4])
  ultra_clear <<- ultra_mx[, 1]
  
  a <- 1
  g <- nrow(ultra_mx)
  
  for (a in a:g) {
  
    vid_use <<- ultra_mx[a, 1]
    hid_use <<- ultra_mx[a, 2]
    
    vid_check <<- which(vid_box_mx[, 1] == vid_use)
    hid_check <<- which(vid_box_mx[, 1] == hid_use)
    if (length(vid_check) > 0 & length(hid_check) > 0) { m_check <- 1 }
    else { m_check <- 0 }

    ultra_clear[a] <- m_check
    
  }
  
  ultra_which <- which(ultra_clear == 1)
  ultra_box_filtered <<- ultra_box_ids[ultra_which, ]
  ultra_box_filtered <<- ultra_box_filtered %>%
    group_by(Date, BOXID, VID, HID, LOC, AWAYNAME, HOMENAME) %>%
    summarize(No = mean(No)) %>%
    arrange(No)
  
  date_vector_ <<- ultra_box_filtered %>%
    group_by(Date) %>%
    summarize(ct = mean(No)) %>%
    arrange(ct)
}


load_game_day_scores <- function(m, d, y) {
  
  link_mid <<- paste(m, "%2F", d, "%2F", y, sep = "")
  
  #2020 Link
  #test_link <<- paste("https://stats.ncaa.org/season_divisions/17060/scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=", link_mid, "&conference_id=0&tournament_id=&commit=Submit", sep = "")
  
  #2021 Link
  test_link <<- paste("https://stats.ncaa.org/season_divisions/17420/scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=", link_mid, "&conference_id=0&tournament_id=&commit=Submit", sep = "")
  
  #2017 Link
  #test_link <<- paste("https://stats.ncaa.org/season_divisions/13100/scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=", link_mid, "&conference_id=0&tournament_id=&commit=Submit", sep = "")
  
  #2018 Link
  #test_link <<- paste("https://stats.ncaa.org/season_divisions/13533/scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=", link_mid, "&conference_id=0&tournament_id=&commit=Submit", sep = "")
  
  #2019 Link
  #test_link <<- paste("https://stats.ncaa.org/season_divisions/16700/scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=", link_mid, "&conference_id=0&tournament_id=&commit=Submit", sep = "")
  
  
  test_html <<- read_html(test_link)
  test_date <<- paste(m, "-", d, "-", y, sep = "")
  
  test_attrs <<- test_html %>%
    html_nodes(".skipMask") %>%
    html_attrs()
  
  neut_html <<- test_html %>%
    html_nodes("tbody > tr > td") %>%
    html_text()

}



grab_box_score_ids <- function(xxxx) {
  a <- 1
  g <- length(test_attrs)
  ct <- 0
  oct <- 0
  bre_ct <- 0
  
  for (a in a:g) {
    
    attr_list <<- as.character(test_attrs[[a]][3])
    tid_list <<- as.character(test_attrs[[a]][1])
    
    if (grepl("box_score", attr_list) == TRUE) { 
      
      ct <- ct + 1
      oct <- 0
      
      last_1 <<- test_attrs[[(a - 1)]][1]
      last_2 <<- test_attrs[[(a - 2)]][[1]]
      
      if (grepl("/teams/", last_1) == TRUE & grepl("/teams", last_2) == TRUE) { 
        act_vid <<- strsplit(last_2, "/")
        act_vid <<- act_vid[[1]][3]
        act_hid <<- strsplit(last_1, "/")
        act_hid <<- act_hid[[1]][3]
        
      }
      
      else { 
        act_vid <<- 0
        act_hid <<- 0
        
      }
      
      if (ct == 1) { 
        box_mx <<- data.frame(test_date, attr_list, act_vid, act_hid, stringsAsFactors = FALSE) }
      else { 
        box_mx2 <<- data.frame(test_date, attr_list, act_vid, act_hid, stringsAsFactors = FALSE) 
        box_mx <<- rbind(box_mx, box_mx2)
      }
    }
  }
  
  a <- 6
  g <- length(neut_html)
  ct <- 0
  
  while (a <= g) {
    
    n_check <- trim.all(neut_html[[a]])
    can_check <- toupper(substring(trim.all(neut_html[[a - 1]]), 1, 3))
    if (nchar(n_check) > 3) { n_do <- "N" }
    else { n_do <- "H" }
    
    if (can_check == "CAN" | can_check == "PPD") { 
      a <- a + 10 
      #box_mx[ct, 5] <<- "C"
    }
    else { 
      bx_check <- neut_html[[(a + 5)]]
      if (bx_check == "Box Score") { 
        ct <- ct + 1
        box_mx[ct, 5] <<- n_do
        a <- a + 12
      }
      else { a <- a + 11 }
    }
  }
  
  a <- 3
  g <- length(neut_html)
  ct <- 0
  
  while (a <= g) {
    
    n_check <- trim.all(neut_html[[a]])
    can_check <- toupper(substring(trim.all(neut_html[[a + 2]]), 1, 3))
    
    f_char <- substring(n_check, 1, 1)
    if (f_char == "#") {
      
      ssplit <- strsplit(n_check, split = " ")
      ctchar <- nchar(ssplit[[1]][1]) + 2
      newsplit <- substring(n_check, ctchar, 100)
      
      n_strip <- strsplit(newsplit, split = "( )")
      b <- 1
      h <- length(n_strip[[1]])
      if (grepl("(.-.)", n_check) == TRUE) { h <- h - 1 }
      
      for (b in b:h) {
        
        if (b == 1) { n_out <- n_strip[[1]][b] }
        else { n_out <- paste(n_out, n_strip[[1]][b], sep = " ") }
        
      }
      
    }
    
    else {
      n_strip <- strsplit(n_check, split = "( )")
      b <- 1
      h <- length(n_strip[[1]])
      if (grepl("(.-.)", n_check) == TRUE) { h <- h - 1 }
      
      for (b in b:h) {
        
        if (b == 1) { n_out <- n_strip[[1]][b] }
        else { n_out <- paste(n_out, n_strip[[1]][b], sep = " ") }
        
      }
    }
    
    v_name_use <- n_out
    
    a <- a + 6
    
    n_check <- trim.all(neut_html[[a]])
    
    f_char <- substring(n_check, 1, 1)
    if (f_char == "#") {
      
      ssplit <- strsplit(n_check, split = " ")
      ctchar <- nchar(ssplit[[1]][1]) + 2
      newsplit <- substring(n_check, ctchar, 100)
      
      n_strip <- strsplit(newsplit, split = "( )")
      b <- 1
      h <- length(n_strip[[1]])
      if (grepl("(.-.)", n_check) == TRUE) { h <- h - 1 }
      
      for (b in b:h) {
        
        if (b == 1) { n_out <- n_strip[[1]][b] }
        else { n_out <- paste(n_out, n_strip[[1]][b], sep = " ") }
        
      }
    }
    
    else {
      n_strip <- strsplit(n_check, split = "( )")
      b <- 1
      h <- length(n_strip[[1]])
      if (grepl("(.-.)", n_check) == TRUE) { h <- h - 1 }
      
      for (b in b:h) {
        
        if (b == 1) { n_out <- n_strip[[1]][b] }
        else { n_out <- paste(n_out, n_strip[[1]][b], sep = " ") }
        
      }
    }
    
    h_name_use <- n_out
    
    if (can_check == "PPD" | can_check == "CAN") { 
      bx_check <- "Roaro"
    }
    else { 
      bx_check <- neut_html[[(a + 2)]]  
    }
    
    if (bx_check == "Box Score") { 
      ct <- ct + 1
      box_mx[ct, 6] <<- v_name_use
      box_mx[ct, 7] <<- h_name_use
      a <- a + 6
    }
    else { 
      if (can_check == "PPD" | can_check == "CAN") { a <- a + 4 }
      else { a <- a + 5 }
    }
  }
  
  colnames(box_mx) <<- c("Date", "BOXID", "VID", "HID", "LOC", "AWAYNAME", "HOMENAME")
}


#-----------PULL BOX SCORE STATS-------------

box_html_cycle_date <- function(s, e) {

  for (s in s:e) {
    dt_ <<- as.character(date_vector_[s, 1])
    which_dt <<- which(ultra_box_filtered$Date == dt_)
    
    dt_min <<- min(which_dt)
    dt_max <<- max(which_dt)
    
    box_html_cycle(dt_min, dt_max)
    
    file_name_csv <<- paste("boxscores\\box_scores_", dt_, ".csv", sep = "")
    write_csv(ultra_box_stat_frame, file_name_csv)
    
  }
}

box_html_cycle_date_old <- function(s, e) {
  
  for (s in s:e) {
    dt_ <- as.character(date_vector_[s, 1])
    which_dt <<- which(ultra_box_filtered$Date == dt_)
    
    dt_min <<- min(which_dt)
    dt_max <<- max(which_dt)
    
    box_html_cycle_old(dt_min, dt_max)
    
    file_name_csv <<- paste("boxscores\\box_scores_", dt_, ".csv", sep = "")
    write_csv(ultra_box_stat_frame, file_name_csv)
    
  }
}

box_score_compile <- function(xxxx) {
  
  box_to_team_do(1)
  team_raw_sums_compile(1)
  lastgame_roster(1)
  
}

box_html_cycle <- function(s, e) {
  
  ct <- 0
  a <- s
  g <- e
  
  for (a in a:g) {
    initialize_box_html(a)
    
    box_teams <<- box_html %>%
      html_nodes("tr > td > a") %>%
      html_attr("href")
    
    if (length(box_teams) > 0) {
      ct <- ct + 1
      get_team_ids(1)
      get_player_ids(1)
      get_box_stats(1)
      merge_id_and_box(1)
      print(paste("Processing: ", ultra_box_filtered[a, 1], ultra_box_filtered[a, 6], ultra_box_filtered[a, 7], sep = " "))
      if (ct == 1) { ultra_box_stat_frame <<- box_stat_frame }
      else { ultra_box_stat_frame <<- rbind(ultra_box_stat_frame, box_stat_frame) }
    }
    else {
      
      print(paste("---Missing Box Score:", ultra_box_filtered[a, 1], ultra_box_filtered[a, 6], ultra_box_filtered[a, 7], sep = " "))
      
    }
  }
}

box_html_cycle_old <- function(s, e) {
  
  ct <- 0
  a <- s
  g <- e
  
  for (a in a:g) {
    initialize_box_html(a)
    
    box_teams <<- box_html %>%
      html_nodes("tr > td > a") %>%
      html_attr("href")
    
    
    if (length(box_teams) > 0) {
      ct <- ct + 1
      get_team_ids(1)
      get_player_ids(1)
      get_box_stats_old(1)
      merge_id_and_box(1)
      print(paste("Processing: ", ultra_box_filtered[a, 1], ultra_box_filtered[a, 6], ultra_box_filtered[a, 7], sep = " "))
      if (ct == 1) { ultra_box_stat_frame <<- box_stat_frame }
      else { ultra_box_stat_frame <<- rbind(ultra_box_stat_frame, box_stat_frame) }
    }
    else {
      
      print(paste("---Missing Box Score:", ultra_box_filtered[a, 1], ultra_box_filtered[a, 6], ultra_box_filtered[a, 7], sep = " "))
      
    }
  }
}

initialize_box_html <- function(act_line) {
  active_date <<- as.character(ultra_box_filtered[act_line, 1])
  active_game_id <<- strsplit(as.character(ultra_box_filtered[act_line, 2]), split = "_")
  active_game_id <<- active_game_id[[1]][3]
  active_hna <<- as.character(ultra_box_filtered[act_line, 5])
  active_no_id <<- as.numeric(ultra_box_filtered[act_line, 8])
  box_link <<- paste("https://stats.ncaa.org/contests/", active_game_id, "/box_score", sep = "")
  box_html <<- read_html(box_link)
}

get_team_ids <- function(xxxx) {
  
  box_teams <<- box_html %>%
    html_nodes("tr > td > a") %>%
    html_attr("href")
  
  v_id <<- strsplit(box_teams[[1]], "/")
  v_id <<- v_id[[1]][3]
  
  h_id <<- strsplit(box_teams[[2]], "/")
  h_id <<- h_id[[1]][3]
  
  box_names <<- box_html %>%
    html_nodes(".heading > td") %>%
    html_text()
  
  v_team <<- trim.all(box_names[[1]])
  h_team <<- trim.all(box_names[[2]])
  
}

get_player_ids <- function(xxxx) {
  
  box_hrefs <<- box_html %>%
    html_nodes("tr.smtext > td > a") %>%
    html_attr("href")
  
  non_box_hrefs <<- box_html %>%
    html_nodes(".smtext > td")
  
  table_hrefs <<- box_html %>%
    html_nodes(".mytable")
  
  away_line_ct <<- table_hrefs %>%
    extract2(2) %>%
    html_nodes(".smtext")
  
  away_line_count <<- length(away_line_ct)
  
  home_line_ct <<- table_hrefs %>%
    extract2(3) %>%
    html_nodes(".smtext")
  
  home_line_count <<- length(home_line_ct)
  
  a <- 1
  g <- length(box_hrefs)
  ct <- 0
  
  for (a in a:g) {
    
    
    ss <- box_hrefs[a]
    equal_ct <<- str_count(ss, "=") + 1
    sss <<- strsplit(ss, "=")
    id_use <<- sss[[1]][equal_ct]
    ct <- ct + 1
    if (ct == 1) { id_vector <<- c(id_use) }
    else { id_vector <<- c(id_vector, id_use) }
    
  }
  
  player_names_vector <<- box_html %>%
    html_nodes("tr.smtext > td > a") %>%
    html_text()
    
}

#2016 Usage 

get_box_stats_old <- function(xxxx) {
  
  headline_vector <<- box_html %>%
    html_nodes("th") %>%
    html_text()
  
  inc_ctr <<- length(headline_vector) / 2
  
  a_player_names_vector <<- box_html %>%
    html_nodes(".mytable") %>%
    extract(2) %>%
    html_nodes(".smtext > td")
  
  h_player_names_vector <<- box_html %>%
    html_nodes(".mytable") %>%
    extract(3) %>%
    html_nodes(".smtext > td")
  
  a <- 1
  g <- length(a_player_names_vector)
  href_counter <<- 0
  
  while (a <= g) {
    
    ht_line <<- trim.all(html_text(a_player_names_vector[[a]]))
    href_line <<- a_player_names_vector[[a]]
    href_chk <<- grepl("stats_player", href_line)
    if (href_chk == TRUE) { 
      href_counter <<- href_counter + 1
      p_id_use <<- id_vector[href_counter]
    }
    else {
      p_id_use <<- 0
    }
    
    if (a == 1) { 
      a_player_names_list <<- c(ht_line) 
      a_player_ids_list <<- c(p_id_use)
    }
    else { 
      a_player_names_list <<- c(a_player_names_list, ht_line)
      a_player_ids_list <<- c(a_player_ids_list, p_id_use)
    }
    
    a <- a + inc_ctr
  }
  
  
  
  a <- 1
  g <- length(h_player_names_vector)
  
  while (a <= g) {
    
    ht_line <<- trim.all(html_text(h_player_names_vector[[a]]))
    href_line <<- h_player_names_vector[[a]]
    href_chk <<- grepl("stats_player", href_line)
    if (href_chk == TRUE) { 
      href_counter <<- href_counter + 1
      p_id_use <<- id_vector[href_counter]
    }
    else {
      p_id_use <<- 0
    }
    
    if (a == 1) { 
      h_player_names_list <<- c(ht_line) 
      h_player_ids_list <<- c(p_id_use)
    }
    else { 
      h_player_names_list <<- c(h_player_names_list, ht_line)
      h_player_ids_list <<- c(h_player_ids_list, p_id_use)
    }
    
    a <- a + inc_ctr
  }
  
  tot_hrefs <<- box_html %>%
    html_nodes("tr.smtext > td")
  
  a <- 1
  g <- length(tot_hrefs)
  v_total_ctr <<- 0
  h_total_ctr <<- 0
  all_ctr <<- 0
  
  
  while (a <= g) {
    
    tot_check <<- trim.all(html_text(tot_hrefs[a]))
    all_ctr <<- all_ctr + 1
    if (tot_check == "TEAM") { 
      if (v_total_ctr == 0) { 
        v_total_ctr <<- all_ctr 
        all_ctr <<- 0
      }
      else { h_total_ctr <<- all_ctr }
    }
    
    a <- a + inc_ctr
  }
  
  bottom_team_ctr <<- v_total_ctr + h_total_ctr
  
  box_stat_vector <<- box_html %>%
    html_nodes("tr.smtext > td") %>%
    html_attr("data-order")
  
  #4 for 2019, 3 for previous years
  
  a <- 4
  g <- length(box_stat_vector)
  ct <- 0
  
  while (a <= g) {
    
    act_min <- box_stat_vector[(a + 0)]
    act_min2 <- strsplit(act_min, ":")
    act_min <- as.numeric(act_min2[[1]][1])
    act_sec <- as.numeric(act_min2[[1]][2])
    act_min <- act_min + (act_sec / 60)
    act_game <- box_stat_vector[(a + 1)]
    act_fgm <- as.numeric(box_stat_vector[(a + 1)])
    act_fga <- as.numeric(box_stat_vector[(a + 2)])
    act_3pm <- as.numeric(box_stat_vector[(a + 3)])
    act_3pa <- as.numeric(box_stat_vector[(a + 4)])
    if (is.na(act_3pa) == TRUE) { 
      act_3pa <- 0
      act_3pm <- 0
    }
    if (is.na(act_3pm) == TRUE) {
      act_3pm <- 0
    }
    if (is.na(act_fgm) == TRUE) {
      act_fgm <- 0
    }
    if (is.na(act_fga) == TRUE) {
      act_fgm <- 0
      act_fga <- 0
    }
    act_2pm <- act_fgm - act_3pm
    act_2pa <- act_fga - act_3pa
    act_ftm <- as.numeric(box_stat_vector[(a + 5)])
    act_fta <- as.numeric(box_stat_vector[(a + 6)])
    act_pts <- as.numeric(box_stat_vector[(a + 7)])
    act_orb <- as.numeric(box_stat_vector[(a + 8)])
    act_drb <- as.numeric(box_stat_vector[(a + 9)])
    act_trb <- as.numeric(box_stat_vector[(a + 10)])
    act_ast <- as.numeric(box_stat_vector[(a + 11)])
    act_tov <- as.numeric(box_stat_vector[(a + 12)])
    act_stl <- as.numeric(box_stat_vector[(a + 13)])
    act_blk <- as.numeric(box_stat_vector[(a + 14)])
    act_ful <- as.numeric(box_stat_vector[(a + 15)])
    
    ct <- ct + 1
    
    if (ct == 1) { 
      box_stat_frame <<- data.frame(act_min, act_fgm, act_fga, act_2pm, act_2pa, act_3pm, act_3pa, act_ftm, act_fta,
                                    act_pts, act_orb, act_drb, act_trb, act_ast, act_tov, act_stl, act_blk, act_ful,
                                    stringsAsFactors = FALSE)
    }
    else {
      box_stat_frame2 <<- data.frame(act_min, act_fgm, act_fga, act_2pm, act_2pa, act_3pm, act_3pa, act_ftm, act_fta,
                                     act_pts, act_orb, act_drb, act_trb, act_ast, act_tov, act_stl, act_blk, act_ful,
                                     stringsAsFactors = FALSE)
      box_stat_frame <<- rbind(box_stat_frame, box_stat_frame2)
    }
    
    a <- a + inc_ctr
  }
  colnames(box_stat_frame) <- c("Mins", "FGM", "FGA", "PM2", "PA2", "PM3", "PA3", "FTM", "FTA", "PTS", "ORB", "DRB", "TRB",
                                "AST", "TOV", "STL", "BLK", "FOUL")
  
  box_stat_frame <<- box_stat_frame %>% 
    replace_na(list(Mins = "-", FGM = 0, FGA = 0, PM2 = 0, PA2 = 0, PM3 = 0, PA3 = 0, FTM = 0, FTA = 0, PTS = 0, ORB = 0,
                    DRB = 0, TRB = 0, AST = 0, TOV = 0, STL = 0, BLK = 0, FOUL = 0))
  
  
}


get_box_stats <- function(xxxx) {

  headline_vector <<- box_html %>%
    html_nodes("th") %>%
    html_text()
  
  inc_ctr <<- length(headline_vector) / 2
  
  a_player_names_vector <<- box_html %>%
    html_nodes(".mytable") %>%
    extract(2) %>%
    html_nodes(".smtext > td")
  
  h_player_names_vector <<- box_html %>%
    html_nodes(".mytable") %>%
    extract(3) %>%
    html_nodes(".smtext > td")
  
  a <- 1
  g <- length(a_player_names_vector)
  href_counter <<- 0
  
  while (a <= g) {
    
    ht_line <<- trim.all(html_text(a_player_names_vector[[a]]))
    href_line <<- a_player_names_vector[[a]]
    href_chk <<- grepl("stats_player", href_line)
    if (href_chk == TRUE) { 
      href_counter <<- href_counter + 1
      p_id_use <<- id_vector[href_counter]
    }
    else {
      p_id_use <<- 0
    }
    
    if (a == 1) { 
      a_player_names_list <<- c(ht_line) 
      a_player_ids_list <<- c(p_id_use)
    }
    else { 
      a_player_names_list <<- c(a_player_names_list, ht_line)
      a_player_ids_list <<- c(a_player_ids_list, p_id_use)
    }
    
    a <- a + inc_ctr
  }
  
  
  
  a <- 1
  g <- length(h_player_names_vector)
  
  while (a <= g) {
    
    ht_line <<- trim.all(html_text(h_player_names_vector[[a]]))
    href_line <<- h_player_names_vector[[a]]
    href_chk <<- grepl("stats_player", href_line)
    if (href_chk == TRUE) { 
      href_counter <<- href_counter + 1
      p_id_use <<- id_vector[href_counter]
    }
    else {
      p_id_use <<- 0
    }
    
    if (a == 1) { 
      h_player_names_list <<- c(ht_line) 
      h_player_ids_list <<- c(p_id_use)
    }
    else { 
      h_player_names_list <<- c(h_player_names_list, ht_line)
      h_player_ids_list <<- c(h_player_ids_list, p_id_use)
    }
    
    a <- a + inc_ctr
  }
  
  tot_hrefs <<- box_html %>%
    html_nodes("tr.smtext > td")
  
  a <- 1
  g <- length(tot_hrefs)
  v_total_ctr <<- 0
  h_total_ctr <<- 0
  all_ctr <<- 0
  
  
  while (a <= g) {
    
    tot_check <<- trim.all(html_text(tot_hrefs[a]))
    all_ctr <<- all_ctr + 1
    if (tot_check == "TEAM") { 
      if (v_total_ctr == 0) { 
        v_total_ctr <<- all_ctr 
        all_ctr <<- 0
      }
      else { h_total_ctr <<- all_ctr }
    }
    
    a <- a + inc_ctr
  }
  
  bottom_team_ctr <<- v_total_ctr + h_total_ctr
  
  box_stat_vector <<- box_html %>%
    html_nodes("tr.smtext > td") %>%
    html_attr("data-order")
  
  a <- 3
  g <- length(box_stat_vector)
  ct <- 0
  
  while (a <= g) {
    
     act_min <- box_stat_vector[(a + 0)]
     act_min2 <- strsplit(act_min, ":")
     act_min <- as.numeric(act_min2[[1]][1])
     act_sec <- as.numeric(act_min2[[1]][2])
     act_min <- act_min + (act_sec / 60)
     act_game <- box_stat_vector[(a + 1)]
     act_fgm <- as.numeric(box_stat_vector[(a + 2)])
     act_fga <- as.numeric(box_stat_vector[(a + 3)])
     act_3pm <- as.numeric(box_stat_vector[(a + 4)])
     act_3pa <- as.numeric(box_stat_vector[(a + 5)])
     if (is.na(act_3pa) == TRUE) { 
       act_3pa <- 0
       act_3pm <- 0
     }
     if (is.na(act_3pm) == TRUE) {
       act_3pm <- 0
     }
     if (is.na(act_fgm) == TRUE) {
       act_fgm <- 0
     }
     if (is.na(act_fga) == TRUE) {
       act_fgm <- 0
       act_fga <- 0
     }
     act_2pm <- act_fgm - act_3pm
     act_2pa <- act_fga - act_3pa
     act_ftm <- as.numeric(box_stat_vector[(a + 6)])
     act_fta <- as.numeric(box_stat_vector[(a + 7)])
     act_pts <- as.numeric(box_stat_vector[(a + 8)])
     act_orb <- as.numeric(box_stat_vector[(a + 9)])
     act_drb <- as.numeric(box_stat_vector[(a + 10)])
     act_trb <- as.numeric(box_stat_vector[(a + 11)])
     act_ast <- as.numeric(box_stat_vector[(a + 12)])
     act_tov <- as.numeric(box_stat_vector[(a + 13)])
     act_stl <- as.numeric(box_stat_vector[(a + 14)])
     act_blk <- as.numeric(box_stat_vector[(a + 15)])
     act_ful <- as.numeric(box_stat_vector[(a + 16)])
     
     ct <- ct + 1
     
     if (ct == 1) { 
       box_stat_frame <<- data.frame(act_min, act_fgm, act_fga, act_2pm, act_2pa, act_3pm, act_3pa, act_ftm, act_fta,
                                                  act_pts, act_orb, act_drb, act_trb, act_ast, act_tov, act_stl, act_blk, act_ful,
                                     stringsAsFactors = FALSE)
     }
     else {
       box_stat_frame2 <<- data.frame(act_min, act_fgm, act_fga, act_2pm, act_2pa, act_3pm, act_3pa, act_ftm, act_fta,
                                     act_pts, act_orb, act_drb, act_trb, act_ast, act_tov, act_stl, act_blk, act_ful,
                                     stringsAsFactors = FALSE)
       box_stat_frame <<- rbind(box_stat_frame, box_stat_frame2)
     }
    
     a <- a + inc_ctr
  }
  colnames(box_stat_frame) <- c("Mins", "FGM", "FGA", "PM2", "PA2", "PM3", "PA3", "FTM", "FTA", "PTS", "ORB", "DRB", "TRB",
                                "AST", "TOV", "STL", "BLK", "FOUL")
  
  box_stat_frame <<- box_stat_frame %>% 
    replace_na(list(Mins = "-", FGM = 0, FGA = 0, PM2 = 0, PA2 = 0, PM3 = 0, PA3 = 0, FTM = 0, FTA = 0, PTS = 0, ORB = 0,
                    DRB = 0, TRB = 0, AST = 0, TOV = 0, STL = 0, BLK = 0, FOUL = 0))
  
  
}

merge_id_and_box <- function(xxxx) {
  
  a <- 1
  g <- nrow(box_stat_frame)
  ct <- 0
  act_id <- v_id
  act_opp <- h_id
  act_name <- v_team
  act_oname <- h_team
  if (act_name == "Sam Houston St.") { act_name <- "Sam Houston" }
  if (act_oname == "Sam Houston St.") { act_oname <- "Sam Houston" }
  if (active_hna == "H") { act_loc <- "A" }
  else { act_loc <- "N" }
  v_total_ctr <<- away_line_count
  running_count <<- 0
  running_side <<- 1
  
  for (a in a:g) {
    
    running_count <<- running_count + 1
    act_min <- box_stat_frame[a, 1]
    if (a == v_total_ctr) {
      box_stat_frame[a, 19] <<- a_player_names_list[running_count]
      box_stat_frame[a, 20] <<- a_player_ids_list[running_count]
      box_stat_frame[a, 21] <<- act_id
      box_stat_frame[a, 22] <<- act_opp
      box_stat_frame[a, 23] <<- act_name
      box_stat_frame[a, 24] <<- act_oname
      box_stat_frame[a, 25] <<- active_date
      box_stat_frame[a, 26] <<- active_game_id
      box_stat_frame[a, 27] <<- active_no_id
      box_stat_frame[a, 28] <<- act_loc
      act_id <- h_id
      act_opp <- v_id
      act_name <- h_team
      act_oname <- v_team
      if (act_loc == "A") { act_loc <- "H" }
      running_count <<- 0
      running_side <<- running_side + 1
    }
    else {
      
      ct <- ct + 1
      if (running_side == 1) {
        box_stat_frame[a, 19] <<- a_player_names_list[running_count]
        box_stat_frame[a, 20] <<- a_player_ids_list[running_count]
      }
      else {
        box_stat_frame[a, 19] <<- h_player_names_list[running_count]
        box_stat_frame[a, 20] <<- h_player_ids_list[running_count]
      }
      box_stat_frame[a, 21] <<- act_id
      box_stat_frame[a, 22] <<- act_opp
      box_stat_frame[a, 23] <<- act_name
      box_stat_frame[a, 24] <<- act_oname
      box_stat_frame[a, 25] <<- active_date
      box_stat_frame[a, 26] <<- active_game_id
      box_stat_frame[a, 27] <<- active_no_id
      box_stat_frame[a, 28] <<- act_loc
      
    }
  }
  
  colnames(box_stat_frame)[19:28] <<- c("PLAYER_NAME", "PLAYER_ID", "TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "DATE", "GAME_ID", "NO_ID", "LOC")
  
  v_box_end <<- which(box_stat_frame$Mins == "-")
  if (length(v_box_end) <= 0) {
    
    v_box_end2 <<- which(box_stat_frame$PLAYER_NAME == "TEAM")
    v_box_end <<- v_box_end2 
    
  }
  v_box_end <<- v_box_end[1]
  h_box_start <<- v_box_end + 1
  box_len <<- nrow(box_stat_frame)
  
  v_box_frame <<- box_stat_frame[1:v_box_end, ]
  h_box_frame <<- box_stat_frame[h_box_start:box_len, ]
}

tmp_box_split <- function(xxxx) {
  #Only Used Once, No Need to Use Again
  tmp_box_frame <- read_csv("boxscores\\box_group1.csv")
  
  a <- 1
  g <- 52
  
  for (a in a:g) {
    
    dt_check <<- as.character(date_vector_[a, 1]) 
    subset_dt <<- tmp_box_frame %>%
      filter(DATE == dt_check)
    
    file_name_csv <<- paste("boxscores\\box_scores_", dt_check, ".csv", sep = "")
    write_csv(subset_dt, file_name_csv)
    
  }
}

#Make a Script that Loads all Box Scores for the Season, Puts them in a frame and then find a way to group NCAA ids

box_fix_old <- function(xxxx) {
  
  #Load Box Scores into List
  a <- 1
  g <- nrow(date_vector_)
  
  for (a in a:g) {
    
    dt_check <<- as.character(date_vector_[a, 1])
    file_name_csv <<- paste("boxscores\\box_scores_", dt_check, ".csv", sep = "")
    box_frame <<- read_csv(file_name_csv)
    
    bx_ids <<- box_frame %>%
      select(TEAM_ID, TEAM_NAME) %>%
      group_by(TEAM_ID, TEAM_NAME) %>%
      summarize(ct = n())
    
    if (a == 1) { bx_idfix <<- bx_ids }
    else { bx_idfix <<- rbind(bx_idfix, bx_ids) }
  }
  
  bx_id_master <<- bx_idfix %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(ct = n())
}

box_to_team_old <- function(xxxx) {
  
  #Load Box Scores into List
  a <- 1
  g <- nrow(date_vector_)
  names_csv <<- read_csv("names.csv")
  
  for (a in a:g) {
    
    dt_check <<- as.character(date_vector_[a, 1])
    file_name_csv <<- paste("boxscores\\box_scores_", dt_check, ".csv", sep = "")
    box_frame <<- read_csv(file_name_csv)
    
    #You Can Take This Out After the Original Fix
    
    pa3_fix <<- box_frame[, 7]
    pm3_fix <<- box_frame[, 6]
    fgm_fix <<- box_frame[, 2]
    fga_fix <<- box_frame[, 3]
    
    new_pa2 <<- fga_fix - pa3_fix
    new_pm2 <<- fgm_fix - pm3_fix
    
    box_frame[, 4] <<- new_pm2
    box_frame[, 5] <<- new_pa2
    
    #-------
    
    if (a == 1) { box_scores_datelist <<- list(box_frame) }
    else { box_scores_datelist[[a]] <<- box_frame }
    
  }
  
  box_dte_mx <<- as.matrix(ultra_box_filtered[, 1])
  box_vis_mx <<- as.matrix(ultra_box_filtered[, 3])
  box_hom_mx <<- as.matrix(ultra_box_filtered[, 4])
  box_id_mx <<- as.matrix(ultra_box_filtered[, 2])
  a <- 1
  g <- nrow(box_id_mx)
  
  for (a in a:g) {
    id_line <- box_id_mx[a]
    box_id_mx[a] <<- strsplit(id_line, "_")[[1]][3]
  }
  
  a <- 1
  g <- nrow(teamid_box_sum)
  #a <- 176
  #g <- 176
  
  for (a in a:g) {
    team_id_lu_new <<- as.character(teamid_box_sum[a, 1])
    team_id_lu_old <<- which(names_csv[, 3] == team_id_lu_new)
    team_id_lu_old <<- as.character(names_csv[team_id_lu_old, 8])
    which_vis <<- which(box_vis_mx == team_id_lu_new)
    which_hom <<- which(box_hom_mx == team_id_lu_new)
    which_comb <<- sort(c(which_vis, which_hom))
    which_dt <<- box_dte_mx[which_comb]
    which_id <<- box_id_mx[which_comb]
    
    b <- 1
    h <- length(which_dt)
    for (b in b:h) {
      
      d <- which_dt[b]
      id_lu <<- which_id[b]
      date_line <<- which(date_vector_$Date == d)
      date_frm <<- box_scores_datelist[[date_line]]
      date_frm_subset_old <<- date_frm[which(date_frm$TEAM_ID == team_id_lu_old & date_frm$GAME_ID == id_lu), ]
      date_frm_subset_new <<- date_frm[which(date_frm$TEAM_ID == team_id_lu_new & date_frm$GAME_ID == id_lu), ]
      
      if (nrow(date_frm_subset_old) > 0) { date_frm_subset <<- date_frm_subset_old }
      else { date_frm_subset <<- date_frm_subset_new }
      date_frm_subset$TEAM_ID <<- team_id_lu_new
      t_actual_name <<- as.character(names_csv[which(names_csv[, 3] == as.numeric(team_id_lu_new)), 4])
      date_frm_subset[, 23] <<- t_actual_name
      
      opp_id_frm <<- as.numeric(date_frm_subset$OPP_ID[1])
      if (opp_id_frm < 100000) {
        
        opp_id_lu_old <<- which(names_csv[, 8] == as.character(opp_id_frm))
        opp_id_lu_old <<- as.character(names_csv[opp_id_lu_old, 3])
        date_frm_subset$OPP_ID <<- opp_id_lu_old
        opp_id_frm <<- opp_id_lu_old
        
      }
      
      o_actual_name <<- as.character(names_csv[which(names_csv[, 3] == as.numeric(opp_id_frm)), 4])
      date_frm_subset[, 24] <<- o_actual_name
      
      if (b == 1) { team_frame_compile <<- date_frm_subset }
      else { team_frame_compile <<- rbind(team_frame_compile, date_frm_subset) }
      
    }
    file_name_csv <<- paste("teamscores\\team_scores_", team_id_lu_new, ".csv", sep = "")
    write_csv(team_frame_compile, file_name_csv)
  }
}

box_to_team_do <- function(xxxx) {
  
  #Load Box Scores into List
  a <- 1
  g <- nrow(date_vector_)
  names_csv <<- read_csv("names.csv")
  
  for (a in a:g) {
    
    print(a)
    dt_check <<- as.character(date_vector_[a, 1])
    file_name_csv <<- paste("boxscores\\box_scores_", dt_check, ".csv", sep = "")
    box_frame <<- read_csv(file_name_csv)
    
    #You Can Take This Out After the Original Fix
    
    pa3_fix <<- box_frame[, 7]
    pm3_fix <<- box_frame[, 6]
    fgm_fix <<- box_frame[, 2]
    fga_fix <<- box_frame[, 3]
    
    new_pa2 <<- fga_fix - pa3_fix
    new_pm2 <<- fgm_fix - pm3_fix
    
    box_frame[, 4] <<- new_pm2
    box_frame[, 5] <<- new_pa2
    
    #-------
    
    if (a == 1) { box_scores_datelist <<- list(box_frame) }
    else { box_scores_datelist[[a]] <<- box_frame }
    
  }
  
  box_dte_mx <<- as.matrix(ultra_box_filtered[, 1])
  box_vis_mx <<- as.matrix(ultra_box_filtered[, 3])
  box_hom_mx <<- as.matrix(ultra_box_filtered[, 4])
  
  a <- 1
  g <- nrow(teamid_box_sum)
  
  for (a in a:g) {
    team_id_lu <<- as.character(teamid_box_sum[a, 1])
    which_vis <<- which(box_vis_mx == team_id_lu)
    which_hom <<- which(box_hom_mx == team_id_lu)
    which_comb <<- sort(c(which_vis, which_hom))
    which_dt <<- box_dte_mx[which_comb]
    
    b <- 1
    h <- length(which_dt)
    for (b in b:h) {
      
      d <- which_dt[b]
      date_line <<- which(date_vector_$Date == d)
      date_frm <<- box_scores_datelist[[date_line]]
      date_frm_subset <<- date_frm[which(date_frm$TEAM_ID == team_id_lu), ]
      
      
      
      if (b == 1) { team_frame_compile <<- date_frm_subset }
      else { team_frame_compile <<- rbind(team_frame_compile, date_frm_subset) }
      
      
      
    }
    file_name_csv <<- paste("teamscores\\team_scores_", team_id_lu, ".csv", sep = "")
    write_csv(team_frame_compile, file_name_csv)
  }
}

team_raw_sums_compile <- function(xxxx) {
  
  a <- 1
  g <- nrow(teamid_box_sum)
  #a <- 176
  #g <- 176
  bbc <- 1
  
  for (a in a:g) {
    
    t_id <<- teamid_box_sum[a, 1]
    file_name_csv <<- paste("teamscores\\team_scores_", t_id, ".csv", sep = "")
    t_csv_frame <<- read_csv(file_name_csv)
    which_hyphen <<- which(t_csv_frame$Mins == "-")
    t_csv_frame$Mins[which_hyphen] <<- 0
    t_csv_minsframe <<- as.matrix(t_csv_frame[, 1])
    t_csv_minsframe <<- as.numeric(t_csv_minsframe)
    t_csv_frame[, 1] <<- t_csv_minsframe
    
    t_actual_name <<- as.character(names_csv[which(names_csv[, 3] == as.numeric(t_id)), 4])
    t_csv_frame[, 23] <<- t_actual_name
    
    if (nrow(t_csv_frame) > 0) {
    
      t_raw_stats_game <<- t_csv_frame %>%
        group_by(TEAM_ID, OPP_ID, TEAM_NAME, OPP_NAME, DATE, GAME_ID, LOC) %>%
        summarize(NO_ID = mean(NO_ID), MINS = sum(Mins), PTS = sum(PTS), FGM = sum(FGM), FGA = sum(FGA), PM2 = sum(PM2), PA2 = sum(PA2), PM3 = sum(PM3), PA3 = sum(PA3),
                  FTM = sum(FTM), FTA = sum(FTA), ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB),
                  AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TO = sum(TOV), FOUL = sum(FOUL)) %>%
        arrange(NO_ID)
      
      i_totals_raw <<- t_csv_frame %>%
        group_by(TEAM_ID, TEAM_NAME, PLAYER_ID) %>%
        summarize(GAMES = n(), MINS = sum(Mins), PTS = sum(PTS), FGM = sum(FGM), FGA = sum(FGA), PM2 = sum(PM2), PA2 = sum(PA2), PM3 = sum(PM3), PA3 = sum(PA3),
                  FTM = sum(FTM), FTA = sum(FTA), ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB),
                  AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TO = sum(TOV), FOUL = sum(FOUL)) %>%
        arrange(desc(MINS))
      
      if (a == bbc) { 
        team_raw_stats_game <<- t_raw_stats_game
        ind_raw_stats_game <<- i_totals_raw
        team_box_scorelist <<- list(t_csv_frame)
      }
      else { 
        team_raw_stats_game <<- rbind(team_raw_stats_game, t_raw_stats_game) 
        ind_raw_stats_game <<- rbind(ind_raw_stats_game, i_totals_raw)
        team_box_scorelist[[a]] <<- t_csv_frame
      }
    }
  }
  
  less_than_200 <<- which(team_raw_stats_game$MINS < 200)
  team_raw_stats_game$MINS[less_than_200] <<- 200
  
  opp_raw_stats_game <<- team_raw_stats_game[, c(1, 6, 9:26)]
  
  team_game_by_game_raw <<- team_raw_stats_game %>% 
    left_join(opp_raw_stats_game, by = c("OPP_ID" = "TEAM_ID", "GAME_ID"), suffix = c("", "d"))
  
  team_totals_raw <<- team_game_by_game_raw %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(MINS = sum(MINS), PTS = sum(PTS), FGM = sum(FGM), FGA = sum(FGA), PM2 = sum(PM2), PA2 = sum(PA2), PM3 = sum(PM3), PA3 = sum(PA3),
              FTM = sum(FTM), FTA = sum(FTA), ORB = sum(ORB), DRB = sum(DRB), TRB = sum(TRB),
              AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TO = sum(TO), FOUL = sum(FOUL),
              oPTS = sum(PTSd), oFGM = sum(FGMd), oFGA = sum(FGAd), oPM2 = sum(PM2d), oPA2 = sum(PA2d), oPM3 = sum(PM3d), oPA3 = sum(PA3d),
              oFTM = sum(FTMd), oFTA = sum(FTAd), oORB = sum(ORBd), oDRB = sum(DRBd), oTRB = sum(TRBd),
              oAST = sum(ASTd), oSTL = sum(STLd), oBLK = sum(BLKd), oTO = sum(TOd), oFOUL = sum(FOULd))
  
  ind_totals_raw <<- ind_raw_stats_game

}

lastgame_roster <- function(xxxx) {
  
  a <- 1
  g <- nrow(teamid_box_sum)
  ct <- 0
  
  for (a in a:g) {
    
    vid_use <<- as.numeric(teamid_box_sum$VID[a])
    file_name_csv <<- paste("teamscores\\team_scores_", vid_use, ".csv", sep = "")
    t_csv_frame <<- read_csv(file_name_csv)
    which_hyphen <<- which(t_csv_frame$Mins == "-")
    t_csv_frame$Mins[which_hyphen] <<- 0
    t_csv_frame$Mins <<- lapply(t_csv_frame$Mins, as.numeric)
    
    t_csv_noids <<- t_csv_frame %>%
      group_by(NO_ID) %>%
      summarize(ct = n()) %>%
      arrange(NO_ID)
    
    t_csv_max <<- nrow(t_csv_noids)
    t_csv_min <<- t_csv_max - 2
    if (t_csv_min <= 0) { t_csv_min <<- 1 }
    
    t_csv_high <<- as.numeric(t_csv_noids[t_csv_max, 1])
    t_csv_low <<- as.numeric(t_csv_noids[t_csv_min, 1])
    
    t_csv_frame$Mins <<- as.numeric(t_csv_frame$Mins)
    
    t_csv_last <<- t_csv_frame %>%
      filter(NO_ID >= t_csv_low & NO_ID <= t_csv_high) %>%
      group_by(PLAYER_ID, TEAM_ID) %>%
      summarize(Mins = sum(Mins))
    
    t_mins_sums <<- sum(t_csv_last$Mins) / 5
    
    t_csv_last$Mins <<- t_csv_last$Mins / t_mins_sums
    
    t_csv_last <<- t_csv_last %>%
      filter(Mins > 0)
    
    if (nrow(t_csv_last) > 0) {
      
      ct <- ct + 1
      if (ct == 1) { lastgame_rosters <<- t_csv_last }
      
      else { lastgame_rosters <<- rbind(lastgame_rosters, t_csv_last) }
      
    }
  }
}


#MERGE WITH TRANSFORM SCRIPT

#Box Score Mistake Checker

box_score_mistake <- function(xxxx) {

  #Load Box Scores into List
  a <- 2
  g <- nrow(date_vector_)
  g <- 2
  
  for (a in a:g) {
    
    dt_check <<- as.character(date_vector_[a, 1])
    file_name_csv <<- paste("boxscores\\box_scores_", dt_check, ".csv", sep = "")
    box_frame <<- read_csv(file_name_csv)
    
    b <- 1
    h <- nrow(box_frame)
    btb_switch <<- 0
    
    for (b in b:h) {
      
      tx <<- as.character(box_frame[b, 19])
      tf <<- is.na(tx)
      if (tf == TRUE) { 
        btb_switch <<- btb_switch + 1
        if (btb_switch > 1) { print(paste(b, dt_check, sep = "---")) }
      }
      else {
        btb_switch <<- 0
      }
    }
  }
}

#Coach Scraping

coach_cycle <- function(s, e) {
  
  year_vector <<- c(2017, 2018, 2019, 2020, 2021)
  
  a <- s
  g <- e
  
  for (a in a:g) {
    
    y <- year_vector[a]
    names_file <<- read_csv(paste("old files\\names - ", y, ".csv", sep = ""))
    coach_vector <<- names_file
    
    b <- 1
    h <- nrow(names_file)
    
    for (b in b:h) {
      
      id_use <<- as.numeric(names_file[b, 3])
      tm_use <<- as.character(names_file[b, 4])
      if (is.na(id_use) == FALSE & id_use > 0) {
        
        print(paste("Coach Retrieve: ", y, " - ", id_use, " - ", tm_use, sep = ""))
        coach_use <<- coach_scraper(id_use)
        coach_vector[b, 9] <<- coach_use[[1]]
        
        
      }
    }
  }
}

coach_scraper <- function(xxxx) {
  
  test_link <<- paste("https://stats.ncaa.org/teams/", xxxx, sep = "")
  test_html <<- read_html(test_link)
  
  coach_html <<- test_html %>%
    html_nodes("fieldset > a") %>%
    html_text()
  
}

#--------------BACKUP SCRAPER-----------------


sref_game_day_scores <- function(m, d, y) {
  
  
  mm_ <- m
  dd_ <- d
  yy_ <- y
  if (nchar(mm_) == 1) { mm <- paste(0, mm_, sep = "") }
  if (nchar(dd_) == 1) { dd <- paste(0, dd_, sep = "") }

  test_link <<- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=", m, "&day=", d, "&year=", y, sep = "")
  
  
  test_html <<- read_html(test_link)

  boxscore_list <<- test_html %>%
    html_nodes(".gamelink > a") %>%
    html_attrs()
  
}

sref_box_cycle <- function(xxxx) {
  
  a <- 1
  g <- 1
  
  for (a in a:g) {
    
    sref_link <- boxscore_list[[a]]
    box_link <<- paste("https://www.sports-reference.com", sref_link, sep = "")
    box_html <<- read_html(box_link)
    
    #Position 5
    name_attrs <<- box_html %>%
      html_nodes(".left") %>%
      html_attrs()

    
    
    stat_attrs <<- box_html %>%
      html_nodes("td.right") %>%
      html_attrs()
    
    stat_text <<- box_html %>%
      html_nodes("td.right") %>%
      html_text()
    
    sref_stat_text_strip(1)
  }
}

sref_stat_text_strip <- function(xxxx) {
  
  a <- 9
  g <- length(stat_attrs)
  
  sref_stat_mx_away <<- matrix(0, nrow = 30, ncol = 18)
  sref_stat_mx_home <<- matrix(0, nrow = 30, ncol = 18)
  
  act_away_slot <<- 0
  act_home_slot <<- 0
  
  count_200 <- 0
  
  while (a <= g) {
    
    
    
    data_stat <- stat_attrs[[a]][2]
    if (data_stat == "mp") { 
      mins_played <- as.numeric(stat_text[[a]])
      if (mins_played > 150) { count_200 <- count_200 + 1 }
      if (count_200 == 0) { 
        act_away_slot <<- act_away_slot + 1
        sref_stat_mx_away[act_away_slot, 1] <<- mins_played
      }
      if (count_200 == 2) {
        act_home_slot <<- act_home_slot + 1
        sref_stat_mx_home[act_home_slot, 1] <<- mins_played
      }
    }
    
    #mp, fg, fga, fg2, fg2a, fg3, fg3a, ft, fta, orb, drb, trb, ast, stl, blk, tov, pf, pts
    
    if (data_stat == "fg") { sref_stat_add(count_200, "fg", stat_text[[a]]) }
    
    a <- a + 1
    
  }
}

sref_stat_add <- function(count_200, stat_type, stat_val) {
  
  
  if (count_200 == 0) {
    if (stat_type == "fg") { sa_slot <<- 2 }
    sref_stat_mx_away[act_away_slot, sa_slot] <<- stat_val
  }
  
  if (count_200 == 2) {
    if (stat_type == "fg") { sa_slot <<- 2 }
    sref_stat_mx_home[act_home_slot, sa_slot] <<- stat_val
  }
}

box_to_team_rehab <- function(xxxx) {
  
  date_vector2 <- date_vector_[1:xxxx, ]
  #Load Box Scores into List
  a <- 1
  g <- nrow(date_vector2)
  names_csv <<- read_csv("names.csv")
  
  for (a in a:g) {
    
    print(a)
    dt_check <<- as.character(date_vector2[a, 1])
    file_name_csv <<- paste("boxscores\\box_scores_", dt_check, ".csv", sep = "")
    box_frame <<- read_csv(file_name_csv)
    
    #You Can Take This Out After the Original Fix
    
    pa3_fix <<- box_frame[, 7]
    pm3_fix <<- box_frame[, 6]
    fgm_fix <<- box_frame[, 2]
    fga_fix <<- box_frame[, 3]
    
    new_pa2 <<- fga_fix - pa3_fix
    new_pm2 <<- fgm_fix - pm3_fix
    
    box_frame[, 4] <<- new_pm2
    box_frame[, 5] <<- new_pa2
    
    #-------
    
    if (a == 1) { box_scores_datelist <<- list(box_frame) }
    else { box_scores_datelist[[a]] <<- box_frame }
    
  }
  
  box_dte_mx <<- as.matrix(ultra_box_filtered[, 1])
  box_vis_mx <<- as.matrix(ultra_box_filtered[, 3])
  box_hom_mx <<- as.matrix(ultra_box_filtered[, 4])
  
  a <- 1
  g <- nrow(teamid_box_sum)
  
  for (a in a:g) {
    team_id_lu <<- as.character(teamid_box_sum[a, 1])
    which_vis <<- which(box_vis_mx == team_id_lu)
    which_hom <<- which(box_hom_mx == team_id_lu)
    which_comb <<- sort(c(which_vis, which_hom))
    which_dt <<- box_dte_mx[which_comb]
    
    b <- 1
    h <- length(which_dt)
    for (b in b:h) {
      
      d <- which_dt[b]
      date_line <<- which(date_vector2$Date == d)
      if (length(date_line) > 0) {
        date_frm <<- box_scores_datelist[[date_line]]
        date_frm_subset <<- date_frm[which(date_frm$TEAM_ID == team_id_lu), ]
        
        if (b == 1) { team_frame_compile <<- date_frm_subset }
        else { team_frame_compile <<- rbind(team_frame_compile, date_frm_subset) }
      }
    }
    file_name_csv <<- paste("teamscores\\team_scores_", team_id_lu, ".csv", sep = "")
    write_csv(team_frame_compile, file_name_csv)
    #print(paste(team_frame_compile[nrow(team_frame_compile), 25], team_frame_compile[nrow(team_frame_compile), 23]))
  }
  
  team_raw_sums_compile(1)
  lastgame_roster(1)
}

rehab_odds_get <- function(m, d, y) {
  
  tmp_odds <<- read_csv("formatted_wpt5.csv")
  
  tmp_odds <<- tmp_odds %>%
    filter(Month == m & Day == d & Year == y) %>%
    select(Month, Day, Year, Spread, AML, HML, Total, Loc, Away, Home, AID, HID)
  
  res_log <- team_game_by_game_raw[, c(3, 5, 10)]
  
  res_log <- res_log %>%
    left_join(names_csv[, c(1, 4)], by = c("TEAM_NAME" = "NCAA ID")) %>%
    select(`Ken Pom`, DATE, PTS) %>%
    mutate(DATE2 = mdy(DATE)) %>%
    select(`Ken Pom`, DATE2, PTS) %>%
    mutate(Month = month(DATE2), Day = day(DATE2), Year = year(DATE2)) %>%
    select(-DATE2)
  
  live_log_join <- tmp_odds %>%
    inner_join(res_log, by = c("Away" = "Ken Pom", "Month", "Day", "Year")) %>%
    inner_join(res_log, by = c("Home" = "Ken Pom", "Month", "Day", "Year")) %>%
    select(Month, Day, Year, Spread, AML, HML, Total, Loc, Away, Home, AID, HID, PTS.x, PTS.y)
  
  View(live_log_join)
  
  write_csv(live_log_join, "tmp_odds.csv", append=TRUE)
  
}


rehab_livelog_ats <- function(xxxx) {
  
  livelog_rehab <- read_csv("livelogrehab.csv")
  liveline_rehab <- read_csv("livelinerehab.csv")
  
  rehab_master_ats1 <- master_df_lineup_ats %>%
    inner_join(liveline_rehab, by = c("TEAM", "OPP", "DAY")) %>%
    mutate(EXPINDY = INDY + LINE, EXPTEAMC = TEAMC + LINE, EXPBLND = BLND + LINE, EXPATM = ATM + LINE, EXPSIM = SIM + LINE, EXPCOMB = COMB + LINE)
  
  rehab_master_ovr <- master_df_lineup_tot %>%
    inner_join(liveline_rehab, by = c("TEAM", "OPP", "DAY")) %>%
    filter(BTYPE == "OVER") %>%
    mutate(EXPINDY = INDY + TOTAL, EXPTEAMC = TEAMC + TOTAL, EXPBLND = BLND + TOTAL, EXPATM = ATM + TOTAL, EXPSIM = SIM + TOTAL, EXPCOMB = COMB + TOTAL)
  
  rehab_master_und <- master_df_lineup_tot %>%
    inner_join(liveline_rehab, by = c("TEAM", "OPP", "DAY")) %>%
    filter(BTYPE == "UNDER") %>%
    mutate(EXPINDY = TOTAL - INDY, EXPTEAMC = TOTAL - TEAMC, EXPBLND = TOTAL - BLND, EXPATM = TOTAL - ATM, EXPSIM = TOTAL - SIM, EXPCOMB = TOTAL - COMB)
  
  rehab_away_ats1 <- livelog_rehab %>%
    filter(AORH == "A") %>%
    inner_join(rehab_master_ats1, by = c("AWAY" = "TEAM", "HOME" = "OPP", "DAY")) %>%
    mutate(IATSMARG = (EXPINDY * PCTREM + CURMARG) + LIVELINE,
           TATSMARG = (EXPTEAMC * PCTREM + CURMARG) + LIVELINE,
           BATSMARG = (EXPBLND * PCTREM + CURMARG) + LIVELINE,
           AATSMARG = (EXPATM * PCTREM + CURMARG) + LIVELINE,
           SATSMARG = (EXPSIM * PCTREM + CURMARG) + LIVELINE,
           CATSMARG = (EXPCOMB * PCTREM + CURMARG) + LIVELINE) %>%
    select(BETTYPE, DAY, AORH, AWAY, HOME, LIVELINE, CURMARG, PCTREM, IATSMARG, TATSMARG, BATSMARG, AATSMARG, SATSMARG, CATSMARG)
  
  rehab_away_ats2 <- livelog_rehab %>%
    filter(AORH == "H") %>%
    inner_join(rehab_master_ats1, by = c("HOME" = "TEAM", "AWAY" = "OPP", "DAY")) %>%
    mutate(IATSMARG = (EXPINDY * PCTREM + CURMARG) + LIVELINE,
           TATSMARG = (EXPTEAMC * PCTREM + CURMARG) + LIVELINE,
           BATSMARG = (EXPBLND * PCTREM + CURMARG) + LIVELINE,
           AATSMARG = (EXPATM * PCTREM + CURMARG) + LIVELINE,
           SATSMARG = (EXPSIM * PCTREM + CURMARG) + LIVELINE,
           CATSMARG = (EXPCOMB * PCTREM + CURMARG) + LIVELINE) %>%
    select(BETTYPE, DAY, AORH, AWAY, HOME, LIVELINE, CURMARG, PCTREM, IATSMARG, TATSMARG, BATSMARG, AATSMARG, SATSMARG, CATSMARG)
  
  rehab_away_tot1 <- livelog_rehab %>%
    filter(AORH == "O") %>%
    inner_join(rehab_master_ovr, by = c("AWAY" = "TEAM", "HOME" = "OPP", "DAY")) %>%
    mutate(IATSMARG = (EXPINDY * PCTREM + CURMARG) - LIVELINE,
           TATSMARG = (EXPTEAMC * PCTREM + CURMARG) - LIVELINE,
           BATSMARG = (EXPBLND * PCTREM + CURMARG) - LIVELINE,
           AATSMARG = (EXPATM * PCTREM + CURMARG) - LIVELINE,
           SATSMARG = (EXPSIM * PCTREM + CURMARG) - LIVELINE,
           CATSMARG = (EXPCOMB * PCTREM + CURMARG) - LIVELINE) %>%
    select(BETTYPE, DAY, AORH, AWAY, HOME, LIVELINE, CURMARG, PCTREM, IATSMARG, TATSMARG, BATSMARG, AATSMARG, SATSMARG, CATSMARG)
  
  rehab_away_tot2 <- livelog_rehab %>%
    filter(AORH == "U") %>%
    inner_join(rehab_master_ovr, by = c("AWAY" = "TEAM", "HOME" = "OPP", "DAY")) %>%
    mutate(IATSMARG = LIVELINE - (EXPINDY * PCTREM + CURMARG),
           TATSMARG = LIVELINE - (EXPTEAMC * PCTREM + CURMARG),
           BATSMARG = LIVELINE - (EXPBLND * PCTREM + CURMARG),
           AATSMARG = LIVELINE - (EXPATM * PCTREM + CURMARG),
           SATSMARG = LIVELINE - (EXPSIM * PCTREM + CURMARG),
           CATSMARG = LIVELINE - (EXPCOMB * PCTREM + CURMARG)) %>%
    select(BETTYPE, DAY, AORH, AWAY, HOME, LIVELINE, CURMARG, PCTREM, IATSMARG, TATSMARG, BATSMARG, AATSMARG, SATSMARG, CATSMARG)

  rehab_comb <- rbind(rehab_away_ats1, rehab_away_ats2, rehab_away_tot1, rehab_away_tot2)
  
  write_csv(rehab_comb, "rehablog.csv")

}

date_rehab <- function(xxxx) {
  
  master_box_ids$Date <<- as.character(master_box_ids$Date)
  a <- 1
  g <- nrow(master_box_ids)
  
  for (a in a:g) {
    
    d <- as.character(master_box_ids[a, 1])
    if (grepl("/", d) == TRUE) {
      yy <- strsplit(d, split = "/")[[1]][3]
      mm <- strsplit(d, split = "/")[[1]][1]
      dd <- strsplit(d, split = "/")[[1]][2]
      
      if (nchar(mm) == 1) { mm <- paste(0, mm, sep = "") }
      if (nchar(dd) == 1) { dd <- paste(0, dd, sep = "") }
      
      rr <- paste(mm, dd, yy, sep = "-")
      
      master_box_ids$Date[a] <<- rr 
    }
  }
}

model_rewind <- function(xxxx) {
  
  box_to_team_rehab(xxxx)
  multiple_adj_pre(10)
  file_name <- paste("statsasof_", xxxx, ".csv", sep = "")
  write_csv(master_team_adj_frame_wt, file_name)
  
}
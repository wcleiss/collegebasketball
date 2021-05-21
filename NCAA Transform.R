#NCAA TRANSFORM

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

#-----------TEAM STATS-------------

compile_raw_per_pos <- function(xxxx) {
  
  a <- 1
  g <- nrow(team_totals_raw)
  
  for (a in a:g) {
    
    act_team <- team_totals_raw$TEAM_NAME[a]
    act_teamid <- team_totals_raw$TEAM_ID[a]
    act_mins <- team_totals_raw$MINS[a] / 5
    act_pos <- team_totals_raw$POS[a]
    act_pts <- team_totals_raw$PTS[a]
    act_fgm <- team_totals_raw$FGM[a]
    act_fga <- team_totals_raw$FGA[a]
    act_3pa <- team_totals_raw$PA3[a]
    act_3pm <- team_totals_raw$PM3[a]
    act_2pa <- team_totals_raw$PA2[a]
    act_2pm <- team_totals_raw$PM2[a]
    act_ftm <- team_totals_raw$FTM[a]
    act_fta <- team_totals_raw$FTA[a]
    act_orb <- team_totals_raw$ORB[a]
    act_drb <- team_totals_raw$DRB[a]
    act_trb <- team_totals_raw$TRB[a]
    act_oorb <- team_totals_raw$oORB[a]
    act_odrb <- team_totals_raw$oDRB[a]
    act_otrb <- team_totals_raw$oTRB[a]
    act_ast <- team_totals_raw$AST[a]
    act_stl <- team_totals_raw$STL[a]
    act_blk <- team_totals_raw$BLK[a]
    act_tov <- team_totals_raw$TO[a]
    act_pfl <- team_totals_raw$FOUL[a]
    
    act_opts <- team_totals_raw$oPTS[a]
    act_ofgm <- team_totals_raw$oFGM[a]
    act_ofga <- team_totals_raw$oFGA[a]
    act_o3pa <- team_totals_raw$oPA3[a]
    act_o3pm <- team_totals_raw$oPM3[a]
    act_o2pa <- team_totals_raw$oPA2[a]
    act_o2pm <- team_totals_raw$oPM2[a]
    act_oftm <- team_totals_raw$oFTM[a]
    act_ofta <- team_totals_raw$oFTA[a]
    act_oast <- team_totals_raw$oAST[a]
    act_ostl <- team_totals_raw$oSTL[a]
    act_oblk <- team_totals_raw$oBLK[a]
    act_otov <- team_totals_raw$oTO[a]
    act_opfl <- team_totals_raw$oFOUL[a]
    
    #Add POS
    
    pos_calc <- (0.5 * (act_fga + (0.475 * act_fta) - act_orb + act_tov) + 
                   (0.5 * (act_ofga + (0.475 * act_ofta) - act_oorb + act_otov)))
    
    ncolplus <- ncol(team_totals_raw) + 1
    
    team_totals_raw[a, 38] <<- pos_calc
    act_pos <- pos_calc
    
    tempo_do <- (act_pos / act_mins) * 40
    oe_do <- act_pts / act_pos
    efg_do <- (act_fgm + (0.5 * act_3pm)) / act_fga
    sr2p_do <- act_2pa / act_pos
    sr3p_do <- act_3pa / act_pos
    mr2p_do <- act_2pm / act_2pa
    mr3p_do <- act_3pm / act_3pa
    ftpct_do <- act_ftm / act_fta
    ftr_do <- act_fta / act_fga
    orb_do <- act_orb / (act_orb + act_odrb)
    drb_do <- act_drb / (act_drb + act_oorb)
    trb_do <- act_trb / (act_trb + act_otrb)
    ast_do <- act_ast / act_pos
    stl_do <- act_stl / act_pos
    blk_do <- act_blk / act_pos
    tov_do <- act_tov / act_pos
    foul_do <- act_pfl / (act_pos * 2)
    
    de_do <- act_opts / act_pos
    defg_do <- (act_ofgm + (0.5 * act_o3pm)) / act_ofga
    dsr2p_do <- act_o2pa / act_pos
    dsr3p_do <- act_o3pa / act_pos
    dmr2p_do <- act_o2pm / act_o2pa
    dmr3p_do <- act_o3pm / act_o3pa
    dftpct_do <- act_oftm / act_ofta
    dftr_do <- act_ofta / act_ofga
    dorb_do <- act_oorb / (act_oorb + act_drb)
    ddrb_do <- act_odrb / (act_odrb + act_orb)
    dtrb_do <- act_otrb / (act_otrb + act_trb)
    dast_do <- act_oast / act_pos
    dstl_do <- act_ostl / act_pos
    dblk_do <- act_oblk / act_pos
    dtov_do <- act_otov / act_pos
    dfoul_do <- act_opfl / (act_pos * 2)
    
    ft2foul_do <- act_fta / act_opfl
    dft2foul_do <- act_ofta / act_pfl
    
    ft_per_pos <<- act_fta / act_pos
    dft_per_pos <<- act_ofta / act_pos
    
    if (a == 1) { raw_perpos_frame <<- data.frame(act_teamid, act_team, tempo_do, oe_do, efg_do, sr2p_do, mr2p_do,
                                                  sr3p_do, mr3p_do, ftpct_do, ftr_do, orb_do, drb_do,
                                                  trb_do, ast_do, stl_do, blk_do, tov_do, foul_do,
                                                  de_do, defg_do, dsr2p_do, dsr3p_do, dmr2p_do, dmr3p_do,
                                                  dftpct_do, dftr_do, dorb_do, ddrb_do, dtrb_do, dast_do,
                                                  dstl_do, dblk_do, dtov_do, dfoul_do, ft2foul_do, dft2foul_do, 
                                                  ft_per_pos, dft_per_pos, stringsAsFactors = FALSE) }
    else { 
      raw_perpos_frame2 <<- data.frame(act_teamid, act_team, tempo_do, oe_do, efg_do, sr2p_do, mr2p_do,
                                      sr3p_do, mr3p_do, ftpct_do, ftr_do, orb_do, drb_do,
                                      trb_do, ast_do, stl_do, blk_do, tov_do, foul_do,
                                      de_do, defg_do, dsr2p_do, dsr3p_do, dmr2p_do, dmr3p_do,
                                      dftpct_do, dftr_do, dorb_do, ddrb_do, dtrb_do, dast_do,
                                      dstl_do, dblk_do, dtov_do, dfoul_do, ft2foul_do, dft2foul_do, ft_per_pos, dft_per_pos, stringsAsFactors = FALSE)
      
      raw_perpos_frame <<- rbind(raw_perpos_frame, raw_perpos_frame2)
    }
  }
  colnames(team_totals_raw)[38] <<- c("POS")
}

#NATIONAL AVERAGES

compile_nat_avg <- function(xxxx) {
  
  a <- 1
  
  nat_game_sums <<- team_game_by_game_raw %>%
    ungroup %>%
    summarize(Games = n(), Mins = sum(MINS), Pts = sum(PTS), FGM = sum(FGM), FGA = sum(FGA), PM2 = sum(PM2), PA2 = sum(PA2),
              PM3 = sum(PM3), PA3 = sum(PA3), FTM = sum(FTM), FTA = sum(FTA), ORB = sum(ORB),
              DRB = sum(DRB), TRB = sum(TRB), AST = sum(AST), STL = sum(STL), BLK = sum(BLK), TOV = sum(TO),
              PFL = sum(FOUL), oPts = sum(PTSd), oFGM = sum(FGMd), oFGA = sum(FGAd), oPM2 = sum(PM2d), oPA2 = sum(PA2d),
              oPM3 = sum(PM3d), oPA3 = sum(PA3d), oFTM = sum(FTMd), oFTA = sum(FTAd), oORB = sum(ORBd),
              oDRB = sum(DRBd), oTRB = sum(TRBd), oAST = sum(ASTd), oSTL = sum(STLd), oBLK = sum(BLKd), oTOV = sum(TOd),
              oPFL = sum(FOULd))
    
  
  act_fga <- nat_game_sums$FGA[a]
  act_fta <- nat_game_sums$FTA[a]
  act_orb <- nat_game_sums$ORB[a]
  act_tov <- nat_game_sums$TOV[a]
  
  act_ofga <- nat_game_sums$oFGA[a]
  act_ofta <- nat_game_sums$oFTA[a]
  act_oorb <- nat_game_sums$oORB[a]
  act_otov <- nat_game_sums$oTOV[a]
  
  pos_calc <- (0.5 * (act_fga + (0.475 * act_fta) - act_orb + act_tov) + 
                 (0.5 * (act_ofga + (0.475 * act_ofta) - act_oorb + act_otov)))
  
  nat_game_sums[1, 37] <<- pos_calc
  colnames(nat_game_sums)[37] <<- "POS"
  
  act_mins <- nat_game_sums$Mins[a] / 5
  nat_game_sums$Mins[a] <<- act_mins
  act_pos <- nat_game_sums$POS[a]
  act_pts <- nat_game_sums$Pts[a]
  act_fgm <- nat_game_sums$FGM[a]
  act_fga <- nat_game_sums$FGA[a]
  act_3pa <- nat_game_sums$PA3[a]
  act_3pm <- nat_game_sums$PM3[a]
  act_2pa <- nat_game_sums$PA2[a]
  act_2pm <- nat_game_sums$PM2[a]
  act_ftm <- nat_game_sums$FTM[a]
  act_fta <- nat_game_sums$FTA[a]
  act_orb <- nat_game_sums$ORB[a]
  act_drb <- nat_game_sums$DRB[a]
  act_trb <- nat_game_sums$TRB[a]
  act_oorb <- nat_game_sums$oORB[a]
  act_odrb <- nat_game_sums$oDRB[a]
  act_otrb <- nat_game_sums$oTRB[a]
  act_ast <- nat_game_sums$AST[a]
  act_stl <- nat_game_sums$STL[a]
  act_blk <- nat_game_sums$BLK[a]
  act_tov <- nat_game_sums$TOV[a]
  act_pfl <- nat_game_sums$PFL[a]
  act_opfl <- nat_game_sums$oPFL[a]
  
  tempo_do <- (act_pos / act_mins) * 40
  oe_do <- act_pts / act_pos
  efg_do <- (act_fgm + (0.5 * act_3pm)) / act_fga
  sr2p_do <- act_2pa / act_pos
  sr3p_do <- act_3pa / act_pos
  mr2p_do <- act_2pm / act_2pa
  mr3p_do <- act_3pm / act_3pa
  ftpct_do <- act_ftm / act_fta
  ftr_do <- act_fta / act_fga
  orb_do <- act_orb / (act_orb + act_odrb)
  drb_do <- act_drb / (act_drb + act_oorb)
  trb_do <- act_trb / (act_trb + act_otrb)
  ast_do <- act_ast / act_pos
  stl_do <- act_stl / act_pos
  blk_do <- act_blk / act_pos
  tov_do <- act_tov / act_pos
  foul_do <- act_pfl / (act_pos * 2)
  ft2foul_do <- act_fta / act_opfl
  ft_per_pos <- act_fta / act_pos
  
  nat_avgs <<- data.frame(tempo_do, oe_do, efg_do, sr2p_do, mr2p_do,
                          sr3p_do, mr3p_do, ftpct_do, ftr_do, orb_do, drb_do,
                          trb_do, ast_do, stl_do, blk_do, tov_do, foul_do,
                          ft2foul_do, ft_per_pos, stringsAsFactors = FALSE)
  
}


#ADV GAME LOG

compile_gamelog_adv <- function(xxxx) {
  
  a <- 1
  game_log_mx <<- as.matrix(team_game_by_game_raw[, c(9:26, 28:44)])
  g <- nrow(game_log_mx)
  game_log_info <<- as.data.frame(team_game_by_game_raw[1:g, 1:8])
  
  for (a in a:g) {
    
    act_mins <- game_log_mx[a, 1]
    act_pts <- game_log_mx[a, 2]
    act_fgm <- game_log_mx[a, 3]
    act_fga <- game_log_mx[a, 4]
    act_2pm <- game_log_mx[a, 5]
    act_2pa <- game_log_mx[a, 6]
    act_3pm <- game_log_mx[a, 7]
    act_3pa <- game_log_mx[a, 8]
    act_ftm <- game_log_mx[a, 9]
    act_fta <- game_log_mx[a, 10]
    act_orb <- game_log_mx[a, 11]
    act_drb <- game_log_mx[a, 12]
    act_trb <- game_log_mx[a, 13]
    act_ast <- game_log_mx[a, 14]
    act_stl <- game_log_mx[a, 15]
    act_blk <- game_log_mx[a, 16]
    act_tov <- game_log_mx[a, 17]
    act_pfl <- game_log_mx[a, 18]
    
    act_opts <- game_log_mx[a, 19]
    act_ofgm <- game_log_mx[a, 20]
    act_ofga <- game_log_mx[a, 21]
    act_o2pm <- game_log_mx[a, 22]
    act_o2pa <- game_log_mx[a, 23]
    act_o3pm <- game_log_mx[a, 24]
    act_o3pa <- game_log_mx[a, 25]
    act_oftm <- game_log_mx[a, 26]
    act_ofta <- game_log_mx[a, 27]
    act_oorb <- game_log_mx[a, 28]
    act_odrb <- game_log_mx[a, 29]
    act_otrb <- game_log_mx[a, 30]
    act_oast <- game_log_mx[a, 31]
    act_ostl <- game_log_mx[a, 32]
    act_oblk <- game_log_mx[a, 33]
    act_otov <- game_log_mx[a, 34]
    act_opfl <- game_log_mx[a, 35]
    
    pos_calc <- (0.5 * (act_fga + (0.475 * act_fta) - act_orb + act_tov) + 
                   (0.5 * (act_ofga + (0.475 * act_ofta) - act_oorb + act_otov)))
    
    act_pos <- pos_calc
    
    tempo_do <- (act_pos / (act_mins / 5)) * 40
    oe_do <- act_pts / act_pos
    efg_do <- (act_fgm + (0.5 * act_3pm)) / act_fga
    sr2p_do <- act_2pa / act_pos
    sr3p_do <- act_3pa / act_pos
    mr2p_do <- act_2pm / act_2pa
    mr3p_do <- act_3pm / act_3pa
    ftpct_do <- act_ftm / act_fta
    ftr_do <- act_fta / act_fga
    orb_do <- act_orb / (act_orb + act_odrb)
    drb_do <- act_drb / (act_drb + act_oorb)
    trb_do <- act_trb / (act_trb + act_otrb)
    ast_do <- act_ast / act_pos
    stl_do <- act_stl / act_pos
    blk_do <- act_blk / act_pos
    tov_do <- act_tov / act_pos
    foul_do <- act_pfl / (act_pos * 2)
    
    de_do <- act_opts / act_pos
    defg_do <- (act_ofgm + (0.5 * act_o3pm)) / act_ofga
    dsr2p_do <- act_o2pa / act_pos
    dsr3p_do <- act_o3pa / act_pos
    dmr2p_do <- act_o2pm / act_o2pa
    dmr3p_do <- act_o3pm / act_o3pa
    dftpct_do <- act_oftm / act_ofta
    dftr_do <- act_ofta / act_ofga
    dorb_do <- act_oorb / (act_oorb + act_drb)
    ddrb_do <- act_odrb / (act_odrb + act_orb)
    dtrb_do <- act_otrb / (act_otrb + act_trb)
    dast_do <- act_oast / act_pos
    dstl_do <- act_ostl / act_pos
    dblk_do <- act_oblk / act_pos
    dtov_do <- act_otov / act_pos
    dfoul_do <- act_opfl / (act_pos * 2)
    
    ft2foul_do <- act_fta / act_opfl
    dft2foul_do <- act_ofta / act_pfl
    
    ft_per_pos_do <- act_fta / act_pos
    dft_per_pos_do <- act_ofta / act_pos
    
    if (a == 1) { adv_game_log <<- matrix(c(act_pos, oe_do, efg_do, sr2p_do, mr2p_do,
                                            sr3p_do, mr3p_do, ftpct_do, ftr_do, orb_do, drb_do,
                                            trb_do, ast_do, stl_do, blk_do, tov_do, foul_do,
                                            de_do, defg_do, dsr2p_do, dmr2p_do, dsr3p_do, dmr3p_do,
                                            dftpct_do, dftr_do, dorb_do, ddrb_do, dtrb_do, dast_do,
                                            dstl_do, dblk_do, dtov_do, dfoul_do, ft2foul_do, dft2foul_do,
                                            ft_per_pos_do, dft_per_pos_do, tempo_do), nrow = 1) 
    }
    
    else {
      
      adv_game_log2 <<- matrix(c(act_pos, oe_do, efg_do, sr2p_do, mr2p_do,
                                 sr3p_do, mr3p_do, ftpct_do, ftr_do, orb_do, drb_do,
                                 trb_do, ast_do, stl_do, blk_do, tov_do, foul_do,
                                 de_do, defg_do, dsr2p_do, dmr2p_do, dsr3p_do, dmr3p_do,
                                 dftpct_do, dftr_do, dorb_do, ddrb_do, dtrb_do, dast_do,
                                 dstl_do, dblk_do, dtov_do, dfoul_do, ft2foul_do, dft2foul_do,
                                 ft_per_pos_do, dft_per_pos_do, tempo_do), nrow = 1) 
      
      adv_game_log <<- rbind(adv_game_log, adv_game_log2)
    }
  }
  
  adv_game_log_bind <<- data.frame(game_log_info, adv_game_log) 
  
  colnames(adv_game_log_bind) <<- c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "Date", "GAME_ID", "Loc", "NO_ID",
                                    "Tempo", "OE", "eFG", "SR2", "SM2", 
                                    "SR3", "SM3", "FT", "FTR", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TO",
                                    "FOUL", "DE", "deFG", "dSR2", "dSM2", 
                                    "dSR3", "dSM3", "dFT", "dFTR", "dORB", "dDRB", "dTRB", "dAST", "dSTL", "dBLK", "dTO",
                                    "dFOUL", "FTperFOUL", "dFTperFOUL", "SRFT", "dSRFT", "Tempo40")
  
  adv_game_log_join <<- adv_game_log_bind
  
  ft_nan <- which(is.nan(adv_game_log_join$FT) == TRUE)
  adv_game_log_join[ft_nan, 16] <<- 0
  
  dft_nan <- which(is.nan(adv_game_log_join$dFT) == TRUE)
  adv_game_log_join[dft_nan, 32] <<- 0
  
}

win_pct_calcu <- function(xxxx) {
  
  master_home_adv <<- team_game_by_game_raw[, c(1:4, 6, 7, 10, 28)]
  
  tmp_adj <- master_team_adj_frame_wt %>%
    select(TEAM_ID, TEAM_NAME, OE, DE) %>%
    mutate(ED = OE - DE) %>%
    select(TEAM_ID, ED)
  
  master_home_adv <<- master_home_adv %>%
    inner_join(tmp_adj, by = c("TEAM_ID")) %>%
    inner_join(tmp_adj, by = c("OPP_ID" = "TEAM_ID")) %>%
    mutate(EDIF = abs(ED.x - ED.y)) %>%
    filter(EDIF <= .08) %>%
    filter(LOC == "A") %>%
    mutate(PTSn = PTS + 1.18, PTSdn = PTSd - 1.18) %>%
    mutate(WIN = ifelse(PTS > PTSd, 1, 0)) %>%
    mutate(WINN = ifelse(PTSn > PTSdn, 1, 0))

  View(master_home_adv)
  
}

home_adv_calcu3 <- function(xxxx) {
  
    master_home_adv <<- adv_game_log_join[, c(1:4, 6, 7)]
    master_home_adv <<- master_home_adv %>%
      filter(Loc != "N") %>%
      left_join(master_team_adj_frame_wt, by = c("TEAM_ID", "TEAM_NAME")) %>%
      select(TEAM_ID, OPP_ID, TEAM_NAME, OPP_NAME, GAME_ID, OE, DE) %>%
      left_join(master_team_adj_frame_wt, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME")) %>%
      select(TEAM_ID, OPP_ID, TEAM_NAME, OPP_NAME, GAME_ID, OE.x, DE.x, OE.y, DE.y) %>%
      mutate(ED.x = (OE.x - DE.x), ED.y = (OE.y - DE.y)) %>%
      mutate(ED.DIF = abs(ED.x - ED.y)) %>%
      arrange(ED.DIF) %>%
      filter(ED.DIF <= .06)
    
    tgbbr <- team_game_by_game_raw %>%
      select("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID", "PTS", "PTSd", "LOC")
    
    master_home_adv <<- master_home_adv[c(1:800), ]
    
    master_home_adv_j <<- master_home_adv[, c(1:5)]
    
    tgbbr_j <<- master_home_adv_j %>%
      left_join(tgbbr, by = c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID"))
    
    master_home_adv <<- master_home_adv_j %>%
      left_join(adv_game_log_join, by = c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID")) %>%
      filter(Loc == "H") %>%
      summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
                SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
                STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
    
    master_away_adv <<- master_home_adv_j %>%
      left_join(adv_game_log_join, by = c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID")) %>%
      filter(Loc == "A") %>%
      summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
                SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
                STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
    
    master_comb_adv <<- master_home_adv_j %>%
      left_join(adv_game_log_join, by = c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID")) %>%
      summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
                SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
                STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
    
    
    master_neut_adv <<- adv_game_log_join %>%
      filter(Loc == "N") %>%
      summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
                SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
                STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
    
    master_nonneut_adv <<- adv_game_log_join %>%
      filter(Loc != "N") %>%
      summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
                SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
                STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
    
    master_all_adv <<- adv_game_log_join %>%
      summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
                SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
                STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
    
    
    master_home_adv_dif <<- master_home_adv / master_comb_adv
    master_away_adv_dif <<- master_away_adv / master_comb_adv
    master_neut_adv_dif <<- master_neut_adv / master_nonneut_adv
    
    master_home_adv_dif[, 1] <- "H"
    master_away_adv_dif[, 1] <- "A"
    master_neut_adv_dif[, 1] <- "N"
    
    master_all_adv[1, 1] <- "T"
    
    advantage_frame <<- rbind(master_home_adv_dif, master_away_adv_dif, master_neut_adv_dif, master_all_adv)
    advantage_frame[3, 3:22] <<- 1
    
}
#HOME ADVANTAGE CALC

home_adv_calcu2 <- function(xxx) {
  
  master_home_adv <<- adv_game_log_join[, c(1:4, 6, 7)]
  master_home_adv <<- master_home_adv %>%
    filter(Loc != "N") %>%
    left_join(master_team_adj_frame_wt, by = c("TEAM_ID", "TEAM_NAME")) %>%
    select(TEAM_ID, OPP_ID, TEAM_NAME, OPP_NAME, GAME_ID, OE, DE) %>%
    left_join(master_team_adj_frame_wt, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME")) %>%
    select(TEAM_ID, OPP_ID, TEAM_NAME, OPP_NAME, GAME_ID, OE.x, DE.x, OE.y, DE.y) %>%
    mutate(ED.x = (OE.x - DE.x), ED.y = (OE.y - DE.y)) %>%
    mutate(ED.DIF = abs(ED.x - ED.y)) %>%
    arrange(ED.DIF) %>%
    filter(ED.DIF <= .06)
  
  tgbbr <- team_game_by_game_raw %>%
    select("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID", "PTS", "PTSd", "LOC")
  
  master_home_adv <<- master_home_adv[c(1:800), ]
  
  master_home_adv_j <<- master_home_adv[, c(1:5)]
  
  tgbbr_j <<- master_home_adv_j %>%
    left_join(tgbbr, by = c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID"))
  
  master_home_adv <<- master_home_adv_j %>%
    left_join(adv_game_log_join, by = c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID")) %>%
    filter(Loc == "H") %>%
    summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
              SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
              STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
  
  master_away_adv <<- master_home_adv_j %>%
    left_join(adv_game_log_join, by = c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_ID")) %>%
    filter(Loc == "A") %>%
    summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
              SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
              STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
  
  
  master_neut_adv <<- adv_game_log_join %>%
    filter(Loc == "N") %>%
    summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
              SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
              STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
  
  master_all_adv <<- adv_game_log_join %>%
    summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
              SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
              STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT), DE = mean(DE))
  
  master_home_adv_dif <<- master_home_adv / master_all_adv
  master_away_adv_dif <<- master_away_adv / master_all_adv
  master_neut_adv_dif <<- master_neut_adv / master_all_adv
  
  master_home_adv_dif[, 1] <- "H"
  master_away_adv_dif[, 1] <- "A"
  master_neut_adv_dif[, 1] <- "N"
  
  master_all_adv[1, 1] <- "T"
  
  advantage_frame <<- rbind(master_home_adv_dif, master_away_adv_dif, master_neut_adv_dif, master_all_adv)
  
}

home_adv_calcu <- function(xxxx) {
  
  master_home_adv <<- adv_game_log_join %>%
    filter(Loc == "H") %>%
    summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
              SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
              STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT))
  
  master_neut_adv <<- adv_game_log_join %>%
    filter(Loc == "N") %>%
    summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
              SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
              STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT))
  
  master_all_adv <<- adv_game_log_join %>%
    summarize(Split = n(), Games = n(), Tempo = mean(Tempo), OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3),
              SM3 = mean(SM3), FT = mean(FT), FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST),
              STL = mean(STL), BLK = mean(BLK), TO = mean(TO), FOUL = mean(FOUL), FTperFOUL = mean(FTperFOUL), SRFT = mean(SRFT))
  
  master_home_adv_dif <<- master_home_adv / master_all_adv
  master_away_adv_dif <<- master_all_adv / master_home_adv
  master_neut_adv_dif <<- master_neut_adv / master_all_adv
  
  master_home_adv_dif[, 1] <- "H"
  master_away_adv_dif[, 1] <- "A"
  master_neut_adv_dif[, 1] <- "N"
  
  advantage_frame <<- rbind(master_home_adv_dif, master_away_adv_dif, master_neut_adv_dif, master_all_adv)
  
  #REMOVE ONCE ENOUGH GAMES HAVE BEEN PLAYED
  #advantage_frame_actual <<- advantage_frame
  #advantage_frame_read <<- read_csv("advantages_2020.csv")
  #advantage_frame[1:3, ] <<- advantage_frame_read
  
}


#MERGE ADV GAME LOG JOIN WITH OPPONENTS RAW ADV STATS

#Feed 1 into function if merging in preseason merge, otherwise 0
other_log_splitter <- function(xxxx) {
  
  opp_log_list <<- adv_game_log_join[, c(2, 4)]
  
  if (xxxx != 1) { 
    opp_log_stats <<- opp_log_list %>%
      left_join(raw_perpos_frame, by = c("OPP_ID" = "act_teamid", "OPP_NAME" = "act_team"))
      numbers_frame_opp <<- as.matrix(opp_log_stats[, 3:39])
    dsm2_slot <<- 22
    dsr3_slot <<- 21
  }
  else {
    opp_log_stats <<- opp_log_list %>%
      left_join(pre_merge_output, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME"))
    numbers_frame_opp <<- as.matrix(opp_log_stats[, c(37, 3:8, 38, 9:23, 38, 24:36)])
    dsm2_slot <<- 21
    dsr3_slot <<- 22
  }
  
  numbers_frame_team <<- as.matrix(adv_game_log_join[, 9:46])
  
  nat_tempo <<- as.numeric(nat_avgs[1, 1])
  nat_oe <<- as.numeric(nat_avgs[1, 2]) 
  nat_efg <<- as.numeric(nat_avgs[1, 3])
  nat_sr2 <<- as.numeric(nat_avgs[1, 4])
  nat_sm2 <<- as.numeric(nat_avgs[1, 5])
  nat_sr3 <<- as.numeric(nat_avgs[1, 6])
  nat_sm3 <<- as.numeric(nat_avgs[1, 7])
  nat_ftpct <<- as.numeric(nat_avgs[1, 8]) 
  nat_ftr <<- as.numeric(nat_avgs[1, 9])
  nat_orb <<- as.numeric(nat_avgs[1, 10])
  nat_drb <<- as.numeric(nat_avgs[1, 11])
  nat_trb <<- as.numeric(nat_avgs[1, 12])
  nat_ast <<- as.numeric(nat_avgs[1, 13])
  nat_stl <<- as.numeric(nat_avgs[1, 14])
  nat_blk <<- as.numeric(nat_avgs[1, 15])
  nat_tov <<- as.numeric(nat_avgs[1, 16])
  nat_foul <<- as.numeric(nat_avgs[1, 17])
  nat_ft2foul <<- as.numeric(nat_avgs[1, 18])
  nat_srft <<- as.numeric(nat_avgs[1, 19])
  
  a <- 1 
  g <- nrow(numbers_frame_opp)
  
  for (a in a:g) {
    
    act_tempo <- as.numeric(numbers_frame_opp[a, 1])
    act_oe <- as.numeric(numbers_frame_opp[a, 2]) 
    act_efg <- as.numeric(numbers_frame_opp[a, 3]) 
    act_sr2 <- as.numeric(numbers_frame_opp[a, 4]) 
    act_sm2 <- as.numeric(numbers_frame_opp[a, 5]) 
    act_sr3 <- as.numeric(numbers_frame_opp[a, 6]) 
    act_sm3 <- as.numeric(numbers_frame_opp[a, 7]) 
    act_ftpct <- as.numeric(numbers_frame_opp[a, 8]) 
    act_ftr <- as.numeric(numbers_frame_opp[a, 9]) 
    act_orb <- as.numeric(numbers_frame_opp[a, 10])
    act_drb <- as.numeric(numbers_frame_opp[a, 11]) 
    act_trb <- as.numeric(numbers_frame_opp[a, 12]) 
    act_ast <- as.numeric(numbers_frame_opp[a, 13]) 
    act_stl <- as.numeric(numbers_frame_opp[a, 14]) 
    act_blk <- as.numeric(numbers_frame_opp[a, 15]) 
    act_tov <- as.numeric(numbers_frame_opp[a, 16]) 
    act_foul <- as.numeric(numbers_frame_opp[a, 17]) 
    
    act_de <- as.numeric(numbers_frame_opp[a, 18]) 
    act_defg <- as.numeric(numbers_frame_opp[a, 19]) 
    act_dsr2 <- as.numeric(numbers_frame_opp[a, 20]) 
    act_dsm2 <- as.numeric(numbers_frame_opp[a, dsm2_slot])
    act_dsr3 <- as.numeric(numbers_frame_opp[a, dsr3_slot]) 
    act_dsm3 <- as.numeric(numbers_frame_opp[a, 23]) 
    act_dftpct <- as.numeric(numbers_frame_opp[a, 24]) 
    act_dftr <- as.numeric(numbers_frame_opp[a, 25]) 
    act_dorb <- as.numeric(numbers_frame_opp[a, 26]) 
    act_ddrb <- as.numeric(numbers_frame_opp[a, 27]) 
    act_dtrb <- as.numeric(numbers_frame_opp[a, 28]) 
    act_dast <- as.numeric(numbers_frame_opp[a, 29]) 
    act_dstl <- as.numeric(numbers_frame_opp[a, 30]) 
    act_dblk <- as.numeric(numbers_frame_opp[a, 31]) 
    act_dtov <- as.numeric(numbers_frame_opp[a, 32]) 
    act_dfoul <- as.numeric(numbers_frame_opp[a, 33]) 
    
    act_ft2foul <- as.numeric(numbers_frame_opp[a, 34]) 
    act_dft2foul <- as.numeric(numbers_frame_opp[a, 35])
    
    act_srft <- as.numeric(numbers_frame_opp[a, 36]) 
    act_dsrft <- as.numeric(numbers_frame_opp[a, 37])
    
    dif_tempo <- act_tempo / nat_tempo
    
    dif_oe <- act_oe / nat_oe
    dif_efg <- act_efg / nat_efg
    dif_sr2 <- 1
    dif_sm2 <- act_sm2 / nat_sm2
    dif_sr3 <- 1
    dif_sm3 <- act_sm3 / nat_sm3
    dif_ftpct <- act_ftpct / nat_ftpct
    dif_ftr <- act_ftr / nat_ftr
    dif_orb <- act_orb / nat_orb
    dif_drb <- act_drb / nat_drb
    dif_trb <- act_trb / nat_trb
    dif_ast <- act_ast / nat_ast
    dif_stl <- act_stl / nat_stl
    dif_blk <- act_blk / nat_blk
    dif_tov <- act_tov / nat_tov
    dif_foul <- act_foul / nat_foul
    
    dif_de <- act_de / nat_oe
    dif_defg <- act_defg / nat_efg
    dif_dsr2 <- 1
    dif_dsm2 <- act_dsm2 / nat_sm2
    dif_dsr3 <- 1
    dif_dsm3 <- act_dsm3 / nat_sm3
    dif_dftpct <- act_dftpct / nat_ftpct
    dif_dftr <- act_dftr / nat_ftr
    dif_dorb <- act_dorb / nat_orb
    dif_ddrb <- act_ddrb / nat_drb
    dif_dtrb <- act_dtrb / nat_trb
    dif_dast <- act_dast / nat_ast
    dif_dstl <- act_dstl / nat_stl
    dif_dblk <- act_dblk / nat_blk
    dif_dtov <- act_dtov / nat_tov
    dif_dfoul <- act_dfoul / nat_foul
    
    dif_ft2foul <- act_ft2foul / nat_ft2foul
    dif_dft2foul <- act_dft2foul / nat_ft2foul
    
    dif_srft <- act_srft / nat_srft
    dif_dsrft <- act_dsrft / nat_srft
    
    
    if (a == 1) { dif_frame_opp <<- matrix(c(dif_tempo, dif_oe, dif_efg, dif_sr2, dif_sm2, dif_sr3, dif_sm3,
                                             dif_ftpct, dif_ftr, dif_orb, dif_drb, dif_trb, dif_ast, dif_stl,
                                             dif_blk, dif_tov, dif_foul, dif_de, dif_defg, dif_dsr2, dif_dsm2,
                                             dif_dsr3, dif_dsm3, dif_dftpct, dif_dftr, dif_dorb, dif_ddrb, dif_dtrb,
                                             dif_dast, dif_dstl, dif_dblk, dif_dtov, dif_dfoul, dif_ft2foul, dif_dft2foul,
                                             dif_srft, dif_dsrft), nrow = 1) }
    else { 
      dif_frame_opp2 <<- matrix(c(dif_tempo, dif_oe, dif_efg, dif_sr2, dif_sm2, dif_sr3, dif_sm3,
                                  dif_ftpct, dif_ftr, dif_orb, dif_drb, dif_trb, dif_ast, dif_stl,
                                  dif_blk, dif_tov, dif_foul, dif_de, dif_defg, dif_dsr2, dif_dsm2,
                                  dif_dsr3, dif_dsm3, dif_dftpct, dif_dftr, dif_dorb, dif_ddrb, dif_dtrb,
                                  dif_dast, dif_dstl, dif_dblk, dif_dtov, dif_dfoul, dif_ft2foul, dif_dft2foul,
                                  dif_srft, dif_dsrft), nrow = 1)
      
      dif_frame_opp <<- rbind(dif_frame_opp, dif_frame_opp2)
    }
  }
  
  colnames(dif_frame_opp) <<- c("Tempo", "OE", "eFG", "SR2", "SM2", 
                                "SR3", "SM3", "FT", "FTR", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TO",
                                "FOUL", "DE", "deFG", "dSR2", "dSM2", 
                                "dSR3", "dSM3", "dFT", "dFTR", "dORB", "dDRB", "dTRB", "dAST", "dSTL", "dBLK", "dTO",
                                "dFOUL", "FTperFOUL", "dFTperFOUL", "SRFT", "dSRFT")
  
  dif_frame_opp_join <<- data.frame(adv_game_log_join[, 1:8], dif_frame_opp, stringsAsFactors = FALSE)
}



game_log_adjust_1 <- function(xxxx) {
  
  team_off_stats <<- numbers_frame_team[, c(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17)]
  team_def_stats <<- numbers_frame_team[, c(18, 19, 20, 21, 22, 23, 25, 26, 27, 28, 29, 30, 31, 32, 33)]
  team_foul_stats <<- numbers_frame_team[, 34:37]
  
  opp_off_stats <<- dif_frame_opp[,  c(2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17)]
  opp_def_stats <<- dif_frame_opp[, c(18, 19, 20, 21, 22, 23, 25, 26, 27, 28, 29, 30, 31, 32, 33)]
  opp_foul_stats <<- cbind(dif_frame_opp[, 35], dif_frame_opp[, 34], dif_frame_opp[, 37], dif_frame_opp[, 36])
  
  team_off_subtract <<- team_off_stats / opp_def_stats
  team_def_subtract <<- team_def_stats / opp_off_stats
  team_foul_subtract <<- team_foul_stats / opp_foul_stats
  
  team_off_htract <<- team_off_subtract
  team_def_htract <<- team_def_subtract
  team_foul_htract <<- team_foul_subtract
  
  #Currently Using Macro Home Advantages
  
  three_adv_lines <<- advantage_frame[1:3, ]
  
  home_oe <<- as.numeric(three_adv_lines[1, 4])
  home_efg <<- as.numeric(three_adv_lines[1, 5])
  home_sr2 <<- as.numeric(three_adv_lines[1, 6])
  home_sm2 <<- as.numeric(three_adv_lines[1, 7])
  home_sr3 <<- as.numeric(three_adv_lines[1, 8])
  home_sm3 <<- as.numeric(three_adv_lines[1, 9])
  home_ft <<- as.numeric(three_adv_lines[1, 10])
  home_ftr <<- as.numeric(three_adv_lines[1, 11])
  home_orb <<- as.numeric(three_adv_lines[1, 12])
  home_drb <<- as.numeric(three_adv_lines[1, 13])
  home_trb <<- as.numeric(three_adv_lines[1, 14])
  home_ast <<- as.numeric(three_adv_lines[1, 15])
  home_stl <<- as.numeric(three_adv_lines[1, 16])
  home_blk <<- as.numeric(three_adv_lines[1, 17])
  home_to <<- as.numeric(three_adv_lines[1, 18])
  home_foul <<- as.numeric(three_adv_lines[1, 19])
  home_ft2foul <<- as.numeric(three_adv_lines[1, 20])
  home_srft <<- as.numeric(three_adv_lines[1, 21])
  
  away_oe <<- as.numeric(three_adv_lines[2, 4])
  away_efg <<- as.numeric(three_adv_lines[2, 5])
  away_sr2 <<- as.numeric(three_adv_lines[2, 6])
  away_sm2 <<- as.numeric(three_adv_lines[2, 7])
  away_sr3 <<- as.numeric(three_adv_lines[2, 8])
  away_sm3 <<- as.numeric(three_adv_lines[2, 9])
  away_ft <<- as.numeric(three_adv_lines[2, 10])
  away_ftr <<- as.numeric(three_adv_lines[2, 11])
  away_orb <<- as.numeric(three_adv_lines[2, 12])
  away_drb <<- as.numeric(three_adv_lines[2, 13])
  away_trb <<- as.numeric(three_adv_lines[2, 14])
  away_ast <<- as.numeric(three_adv_lines[2, 15])
  away_stl <<- as.numeric(three_adv_lines[2, 16])
  away_blk <<- as.numeric(three_adv_lines[2, 17])
  away_to <<- as.numeric(three_adv_lines[2, 18])
  away_foul <<- as.numeric(three_adv_lines[2, 19])
  away_ft2foul <<- as.numeric(three_adv_lines[2, 20])
  away_srft <<- as.numeric(three_adv_lines[2, 21])
  
  neut_oe <<- as.numeric(three_adv_lines[3, 4])
  neut_efg <<- as.numeric(three_adv_lines[3, 5])
  neut_sr2 <<- as.numeric(three_adv_lines[3, 6])
  neut_sm2 <<- as.numeric(three_adv_lines[3, 7])
  neut_sr3 <<- as.numeric(three_adv_lines[3, 8])
  neut_sm3 <<- as.numeric(three_adv_lines[3, 9])
  neut_ft <<- as.numeric(three_adv_lines[3, 10])
  neut_ftr <<- as.numeric(three_adv_lines[3, 11])
  neut_orb <<- as.numeric(three_adv_lines[3, 12])
  neut_drb <<- as.numeric(three_adv_lines[3, 13])
  neut_trb <<- as.numeric(three_adv_lines[3, 14])
  neut_ast <<- as.numeric(three_adv_lines[3, 15])
  neut_stl <<- as.numeric(three_adv_lines[3, 16])
  neut_blk <<- as.numeric(three_adv_lines[3, 17])
  neut_to <<- as.numeric(three_adv_lines[3, 18])
  neut_foul <<- as.numeric(three_adv_lines[3, 19])
  neut_ft2foul <<- as.numeric(three_adv_lines[3, 20])
  neut_srft <<- as.numeric(three_adv_lines[3, 21])
  
  loc_mx <<- as.matrix(adv_game_log_join[, 7])
  
  a <- 1
  g <- nrow(loc_mx)
  
  for (a in a:g) {
    
    act_loc <- loc_mx[a]
    
    act_oe <- team_off_subtract[a, 1]
    act_efg <- team_off_subtract[a, 2]
    act_sr2 <- team_off_subtract[a, 3]
    act_sm2 <- team_off_subtract[a, 4]
    act_sr3 <- team_off_subtract[a, 5]
    act_sm3 <- team_off_subtract[a, 6]
    act_ftr <- team_off_subtract[a, 7]
    act_orb <- team_off_subtract[a, 8]
    act_drb <- team_off_subtract[a, 9]
    act_trb <- team_off_subtract[a, 10]
    act_ast <- team_off_subtract[a, 11]
    act_stl <- team_off_subtract[a, 12]
    act_blk <- team_off_subtract[a, 13]
    act_to <- team_off_subtract[a, 14]
    act_foul <- team_off_subtract[a, 15]
    
    act_de <- team_def_subtract[a, 1]
    act_defg <- team_def_subtract[a, 2]
    act_dsr2 <- team_def_subtract[a, 3]
    act_dsm2 <- team_def_subtract[a, 4]
    act_dsr3 <- team_def_subtract[a, 5]
    act_dsm3 <- team_def_subtract[a, 6]
    act_dftr <- team_def_subtract[a, 7]
    act_dorb <- team_def_subtract[a, 8]
    act_ddrb <- team_def_subtract[a, 9]
    act_dtrb <- team_def_subtract[a, 10]
    act_dast <- team_def_subtract[a, 11]
    act_dstl <- team_def_subtract[a, 12]
    act_dblk <- team_def_subtract[a, 13]
    act_dto <- team_def_subtract[a, 14]
    act_dfoul <- team_def_subtract[a, 15]
    
    act_ft2foul <- team_foul_subtract[a, 1]
    act_dft2foul <- team_foul_subtract[a, 2]
    act_srft <- team_foul_subtract[a, 3]
    act_dsrft <- team_foul_subtract[a, 4]
    
    if (act_loc == "H") {
      
      team_off_htract[a, 1] <<- act_oe / home_oe
      team_off_htract[a, 2] <<- act_efg / home_efg 
      team_off_htract[a, 3] <<- act_sr2 / 1
      team_off_htract[a, 4] <<- act_sm2 / home_sm2 
      team_off_htract[a, 5] <<- act_sr3 / 1
      team_off_htract[a, 6] <<- act_sm3 / home_sm3
      team_off_htract[a, 7] <<- act_ftr / home_ftr
      team_off_htract[a, 8] <<- act_orb / home_orb 
      team_off_htract[a, 9] <<- act_drb / home_drb 
      team_off_htract[a, 10] <<- act_trb / home_trb
      team_off_htract[a, 11] <<- act_ast / home_ast 
      team_off_htract[a, 12] <<- act_stl / home_stl 
      team_off_htract[a, 13] <<- act_blk / home_blk 
      team_off_htract[a, 14] <<- act_to / home_to 
      team_off_htract[a, 15] <<- act_foul / home_foul 
      
      team_def_htract[a, 1] <<- act_de * home_oe
      team_def_htract[a, 2] <<- act_defg * home_efg 
      team_def_htract[a, 3] <<- act_dsr2 * 1
      team_def_htract[a, 4] <<- act_dsm2 * home_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * 1
      team_def_htract[a, 6] <<- act_dsm3 * home_sm3
      team_def_htract[a, 7] <<- act_dftr * home_ftr
      team_def_htract[a, 8] <<- act_dorb * home_orb 
      team_def_htract[a, 9] <<- act_ddrb * home_drb 
      team_def_htract[a, 10] <<- act_dtrb * home_trb
      team_def_htract[a, 11] <<- act_dast * home_ast 
      team_def_htract[a, 12] <<- act_dstl * home_stl 
      team_def_htract[a, 13] <<- act_dblk * home_blk 
      team_def_htract[a, 14] <<- act_dto * home_to 
      team_def_htract[a, 15] <<- act_dfoul * home_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / home_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * home_ft2foul
      team_foul_htract[a, 3] <<- act_srft / home_srft
      team_foul_htract[a, 4] <<- act_dsrft * home_srft
      
    }
    
    if (act_loc == "A") {
      
      team_off_htract[a, 1] <<- act_oe / away_oe
      team_off_htract[a, 2] <<- act_efg / away_efg 
      team_off_htract[a, 3] <<- act_sr2 / 1
      team_off_htract[a, 4] <<- act_sm2 / away_sm2 
      team_off_htract[a, 5] <<- act_sr3 / 1
      team_off_htract[a, 6] <<- act_sm3 / away_sm3
      team_off_htract[a, 7] <<- act_ftr / away_ftr 
      team_off_htract[a, 8] <<- act_orb / away_orb 
      team_off_htract[a, 9] <<- act_drb / away_drb 
      team_off_htract[a, 10] <<- act_trb / away_trb
      team_off_htract[a, 11] <<- act_ast / away_ast 
      team_off_htract[a, 12] <<- act_stl / away_stl 
      team_off_htract[a, 13] <<- act_blk / away_blk 
      team_off_htract[a, 14] <<- act_to / away_to 
      team_off_htract[a, 15] <<- act_foul / away_foul 
      
      team_def_htract[a, 1] <<- act_de * away_oe
      team_def_htract[a, 2] <<- act_defg * away_efg 
      team_def_htract[a, 3] <<- act_dsr2 * 1
      team_def_htract[a, 4] <<- act_dsm2 * away_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * 1
      team_def_htract[a, 6] <<- act_dsm3 * away_sm3
      team_def_htract[a, 7] <<- act_dftr * away_ftr 
      team_def_htract[a, 8] <<- act_dorb * away_orb 
      team_def_htract[a, 9] <<- act_ddrb * away_drb 
      team_def_htract[a, 10] <<- act_dtrb * away_trb
      team_def_htract[a, 11] <<- act_dast * away_ast 
      team_def_htract[a, 12] <<- act_dstl * away_stl 
      team_def_htract[a, 13] <<- act_dblk * away_blk 
      team_def_htract[a, 14] <<- act_dto * away_to 
      team_def_htract[a, 15] <<- act_dfoul * away_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / away_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * away_ft2foul
      team_foul_htract[a, 3] <<- act_srft / away_srft
      team_foul_htract[a, 4] <<- act_dsrft * away_srft
      
    }
    
    if (act_loc == "N") {
      
      team_off_htract[a, 1] <<- act_oe / neut_oe
      team_off_htract[a, 2] <<- act_efg / neut_efg 
      team_off_htract[a, 3] <<- act_sr2 / 1
      team_off_htract[a, 4] <<- act_sm2 / neut_sm2 
      team_off_htract[a, 5] <<- act_sr3 / 1
      team_off_htract[a, 6] <<- act_sm3 / neut_sm3
      team_off_htract[a, 7] <<- act_ftr / neut_ftr
      team_off_htract[a, 8] <<- act_orb / neut_orb 
      team_off_htract[a, 9] <<- act_drb / neut_drb 
      team_off_htract[a, 10] <<- act_trb / neut_trb
      team_off_htract[a, 11] <<- act_ast / neut_ast 
      team_off_htract[a, 12] <<- act_stl / neut_stl 
      team_off_htract[a, 13] <<- act_blk / neut_blk 
      team_off_htract[a, 14] <<- act_to / neut_to 
      team_off_htract[a, 15] <<- act_foul / neut_foul 
      
      team_def_htract[a, 1] <<- act_de * neut_oe
      team_def_htract[a, 2] <<- act_defg * neut_efg 
      team_def_htract[a, 3] <<- act_dsr2 * 1
      team_def_htract[a, 4] <<- act_dsm2 * neut_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * 1
      team_def_htract[a, 6] <<- act_dsm3 * neut_sm3
      team_def_htract[a, 7] <<- act_dftr * neut_ftr 
      team_def_htract[a, 8] <<- act_dorb * neut_orb 
      team_def_htract[a, 9] <<- act_ddrb * neut_drb 
      team_def_htract[a, 10] <<- act_dtrb * neut_trb
      team_def_htract[a, 11] <<- act_dast * neut_ast 
      team_def_htract[a, 12] <<- act_dstl * neut_stl 
      team_def_htract[a, 13] <<- act_dblk * neut_blk 
      team_def_htract[a, 14] <<- act_dto * neut_to 
      team_def_htract[a, 15] <<- act_dfoul * neut_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / neut_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * neut_ft2foul
      team_foul_htract[a, 3] <<- act_srft / neut_srft
      team_foul_htract[a, 4] <<- act_dsrft * neut_srft
      
    }
  }
  
  team_list <<- adv_game_log_join[, c(1, 3)]
  game_log_adj1 <<- data.frame(team_list, team_off_htract, team_def_htract, team_foul_htract, stringsAsFactors = FALSE)
  team_sums_adj1 <<- game_log_adj1 %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3), SM3 = mean(SM3),
              FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST), STL = mean(STL), BLK = mean(BLK),
              TO = mean(TO), FOUL = mean(FOUL), DE = mean(DE), deFG = mean(deFG), dSR2 = mean(dSR2), dSM2 = mean(dSM2), 
              dSR3 = mean(dSR3), dSM3 = mean(dSM3), dFTR = mean(dFTR),
              dORB = mean(dORB), dDRB = mean(dDRB), dTRB = mean(dTRB), dAST = mean(dAST), dSTL = mean(dSTL), dBLK = mean(dBLK),
              dTO = mean(dTO), dFOUL = mean(dFOUL), FTperFOUL = mean(FTperFOUL), dFTperFOUL = mean(dFTperFOUL),
              SRFT = mean(SRFT), dSRFT = mean(dSRFT))
}



other_log_splitter_adj2 <- function(xxxx) {
  
  opp_log_stats <<- opp_log_list %>%
    left_join(team_sums_adj1, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME"))
  
  numbers_frame_team <<- as.matrix(adv_game_log_join[, 9:46])
  numbers_frame_opp <<- as.matrix(opp_log_stats[, 3:36])
  
  a <- 1 
  g <- nrow(numbers_frame_opp)
  
  for (a in a:g) {
    
    act_oe <- as.numeric(numbers_frame_opp[a, 1]) 
    act_efg <- as.numeric(numbers_frame_opp[a, 2]) 
    act_sr2 <- as.numeric(numbers_frame_opp[a, 3]) 
    act_sm2 <- as.numeric(numbers_frame_opp[a, 4]) 
    act_sr3 <- as.numeric(numbers_frame_opp[a, 5]) 
    act_sm3 <- as.numeric(numbers_frame_opp[a, 6]) 
    act_ftr <- as.numeric(numbers_frame_opp[a, 7])
    act_orb <- as.numeric(numbers_frame_opp[a, 8])
    act_drb <- as.numeric(numbers_frame_opp[a, 9]) 
    act_trb <- as.numeric(numbers_frame_opp[a, 10]) 
    act_ast <- as.numeric(numbers_frame_opp[a, 11]) 
    act_stl <- as.numeric(numbers_frame_opp[a, 12]) 
    act_blk <- as.numeric(numbers_frame_opp[a, 13]) 
    act_tov <- as.numeric(numbers_frame_opp[a, 14]) 
    act_foul <- as.numeric(numbers_frame_opp[a, 15]) 
    
    act_de <- as.numeric(numbers_frame_opp[a, 16]) 
    act_defg <- as.numeric(numbers_frame_opp[a, 17]) 
    act_dsr2 <- as.numeric(numbers_frame_opp[a, 18]) 
    act_dsm2 <- as.numeric(numbers_frame_opp[a, 19])
    act_dsr3 <- as.numeric(numbers_frame_opp[a, 20]) 
    act_dsm3 <- as.numeric(numbers_frame_opp[a, 21]) 
    act_dftr <- as.numeric(numbers_frame_opp[a, 22]) 
    act_dorb <- as.numeric(numbers_frame_opp[a, 23]) 
    act_ddrb <- as.numeric(numbers_frame_opp[a, 24]) 
    act_dtrb <- as.numeric(numbers_frame_opp[a, 25]) 
    act_dast <- as.numeric(numbers_frame_opp[a, 26]) 
    act_dstl <- as.numeric(numbers_frame_opp[a, 27]) 
    act_dblk <- as.numeric(numbers_frame_opp[a, 28]) 
    act_dtov <- as.numeric(numbers_frame_opp[a, 29]) 
    act_dfoul <- as.numeric(numbers_frame_opp[a, 30]) 
    
    act_ft2foul <- as.numeric(numbers_frame_opp[a, 31]) 
    act_dft2foul <- as.numeric(numbers_frame_opp[a, 32])
    
    act_srft <- as.numeric(numbers_frame_opp[a, 33]) 
    act_dsrft <- as.numeric(numbers_frame_opp[a, 34])
    
    dif_oe <- act_oe / nat_oe
    dif_efg <- act_efg / nat_efg
    dif_sr2 <- 1
    dif_sm2 <- act_sm2 / nat_sm2
    dif_sr3 <- 1
    dif_sm3 <- act_sm3 / nat_sm3
    dif_ftr <- act_ftr / nat_ftr
    dif_orb <- act_orb / nat_orb
    dif_drb <- act_drb / nat_drb
    dif_trb <- act_trb / nat_trb
    dif_ast <- act_ast / nat_ast
    dif_stl <- act_stl / nat_stl
    dif_blk <- act_blk / nat_blk
    dif_tov <- act_tov / nat_tov
    dif_foul <- act_foul / nat_foul
    
    dif_de <- act_de / nat_oe
    dif_defg <- act_defg / nat_efg
    dif_dsr2 <- 1
    dif_dsm2 <- act_dsm2 / nat_sm2
    dif_dsr3 <- 1
    dif_dsm3 <- act_dsm3 / nat_sm3
    dif_dftr <- act_dftr / nat_ftr
    dif_dorb <- act_dorb / nat_orb
    dif_ddrb <- act_ddrb / nat_drb
    dif_dtrb <- act_dtrb / nat_trb
    dif_dast <- act_dast / nat_ast
    dif_dstl <- act_dstl / nat_stl
    dif_dblk <- act_dblk / nat_blk
    dif_dtov <- act_dtov / nat_tov
    dif_dfoul <- act_dfoul / nat_foul
    
    dif_ft2foul <- act_ft2foul / nat_ft2foul
    dif_dft2foul <- act_dft2foul / nat_ft2foul
    
    dif_srft <- act_srft / nat_srft
    dif_dsrft <- act_dsrft / nat_srft
    
    
    if (a == 1) { dif_frame_opp <<- matrix(c(dif_oe, dif_efg, dif_sr2, dif_sm2, dif_sr3, dif_sm3, dif_ftr,
                                             dif_orb, dif_drb, dif_trb, dif_ast, dif_stl,
                                             dif_blk, dif_tov, dif_foul, dif_de, dif_defg, dif_dsr2, dif_dsm2,
                                             dif_dsr3, dif_dsm3, dif_dftr, dif_dorb, dif_ddrb, dif_dtrb,
                                             dif_dast, dif_dstl, dif_dblk, dif_dtov, dif_dfoul, dif_ft2foul, dif_dft2foul,
                                             dif_srft, dif_dsrft), nrow = 1) }
    else { 
      dif_frame_opp2 <<- matrix(c(dif_oe, dif_efg, dif_sr2, dif_sm2, dif_sr3, dif_sm3, dif_ftr,
                                  dif_orb, dif_drb, dif_trb, dif_ast, dif_stl,
                                  dif_blk, dif_tov, dif_foul, dif_de, dif_defg, dif_dsr2, dif_dsm2,
                                  dif_dsr3, dif_dsm3, dif_dftr, dif_dorb, dif_ddrb, dif_dtrb,
                                  dif_dast, dif_dstl, dif_dblk, dif_dtov, dif_dfoul, dif_ft2foul, dif_dft2foul,
                                  dif_srft, dif_dsrft), nrow = 1)
      
      dif_frame_opp <<- rbind(dif_frame_opp, dif_frame_opp2)
    }
  }
  
  colnames(dif_frame_opp) <<- c("OE", "eFG", "SR2", "SM2", 
                                "SR3", "SM3", "FTR", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TO",
                                "FOUL", "DE", "deFG", "dSR2", "dSM2", 
                                "dSR3", "dSM3", "dFTR", "dORB", "dDRB", "dTRB", "dAST", "dSTL", "dBLK", "dTO",
                                "dFOUL", "FTperFOUL", "dFTperFOUL", "SRFT", "dSRFT")
  
  dif_frame_opp_join <<- data.frame(adv_game_log_join[, 1:8], dif_frame_opp, stringsAsFactors = FALSE)
}



game_log_adjust_2 <- function(xxxx) {
  
  opp_off_stats <<- dif_frame_opp[,  1:15]
  opp_def_stats <<- dif_frame_opp[, 16:30]
  opp_foul_stats <<- cbind(dif_frame_opp[, 32], dif_frame_opp[, 31], dif_frame_opp[, 34], dif_frame_opp[, 33])
  
  team_off_subtract <<- team_off_stats / opp_def_stats
  team_def_subtract <<- team_def_stats / opp_off_stats
  team_foul_subtract <<- team_foul_stats / opp_foul_stats
  
  team_off_htract <<- team_off_subtract
  team_def_htract <<- team_def_subtract
  team_foul_htract <<- team_foul_subtract
  
  #Currently Using Macro Home Advantages
  
  a <- 1
  g <- nrow(loc_mx)
  
  for (a in a:g) {
    
    act_loc <- loc_mx[a]
    
    act_oe <- team_off_subtract[a, 1]
    act_efg <- team_off_subtract[a, 2]
    act_sr2 <- team_off_subtract[a, 3]
    act_sm2 <- team_off_subtract[a, 4]
    act_sr3 <- team_off_subtract[a, 5]
    act_sm3 <- team_off_subtract[a, 6]
    act_ftr <- team_off_subtract[a, 7]
    act_orb <- team_off_subtract[a, 8]
    act_drb <- team_off_subtract[a, 9]
    act_trb <- team_off_subtract[a, 10]
    act_ast <- team_off_subtract[a, 11]
    act_stl <- team_off_subtract[a, 12]
    act_blk <- team_off_subtract[a, 13]
    act_to <- team_off_subtract[a, 14]
    act_foul <- team_off_subtract[a, 15]
    
    act_de <- team_def_subtract[a, 1]
    act_defg <- team_def_subtract[a, 2]
    act_dsr2 <- team_def_subtract[a, 3]
    act_dsm2 <- team_def_subtract[a, 4]
    act_dsr3 <- team_def_subtract[a, 5]
    act_dsm3 <- team_def_subtract[a, 6]
    act_dftr <- team_def_subtract[a, 7]
    act_dorb <- team_def_subtract[a, 8]
    act_ddrb <- team_def_subtract[a, 9]
    act_dtrb <- team_def_subtract[a, 10]
    act_dast <- team_def_subtract[a, 11]
    act_dstl <- team_def_subtract[a, 12]
    act_dblk <- team_def_subtract[a, 13]
    act_dto <- team_def_subtract[a, 14]
    act_dfoul <- team_def_subtract[a, 15]
    
    act_ft2foul <- team_foul_subtract[a, 1]
    act_dft2foul <- team_foul_subtract[a, 2]
    act_srft <- team_foul_subtract[a, 3]
    act_dsrft <- team_foul_subtract[a, 4]
    
    if (act_loc == "H") {
      
      team_off_htract[a, 1] <<- act_oe / home_oe
      team_off_htract[a, 2] <<- act_efg / home_efg 
      team_off_htract[a, 3] <<- act_sr2 / home_sr2
      team_off_htract[a, 4] <<- act_sm2 / home_sm2 
      team_off_htract[a, 5] <<- act_sr3 / home_sr3 
      team_off_htract[a, 6] <<- act_sm3 / home_sm3
      team_off_htract[a, 7] <<- act_ftr / home_ftr
      team_off_htract[a, 8] <<- act_orb / home_orb 
      team_off_htract[a, 9] <<- act_drb / home_drb 
      team_off_htract[a, 10] <<- act_trb / home_trb
      team_off_htract[a, 11] <<- act_ast / home_ast 
      team_off_htract[a, 12] <<- act_stl / home_stl 
      team_off_htract[a, 13] <<- act_blk / home_blk 
      team_off_htract[a, 14] <<- act_to / home_to 
      team_off_htract[a, 15] <<- act_foul / home_foul 
      
      team_def_htract[a, 1] <<- act_de * home_oe
      team_def_htract[a, 2] <<- act_defg * home_efg 
      team_def_htract[a, 3] <<- act_dsr2 * home_sr2
      team_def_htract[a, 4] <<- act_dsm2 * home_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * home_sr3 
      team_def_htract[a, 6] <<- act_dsm3 * home_sm3
      team_def_htract[a, 7] <<- act_dftr * home_ftr
      team_def_htract[a, 8] <<- act_dorb * home_orb 
      team_def_htract[a, 9] <<- act_ddrb * home_drb 
      team_def_htract[a, 10] <<- act_dtrb * home_trb
      team_def_htract[a, 11] <<- act_dast * home_ast 
      team_def_htract[a, 12] <<- act_dstl * home_stl 
      team_def_htract[a, 13] <<- act_dblk * home_blk 
      team_def_htract[a, 14] <<- act_dto * home_to 
      team_def_htract[a, 15] <<- act_dfoul * home_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / home_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * home_ft2foul
      team_foul_htract[a, 3] <<- act_srft / home_srft
      team_foul_htract[a, 4] <<- act_dsrft * home_srft
      
    }
    
    if (act_loc == "A") {
      
      team_off_htract[a, 1] <<- act_oe / away_oe
      team_off_htract[a, 2] <<- act_efg / away_efg 
      team_off_htract[a, 3] <<- act_sr2 / away_sr2
      team_off_htract[a, 4] <<- act_sm2 / away_sm2 
      team_off_htract[a, 5] <<- act_sr3 / away_sr3 
      team_off_htract[a, 6] <<- act_sm3 / away_sm3
      team_off_htract[a, 7] <<- act_ftr / away_ftr 
      team_off_htract[a, 8] <<- act_orb / away_orb 
      team_off_htract[a, 9] <<- act_drb / away_drb 
      team_off_htract[a, 10] <<- act_trb / away_trb
      team_off_htract[a, 11] <<- act_ast / away_ast 
      team_off_htract[a, 12] <<- act_stl / away_stl 
      team_off_htract[a, 13] <<- act_blk / away_blk 
      team_off_htract[a, 14] <<- act_to / away_to 
      team_off_htract[a, 15] <<- act_foul / away_foul 
      
      team_def_htract[a, 1] <<- act_de * away_oe
      team_def_htract[a, 2] <<- act_defg * away_efg 
      team_def_htract[a, 3] <<- act_dsr2 * away_sr2
      team_def_htract[a, 4] <<- act_dsm2 * away_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * away_sr3 
      team_def_htract[a, 6] <<- act_dsm3 * away_sm3
      team_def_htract[a, 7] <<- act_dftr * away_ftr 
      team_def_htract[a, 8] <<- act_dorb * away_orb 
      team_def_htract[a, 9] <<- act_ddrb * away_drb 
      team_def_htract[a, 10] <<- act_dtrb * away_trb
      team_def_htract[a, 11] <<- act_dast * away_ast 
      team_def_htract[a, 12] <<- act_dstl * away_stl 
      team_def_htract[a, 13] <<- act_dblk * away_blk 
      team_def_htract[a, 14] <<- act_dto * away_to 
      team_def_htract[a, 15] <<- act_dfoul * away_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / away_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * away_ft2foul
      team_foul_htract[a, 3] <<- act_srft / away_srft
      team_foul_htract[a, 4] <<- act_dsrft * away_srft
      
    }
    
    if (act_loc == "N") {
      
      team_off_htract[a, 1] <<- act_oe / neut_oe
      team_off_htract[a, 2] <<- act_efg / neut_efg 
      team_off_htract[a, 3] <<- act_sr2 / neut_sr2
      team_off_htract[a, 4] <<- act_sm2 / neut_sm2 
      team_off_htract[a, 5] <<- act_sr3 / neut_sr3 
      team_off_htract[a, 6] <<- act_sm3 / neut_sm3
      team_off_htract[a, 7] <<- act_ftr / neut_ftr
      team_off_htract[a, 8] <<- act_orb / neut_orb 
      team_off_htract[a, 9] <<- act_drb / neut_drb 
      team_off_htract[a, 10] <<- act_trb / neut_trb
      team_off_htract[a, 11] <<- act_ast / neut_ast 
      team_off_htract[a, 12] <<- act_stl / neut_stl 
      team_off_htract[a, 13] <<- act_blk / neut_blk 
      team_off_htract[a, 14] <<- act_to / neut_to 
      team_off_htract[a, 15] <<- act_foul / neut_foul 
      
      team_def_htract[a, 1] <<- act_de * neut_oe
      team_def_htract[a, 2] <<- act_defg * neut_efg 
      team_def_htract[a, 3] <<- act_dsr2 * neut_sr2
      team_def_htract[a, 4] <<- act_dsm2 * neut_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * neut_sr3 
      team_def_htract[a, 6] <<- act_dsm3 * neut_sm3
      team_def_htract[a, 7] <<- act_dftr * neut_ftr 
      team_def_htract[a, 8] <<- act_dorb * neut_orb 
      team_def_htract[a, 9] <<- act_ddrb * neut_drb 
      team_def_htract[a, 10] <<- act_dtrb * neut_trb
      team_def_htract[a, 11] <<- act_dast * neut_ast 
      team_def_htract[a, 12] <<- act_dstl * neut_stl 
      team_def_htract[a, 13] <<- act_dblk * neut_blk 
      team_def_htract[a, 14] <<- act_dto * neut_to 
      team_def_htract[a, 15] <<- act_dfoul * neut_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / neut_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * neut_ft2foul
      team_foul_htract[a, 3] <<- act_srft / neut_srft
      team_foul_htract[a, 4] <<- act_dsrft * neut_srft
      
    }
  }
  
  game_log_adj2 <<- data.frame(team_list, team_off_htract, team_def_htract, team_foul_htract, stringsAsFactors = FALSE)
  team_sums_adj2 <<- game_log_adj2 %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3), SM3 = mean(SM3),
              FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST), STL = mean(STL), BLK = mean(BLK),
              TO = mean(TO), FOUL = mean(FOUL), DE = mean(DE), deFG = mean(deFG), dSR2 = mean(dSR2), dSM2 = mean(dSM2), 
              dSR3 = mean(dSR3), dSM3 = mean(dSM3), dFTR = mean(dFTR),
              dORB = mean(dORB), dDRB = mean(dDRB), dTRB = mean(dTRB), dAST = mean(dAST), dSTL = mean(dSTL), dBLK = mean(dBLK),
              dTO = mean(dTO), dFOUL = mean(dFOUL), FTperFOUL = mean(FTperFOUL), dFTperFOUL = mean(dFTperFOUL),
              SRFT = mean(SRFT), dSRFT = mean(dSRFT))
}


other_log_splitter_adj3 <- function(xxxx) {
  
  opp_log_stats <<- opp_log_list %>%
    left_join(team_sums_adj2, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME"))
  
  numbers_frame_team <<- as.matrix(adv_game_log_join[, 9:46])
  numbers_frame_opp <<- as.matrix(opp_log_stats[, 3:36])
  
  a <- 1 
  g <- nrow(numbers_frame_opp)
  
  for (a in a:g) {
    
    act_oe <- as.numeric(numbers_frame_opp[a, 1]) 
    act_efg <- as.numeric(numbers_frame_opp[a, 2]) 
    act_sr2 <- as.numeric(numbers_frame_opp[a, 3]) 
    act_sm2 <- as.numeric(numbers_frame_opp[a, 4]) 
    act_sr3 <- as.numeric(numbers_frame_opp[a, 5]) 
    act_sm3 <- as.numeric(numbers_frame_opp[a, 6]) 
    act_ftr <- as.numeric(numbers_frame_opp[a, 7])
    act_orb <- as.numeric(numbers_frame_opp[a, 8])
    act_drb <- as.numeric(numbers_frame_opp[a, 9]) 
    act_trb <- as.numeric(numbers_frame_opp[a, 10]) 
    act_ast <- as.numeric(numbers_frame_opp[a, 11]) 
    act_stl <- as.numeric(numbers_frame_opp[a, 12]) 
    act_blk <- as.numeric(numbers_frame_opp[a, 13]) 
    act_tov <- as.numeric(numbers_frame_opp[a, 14]) 
    act_foul <- as.numeric(numbers_frame_opp[a, 15]) 
    
    act_de <- as.numeric(numbers_frame_opp[a, 16]) 
    act_defg <- as.numeric(numbers_frame_opp[a, 17]) 
    act_dsr2 <- as.numeric(numbers_frame_opp[a, 18]) 
    act_dsm2 <- as.numeric(numbers_frame_opp[a, 19])
    act_dsr3 <- as.numeric(numbers_frame_opp[a, 20]) 
    act_dsm3 <- as.numeric(numbers_frame_opp[a, 21]) 
    act_dftr <- as.numeric(numbers_frame_opp[a, 22]) 
    act_dorb <- as.numeric(numbers_frame_opp[a, 23]) 
    act_ddrb <- as.numeric(numbers_frame_opp[a, 24]) 
    act_dtrb <- as.numeric(numbers_frame_opp[a, 25]) 
    act_dast <- as.numeric(numbers_frame_opp[a, 26]) 
    act_dstl <- as.numeric(numbers_frame_opp[a, 27]) 
    act_dblk <- as.numeric(numbers_frame_opp[a, 28]) 
    act_dtov <- as.numeric(numbers_frame_opp[a, 29]) 
    act_dfoul <- as.numeric(numbers_frame_opp[a, 30]) 
    
    act_ft2foul <- as.numeric(numbers_frame_opp[a, 31]) 
    act_dft2foul <- as.numeric(numbers_frame_opp[a, 32])
    act_srft <- as.numeric(numbers_frame_opp[a, 33]) 
    act_dsrft <- as.numeric(numbers_frame_opp[a, 34])
    
    dif_oe <- act_oe / nat_oe
    dif_efg <- act_efg / nat_efg
    dif_sr2 <- act_sr2 / nat_sr2
    dif_sm2 <- act_sm2 / nat_sm2
    dif_sr3 <- act_sr3 / nat_sr3
    dif_sm3 <- act_sm3 / nat_sm3
    dif_ftr <- act_ftr / nat_ftr
    dif_orb <- act_orb / nat_orb
    dif_drb <- act_drb / nat_drb
    dif_trb <- act_trb / nat_trb
    dif_ast <- act_ast / nat_ast
    dif_stl <- act_stl / nat_stl
    dif_blk <- act_blk / nat_blk
    dif_tov <- act_tov / nat_tov
    dif_foul <- act_foul / nat_foul
    
    dif_de <- act_de / nat_oe
    dif_defg <- act_defg / nat_efg
    dif_dsr2 <- act_dsr2 / nat_sr2
    dif_dsm2 <- act_dsm2 / nat_sm2
    dif_dsr3 <- act_dsr3 / nat_sr3
    dif_dsm3 <- act_dsm3 / nat_sm3
    dif_dftr <- act_dftr / nat_ftr
    dif_dorb <- act_dorb / nat_orb
    dif_ddrb <- act_ddrb / nat_drb
    dif_dtrb <- act_dtrb / nat_trb
    dif_dast <- act_dast / nat_ast
    dif_dstl <- act_dstl / nat_stl
    dif_dblk <- act_dblk / nat_blk
    dif_dtov <- act_dtov / nat_tov
    dif_dfoul <- act_dfoul / nat_foul
    
    dif_ft2foul <- act_ft2foul / nat_ft2foul
    dif_dft2foul <- act_dft2foul / nat_ft2foul
    dif_srft <- act_srft / nat_srft
    dif_dsrft <- act_dsrft / nat_srft
    
    
    if (a == 1) { dif_frame_opp <<- matrix(c(dif_oe, dif_efg, dif_sr2, dif_sm2, dif_sr3, dif_sm3, dif_ftr,
                                             dif_orb, dif_drb, dif_trb, dif_ast, dif_stl,
                                             dif_blk, dif_tov, dif_foul, dif_de, dif_defg, dif_dsr2, dif_dsm2,
                                             dif_dsr3, dif_dsm3, dif_dftr, dif_dorb, dif_ddrb, dif_dtrb,
                                             dif_dast, dif_dstl, dif_dblk, dif_dtov, dif_dfoul, dif_ft2foul, dif_dft2foul,
                                             dif_srft, dif_dsrft), nrow = 1) }
    else { 
      dif_frame_opp2 <<- matrix(c(dif_oe, dif_efg, dif_sr2, dif_sm2, dif_sr3, dif_sm3, dif_ftr,
                                  dif_orb, dif_drb, dif_trb, dif_ast, dif_stl,
                                  dif_blk, dif_tov, dif_foul, dif_de, dif_defg, dif_dsr2, dif_dsm2,
                                  dif_dsr3, dif_dsm3, dif_dftr, dif_dorb, dif_ddrb, dif_dtrb,
                                  dif_dast, dif_dstl, dif_dblk, dif_dtov, dif_dfoul, dif_ft2foul, dif_dft2foul,
                                  dif_srft, dif_dsrft), nrow = 1)
      
      dif_frame_opp <<- rbind(dif_frame_opp, dif_frame_opp2)
    }
  }
  
  colnames(dif_frame_opp) <<- c("OE", "eFG", "SR2", "SM2", 
                                "SR3", "SM3", "FTR", "ORB", "DRB", "TRB", "AST", "STL", "BLK", "TO",
                                "FOUL", "DE", "deFG", "dSR2", "dSM2", 
                                "dSR3", "dSM3", "dFTR", "dORB", "dDRB", "dTRB", "dAST", "dSTL", "dBLK", "dTO",
                                "dFOUL", "FTperFOUL", "dFTperFOUL", "SRFT", "dSRFT")
  
  dif_frame_opp_join <<- data.frame(adv_game_log_join[, 1:8], dif_frame_opp, stringsAsFactors = FALSE)
}



game_log_adjust_3 <- function(xxxx) {
  
  opp_off_stats <<- dif_frame_opp[,  1:15]
  opp_def_stats <<- dif_frame_opp[, 16:30]
  opp_foul_stats <<- cbind(dif_frame_opp[, 32], dif_frame_opp[, 31], dif_frame_opp[, 34], dif_frame_opp[, 33])
  
  team_off_subtract <<- team_off_stats / opp_def_stats
  team_def_subtract <<- team_def_stats / opp_off_stats
  team_foul_subtract <<- team_foul_stats / opp_foul_stats
  
  team_off_htract <<- team_off_subtract
  team_def_htract <<- team_def_subtract
  team_foul_htract <<- team_foul_subtract
  
  #Currently Using Macro Home Advantages
  
  a <- 1
  g <- nrow(loc_mx)
  
  for (a in a:g) {
    
    act_loc <- loc_mx[a]
    
    act_oe <- team_off_subtract[a, 1]
    act_efg <- team_off_subtract[a, 2]
    act_sr2 <- team_off_subtract[a, 3]
    act_sm2 <- team_off_subtract[a, 4]
    act_sr3 <- team_off_subtract[a, 5]
    act_sm3 <- team_off_subtract[a, 6]
    act_ftr <- team_off_subtract[a, 7]
    act_orb <- team_off_subtract[a, 8]
    act_drb <- team_off_subtract[a, 9]
    act_trb <- team_off_subtract[a, 10]
    act_ast <- team_off_subtract[a, 11]
    act_stl <- team_off_subtract[a, 12]
    act_blk <- team_off_subtract[a, 13]
    act_to <- team_off_subtract[a, 14]
    act_foul <- team_off_subtract[a, 15]
    
    act_de <- team_def_subtract[a, 1]
    act_defg <- team_def_subtract[a, 2]
    act_dsr2 <- team_def_subtract[a, 3]
    act_dsm2 <- team_def_subtract[a, 4]
    act_dsr3 <- team_def_subtract[a, 5]
    act_dsm3 <- team_def_subtract[a, 6]
    act_dftr <- team_def_subtract[a, 7]
    act_dorb <- team_def_subtract[a, 8]
    act_ddrb <- team_def_subtract[a, 9]
    act_dtrb <- team_def_subtract[a, 10]
    act_dast <- team_def_subtract[a, 11]
    act_dstl <- team_def_subtract[a, 12]
    act_dblk <- team_def_subtract[a, 13]
    act_dto <- team_def_subtract[a, 14]
    act_dfoul <- team_def_subtract[a, 15]
    
    act_ft2foul <- team_foul_subtract[a, 1]
    act_dft2foul <- team_foul_subtract[a, 2]
    act_srft <- team_foul_subtract[a, 3]
    act_dsrft <- team_foul_subtract[a, 4]
    
    if (act_loc == "H") {
      
      team_off_htract[a, 1] <<- act_oe / home_oe
      team_off_htract[a, 2] <<- act_efg / home_efg 
      team_off_htract[a, 3] <<- act_sr2 / home_sr2
      team_off_htract[a, 4] <<- act_sm2 / home_sm2 
      team_off_htract[a, 5] <<- act_sr3 / home_sr3 
      team_off_htract[a, 6] <<- act_sm3 / home_sm3
      team_off_htract[a, 7] <<- act_ftr / home_ftr
      team_off_htract[a, 8] <<- act_orb / home_orb 
      team_off_htract[a, 9] <<- act_drb / home_drb 
      team_off_htract[a, 10] <<- act_trb / home_trb
      team_off_htract[a, 11] <<- act_ast / home_ast 
      team_off_htract[a, 12] <<- act_stl / home_stl 
      team_off_htract[a, 13] <<- act_blk / home_blk 
      team_off_htract[a, 14] <<- act_to / home_to 
      team_off_htract[a, 15] <<- act_foul / home_foul 
      
      team_def_htract[a, 1] <<- act_de * home_oe
      team_def_htract[a, 2] <<- act_defg * home_efg 
      team_def_htract[a, 3] <<- act_dsr2 * home_sr2
      team_def_htract[a, 4] <<- act_dsm2 * home_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * home_sr3 
      team_def_htract[a, 6] <<- act_dsm3 * home_sm3
      team_def_htract[a, 7] <<- act_dftr * home_ftr
      team_def_htract[a, 8] <<- act_dorb * home_orb 
      team_def_htract[a, 9] <<- act_ddrb * home_drb 
      team_def_htract[a, 10] <<- act_dtrb * home_trb
      team_def_htract[a, 11] <<- act_dast * home_ast 
      team_def_htract[a, 12] <<- act_dstl * home_stl 
      team_def_htract[a, 13] <<- act_dblk * home_blk 
      team_def_htract[a, 14] <<- act_dto * home_to 
      team_def_htract[a, 15] <<- act_dfoul * home_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / home_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * home_ft2foul
      team_foul_htract[a, 3] <<- act_srft / home_srft
      team_foul_htract[a, 4] <<- act_dsrft * home_srft
      
    }
    
    if (act_loc == "A") {
      
      team_off_htract[a, 1] <<- act_oe / away_oe
      team_off_htract[a, 2] <<- act_efg / away_efg 
      team_off_htract[a, 3] <<- act_sr2 / away_sr2
      team_off_htract[a, 4] <<- act_sm2 / away_sm2 
      team_off_htract[a, 5] <<- act_sr3 / away_sr3 
      team_off_htract[a, 6] <<- act_sm3 / away_sm3
      team_off_htract[a, 7] <<- act_ftr / away_ftr 
      team_off_htract[a, 8] <<- act_orb / away_orb 
      team_off_htract[a, 9] <<- act_drb / away_drb 
      team_off_htract[a, 10] <<- act_trb / away_trb
      team_off_htract[a, 11] <<- act_ast / away_ast 
      team_off_htract[a, 12] <<- act_stl / away_stl 
      team_off_htract[a, 13] <<- act_blk / away_blk 
      team_off_htract[a, 14] <<- act_to / away_to 
      team_off_htract[a, 15] <<- act_foul / away_foul 
      
      team_def_htract[a, 1] <<- act_de * away_oe
      team_def_htract[a, 2] <<- act_defg * away_efg 
      team_def_htract[a, 3] <<- act_dsr2 * away_sr2
      team_def_htract[a, 4] <<- act_dsm2 * away_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * away_sr3 
      team_def_htract[a, 6] <<- act_dsm3 * away_sm3
      team_def_htract[a, 7] <<- act_dftr * away_ftr 
      team_def_htract[a, 8] <<- act_dorb * away_orb 
      team_def_htract[a, 9] <<- act_ddrb * away_drb 
      team_def_htract[a, 10] <<- act_dtrb * away_trb
      team_def_htract[a, 11] <<- act_dast * away_ast 
      team_def_htract[a, 12] <<- act_dstl * away_stl 
      team_def_htract[a, 13] <<- act_dblk * away_blk 
      team_def_htract[a, 14] <<- act_dto * away_to 
      team_def_htract[a, 15] <<- act_dfoul * away_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / away_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * away_ft2foul
      team_foul_htract[a, 3] <<- act_srft / away_srft
      team_foul_htract[a, 4] <<- act_dsrft * away_srft
      
    }
    
    if (act_loc == "N") {
      
      team_off_htract[a, 1] <<- act_oe / neut_oe
      team_off_htract[a, 2] <<- act_efg / neut_efg 
      team_off_htract[a, 3] <<- act_sr2 / neut_sr2
      team_off_htract[a, 4] <<- act_sm2 / neut_sm2 
      team_off_htract[a, 5] <<- act_sr3 / neut_sr3 
      team_off_htract[a, 6] <<- act_sm3 / neut_sm3
      team_off_htract[a, 7] <<- act_ftr / neut_ftr
      team_off_htract[a, 8] <<- act_orb / neut_orb 
      team_off_htract[a, 9] <<- act_drb / neut_drb 
      team_off_htract[a, 10] <<- act_trb / neut_trb
      team_off_htract[a, 11] <<- act_ast / neut_ast 
      team_off_htract[a, 12] <<- act_stl / neut_stl 
      team_off_htract[a, 13] <<- act_blk / neut_blk 
      team_off_htract[a, 14] <<- act_to / neut_to 
      team_off_htract[a, 15] <<- act_foul / neut_foul 
      
      team_def_htract[a, 1] <<- act_de * neut_oe
      team_def_htract[a, 2] <<- act_defg * neut_efg 
      team_def_htract[a, 3] <<- act_dsr2 * neut_sr2
      team_def_htract[a, 4] <<- act_dsm2 * neut_sm2 
      team_def_htract[a, 5] <<- act_dsr3 * neut_sr3 
      team_def_htract[a, 6] <<- act_dsm3 * neut_sm3
      team_def_htract[a, 7] <<- act_dftr * neut_ftr 
      team_def_htract[a, 8] <<- act_dorb * neut_orb 
      team_def_htract[a, 9] <<- act_ddrb * neut_drb 
      team_def_htract[a, 10] <<- act_dtrb * neut_trb
      team_def_htract[a, 11] <<- act_dast * neut_ast 
      team_def_htract[a, 12] <<- act_dstl * neut_stl 
      team_def_htract[a, 13] <<- act_dblk * neut_blk 
      team_def_htract[a, 14] <<- act_dto * neut_to 
      team_def_htract[a, 15] <<- act_dfoul * neut_foul 
      
      team_foul_htract[a, 1] <<- act_ft2foul / neut_ft2foul
      team_foul_htract[a, 2] <<- act_dft2foul * neut_ft2foul
      team_foul_htract[a, 3] <<- act_srft / neut_srft
      team_foul_htract[a, 4] <<- act_dsrft * neut_srft
      
    }
  }
  
  game_log_adj2 <<- data.frame(team_list, team_off_htract, team_def_htract, team_foul_htract, stringsAsFactors = FALSE)
  team_sums_adj2 <<- game_log_adj2 %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(OE = mean(OE), eFG = mean(eFG), SR2 = mean(SR2), SM2 = mean(SM2), SR3 = mean(SR3), SM3 = mean(SM3),
              FTR = mean(FTR), ORB = mean(ORB), DRB = mean(DRB), TRB = mean(TRB), AST = mean(AST), STL = mean(STL), BLK = mean(BLK),
              TO = mean(TO), FOUL = mean(FOUL), DE = mean(DE), deFG = mean(deFG), dSR2 = mean(dSR2), dSM2 = mean(dSM2), 
              dSR3 = mean(dSR3), dSM3 = mean(dSM3), dFTR = mean(dFTR),
              dORB = mean(dORB), dDRB = mean(dDRB), dTRB = mean(dTRB), dAST = mean(dAST), dSTL = mean(dSTL), dBLK = mean(dBLK),
              dTO = mean(dTO), dFOUL = mean(dFOUL), FTperFOUL = mean(FTperFOUL), dFTperFOUL = mean(dFTperFOUL),
              SRFT = mean(SRFT), dSRFT = mean(dSRFT))
}

multiple_adj_pre <- function(x) {
  
  preseason_frame <<- read_csv("preseason_2021.csv")
  print("---Compiling Raw Per Possession Stats---")
  compile_raw_per_pos(1)
  print("---Compiling National Averages---")
  compile_nat_avg(1)
  print("---Compiling Game by Game Per Pos Log---")
  compile_gamelog_adv(1)
  print("---Compiling Home Advantage Calculations---")
  home_adv_calcu3(1)
  print("---Adjustment Round 1---")
  other_log_splitter(0)
  game_log_adjust_1(0)
  print("---Adjustment Round 2---")
  other_log_splitter_adj2(0)
  game_log_adjust_2(0)
  
  g <- x - 2
  
  a <- 1
  
  for (a in a:g) {
    
    v <- a + 2
    print(paste("---Adjustment Round ", v, "---", sep = ""))
    other_log_splitter_adj3(0)
    game_log_adjust_3(0)
    
  }
  game_sums_final_adj <<- game_log_adj2
  team_sums_final_adj <<- team_sums_adj2
  
  a <- 3
  g <- 36
  for (a in a:g) {
    
    nan_search <<- which(team_sums_final_adj[, a] == "NaN")
    if (length(nan_search) > 0) {
      if (a == 8 | a == 23) { repper <<- nat_avgs$mr3p_do }
      if (a == 9 | a == 24) { repper <<- nat_avgs$ftr_do }
      if (a == 10 | a == 25) { repper <<- nat_avgs$orb_do }
      if (a == 13 | a == 28) { repper <<- nat_avgs$ast_do }
      if (a == 14 | a == 29) { repper <<- nat_avgs$stl_do }
      if (a == 15 | a == 30) { repper <<- nat_avgs$blk_do }
      if (a == 16 | a == 31) { repper <<- nat_avgs$tov_do }
      if (a == 17 | a == 32) { repper <<- nat_avgs$foul_do }
      team_sums_final_adj[nan_search, a] <<- repper
      
    }
  }
  
  print("---Tempo Adjustment Round 1---")
  tempo_adjust_do1(0)
  print("---Tempo Adjustment Round 2---")
  tempo_adjust_do2(0)
  
  a <- 1
  g <- x - 2
  
  for (a in a:g) {
    
    v <- a + 2
    print(paste("---Tempo Adjustment Round ", v, "---", sep = ""))
    tempo_adjust_do3(0)
    
  }
  
  game_tempo_final_adj <<- tempo_adj2_frame
  team_tempo_final_adj <<- tempo_sums_adj2
  print("---Free Throw Adjustment---")
  free_throw_fixer(1)
  game_ft_final_adj <<- update_ft_frame
  
  master_game_adj_frame_uw <<- cbind(game_sums_final_adj, game_tempo_final_adj[, 3], game_ft_final_adj[, 3:4])
  master_team_adj_frame_uw <<- data.frame(team_sums_final_adj, team_tempo_final_adj[ ,3], team_ft_final_adj[, 5], stringsAsFactors = FALSE)
  colnames(master_game_adj_frame_uw)[37] <<- "Tempo"
  
  print("----Team Weighted Average Do----")
  team_weighted_avg(1)
  print("---Preseason Merge 1-----")
  preseason_merge(1)
  master_team_adj_frame_wt <<- pre_merge_output
  
  print("---Pre Adjustment Round 1---")
  other_log_splitter(1)
  game_log_adjust_1(1)
  print("---Pre Adjustment Round 2---")
  other_log_splitter_adj2(1)
  game_log_adjust_2(1)
  
  g <- x - 2
  
  a <- 1
  
  for (a in a:g) {
    
    v <- a + 2
    print(paste("---Pre Adjustment Round ", v, "---", sep = ""))
    other_log_splitter_adj3(1)
    game_log_adjust_3(1)
    
  }
  game_sums_final_adj <<- game_log_adj2
  team_sums_final_adj <<- team_sums_adj2
  
  a <- 3
  g <- 36
  for (a in a:g) {
    
    nan_search <<- which(team_sums_final_adj[, a] == "NaN")
    if (length(nan_search) > 0) {
      if (a == 8 | a == 23) { repper <<- nat_avgs$mr3p_do }
      if (a == 9 | a == 24) { repper <<- nat_avgs$ftr_do }
      if (a == 10 | a == 25) { repper <<- nat_avgs$orb_do }
      if (a == 13 | a == 28) { repper <<- nat_avgs$ast_do }
      if (a == 14 | a == 29) { repper <<- nat_avgs$stl_do }
      if (a == 15 | a == 30) { repper <<- nat_avgs$blk_do }
      if (a == 16 | a == 31) { repper <<- nat_avgs$tov_do }
      if (a == 17 | a == 32) { repper <<- nat_avgs$foul_do }
      team_sums_final_adj[nan_search, a] <<- repper
      
    }
  }
  
  print("---Pre Tempo Adjustment Round 1---")
  tempo_adjust_do1(1)
  print("---Pre Tempo Adjustment Round 2---")
  tempo_adjust_do2(1)
  
  a <- 1
  g <- x - 2
  
  for (a in a:g) {
    
    v <- a + 2
    print(paste("---Pre Tempo Adjustment Round ", v, "---", sep = ""))
    tempo_adjust_do3(1)
    
  }
  
  game_tempo_final_adj <<- tempo_adj2_frame
  team_tempo_final_adj <<- tempo_sums_adj2
  
  print("---Pre Free Throw Adjustment---")
  free_throw_fixer(1)
  game_ft_final_adj <<- update_ft_frame
  
  master_game_adj_frame_uw <<- cbind(game_sums_final_adj, game_tempo_final_adj[, 3], game_ft_final_adj[, 3:4])
  master_team_adj_frame_uw <<- data.frame(team_sums_final_adj, team_tempo_final_adj[ ,3], team_ft_final_adj[, 5], stringsAsFactors = FALSE)
  colnames(master_game_adj_frame_uw)[37] <<- "Tempo"
  
  print("----Pre Team Weighted Average Do----")
  team_weighted_avg(1)
  print("----Preseason Merge 2------")
  preseason_merge(1)
  master_team_adj_frame_wt <<- pre_merge_output
  
  print("----Individual Stat Box Score Join-----")
  ind_box_join(1)
  
  print("----Individual Raw Average Compute-----")
  ind_totals_raw_to_avg(1)
  
  print("----Individual Adjusted Stats-----")
  ind_log_to_opp(1)
  
  print("----Individual Weighted Average-----")
  ind_weighted_avg(1)
  
  print("----Free Throw Adjustment Ind-------")
  free_throw_fixer_ind(1)
  
  print("------Ind Name Append---------")
  ind_name_append(1)
  
  print("-----Finished------")
}

ind_name_append <- function(xxxx) {
  
  a <- 1
  g <- nrow(teamid_box_sum)
  ct <- 0
  
  for (a in a:g) {
    
    vid_use <<- as.numeric(teamid_box_sum$VID[a])
    file_name_csv <<- paste("teamscores\\team_scores_", vid_use, ".csv", sep = "")
    t_csv_frame <<- read_csv(file_name_csv)
    t_csv_gb <<- t_csv_frame %>%
      group_by(PLAYER_NAME, PLAYER_ID) %>%
      summarize(ct = n()) %>%
      arrange(desc(ct))
    
    ind_adj_filt <<- ind_all_adj_frame %>%
      filter(TEAM_ID == vid_use)
    
    b <- 1
    h <- nrow(ind_adj_filt)
    
    for (b in b:h) {
    
      pid_use <<- as.numeric(ind_adj_filt[b, 1]) 
      pid_which <<- which(t_csv_gb$PLAYER_ID == pid_use)
      pid_which <<- pid_which[[1]]
      pid_name_use <<- as.character(t_csv_gb[pid_which, 1])
      ind_adj_filt[b, 31] <<- pid_name_use
      
    }
    
    ind_adj_filt <<- ind_adj_filt[, c(31, 1:30)]
    colnames(ind_adj_filt)[1] <<- "PLAYER_NAME"
    if (a == 1) { ind_adj_rbind <<- ind_adj_filt }
    else { ind_adj_rbind <<- rbind(ind_adj_rbind, ind_adj_filt) }
  }
  
  ind_all_adj_frame <<- ind_adj_rbind
}

preseason_merge <- function(xxxx) {
  
  fade_out <<- 28
  fade_out_tempo <<- 8
  
  ngames_frame <<- team_game_by_game_raw %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(Games = n())
  
  preseason_frame_use <<- preseason_frame[, c(1:2, 5:38, 4, 39)]
  pre_merge_output <<- master_team_adj_frame_wt
  pre_merge_output[, 3:38] <<- 0
  
  a <- 1
  g <- nrow(master_team_adj_frame_wt)
  #g <- 1
  
  for (a in a:g) {
    
    id_use <<- as.numeric(master_team_adj_frame_wt[a, 1])
    
    b <- 3
    h <- 38
    #h <- 3
    pre_line <<- which(preseason_frame_use$NCAA == id_use)
    n_games_use <<- as.numeric(ngames_frame[which(ngames_frame$TEAM_ID == id_use), 3])
    n_games_use_tempo <<- n_games_use
    if (n_games_use > fade_out) { n_games_use <<- fade_out }
    if (n_games_use_tempo > fade_out_tempo) { n_games_use_tempo <<- fade_out_tempo }
    
    for (b in b:h) {
      
      if (b == 37) { 
        fade_out_use <<- fade_out_tempo 
        ng_use <<- n_games_use_tempo
      }
      else { 
        fade_out_use <<- fade_out
        ng_use <<- n_games_use
      }
      
      pre_stat <<- as.numeric(preseason_frame_use[pre_line, b])
      act_stat <<- as.numeric(master_team_adj_frame_wt[a, b])
      act_stat_portion <<- (ng_use / fade_out_use) * act_stat
      pre_stat_portion <<- (1 - (ng_use / fade_out_use)) * pre_stat
      merge_stat <<- act_stat_portion + pre_stat_portion
      pre_merge_output[a, b] <<- merge_stat
      
    }
  }
}

multiple_adj_do <- function(x) {
  
  print("---Compiling Raw Per Possession Stats---")
  compile_raw_per_pos(1)
  print("---Compiling National Averages---")
  compile_nat_avg(1)
  print("---Compiling Game by Game Per Pos Log---")
  compile_gamelog_adv(1)
  print("---Compiling Home Advantage Calculations---")
  home_adv_calcu(1)
  print("---Adjustment Round 1---")
  other_log_splitter(1)
  game_log_adjust_1(1)
  print("---Adjustment Round 2---")
  other_log_splitter_adj2(1)
  game_log_adjust_2(1)
  
  g <- x - 2
  
  a <- 1
  
  for (a in a:g) {
    
    v <- a + 2
    print(paste("---Adjustment Round ", v, "---", sep = ""))
    other_log_splitter_adj3(1)
    game_log_adjust_3(1)
    
  }
  game_sums_final_adj <<- game_log_adj2
  team_sums_final_adj <<- team_sums_adj2
  
  a <- 3
  g <- 36
  for (a in a:g) {
    
    nan_search <<- which(team_sums_final_adj[, a] == "NaN")
    if (length(nan_search) > 0) {
      if (a == 8 | a == 23) { repper <<- nat_avgs$mr3p_do }
      if (a == 9 | a == 24) { repper <<- nat_avgs$ftr_do }
      if (a == 10 | a == 25) { repper <<- nat_avgs$orb_do }
      if (a == 13 | a == 28) { repper <<- nat_avgs$ast_do }
      if (a == 14 | a == 29) { repper <<- nat_avgs$stl_do }
      if (a == 15 | a == 30) { repper <<- nat_avgs$blk_do }
      if (a == 16 | a == 31) { repper <<- nat_avgs$tov_do }
      if (a == 17 | a == 32) { repper <<- nat_avgs$foul_do }
      team_sums_final_adj[nan_search, a] <<- repper
      
    }
  }
  
  print("---Tempo Adjustment Round 1---")
  tempo_adjust_do1(1)
  print("---Tempo Adjustment Round 2---")
  tempo_adjust_do2(1)
  
  a <- 1
  g <- x - 2
  
  for (a in a:g) {
    
    v <- a + 2
    print(paste("---Tempo Adjustment Round ", v, "---", sep = ""))
    tempo_adjust_do3(1)
    
  }
  
  game_tempo_final_adj <<- tempo_adj2_frame
  team_tempo_final_adj <<- tempo_sums_adj2
  print("---Free Throw Adjustment---")
  free_throw_fixer(1)
  game_ft_final_adj <<- update_ft_frame
  
  master_game_adj_frame_uw <<- cbind(game_sums_final_adj, game_tempo_final_adj[, 3], game_ft_final_adj[, 3:4])
  master_team_adj_frame_uw <<- data.frame(team_sums_final_adj, team_tempo_final_adj[ ,3], team_ft_final_adj[, 5], stringsAsFactors = FALSE)
  colnames(master_game_adj_frame_uw)[37] <<- "Tempo"
  
  print("----Team Weighted Average Do----")
  team_weighted_avg(1)
  
  print("----Individual Stat Box Score Join-----")
  ind_box_join(1)
  
  print("----Individual Raw Average Compute-----")
  ind_totals_raw_to_avg(1)
  
  print("----Individual Adjusted Stats-----")
  ind_log_to_opp(1)
  
  print("----Individual Weighted Average-----")
  ind_weighted_avg(1)
  
  print("----Free Throw Adjustment Ind-------")
  free_throw_fixer_ind(1)
  
  print("-----Finished------")
}


#TEMPO ADJUSTMENT

tempo_adjust_do1 <- function(xxxx) {
  
  if (xxxx != 1) {
    raw_tempo_frame <<- raw_perpos_frame[, 1:3]
    game_tempo_frame <<- adv_game_log_join[, c(1, 2, 3, 4, 46)]
    
    game_tempo_frame_join <<- game_tempo_frame %>%
      left_join(raw_tempo_frame, by = c("TEAM_ID" = "act_teamid", "TEAM_NAME" = "act_team"))
    
    game_tempo_frame_joins <<- game_tempo_frame_join %>%
      left_join(raw_tempo_frame, by = c("OPP_ID" = "act_teamid", "OPP_NAME" = "act_team"))
    
  }
  else {
    raw_tempo_frame <<- pre_merge_output[, c(1:2, 37)]
    game_tempo_frame <<- adv_game_log_join[, c(1, 2, 3, 4, 46)]
    
    game_tempo_frame_join <<- game_tempo_frame %>%
      left_join(raw_tempo_frame, by = c("TEAM_ID", "TEAM_NAME"))
    
    game_tempo_frame_joins <<- game_tempo_frame_join %>%
      left_join(raw_tempo_frame, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME"))
    
  }
  
  colnames(game_tempo_frame_joins) <<- c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_TEMPO", "TEAM_TEMPO", "OPP_TEMPO")
  
  tempo_mx <<- as.matrix(game_tempo_frame_joins[, c(5, 6, 7)])
  tempo_rep_mx <<- tempo_mx[, 2:3]
  nat_avg_tempo <<- as.numeric(nat_avgs$tempo_do)
  
  a <- 1
  g <- nrow(tempo_mx)
  
  for (a in a:g) {
    
    t_pace <<- tempo_mx[a, 2]
    o_pace <<- tempo_mx[a, 3]
    g_pace <<- tempo_mx[a, 1]
    
    exp_pace <<- ((t_pace - nat_avg_tempo) + (o_pace - nat_avg_tempo)) + nat_avg_tempo
    
    exp_dif <<- (g_pace - exp_pace) / 2
    exp_pct <<- exp_dif / g_pace
    exp_mult <<- 1 + exp_pct
    
    t_pace_adj <<- exp_mult * t_pace
    o_pace_adj <<- exp_mult * o_pace
    
    tempo_rep_mx[a, 1] <<- t_pace_adj
    tempo_rep_mx[a, 2] <<- o_pace_adj
    
  }
  
  tempo_adj1_frame <<- data.frame(game_tempo_frame_joins[, c(1, 3)], tempo_rep_mx[, 1], stringsAsFactors = FALSE)
  colnames(tempo_adj1_frame) <<- c("TEAM_ID", "TEAM_NAME", "ADJTempo")
  tempo_sums_adj1 <<- tempo_adj1_frame %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(Tempo = mean(ADJTempo))
}


tempo_adjust_do2 <- function(xxxx) {
  
  raw_tempo_frame <<- tempo_sums_adj1
  game_tempo_frame <<- adv_game_log_join[, c(1, 2, 3, 4, 46)]
  
  game_tempo_frame_join <<- game_tempo_frame %>%
    left_join(raw_tempo_frame, by = c("TEAM_ID", "TEAM_NAME"))
  
  game_tempo_frame_joins <<- game_tempo_frame_join %>%
    left_join(raw_tempo_frame, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME"))
  
  colnames(game_tempo_frame_joins) <<- c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_TEMPO", "TEAM_TEMPO", "OPP_TEMPO")
  
  tempo_mx <<- as.matrix(game_tempo_frame_joins[, c(5, 6, 7)])
  tempo_rep_mx <<- tempo_mx[, 2:3]
  nat_avg_tempo <<- as.numeric(nat_avgs$tempo_do)
  
  a <- 1
  g <- nrow(tempo_mx)
  
  for (a in a:g) {
    
    t_pace <<- tempo_mx[a, 2]
    o_pace <<- tempo_mx[a, 3]
    g_pace <<- tempo_mx[a, 1]
    
    exp_pace <<- ((t_pace - nat_avg_tempo) + (o_pace - nat_avg_tempo)) + nat_avg_tempo
    
    exp_dif <<- (g_pace - exp_pace) / 2
    exp_pct <<- exp_dif / g_pace
    exp_mult <<- 1 + exp_pct
    
    t_pace_adj <<- exp_mult * t_pace
    o_pace_adj <<- exp_mult * o_pace
    
    tempo_rep_mx[a, 1] <<- t_pace_adj
    tempo_rep_mx[a, 2] <<- o_pace_adj
    
  }
  
  tempo_adj2_frame <<- data.frame(game_tempo_frame_joins[, c(1, 3)], tempo_rep_mx[, 1], stringsAsFactors = FALSE)
  colnames(tempo_adj2_frame) <<- c("TEAM_ID", "TEAM_NAME", "ADJTempo")
  tempo_sums_adj2 <<- tempo_adj2_frame %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(Tempo = mean(ADJTempo))
}


tempo_adjust_do3 <- function(xxxx) {
  
  raw_tempo_frame <<- tempo_sums_adj2
  game_tempo_frame <<- adv_game_log_join[, c(1, 2, 3, 4, 46)]
  
  game_tempo_frame_join <<- game_tempo_frame %>%
    left_join(raw_tempo_frame, by = c("TEAM_ID", "TEAM_NAME"))
  
  game_tempo_frame_joins <<- game_tempo_frame_join %>%
    left_join(raw_tempo_frame, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME"))
  
  colnames(game_tempo_frame_joins) <<- c("TEAM_ID", "OPP_ID", "TEAM_NAME", "OPP_NAME", "GAME_TEMPO", "TEAM_TEMPO", "OPP_TEMPO")
  
  tempo_mx <<- as.matrix(game_tempo_frame_joins[, c(5, 6, 7)])
  tempo_rep_mx <<- tempo_mx[, 2:3]
  nat_avg_tempo <<- as.numeric(nat_avgs$tempo_do)
  
  a <- 1
  g <- nrow(tempo_mx)
  
  for (a in a:g) {
    
    t_pace <<- tempo_mx[a, 2]
    o_pace <<- tempo_mx[a, 3]
    g_pace <<- tempo_mx[a, 1]
    
    exp_pace <<- ((t_pace - nat_avg_tempo) + (o_pace - nat_avg_tempo)) + nat_avg_tempo
    
    exp_dif <<- (g_pace - exp_pace) / 2
    exp_pct <<- exp_dif / g_pace
    exp_mult <<- 1 + exp_pct
    
    t_pace_adj <<- exp_mult * t_pace
    o_pace_adj <<- exp_mult * o_pace
    
    tempo_rep_mx[a, 1] <<- t_pace_adj
    tempo_rep_mx[a, 2] <<- o_pace_adj
    
  }
  
  tempo_adj2_frame <<- data.frame(game_tempo_frame_joins[, c(1, 3)], tempo_rep_mx[, 1], stringsAsFactors = FALSE)
  colnames(tempo_adj2_frame) <<- c("TEAM_ID", "TEAM_NAME", "ADJTempo")
  tempo_sums_adj2 <<- tempo_adj2_frame %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(Tempo = mean(ADJTempo))
}

free_throw_fixer <- function(xxxx) {
  
  ft_frame <<- team_game_by_game_raw[, c(1, 3, 7, 17, 18)]
  
  ft_loc_frame <<- as.matrix(ft_frame[, 3])
  ft_num_frame <<- as.matrix(ft_frame[, 4:5])
  ft_rep_frame <<- ft_num_frame
  
  h_ft <<- advantage_frame[1, 10]
  a_ft <<- advantage_frame[2, 10]
  n_ft <<- advantage_frame[3, 10]
  
  a <- 1
  g <- nrow(ft_loc_frame)
  
  for (a in a:g) {
    
    act_ftm <<- ft_num_frame[a, 1]
    act_fta <<- ft_num_frame[a, 2]
    act_loc <<- ft_loc_frame[a]
    
    if (act_loc == "H") {
      
      ft_adj <<- act_ftm + (act_fta * (1 - h_ft))
      new_ftm <<- ft_adj
      
    }
    
    if (act_loc == "A") {
      
      ft_adj <<- act_ftm + (act_fta * (1 - a_ft))
      new_ftm <<- ft_adj
      
    }
    
    if (act_loc == "N") {
      
      ft_adj <<- act_ftm + (act_fta * (1 - n_ft))
      new_ftm <<- ft_adj
      
    }
    
    ft_rep_frame[a, 1] <<- new_ftm
  }
  
  update_ft_frame <<- data.frame(ft_frame[, 1:2], ft_rep_frame)
  
  colnames(update_ft_frame)[1:2] <<- c("TEAM_ID", "TEAM_NAME")
  team_ft_final_adj <<- update_ft_frame %>%
    group_by(TEAM_ID, TEAM_NAME) %>%
    summarize(FTM = sum(FTM), FTA = sum(FTA)) %>%
    mutate(FT = FTM / FTA)
}



#----------INDIVIDUAL STATS--------------

#Load Ind Game by Game Box Scores fro team_box_scorelist and calculate per possession stats

ind_box_join <- function(xxxx) {
  
  a <- 1
  g <- length(team_box_scorelist)


  
  for (a in a:g) {
    
    print(a)
    tmp_ind_frame <<- team_box_scorelist[[a]]
    
    if (length(team_box_scorelist[[a]]) > 0) {
    
      act_teamid <<- tmp_ind_frame$TEAM_ID[1]
      act_teamname <<- tmp_ind_frame$TEAM_NAME[1]
      team_raw_tmp <<- team_game_by_game_raw %>%
        ungroup() %>%
        filter(TEAM_ID == act_teamid) %>%
        select(TEAM_ID, TEAM_NAME, GAME_ID, MINS, ORB, DRB, ORBd, DRBd, PTS, FGM, FGA, FTM, FTA, AST, TO, BLK, STL, FOUL,
               PTSd, FGMd, FGAd, FTMd, FTAd, TRBd, TOd, PM3)
      
      colnames(team_raw_tmp)[5:26] <<- c("TEAM_ORB", "TEAM_DRB", "OPP_ORB", "OPP_DRB", "TEAM_PTS", "TEAM_FGM", "TEAM_FGA", "TEAM_FTM",
                                        "TEAM_FTA", "TEAM_AST", "TEAM_TO", "TEAM_BLK", "TEAM_STL", "TEAM_FOUL", "OPP_PTS", "OPP_FGM",
                                        "OPP_FGA", "OPP_FTM", "OPP_FTA", "OPP_TRB", "OPP_TO", "TEAM_PM3")
      
      adv_log_tmp <<- adv_game_log_join %>%
        filter(TEAM_ID == act_teamid) %>%
        select(TEAM_ID, TEAM_NAME, GAME_ID, Tempo)
      
      team_adv_comb <<- team_raw_tmp %>%
        left_join(adv_log_tmp, by = c("TEAM_ID", "TEAM_NAME", "GAME_ID")) %>%
        mutate(GAME_MINS = MINS / 5, GAME_POS = ((Tempo / 40) * GAME_MINS)) %>%
        select(TEAM_ID, TEAM_NAME, GAME_ID, GAME_MINS, GAME_POS, TEAM_ORB, TEAM_DRB, OPP_ORB, OPP_DRB, TEAM_PTS, TEAM_FGM, TEAM_FGA, 
               TEAM_FTM, TEAM_FTA, TEAM_AST, TEAM_TO, TEAM_BLK, TEAM_STL, TEAM_FOUL, OPP_PTS, OPP_FGM,
               OPP_FGA, OPP_FTM, OPP_FTA, OPP_TRB, OPP_TO, TEAM_PM3)
      
      ind_boxscore_join <<- tmp_ind_frame %>%
        left_join(team_adv_comb, by = c("TEAM_ID", "TEAM_NAME", "GAME_ID"))
      
      ind_stats_mx <<- as.matrix(ind_boxscore_join[, c(1:18, 29:52)])
      ind_rep_mx <<- ind_stats_mx[, 1:23]
      colnames(ind_rep_mx) <<- c("pMIN", "pPOS", "pOE", "peFG", "pSR2", "pSM2", "pSR3", "pSM3", "pFTPCT", "pFTR",
                                "pORB", "pDRB", "pTRB", "pAST", "pSTL", "pBLK", "pTO", "pFOUL", "pORTG",
                                "pDRTG", "pUSG", "pPROD", "pSRFT")
      ind_rep_mx[, 1:23] <<- 0
      
      ind_joiner_frame <<- ind_boxscore_join[, 19:30]
      
      b <- 1
      h <- nrow(ind_stats_mx)
      for (b in b:h) {
        act_mins <<- ind_stats_mx[b, 1]
        act_fgm <<- ind_stats_mx[b, 2]
        act_fga <<- ind_stats_mx[b, 3]
        act_pm2 <<- ind_stats_mx[b, 4]
        act_pa2 <<- ind_stats_mx[b, 5]
        act_pm3 <<- ind_stats_mx[b, 6]
        act_pa3 <<- ind_stats_mx[b, 7]
        act_ftm <<- ind_stats_mx[b, 8]
        act_fta <<- ind_stats_mx[b, 9]
        act_pts <<- ind_stats_mx[b, 10]
        act_orb <<- ind_stats_mx[b, 11]
        act_drb <<- ind_stats_mx[b, 12]
        act_trb <<- ind_stats_mx[b, 13]
        act_ast <<- ind_stats_mx[b, 14]
        act_tov <<- ind_stats_mx[b, 15]
        act_stl <<- ind_stats_mx[b, 16]
        act_blk <<- ind_stats_mx[b, 17]
        act_pfl <<- ind_stats_mx[b, 18]
        game_mins <<- ind_stats_mx[b, 19]
        whole_mins <<- game_mins * 5
        game_pos <<- ind_stats_mx[b, 20]
        team_orb <<- ind_stats_mx[b, 21]
        team_drb <<- ind_stats_mx[b, 22]
        opp_orb <<- ind_stats_mx[b, 23]
        opp_drb <<- ind_stats_mx[b, 24]
        team_pts <<- ind_stats_mx[b, 25]
        team_fgm <<- ind_stats_mx[b, 26]
        team_fga <<- ind_stats_mx[b, 27]
        team_ftm <<- ind_stats_mx[b, 28]
        team_fta <<- ind_stats_mx[b, 29]
        team_ast <<- ind_stats_mx[b, 30]
        team_to <<- ind_stats_mx[b, 31]
        team_blk <<- ind_stats_mx[b, 32]
        team_stl <<- ind_stats_mx[b, 33]
        team_pfl <<- ind_stats_mx[b, 34]
        opp_pts <<- ind_stats_mx[b, 35]
        opp_fgm <<- ind_stats_mx[b, 36]
        opp_fga <<- ind_stats_mx[b, 37]
        opp_ftm <<- ind_stats_mx[b, 38]
        opp_fta <<- ind_stats_mx[b, 39]
        opp_trb <<- ind_stats_mx[b, 40]
        opp_tov <<- ind_stats_mx[b, 41]
        team_pm3 <<- ind_stats_mx[b, 42]
        team_orbpct <<- team_orb / (team_orb + opp_drb)
        dorbpct <<- opp_orb / (opp_orb + team_drb)
        dfgpct <<- opp_fgm / opp_fga
        
        mins_pct <<- act_mins / game_mins
        pos_played <<- mins_pct * game_pos
        ind_oe <<- act_pts / pos_played
        ind_efg <<- (act_fgm + (0.5 * act_pm3)) / act_fga
        ind_sr2 <<- act_pa2 / pos_played
        ind_sm2 <<- act_pm2 / act_pa2
        ind_sr3 <<- act_pa3 / pos_played
        ind_sm3 <<- act_pm3 / act_pa3
        ind_ftp <<- act_ftm / act_fta
        if (act_fta == 0) { ind_ftp <<- 0 }
        ind_ftr <<- act_fta / act_fga
        if (act_fga == 0) { 
          ind_efg <<- 0 
          ind_ftr <<- 0
        }
        if (act_pa2 == 0) {
          ind_sm2 <<- 0
        }
        if (act_pa3 == 0)  {
          ind_sm3 <<- 0
        }
        ind_ast <<- act_ast / pos_played
        ind_stl <<- act_stl / pos_played
        ind_blk <<- act_blk / pos_played
        ind_tov <<- act_tov / pos_played
        ind_pfl <<- act_pfl / (pos_played * 2)
        ind_srft <<- act_fta / pos_played
        
        #Rebounds
        
        avail_team_orb <<- team_orb * mins_pct 
        avail_team_drb <<- team_drb * mins_pct
        avail_opp_orb <<- opp_orb * mins_pct
        avail_opp_drb <<- opp_drb * mins_pct
        avail_team_trb <<- avail_team_orb + avail_team_drb
        avail_opp_trb <<- avail_opp_orb + avail_opp_drb
        
        ind_orb <<- act_orb / (avail_team_orb + avail_opp_drb)
        ind_drb <<- act_drb / (avail_team_drb + avail_opp_orb)
        ind_trb <<- act_trb / (avail_team_trb + avail_opp_trb)
        
        #Personal Efficiency Ratings
        
        #Offensive Rating
        
        if (act_fta == 0) { fta_filler <<- 0 }
        else { fta_filler <<- act_ftm / act_fta }
        if (act_fga == 0) { fga_filler <<- 0 }
        else { fga_filler <<- 2 * act_fga }
        
        q_ast <<- ((act_mins / (whole_mins / 5)) * (1.14 * ((team_ast - act_ast) / team_fgm))) + 
                  ((((team_ast / whole_mins) * act_mins * 5 - act_ast) / ((team_fgm / whole_mins) * act_mins * 5 - act_fgm)) * 
                     (1 - (act_mins / (whole_mins / 5))))
        
        fg_part <<- act_fgm * (1 - 0.5 * ((act_pts - act_ftm) / (fga_filler)) * q_ast)
        if (act_fga == 0) { fg_part <<- 0 }
        ast_part <<- 0.5 * (((team_pts - team_ftm) - (act_pts - act_ftm)) / (2 * (team_fga - act_fga))) * act_ast
        ft_part <<- (1-(1-(fta_filler))^2) * 0.475 * act_fta
        team_scoring_pos <<- team_fgm + (1 - (1 - (team_ftm / team_fta))^2) * team_fta * 0.475
        team_play_pct <<- team_scoring_pos / (team_fga + team_fta * 0.475 + team_to)
        team_orb_weight <<- ((1 - team_orbpct) * team_play_pct) / ((1 - team_orbpct) * team_play_pct + team_orbpct * (1 - team_play_pct))
        team_orb_eq <<- team_orb / (team_orb + (opp_trb - opp_orb))
        orb_part <<- act_orb * team_orb_weight * team_play_pct
        
        score_poss <<- (fg_part + ast_part + ft_part) * (1 - (team_orb / team_scoring_pos) * (team_orb_weight * team_play_pct)) + orb_part
        
        fgxposs <<- (act_fga - act_fgm) * (1 - 1.07 * team_orb_eq)
        ftxposs <<- ((1 - (fta_filler)) ^ 2) * 0.475 * act_fta
        
        tot_poss <<- score_poss + fgxposs + ftxposs + act_tov
        pprod_fg_part <<- 2 * (act_fgm + 0.5 * act_pm3) * (1 - 0.5 * ((act_pts - act_ftm) / (fga_filler)) * q_ast)
        if (act_fga == 0) { pprod_fg_part <<- 0 }
        pprod_ast_part <<- 2 * ((team_fgm - act_fgm + 0.5 * (team_pm3 - act_pm3)) / (team_fgm - act_fgm)) * 0.5 * (((team_pts - team_ftm) - (act_pts - act_ftm)) / (2 * (team_fga - act_fga))) * act_ast
        pprod_orb_part <<- act_orb * team_orb_weight * team_play_pct * (team_pts / (team_fgm + (1 - (1 - (team_ftm / team_fta)) ^ 2) * 0.475 * team_fta))
        
        pprod <<- (pprod_fg_part + pprod_ast_part + act_ftm) * (1 - (team_orb / team_scoring_pos) * team_orb_weight * team_play_pct) + pprod_orb_part
        
        offensive_rating <<- 100 * (pprod / tot_poss)
        
        #Defensive Rating
        
        fmwt <<- (dfgpct * (1 - dorbpct)) / (dfgpct * (1 - dorbpct) + (1 - dfgpct) * dorbpct)
        stops1 <<- act_stl + act_blk * fmwt * (1 - 1.07 * dorbpct) + act_drb * (1 - fmwt)
        stops2 <<- (((opp_fga - opp_fgm - team_blk) / whole_mins) * fmwt * (1 - 1.07 * dorbpct) + ((opp_tov - team_stl) / whole_mins)) *
                     act_mins + (act_pfl / team_pfl) * .475 * opp_fta * (1 - (opp_ftm / opp_fta))^2
        stops <<- stops1 + stops2
        stop_pct <<- (stops * whole_mins) / (game_pos * act_mins)
        
        team_def_rtg <<- 100 * (opp_pts / game_pos)
        d_pts_per_scposs <<- opp_pts / (opp_fgm + (1 - (1 - (opp_ftm / opp_fta))^2) * opp_fta * .475)
              
        defensive_rating <<- team_def_rtg + 0.2375 * (100 * d_pts_per_scposs * (1 - stop_pct) - team_def_rtg)
        
        usage_ <<- 100 * ((act_fga + 0.475 * act_fta + act_tov) * (game_mins)) / (act_mins * (team_fga + 0.475 * team_fta + team_to))
        
        ind_rep_mx[b, 1] <<- mins_pct
        ind_rep_mx[b, 2] <<- pos_played
        ind_rep_mx[b, 3] <<- ind_oe
        ind_rep_mx[b, 4] <<- ind_efg
        ind_rep_mx[b, 5] <<- ind_sr2
        ind_rep_mx[b, 6] <<- ind_sm2
        ind_rep_mx[b, 7] <<- ind_sr3
        ind_rep_mx[b, 8] <<- ind_sm3
        ind_rep_mx[b, 9] <<- ind_ftp
        ind_rep_mx[b, 10] <<- ind_ftr
        ind_rep_mx[b, 11] <<- ind_orb
        ind_rep_mx[b, 12] <<- ind_drb
        ind_rep_mx[b, 13] <<- ind_trb
        ind_rep_mx[b, 14] <<- ind_ast
        ind_rep_mx[b, 15] <<- ind_stl
        ind_rep_mx[b, 16] <<- ind_blk
        ind_rep_mx[b, 17] <<- ind_tov
        ind_rep_mx[b, 18] <<- ind_pfl
        ind_rep_mx[b, 19] <<- offensive_rating
        ind_rep_mx[b, 20] <<- defensive_rating
        ind_rep_mx[b, 21] <<- usage_
        ind_rep_mx[b, 22] <<- pprod
        ind_rep_mx[b, 23] <<- ind_srft
        
        if (pos_played <= 0) { ind_rep_mx[b, ] <<- 0 }
        
        
        
      }
      
      which_nan_ortg <<- which(is.nan(ind_rep_mx[, 19]) == TRUE)
      which_nan_prod <<- which(is.nan(ind_rep_mx[, 22]) == TRUE)
      
      ind_rep_mx[which_nan_ortg, 19] <<- 0
      ind_rep_mx[which_nan_prod, 22] <<- 0
      
      ind_game_log_join <<- data.frame(ind_joiner_frame, ind_rep_mx, stringsAsFactors = FALSE)
      
      if (a == 1) { ind_game_log_list <<- list(ind_game_log_join) }
      else { ind_game_log_list[[a]] <<- ind_game_log_join }
    
    }
  }
}

ind_totals_raw_to_avg <- function(xxxx) {
  
  a <- 1
  g <- length(ind_game_log_list)
  
  for (a in a:g) {
    
    tmp_log_use <<- ind_game_log_list[[a]]
    
    if (length(tmp_log_use) > 0) {
    
      tmp_teamid <<- tmp_log_use[1, 3]
      ind_totals_raw_filt <<- ind_totals_raw %>%
        filter(TEAM_ID == tmp_teamid, MINS > 0) 
      
      pos_log_use <<- tmp_log_use %>%
        group_by(PLAYER_ID, TEAM_NAME) %>%
        summarize(POS = sum(pPOS)) %>%
        filter(POS > 0)
      
      ind_totals_wpos <<- ind_totals_raw_filt %>%
        left_join(pos_log_use, by = c("TEAM_NAME", "PLAYER_ID"))
      
      ttr_use <<- which(team_totals_raw$TEAM_ID == tmp_teamid)
      
      team_total_line <<- team_totals_raw[ttr_use, ]
      
      team_pos <<- team_total_line$POS[1]
      team_mins <<- team_total_line$MINS[1] / 5
      whole_mins <<- team_mins * 5
      team_pts <<- team_total_line$PTS[1]
      team_fgm <<- team_total_line$FGM[1]
      team_fga <<- team_total_line$FGA[1]
      team_pm2 <<- team_total_line$PM2[1]
      team_pa2 <<- team_total_line$PA2[1]
      team_pm3 <<- team_total_line$PM3[1]
      team_pa3 <<- team_total_line$PA3[1]
      team_ftm <<- team_total_line$FTM[1]
      team_fta <<- team_total_line$FTA[1]
      team_orb <<- team_total_line$ORB[1]
      team_drb <<- team_total_line$DRB[1]
      team_trb <<- team_total_line$TRB[1]
      team_ast <<- team_total_line$AST[1]
      team_stl <<- team_total_line$STL[1]
      team_blk <<- team_total_line$BLK[1]
      team_to <<- team_total_line$TO[1]
      team_pfl <<- team_total_line$FOUL[1]
      
      opp_pts <<- team_total_line$oPTS[1]
      opp_fgm <<- team_total_line$oFGM[1]
      opp_fga <<- team_total_line$oFGA[1]
      opp_pm2 <<- team_total_line$oPM2[1]
      opp_pa2 <<- team_total_line$oPA2[1]
      opp_pm3 <<- team_total_line$oPM3[1]
      opp_pa3 <<- team_total_line$oPA3[1]
      opp_ftm <<- team_total_line$oFTM[1]
      opp_fta <<- team_total_line$oFTA[1]
      opp_orb <<- team_total_line$oORB[1]
      opp_drb <<- team_total_line$oDRB[1]
      opp_trb <<- team_total_line$oTRB[1]
      opp_ast <<- team_total_line$oAST[1]
      opp_stl <<- team_total_line$oSTL[1]
      opp_blk <<- team_total_line$oBLK[1]
      opp_tov <<- team_total_line$oTO[1]
      opp_pfl <<- team_total_line$oFOUL[1]
      
      team_orbpct <<- team_orb / (team_orb + opp_drb)
      dorbpct <<- opp_orb / (opp_orb + team_drb)
      dfgpct <<- opp_fgm / opp_fga
      game_mins <<- team_mins
      
      ind_totals_mx <<- as.matrix(ind_totals_wpos[, 5:23])
      mx_row <<- nrow(ind_totals_mx)
      ind_repla_mx <<- matrix(0, nrow = mx_row, ncol = 23)
      ind_info_mx <<- ind_totals_wpos[, 1:6]
      colnames(ind_repla_mx) <<- c("pMIN", "pPOS", "pOE", "peFG", "pSR2", "pSM2", "pSR3", "pSM3", "pFTPCT", "pFTR",
                                 "pORB", "pDRB", "pTRB", "pAST", "pSTL", "pBLK", "pTO", "pFOUL", "pORTG",
                                 "pDRTG", "pUSG", "pPROD", "SRFT")
      
      b <- 1
      h <- nrow(ind_totals_mx)
      
      for (b in b:h) {
      
        act_mins <<- ind_totals_mx[b, 1]
        act_pts <<- ind_totals_mx[b, 2]
        act_fgm <<- ind_totals_mx[b, 3]
        act_fga <<- ind_totals_mx[b, 4]
        act_pm2 <<- ind_totals_mx[b, 5]
        act_pa2 <<- ind_totals_mx[b, 6]
        act_pm3 <<- ind_totals_mx[b, 7]
        act_pa3 <<- ind_totals_mx[b, 8]
        act_ftm <<- ind_totals_mx[b, 9]
        act_fta <<- ind_totals_mx[b, 10]
        act_orb <<- ind_totals_mx[b, 11]
        act_drb <<- ind_totals_mx[b, 12]
        act_trb <<- ind_totals_mx[b, 13]
        act_ast <<- ind_totals_mx[b, 14]
        act_stl <<- ind_totals_mx[b, 15]
        act_blk <<- ind_totals_mx[b, 16]
        act_tov <<- ind_totals_mx[b, 17]
        act_pfl <<- ind_totals_mx[b, 18]
        act_pos <<- ind_totals_mx[b, 19]
        
        mins_pct <<- act_mins / team_mins
        pos_played <<- mins_pct * team_pos
        ind_oe <<- act_pts / pos_played
        ind_efg <<- (act_fgm + (0.5 * act_pm3)) / act_fga
        ind_sr2 <<- act_pa2 / pos_played
        ind_sm2 <<- act_pm2 / act_pa2
        ind_sr3 <<- act_pa3 / pos_played
        ind_sm3 <<- act_pm3 / act_pa3
        ind_ftp <<- act_ftm / act_fta
        if (act_fta == 0) { ind_ftp <<- 0 }
        ind_ftr <<- act_fta / act_fga
        if (act_fga == 0) { 
          ind_efg <<- 0 
          ind_ftr <<- 0
        }
        if (act_pa2 == 0) {
          ind_sm2 <<- 0
        }
        if (act_pa3 == 0)  {
          ind_sm3 <<- 0
        }
        ind_ast <<- act_ast / pos_played
        ind_stl <<- act_stl / pos_played
        ind_blk <<- act_blk / pos_played
        ind_tov <<- act_tov / pos_played
        ind_pfl <<- act_pfl / (pos_played * 2)
        ind_srft <<- act_fta / pos_played
        
        #Rebounds
        
        avail_team_orb <<- team_orb * mins_pct 
        avail_team_drb <<- team_drb * mins_pct
        avail_opp_orb <<- opp_orb * mins_pct
        avail_opp_drb <<- opp_drb * mins_pct
        avail_team_trb <<- avail_team_orb + avail_team_drb
        avail_opp_trb <<- avail_opp_orb + avail_opp_drb
        
        ind_orb <<- act_orb / (avail_team_orb + avail_opp_drb)
        ind_drb <<- act_drb / (avail_team_drb + avail_opp_orb)
        ind_trb <<- act_trb / (avail_team_trb + avail_opp_trb)
        
        #Offensive Rating
        
        if (act_fta == 0) { fta_filler <<- 0 }
        else { fta_filler <<- act_ftm / act_fta }
        
        q_ast <<- ((act_mins / (whole_mins / 5)) * (1.14 * ((team_ast - act_ast) / team_fgm))) + 
          ((((team_ast / whole_mins) * act_mins * 5 - act_ast) / ((team_fgm / whole_mins) * act_mins * 5 - act_fgm)) * 
             (1 - (act_mins / (whole_mins / 5))))
        
        fg_part <<- act_fgm * (1 - 0.5 * ((act_pts - act_ftm) / (2 * act_fga)) * q_ast)
        if (act_fga == 0) { fg_part <<- 0 }
        ast_part <<- 0.5 * (((team_pts - team_ftm) - (act_pts - act_ftm)) / (2 * (team_fga - act_fga))) * act_ast
        ft_part <<- (1-(1-(fta_filler))^2) * 0.475 * act_fta
        team_scoring_pos <<- team_fgm + (1 - (1 - (team_ftm / team_fta))^2) * team_fta * 0.475
        team_play_pct <<- team_scoring_pos / (team_fga + team_fta * 0.475 + team_to)
        team_orb_weight <<- ((1 - team_orbpct) * team_play_pct) / ((1 - team_orbpct) * team_play_pct + team_orbpct * (1 - team_play_pct))
        team_orb_eq <<- team_orb / (team_orb + (opp_trb - opp_orb))
        orb_part <<- act_orb * team_orb_weight * team_play_pct
        
        score_poss <<- (fg_part + ast_part + ft_part) * (1 - (team_orb / team_scoring_pos) * (team_orb_weight * team_play_pct)) + orb_part
        
        fgxposs <<- (act_fga - act_fgm) * (1 - 1.07 * team_orb_eq)
        ftxposs <<- ((1 - (fta_filler)) ^ 2) * 0.475 * act_fga
        
        tot_poss <<- score_poss + fgxposs + ftxposs + act_tov
        pprod_fg_part <<- 2 * (act_fgm + 0.5 * act_pm3) * (1 - 0.5 * ((act_pts - act_ftm) / (2 * act_fga)) * q_ast)
        if (act_fga == 0) { pprod_fg_part <<- 0 }
        pprod_ast_part <<- 2 * ((team_fgm - act_fgm + 0.5 * (team_pm3 - act_pm3)) / (team_fgm - act_fgm)) * 0.5 * (((team_pts - team_ftm) - (act_pts - act_ftm)) / (2 * (team_fga - act_fga))) * act_ast
        pprod_orb_part <<- act_orb * team_orb_weight * team_play_pct * (team_pts / (team_fgm + (1 - (1 - (team_ftm / team_fta)) ^ 2) * 0.475 * team_fta))
        
        pprod <<- (pprod_fg_part + pprod_ast_part + act_ftm) * (1 - (team_orb / team_scoring_pos) * team_orb_weight * team_play_pct) + pprod_orb_part
        
        offensive_rating <<- 100 * (pprod / tot_poss)
        
        #Defensive Rating
        
        fmwt <<- (dfgpct * (1 - dorbpct)) / (dfgpct * (1 - dorbpct) + (1 - dfgpct) * dorbpct)
        stops1 <<- act_stl + act_blk * fmwt * (1 - 1.07 * dorbpct) + act_drb * (1 - fmwt)
        stops2 <<- (((opp_fga - opp_fgm - team_blk) / whole_mins) * fmwt * (1 - 1.07 * dorbpct) + ((opp_tov - team_stl) / whole_mins)) *
          act_mins + (act_pfl / team_pfl) * .475 * opp_fta * (1 - (opp_ftm / opp_fta))^2
        stops <<- stops1 + stops2
        stop_pct <<- (stops * whole_mins) / (team_pos * act_mins)
        
        team_def_rtg <<- 100 * (opp_pts / team_pos)
        d_pts_per_scposs <<- opp_pts / (opp_fgm + (1 - (1 - (opp_ftm / opp_fta))^2) * opp_fta * .475)
        
        defensive_rating <<- team_def_rtg + 0.2375 * (100 * d_pts_per_scposs * (1 - stop_pct) - team_def_rtg)
        
        usage_ <<- 100 * ((act_fga + 0.475 * act_fta + act_tov) * (team_mins)) / (act_mins * (team_fga + 0.475 * team_fta + team_to))
        
        ind_repla_mx[b, 1] <<- mins_pct
        ind_repla_mx[b, 2] <<- pos_played
        ind_repla_mx[b, 3] <<- ind_oe
        ind_repla_mx[b, 4] <<- ind_efg
        ind_repla_mx[b, 5] <<- ind_sr2
        ind_repla_mx[b, 6] <<- ind_sm2
        ind_repla_mx[b, 7] <<- ind_sr3
        ind_repla_mx[b, 8] <<- ind_sm3
        ind_repla_mx[b, 9] <<- ind_ftp
        ind_repla_mx[b, 10] <<- ind_ftr
        ind_repla_mx[b, 11] <<- ind_orb
        ind_repla_mx[b, 12] <<- ind_drb
        ind_repla_mx[b, 13] <<- ind_trb
        ind_repla_mx[b, 14] <<- ind_ast
        ind_repla_mx[b, 15] <<- ind_stl
        ind_repla_mx[b, 16] <<- ind_blk
        ind_repla_mx[b, 17] <<- ind_tov
        ind_repla_mx[b, 18] <<- ind_pfl
        ind_repla_mx[b, 19] <<- offensive_rating
        ind_repla_mx[b, 20] <<- defensive_rating
        ind_repla_mx[b, 21] <<- usage_
        ind_repla_mx[b, 22] <<- pprod
        ind_repla_mx[b, 23] <<- ind_srft
        
        if (pos_played <= 0) { ind_repla_mx[b, ] <<- 0 }
        
        which_nan_ortg <<- which(is.nan(ind_repla_mx[, 19]) == TRUE)
        which_nan_prod <<- which(is.nan(ind_repla_mx[, 22]) == TRUE)
        
        ind_repla_mx[which_nan_ortg, 19] <<- 0
        ind_repla_mx[which_nan_prod, 22] <<- 0
        
        ind_season_raw_join <<- data.frame(ind_info_mx, ind_repla_mx, stringsAsFactors = FALSE)
        if (a == 1) { ind_season_raw_list <<- list(ind_season_raw_join) }
        else { ind_season_raw_list[[a]] <<- ind_season_raw_join }
      
      }
    }
  }
} 

#Join IND GAME LOG LIST frames with OPP ADJ SEASON STATS

ind_log_to_opp <- function(xxxx) {
  
  master_adj_use <<- master_team_adj_frame_wt[, 1:36]
  
  a <- 1
  g <- length(ind_game_log_list)
  slotting_mx <<- matrix(0, nrow = g, ncol = 2)
  
  for (a in a:g) {
    
    game_log_use <<- ind_game_log_list[[a]]
    if (length(game_log_use) > 0) { 
      game_log_use <<- game_log_use[, c(1, 2, 3, 4, 5, 6, 9, 10:35)]
      
      act_teamid <<- game_log_use[1, 3]
      
      game_log_use_join <<- game_log_use %>%
        left_join(master_adj_use, by = c("OPP_ID" = "TEAM_ID", "OPP_NAME" = "TEAM_NAME"))
      
      info_mx <<- as.matrix(game_log_use_join[, 1:8])
      own_mx <<- as.matrix(game_log_use_join[, 9:33])
      opp_mx <<- as.matrix(game_log_use_join[, 34:67])
      rep_mx <<- own_mx
      
      nat_oe <<- nat_avgs$oe_do
      nat_efg <<- nat_avgs$efg_do
      nat_sr2 <<- nat_avgs$sr2p_do
      nat_sm2 <<- nat_avgs$mr2p_do
      nat_sr3 <<- nat_avgs$sr3p_do
      nat_sm3 <<- nat_avgs$mr3p_do
      nat_ftr <<- nat_avgs$ftr_do
      nat_orb <<- nat_avgs$orb_do
      nat_drb <<- nat_avgs$drb_do
      nat_trb <<- nat_avgs$trb_do
      nat_ast <<- nat_avgs$ast_do
      nat_stl <<- nat_avgs$stl_do
      nat_blk <<- nat_avgs$blk_do
      nat_tov <<- nat_avgs$tov_do
      nat_pfl <<- nat_avgs$foul_do
      nat_srft <<- nat_avgs$ft_per_pos
      
      b <- 1
      h <- nrow(own_mx)
      
      for (b in b:h) {
        
        act_loc <<- info_mx[b, 8]
        
        act_gmins <<- own_mx[b, 1]
        act_gpos <<- own_mx[b, 2]
        act_pmins <<- own_mx[b, 3]
        act_ppos <<- own_mx[b, 4]
        act_oe <<- own_mx[b, 5]
        act_efg <<- own_mx[b, 6]
        act_sr2 <<- own_mx[b, 7]
        act_sm2 <<- own_mx[b, 8]
        act_sr3 <<- own_mx[b, 9]
        act_sm3 <<- own_mx[b, 10]
        act_ftpct <<- own_mx[b, 11]
        act_ftr <<- own_mx[b, 12]
        act_orb <<- own_mx[b, 13]
        act_drb <<- own_mx[b, 14]
        act_trb <<- own_mx[b, 15]
        act_ast <<- own_mx[b, 16]
        act_stl <<- own_mx[b, 17]
        act_blk <<- own_mx[b, 18]
        act_tov <<- own_mx[b, 19]
        act_pfl <<- own_mx[b, 20]
        act_ortg <<- own_mx[b, 21]
        act_drtg <<- own_mx[b, 22]
        act_usg <<- own_mx[b, 23]
        act_prod <<- own_mx[b, 24]
        act_srft <<- own_mx[b, 25]
  
        opp_oe <<- opp_mx[b, 1] / nat_oe
        opp_efg <<- opp_mx[b, 2] / nat_efg
        opp_sr2 <<- opp_mx[b, 3] / nat_sr2
        opp_sm2 <<- opp_mx[b, 4] / nat_sm2
        opp_sr3 <<- opp_mx[b, 5] / nat_sr3
        opp_sm3 <<- opp_mx[b, 6] / nat_sm3
        opp_ftr <<- opp_mx[b, 7] / nat_ftr
        opp_orb <<- opp_mx[b, 8] / nat_orb
        opp_drb <<- opp_mx[b, 9] / nat_drb
        opp_trb <<- opp_mx[b, 10] / nat_trb
        opp_ast <<- opp_mx[b, 11] / nat_ast
        opp_stl <<- opp_mx[b, 12] / nat_stl
        opp_blk <<- opp_mx[b, 13] / nat_blk
        opp_tov <<- opp_mx[b, 14] / nat_tov
        opp_pfl <<- opp_mx[b, 15] / nat_pfl
        opp_de <<- opp_mx[b, 16] / nat_oe
        opp_defg <<- opp_mx[b, 17] / nat_efg
        opp_dsr2 <<- opp_mx[b, 18] / nat_sr2
        opp_dsm2 <<- opp_mx[b, 19] / nat_sm2
        opp_dsr3 <<- opp_mx[b, 20] / nat_sr3
        opp_dsm3 <<- opp_mx[b, 21] / nat_sm3
        opp_dftr <<- opp_mx[b, 22] / nat_ftr
        opp_dorb <<- opp_mx[b, 23] / nat_orb
        opp_ddrb <<- opp_mx[b, 24] / nat_drb
        opp_dtrb <<- opp_mx[b, 25] / nat_trb
        opp_dast <<- opp_mx[b, 26] / nat_ast
        opp_dstl <<- opp_mx[b, 27] / nat_stl
        opp_dblk <<- opp_mx[b, 28] / nat_blk
        opp_dto <<- opp_mx[b, 29] / nat_tov
        opp_dpfl <<- opp_mx[b, 30] / nat_pfl
        opp_srft <<- opp_mx[b, 33] / nat_srft
        opp_dsrft <<- opp_mx[b, 34] / nat_srft
        
        adj_oe <<- act_oe / opp_de
        adj_efg <<- act_efg / opp_defg
        adj_sr2 <<- act_sr2 / 1
        adj_sm2 <<- act_sm2 / opp_dsm2
        adj_sr3 <<- act_sr3 / 1
        adj_sm3 <<- act_sm3 / opp_dsm3
        adj_ftr <<- act_ftr / opp_dftr
        adj_orb <<- act_orb / opp_dorb
        adj_drb <<- act_drb / opp_ddrb
        adj_trb <<- act_trb / opp_dtrb
        adj_ast <<- act_ast / opp_dast
        adj_stl <<- act_stl / opp_dstl
        adj_blk <<- act_blk / opp_dblk
        adj_tov <<- act_tov / opp_dto
        adj_foul <<- act_pfl / opp_dpfl
        adj_ortg <<- act_ortg / opp_de
        adj_drtg <<- act_drtg / opp_oe
        adj_prod <<- act_prod / opp_de
        adj_srft <<- act_srft / opp_dsrft
        
        if (act_loc == "H") {
          
          adj_oe2 <<- adj_oe / home_oe 
          adj_efg2 <<- adj_efg / home_efg
          adj_sm22 <<- adj_sm2 / home_sm2
          adj_sm32 <<- adj_sm3 / home_sm3
          adj_ftr2 <<- adj_ftr / home_ftr
          adj_orb2 <<- adj_orb / home_orb
          adj_drb2 <<- adj_drb / home_drb
          adj_trb2 <<- adj_trb / home_trb
          adj_ast2 <<- adj_ast / home_ast
          adj_stl2 <<- adj_stl / home_stl
          adj_blk2 <<- adj_blk / home_blk
          adj_tov2 <<- adj_tov / home_to
          adj_foul2 <<- adj_foul / home_foul
          adj_ortg2 <<- adj_ortg / home_oe
          adj_drtg2 <<- adj_drtg * home_oe
          adj_prod2 <<- adj_prod / home_oe
          adj_srft2 <<- adj_srft / home_srft
          
        }
        
        if (act_loc == "A") {
          
          adj_oe2 <<- adj_oe / away_oe 
          adj_efg2 <<- adj_efg / away_efg
          adj_sm22 <<- adj_sm2 / away_sm2
          adj_sm32 <<- adj_sm3 / away_sm3
          adj_ftr2 <<- adj_ftr / away_ftr
          adj_orb2 <<- adj_orb / away_orb
          adj_drb2 <<- adj_drb / away_drb
          adj_trb2 <<- adj_trb / away_trb
          adj_ast2 <<- adj_ast / away_ast
          adj_stl2 <<- adj_stl / away_stl
          adj_blk2 <<- adj_blk / away_blk
          adj_tov2 <<- adj_tov / away_to
          adj_foul2 <<- adj_foul / away_foul
          adj_ortg2 <<- adj_ortg / away_oe
          adj_drtg2 <<- adj_drtg * away_oe
          adj_prod2 <<- adj_prod / away_oe
          adj_srft2 <<- adj_srft / away_srft
          
        }
        
        if (act_loc == "N") {
          
          adj_oe2 <<- adj_oe / neut_oe 
          adj_efg2 <<- adj_efg / neut_efg
          adj_sm22 <<- adj_sm2 / neut_sm2
          adj_sm32 <<- adj_sm3 / neut_sm3
          adj_ftr2 <<- adj_ftr / neut_ftr
          adj_orb2 <<- adj_orb / neut_orb
          adj_drb2 <<- adj_drb / neut_drb
          adj_trb2 <<- adj_trb / neut_trb
          adj_ast2 <<- adj_ast / neut_ast
          adj_stl2 <<- adj_stl / neut_stl
          adj_blk2 <<- adj_blk / neut_blk
          adj_tov2 <<- adj_tov / neut_to
          adj_foul2 <<- adj_foul / neut_foul
          adj_ortg2 <<- adj_ortg / neut_oe
          adj_drtg2 <<- adj_drtg * neut_oe
          adj_prod2 <<- adj_prod / neut_oe
          adj_srft2 <<- adj_srft / neut_srft
          
        }
        
        rep_mx[b, 5] <<- adj_oe2
        rep_mx[b, 6] <<- adj_efg2
        rep_mx[b, 7] <<- adj_sr2
        rep_mx[b, 8] <<- adj_sm22
        rep_mx[b, 9] <<- adj_sr3
        rep_mx[b, 10] <<- adj_sm32
        rep_mx[b, 12] <<- adj_ftr2
        rep_mx[b, 13] <<- adj_orb2
        rep_mx[b, 14] <<- adj_drb2
        rep_mx[b, 15] <<- adj_trb2
        rep_mx[b, 16] <<- adj_ast2
        rep_mx[b, 17] <<- adj_stl2
        rep_mx[b, 18] <<- adj_blk2
        rep_mx[b, 19] <<- adj_tov2
        rep_mx[b, 20] <<- adj_foul2
        rep_mx[b, 21] <<- adj_ortg2
        rep_mx[b, 22] <<- adj_drtg2
        rep_mx[b, 24] <<- adj_prod2
        rep_mx[b, 25] <<- adj_srft2
  
      }
      
      ind_adj_frame <<- data.frame(info_mx, rep_mx, stringsAsFactors = FALSE)
      ind_adj_frame[, 2] <<- as.numeric(ind_adj_frame[, 2])
      if (a == 1) { ind_gamelog_adj_list <<- list(ind_adj_frame) }
      else { ind_gamelog_adj_list[[a]] <<- ind_adj_frame }
      slotting_mx[a, 1] <<- act_teamid
      slotting_mx[a, 2] <<- a
    }
  }
}

ind_weighted_avg <- function(xxxx) {
  
  a <- 1
  g <- length(ind_gamelog_adj_list)
  
  for (a in a:g) {
    
    tmp_log_use <<- team_box_scorelist[[a]]
    
    if (length(tmp_log_use) > 0) {
    
      tmp_teamid <<- as.numeric(tmp_log_use[1, 21])
      
      slot_where <<- which(slotting_mx[, 1] == tmp_teamid)
      
      ind_adj_frame <<- ind_gamelog_adj_list[[slot_where]]
      
      players_checklist <<- ind_adj_frame %>%
        filter(pMIN > 0) %>%
        filter(PLAYER_ID > 0) %>%
        group_by(PLAYER_ID, TEAM_ID, TEAM_NAME) %>%
        summarize(ct = n())
      
      b <- 1
      h <- nrow(players_checklist)
      team_games <<- max(players_checklist$ct)
      
      for (b in b:h) {
      
        act_id <<- as.numeric(players_checklist[b, 1])
        
        
          raw_half <<- tmp_log_use %>%
            arrange(NO_ID) %>%
            filter(PLAYER_ID == act_id)
          
          adj_half <<- ind_adj_frame %>%
            arrange(NO_ID) %>%
            filter(PLAYER_ID == act_id) 
          
          portion_checklist_raw <<- raw_half[, c(3, 5, 7, 8, 9)]
          portion_checklist_adj <<- adj_half[, 12]
          loc_checklist <<- adj_half[, c(8)]
          portion_checklist <<- cbind(portion_checklist_raw, portion_checklist_adj)
          portion_checklist <<- as.matrix(portion_checklist)
          colnames(portion_checklist)[6] <<- "POS"
          checklist_sums <<- colSums(portion_checklist)
          
          fga_ratio <<- portion_checklist[, 1] / checklist_sums[1]
          pa2_ratio <<- portion_checklist[, 2] / checklist_sums[2]
          pa3_ratio <<- portion_checklist[, 3] / checklist_sums[3]
          fta_ratio <<- portion_checklist[, 5] / checklist_sums[5]
          pos_ratio <<- portion_checklist[, 6] / checklist_sums[6]
          
          if (checklist_sums[1] <= 0) { fga_ratio <<- 0 }
          if (checklist_sums[2] <= 0) { pa2_ratio <<- 0 }
          if (checklist_sums[3] <= 0) { pa3_ratio <<- 0 }
          if (checklist_sums[5] <= 0) { fta_ratio <<- 0 }
          if (checklist_sums[6] <= 0) { pos_ratio <<- 0 }
          
          adj_mx <<- as.matrix(adj_half[, 11:33])
          wt_rep_mx <<- matrix(0, nrow = 1, ncol = 26)
          
          #Calculate Weighting Curve, Assuming 4:1 full ratio
          
          ngames <<- nrow(portion_checklist)
          if (ngames > 20) { weight_max <<- 4 } else { weight_max <<- 1 + ((ngames - 1) * 0.157894737) }
          weight_min <<- 1
          weight_dist <<- (weight_max - weight_min) / (ngames - 1)
          if (ngames < 2) { weight_dist <<- 0 }
          
          weighting_curve <<- matrix(0, nrow = ngames, ncol = 1)
          c <- 1
          i <- nrow(weighting_curve)
          for (c in c:i) {
            
            weighting_curve[c] <<- weight_min + ((c - 1) * weight_dist)
            
          }
          
          weighting_sums <<- sum(weighting_curve)
          weighting_avgs <<- mean(weighting_curve)
          
          #Calculate Portions for Stats that Use It
          
          oe_portion <<- adj_mx[, 3] * pos_ratio
          efg_portion <<- adj_mx[, 4] * fga_ratio
          sr2_portion <<- adj_mx[, 5] * pos_ratio
          sm2_portion <<- adj_mx[, 6] * pa2_ratio
          sr3_portion <<- adj_mx[, 7] * pos_ratio
          sm3_portion <<- adj_mx[, 8] * pa3_ratio
          ftr_portion <<- adj_mx[, 10] * fga_ratio
          orb_portion <<- adj_mx[, 11] * pos_ratio
          drb_portion <<- adj_mx[, 12] * pos_ratio
          trb_portion <<- adj_mx[, 13] * pos_ratio
          ast_portion <<- adj_mx[, 14] * pos_ratio
          stl_portion <<- adj_mx[, 15] * pos_ratio
          blk_portion <<- adj_mx[, 16] * pos_ratio
          tov_portion <<- adj_mx[, 17] * pos_ratio
          pfl_portion <<- adj_mx[, 18] * pos_ratio
          ortg_portion <<- adj_mx[, 19] * pos_ratio
          drtg_portion <<- adj_mx[, 20] * pos_ratio
          usg_portion <<- adj_mx[, 21] * pos_ratio
          srft_portion <<- adj_mx[, 23] * pos_ratio
          
          #Portion X Weight
          
          oe_pxw <<- oe_portion * weighting_curve
          efg_pxw <<- efg_portion * weighting_curve
          sr2_pxw <<- sr2_portion * weighting_curve
          sm2_pxw <<- sm2_portion * weighting_curve
          sr3_pxw <<- sr3_portion * weighting_curve
          sm3_pxw <<- sm3_portion * weighting_curve
          ftr_pxw <<- ftr_portion * weighting_curve
          orb_pxw <<- orb_portion * weighting_curve
          drb_pxw <<- drb_portion * weighting_curve
          trb_pxw <<- trb_portion * weighting_curve
          ast_pxw <<- ast_portion * weighting_curve
          stl_pxw <<- stl_portion * weighting_curve
          blk_pxw <<- blk_portion * weighting_curve
          tov_pxw <<- tov_portion * weighting_curve
          pfl_pxw <<- pfl_portion * weighting_curve
          ortg_pxw <<- ortg_portion * weighting_curve
          drtg_pxw <<- drtg_portion * weighting_curve
          usg_pxw <<- usg_portion * weighting_curve
          srft_pxw <<- srft_portion * weighting_curve
          
          #PXW Sums
          
          oe_pxw_sum <<- sum(oe_pxw)
          efg_pxw_sum <<- sum(efg_pxw)
          sr2_pxw_sum <<- sum(sr2_pxw)
          sm2_pxw_sum <<- sum(sm2_pxw)
          sr3_pxw_sum <<- sum(sr3_pxw)
          sm3_pxw_sum <<- sum(sm3_pxw)
          ftr_pxw_sum <<- sum(ftr_pxw)
          orb_pxw_sum <<- sum(orb_pxw)
          drb_pxw_sum <<- sum(drb_pxw)
          trb_pxw_sum <<- sum(trb_pxw)
          ast_pxw_sum <<- sum(ast_pxw)
          stl_pxw_sum <<- sum(stl_pxw)
          blk_pxw_sum <<- sum(blk_pxw)
          tov_pxw_sum <<- sum(tov_pxw)
          pfl_pxw_sum <<- sum(pfl_pxw)
          ortg_pxw_sum <<- sum(ortg_pxw)
          drtg_pxw_sum <<- sum(drtg_pxw)
          usg_pxw_sum <<- sum(usg_pxw)
          srft_pxw_sum <<- sum(srft_pxw)
          
          #Apply Weights
          
          mins_weighted <<- sum((adj_mx[, 1] * weighting_curve)) / weighting_sums
          pos_weighted <<- sum((adj_mx[, 2] * weighting_curve)) / weighting_sums
          oe_weighted <<- oe_pxw_sum / weighting_avgs
          efg_weighted <<- efg_pxw_sum / weighting_avgs
          sr2_weighted <<- sr2_pxw_sum / weighting_avgs
          sm2_weighted <<- sm2_pxw_sum / weighting_avgs
          sr3_weighted <<- sr3_pxw_sum / weighting_avgs
          sm3_weighted <<- sm3_pxw_sum / weighting_avgs
          ftr_weighted <<- ftr_pxw_sum / weighting_avgs
          orb_weighted <<- orb_pxw_sum / weighting_avgs
          drb_weighted <<- drb_pxw_sum / weighting_avgs
          trb_weighted <<- trb_pxw_sum / weighting_avgs
          ast_weighted <<- ast_pxw_sum / weighting_avgs
          stl_weighted <<- stl_pxw_sum / weighting_avgs
          blk_weighted <<- blk_pxw_sum / weighting_avgs
          tov_weighted <<- tov_pxw_sum / weighting_avgs
          pfl_weighted <<- pfl_pxw_sum / weighting_avgs
          ortg_weighted <<- ortg_pxw_sum / weighting_avgs
          drtg_weighted <<- drtg_pxw_sum / weighting_avgs
          usg_weighted <<- usg_pxw_sum / weighting_avgs
          srft_weighted <<- srft_pxw_sum / weighting_avgs
          prod_weighted <<- sum((adj_mx[, 22] * weighting_curve)) / weighting_sums
          ftp_weighted <<- sum(portion_checklist[, 4] / sum(portion_checklist[, 5]))
          if (sum(portion_checklist[, 5]) == 0) { ftp_weighted <<- nat_ftpct }
          mins_game <<- (mins_weighted * ngames) / team_games
          pos_game <<- (pos_weighted * ngames) / team_games
          
          #Insert Weights
          
          wt_rep_mx[1:26] <<- c(mins_weighted, pos_weighted, oe_weighted, efg_weighted, sr2_weighted, sm2_weighted, 
                                sr3_weighted, sm3_weighted, ftp_weighted, ftr_weighted, orb_weighted, drb_weighted, trb_weighted, 
                                ast_weighted, stl_weighted, blk_weighted, tov_weighted, pfl_weighted, ortg_weighted, 
                                drtg_weighted, usg_weighted, prod_weighted, team_games, mins_game, pos_game, srft_weighted)
          
          colnames(wt_rep_mx) <<- c("pMIN", "pPOS", "pOE", "peFG", "pSR2", "pSM2", "pSR3", "pSM3", "pFTPCT", "pFTR", "pORB",
                                          "pDRB", "pTRB", "pAST", "pSTL",  
                                          "pBLK", "pTO", "pFOUL", "pORTG", "pDRTG", "pUSG", "pPROD", "TM_GAMES", 
                                          "MINS/TGM", "POS/TGM", "pSRFT")
          
          if (b == 1) { wt_rep_stack <<- wt_rep_mx }
          else { wt_rep_stack <<- rbind(wt_rep_stack, wt_rep_mx) }
        
      }
        
      wt_rep_comb <<- data.frame(players_checklist, wt_rep_stack)
      if (a == 1) { ind_all_adj_frame <<- wt_rep_comb }
      else { ind_all_adj_frame <<- rbind(ind_all_adj_frame, wt_rep_comb) }
    }
  }  
}

free_throw_fixer_ind <- function(xxxx) {
  
  a <- 1
  g <- length(ind_gamelog_adj_list)
  
  h_ft <<- advantage_frame[1, 10]
  a_ft <<- advantage_frame[2, 10]
  n_ft <<- advantage_frame[3, 10]
  
  slotter_mx <<- as.matrix(ind_all_adj_frame[ ,1])
  slotter_rep <<- matrix(nat_ftpct, nrow = nrow(slotter_mx), ncol = 1)
  
  for (a in a:g) {
    
    tmp_log_use <<- team_box_scorelist[[a]]
    if (length(tmp_log_use) > 0) {
      tmp_teamid <<- as.numeric(tmp_log_use[1, 21])
      
      ind_ft_frame <<- tmp_log_use[, c(8, 9, 19, 20, 21, 28)]
      
      ft_checklist <<- ind_ft_frame %>%
        filter(is.na(PLAYER_ID) == FALSE) %>%
        group_by(PLAYER_ID, TEAM_ID) %>%
        summarize(ct = n())
      
      b <- 1
      h <- nrow(ft_checklist)
      
      for (b in b:h) {
      
        act_ft_id <<- as.numeric(ft_checklist[b, 1])
        
        ind_ft_frame_ind <<- ind_ft_frame %>%
          filter(PLAYER_ID == act_ft_id)
        
        ft_num_mx <<- as.matrix(ind_ft_frame_ind[, 1:2])
        ft_loc_mx <<- as.matrix(ind_ft_frame_ind[, 6])
        ft_rep_mx <<- ft_num_mx
        
        c <- 1
        i <- nrow(ft_num_mx)
        
        for (c in c:i) {
        
          act_ftm <<- ft_num_mx[c, 1]
          act_fta <<- ft_num_mx[c, 2]
          act_loc <<- ft_loc_mx[c]
          
          if (act_loc == "H") {
            
            ft_adj <<- act_ftm + (act_fta * (1 - h_ft))
            new_ftm <<- ft_adj
            
          }
          
          if (act_loc == "A") {
            
            ft_adj <<- act_ftm + (act_fta * (1 - a_ft))
            new_ftm <<- ft_adj
            
          }
          
          if (act_loc == "N") {
            
            ft_adj <<- act_ftm + (act_fta * (1 - n_ft))
            new_ftm <<- ft_adj
            
          }
          
          ft_rep_mx[c, 1] <<- new_ftm
        }
        
        sum_ftm <<- sum(ft_rep_mx[, 1])
        sum_fta <<- sum(ft_rep_mx[, 2])
        ft_pct_frame <<- ft_rep_mx[, 1] / ft_rep_mx[ ,2]
        ft_pct_which <<- which(is.nan(ft_pct_frame) == TRUE)
        ft_pct_frame[ft_pct_which] <<- 0
        
        fta_ratio <<- ft_rep_mx[, 2] / sum_fta
        
        ngames <<- nrow(ft_rep_mx)
        if (ngames > 20) { weight_max <<- 4 } else { weight_max <<- 1 + ((ngames - 1) * 0.157894737) }
        weight_min <<- 1
        weight_dist <<- (weight_max - weight_min) / (ngames - 1)
        if (ngames < 2) { weight_dist <<- 0 }
        
        weighting_curve <<- matrix(0, nrow = ngames, ncol = 1)
        d <- 1
        j <- nrow(weighting_curve)
        for (d in d:j) {
          
          weighting_curve[d] <<- weight_min + ((d - 1) * weight_dist)
          
        }
        
        weighting_avgs <<- mean(weighting_curve)
  
        ftm_portion <<- ft_rep_mx[, 1] * fta_ratio
        fta_portion <<- ft_rep_mx[, 2] * fta_ratio
        
        ftm_pxw <<- ftm_portion * weighting_curve
        fta_pxw <<- fta_portion * weighting_curve
        
        ftm_pxw_sum <<- sum(ftm_pxw)
        fta_pxw_sum <<- sum(fta_pxw)
        
        ftm_weighted <<- ftm_pxw_sum / weighting_avgs
        fta_weighted <<- fta_pxw_sum / weighting_avgs
        
        if (sum_fta == 0) { adj_ftpct <<- nat_ftpct }
        else { adj_ftpct <<- ftm_weighted / fta_weighted }
        
        slotter <<- which(slotter_mx == act_ft_id)
        slotter_rep[slotter] <<- adj_ftpct
        
      } 
    }
  }
  ind_all_adj_frame[ ,13] <<- slotter_rep
}
  

  

#To Do List:

#1. Weighted Team Averages


team_weighted_avg <- function(xxxx) {
  
  a <- 1
  g <- nrow(master_team_adj_frame_uw)

  master_team_adj_frame_wt <<- master_team_adj_frame_uw
  
  for (a in a:g) {
    
    act_teamid <<- as.numeric(master_team_adj_frame_uw[a, 1])
    
    adj_game_by_game_frame <<- master_game_adj_frame_uw %>%
      filter(TEAM_ID == act_teamid)
    
    raw_game_by_game_frame <<- team_game_by_game_raw %>%
      filter(TEAM_ID == act_teamid)
    
    adj_gxg_mx <<- as.matrix(adj_game_by_game_frame[, 3:39])
    raw_gxg_mx <<- as.matrix(raw_game_by_game_frame[, c(9, 12, 14, 16)])
    pos_gxg_mx <<- (adj_gxg_mx[, 35] / 40) * (raw_gxg_mx[, 1] / 5)
    fga_gxg_mx <<- raw_gxg_mx[, 2]
    pa2_gxg_mx <<- raw_gxg_mx[, 3]
    pa3_gxg_mx <<- raw_gxg_mx[, 4]
    fta_gxg_mx <<- adj_gxg_mx[, 37]
    pos_sums <<- sum(pos_gxg_mx)
    fga_sums <<- sum(fga_gxg_mx)
    pa2_sums <<- sum(pa2_gxg_mx)
    pa3_sums <<- sum(pa3_gxg_mx)
    fta_sums <<- sum(fta_gxg_mx)
    
    fga_ratio <<- fga_gxg_mx / fga_sums
    pos_ratio <<- pos_gxg_mx / pos_sums
    pa2_ratio <<- pa2_gxg_mx / pa2_sums
    pa3_ratio <<- pa3_gxg_mx / pa3_sums
    fta_ratio <<- fta_gxg_mx / fta_sums
    
    ngames <<- nrow(raw_gxg_mx)
    if (ngames > 20) { weight_max <<- 4 } else { weight_max <<- 1 + ((ngames - 1) * 0.157894737) }
    weight_min <<- 1
    weight_dist <<- (weight_max - weight_min) / (ngames - 1)
    if (ngames < 2) { weight_dist <<- 0 }
    
    weighting_curve <<- matrix(0, nrow = ngames, ncol = 1)
    c <- 1
    i <- nrow(weighting_curve)
    for (c in c:i) {
      
      weighting_curve[c] <<- weight_min + ((c - 1) * weight_dist)
      
    }
    
    weighting_sums <<- sum(weighting_curve)
    weighting_avgs <<- mean(weighting_curve)
    
    #Calculate Portions for Stats that Use It
    
    oe_portion <<- adj_gxg_mx[, 1] * pos_ratio
    efg_portion <<- adj_gxg_mx[, 2] * fga_ratio
    sr2_portion <<- adj_gxg_mx[, 3] * pos_ratio
    sm2_portion <<- adj_gxg_mx[, 4] * pa2_ratio
    sr3_portion <<- adj_gxg_mx[, 5] * pos_ratio
    sm3_portion <<- adj_gxg_mx[, 6] * pa3_ratio
    ftr_portion <<- adj_gxg_mx[, 7] * fga_ratio
    orb_portion <<- adj_gxg_mx[, 8] * pos_ratio
    drb_portion <<- adj_gxg_mx[, 9] * pos_ratio
    trb_portion <<- adj_gxg_mx[, 10] * pos_ratio
    ast_portion <<- adj_gxg_mx[, 11] * pos_ratio
    stl_portion <<- adj_gxg_mx[, 12] * pos_ratio
    blk_portion <<- adj_gxg_mx[, 13] * pos_ratio
    tov_portion <<- adj_gxg_mx[, 14] * pos_ratio
    pfl_portion <<- adj_gxg_mx[, 15] * pos_ratio
    
    de_portion <<- adj_gxg_mx[, 16] * pos_ratio
    defg_portion <<- adj_gxg_mx[, 17] * fga_ratio
    dsr2_portion <<- adj_gxg_mx[, 18] * pos_ratio
    dsm2_portion <<- adj_gxg_mx[, 19] * pa2_ratio
    dsr3_portion <<- adj_gxg_mx[, 20] * pos_ratio
    dsm3_portion <<- adj_gxg_mx[, 21] * pa3_ratio
    dftr_portion <<- adj_gxg_mx[, 22] * fga_ratio
    dorb_portion <<- adj_gxg_mx[, 23] * pos_ratio
    ddrb_portion <<- adj_gxg_mx[, 24] * pos_ratio
    dtrb_portion <<- adj_gxg_mx[, 25] * pos_ratio
    dast_portion <<- adj_gxg_mx[, 26] * pos_ratio
    dstl_portion <<- adj_gxg_mx[, 27] * pos_ratio
    dblk_portion <<- adj_gxg_mx[, 28] * pos_ratio
    dtov_portion <<- adj_gxg_mx[, 29] * pos_ratio
    dpfl_portion <<- adj_gxg_mx[, 30] * pos_ratio
    
    ft2foul_portion <<- adj_gxg_mx[, 31] * pos_ratio
    dft2foul_portion <<- adj_gxg_mx[, 32] * pos_ratio
    
    srft_portion <<- adj_gxg_mx[, 33] * pos_ratio
    dsrft_portion <<- adj_gxg_mx[, 34] * pos_ratio
    
    ftm_portion <<- adj_gxg_mx[, 36] * fta_ratio
    fta_portion <<- adj_gxg_mx[, 37] * fta_ratio

    #Portion X Weight
    
    oe_pxw <<- oe_portion * weighting_curve
    efg_pxw <<- efg_portion * weighting_curve
    sr2_pxw <<- sr2_portion * weighting_curve
    sm2_pxw <<- sm2_portion * weighting_curve
    sr3_pxw <<- sr3_portion * weighting_curve
    sm3_pxw <<- sm3_portion * weighting_curve
    ftr_pxw <<- ftr_portion * weighting_curve
    orb_pxw <<- orb_portion * weighting_curve
    drb_pxw <<- drb_portion * weighting_curve
    trb_pxw <<- trb_portion * weighting_curve
    ast_pxw <<- ast_portion * weighting_curve
    stl_pxw <<- stl_portion * weighting_curve
    blk_pxw <<- blk_portion * weighting_curve
    tov_pxw <<- tov_portion * weighting_curve
    pfl_pxw <<- pfl_portion * weighting_curve
    
    de_pxw <<- de_portion * weighting_curve
    defg_pxw <<- defg_portion * weighting_curve
    dsr2_pxw <<- dsr2_portion * weighting_curve
    dsm2_pxw <<- dsm2_portion * weighting_curve
    dsr3_pxw <<- dsr3_portion * weighting_curve
    dsm3_pxw <<- dsm3_portion * weighting_curve
    dftr_pxw <<- dftr_portion * weighting_curve
    dorb_pxw <<- dorb_portion * weighting_curve
    ddrb_pxw <<- ddrb_portion * weighting_curve
    dtrb_pxw <<- dtrb_portion * weighting_curve
    dast_pxw <<- dast_portion * weighting_curve
    dstl_pxw <<- dstl_portion * weighting_curve
    dblk_pxw <<- dblk_portion * weighting_curve
    dtov_pxw <<- dtov_portion * weighting_curve
    dpfl_pxw <<- dpfl_portion * weighting_curve
    
    ft2foul_pxw <<- ft2foul_portion * weighting_curve
    dft2foul_pxw <<- dft2foul_portion * weighting_curve
    srft_pxw <<- srft_portion * weighting_curve
    dsrft_pxw <<- dsrft_portion * weighting_curve
    
    ftm_pxw <<- ftm_portion * weighting_curve
    fta_pxw <<- fta_portion * weighting_curve
    
    #PXW Sums
    
    oe_pxw_sum <<- sum(oe_pxw)
    efg_pxw_sum <<- sum(efg_pxw)
    sr2_pxw_sum <<- sum(sr2_pxw)
    sm2_pxw_sum <<- sum(sm2_pxw)
    sr3_pxw_sum <<- sum(sr3_pxw)
    sm3_pxw_sum <<- sum(sm3_pxw)
    ftr_pxw_sum <<- sum(ftr_pxw)
    orb_pxw_sum <<- sum(orb_pxw)
    drb_pxw_sum <<- sum(drb_pxw)
    trb_pxw_sum <<- sum(trb_pxw)
    ast_pxw_sum <<- sum(ast_pxw)
    stl_pxw_sum <<- sum(stl_pxw)
    blk_pxw_sum <<- sum(blk_pxw)
    tov_pxw_sum <<- sum(tov_pxw)
    pfl_pxw_sum <<- sum(pfl_pxw)
    
    de_pxw_sum <<- sum(de_pxw)
    defg_pxw_sum <<- sum(defg_pxw)
    dsr2_pxw_sum <<- sum(dsr2_pxw)
    dsm2_pxw_sum <<- sum(dsm2_pxw)
    dsr3_pxw_sum <<- sum(dsr3_pxw)
    dsm3_pxw_sum <<- sum(dsm3_pxw)
    dftr_pxw_sum <<- sum(dftr_pxw)
    dorb_pxw_sum <<- sum(dorb_pxw)
    ddrb_pxw_sum <<- sum(ddrb_pxw)
    dtrb_pxw_sum <<- sum(dtrb_pxw)
    dast_pxw_sum <<- sum(dast_pxw)
    dstl_pxw_sum <<- sum(dstl_pxw)
    dblk_pxw_sum <<- sum(dblk_pxw)
    dtov_pxw_sum <<- sum(dtov_pxw)
    dpfl_pxw_sum <<- sum(dpfl_pxw)
    
    ft2foul_pxw_sum <<- sum(ft2foul_pxw)
    dft2foul_pxw_sum <<- sum(dft2foul_pxw)
    srft_pxw_sum <<- sum(srft_pxw)
    dsrft_pxw_sum <<- sum(dsrft_pxw)
    
    fta_pxw_sum <<- sum(fta_pxw)
    ftm_pxw_sum <<- sum(ftm_pxw)
    
    #Apply Weights
    
    oe_weighted <<- oe_pxw_sum / weighting_avgs
    efg_weighted <<- efg_pxw_sum / weighting_avgs
    sr2_weighted <<- sr2_pxw_sum / weighting_avgs
    sm2_weighted <<- sm2_pxw_sum / weighting_avgs
    sr3_weighted <<- sr3_pxw_sum / weighting_avgs
    sm3_weighted <<- sm3_pxw_sum / weighting_avgs
    ftr_weighted <<- ftr_pxw_sum / weighting_avgs
    orb_weighted <<- orb_pxw_sum / weighting_avgs
    drb_weighted <<- drb_pxw_sum / weighting_avgs
    trb_weighted <<- trb_pxw_sum / weighting_avgs
    ast_weighted <<- ast_pxw_sum / weighting_avgs
    stl_weighted <<- stl_pxw_sum / weighting_avgs
    blk_weighted <<- blk_pxw_sum / weighting_avgs
    tov_weighted <<- tov_pxw_sum / weighting_avgs
    pfl_weighted <<- pfl_pxw_sum / weighting_avgs
    
    de_weighted <<- de_pxw_sum / weighting_avgs
    defg_weighted <<- defg_pxw_sum / weighting_avgs
    dsr2_weighted <<- dsr2_pxw_sum / weighting_avgs
    dsm2_weighted <<- dsm2_pxw_sum / weighting_avgs
    dsr3_weighted <<- dsr3_pxw_sum / weighting_avgs
    dsm3_weighted <<- dsm3_pxw_sum / weighting_avgs
    dftr_weighted <<- dftr_pxw_sum / weighting_avgs
    dorb_weighted <<- dorb_pxw_sum / weighting_avgs
    ddrb_weighted <<- ddrb_pxw_sum / weighting_avgs
    dtrb_weighted <<- dtrb_pxw_sum / weighting_avgs
    dast_weighted <<- dast_pxw_sum / weighting_avgs
    dstl_weighted <<- dstl_pxw_sum / weighting_avgs
    dblk_weighted <<- dblk_pxw_sum / weighting_avgs
    dtov_weighted <<- dtov_pxw_sum / weighting_avgs
    dpfl_weighted <<- dpfl_pxw_sum / weighting_avgs
    
    ft2foul_weighted <<- ft2foul_pxw_sum / weighting_avgs
    dft2foul_weighted <<- dft2foul_pxw_sum / weighting_avgs
    srft_weighted <<- srft_pxw_sum / weighting_avgs
    dsrft_weighted <<- dsrft_pxw_sum / weighting_avgs
    
    ftm_weighted <<- ftm_pxw_sum / weighting_avgs
    fta_weighted <<- fta_pxw_sum / weighting_avgs
    
    tempo_weighted <<- sum((adj_gxg_mx[, 35] * weighting_curve)) / weighting_sums
    
    #Insert New Weighted Values
    
    master_team_adj_frame_wt[a, 3] <<- oe_weighted
    master_team_adj_frame_wt[a, 4] <<- efg_weighted
    master_team_adj_frame_wt[a, 5] <<- sr2_weighted
    master_team_adj_frame_wt[a, 6] <<- sm2_weighted
    master_team_adj_frame_wt[a, 7] <<- sr3_weighted
    master_team_adj_frame_wt[a, 8] <<- sm3_weighted
    master_team_adj_frame_wt[a, 9] <<- ftr_weighted
    master_team_adj_frame_wt[a, 10] <<- orb_weighted
    master_team_adj_frame_wt[a, 11] <<- drb_weighted
    master_team_adj_frame_wt[a, 12] <<- trb_weighted
    master_team_adj_frame_wt[a, 13] <<- ast_weighted
    master_team_adj_frame_wt[a, 14] <<- stl_weighted
    master_team_adj_frame_wt[a, 15] <<- blk_weighted
    master_team_adj_frame_wt[a, 16] <<- tov_weighted
    master_team_adj_frame_wt[a, 17] <<- pfl_weighted
    
    master_team_adj_frame_wt[a, 18] <<- de_weighted
    master_team_adj_frame_wt[a, 19] <<- defg_weighted
    master_team_adj_frame_wt[a, 20] <<- dsr2_weighted
    master_team_adj_frame_wt[a, 21] <<- dsm2_weighted
    master_team_adj_frame_wt[a, 22] <<- dsr3_weighted
    master_team_adj_frame_wt[a, 23] <<- dsm3_weighted
    master_team_adj_frame_wt[a, 24] <<- dftr_weighted
    master_team_adj_frame_wt[a, 25] <<- dorb_weighted
    master_team_adj_frame_wt[a, 26] <<- ddrb_weighted
    master_team_adj_frame_wt[a, 27] <<- dtrb_weighted
    master_team_adj_frame_wt[a, 28] <<- dast_weighted
    master_team_adj_frame_wt[a, 29] <<- dstl_weighted
    master_team_adj_frame_wt[a, 30] <<- dblk_weighted
    master_team_adj_frame_wt[a, 31] <<- dtov_weighted
    master_team_adj_frame_wt[a, 32] <<- dpfl_weighted
    
    master_team_adj_frame_wt[a, 33] <<- ft2foul_weighted
    master_team_adj_frame_wt[a, 34] <<- dft2foul_weighted
    master_team_adj_frame_wt[a, 35] <<- srft_weighted
    master_team_adj_frame_wt[a, 36] <<- dsrft_weighted
    
    master_team_adj_frame_wt[a, 37] <<- tempo_weighted
    
    master_team_adj_frame_wt[a, 38] <<- ftm_weighted / fta_weighted

  }
  a <- 3
  g <- 38
  for (a in a:g) {
    
    nan_search <<- which(is.nan(master_team_adj_frame_wt[, a]) == TRUE)
    if (length(nan_search) > 0) {
      if (a == 8 | a == 23) { repper <<- nat_avgs$mr3p_do }
      if (a == 9 | a == 24) { repper <<- nat_avgs$ftr_do }
      if (a == 10 | a == 25) { repper <<- nat_avgs$orb_do }
      if (a == 13 | a == 28) { repper <<- nat_avgs$ast_do }
      if (a == 14 | a == 29) { repper <<- nat_avgs$stl_do }
      if (a == 15 | a == 30) { repper <<- nat_avgs$blk_do }
      if (a == 16 | a == 31) { repper <<- nat_avgs$tov_do }
      if (a == 17 | a == 32) { repper <<- nat_avgs$foul_do }
      master_team_adj_frame_wt[nan_search, a] <<- repper
      
    }
  }
}

#-------------------------
#Master Schedule Create
#-------------------------

multi_schedule <- function(xxxx) {
  
  multi_boxdate <<- read_csv("master_boxdates.csv")
  a <- 1
  g <- nrow(multi_boxdate)
  
  for (a in a:g) {
    
    mm <- as.character(multi_boxdate[a, 1])
    dd <- as.character(multi_boxdate[a, 2])
    yy <- as.character(multi_boxdate[a, 3])
    ss <- as.character(multi_boxdate[a, 4])
    
    if (nchar(mm) == 1) { mm <- paste("0", mm, sep = "") }
    if (nchar(dd) == 1) { dd <- paste("0", dd, sep = "") }
    
    fn <- paste("boxscores\\box_scores_", mm, "-", dd, "-", yy, ".csv", sep = "")
    print(fn)
    if (file.exists(fn) == TRUE) { 
      tmp_box <- read_csv(fn)
      
      box_group <- tmp_box %>%
        group_by(TEAM_ID, TEAM_NAME, OPP_ID, OPP_NAME, DATE, GAME_ID, NO_ID, LOC) %>%
        summarize(PF = sum(PTS))
      
      id_mx <- as.matrix(box_group[, c(1, 3, 6)])
      
      b <- 1
      h <- nrow(id_mx)
      
      for (b in b:h) {
        
        opp_id <- as.numeric(id_mx[b, 2])
        gm_id <- as.numeric(id_mx[b, 3])
        opp_wh <- which(id_mx[, 1] == opp_id & id_mx[, 3] == gm_id)
        opp_pf <- as.numeric(box_group[opp_wh, 9])
        box_group[b, 10] <- opp_pf
        
      }
      box_group[, 11] <- ss
      colnames(box_group)[10:11] <- c("PA", "Season")
      if (a == 1) { box_group_all <<- box_group }
      else { box_group_all <<- rbind(box_group_all, box_group) }
    }
  }
}

atm_db_setup <- function(xxxx) {
  
  stats_2017 <- read_csv("team_2017_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, ED)
  stats_2018 <- read_csv("team_2018_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, ED)
  stats_2019 <- read_csv("team_2019_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, ED)
  stats_2020 <- read_csv("team_2020_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, ED)
  
  multi_filt_2017 <- box_group_all %>%
    filter(Season == 2017) %>%
    left_join(stats_2017, by = c("TEAM_ID")) %>%
    left_join(stats_2017, by = c("OPP_ID" = "TEAM_ID"))
  
  multi_filt_2018 <- box_group_all %>%
    filter(Season == 2018) %>%
    left_join(stats_2018, by = c("TEAM_NAME")) %>%
    left_join(stats_2018, by = c("OPP_NAME" = "TEAM_NAME"))
  
  multi_filt_2019 <- box_group_all %>%
    filter(Season == 2019) %>%
    left_join(stats_2019, by = c("TEAM_NAME")) %>%
    left_join(stats_2019, by = c("OPP_NAME" = "TEAM_NAME"))
  
  multi_filt_2020 <- box_group_all %>%
    filter(Season == 2020) %>%
    left_join(stats_2020, by = c("TEAM_ID")) %>%
    left_join(stats_2020, by = c("OPP_ID" = "TEAM_ID"))
  
  master_reg_db <<- rbind(multi_filt_2017, multi_filt_2018, multi_filt_2019, multi_filt_2020) %>%
    mutate(Margin = PF - PA, Total = PF + PA)
  
  master_reg_db_w <<- which(is.na(master_reg_db$OE.x) == TRUE)
  master_reg_db_w <<- master_reg_db_w * -1
  master_reg_db <<- master_reg_db[master_reg_db_w, ]
  
  master_reg_db_w <<- which(is.na(master_reg_db$OE.y) == TRUE)
  master_reg_db_w <<- master_reg_db_w * -1
  master_reg_db <<- master_reg_db[master_reg_db_w, ]
  
  master_reg_db <<- master_reg_db %>%
    mutate(Win = ifelse(PF > PA, 1, 0))
  
  master_reg_ah <<- master_reg_db %>%
    filter(LOC == "A")
  
  master_reg_n <<- master_reg_db %>%
    filter(LOC == "N")
  
  master_reg_atm <<- master_reg_db[, c(5, 8, 2, 4, 9, 10, 66:67, 13:37, 39:63)]
  
  master_reg_atm_amx <<- master_reg_atm %>%
    filter(LOC == "A")
  
  master_reg_atm_adx <<- as.matrix(master_reg_atm_amx[, 1:4])
  master_reg_atm_amx <<- as.matrix(master_reg_atm_amx[, 5:58])
  
  master_reg_atm_ndx <<- as.matrix(master_reg_atm[, 1:4])
  master_reg_atm_nmx <<- as.matrix(master_reg_atm[, 5:58])
  
  master_reg_atm_amx <<- master_reg_atm_amx[, c(-7, -23, -24, -25, -26, -27, -28, -29, -32, -46, -47, -48, -49, -50, -51, -52, -53, -54)]
  master_reg_atm_nmx <<- master_reg_atm_nmx[, c(-7, -23, -24, -25, -26, -27, -28, -29, -32, -46, -47, -48, -49, -50, -51, -52, -53, -54)]
  
  a <- 1
  g <- ncol(master_reg_atm_nmx)
  
  for (a in a:g) {
    
    s <- sd(master_reg_atm_nmx[, a])
    if (a == 1) { master_reg_sd <<- s }
    else { master_reg_sd <<- c(master_reg_sd, s) }
    
  }
}

multi_team_joins <- function(xxxx) {
  
  stats_2017 <- read_csv("team_2017_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, dSR2, dSR3, dSM2, dSM3, dAST, dSTL, dBLK, dFOUL, ED)
  stats_2018 <- read_csv("team_2018_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, dSR2, dSR3, dSM2, dSM3, dAST, dSTL, dBLK, dFOUL, ED)
  stats_2019 <- read_csv("team_2019_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, dSR2, dSR3, dSM2, dSM3, dAST, dSTL, dBLK, dFOUL, ED)
  stats_2020 <- read_csv("team_2020_stats.csv") %>%
    select(TEAM_ID, TEAM_NAME, OE, eFG, FTR, ORB, DRB, TRB, TO, SRFT, DE, deFG, dFTR, dORB, dDRB, dTO, dSRFT, Tempo, SR2, SR3, SM2, SM3, AST, STL, BLK, FOUL, dSR2, dSR3, dSM2, dSM3, dAST, dSTL, dBLK, dFOUL, ED)
  
  multi_filt_2017 <- box_group_all %>%
    filter(Season == 2017) %>%
    left_join(stats_2017, by = c("TEAM_ID")) %>%
    left_join(stats_2017, by = c("OPP_ID" = "TEAM_ID"))
  
  multi_filt_2018 <- box_group_all %>%
    filter(Season == 2018) %>%
    left_join(stats_2018, by = c("TEAM_NAME")) %>%
    left_join(stats_2018, by = c("OPP_NAME" = "TEAM_NAME"))
  
  multi_filt_2019 <- box_group_all %>%
    filter(Season == 2019) %>%
    left_join(stats_2019, by = c("TEAM_NAME")) %>%
    left_join(stats_2019, by = c("OPP_NAME" = "TEAM_NAME"))
  
  multi_filt_2020 <- box_group_all %>%
    filter(Season == 2020) %>%
    left_join(stats_2020, by = c("TEAM_ID")) %>%
    left_join(stats_2020, by = c("OPP_ID" = "TEAM_ID"))
  
  master_reg_db <<- rbind(multi_filt_2017, multi_filt_2018, multi_filt_2019, multi_filt_2020) %>%
    mutate(Margin = PF - PA, Total = PF + PA)
}
  
  master_reg_db_w <<- which(is.na(master_reg_db$OE.x) == TRUE)
  master_reg_db_w <<- master_reg_db_w * -1
  master_reg_db <<- master_reg_db[master_reg_db_w, ]
  
  master_reg_db_w <<- which(is.na(master_reg_db$OE.y) == TRUE)
  master_reg_db_w <<- master_reg_db_w * -1
  master_reg_db <<- master_reg_db[master_reg_db_w, ]
  
  master_reg_db <<- master_reg_db %>%
    mutate(Win = ifelse(PF > PA, 1, 0))
  
  master_reg_ah <<- master_reg_db %>%
    filter(LOC == "A")
  
  master_reg_n <<- master_reg_db %>%
    filter(LOC == "N")
}

multi_team_fits <- function(x) {
  
  
  marg_ah_fit <<- step(lm(Margin ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                            TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                            AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                            dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                            DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                            dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                            AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                            dSTL.y + dBLK.y + dFOUL.y, data = master_reg_ah), direction='backward')
  
  score_aa_fit <<- step(lm(PF ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                             TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
                             AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
                             dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                             DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
                             AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                             dSTL.y + dBLK.y + dFOUL.y, data = master_reg_ah), direction='backward')
  
  score_ah_fit <<- step(lm(PA ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                             TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
                             AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
                             dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                             DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
                             AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                             dSTL.y + dBLK.y + dFOUL.y, data = master_reg_ah), direction='backward')
  
  total_ah_fit <<- step(lm(Total ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                             TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
                             AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
                             dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                             DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
                             AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                             dSTL.y + dBLK.y + dFOUL.y, data = master_reg_ah), direction='backward')
  
  marg_nn_fit <<- step(lm(Margin ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                            TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                            AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                            dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                            DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                            dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                            AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                            dSTL.y + dBLK.y + dFOUL.y, data = master_reg_n), direction='backward')
  
  score_na_fit <<- step(lm(PF ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                        TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
                        AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
                        dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                        DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                        dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
                        AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                        dSTL.y + dBLK.y + dFOUL.y, data = master_reg_n), direction='backward')
  
  score_nh_fit <<- step(lm(PA ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                        TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
                        AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
                        dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                        DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                        dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
                        AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                        dSTL.y + dBLK.y + dFOUL.y, data = master_reg_n), direction='backward')
  
  total_nn_fit <<- step(lm(Total ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                        TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
                        AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
                        dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                        DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                        dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
                        AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                        dSTL.y + dBLK.y + dFOUL.y, data = master_reg_n), direction='backward')
  
  winpct_ah_fit <<- step(glm(Win ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                               TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                               AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                               dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                               DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                               dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                               AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                               dSTL.y + dBLK.y + dFOUL.y, data = master_reg_ah, family = "binomial"), direction='backward')
  
  winpct_na_fit <<- step(glm(Win ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                          TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                          AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                          dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                          DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                          dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                          AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                          dSTL.y + dBLK.y + dFOUL.y, data = master_reg_n, family = "binomial"), direction='backward')
  
}


#Clustering Tests

cluster_cbb <- function(xxxx) {
  
  #Original Frame
  sss <- master_team_adj_frame_wt[, c(2, 3, 4, 10, 11, 16, 18, 19, 31, 35, 36)]
  sss_scale <- scale(sss[, 2:11])
  sss[, 2:11] <- sss_scale
  
  sss[, c(6, 9)] <- sss[, c(6, 9)] * -1

  #PCA Reduction
  ppp <- prcomp(sss[, 2:11], scale = FALSE, center = TRUE)
  
  #7 PC Components
  
  #Hierarchal Cluster
  
  hhh <- hclust(dist(ppp$x[, 1:7]), method = 'complete')
  
  #Cut into 14 Clusters
  
  ccc <- cutree(hhh, k = 25)
  
  #KMeans Cluster
  
  kkk <- kmeans(sss[, 2:11], centers = 25, nstart = 20)
  
  print(kkk)
  
  #Create Offensive Mean and Defensive Mean
  
  sss_wm <- sss %>%
    mutate(OMEAN = ((OE + eFG + TO + ORB + SRFT) / 5)) %>%
    mutate(DMEAN = ((DE + deFG + dTO + DRB + dSRFT) / 5))
  
  sss_wm[, 14] <- ccc
  sss_wm[, 15] <- kkk$cluster
  
  ggplot(sss_wm, aes(x = OMEAN, y = DMEAN, col = factor(V14))) + geom_point() + geom_text(aes(label = TEAM_NAME, V14))
  ggplot(sss_wm, aes(x = OMEAN, y = DMEAN, col = factor(V15))) + geom_point() + geom_text(aes(label = paste(TEAM_NAME, V15)))
  
  #OMEAN and DMEAN ONLY
  
  kkk2 <- kmeans(sss_wm[, 14:15], centers = 25, nstart = 20)
  sss_wm[, 16] <- kkk2$cluster
  ggplot(sss_wm, aes(x = OMEAN, y = DMEAN, col = factor(V16))) + geom_point() + geom_text(aes(label = paste(TEAM_NAME, V16)))

}


cluster_cbb2 <- function(ctype) {
  
  #Original Frame
  if (ctype == "OSKILL") { 
    sss <- master_team_adj_frame_wt[, c(2, 3, 4, 6, 8, 10, 13, 16, 35)]
    sss_col <- ncol(sss)
    
    sss_scale <- scale(sss[, 2:sss_col])
    sss[, 2:sss_col] <- sss_scale
    sss[, 8] <- sss[, 8] * -1
    
  }
  
  if (ctype == "OTEND") { 
    sss <- master_team_adj_frame_wt[, c(2, 5, 7, 10, 13, 35, 37)] 
    sss_col <- ncol(sss)
    
    sss_scale <- scale(sss[, 2:sss_col])
    sss[, 2:sss_col] <- sss_scale
  }
  
  if (ctype == "DSKILL") { 
    sss <- master_team_adj_frame_wt[, c(2, 11, 14, 15, 17, 18, 19, 21, 23, 28, 31, 36)]
    sss_col <- ncol(sss)
    
    sss_scale <- scale(sss[, 2:sss_col])
    sss[, 2:sss_col] <- sss_scale
    sss[, c(5, 6, 7, 8, 9, 10, 12)] <- sss[, c(5, 6, 7, 8, 9, 10, 12)] * -1
    
  }
  
 
  #PCA Reduction
  ppp <- prcomp(sss[, 2:sss_col], scale = FALSE, center = TRUE)
 
  print(summary(ppp))
  
  #5 PC Components
  
  #Hierarchal Cluster
  
  hhh <- hclust(dist(ppp$x[, 1:5]), method = 'complete')
  #plot(hhh)
  
  #Cut into 20 Clusters
  
  kmeans_ct <- 32
  ccc <- cutree(hhh, k = kmeans_ct)
  
  #KMeans Cluster
  
  kkk <- kmeans(sss[, 2:sss_col], centers = kmeans_ct, nstart = 20)
  
  sss[, (sss_col + 1)] <- ccc
  sss[, (sss_col + 2)] <- kkk$cluster
  colnames(sss)[sss_col + 1] <- "HCL"
  colnames(sss)[sss_col + 2] <- "KCL"
  
  sss <<- sss
  
  cploth <<- ggplot(sss, aes(x = OE, y = eFG, col = factor(HCL))) + geom_point() + geom_text(aes(label = TEAM_NAME))
  #cplotk <<- ggplot(sss, aes(x = OE, y = eFG, col = factor(KCL))) + geom_point() + geom_text(aes(label = TEAM_NAME))
  cplotk <<- ggplot(sss, aes(x = Tempo, y = ORB, col = factor(KCL))) + geom_point() + geom_text(aes(label = TEAM_NAME))
  
  #ssss <<- sss %>% group_by(KCL) %>% summarize(OE = mean(OE), eFG = mean(eFG), SM2 = mean(SM2), SM3 = mean(SM3), AST = mean(AST), TO = mean(TO), SRFT = mean(SRFT), ORB = mean(ORB))
  tttt <<- sss %>% group_by(HCL) %>% summarize(Tempo = mean(Tempo), SR2 = mean(SR2), SR3 = mean(SR3), ORB = mean(ORB), AST = mean(AST), SRFT = mean(SRFT))
  
}
master_reg_ah2 <- as.data.frame(scale.default(master_reg_ah[, c(9, 13:44, 47:78)], center = TRUE, scale = TRUE))
master_reg_ah3 <- as.data.frame(master_reg_ah[, c(82, 13:44, 47:78)])

zzzz <- lm(PF~., data = master_reg_ah2)
yyyy <- lm(Margin~., data = master_reg_ah3)
xxxx <- lm(PF~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
             TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
             AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
             dSTL.x + dBLK.x + dFOUL.x + OE.y + 
             DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
             dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
             AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
             dSTL.y + dBLK.y + dFOUL.y, master_reg_ah)
uuuu <- lm(Margin~OE.x + DE.x + eFG.x + ORB.x + 
             TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
             AST.x + dSM2.x + dSM3.x + dAST.x +
             OE.y + 
             DE.y + eFG.y + ORB.y + TO.y + deFG.y + 
             dORB.y + SM2.y + SM3.y +
             AST.y + dSM2.y + dSM3.y + dAST.y, master_reg_ah)
uuuu <- step(lm(Margin~OE.x + DE.x + OE.y + DE.y + eFG.x + ORB.x + 
             TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
             dSM2.x + dSM3.x + dAST.x +
             eFG.y + ORB.y + TO.y + deFG.y + 
             dORB.y + SM2.y + SM3.y +
             dSM2.y + dSM3.y + dAST.y, master_reg_ah), direction='backward')
uuuu <- step(lm(Margin~eFG.x + ORB.x + 
                  TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                  dSM2.x + dSM3.x + dAST.x +
                  eFG.y + ORB.y + TO.y + deFG.y + 
                  dORB.y + SM2.y + SM3.y +
                  dSM2.y + dSM3.y + dAST.y, master_reg_ah), direction='backward')
uuuu <- lm(Margin~OE.x + DE.x + dSM3.x + OE.y + 
             DE.y + eFG.y + dSM3.y + dAST.y, master_reg_ah)

zx <-  model.matrix(Margin~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                     TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SR2.x + SR3.x + SM2.x + SM3.x +
                     AST.x + STL.x + BLK.x + FOUL.x + dSR2.x + dSR3.x + dSM2.x + dSM3.x + dAST.x +
                     dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                     DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                     dORB.y + dTO.y + dSRFT.y + SR2.y + SR3.y + SM2.y + SM3.y +
                     AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                     dSTL.y + dBLK.y + dFOUL.y, master_reg_ah)[,-1]

zy <- y <- master_reg_ah$Margin

zzz <- rfcv(zx, zy, )

library(glmnet)
x <- model.matrix(Margin~OE.x + DE.x + OE.y + DE.y + eFG.x + ORB.x + 
                    TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                    dSM2.x + dSM3.x + dAST.x +
                    eFG.y + ORB.y + TO.y + deFG.y + 
                    dORB.y + SM2.y + SM3.y +
                    dSM2.y + dSM3.y + dAST.y, master_reg_ah)[,-1]
y <- master_reg_ah$Margin
lambda <- 10^seq(10, -2, length = 100)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]
cv.glmnet(x[train,], y[train], nfolds=5)
swisslm <- lm(Margin ~ OE.x + DE.x + OE.y + DE.y + eFG.x + ORB.x + 
                TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                dSM2.x + dSM3.x + dAST.x +
                eFG.y + ORB.y + TO.y + deFG.y + 
                dORB.y + SM2.y + SM3.y +
                dSM2.y + dSM3.y + dAST.y, data = master_reg_ah)
coef(swisslm)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = lambda)
predict(ridge.mod, s = 0, type = 'coefficients')

swisslm <- lm(Margin ~ OE.x + DE.x + OE.y + DE.y + eFG.x + ORB.x + 
                TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                dSM2.x + dSM3.x + dAST.x +
                eFG.y + ORB.y + TO.y + deFG.y + 
                dORB.y + SM2.y + SM3.y +
                dSM2.y + dSM3.y + dAST.y, data = master_reg_ah, subset = train)
ridge.mod <- glmnet(x[train,], y[train], alpha = 0, lambda = lambda)
#find the best lambda from our list via cross-validation
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
bestlam <- cv.out$lambda.min
ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test,])
s.pred <- predict(swisslm, newdata = master_reg_ah[test,])
#check MSE
mean((s.pred-ytest)^2)
mean((ridge.pred-ytest)^2)
out = glmnet(x[train,],y[train],alpha = 0)
predict(ridge.mod, type = "coefficients", s = bestlam)

lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = lambda)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred-ytest)^2)
lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)

#--------KNN---------



#----------BLENDER TRIALS----------------


blender_test_ml <- function(xxxx) {
  
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  g_file$WinNeut <- as.factor(g_file$WinNeut)
  print(table(g_file$WinNeut))
  
  bml_full <<- glm(WinNeut~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                    TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                    AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                    dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                    DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                    dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                    AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                    dSTL.y + dBLK.y + dFOUL.y, data = g_file, family = 'binomial')

  
  bml_trim <<- glm(WinNeut~OE.x + DE.x + eFG.x + ORB.x + 
                        TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                        AST.x + dSM2.x + dSM3.x + dAST.x +
                        OE.y + SRFT.x + SRFT.y + dSRFT.x + dSRFT.y +
                        DE.y + eFG.y + ORB.y + TO.y + deFG.y + 
                        dORB.y + SM2.y + SM3.y +
                        AST.y + dSM2.y + dSM3.y + dAST.y, data = g_file, family = 'binomial')
  
  bml_shorter <<- glm(WinNeut~OE.x + DE.x + OE.y + DE.y + eFG.x + ORB.x + 
                           TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                           dSM2.x + dSM3.x + dAST.x +
                           eFG.y + ORB.y + TO.y + deFG.y + 
                           dORB.y + SM2.y + SM3.y +
                           dSM2.y + dSM3.y + dAST.y, data = g_file, family = 'binomial')
  
  bml_no <<- glm(WinNeut~eFG.x + ORB.x + 
                   TO.x + deFG.x + dORB.x + SM2.x +
                   dSM2.x + dAST.x +
                   eFG.y + ORB.y + TO.y + deFG.y + 
                   dORB.y + SM2.y +
                   dSM2.y + dAST.y, data = g_file, family = 'binomial')
  
  bml_four <<- glm(WinNeut~OE.x + DE.x + eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x + dSRFT.x +
                        OE.y + DE.y + eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + dSRFT.y, data = g_file, family = 'binomial')
  
  bml_fouract <<- glm(WinNeut~eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x + dSRFT.x +
                           eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + dSRFT.y, data = g_file, family = 'binomial')
  
  bml_bare <<- glm(WinNeut~OE.x + DE.x + eFG.x + OE.y + 
                        DE.y + eFG.y + deFG.x + deFG.y, data = g_file, family = 'binomial')
  
  bml_step <<- step(glm(WinNeut~eFG.x + ORB.x + 
                             TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                             AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                             dSTL.x + dBLK.x + dFOUL.x + 
                             eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                             AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                             dSTL.y + dBLK.y + dFOUL.y, data = g_file, family = 'binomial'))
  
}

blender_ml_hone <- function(xxxx) {
  
  #WINNER: NO
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  bml_no <<- glm(WinNeut~eFG.x + ORB.x + 
                   TO.x + deFG.x + dORB.x + SM2.x +
                   dSM2.x + dAST.x +
                   eFG.y + ORB.y + TO.y + deFG.y + 
                   dORB.y + SM2.y +
                   dSM2.y + dAST.y, data = g_file, family = 'binomial')
  
  bml_step <<- glm(WinNeut~eFG.x + ORB.x + TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + 
                     eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + dORB.y + dTO.y + dSRFT.y, data = g_file, family = 'binomial')
}

blender_use <- function(xxx) {
  
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  ss <- sample(c(1:nrow(g_file)), round(nrow(g_file) * .5, 0))
  g_file_train <- g_file[ss, ]
  g_file$WinNeut <- as.factor(g_file$WinNeut)
  
  
  #Blender No for ATS
  #blender_ats_use <<- lm(Margin~eFG.x + ORB.x + 
                           #TO.x + deFG.x + dORB.x + SM2.x +
                           #dSM2.x + dAST.x +
                           #eFG.y + ORB.y + TO.y + deFG.y + 
                           #dORB.y + SM2.y +
                           #dSM2.y + dAST.y, data = g_file)
  
  #Blender No for Total
  #blender_total_use <<- lm(Total~eFG.x + ORB.x + 
                             #TO.x + deFG.x + dORB.x +
                             #+ eFG.y + ORB.y + TO.y + deFG.y + dORB.y +
                             #Tempo.x + Tempo.y +
                             #dTO.x + dTO.y, data = g_file)
  
  #Blender No for ML
  blender_ml_use <<- glm(WinNeut~eFG.x + ORB.x + 
                           TO.x + deFG.x + dORB.x + SM2.x +
                           dSM2.x + dAST.x +
                           eFG.y + ORB.y + TO.y + deFG.y + 
                           dORB.y + SM2.y +
                           dSM2.y + dAST.y, data = g_file, family = 'binomial')
  
  #RF Four Act for ATS
  #zz_ats_use <<- randomForest(Margin~eFG.x + TO.x + ORB.x + DRB.x + deFG.x +
                                #eFG.y + TO.y + ORB.y + DRB.y + deFG.y, data = g_file_train, do.trace=2)
  
  #RF Four for TOT
  #zzz_total_use <<- randomForest(Total~OE.x + DE.x + eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x +
                                 #OE.y + DE.y + eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + Tempo.x + Tempo.y, data = g_file_train, do.trace=2)
  
  #RF No for ML
  zzz_ml_use <<- randomForest(WinNeut~eFG.x + ORB.x + 
                               TO.x + deFG.x + dORB.x + SM2.x +
                               dSM2.x + dAST.x +
                               eFG.y + ORB.y + TO.y + deFG.y + 
                               dORB.y + SM2.y +
                               dSM2.y + dAST.y + SM3.x + dSM3.x + SM3.y + dSM3.y, data = g_file, mtry = 10, do.trace=2)
}

blender_test_ats <- function(xxxx) {
  
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  #WINNER: BLENDER NO
  
  blender_full <<- lm(Margin~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                       TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                       AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                       dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                       DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                       dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                       AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                       dSTL.y + dBLK.y + dFOUL.y, data = g_file)
  
  blender_trim <<- lm(Margin~OE.x + DE.x + eFG.x + ORB.x + 
                       TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                       AST.x + dSM2.x + dSM3.x + dAST.x +
                       OE.y + SRFT.x + SRFT.y + dSRFT.x + dSRFT.y +
                       DE.y + eFG.y + ORB.y + TO.y + deFG.y + 
                       dORB.y + SM2.y + SM3.y +
                       AST.y + dSM2.y + dSM3.y + dAST.y, data = g_file)
  
  blender_shorter <<- lm(Margin~OE.x + DE.x + OE.y + DE.y + eFG.x + ORB.x + 
                          TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                          dSM2.x + dSM3.x + dAST.x +
                          eFG.y + ORB.y + TO.y + deFG.y + 
                          dORB.y + SM2.y + SM3.y +
                          dSM2.y + dSM3.y + dAST.y, data = g_file)
  
  blender_no <<- lm(Margin~eFG.x + ORB.x + 
                     TO.x + deFG.x + dORB.x + SM2.x +
                     dSM2.x + dAST.x +
                     eFG.y + ORB.y + TO.y + deFG.y + 
                     dORB.y + SM2.y +
                     dSM2.y + dAST.y, data = g_file)
  
  blender_four <<- lm(Margin~OE.x + DE.x + eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x + dSRFT.x +
                       OE.y + DE.y + eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + dSRFT.y, data = g_file)
  
  blender_fouract <<- lm(Margin~eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x + dSRFT.x +
                          eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + dSRFT.y, data = g_file)
  
  blender_bare <<- lm(Margin~OE.x + DE.x + eFG.x + OE.y + 
                       DE.y + eFG.y + deFG.x + deFG.y, data = g_file)
  
  #blender_step <<- step(lm(Margin~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                                         #TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                                         #AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                                         #dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                                         #DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                                         ##dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                                         #AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                                         #dSTL.y + dBLK.y + dFOUL.y, data = g_file))
  
  blender_step <<- step(lm(Margin~eFG.x + ORB.x + 
                             TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                             AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                             dSTL.x + dBLK.x + dFOUL.x + 
                             eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                             AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                             dSTL.y + dBLK.y + dFOUL.y, data = g_file))
  
}

blender_test_total <- function(xxxx) {
  
  #WINNER: BLENDER NO
  
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  
  blender_full <<- lm(Total~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                        TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                        AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                        dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                        DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                        dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                        AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                        dSTL.y + dBLK.y + dFOUL.y, data = g_file)
  
  blender_trim <<- lm(Total~OE.x + DE.x + eFG.x + ORB.x + 
                        TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                        AST.x + dSM2.x + dSM3.x + dAST.x +
                        OE.y + SRFT.x + SRFT.y + dSRFT.x + dSRFT.y +
                        DE.y + eFG.y + ORB.y + TO.y + deFG.y + 
                        dORB.y + SM2.y + SM3.y +
                        AST.y + dSM2.y + dSM3.y + dAST.y + Tempo.x + Tempo.y, data = g_file)
  
  blender_shorter <<- lm(Total~OE.x + DE.x + OE.y + DE.y + eFG.x + ORB.x + 
                           TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                           dSM2.x + dSM3.x + dAST.x +
                           eFG.y + ORB.y + TO.y + deFG.y + 
                           dORB.y + SM2.y + SM3.y +
                           dSM2.y + dSM3.y + dAST.y + Tempo.x + Tempo.y, data = g_file)
  
  blender_no <<- lm(Total~eFG.x + ORB.x + 
                      TO.x + deFG.x + dORB.x +
                       + eFG.y + ORB.y + TO.y + deFG.y + dORB.y +
                      Tempo.x + Tempo.y +
                      dTO.x + dTO.y, data = g_file)
  
  blender_four <<- lm(Total~OE.x + DE.x + eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x +
                        OE.y + DE.y + eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + Tempo.x + Tempo.y, data = g_file)
  
  blender_fouract <<- lm(Total~eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x + dSRFT.x +
                           eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + dSRFT.y + Tempo.x + Tempo.y, data = g_file)
  
  blender_bare <<- lm(Total~OE.x + DE.x + eFG.x + OE.y + 
                        DE.y + eFG.y + deFG.x + deFG.y + Tempo.x + Tempo.y, data = g_file)
  
  #blender_step <<- step(lm(Total~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                             #TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                             #AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                             #dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                             #DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             #dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                             #AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                             #dSTL.y + dBLK.y + dFOUL.y, data = g_file))
  
  #blender_step_no <<- step(lm(Total~eFG.x + ORB.x + 
                             #TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                             #AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                             #dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                             #DE.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             #dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                             #AST.y + STL.y + BLK.y + FOUL.y + dSR2.y + dSR3.y + dSM2.y + dSM3.y + dAST.y +
                             #dSTL.y + dBLK.y + dFOUL.y + Tempo.x + Tempo.y, data = g_file))
  
}

rf_test_ats <- function(x) {
  
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  ss <- sample(c(1:nrow(g_file)), round(nrow(g_file) * .425, 0))
  g_file_train <- g_file[ss, ]
  g_file <- g_file_train
  
  #zzz_ats <<- randomForest(Margin~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                             #TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                             #AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                             #dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                             #DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             #dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                             #AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                             #dSTL.y + dBLK.y + dFOUL.y, data=g_file_train, do.trace=2)
  
  #zzz_tot <<- randomForest(Total~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
                             #TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
                             #AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
                             #dSTL.x + dBLK.x + dFOUL.x + OE.y + 
                             ##DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
                             #dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
                             #AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
                             #dSTL.y + dBLK.y + dFOUL.y, data=g_file_train, do.trace=2)
  
  
  print("Tier 2")
  zzz_ats_tier2 <<- randomForest(Margin~OE.x + DE.x + eFG.x + OE.y + DE.y + eFG.y, data = g_file, do.trace=2)
  print("Tier 3")
  zzz_ats_tier3 <<- randomForest(Margin~OE.x + DE.x + eFG.x + OE.y + DE.y + eFG.y
                                + SM2.x + dSM2.x + SM2.y + dSM2.y, data = g_file, do.trace=2)
  zzz_ats_tier4 <<- randomForest(Margin~OE.x + DE.x + eFG.x + OE.y + DE.y + eFG.y
                                + SM2.x + dSM2.x + SM2.y + dSM2.y
                                + AST.x + dAST.x + AST.y + dAST.y, data = g_file, do.trace=2)
  zzz_ats_tier5 <<- randomForest(Margin~OE.x + DE.x + eFG.x + OE.y + DE.y + eFG.y
                                + SM2.x + dSM2.x + SM2.y + dSM2.y
                                + AST.x + dAST.x + AST.y + dAST.y +
                                ORB.x + dORB.x + ORB.y + dORB.y + SM3.x + dSM3.x + SM3.y + dSM3.y + TO.x + TO.y + dTO.x + dTO.y, data = g_file, do.trace=2)
}

rf_test_ats2 <- function(x) {

  #Winner: FourAct
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  ss <- sample(c(1:nrow(g_file)), round(nrow(g_file) * .425, 0))
  g_file_train <- g_file[ss, ]
  g_file <- g_file_train
  
  #zzz_ats_four <<- randomForest(Margin~OE.x + DE.x + eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x +
                                 #OE.y + DE.y + eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y, data = g_file, do.trace=2)
  
  #zz_ats_fouract <<- randomForest(Margin~eFG.x + TO.x + ORB.x + DRB.x + deFG.x +
                                 # eFG.y + TO.y + ORB.y + DRB.y + deFG.y, data = g_file, do.trace=2)
  
  #zzz_ats_tier4 <<- randomForest(Margin~OE.x + DE.x + eFG.x + OE.y + DE.y + eFG.y
                                 #+ SM2.x + dSM2.x + SM2.y + dSM2.y, data = g_file, do.trace=2)
  
  zzz_ats_no <<- randomForest(Margin~eFG.x + ORB.x + 
                                TO.x + deFG.x + dORB.x + SM2.x +
                                dSM2.x + dAST.x +
                                eFG.y + ORB.y + TO.y + deFG.y + 
                                dORB.y + SM2.y +
                                dSM2.y + dAST.y, data = g_file, do.trace=2)
}

rf_test_tot <- function(x) {
  
  #Winner: Four
  
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  ss <- sample(c(1:nrow(g_file)), round(nrow(g_file) * .4, 0))
  g_file_train <- g_file[ss, ]
  g_file <- g_file_train

  
  #zzz_tot <<- randomForest(Total~OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
  #TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
  #AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
  #dSTL.x + dBLK.x + dFOUL.x + OE.y + 
  ##DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
  #dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
  #AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
  #dSTL.y + dBLK.y + dFOUL.y, data=g_file_train, do.trace=2)
  
  
  #print("Tier 2")
  #zzz_tot_tier2 <<- randomForest(Total~OE.x + DE.x + Tempo.x + OE.y + DE.y + Tempo.y, data = g_file, do.trace=2)
  print("Tier 3")
  #zzz_tot_tier3 <<- randomForest(Total~OE.x + DE.x + Tempo.x + OE.y + DE.y + Tempo.y
                                # + eFG.x + eFG.y, data = g_file, do.trace=2)
  
  #zzz_tot_tier3 <<- randomForest(Total~OE.x + DE.x + Tempo.x + OE.y + DE.y + Tempo.y
  #+ eFG.x + eFG.y, data = g_file, do.trace=2)
  
  #zzz_tot_tier4 <<- randomForest(Total~OE.x + DE.x + Tempo.x + OE.y + DE.y + Tempo.y
                                 #+ eFG.x + eFG.y + SM3.x + dSM3.x + SM3.y + dSM3.y +
                                 #SM2.x + dSM2.x + SM2.y + dSM2.y + 
                                 #TO.x + dTO.x + TO.y + dTO.y +
                                 #AST.x + dAST.x + AST.y + dAST.y, data = g_file, do.trace=2)
  
  #zzz_tot_tier5 <<- randomForest(Total~OE.x + DE.x + Tempo.x + OE.y + DE.y + Tempo.y
                                 #+ eFG.x + eFG.y + SM3.x + dSM3.x + SM3.y + dSM3.y +
                                   #SM2.x + dSM2.x + SM2.y + dSM2.y + 
                                   #TO.x + dTO.x + TO.y + dTO.y +
                                   #AST.x + dAST.x + AST.y + dAST.y +
                                   #ORB.x + DRB.x + ORB.y + DRB.y +
                                   #BLK.x + STL.x + dBLK.x + dBLK.y + dSTL.x + dSTL.y, data = g_file, do.trace=2)
  
  
  #zzz_tot_four <<- randomForest(Total~OE.x + DE.x + eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x +
                                    #OE.y + DE.y + eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + Tempo.x + Tempo.y, data = g_file, do.trace=2)
  
  #zzz_tot_fouract <<- randomForest(Total~eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x +
                                  #eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + Tempo.x + Tempo.y, data = g_file, do.trace=2)
  
}
rf_test_tot2 <- function(xxx) {
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  ss <- sample(c(1:nrow(g_file)), round(nrow(g_file) * .4, 0))
  g_file_train <- g_file[ss, ]
  g_file <- g_file_train
  zzz_tot_no <<- randomForest(Total ~ eFG.x + ORB.x + 
                                TO.x + deFG.x + dORB.x + SM2.x + SM3.x +
                                dSM2.x + dSM3.x + dAST.x +
                                eFG.y + ORB.y + TO.y + deFG.y + 
                                dORB.y + SM2.y + SM3.y + dSM3.y +
                                dSM2.y + dAST.y, data = g_file, do.trace=2)
}

rf_test_ml <- function(x) {
  
  g_file <- read_csv('pyvars\\master_reg_haneut.csv')
  g_file$WinNeut <- as.factor(g_file$WinNeut)
  ss <- sample(c(1:nrow(g_file)), round(nrow(g_file) * .7, 0))
  g_file_train <- g_file[ss, ]
  g_file <- g_file_train
  
  #zzz_ml <<- randomForest(WinNeut ~ OE.x + DE.x + Tempo.x + eFG.x + ORB.x + 
  #TO.x + SRFT.x + deFG.x + dORB.x + dTO.x + dSRFT.x + SM2.x + SM3.x +
  #AST.x + STL.x + BLK.x + FOUL.x + dSM2.x + dSM3.x + dAST.x +
  #dSTL.x + dBLK.x + dFOUL.x + OE.y + 
  #DE.y + Tempo.y + eFG.y + ORB.y + TO.y + SRFT.y + deFG.y + 
  #dORB.y + dTO.y + dSRFT.y + SM2.y + SM3.y +
  #AST.y + STL.y + BLK.y + FOUL.y + dSM2.y + dSM3.y + dAST.y +
  #dSTL.y + dBLK.y + dFOUL.y, data=g_file_train, do.trace=2)
  
  zzz_ml_tier2 <<- randomForest(WinNeut ~ OE.x + DE.x + eFG.x + OE.y + DE.y + deFG.y, data=g_file_train, mtry = 6, do.trace=2)
  
  zzz_ml_tier3 <<- randomForest(WinNeut ~ OE.x + DE.x + eFG.x + OE.y + DE.y + deFG.y + 
                                  SM2.x + dSM2.x + SM2.y + dSM2.y + AST.x + dAST.x + AST.y + dAST.y, data=g_file_train, mtry = 9, do.trace=2)
  
  zzz_ml_tier4 <<- randomForest(WinNeut ~ OE.x + DE.x + eFG.x + OE.y + DE.y + deFG.y + 
                                  SM2.x + dSM2.x + SM2.y + dSM2.y + AST.x + dAST.x + AST.y + dAST.y +
                                SM3.x + ORB.x + TO.x + dSM3.x + dORB.x + dTO.x +
                                SM3.y + ORB.y + TO.y + dSM3.y + dORB.y + dTO.y, data=g_file_train, mtry = 12, do.trace=2)
  
  zzz_ml_tier5 <<- randomForest(WinNeut ~ OE.x + DE.x + eFG.x + OE.y + DE.y + deFG.y + 
                                  SM2.x + dSM2.x + SM2.y + dSM2.y + AST.x + dAST.x + AST.y + dAST.y +
                                SM3.x + ORB.x + TO.x + dSM3.x + dORB.x + dTO.x +
                                  SM3.y + ORB.y + TO.y + dSM3.y + dORB.y + dTO.y +
                                  SRFT.x + BLK.x + dSRFT.x + dBLK.x + SRFT.y + BLK.y + dSRFT.y + dBLK.y, data=g_file_train, mtry = 15, do.trace=2)
  
  zzz_ml_no <<- randomForest(WinNeut~eFG.x + ORB.x + 
                   TO.x + deFG.x + dORB.x + SM2.x +
                   dSM2.x + dAST.x +
                   eFG.y + ORB.y + TO.y + deFG.y + 
                   dORB.y + SM2.y +
                   dSM2.y + dAST.y + SM3.x + dSM3.x + SM3.y + dSM3.y, data = g_file_train, mtry = 10, do.trace=2)
  
  zzz_ml_four <<- randomForest(WinNeut~OE.x + DE.x + eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x + dSRFT.x +
                     OE.y + DE.y + eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + dSRFT.y, data = g_file_train, mtry = 9, do.trace=2)
  
  zzz_ml_fouract <<- randomForest(WinNeut~eFG.x + TO.x + ORB.x + DRB.x + deFG.x + dTO.x + SRFT.x + dSRFT.x +
                        eFG.y + TO.y + ORB.y + DRB.y + deFG.y + dTO.y + SRFT.y + dSRFT.y, data = g_file_train, mtry = 9, do.trace=2)
  

}

rf_ml_hone <- function(x) {
  
  zzz_ml_no <<- randomForest(WinNeut~eFG.x + ORB.x + 
                               TO.x + deFG.x + dORB.x + SM2.x +
                               dSM2.x + dAST.x +
                               eFG.y + ORB.y + TO.y + deFG.y + 
                               dORB.y + SM2.y +
                               dSM2.y + dAST.y + SM3.x + dSM3.x + SM3.y + dSM3.y, data = g_file_train, mtry = 10, do.trace=2)
  
}


backtest_date_setup <- function(x) {
  my_date_vec <- date_vector_[c(81:94, 97), ]
  my_date_vec[, 2] <- c(80:93, 96)
  vec_vec <- c(80:93, 96)
  
  a <- 1
  g <- nrow(my_date_vec)
  
  for (a in a:g) {
    
    b <- as.character(my_date_vec[a, 1])
    print(b)
    s <- strsplit(b, "-")
    m <- s[[1]][1]
    d <- s[[1]][2]
    y <- s[[1]][3]
    
    my_date_vec[a, 3] <- as.numeric(m)
    my_date_vec[a, 4] <- as.numeric(d)
    my_date_vec[a, 5] <- as.numeric(y)
    
  }
  colnames(my_date_vec)[3:5] <- c("Month", "Day", "Year")
  
  fw <- read_csv('formatted_wpt5.csv')
  
  my_date_vec2 <- my_date_vec %>%
    inner_join(fw, by = c("Month", "Day", "Year"))
  
  print(colnames(my_date_vec2))
  
  my_date_vec2 <- my_date_vec2[, c(1:14, 59, 60, 62)]
  
  
  ct <- 0
  for (a in vec_vec) {
    as_file <- paste("statsasof_", a, ".csv", sep = "")
    as_file <- read_csv(as_file)
    moron <- my_date_vec2 %>%
      filter(ct == a) %>%
      left_join(as_file, by = c("AID" = "TEAM_ID")) %>%
      left_join(as_file, by = c("HID" = "TEAM_ID"))
    
    if (ct == 0) { my_date_vec3 <- moron }
    else { my_date_vec3 <- rbind(my_date_vec3, moron) }
    ct <- ct + 1
  }
  
  backtest_frame <<- my_date_vec3
}

backtest_predict <- function(xxx) {
  
  blender_list <<- list(blender_ats_use)
  rf_list <<- list(zz_ats_use)
  blender_names <- c("Use")
  rf_names <- c("Use")
  backtest_xs <<- backtest_frame[, c(18:54, 56:91)]
  backtest_ys <<- backtest_frame[, c(15:17)] %>%
    mutate(MARG = ACTASCORE - ACTHSCORE)
  
  backtest_info <<- backtest_frame[, c(3:12)]
  backtest_info <<- cbind(backtest_info, backtest_ys)
  
  a <- 1
  g <- length(blender_list)
  #g <- length(rf_list)
  #g <- 1
  
  for (a in a:g) {
  
    #blender_obj <- blender_list[[a]]
    #blender_name <- blender_names[a]
    #print(blender_name)
    blender_obj <- rf_list[[a]]
    blender_name <- rf_names[a]
    print(rf_names)
    full_pred <- predict(blender_obj, backtest_xs)
    full_pred <- full_pred - 2.36
    pred_comb <- cbind(backtest_info, full_pred)
    pred_comb <- pred_comb %>%
      mutate(ATSVALUE = Spread - full_pred) %>%
      mutate(ATSPICK = ifelse(ATSVALUE < 0, 0, 1)) %>%
      mutate(ASCOREATS = ACTASCORE - Spread) %>%
      mutate(ATSRES = ifelse(ASCOREATS == ACTHSCORE, 0.5, ifelse(ASCOREATS > ACTHSCORE, 0, 1))) %>%
      filter(ATSRES != 0.5) %>%
      mutate(GRADE = ifelse(ATSRES == ATSPICK, 1, -1.1)) %>%
      mutate(ABSVALUE = abs(ATSVALUE)) %>%
      arrange(desc(ABSVALUE)) %>%
      mutate(MARGERROR = abs(full_pred - MARG))
    
    pred_comb[, 23] <- cumsum(pred_comb$GRADE)
    colnames(pred_comb)[23] <- "CUME"
    
    model_w <- length(which(pred_comb$GRADE > 0))
    model_l <- length(which(pred_comb$GRADE < 0))
    model_p <- model_w / (model_w + model_l)
    model_u <- sum(pred_comb$GRADE)
    model_k <- max(pred_comb$CUME)
    model_e <- pred_comb$ABSVALUE[which.max(pred_comb$CUME)[1]]
    model_x <- which.max(pred_comb$CUME)[1] / nrow(pred_comb)
    model_r <- mean(pred_comb$MARGERROR)
    
    print(c(model_w, model_l, model_p, model_u, model_k, model_e, model_x))
    if (a == 1) { btest_frame <<- data.frame(blender_name, model_w, model_l, model_p, model_u, model_k, model_e, model_x, model_r, stringsAsFactors = FALSE) }
    else { btest_frame[a, ] <<- c(blender_name, model_w, model_l, model_p, model_u, model_k, model_e, model_x, model_r) }
  }
  View(pred_comb)
  View(btest_frame)
}

backtest_total_predict <- function(xxx) {
  
  blender_list <<- list(blender_total_use)
  blender_names <- c("Use")
  rf_list <<- list(zzz_total_use)
  rf_names <<- c("Use")
  backtest_xs <<- backtest_frame[, c(18:54, 56:91)]
  backtest_ys <<- backtest_frame[, c(15:17)] %>%
    mutate(MARG = ACTASCORE - ACTHSCORE)
  
  backtest_info <<- backtest_frame[, c(3:12)]
  backtest_info <<- cbind(backtest_info, backtest_ys)
  
  a <- 1
  #g <- length(blender_list)
  g <- length(rf_list)
  
  for (a in a:g) {
    
    #blender_obj <- blender_list[[a]]
    #blender_name <- blender_names[a]
    #print(blender_name)
    blender_obj <- rf_list[[a]]
    blender_name <- rf_names[a]
    print(blender_name)
    full_pred <- predict(blender_obj, backtest_xs)
    pred_comb <- cbind(backtest_info, full_pred)
    pred_comb <- pred_comb %>%
      mutate(TOTVALUE = full_pred - Total) %>%
      mutate(TOTPICK = ifelse(TOTVALUE > 0, 0, 1)) %>%
      mutate(TSCORE = ACTTOTAL) %>%
      mutate(TOTRES = ifelse(TSCORE == Total, 0.5, ifelse(TSCORE > Total, 0, 1))) %>%
      filter(TOTRES != 0.5) %>%
      mutate(GRADE = ifelse(TOTRES == TOTPICK, 1, -1.1)) %>%
      mutate(ABSVALUE = abs(TOTVALUE)) %>%
      arrange(desc(ABSVALUE)) %>%
      mutate(MARGERROR = abs(full_pred - ACTTOTAL))
    
    pred_comb[, 23] <- cumsum(pred_comb$GRADE)
    colnames(pred_comb)[23] <- "CUME"
    
    model_w <- length(which(pred_comb$GRADE > 0))
    model_l <- length(which(pred_comb$GRADE < 0))
    model_p <- model_w / (model_w + model_l)
    model_u <- sum(pred_comb$GRADE)
    model_k <- max(pred_comb$CUME)
    model_e <- pred_comb$ABSVALUE[which.max(pred_comb$CUME)[1]]
    model_x <- which.max(pred_comb$CUME)[1] / nrow(pred_comb)
    model_r <- mean(pred_comb$MARGERROR)
    
    print(c(model_w, model_l, model_p, model_u, model_k, model_e, model_x))
    if (a == 1) { btest_frame <<- data.frame(blender_name, model_w, model_l, model_p, model_u, model_k, model_e, model_x, model_r, stringsAsFactors = FALSE) }
    else { btest_frame[a, ] <<- c(blender_name, model_w, model_l, model_p, model_u, model_k, model_e, model_x, model_r) }
  }
  
  View(pred_comb)
  View(btest_frame)
}


backtest_ml_predict <- function(xxx) {
  
  blender_list <<- list(blender_ml_use)
  rf_list <<- list(zzz_ml_use)
  blender_names <- c("Use")
  rf_names <- c("Use")
  backtest_xs <<- backtest_frame[, c(18:54, 56:91)]
  backtest_ys <<- backtest_frame[, c(15:17)] %>%
    mutate(MARG = ACTASCORE - ACTHSCORE)
  
  backtest_info <<- backtest_frame[, c(3:12)]
  backtest_info <<- cbind(backtest_info, backtest_ys)
  
  a <- 1
  g <- length(blender_list)
  #g <- length(rf_list)
  #g <- 6
  h_const <- .933
  
  for (a in a:g) {
    
    #blender_obj <- blender_list[[a]]
    #blender_name <- blender_names[a]
    #print(blender_name)
    #full_pred <- predict(blender_obj, backtest_xs, type='response')
    blender_obj <- rf_list[[a]]
    blender_name <- rf_names[a]
    print(rf_names)
    full_pred <- predict(blender_obj, backtest_xs, type='prob')
    full_pred <- as.data.frame(full_pred[, 2])
    colnames(full_pred)[1] <- "full_pred"
    
    pred_comb <- cbind(backtest_info, full_pred) %>%
      mutate(ANPROB = ifelse(full_pred < .5, full_pred * h_const, (1 - (1 - full_pred * h_const)))) %>%
      mutate(HNPROB = 1 - ANPROB)
    
    b <- 1
    h <- nrow(pred_comb)
    for (b in b:h) {
      
      aml <- pred_comb$AML[b]
      hml <- pred_comb$HML[b]
      pred_comb[b, 18] <- moneyline_conversion(aml)
      pred_comb[b, 19] <- moneyline_conversion(hml)
      
    }
    pred_comb <- pred_comb %>%
      mutate(APAYOUT = 1 * ((1 - V18) / V18)) %>%
      mutate(HPAYOUT = 1 * ((1 - V19) / V19)) %>%
      mutate(AEXPVALUE = (APAYOUT * ANPROB) + (-1 * (1 - ANPROB))) %>%
      mutate(HEXPVALUE = (HPAYOUT * HNPROB) + (-1 * (1 - HNPROB))) %>%
      mutate(MLPICK = ifelse(AEXPVALUE > HEXPVALUE, 1, 0)) %>%
      mutate(PICKVALUE = ifelse(AEXPVALUE > HEXPVALUE, AEXPVALUE, HEXPVALUE)) %>%
      mutate(RESULT = ifelse(ACTASCORE > ACTHSCORE, 1, 0)) %>%
      mutate(GRADE = ifelse(RESULT == MLPICK, 1, -1)) %>%
      mutate(PICKPAYOUT = ifelse(AEXPVALUE > HEXPVALUE, APAYOUT, HPAYOUT)) %>%
      mutate(GRADEPAY = ifelse(GRADE == 1, PICKPAYOUT, -1)) %>%
      filter(PICKPAYOUT <= 9.5 & PICKPAYOUT >= -4) %>%
      arrange(desc(PICKVALUE))
    
    pred_comb[, 30] <- cumsum(pred_comb$GRADEPAY)
    colnames(pred_comb)[30] <- "CUME"
    
    model_w <- length(which(pred_comb$GRADE > 0))
    model_l <- length(which(pred_comb$GRADE < 0))
    model_p <- model_w / (model_w + model_l)
    model_u <- sum(pred_comb$GRADEPAY)
    model_k <- max(pred_comb$CUME)
    model_e <- pred_comb$PICKVALUE[which.max(pred_comb$CUME)[1]]
    model_x <- which.max(pred_comb$CUME)[1] / nrow(pred_comb)
    
    if (a == 1) { btest_frame <<- data.frame(blender_name, model_w, model_l, model_p, model_u, model_k, model_e, model_x, stringsAsFactors = FALSE) }
    else { btest_frame[a, ] <<- c(blender_name, model_w, model_l, model_p, model_u, model_k, model_e, model_x) }

  }

  View(pred_comb)
  View(btest_frame)
  ggplot(pred_comb, aes(x = PICKPAYOUT, y = GRADEPAY)) + geom_point()
}

#--------------K NEAREST NEIGHBORS------------------

kmca_set_file <- function(x) {
  f <- read_csv('pyvars\\master_reg_haneut.csv')
  k <- read_csv('kmca\\kmca.csv')
  
  a <- 1
  g <- max(k$CLUST)
  
  min_matrix <- matrix(0, nrow = g, ncol = 16)
  max_matrix <- matrix(0, nrow = g, ncol = 16)
  
  for (a in a:g) {
    
    filt_k <- k %>%
      filter(CLUST == a)
    
    loc_x <- filt_k$LOCX
    loc_y <- filt_k$LOCY
    
    frame_use <- f[, c(loc_x, loc_y)]
    
    b <- 1
    h <- ncol(frame_use)
    for (b in b:h) { 
      min_matrix[a, b] <- min(frame_use[, b])
      max_matrix[a, b] <- max(frame_use[, b])
      frame_use[, b] <- nor(frame_use[, b]) 
    }
    
    if (a == 1) { stat_list <- list(frame_use) }
    else { stat_list[[a]] <- frame_use }
  }

  info_frame <- f[, c(4, 6, 7, 10, 11, 12, 13, 45, 77, 78, 79, 80, 81, 82)]
  info_frame_low <- min(info_frame[, 8])
  info_frame_high <- max(info_frame[, 8])
  info_frame_range <- info_frame_high - info_frame_low

  info_frame <- info_frame %>%
    mutate(PCTILE.x = (ED.x - info_frame_low) / info_frame_range) %>%
    mutate(PCTILE.y = (ED.y - info_frame_low) / info_frame_range)

  write_csv(info_frame, 'kmca\\info_frame.csv')
  write_csv(as.data.frame(min_matrix), 'kmca\\min_matrix.csv')
  write_csv(as.data.frame(max_matrix), 'kmca\\max_matrix.csv')
  
  a <- 1
  g <- length(stat_list)
  for (a in a:g) {
    
    f_n <- paste("kmca\\kclust", a, ".csv", sep = "")
    write_csv(stat_list[[a]], f_n)
    
  }
}

kmca_stat_set <- function(x) {
  
  k <- read_csv('kmca\\kmca.csv')
  min_matrix <- read_csv('kmca\\min_matrix.csv')
  max_matrix <- read_csv('kmca\\max_matrix.csv')
  
  mc_away_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_away_id)
  
  mc_home_teamstats <<- master_team_adj_frame_wt %>%
    filter(TEAM_ID == mc_home_id)
  
  mc_away_teamstats <<- as.matrix(mc_away_teamstats[, 3:38])
  mc_home_teamstats <<- as.matrix(mc_home_teamstats[, 3:38])
  
  a <- 1
  g <- max(k$CLUST)
  
  kmca_matrix <- matrix(0, nrow = g, ncol = 20)
  
  for (a in a:g) {
    
    filt_k <- k %>%
      filter(CLUST == a)
    
    loc_mcts <- filt_k$LOCMC
    
    mc_a_split <- as.vector(mc_away_teamstats[1, loc_mcts])
    mc_h_split <- as.vector(mc_home_teamstats[1, loc_mcts])
    
    mc_a_neut <- mc_a_split
    mc_h_neut <- mc_h_split
    
    b <- 1
    h <- length(mc_a_split)
    
    for (b in b:h) {
      
      raw_stat <- as.numeric(mc_a_split[b])
      max_stat <- as.numeric(max_matrix[a, b])
      min_stat <- as.numeric(min_matrix[a, b])
      neut_stat <- (raw_stat - min_stat) / (max_stat - min_stat)
      mc_a_neut[b] <- neut_stat
      
      raw_stat <- as.numeric(mc_h_split[b])
      neut_stat <- (raw_stat - min_stat) / (max_stat - min_stat)
      mc_h_neut[b] <- neut_stat
      
    }
    
    a_start <- 1
    a_end <- length(mc_a_split)
    h_start <- a_end + 1
    h_end <- h_start + length(mc_h_split) - 1
    
    kmca_matrix[a, a_start:a_end] <- mc_a_neut
    kmca_matrix[a, h_start:h_end] <- mc_h_neut
    
  }
  kmca_clust_ass(kmca_matrix)
}

kmca_clust_ass <- function(kmca_matrix) {
  
  
  #MUST ADD HOME ADV NEXT SEASON
  k <- read_csv('kmca\\kmca.csv')
  info_frame <- read_csv('kmca\\info_frame.csv')
  
  a <- 1
  g <- max(k$CLUST)
  
  
  for (a in a:g) {
    
    f_n <- paste("kmca\\kclust", a, ".csv", sep = "")
    f <- read_csv(f_n)
    if (a == 1) { kclust_list <- list(f) }
    else { kclust_list[[a]] <- f }
    
  }
  
  master_ranks <- matrix(0, nrow = nrow(kclust_list[[a]]), ncol = g)
  
  a <- 1
  g <- max(k$CLUST)
  
  for (a in a:g) {
    
    filt_k <- k %>%
      filter(CLUST == a)
    
    no_of_stats <- nrow(filt_k) * 2
    
    knn_list_use <- kclust_list[[a]]
    
    kmatrix_line <- kmca_matrix[a, 1:no_of_stats]
    
    b <- 1
    h <- length(kmatrix_line)
    for (b in b:h) {
      
      col_use <- knn_list_use[, b]
      col_dif <- abs(col_use - kmatrix_line[b])
      if (b == 1) { kdif_matrix <- col_dif }
      else { kdif_matrix <- cbind(kdif_matrix, col_dif) }
      
    }
    
    kdif_matrix[, (no_of_stats + 1)] <- rowMeans(kdif_matrix)
    kdif_ranks <- rank(kdif_matrix[, (no_of_stats + 1)])
    master_ranks[, a] <- kdif_ranks
    
  }


  master_ranks_whole <- cbind(master_ranks, rowMeans(master_ranks))
  info_frame <- cbind(info_frame, master_ranks_whole)
  e_info_frame <- info_frame
  
  thresh <- as.numeric(k$THRESH[1])
  grouped_thresh <- k %>%
    select(CLUST, TIER) %>%
    group_by(CLUST) %>%
    summarize(TIER = max(TIER)) %>%
    filter(TIER == 1)
  
  thresh_vec <- as.vector(grouped_thresh$CLUST)
  thresh_vec <- thresh_vec + 16
  
  info_frame[, (1 + ncol(info_frame))] <- rowMeans(info_frame[, thresh_vec])
  colnames(info_frame)[ncol(info_frame)] <- "TIER1"
  
  info_frame <- info_frame %>%
    arrange(TIER1)
  
  info_frame <- info_frame[1:thresh, ]
  
  a <- 1
  g <- max(k$CLUST)
  
  for (a in a:g) {
    
    slot <- a + 16
    tmp_rank <- rank(info_frame[, slot])
    info_frame[, slot] <- tmp_rank
    
  }
  
  mstart <- 17
  mend <- (mstart + g) - 1
  master_ranks <- rowMeans(info_frame[, mstart:mend])
  info_frame[, (mend + 1)] <- master_ranks
  info_end <<- ncol(info_frame) + 1
  info_frame <<- info_frame[1:kmca_trim, ]
  kmca_avg_ascore <<- mean(info_frame$PF)
  kmca_avg_hscore <<- mean(info_frame$PA)
  kmca_avg_tscore <<- mean(info_frame$Total)
  kmca_avg_awinpct <<- mean(info_frame$WinNeut)
  kmca_avg_hwinpct <<- 1 - kmca_avg_awinpct
  
}

kmca_loop <- function(x) {
  
  kmca_roster <- read_csv('kmca\\kmca_roster.csv')
  kmca_trim <<- x
  a <- 1
  g <- nrow(kmca_roster)
  for (a in a:g) {
    
    kmid <- as.character(kmca_roster[a, 1])
    tma <- as.character(kmca_roster[a, 2])
    tmb <- as.character(kmca_roster[a, 3])
    
    tmaid <- final_stats_frame$TEAM_ID[which(final_stats_frame$TEAM_NAME == tma)]
    tmbid <- final_stats_frame$TEAM_ID[which(final_stats_frame$TEAM_NAME == tmb)]
    mc_away_id <<- tmaid
    mc_home_id <<- tmbid
    mc_loc <- "N"
    
    info_frame <<- NULL
    kmca_stat_set(1)
    info_frame[, info_end] <<- kmid
    out_vector <- c(kmid, kmca_avg_ascore, kmca_avg_hscore, kmca_avg_tscore, kmca_avg_awinpct, kmca_avg_hwinpct)
    if (a == 1) { 
      out_frame <- matrix(as.numeric(out_vector), nrow = 1, ncol = 6) 
      top10_frame <- info_frame[1:10, ]
      top50_frame <- info_frame[1:50, ]
      
    }
    else {
      out_frame2 <- matrix(as.numeric(out_vector), nrow = 1, ncol = 6)
      out_frame <- rbind(out_frame, out_frame2)
      top10_frame <- rbind(top10_frame, info_frame[1:10, ])
      top50_frame <- rbind(top50_frame, info_frame[1:50, ])
    }
  }
  View(info_frame)
  top10_frame <- top10_frame[, c(1, 2, 3, 4, 5, 6, 10, 11, 14, 15, 16, 25)]
  top50_frame <- top50_frame[, c(1, 2, 3, 4, 5, 6, 10, 11, 14, 15, 16, 25)]

  a <- 1
  g <- nrow(top10_frame)
  for (a in a:g) {
    
    tma <- as.character(top10_frame[a, 1])
    tmb <- as.character(top10_frame[a, 2])
    loca <- as.character(top10_frame[a, 4])
    dt <- as.character(top10_frame[a, 3])
    
    if (loca == "N") { 
      desc_use <- paste(dt, "-", tma, "vsn.", tmb, sep = " ")
    }
    else if (loca == "A") {
      desc_use <- paste(dt, "-", tma, "@", tmb, sep = " ")
    }
    else {
      desc_use <- paste(dt, "-", tma, "vs.", tmb, sep = " ")
    }
    top10_frame[a, 13] <- desc_use
    
  }
  top10_frame <- top10_frame[, c(12:13, 1:11)]
  write_csv(top10_frame, 'kmca\\top10frame.csv')
  
  a <- 1
  g <- nrow(top50_frame)
  for (a in a:g) {
    
    tma <- as.character(top50_frame[a, 1])
    tmb <- as.character(top50_frame[a, 2])
    loca <- as.character(top50_frame[a, 4])
    dt <- as.character(top50_frame[a, 3])
    
    if (loca == "N") { 
      desc_use <- paste(dt, "-", tma, "vsn.", tmb, sep = " ")
    }
    else if (loca == "A") {
      desc_use <- paste(dt, "-", tma, "@", tmb, sep = " ")
    }
    else {
      desc_use <- paste(dt, "-", tma, "vs.", tmb, sep = " ")
    }
    top50_frame[a, 13] <- desc_use
    
  }
  top50_frame <- top50_frame[, c(12:13, 1:11)]
  write_csv(top50_frame, 'kmca\\top50frame.csv')
  
  out_frame <- as.data.frame(out_frame)
  write_csv(out_frame, 'kmca\\top50results.csv')
}
    
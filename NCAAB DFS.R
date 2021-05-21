#This file runs an optimizer to determine the ideal daily fantasy lineups for college basketball games.
#It cycles through all possible combinations of a lineup and returns the highest average expected value.

dfs_showdown <- function(ccc, uuu) {
  
  sd_combine(1)
  sd_pos_split(1)
  sd_optimize_2(ccc, uuu)
  
}

dfs_classic <- function(ggg, fff, uuu) {
  
  dfs_combine(1)
  dfs_pos_split(1)
  cl_optimize_2(ggg, fff, uuu)
  
}

dfs_name_fix <- function(xxxx) {
  
  dfs_frame2 <<- dfs_frame
  name_fix <<- as.matrix(dfs_frame[, 2])
  name_fix2 <<- sapply(name_fix, name_switch)
  dfs_frame2[, 2] <<- name_fix2
  dfs_frame2[, 2] <<- trim.all(dfs_frame2[, 2])
    
}

tmp_func <- function(x) {
  dfs_frame2[, 2] <<- trim.all(dfs_frame2[, 2])
  
}

name_switch <- function(x) {
  
  c <- strsplit(x, ",")
  fn <- substring(c[[1]][2], 2, 100)
  ln <- c[[1]][1]
  return(paste(fn, ln, sep = " "))
  
}

#Regular

dfs_combine <- function(xxxx) {
  
  dk_list <<- read_csv("DKSalaries.csv") %>%
    filter(`Game Info` != "Postponed")
  
  dk_list <<- dk_list[, c(3, 5, 6, 8)]
  
  dfs_cure <<- read_excel("DFSCure.xlsx")
  
  a <- 1
  g <- nrow(dfs_cure)
  
  for (a in a:g) {
    
    cure_check <<- trim.all(as.character(dfs_cure[a, 1]))
    cure_list <<- which(dk_list$Name == cure_check)
    
    
    if (length(cure_list) > 0) {
    
      act_name <<- as.character(dfs_cure[a, 2]) 
      print(act_name)
      dk_list[cure_list, 1] <<- act_name
      
    }
  }
  
  dk_list_comb <<- dk_list %>%
    left_join(dfs_frame2, by = c("Name" = "PLAYER_NAME"))
  
}

dfs_pos_split <- function(xxxx) {
  
  dk_list_comb <<- dk_list_comb[, -4]
  
  #Guards
  
  guards_grep <<- dk_list_comb[grepl("G", dk_list_comb$`Roster Position`), ] %>%
    arrange(desc(DFS))
  
  #Forwards
  
  forwards_grep <<- dk_list_comb[grepl("F", dk_list_comb$`Roster Position`), ] %>%
    arrange(desc(DFS))
  
  util_grep <<- dk_list_comb %>%
    arrange(desc(DFS))
  
}

          
#Showdown


sd_combine <- function(xxxx) {
  
  dk_list <<- read_csv("DKSalaries.csv") %>%
    filter(`Game Info` != "Postponed")
  
  dk_list <<- dk_list[, c(3, 5, 6)]
  
  dfs_cure <<- read_excel("DFSCure.xlsx")
  
  a <- 1
  g <- nrow(dfs_cure)
  
  for (a in a:g) {
    
    cure_check <<- trim.all(as.character(dfs_cure[a, 1]))
    cure_list <<- which(dk_list$Name == cure_check)
    
    
    if (length(cure_list) > 0) {
      
      act_name <<- as.character(dfs_cure[a, 2]) 
      print(act_name)
      dk_list[cure_list, 1] <<- act_name
      
    }
  }
  
  dk_list_comb <<- dk_list %>%
    left_join(dfs_frame2, by = c("Name" = "PLAYER_NAME"))
  
}

sd_pos_split <- function(xxxx) {
  
  #Guards
  
  cpt_grep <<- dk_list_comb[grepl("CPT", dk_list_comb$`Roster Position`), ] %>%
    arrange(desc(DFS))
  
  cpt_grep[, 6] <<- cpt_grep[, 6] * 1.5
  
  #Forwards
  
  util_grep <<- dk_list_comb[grepl("UTIL", dk_list_comb$`Roster Position`), ] %>%
    arrange(desc(DFS))
  
}

sd_optimize_2 <- function(ct, ut) {
  
  cpt_thresh <<- ct
  util_thresh <<- ut
  
  c_dfs_cutoff <<- ct
  u_dfs_cutoff <<- ut
  dfs_ct <<- 0
  
  cpt_dfs <<- cpt_grep %>%
    filter(is.na(DFS) == FALSE)
  
  util_dfs <<- util_grep %>%
    filter(is.na(DFS) == FALSE)
  
  if (nrow(cpt_dfs) < c_dfs_cutoff) { c_dfs_cutoff <<- nrow(cpt_dfs) }
  if (nrow(util_dfs) < u_dfs_cutoff) { u_dfs_cutoff <<- nrow(util_dfs) }
  
  cpt_dfs <<- cpt_dfs[1:c_dfs_cutoff, ]
  util_dfs <<- util_dfs[1:u_dfs_cutoff, ]
  
  cpt_mx <<- as.matrix(cpt_dfs[, c(3, 6, 4)])
  util_mx <<- as.matrix(util_dfs[, c(3, 6, 4)])
  
  dfs_high <<- -999
  dfs_low_sal <<- 9999999
  
  opt_vector_max <<- c(cpt_thresh, (util_thresh - 4):util_thresh)
  opt_vector_start <<- c(1, 1, 2, 3, 4, 4)
  opt_vector_curr <<- opt_vector_start
  
  opt_vec_max_1 <<- opt_vector_max[1]
  opt_vec_max_2 <<- opt_vector_max[2]
  opt_vec_max_3 <<- opt_vector_max[3]
  opt_vec_max_4 <<- opt_vector_max[4]
  opt_vec_max_5 <<- opt_vector_max[5]
  opt_vec_max_6 <<- opt_vector_max[6]
  ct <<- 0
  
  opt_vector_check_1 <<- 0
  a <- 1
  while (opt_vector_check_1 == 0) {
   
    opt_vector_1 <<- opt_vector_curr[1]
    opt_vector_2 <<- opt_vector_curr[2]
    opt_vector_3 <<- opt_vector_curr[3]
    opt_vector_4 <<- opt_vector_curr[4]
    opt_vector_5 <<- opt_vector_curr[5]
    opt_vector_6 <<- opt_vector_curr[6]
    
    opt_vector_calc_6 <<- opt_vector_6 + 1
    if (opt_vector_calc_6 > opt_vec_max_6) {
      
      opt_vector_calc_5 <<- opt_vector_5 + 1
      if (opt_vector_calc_5 > opt_vec_max_5) {
        
        opt_vector_calc_4 <<- opt_vector_4 + 1
        if (opt_vector_calc_4 > opt_vec_max_4) {
          
          opt_vector_calc_3 <<- opt_vector_3 + 1
          if (opt_vector_calc_3 > opt_vec_max_3) {
            
            opt_vector_calc_2 <<- opt_vector_2 + 1
            if (opt_vector_calc_2 > opt_vec_max_2) {
              
              opt_vector_calc_1 <<- opt_vector_1 + 1
              if (opt_vector_calc_1 > opt_vec_max_1) {
                
                opt_vector_check_1 <<- 1
                
              }
              else {
                
                opt_vector_calc_2 <<- opt_vector_start[2]
                opt_vector_calc_3 <<- opt_vector_start[3]
                opt_vector_calc_4 <<- opt_vector_start[4]
                opt_vector_calc_5 <<- opt_vector_start[5]
                opt_vector_calc_6 <<- opt_vector_start[6] + 1
                opt_vector_curr <<- c(opt_vector_calc_1, opt_vector_calc_2, opt_vector_calc_3,
                      opt_vector_calc_4, opt_vector_calc_5, opt_vector_calc_6)
                
              }
              
              
            }
            else {
              opt_vector_calc_3 <<- opt_vector_calc_2 + 1
              opt_vector_calc_4 <<- opt_vector_calc_3 + 1
              opt_vector_calc_5 <<- opt_vector_calc_4 + 1
              opt_vector_calc_6 <<- opt_vector_calc_5 + 1
              opt_vector_curr[2:6] <<- c(opt_vector_calc_2, opt_vector_calc_3,
                      opt_vector_calc_4, opt_vector_calc_5, opt_vector_calc_6)
              
              
            }
            
            
          }
          else {
            opt_vector_calc_4 <<- opt_vector_calc_3 + 1
            opt_vector_calc_5 <<- opt_vector_calc_4 + 1
            opt_vector_calc_6 <<- opt_vector_calc_5 + 1
            opt_vector_curr[3:6] <<- c(opt_vector_calc_3,
              opt_vector_calc_4, opt_vector_calc_5, opt_vector_calc_6)
            
          }
          
          
        }
        else {
          opt_vector_calc_5 <<- opt_vector_calc_4 + 1
          opt_vector_calc_6 <<- opt_vector_calc_5 + 1
          opt_vector_curr[4:6] <<- c(opt_vector_calc_4, opt_vector_calc_5, opt_vector_calc_6)
        }
      }
      else {
        
        opt_vector_calc_6 <<- opt_vector_calc_5 + 1
        opt_vector_curr[5:6] <<- c(opt_vector_calc_5, opt_vector_calc_6)
        
      }
      
    }
    else {
      
      opt_vector_curr[6] <<- opt_vector_calc_6
      
    }
    
    if (opt_vector_check_1 == 0) {
      
      curr_vec_1 <<- opt_vector_curr[1]
      curr_vec_2 <<- opt_vector_curr[2]
      curr_vec_3 <<- opt_vector_curr[3]
      curr_vec_4 <<- opt_vector_curr[4]
      curr_vec_5 <<- opt_vector_curr[5]
      curr_vec_6 <<- opt_vector_curr[6]
      
      cpt_id <<- cpt_mx[curr_vec_1, 3]
      util_vec <<- c(util_mx[curr_vec_2, 3], util_mx[curr_vec_3, 3], util_mx[curr_vec_4, 3],
                     util_mx[curr_vec_5, 3], util_mx[curr_vec_6, 3])
      which_check <<- which(util_vec == cpt_id)
      if (length(which_check) == 0) { 
      
        cpt_sal <<- cpt_mx[curr_vec_1, 1]
        util_sal <<- c(util_mx[curr_vec_2, 1], util_mx[curr_vec_3, 1], util_mx[curr_vec_4, 1],
                       util_mx[curr_vec_5, 1], util_mx[curr_vec_6, 1])
        sal_check <<- sum(util_sal)
        sal_check <<- cpt_sal + sal_check
        
        if (sal_check <= 50000) {
          
          cpt_dfs_ <<- cpt_mx[curr_vec_1, 2]
          util_dfs_ <<- c(util_mx[curr_vec_2, 2], util_mx[curr_vec_3, 2], util_mx[curr_vec_4, 2],
                         util_mx[curr_vec_5, 2], util_mx[curr_vec_6, 2])
          dfs_check <<- sum(util_dfs_)
          dfs_check <<- cpt_dfs_ + dfs_check
          
          if (dfs_check > dfs_high) {
            
            dfs_high <<- dfs_check
            dfs_use_vector <<- opt_vector_curr
            
          }
        }
      }
    }
  }
  
  dfs_showdown_frame <<- rbind(cpt_dfs[dfs_use_vector[1], ],
                               util_dfs[dfs_use_vector[2], ],
                               util_dfs[dfs_use_vector[3], ],
                               util_dfs[dfs_use_vector[4], ],
                               util_dfs[dfs_use_vector[5], ],
                               util_dfs[dfs_use_vector[6], ])
  
  dfs_showdown_frame
}

cl_optimize_2 <- function(ggg, fff, uuu) {
  
  guard_cutoff <<- ggg
  forward_cutoff <<- fff
  util_cutoff <<- uuu
  dfs_ct <<- 0
  
  guard_vector_max <<- c(guard_cutoff - 2, guard_cutoff - 1, guard_cutoff)
  forward_vector_max <<- c(forward_cutoff - 2, forward_cutoff - 1, forward_cutoff)
  
  guards_dfs <<- guards_grep %>%
    filter(is.na(DFS) == FALSE)
  
  forwards_dfs <<- forwards_grep %>%
    filter(is.na(DFS) == FALSE)
  
  util_dfs <<- util_grep %>%
    filter(is.na(DFS) == FALSE)
  
  if (nrow(guards_dfs) < guard_cutoff) { guard_cutoff <<- nrow(guards_dfs) }
  if (nrow(forwards_dfs) < forward_cutoff) { forward_cutoff <<- nrow(forwards_dfs) }
  if (nrow(util_dfs) < util_cutoff) { util_cutoff <<- nrow(util_dfs) }
  
  guards_dfs <<- guards_dfs[1:guard_cutoff, ]
  forwards_dfs <<- forwards_dfs[1:forward_cutoff, ]
  util_dfs <<- util_dfs[1:util_cutoff, ]
  
  guards_mx <<- as.matrix(guards_dfs[, c(3, 6, 4)])
  forwards_mx <<- as.matrix(forwards_dfs[, c(3, 6, 4)])
  util_mx <<- as.matrix(util_dfs[, c(3, 6, 4)])
  
  gvec_max1 <<- guard_vector_max[1]
  gvec_max2 <<- guard_vector_max[2]
  gvec_max3 <<- guard_vector_max[3]
  
  fvec_max1 <<- forward_vector_max[1]
  fvec_max2 <<- forward_vector_max[2]
  fvec_max3 <<- forward_vector_max[3]
  
  opt_vector_g_curr <<- c(1, 2, 3)
  opt_vector_f_curr <<- c(1, 2, 2)
  
  opt_vec_g_start <<- opt_vector_g_curr
  opt_vec_f_start <<- c(1, 2, 3)
  
  opt_vector_check <<- 0
  
  dfs_high <<- -9999
  
  while (opt_vector_check == 0) {
  
     curr_g1 <<- opt_vector_g_curr[1]
     curr_g2 <<- opt_vector_g_curr[2]
     curr_g3 <<- opt_vector_g_curr[3]
     
     curr_f1 <<- opt_vector_f_curr[1]
     curr_f2 <<- opt_vector_f_curr[2]
     curr_f3 <<- opt_vector_f_curr[3]
     
     opt_calc_f3 <<- curr_f3 + 1
     if (opt_calc_f3 > fvec_max3) {
       
       opt_calc_f2 <<- curr_f2 + 1
       if (opt_calc_f2 > fvec_max2) {
         
         opt_calc_f1 <<- curr_f1 + 1
         if (opt_calc_f1 > fvec_max1) {
           
           opt_calc_g3 <<- curr_g3 + 1
           if (opt_calc_g3 > gvec_max3) {
             
              opt_calc_g2 <<- curr_g2 + 1
              if (opt_calc_g2 > gvec_max2) {
                
                opt_calc_g1 <<- curr_g1 + 1
                if (opt_calc_g1 > gvec_max1) {
                  
                  opt_vector_check <<- 1
                  
                }
                else {
                  
                  opt_vector_f_curr[1] <<- opt_vec_f_start[1]
                  opt_vector_f_curr[2] <<- opt_vec_f_start[2]
                  opt_vector_f_curr[3] <<- opt_vec_f_start[3]
                  opt_vector_g_curr[3] <<- opt_calc_g1 + 2
                  opt_vector_g_curr[2] <<- opt_calc_g1 + 1
                  opt_vector_g_curr[1] <<- opt_calc_g1
                  
                  
                }
                
              }
              else {
                
                opt_vector_f_curr[1] <<- opt_vec_f_start[1]
                opt_vector_f_curr[2] <<- opt_vec_f_start[2]
                opt_vector_f_curr[3] <<- opt_vec_f_start[3]
                opt_vector_g_curr[3] <<- opt_calc_g2 + 1
                opt_vector_g_curr[2] <<- opt_calc_g2
                
              }
           }
           else {
             
              opt_vector_f_curr[1] <<- opt_vec_f_start[1]
              opt_vector_f_curr[2] <<- opt_vec_f_start[2]
              opt_vector_f_curr[3] <<- opt_vec_f_start[3]
              opt_vector_g_curr[3] <<- opt_calc_g3
             
             
           }
         }
         
         else {
           
           opt_calc_f2 <<- opt_calc_f1 + 1
           opt_calc_f3 <<- opt_calc_f2 + 1
           opt_vector_f_curr <<- c(opt_calc_f1, opt_calc_f2, opt_calc_f3)
           
         }
       }
       
       else {
         
         opt_calc_f3 <<- opt_calc_f2 + 1
         opt_vector_f_curr[2:3] <<- c(opt_calc_f2, opt_calc_f3)
         
       }
     }
     else {
      
      opt_vector_f_curr[3] <<- opt_calc_f3 
       
     }
     
     #Separator------------------------
     
     if (opt_vector_check == 0) {
       
       #print(paste(opt_vector_g_curr[1], opt_vector_g_curr[2], opt_vector_g_curr[3],
                   #opt_vector_f_curr[1], opt_vector_f_curr[2], opt_vector_f_curr[3]))
       
       guard_ids <<- guards_mx[opt_vector_g_curr, 3]
       forward_ids <<- forwards_mx[opt_vector_f_curr, 3]
       comb_id <<- c(guard_ids, forward_ids)
       
       dup_check <<- which(duplicated(comb_id) == TRUE)
       if (length(dup_check) > 0) { dup_roaro <<- 1 }
       else { dup_roaro <<- 0 }
       
       util_leftover_mx <<- which(util_mx[, 3] != comb_id[1] &
                                  util_mx[, 3] != comb_id[2] &
                                    util_mx[, 3] != comb_id[3] &
                                    util_mx[, 3] != comb_id[4] &
                                    util_mx[, 3] != comb_id[5] &
                                    util_mx[, 3] != comb_id[6])
       
       util_leftover_mx <<- util_mx[util_leftover_mx, ]
       util_lo_nrow <<- nrow(util_leftover_mx)
       
       opt_vector_u_max <<- c(util_lo_nrow - 1, util_lo_nrow)
       uvec_max1 <<- opt_vector_u_max[1]
       uvec_max2 <<- opt_vector_u_max[2]
       
       opt_vector_u_curr <<- c(1, 1)
       
       util_checker <<- 0
       while (util_checker == 0) {
         
         curr_u1 <<- opt_vector_u_curr[1]
         curr_u2 <<- opt_vector_u_curr[2]
         
         opt_calc_u2 <<- curr_u2 + 1
         if (opt_calc_u2 > uvec_max2) {
           
           opt_calc_u1 <<- curr_u1 + 1
           
           if (opt_calc_u1 > uvec_max1) {
             
             util_checker <<- 1
             
           }
           else {
             
             opt_vector_u_curr[1] <<- opt_calc_u1
             opt_vector_u_curr[2] <<- opt_calc_u1 + 1
             
           }
         }
         else {
          
          opt_vector_u_curr[2] <<- opt_calc_u2
            
         }
         
         dfs_ct <<- dfs_ct + 1
         
         guard_sal_vector <<- sum(guards_mx[opt_vector_g_curr, 1])
         forward_sal_vector <<- sum(forwards_mx[opt_vector_f_curr, 1])
         util_sal_vector <<- sum(util_leftover_mx[opt_vector_u_curr, 1])
         cume_sal <<- (guard_sal_vector + forward_sal_vector + util_sal_vector)
         if (dup_roaro == 1) { cume_sal <<- 99999 }
         if (cume_sal <= 50000) {
          
           guard_pts_vector <<- sum(guards_mx[opt_vector_g_curr, 2])
           forward_pts_vector <<- sum(forwards_mx[opt_vector_f_curr, 2])
           util_pts_vector <<- sum(util_leftover_mx[opt_vector_u_curr, 2])
           cume_pts <<- (guard_pts_vector + forward_pts_vector + util_pts_vector)
           
           if (cume_pts > dfs_high) {
             
             dfs_high <<- cume_pts
             print(dfs_high)
             print(cume_sal)
             dfs_use_vector <<- c(opt_vector_g_curr, opt_vector_f_curr, opt_vector_u_curr)
             comb_id_use <<- comb_id
             umx_debug <<- util_leftover_mx
             guard_sal_debug <<- guard_sal_vector
             forward_sal_debug <<- forward_sal_vector
             util_sal_debug <<- util_sal_vector
             
           }
         }
      }
    }
  }

  if (dfs_high > 0) {
    
    util_id1 <<- util_dfs[which(util_dfs$ID == umx_debug[dfs_use_vector[7], 3]), ]
    util_id2 <<- util_dfs[which(util_dfs$ID == umx_debug[dfs_use_vector[8], 3]), ]
    
    dfs_classic_frame <<- rbind(guards_dfs[dfs_use_vector[1], ],
                                 guards_dfs[dfs_use_vector[2], ],
                                 guards_dfs[dfs_use_vector[3], ],
                                 forwards_dfs[dfs_use_vector[4], ],
                                 forwards_dfs[dfs_use_vector[5], ],
                                 forwards_dfs[dfs_use_vector[6], ],
                                 util_id1,
                                util_id2)
    
    dfs_classic_frame
  }
  else { 
    print("No lineup found under salary cap.")  
  }
}

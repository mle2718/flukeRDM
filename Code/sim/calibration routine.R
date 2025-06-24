
#This file pulls in the data from step 1, i.e., the differences between model simulated harvest 
#and MRIP estimates of harvest, and re-runs the calibration model but this time adjusts per-trip
#outcomes until simulated harvest in numbers of fish is within 5% or 500 fish of the MRIP estimate. 

input_data_cd=here("Data")
test_data_cd=here("Data", "Test_data")
code_cd=here("Code", "sim")
#output_data_cd=here("lou_files","cod_haddock","output_data")
iterative_input_data_cd="C:/Users/andrew.carr-harris/Desktop/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"
#Set number of original draws. We use 150 for the final run. Choose a lot fewer for test runs
n_simulations<-100

n_draws<-50 #Number of simulated trips per day


MRIP_comparison = read_dta(file.path(iterative_input_data_cd,"simulated_catch_totals.dta")) %>% 
  dplyr::rename(estimated_trips=tot_dtrip_sim, 
                sf_catch=tot_sf_cat_sim, 
                bsb_catch=tot_bsb_cat_sim, 
                scup_catch=tot_scup_cat_sim, 
                sf_keep=tot_sf_keep_sim, 
                bsb_keep=tot_bsb_keep_sim, 
                scup_keep=tot_scup_keep_sim,
                sf_rel=tot_sf_rel_sim, 
                bsb_rel=tot_bsb_rel_sim, 
                scup_rel=tot_scup_rel_sim)

baseline_output0<-feather::read_feather(file.path(iterative_input_data_cd, "calibration_comparison.feather")) 




states <- c("MA", "RI")
mode_draw <- c("sh", "pr", "fh")
draws <- 1:2

 i<-1
 s<-"MA"
 md<-"pr"
# Create an empty list to collect results
calib_comparison <- list()

# Counter for appending to list
k <- 1

# Loop over all combinations
# for (s in states) {
#   for (md in mode_draw) {
#     for (i in draws) {
      
      calib_comparison<-feather::read_feather(file.path(iterative_input_data_cd, "calibration_comparison.feather")) %>% 
        dplyr::filter(state==s & draw==i & mode==md)
      

      
      for (p in 1:nrow(calib_comparison)) {
        sp <- calib_comparison$species[p]
        
        assign(paste0("rel_to_keep_", sp), calib_comparison$rel_to_keep[p])
        assign(paste0("keep_to_rel_", sp), calib_comparison$keep_to_rel[p])
        
        
        if (calib_comparison$rel_to_keep[p] == 1) {
          assign(paste0("p_rel_to_keep_", sp), calib_comparison$p_rel_to_keep[p])
          assign(paste0("p_keep_to_rel_", sp), 0)
          
        }
        
        if (calib_comparison$keep_to_rel[p] == 1) {
          assign(paste0("p_keep_to_rel_", sp), calib_comparison$p_keep_to_rel[p])
          assign(paste0("p_rel_to_keep_", sp), 0)
          
        }
      }
      
      MRIP_comparison_draw <- MRIP_comparison %>% 
        dplyr::filter(draw==i & state==s) %>% 
        dplyr::filter(mode==md)
      
      mode_val <- MRIP_comparison_draw$mode
      
      # Loop over summary columns
      for (var in names(MRIP_comparison_draw)[names(MRIP_comparison_draw) != "mode"]) {
        value <- MRIP_comparison_draw[[var]]
        obj_name <- paste0(var, "_", mode_val, "_MRIP")
        assign(obj_name, value)
      }
      
      
      species <- c("sf", "bsb", "scup")
      dispositions <- c("keep", "rel", "catch")
      
      compare <- data.frame()
      
      for (sp in species) {
        for (disp in dispositions) {
          
          # Construct variable names
          base_name <- paste(sp, disp, md, sep = "_")
          mrip_var <- paste0(base_name, "_MRIP")
          model_var <- paste0(base_name, "_model")
          
          # Check if both variables exist
          if (exists(mrip_var) && exists(model_var)) {
            # Retrieve values
            mrip_val <- get(mrip_var)
            model_val <- get(model_var)
            
            # Calculate differences
            diff_val <- model_val - mrip_val
            pct_diff_val <- if (mrip_val != 0)  (diff_val / mrip_val) * 100 else NA
            abs_diff_val <- abs(model_val - mrip_val)
            abs_pct_diff_val <- if (mrip_val != 0)  abs((diff_val / mrip_val) * 100) else NA
            
            # Create output variable names
            assign(paste0(base_name, "_diff"), diff_val)
            assign(paste0(base_name, "_pctdiff"), pct_diff_val)
            assign(paste0(base_name, "_abs_diff"), abs_diff_val)
            assign(paste0(base_name, "_abs_pctdiff"), abs_pct_diff_val)
            
            compare <- rbind(compare, data.frame(
              species = sp,
              disposition = disp,
              mode = md,
              MRIP = mrip_val,
              model = model_val,
              diff = diff_val,
              pct_diff = pct_diff_val, 
              abs_diff_val= abs_diff_val, 
              abs_pct_diff_val= abs_pct_diff_val
            ))
          } 
          
          else {
            warning(paste("Missing variable:", mrip_var, "or", model_var))
            
            
          }
        }
      }
      
      base_sf_achieved<-case_when((harv_diff_sf<500 | harv_pct_diff_sf<5)~1, TRUE~0)
      base_bsb_achieved<-case_when((harv_diff_bsb<500 | harv_pct_diff_bsb<5)~1, TRUE~0)
      base_scup_achieved<-case_when((harv_diff_scup<500 | harv_pct_diff_scup<5)~1, TRUE~0)
      
      sf_achieved<-case_when(base_sf_achieved==1~1, TRUE~0)
      bsb_achieved<-case_when(base_bsb_achieved==1~1, TRUE~0)
      scup_achieved<-case_when(base_scup_achieved==1~1, TRUE~0)
      
      if(base_sf_achieved==1  & base_bsb_achieved==1 & base_scup_achieved==1) break
      if(base_sf_achieved!=1  | base_bsb_achieved!=1 | base_scup_achieved!=1) {
        
        
      source(file.path(code_cd, "calibrate_rec_catch1.R"))
      
        message("run ", i)
        message("model_sf_harv: ", sf_keep_model)
        message("mrip_sf_harv: ", sf_keep_MRIP)
        message("diff_sf_harv: ", sf_keep_diff)
        message("pct_diff_sf_harv: ", sf_keep_pct_diff)
        message("rel_to_keep_sf: ", rel_to_keep_sf)
        message("p_rel_to_keep_sf: ", p_rel_to_keep_sf)
        message("p_keep_to_rel_sf: ", p_keep_to_rel_sf)
        
        message("model_bsb_harv: ", bsb_keep_model)
        message("mrip_bsb_harv: ", bsb_keep_MRIP)
        message("diff_bsb_harv: ", bsb_keep_diff)
        message("pct_diff_bsb_harv: ", bsb_keep_pct_diff)
        message("rel_to_keep_bsb: ", rel_to_keep_bsb)
        message("p_rel_to_keep_bsb: ", p_rel_to_keep_bsb)
        message("p_keep_to_rel_bsb: ", p_keep_to_rel_bsb)
        
        message("model_scup_harv: ", scup_keep_model)
        message("mrip_scup_harv: ", scup_keep_MRIP)
        message("diff_scup_harv: ", scup_keep_diff)
        message("pct_diff_scup_harv: ", scup_keep_pct_diff)
        message("rel_to_keep_scup: ", rel_to_keep_scup)
        message("p_rel_to_keep_scup: ", p_rel_to_keep_scup)
        message("p_keep_to_rel_scup: ", p_keep_to_rel_scup)
        
        
        repeat{
          
          #For draws where release_to_keep==1:
            #If baseline sf harvest is less than MRIP, but in a new run sf harvest is greater than MRIP, 
            #reduce the baseline p_rel_to_keep value 
          
          if(rel_to_keep_sf==1 & sf_achieved!=1) {
            
            if(sf_keep_diff>0){# & sf_keep_pct_diff>10){
              p_rel_to_keep_sf<-p_rel_to_keep_sf -p_rel_to_keep_sf*.05
            }
            
            
            # if(sf_keep_diff>0 & sf_keep_pct_diff<= 10){
            #   p_rel_to_keep_sf<-p_rel_to_keep_sf -.025
            # }
            
            
          #If baseline sf harvest is less than MRIP, and in the new run sf harvest is still less than MRIP, 
          #increase the baseline h_star_release_to_keep value 
            if(sf_keep_diff<0) {#& sf_keep_pct_diff< -10){
              p_rel_to_keep_sf<-p_rel_to_keep_sf +p_rel_to_keep_sf*.06
            }
            
            # if(sf_keep_diff<0 & sf_keep_pct_diff>= -10){
            #   p_rel_to_keep_sf<-p_rel_to_keep_sf +.03
            # }
          }
          
          
          #same for bsb
          #If baseline harvest is less than MRIP, but in  a new run sf harvest is greater than MRIP, 
          #reduce the baseline h_star_release_to_keep value 
          
          if(rel_to_keep_bsb==1 & bsb_achieved!=1) {
            
            if(bsb_keep_diff>0){ #& bsb_keep_pct_diff> 10){
              p_rel_to_keep_bsb<-p_rel_to_keep_bsb-p_rel_to_keep_bsb*.05
            }
            
            
            # if(bsb_keep_diff>0 & bsb_keep_pct_diff<= 10){
            #   p_rel_to_keep_bsb<-p_rel_to_keep_bsb -.025
            # }
            # 
            
            #If baseline bsb harvest is less than MRIP, and in the new run bsb harvest is still less than MRIP, 
            #increase the baseline h_star_release_to_keep value 
            if(bsb_keep_diff<0){ #& bsb_keep_pct_diff< -10){
              p_rel_to_keep_bsb<-p_rel_to_keep_bsb +p_rel_to_keep_bsb*.06
            }
            
            # if(bsb_keep_diff<0 & bsb_keep_pct_diff>= -10){
            #   p_rel_to_keep_bsb<-p_rel_to_keep_bsb +.03
            # }
          }
          
          #same for scup
          #If baseline sf harvest is less than MRIP, but in  a new run sf harvest is greater than MRIP, 
          #reduce the baseline h_star_release_to_keep value 
          
          if(rel_to_keep_scup==1 & scup_achieved!=1) {
            
            if(scup_keep_diff>0){# & scup_keep_pct_diff> 10){
              p_rel_to_keep_scup<-p_rel_to_keep_scup -p_rel_to_keep_scup*.05
            }
            
            
            # if(scup_keep_diff>0 & scup_keep_pct_diff<= 10){
            #   p_rel_to_keep_scup<-p_rel_to_keep_scup -.025
            # }
            
            
            #If baseline scup harvest is less than MRIP, and in the new run scup harvest is still less than MRIP, 
            #increase the baseline h_star_release_to_keep value 
            if(scup_keep_diff<0){# & scup_keep_pct_diff< -10){
              p_rel_to_keep_scup<-p_rel_to_keep_scup +p_rel_to_keep_scup*.06
            }
            
            # if(scup_keep_diff<0 & scup_keep_pct_diff>= -10){
            #   p_rel_to_keep_scup<-p_rel_to_keep_scup +.03
            # }
          }
          
          
       
          
          #For draws where keep_to_release==1
          #If in the baseline run, harvest is less than MRIP, but in a new run harvest is greater than MRIP, 
          #reduce the baseline h_star_release_to_keep value 
          
          if(keep_to_rel_sf==1 & sf_achieved!=1 & p_keep_to_rel_sf!=1) {
            
            if(sf_keep_diff>0){# & comparison$perc_diff_cod_harv> 10){
              p_keep_to_rel_sf<-p_keep_to_rel_sf +p_keep_to_rel_sf*.02
            }
            
            # if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv<= 10){
            #   p_cod_kp_2_rl<-p_cod_kp_2_rl +.005
            # }
            
            #If in the baseline run, harvest is less than MRIP, and in the new run harvest is still less than MRIP, 
            #increase the baseline h_star_release_to_keep value 
            if(sf_keep_diff<0){
              p_keep_to_rel_sf<-p_keep_to_rel_sf -p_keep_to_rel_sf*.01
            }
          }
          
          
          #same for bsb
            if(keep_to_rel_bsb==1 & bsb_achieved!=1 & p_keep_to_rel_bsb!=1) {
              
              if(bsb_keep_diff>0){# & comparison$perc_diff_cod_harv> 10){
                p_keep_to_rel_bsb<-p_keep_to_rel_bsb +p_keep_to_rel_bsb*.02
              }
              
              # if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv<= 10){
              #   p_cod_kp_2_rl<-p_cod_kp_2_rl +.005
              # }
              
              #If in the baseline run, harvest is less than MRIP, and in the new run harvest is still less than MRIP, 
              #increase the baseline h_star_release_to_keep value 
              if(bsb_keep_diff<0){
                p_keep_to_rel_bsb<-p_keep_to_rel_bsb -p_keep_to_rel_bsb*.01
              }
            }
          
            #same for scup
            if(keep_to_rel_scup==1 & scup_achieved!=1 & p_keep_to_rel_scup!=1) {
              
              if(scup_keep_diff>0){# & comparison$perc_diff_cod_harv> 10){
                p_keep_to_rel_scup<-p_keep_to_rel_scup +p_keep_to_rel_scup*.02
              }
              
              # if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv<= 10){
              #   p_cod_kp_2_rl<-p_cod_kp_2_rl +.005
              # }
              
              #If in the baseline run, harvest is less than MRIP, and in the new run harvest is still less than MRIP, 
              #increase the baseline h_star_release_to_keep value 
              if(scup_keep_diff<0){
                p_keep_to_rel_scup<-p_keep_to_rel_scup -p_keep_to_rel_scup*.01
              }
            }
          
          if(p_keep_to_rel_sf==1) {
              p_keep_to_rel_sf<-1
              
          }
            
          if(p_keep_to_rel_bsb==1 ) {
              p_keep_to_rel_bsb<-1
              
          }
            
          if(p_keep_to_rel_scup==1 ) {
            p_keep_to_rel_scup<-1
            
          }
          
            source(file.path(code_cd, "calibrate_rec_catch1.R"))
            
          message("run ", i)
          message("model_sf_harv: ", sf_keep_model)
          message("mrip_sf_harv: ", sf_keep_MRIP)
          message("diff_sf_harv: ", sf_keep_diff)
          message("pct_diff_sf_harv: ", sf_keep_pct_diff)
          message("rel_to_keep_sf: ", rel_to_keep_sf)
          message("p_rel_to_keep_sf: ", p_rel_to_keep_sf)
          message("p_keep_to_rel_sf: ", p_keep_to_rel_sf)
          
          message("model_bsb_harv: ", bsb_keep_model)
          message("mrip_bsb_harv: ", bsb_keep_MRIP)
          message("diff_bsb_harv: ", bsb_keep_diff)
          message("pct_diff_bsb_harv: ", bsb_keep_pct_diff)
          message("rel_to_keep_bsb: ", rel_to_keep_bsb)
          message("p_rel_to_keep_bsb: ", p_rel_to_keep_bsb)
          message("p_keep_to_rel_bsb: ", p_keep_to_rel_bsb)
          
          message("model_scup_harv: ", scup_keep_model)
          message("mrip_scup_harv: ", scup_keep_MRIP)
          message("diff_scup_harv: ", scup_keep_diff)
          message("pct_diff_scup_harv: ", scup_keep_pct_diff)
          message("rel_to_keep_scup: ", rel_to_keep_scup)
          message("p_rel_to_keep_scup: ", p_rel_to_keep_scup)
          message("p_keep_to_rel_scup: ", p_keep_to_rel_scup)
          
          sf_achieved<-case_when((harv_diff_sf<500 | harv_pct_diff_sf<5)~1, TRUE~0)
          bsb_achieved<-case_when((harv_diff_bsb<500 | harv_pct_diff_bsb<5)~1, TRUE~0)
          scup_achieved<-case_when((harv_diff_scup<500 | harv_pct_diff_scup<5)~1, TRUE~0)
          
            if (sf_achieved==1 & bsb_achieved==1 & scup_achieved==1) break
            
      }
      }
      
      
#     }
#   }
# }

# #l_w_conversion =
# cod_lw_a = 0.000005132
# cod_lw_b = 3.1625
# had_lw_a = 0.000009298
# had_lw_b = 3.0205
# 
# Disc_mort<- readr::read_csv(file.path(input_data_cd, "Discard_Mortality.csv"), show_col_types = FALSE)



  
  #Do we need to reallocate some cod/haddock keep as releases, and vice versa? Makes indicator objects
  # cod_keep_2_release<-mean(baseline_output$cod_keep_2_release)
  # cod_release_2_keep<-mean(baseline_output$cod_release_2_keep)
  # hadd_keep_2_release<-mean(baseline_output$hadd_keep_2_release)
  # hadd_release_2_keep<-mean(baseline_output$hadd_release_2_keep)
  
  #Do we need to reallocate ALL cod/haddock keep as releases? Makes indicator objects
  # baseline_output<-baseline_output %>% 
  #   dplyr::mutate(all_cod_keep_2_release=ifelse(tot_keep_cod_model>0 & tot_keep_cod_mrip==0, 1, 0),
  #                 all_hadd_keep_2_release=ifelse(tot_keep_hadd_model>0 & tot_keep_hadd_mrip==0, 1, 0))
  # 
  # all_cod_keep_2_release<-mean(baseline_output$all_cod_keep_2_release)
  # all_hadd_keep_2_release<-mean(baseline_output$all_hadd_keep_2_release)
  
  
  # h_star_cod_keep_to_release_variable<-0
  # h_star_hadd_keep_to_release_variable<-0
  # h_star_cod_release_to_keep_variable<-0
  # h_star_hadd_release_to_keep_variable<-0
  # 
  # base_cod_harv_diff<-abs(baseline_output$diff_cod_harv)
  # base_hadd_harv_diff<-abs(baseline_output$diff_hadd_harv)
  # base_cod_harv_diff
  # base_hadd_harv_diff
  
  # base_cod_harv_perc_diff<-abs((baseline_output$tot_keep_cod_model-baseline_output$tot_keep_cod_mrip)/baseline_output$tot_keep_cod_mrip)*100
  # base_hadd_harv_perc_diff<-abs((baseline_output$tot_keep_hadd_model-baseline_output$tot_keep_hadd_mrip)/baseline_output$tot_keep_hadd_mrip)*100
  # base_cod_harv_perc_diff
  # base_hadd_harv_perc_diff
  # 
  # base_cod_achieved<-case_when((base_cod_harv_diff<500 | base_cod_harv_perc_diff<5)~1, TRUE~0)
  # base_hadd_achieved<-case_when((base_hadd_harv_diff<500 | base_hadd_harv_perc_diff<5)~1, TRUE~0)
  # base_cod_achieved
  # base_hadd_achieved
  

  
  
  
  #If all_cod/hadd_keep_2_release, then set h_stars to one (for all CE's with harvest, re-allocate harvest as release)
  if(all_cod_keep_2_release==1 & base_cod_achieved!=1){
    h_star_cod_keep_to_release_variable<-1
  }
  
  if(all_hadd_keep_2_release==1 & base_hadd_achieved!=1){
    h_star_hadd_keep_to_release_variable<-1
  }
  
  #If some portion of keep needs to be reallocated as released, then set h_stars to baseline proportion 
  if(all_cod_keep_2_release!=1 & cod_keep_2_release==1  & base_cod_achieved!=1){
    h_star_cod_keep_to_release_variable<-max(0.06, mean(baseline_output$h_star_cod_keep_to_release_variable))
  }
  
  if(all_hadd_keep_2_release!=1 & hadd_keep_2_release==1 & base_hadd_achieved!=1){
    h_star_hadd_keep_to_release_variable<-max(0.06,mean(baseline_output$h_star_hadd_keep_to_release_variable))
  }
  
  #If some release needs to be re-allocated kept, then set h_stars to baseline proportion 
  if(cod_release_2_keep==1  & base_cod_achieved!=1){
    h_star_cod_release_to_keep_variable<-max(0.06, mean(baseline_output$h_star_cod_release_to_keep_variable))
  }
  
  if(hadd_release_2_keep==1 & base_hadd_achieved!=1){
    h_star_hadd_release_to_keep_variable<-max(0.06, mean(baseline_output$h_star_hadd_release_to_keep_variable))
  }
  
  
  source(file.path(code_cd, "calibrate_rec_catch_hstar_code2_new.R"))
  
  
  print("new run")
  
  print("h_star_cod_keep_to_release")
  print(comparison$h_star_cod_keep_to_release_variable)
  
  print("h_star_cod_release_to_keep")
  print(comparison$h_star_cod_release_to_keep_variable)
  
  print("tot_keep_cod_model")
  print(comparison$tot_keep_cod_model)
  
  print("tot_cod_keep_mrip")
  print(comparison$tot_cod_keep_mrip)
  
  print("diff_cod_harv")
  print(comparison$diff_cod_harv)
  
  print("perc_diff_cod_harv")
  print(comparison$perc_diff_cod_harv)
  
  print("h_star_hadd_keep_to_release_variable")
  print(comparison$h_star_hadd_keep_to_release_variable)
  
  print("h_star_hadd_release_to_keep_variable")
  print(comparison$h_star_hadd_release_to_keep_variable)
  
  print("tot_keep_hadd_model")
  print(comparison$tot_keep_hadd_model)
  
  print("tot_hadd_keep_mrip")
  print(comparison$tot_hadd_keep_mrip)
  
  print("diff_hadd_harv")
  print(comparison$diff_hadd_harv)
  
  print("perc_diff_hadd_harv")
  print(comparison$perc_diff_hadd_harv)
  
  
  repeat{
    
    #For draws where release_to_keep==1:
    
    #If in the baseline run, cod harvest is less than MRIP, but in  a new run cod harvest is greater than MRIP, 
    #reduce the baseline h_star_release_to_keep value 
    
    if(cod_release_2_keep==1 & comparison$cod_achieved!=1) {
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv> 10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp -.05
      }
      
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv<= 10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp -.025
      }
      
      
      #If in the baseline run, cod harvest is less than MRIP, and in the new run cod harvest is still less than MRIP, 
      #increase the baseline h_star_release_to_keep value 
      if(comparison$diff_cod_harv<0 & comparison$perc_diff_cod_harv< -10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp +.06
      }
      
      if(comparison$diff_cod_harv<0 & comparison$perc_diff_cod_harv>= -10){
        p_cod_rl_2_kp<-p_cod_rl_2_kp +.03
      }
    }
    
    
    
    #same for haddock
    if(hadd_release_2_keep==1 & comparison$hadd_achieved!=1) {
      
      
      if(comparison$diff_hadd_harv>0 & comparison$perc_diff_hadd_harv> 25 ){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp -.05
      }
      
      if(comparison$diff_hadd_harv>0 & comparison$perc_diff_hadd_harv<= 25 ){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp -.03
      }
      
      
      if(comparison$diff_hadd_harv<0 & comparison$perc_diff_hadd_harv< -25){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp +.06
      }
      
      if(comparison$diff_hadd_harv<0 & comparison$perc_diff_hadd_harv>= -25){
        p_hadd_rl_2_kp<-p_hadd_rl_2_kp +.04
      }
      
    }
    
    
    #For draws where release_to_keep==0, keep_to_release==1
    #If in the baseline run, cod harvest is less than MRIP, but in  a new run cod harvest is greater than MRIP, 
    #reduce the baseline h_star_release_to_keep value 
    
    if(cod_keep_2_release==1 & comparison$cod_achieved!=1) {
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv> 10){
        p_cod_kp_2_rl<-p_cod_kp_2_rl +.02
      }
      
      if(comparison$diff_cod_harv>0 & comparison$perc_diff_cod_harv<= 10){
        p_cod_kp_2_rl<-p_cod_kp_2_rl +.005
      }
      
      #If in the baseline run, cod harvest is less than MRIP, and in the new run cod harvest is still less than MRIP, 
      #increase the baseline h_star_release_to_keep value 
      if(comparison$diff_cod_harv<0){
        p_cod_kp_2_rl<-p_cod_kp_2_rl -.004
      }
    }
    
    
    #same for haddock
    if(hadd_keep_2_release==1 & comparison$hadd_achieved!=1) {
      
      if(comparison$diff_hadd_harv>0){
        p_hadd_kp_2_rl<-p_hadd_kp_2_rl +.005
      }
      
      if(comparison$diff_hadd_harv<0){
        p_hadd_kp_2_rl<-p_hadd_kp_2_rl -.004
      }
    }
    
    
    
    if (comparison$hadd_achieved==1 & comparison$cod_achieved==1) break
    if (comparison$hadd_achieved==0 & mean(baseline_output$hadd_release_2_keep==1) & comparison$h_star_hadd_release_to_keep_variable>1) break
    if (comparison$cod_achieved==0 & mean(baseline_output$cod_release_2_keep==1) & comparison$h_star_cod_release_to_keep_variable>1) break
    
    if(all_cod_keep_2_release==1){
      h_star_cod_keep_to_release_variable<-1
    }
    
    
    if(all_hadd_keep_2_release==1){
      h_star_hadd_keep_to_release_variable<-1
    }
    
    
    if(all_cod_keep_2_release!=1 & cod_keep_2_release==1 & comparison$cod_achieved!=1){
      h_star_cod_keep_to_release_variable<-mean(baseline_output$h_star_cod_keep_to_release_variable)+p_cod_kp_2_rl
    }
    
    
    if(all_hadd_keep_2_release!=1 & hadd_keep_2_release==1 & comparison$hadd_achieved!=1){
      h_star_hadd_keep_to_release_variable<-mean(baseline_output$h_star_hadd_keep_to_release_variable)+p_hadd_kp_2_rl
    }
    
    
    if(cod_release_2_keep==1 & comparison$cod_achieved!=1){
      h_star_cod_release_to_keep_variable<-mean(baseline_output$h_star_cod_release_to_keep_variable)+p_cod_rl_2_kp
    }
    
    
    if(hadd_release_2_keep==1 & comparison$hadd_achieved!=1){
      h_star_hadd_release_to_keep_variable<-mean(baseline_output$h_star_hadd_release_to_keep_variable)+p_hadd_rl_2_kp
    }
    
    
    h_star_cod_release_to_keep_variable
    h_star_hadd_release_to_keep_variable
    h_star_hadd_keep_to_release_variable
    h_star_cod_keep_to_release_variable
    
    source(file.path(code_cd, "calibrate_rec_catch_hstar_code2_new.R"))
    
    print("new run")
    
    print("h_star_cod_keep_to_release")
    print(comparison$h_star_cod_keep_to_release_variable)
    
    print("h_star_cod_release_to_keep")
    print(comparison$h_star_cod_release_to_keep_variable)
    
    print("tot_keep_cod_model")
    print(comparison$tot_keep_cod_model)
    
    print("tot_cod_keep_mrip")
    print(comparison$tot_cod_keep_mrip)
    
    print("diff_cod_harv")
    print(comparison$diff_cod_harv)
    
    print("perc_diff_cod_harv")
    print(comparison$perc_diff_cod_harv)
    
    print("h_star_hadd_keep_to_release_variable")
    print(comparison$h_star_hadd_keep_to_release_variable)
    
    print("h_star_hadd_release_to_keep_variable")
    print(comparison$h_star_hadd_release_to_keep_variable)
    
    print("tot_keep_hadd_model")
    print(comparison$tot_keep_hadd_model)
    
    print("tot_hadd_keep_mrip")
    print(comparison$tot_hadd_keep_mrip)
    
    print("diff_hadd_harv")
    print(comparison$diff_hadd_harv)
    
    print("perc_diff_hadd_harv")
    print(comparison$perc_diff_hadd_harv)
    
    
  }
  
  
  ##Uncomment this if you want calibration catch weights
  #source(file.path(code_cd, "calibration_catch_weights2.R"))
  
}


baseline_output0<-readRDS(file.path(input_data_cd, "harvest_differences_check.rds")) 

n_distinct(baseline_output0$draw)

check1<-data.frame() 
check2<-data.frame() 

for(i in unique(baseline_output0$mrip_index)){
  
  check0<-baseline_output0 %>% dplyr::filter(mrip_index==i)
  
  season1<-unique(check0$open)
  mode1<-unique(check0$mode)
  draw1<-unique(check0$draw)
  
  check1 <- feather::read_feather(file.path(iterative_input_data_cd, paste0("comparison_", mode1,"_", season1, "_", draw1, ".feather")))  
  check2<-rbind(check1, check2)
  
}


baseline<- readRDS(file.path(input_data_cd, "harvest_differences_check.rds")) %>% 
  dplyr::select(cod_keep_2_release, cod_release_2_keep, hadd_keep_2_release, hadd_release_2_keep, draw, 
                mode, mrip_index, diff_cod_harv, diff_hadd_harv, tot_cod_catch_model, tot_hadd_catch_model, 
                tot_keep_cod_model, tot_keep_hadd_model, tot_rel_cod_model, tot_rel_hadd_model) 
colnames(baseline) <- gsub("_model", "_model_base", colnames(baseline))
colnames(baseline) <- gsub("_harv", "_harv_base", colnames(baseline))

check2<-check2 %>% 
  dplyr::left_join(baseline, by=c("draw", "mode", "mrip_index")) %>% 
  dplyr::mutate(tab=case_when((cod_achieved==0 | hadd_achieved==0)~1, TRUE~0)) %>% 
  dplyr::group_by(draw) %>% 
  dplyr::mutate(sumtab=sum(tab)) %>% 
  dplyr::filter(sumtab==0)

n_distinct(check2$draw)

check3<-check2 %>% 
  dplyr::filter((abs_perc_diff_cod_harv>5 & abs(diff_cod_harv)>500) | (abs_perc_diff_hadd_harv>5 & abs(diff_hadd_harv)>500))

saveRDS(check2, file = file.path(input_data_cd, "calibration_comparison.rds"))
n_distinct(check2$draw)


##Now we have the data for the projections stored in input_data_cd:
#pds_new_x = number of choice occasion
#comparison_x = percent of choice occasions the keep all harvest/release all harvest 
#costs_x = baseline catch levels, trip costs, and demographics. 




##Uncomment this if you want calibration catch weights
#Compile the calibration catch weights
# check1a<-data.frame()
# check2a<-data.frame()
# 
# for(i in unique(check2$mrip_index)){
#   check0a<- check2 %>% dplyr::filter(mrip_index==i)
# 
#   season1<-unique(check0a$open)
#   mode1<-unique(check0a$mode)
#   draw1<-unique(check0a$draw)
# 
#   check1a<- readRDS(file.path(iterative_input_data_cd, paste0("calibrate_catch_wts_", mode1,"_", season1, "_", draw1, ".rds")))
#   check2a<-rbind(check1a, check2a)
# 
# }
# n_distinct(check2a$run)

# write_xlsx(check2a, file.path(input_data_cd, "calibration_catch_weights_cm.xlsx"))




#This is the simulation model for the calibration year WITH adjustments for illegal harvest or voluntary release

states <- c("MA", "RI")
mode_draw <- c("sh", "pr", "fh")
draws <- 1:2
 i<-1
 s="MA"
 md="fh"
# Create an empty list to collect results
calib_comparison <- list()

# Loop over all combinations
for (s in states) {
  for (md in mode_draw) {
    for (i in draws) {
      
      # import necessary data
      dtripz<-feather::read_feather(file.path(input_data_cd, paste0("directed_trips_calibration_", s, ".feather"))) %>% 
        tibble::tibble() %>%
        dplyr::filter(draw == i) %>%
        dplyr::select(mode, dtrip, date, bsb_bag, bsb_min, fluke_bag,fluke_min, scup_bag, scup_min) %>% 
        dplyr::filter(mode==md)
      
      catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("calib_catch_draws_",s, "_", i,".feather"))) %>% 
        dplyr::left_join(dtripz, by=c("mode", "date"))%>% 
        dplyr::filter(mode==md)
      
      angler_dems<-catch_data %>% 
        dplyr::select(date, mode, tripid, total_trips_12, age, cost) %>% 
        dplyr::filter(mode==md)
      
      catch_data<-catch_data %>% 
        dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                      -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)
        
      
      angler_dems<-dplyr::distinct(angler_dems)
      
      sf_size_data <- read_csv(file.path(test_data_cd, "fluke_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
        dplyr::filter(state == s, draw==0 ) %>% 
        dplyr::filter(!is.na(fitted_prob)) %>% 
        dplyr::select(state, fitted_prob, length)
      
      bsb_size_data <- read_csv(file.path(test_data_cd, "bsb_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
        dplyr::filter(state == s, draw==0 ) %>% 
        dplyr::filter(!is.na(fitted_prob)) %>% 
        dplyr::select(state, fitted_prob, length)
      
      scup_size_data <- read_csv(file.path(test_data_cd, "scup_projected_catch_at_lengths.csv"), show_col_types = FALSE)  %>% 
        dplyr::filter(state == s, draw==0 ) %>% 
        dplyr::filter(!is.na(fitted_prob)) %>% 
        dplyr::select(state,  fitted_prob, length)
      
     
      #Create as an object the minimum size at which fish may be illegally harvested.
      #1) This floor_subl_harvest size will be 2 inches below the minimum size, by mode. 
      #1a) If the minimum size changes across the season, floor_subl_harvest=min(min_size). 
      #2) If the fishery is closed the entire season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 
      #1) and #1a) below:
      
      floor_subl_sf_harv<-min(dtripz$fluke_min)-2*2.54
      floor_subl_bsb_harv<-min(dtripz$bsb_min)-2*2.54
      floor_subl_scup_harv<-min(dtripz$scup_min)-2*2.54
      
      # begin trip simulation
      
      # subset trips with zero catch, as no size draws are required
      sf_zero_catch <- dplyr::filter(catch_data, sf_cat == 0)
      bsb_zero_catch <- dplyr::filter(catch_data, bsb_cat == 0)
      scup_zero_catch <- dplyr::filter(catch_data, scup_cat == 0)
      
      # Check if there is zero catch for any species and if so, pipe code around keep/release determination
      sf_catch_check<-base::sum(catch_data$sf_cat)
      bsb_catch_check<-base::sum(catch_data$bsb_cat)
      scup_catch_check<-base::sum(catch_data$scup_cat)
      
      calib_comparison<-feather::read_feather(file.path(iterative_input_data_cd, "calibration_comparison.feather")) %>% 
        dplyr::filter(state==s & draw==i)
      #& mode==md)
      
      for (p in 1:nrow(calib_comparison)) {
        sp <- calib_comparison$species[p]
        
        assign(paste0("rel_to_keep_", sp), calib_comparison$rel_to_keep[p])
        assign(paste0("keep_to_rel_", sp), calib_comparison$keep_to_rel[p])
        
        
        if (calib_comparison$rel_to_keep[p] == 1) {
          assign(paste0("p_rel_to_keep_", sp), calib_comparison$p_rel_to_keep[p])
        }
        
        if (calib_comparison$keep_to_rel[p] == 1) {
          assign(paste0("p_keep_to_rel_", sp), calib_comparison$p_keep_to_rel[p])
        }
      }
      
      
      
      
      # Summer flounder trip simulation
      
      #keep trips with positive sf catch
      sf_catch_data <- dplyr::filter(catch_data, sf_cat > 0)
      
      row_inds <- seq_len(nrow(sf_catch_data))
      
      sf_catch_data<-sf_catch_data %>%
        dplyr::slice(rep(row_inds, sf_cat))   %>%
        dplyr::mutate(fishid=dplyr::row_number())
      
      # generate lengths for each fish
      catch_size_data <- sf_catch_data %>%
        dplyr::mutate(fitted_length = sample(sf_size_data$length,
                                             nrow(.),
                                             prob = sf_size_data$fitted_prob,
                                             replace = TRUE)) %>%  
        dplyr::mutate(fitted_length=fitted_length*2.54)
      
      
      # Impose regulations, calculate keep and release per trip
      catch_size_data <- catch_size_data %>%
        dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min ,1,0)) %>%
        dplyr::group_by(tripid, date, mode, catch_draw) %>%
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj = dplyr::case_when(
            fluke_bag > 0 ~ ifelse(csum_keep<=fluke_bag & posskeep==1,1,0),
            TRUE ~ 0))
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate(keep = keep_adj,
                      release = ifelse(keep==0,1,0)) %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  
      
      # code for trip level harvest re-allocation
      catch_size_data<- catch_size_data %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
        dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_sf_harv~1,TRUE~0))
      
      sum_sf_rel<-sum(catch_size_data$release)
      sum_sf_keep<-sum(catch_size_data$keep)
      
      # reallocate a portion of all releases as kept if needed 
      if (rel_to_keep_sf==1 & sum_sf_rel>0){
        
        catch_size_data_re_allocate<- catch_size_data %>%
          dplyr::filter(subl_harv_indicator==1) 
        
        catch_size_data_re_allocate_base<- catch_size_data %>%
          dplyr::filter(subl_harv_indicator==0) 
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
          dplyr::arrange(uniform) %>%
          dplyr::ungroup()
        
        n_row_re_allocate<-nrow(catch_size_data_re_allocate)
        n_sub_sf_kept=round(p_rel_to_keep_sf*n_row_re_allocate)  
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
          dplyr::mutate(keep_new=case_when(fishid2<=n_sub_sf_kept~1, TRUE~ 0))
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>% 
          dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
          dplyr::rename(keep=keep_new, release=rel_new)
        
        
        catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
          dplyr::select(-subl_harv_indicator)
        
        # sum(catch_size_data$keep)
        # sum(catch_size_data_check$keep)
        # 
        # sum(catch_size_data$release)
        # sum(catch_size_data_check$release)
        
        n_legal_sf_rel<-0
        
      }
      
      trip_data <- catch_size_data %>%
        dplyr::group_by(date, catch_draw, tripid, mode) %>%
        dplyr::summarize(tot_keep_sf_new = sum(keep),
                         tot_rel_sf_new = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()
      
      sf_zero_catch<-sf_zero_catch %>%
        dplyr::select(date, catch_draw, tripid, mode) %>%
        dplyr::mutate(tot_keep_sf_new=0,
                      tot_rel_sf_new=0)
      
      sf_trip_data <- dplyr::bind_rows(trip_data, sf_zero_catch) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::select(c("date", "catch_draw","tripid","mode",
                        "tot_keep_sf_new","tot_rel_sf_new"))
      
      sf_trip_data<- sf_trip_data %>% dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))
      sf_trip_data<-data.table::as.data.table(sf_trip_data)
      data.table::setkey(sf_trip_data, "domain2")
      
      
      # if (cod_catch_check==0 & had_catch_check!=0){
      #   trip_data<-cod_catch_data
      #   trip_data<- trip_data %>% 
      #     dplyr::mutate(domain2 = paste0(period2, "_", catch_draw, "_", tripid)) %>% 
      #     dplyr::select(-mode) %>% 
      #     as.data.table()
      #   
      #   data.table::setkey(trip_data, "domain2")
      #   
      #   trip_data$tot_keep_cod_new<-0
      #   trip_data$tot_rel_cod_new<-0
      # }
      
      # BSB flounder trip simulation
      
      # keep trips with positive bsb catch
      bsb_catch_data <- dplyr::filter(catch_data, bsb_cat > 0)
      
      row_inds <- seq_len(nrow(bsb_catch_data))
      
      bsb_catch_data<-bsb_catch_data %>%
        dplyr::slice(rep(row_inds, bsb_cat))   %>%
        dplyr::mutate(fishid=dplyr::row_number())
      
      # generate lengths for each fish
      catch_size_data <- bsb_catch_data %>%
        dplyr::mutate(fitted_length = sample(bsb_size_data$length,
                                             nrow(.),
                                             prob = bsb_size_data$fitted_prob,
                                             replace = TRUE)) %>%     #dplyr::arrange(period2, tripid, catch_draw) 
        dplyr::mutate(fitted_length=fitted_length*2.54)
      
      # Impose regulations, calculate keep and release per trip
      catch_size_data <- catch_size_data %>%
        dplyr::mutate(posskeep = ifelse(fitted_length>=bsb_min ,1,0)) %>%
        dplyr::group_by(tripid, date, mode, catch_draw) %>%
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj = dplyr::case_when(
            bsb_bag > 0 ~ ifelse(csum_keep<=bsb_bag & posskeep==1,1,0),
            TRUE ~ 0))
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate(keep = keep_adj,
                      release = ifelse(keep==0,1,0)) %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) 
      
      # code for trip level harvest re-allocation
      catch_size_data<- catch_size_data %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
        dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_bsb_harv~1,TRUE~0))
      
      sum_bsb_rel<-sum(catch_size_data$release)
      sum_bsb_keep<-sum(catch_size_data$keep)
      
      # reallocate a portion of all releases as kept if needed 
      if (rel_to_keep_bsb==1 & sum_bsb_rel>0){
        
        catch_size_data_re_allocate<- catch_size_data %>%
          dplyr::filter(subl_harv_indicator==1) 
        
        catch_size_data_re_allocate_base<- catch_size_data %>%
          dplyr::filter(subl_harv_indicator==0) 
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
          dplyr::arrange(uniform) %>%
          dplyr::ungroup()
        
        n_row_re_allocate<-nrow(catch_size_data_re_allocate)
        n_sub_bsb_kept=round(p_rel_to_keep_bsb*n_row_re_allocate)  
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
          dplyr::mutate(keep_new=case_when(fishid2<=n_sub_bsb_kept~1, TRUE~ 0))
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>% 
          dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
          dplyr::rename(keep=keep_new, release=rel_new)
        
        
        catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
          dplyr::select(-subl_harv_indicator)
        
        # sum(catch_size_data$keep)
        # sum(catch_size_data_check$keep)
        # 
        # sum(catch_size_data$release)
        # sum(catch_size_data_check$release)
        
        n_legal_bsb_rel<-0
        
      }
      
      trip_data <- catch_size_data %>%
        dplyr::group_by(date, catch_draw, tripid, mode) %>%
        dplyr::summarize(tot_keep_bsb_new = sum(keep),
                         tot_rel_bsb_new = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()
      
      bsb_zero_catch<-bsb_zero_catch %>%
        dplyr::select(date, catch_draw, tripid, mode) %>%
        dplyr::mutate(tot_keep_bsb_new=0,
                      tot_rel_bsb_new=0)
      
      bsb_trip_data <- dplyr::bind_rows(trip_data, bsb_zero_catch) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::select(c("date", "catch_draw","tripid","mode",
                        "tot_keep_bsb_new","tot_rel_bsb_new"))
      
      bsb_trip_data<- bsb_trip_data %>% 
        dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
        dplyr::select(-c("date", "catch_draw","tripid","mode"))
      
      bsb_trip_data<-data.table::as.data.table(bsb_trip_data)
      data.table::setkey(bsb_trip_data, "domain2")
      
      
      # Scup  trip simulation
      
      #keep trips with positive scup catch
      scup_catch_data <- dplyr::filter(catch_data, scup_cat > 0)
      
      row_inds <- seq_len(nrow(scup_catch_data))
      
      scup_catch_data<-scup_catch_data %>%
        dplyr::slice(rep(row_inds, scup_cat))   %>%
        dplyr::mutate(fishid=dplyr::row_number())
      
      # generate lengths for each fish
      catch_size_data <- scup_catch_data %>%
        dplyr::mutate(fitted_length = sample(scup_size_data$length,
                                             nrow(.),
                                             prob = scup_size_data$fitted_prob,
                                             replace = TRUE)) %>% 
        dplyr::mutate(fitted_length=fitted_length*2.54)
      
      
      # Impose regulations, calculate keep and release per trip
      catch_size_data <- catch_size_data %>%
        dplyr::mutate(posskeep = ifelse(fitted_length>=scup_min ,1,0)) %>%
        dplyr::group_by(tripid, date, mode, catch_draw) %>%
        dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          keep_adj = dplyr::case_when(
            scup_bag > 0 ~ ifelse(csum_keep<=scup_bag & posskeep==1,1,0),
            TRUE ~ 0))
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
      
      catch_size_data <- catch_size_data %>%
        dplyr::mutate(keep = keep_adj,
                      release = ifelse(keep==0,1,0)) %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  
      
      # code for trip level harvest re-allocation
      catch_size_data<- catch_size_data %>%
        dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
        dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_scup_harv~1,TRUE~0))
      
      sum_scup_rel<-sum(catch_size_data$release)
      sum_scup_keep<-sum(catch_size_data$keep)
      
      # reallocate a portion of all releases as kept if needed 
      if (rel_to_keep_scup==1 & sum_scup_rel>0){
        
        catch_size_data_re_allocate<- catch_size_data %>%
          dplyr::filter(subl_harv_indicator==1) 
        
        catch_size_data_re_allocate_base<- catch_size_data %>%
          dplyr::filter(subl_harv_indicator==0) 
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
          dplyr::arrange(uniform) %>%
          dplyr::ungroup()
        
        n_row_re_allocate<-nrow(catch_size_data_re_allocate)
        n_sub_scup_kept=round(p_rel_to_keep_scup*n_row_re_allocate)  
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
          dplyr::mutate(keep_new=case_when(fishid2<=n_sub_scup_kept~1, TRUE~ 0))
        
        catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
          dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>% 
          dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
          dplyr::rename(keep=keep_new, release=rel_new)
        
        
        catch_size_data<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
          dplyr::select(-subl_harv_indicator)
        
        # sum(catch_size_data$keep)
        # sum(catch_size_data_check$keep)
        # 
        # sum(catch_size_data$release)
        # sum(catch_size_data_check$release)
        
        n_legal_scup_rel<-0
        
      }
      
      trip_data <- catch_size_data %>%
        dplyr::group_by(date, catch_draw, tripid, mode) %>%
        dplyr::summarize(tot_keep_scup_new = sum(keep),
                         tot_rel_scup_new = sum(release),
                         .groups = "drop") %>%
        dplyr::ungroup()
      
      scup_zero_catch<-scup_zero_catch %>%
        dplyr::select(date, catch_draw, tripid, mode) %>%
        dplyr::mutate(tot_keep_scup_new=0,
                      tot_rel_scup_new=0)
      
      scup_trip_data <- dplyr::bind_rows(trip_data, scup_zero_catch) %>%
        dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
        dplyr::select(c("date", "catch_draw","tripid","mode",
                        "tot_keep_scup_new","tot_rel_scup_new"))
      
      scup_trip_data<- scup_trip_data %>% 
        dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
        dplyr::select(-c("date", "catch_draw","tripid","mode"))
      scup_trip_data<-data.table::as.data.table(scup_trip_data)
      data.table::setkey(scup_trip_data, "domain2") 
      
      
      # merge the hadd trip data with the rest of the trip data
      trip_data<- sf_trip_data[bsb_trip_data, on = "domain2"]
      trip_data<- trip_data[scup_trip_data, on = "domain2"]
      
      trip_data<- trip_data %>% 
        dplyr::mutate(tot_scup_catch = tot_keep_scup_new + tot_rel_scup_new, 
                      tot_bsb_catch = tot_keep_bsb_new + tot_rel_bsb_new, 
                      tot_sf_catch = tot_keep_sf_new + tot_rel_sf_new)
      #}
      
      parameters <- trip_data %>% 
        dplyr::select(date, mode, tripid) 
      
      parameters <- dplyr::distinct(parameters) 
      parameters<-  parameters %>% 
        #dplyr::arrange(date, mode, tripid) %>% 
        dplyr::mutate(beta_sqrt_sf_keep= rnorm(nrow(parameters), mean = 0.827, sd = 1.267), 
                      beta_sqrt_sf_release = rnorm(nrow(parameters), mean = 0.065 , sd = 0.325) , 
                      beta_sqrt_bsb_keep = rnorm(nrow(parameters), mean = 0.353, sd = 0.129), 
                      beta_sqrt_bsb_release = rnorm(nrow(parameters), mean = 0.074 , sd = 0), 
                      beta_sqrt_sf_bsb_keep = rnorm(nrow(parameters), mean=-0.056  , sd = 0.196 ), 
                      beta_sqrt_scup_catch = rnorm(nrow(parameters), mean = 0.018 , sd = 0), 
                      beta_opt_out = rnorm(nrow(parameters), mean =-2.056 , sd = 1.977), 
                      beta_opt_out_avidity = rnorm(nrow(parameters), mean =-0.010 , sd = 0), 
                      beta_opt_out_age = rnorm(nrow(parameters), mean =0.010 , sd = 0), 
                      beta_cost = -0.012) 
      
      trip_data<- trip_data %>% 
        dplyr::left_join(parameters, by = c("date", "mode", "tripid")) %>% 
        dplyr::arrange(date, mode, tripid, tripid, catch_draw) %>% 
        dplyr::left_join(angler_dems, by = c("date", "mode", "tripid")) 
      
      
      # base_outcomes_s_md_i data sets will retain trip outcomes from the baseline scenario.
      # We will merge these data to the prediction year outcomes to calculate changes in effort and CS.
      baseline_outcomes<- trip_data %>%
        dplyr::rename(tot_keep_bsb_base = tot_keep_bsb_new,
                      tot_keep_scup_base = tot_keep_scup_new,
                      tot_keep_sf_base = tot_keep_sf_new,
                      tot_rel_bsb_base = tot_rel_bsb_new, 
                      tot_rel_scup_base = tot_rel_scup_new,
                      tot_rel_sf_base = tot_rel_sf_new, 
                      tot_cat_bsb_base = tot_bsb_catch, 
                      tot_cat_scup_base = tot_scup_catch,
                      tot_cat_sf_base = tot_sf_catch)
      
      write_feather(baseline_outcomes, file.path(iterative_input_data_cd, paste0("base_outcomes_", s,"_", md, "_", i,".feather")))
      
      #  utility
      trip_data <-trip_data %>%
        dplyr::mutate(
          vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf_new) +
            beta_sqrt_sf_release*sqrt(tot_rel_sf_new) +
            beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_new) +
            beta_sqrt_bsb_release*sqrt(tot_rel_bsb_new) +
            beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf_new)*sqrt(tot_keep_bsb_new)) +
            beta_sqrt_scup_catch*sqrt(tot_scup_catch) +
            beta_cost*cost)
      
      
      mean_trip_data <- trip_data %>% data.table::data.table() %>% 
        .[, group_index := .GRP, by = .(date, mode, catch_draw, tripid)]
      
      # Now expand the data to create two alternatives, representing the alternatives available in choice survey
      mean_trip_data <- mean_trip_data %>%
        dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
        tidyr::uncount(n_alt) %>%
        dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                      opt_out = ifelse(alt == 2, 1, 0))
      
      #Calculate the expected utility of alts 2 parameters of the utility function,
      #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
      
      setDT(mean_trip_data)
      
      # Filter only alt == 2 once, and calculate vA 
      mean_trip_data[alt == 2, "vA" := .(
        beta_opt_out * opt_out +
          beta_opt_out_age * (age * opt_out) +
          beta_opt_out_avidity * (total_trips_12 * opt_out) 
      )]
      
      # Pre-compute exponential terms
      mean_trip_data[, `:=`(exp_vA = exp(vA))]
      
      # Group by group_index and calculate probabilities and log-sums
      mean_trip_data[, `:=`(
        probA = exp_vA / sum(exp_vA)
      ), by = group_index]
      
      
      mean_trip_data<- subset(mean_trip_data, alt==1) %>% 
        dplyr::select(-domain2, -group_index, -exp_vA) 
      
      # Get rid of things we don't need.
      mean_trip_data <- mean_trip_data %>% 
        dplyr::filter(alt==1) %>% 
        dplyr::select(-matches("beta")) %>% 
        dplyr::select(-"alt", -"opt_out", -"vA" ,-"cost", -"age", -"total_trips_12", -"catch_draw") 
      
      all_vars<-c()
      all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("date","mode", "tripid")]
      all_vars
      
      # average outcomes across draws
      mean_trip_data<-mean_trip_data  %>% as.data.table() %>%
        .[,lapply(.SD, mean), by = c("date","mode", "tripid"), .SDcols = all_vars]
      
      # multiply the average trip probability (probA) by each catch variable to get probability-weighted catch
      list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",    "tot_keep_bsb_new",  "tot_rel_bsb_new",   "tot_keep_scup_new",
                      "tot_rel_scup_new",  "tot_scup_catch",    "tot_bsb_catch",     "tot_sf_catch")
      
      mean_trip_data <- mean_trip_data %>%
        as.data.table() %>%
        .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
        .[]
      
      
      dtrips<-feather::read_feather(file.path(input_data_cd, paste0("directed_trips_calibration_", s, ".feather"))) %>% 
        tibble::tibble() %>%
        dplyr::filter(draw == i) %>%
        dplyr::select(mode, date, dtrip) %>% 
        dplyr::filter(mode==md)
      
      mean_trip_data<-mean_trip_data %>% 
        left_join(dtrips, by = c("mode", "date"))
      
      mean_trip_data <-mean_trip_data %>% 
        group_by(mode, date) %>% 
        dplyr::mutate(mean_prob=mean(probA)) %>% 
        dplyr::ungroup() %>%       
        dplyr::mutate(sims=round(dtrip/mean_prob), 
                      expand=sims/n_draws, 
                      n_choice_occasions=1)
      
      mean_trip_data <- mean_trip_data %>% 
        mutate(uniform=runif(n(), min=0, max=1)) %>% 
        dplyr::arrange(date, mode, uniform)
      
      mean_trip_data1 <- mean_trip_data %>% 
        dplyr::group_by(date, mode) %>%
        dplyr::mutate(id_within_group = row_number()) %>% 
        dplyr::filter(expand<1 & id_within_group<=sims) 
      
      mean_trip_data2 <- mean_trip_data %>% 
        dplyr::filter(expand>1)  %>% 
        dplyr::mutate(expand2=ceiling(expand)) 
      
      row_inds <- seq_len(nrow(mean_trip_data2))
      
      mean_trip_data2<-mean_trip_data2 %>% 
        slice(rep(row_inds,expand2))  
      
      mean_trip_data2 <- mean_trip_data2 %>%
        dplyr::group_by(date, mode) %>%
        dplyr::mutate(id_within_group = row_number()) %>% 
        dplyr::filter(id_within_group<=sims)
      
      results<-mean_trip_data1 %>% 
        dplyr::bind_rows(mean_trip_data2)
      
      list_names = c("tot_bsb_catch","tot_keep_bsb_new","tot_keep_scup_new","tot_keep_sf_new","tot_rel_bsb_new",   
                     "tot_rel_scup_new","tot_rel_sf_new","tot_scup_catch","tot_sf_catch",
                     "probA","n_choice_occasions")
      
      aggregate_trip_data <- results %>%
        data.table::as.data.table() %>%
        .[,lapply(.SD, sum),  by = c("date", "mode"), .SDcols = list_names]
      
      aggregate_trip_data<-aggregate_trip_data %>% 
        dplyr::rename(estimated_trips=probA, 
                      sf_catch=tot_sf_catch, 
                      bsb_catch=tot_bsb_catch, 
                      scup_catch=tot_scup_catch, 
                      sf_keep=tot_keep_sf_new, 
                      bsb_keep=tot_keep_bsb_new, 
                      scup_keep=tot_keep_scup_new,
                      sf_rel=tot_rel_sf_new, 
                      bsb_rel=tot_rel_bsb_new, 
                      scup_rel=tot_rel_scup_new)
      
      list_names = c("bsb_catch","bsb_keep","bsb_rel", 
                     "scup_catch", "scup_keep","scup_rel", 
                     "sf_catch", "sf_keep","sf_rel",
                     "estimated_trips","n_choice_occasions")
      
      summed_results <- aggregate_trip_data %>%
        data.table::as.data.table() %>%
        .[,lapply(.SD, sum),  by = c("mode"), .SDcols = list_names]
      
      aggregate_trip_data<-aggregate_trip_data %>% 
        dplyr::select(date, mode, n_choice_occasions)
      
      write_feather(aggregate_trip_data, file.path(iterative_input_data_cd, paste0("n_choice_occasions_", s,"_", md, "_", i, ".feather")))
      
    }
  }
}



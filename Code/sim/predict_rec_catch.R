# Predict Rec Catch
# This function predict recreational catch for summer flounder, black sea bass, and scup. 

# Run predict_rec_catch_data_read testing. 
     
 
predict_rec_catch <- function(st, dr, directed_trips, catch_data, 
                              sf_size_data, bsb_size_data, scup_size_data, 
                              l_w_conversion, calib_comparison, n_choice_occasions, 
                              base_outcomes){
  
  options(scipen = 999)
  
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
      
      #Create as an object the minimum size at which fish may be illegally harvested.
      #1) This floor_subl_harvest size will be 2 inches below the minimum size, by mode. 
      #1a) If the minimum size changes across the season, floor_subl_harvest=min(min_size). 
      #2) If the fishery is closed the entire season, floor_subl_harvest=mean(catch_length)-0.5*sd(catch_length). 
      #1) and #1a) below:
      
  floor_subl_sf_harv<-min(directed_trips$fluke_min)
  floor_subl_bsb_harv<-min(directed_trips$bsb_min)
  floor_subl_scup_harv<-min(directed_trips$scup_min)
  
  #####END Part A 
  
  
  
  # begin trip simulation
  
  # subset trips with zero catch, as no size draws are required
  sf_zero_catch0 <- dplyr::filter(catch_data, sf_cat == 0)
  bsb_zero_catch0 <- dplyr::filter(catch_data, bsb_cat == 0)
  scup_zero_catch0 <- dplyr::filter(catch_data, scup_cat == 0)
  
  # Check if there is zero catch for any species and if so, pipe code around keep/release determination
  sf_catch_check<-base::sum(catch_data$sf_cat)
  bsb_catch_check<-base::sum(catch_data$bsb_cat)
  scup_catch_check<-base::sum(catch_data$scup_cat)
  
  
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
                                         replace = TRUE)) 
  
  
  # Impose regulations, calculate keep and release per trip
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min_y2 ,1,0)) %>%
    dplyr::group_by(tripid, date, mode, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        fluke_bag_y2 > 0 ~ ifelse(csum_keep<=fluke_bag_y2 & posskeep==1,1,0),
        TRUE ~ 0))
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0)) %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  
  
  
  
  # code for trip-level harvest re-allocation -- NEED TO ADD voluntary release code
  catch_size_data0<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
    dplyr::mutate(subl_harv_indicator=dplyr::case_when(release==1 & fitted_length>=floor_subl_sf_harv~1,TRUE~0))
  
  keep_release_list_sf<- list()
  mode_list_sf <- list()
  mode_draw <- c("sh", "pr", "fh")
  
  for (md in mode_draw) {
    
    catch_size_data<- catch_size_data0 %>%
      dplyr::filter(mode==md)
    
    sum_sf_rel<-sum(catch_size_data$release)
    sum_sf_keep<-sum(catch_size_data$keep)
    
    calib_comparison_md<-calib_comparison %>% 
      dplyr::filter(species=="sf" & mode==md)
    
    rel_to_keep_md<- mean(calib_comparison_md$rel_to_keep)
    p_rel_to_keep_md<- mean(calib_comparison_md$p_rel_to_keep)
    
    
    # reallocate a portion of all releases as kept if needed 
    
    if (rel_to_keep_md==1 & sum_sf_rel>0){
      
      catch_size_data_re_allocate<- catch_size_data %>%
        dplyr::filter(subl_harv_indicator==1) 
      
      catch_size_data_re_allocate_base<- catch_size_data %>%
        dplyr::filter(subl_harv_indicator==0) 
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(uniform=runif(dplyr::n(), min=0, max=1)) %>% 
        dplyr::arrange(uniform) %>%
        dplyr::ungroup()
      
      n_row_re_allocate<-nrow(catch_size_data_re_allocate)
      n_sub_sf_kept=round(p_rel_to_keep_md*n_row_re_allocate)  
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
        dplyr::mutate(keep_new=dplyr::case_when(fishid2<=n_sub_sf_kept~1, TRUE~ 0))
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(rel_new=dplyr::case_when(keep_new==0~1, TRUE~ 0)) %>% 
        dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
        dplyr::rename(keep=keep_new, release=rel_new)
      
      
      catch_size_data<- plyr::rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
        dplyr::select(-subl_harv_indicator)
      
      n_legal_sf_rel<-0
      
    }
    
    # length data 
    catch_size_data <- data.table::as.data.table(catch_size_data)
    
    new_size_data <- catch_size_data[, .(
      keep = sum(keep),
      release = sum(release)
    ), by = .(mode, date, catch_draw, tripid, fitted_length)]
    
    keep_size_data <- new_size_data %>%
      dplyr::select(-release) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "keep_sf_{fitted_length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    release_size_data <- new_size_data %>%
      dplyr::select(-keep) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "release_sf_{fitted_length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    keep_release_list_sf[[md]] <- keep_size_data %>%
      dplyr::left_join(release_size_data, by = c("date","mode", "tripid", "catch_draw")) 
    
    #End length data SF
    
    
    trip_data <- catch_size_data %>%
      dplyr::group_by(date, catch_draw, tripid, mode) %>%
      dplyr::summarize(tot_keep_sf_new = sum(keep),
                       tot_rel_sf_new = sum(release),
                       .groups = "drop") %>%
      dplyr::ungroup()
    
    sf_zero_catch<-sf_zero_catch0 %>%
      dplyr::filter(mode==md) %>% 
      dplyr::select(date, catch_draw, tripid, mode) %>%
      dplyr::mutate(tot_keep_sf_new=0,
                    tot_rel_sf_new=0)
    
    sf_trip_data <- dplyr::bind_rows(trip_data, sf_zero_catch) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::select(c("date", "catch_draw","tripid","mode",
                      "tot_keep_sf_new","tot_rel_sf_new"))
    
    mode_list_sf[[md]]<- sf_trip_data %>% 
      dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))
    
  }
  
  sf_trip_data <- dplyr::bind_rows(mode_list_sf)
  sf_trip_data<-data.table::as.data.table(sf_trip_data)
  data.table::setkey(sf_trip_data, "domain2")
  
  keep_release_sf <- dplyr::bind_rows(keep_release_list_sf)
  keep_release_sf<-data.table::as.data.table(keep_release_sf)
  
  # remove unneccessary and large dataset
  rm_list <- c("catch_size_data", "catch_size_data_re_allocate", "catch_size_data_re_allocate_base",
               "catch_size_data0", "keep_release_list_sf", "keep_size_data", "new_size_data","release_size_data", 
               "sf_catch_data", "trip_data", "sf_zero_catch0") 
  
  rm(list = rm_list)
  
  
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
                                         replace = TRUE)) 
  
  # Impose regulations, calculate keep and release per trip
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=bsb_min_y2 ,1,0)) %>%
    dplyr::group_by(tripid, date, mode, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        bsb_bag_y2 > 0 ~ ifelse(csum_keep<=bsb_bag_y2 & posskeep==1,1,0),
        TRUE ~ 0))
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0)) %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) 
  
  # code for trip level harvest re-allocation
  catch_size_data0<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
    dplyr::mutate(subl_harv_indicator=dplyr::case_when(release==1 & fitted_length>=floor_subl_bsb_harv~1,TRUE~0))
  
  keep_release_list_bsb<- list()
  mode_list_bsb <- list()
  mode_draw <- c("sh", "pr", "fh")
  
  for (md in mode_draw) {
    
    catch_size_data<- catch_size_data0 %>%
      dplyr::filter(mode==md)
    
    sum_bsb_rel<-sum(catch_size_data$release)
    sum_bsb_keep<-sum(catch_size_data$keep)
    
    calib_comparison_md<-calib_comparison %>% 
      dplyr::filter(species=="bsb" & mode==md)
    
    rel_to_keep_md<- mean(calib_comparison_md$rel_to_keep)
    p_rel_to_keep_md<- mean(calib_comparison_md$p_rel_to_keep)
    
    
    # reallocate a portion of all releases as kept if needed 
    
    if (rel_to_keep_md==1 & sum_bsb_rel>0){
      
      catch_size_data_re_allocate<- catch_size_data %>%
        dplyr::filter(subl_harv_indicator==1) 
      
      catch_size_data_re_allocate_base<- catch_size_data %>%
        dplyr::filter(subl_harv_indicator==0) 
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(uniform=runif(dplyr::n(), min=0, max=1)) %>% 
        dplyr::arrange(uniform) %>%
        dplyr::ungroup()
      
      n_row_re_allocate<-nrow(catch_size_data_re_allocate)
      n_sub_bsb_kept=round(p_rel_to_keep_md*n_row_re_allocate)  
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
        dplyr::mutate(keep_new=dplyr::case_when(fishid2<=n_sub_bsb_kept~1, TRUE~ 0))
     
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(rel_new=dplyr::case_when(keep_new==0~1, TRUE~ 0)) %>% 
        dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
        dplyr::rename(keep=keep_new, release=rel_new)
      
      
      catch_size_data<- plyr::rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
        dplyr::select(-subl_harv_indicator)
      
      n_legal_bsb_rel<-0
      
    }
    
    # length data 
    catch_size_data <- data.table::as.data.table(catch_size_data)
    
    new_size_data <- catch_size_data[, .(
      keep = sum(keep),
      release = sum(release)
    ), by = .(mode, date, catch_draw, tripid, fitted_length)]
    
    keep_size_data <- new_size_data %>%
      dplyr::select(-release) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "keep_bsb_{fitted_length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    release_size_data <- new_size_data %>%
      dplyr::select(-keep) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "release_bsb_{fitted_length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    keep_release_list_bsb[[md]] <- keep_size_data %>%
      dplyr::left_join(release_size_data, by = c("date","mode", "tripid", "catch_draw")) 
    
    #End length data BSB
    
    
    trip_data <- catch_size_data %>%
      dplyr::group_by(date, catch_draw, tripid, mode) %>%
      dplyr::summarize(tot_keep_bsb_new = sum(keep),
                       tot_rel_bsb_new = sum(release),
                       .groups = "drop") %>%
      dplyr::ungroup()
    
    bsb_zero_catch<-bsb_zero_catch0 %>%
      dplyr::filter(mode==md) %>% 
      dplyr::select(date, catch_draw, tripid, mode) %>%
      dplyr::mutate(tot_keep_bsb_new=0,
                    tot_rel_bsb_new=0)
    
    bsb_trip_data <- dplyr::bind_rows(trip_data, bsb_zero_catch) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::select(c("date", "catch_draw","tripid","mode",
                      "tot_keep_bsb_new","tot_rel_bsb_new"))
    
    mode_list_bsb[[md]]<- bsb_trip_data %>% 
      dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))%>% 
      dplyr::select(-date, -mode, -catch_draw,-tripid)
    
  }
  
  bsb_trip_data <- dplyr::bind_rows(mode_list_bsb)
  bsb_trip_data<-data.table::as.data.table(bsb_trip_data)
  data.table::setkey(bsb_trip_data, "domain2")
  
  keep_release_bsb <- dplyr::bind_rows(keep_release_list_bsb)
  keep_release_bsb<-data.table::as.data.table(keep_release_bsb)
  
  
  rm_list <- c("catch_size_data", "catch_size_data_re_allocate", "catch_size_data_re_allocate_base",
               "catch_size_data0", "keep_release_list_bsb", "keep_size_data", "new_size_data","release_size_data", 
               "bsb_catch_data", "trip_data", "bsb_zero_catch0") 
  rm(list = rm_list)
  
  
  
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
                                         replace = TRUE)) 
  
  
  # Impose regulations, calculate keep and release per trip
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(posskeep = ifelse(fitted_length>=scup_min_y2 ,1,0)) %>%
    dplyr::group_by(tripid, date, mode, catch_draw) %>%
    dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      keep_adj = dplyr::case_when(
        scup_bag > 0 ~ ifelse(csum_keep<=scup_bag_y2 & posskeep==1,1,0),
        TRUE ~ 0))
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
  
  catch_size_data <- catch_size_data %>%
    dplyr::mutate(keep = keep_adj,
                  release = ifelse(keep==0,1,0)) %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode)  
  
  
  # code for trip level harvest re-allocation
  catch_size_data0<- catch_size_data %>%
    dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
    dplyr::mutate(subl_harv_indicator=dplyr::case_when(release==1 & fitted_length>=floor_subl_scup_harv~1,TRUE~0))
  
  keep_release_list_scup<- list()
  mode_list_scup <- list()
  mode_draw <- c("sh", "pr", "fh")
  
  for (md in mode_draw) {
    
    catch_size_data<- catch_size_data0 %>%
      dplyr::filter(mode==md)
    
    sum_scup_rel<-sum(catch_size_data$release)
    sum_scup_keep<-sum(catch_size_data$keep)
    
    calib_comparison_md<-calib_comparison %>% 
      dplyr::filter(species=="scup" & mode==md)
    
    rel_to_keep_md<- mean(calib_comparison_md$rel_to_keep)
    p_rel_to_keep_md<- mean(calib_comparison_md$p_rel_to_keep)
    
    # reallocate a portion of all releases as kept if needed 
    
    if (rel_to_keep_md==1 & sum_scup_rel>0){
      
      catch_size_data_re_allocate<- catch_size_data %>%
        dplyr::filter(subl_harv_indicator==1) 
      
      catch_size_data_re_allocate_base<- catch_size_data %>%
        dplyr::filter(subl_harv_indicator==0) 
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(uniform=runif(dplyr::n(), min=0, max=1)) %>% 
        dplyr::arrange(uniform) %>%
        dplyr::ungroup()
      
      n_row_re_allocate<-nrow(catch_size_data_re_allocate)
      n_sub_scup_kept=round(p_rel_to_keep_md*n_row_re_allocate)  
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
        dplyr::mutate(keep_new=dplyr::case_when(fishid2<=n_sub_scup_kept~1, TRUE~ 0))
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(rel_new=dplyr::case_when(keep_new==0~1, TRUE~ 0)) %>% 
        dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
        dplyr::rename(keep=keep_new, release=rel_new)
      
      
      catch_size_data<- plyr::rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
        dplyr::select(-subl_harv_indicator)
      
      n_legal_scup_rel<-0
      
    }
    
    # length data 
    catch_size_data <- data.table::as.data.table(catch_size_data)
    
    new_size_data <- catch_size_data[, .(
      keep = sum(keep),
      release = sum(release)
    ), by = .(mode, date, catch_draw, tripid, fitted_length)]
    
    keep_size_data <- new_size_data %>%
      dplyr::select(-release) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "keep_scup_{fitted_length}",
                         names_sort = TRUE,
                         values_from = keep,
                         values_fill = 0)
    
    release_size_data <- new_size_data %>%
      dplyr::select(-keep) %>%
      tidyr::pivot_wider(names_from = fitted_length, #_length,
                         names_glue = "release_scup_{fitted_length}",
                         names_sort = TRUE,
                         values_from = release,
                         values_fill = 0)
    
    keep_release_list_scup[[md]] <- keep_size_data %>%
      dplyr::left_join(release_size_data, by = c("date","mode", "tripid", "catch_draw")) 
    
    #End length data scup
    
    trip_data <- catch_size_data %>%
      dplyr::group_by(date, catch_draw, tripid, mode) %>%
      dplyr::summarize(tot_keep_scup_new = sum(keep),
                       tot_rel_scup_new = sum(release),
                       .groups = "drop") %>%
      dplyr::ungroup()
    
    scup_zero_catch<-scup_zero_catch0 %>%
      dplyr::filter(mode==md) %>% 
      dplyr::select(date, catch_draw, tripid, mode) %>%
      dplyr::mutate(tot_keep_scup_new=0,
                    tot_rel_scup_new=0)
    
    scup_trip_data <- dplyr::bind_rows(trip_data, scup_zero_catch) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::select(c("date", "catch_draw","tripid","mode",
                      "tot_keep_scup_new","tot_rel_scup_new"))
    
    mode_list_scup[[md]]<- scup_trip_data %>% 
      dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) %>% 
      dplyr::select(-date, -mode, -catch_draw,-tripid)
    
  }
  
  scup_trip_data <- dplyr::bind_rows(mode_list_scup)
  scup_trip_data<-data.table::as.data.table(scup_trip_data)
  data.table::setkey(scup_trip_data, "domain2")
  
  keep_release_scup <- dplyr::bind_rows(keep_release_list_scup)
  keep_release_scup<-data.table::as.data.table(keep_release_scup)
  
  
  rm_list <- c("catch_size_data", "catch_size_data_re_allocate", "catch_size_data_re_allocate_base",
               "catch_size_data0", "keep_release_list_scup", "keep_size_data", "new_size_data","release_size_data", 
               "scup_catch_data", "trip_data", "scup_zero_catch0") 
  
  # Remove everything else
  rm(list = rm_list)
  
  
  # merge the hadd trip data with the rest of the trip data
  trip_data<- sf_trip_data[bsb_trip_data, on = "domain2"]
  trip_data<- trip_data[scup_trip_data, on = "domain2"]
  
  trip_data<- trip_data %>% 
    dplyr::mutate(tot_cat_scup_new = tot_keep_scup_new + tot_rel_scup_new, 
                  tot_cat_bsb_new = tot_keep_bsb_new + tot_rel_bsb_new, 
                  tot_cat_sf_new = tot_keep_sf_new + tot_rel_sf_new) %>% 
    data.table::as.data.table()
  #}
  
  rm(catch_data)
  
  # Combine all length data
  #If there is catch of both species:
  if(sf_catch_check !=0 & bsb_catch_check!=0 & scup_catch_check!=0){
    
    # Convert to data.table
    data.table::setDT(keep_release_sf)
    data.table::setDT(keep_release_bsb)
    data.table::setDT(keep_release_scup)
    data.table::setDT(sf_zero_catch)
    data.table::setDT(bsb_zero_catch)
    data.table::setDT(scup_zero_catch)
    
    # First merge sf and bsb
    length_temp <- merge(keep_release_sf, keep_release_bsb,
                         by = c("date", "mode", "tripid", "catch_draw"),
                         all = TRUE)
    
    # Then merge the result with scup
    length_data <- merge(length_temp, keep_release_scup,
                         by = c("date", "mode", "tripid", "catch_draw"),
                         all = TRUE)
    
    #First merge sf and bsb zero catches
    zero_catch_temp<- merge(sf_zero_catch, bsb_zero_catch,
                            by = c("date", "mode", "tripid", "catch_draw"),
                            all = TRUE)
    
    # Then merge the zero catches result with scup
    zero_catch_check <- merge(zero_catch_temp, scup_zero_catch,
                              by = c("date", "mode", "tripid", "catch_draw"),
                              all = TRUE)[
                                tot_keep_sf_new == 0 & tot_rel_sf_new == 0 & 
                                  tot_keep_bsb_new == 0 & tot_rel_bsb_new == 0 & 
                                  tot_keep_scup_new == 0 & tot_rel_scup_new == 0, 
                                .(date, mode, tripid, catch_draw)
                              ]
    
    
    # Bind rows (rbindlist is faster and more memory-efficient)
    length_data <- data.table::rbindlist(list(length_data, zero_catch_check), fill = TRUE) 
    
    
    # Replace NA values with 0 again (if necessary)
    length_data[is.na(length_data)] <- 0
    
    rm(sf_zero_catch,bsb_zero_catch,scup_zero_catch,zero_catch_check )
    
    length_data<-data.table::as.data.table(length_data) 
  }
  
  # #If there is no catch of any species 
  # if(sf_catch_check ==0 & bsb_catch_check==0 & scup_catch_check==0){
  #   length_data <- trip_data %>%  
  #     dplyr::select("date","mode", "tripid", "catch_draw") %>% 
  #     dplyr::mutate(keep_sf_1=0, release_sf_1=0, keep_bsb_1=0, release_bsb_1=0, keep_scup_1=0, release_scup_1=0)
  #   
  # }
  # 
  # #If there is catch of only sf 
  # if(sf_catch_check !=0 & bsb_catch_check==0){
  #   
  #   keep_release_cod<-trip_data %>% 
  #     dplyr::select("period2","tripid", "catch_draw") %>% 
  #     dplyr::mutate(keep_cod_1=0, release_cod_1=0)
  #   
  #   length_data <- keep_release_hadd %>%
  #     dplyr::full_join(keep_release_cod, by = c("period2","tripid", "catch_draw"))
  #   
  #   length_data[is.na(length_data)] <- 0
  #   
  #   
  # }
  # 
  # #If there is catch of only cod 
  # if(cod_catch_check !=0 & had_catch_check==0){
  #   
  #   keep_release_hadd<-trip_data %>% 
  #     dplyr::select("period2","tripid", "catch_draw") %>% 
  #     dplyr::mutate(keep_had_1=0, release_had_1=0)
  #   
  #   length_data <- keep_release_cod %>%
  #     dplyr::full_join(keep_release_hadd, by = c("period2","tripid", "catch_draw"))
  #   
  #   length_data[is.na(length_data)] <- 0
  #   
  # }
  
  
  
  # merge to the baseline-year trip outcomes 
  trip_data<-trip_data %>% 
    dplyr::select(-domain2) %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
  length_data<-length_data %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
  trip_data<- trip_data[base_outcomes, on = c("date_parsed", "mode", "tripid", "catch_draw")]
  trip_data<- trip_data[length_data, on = c("date_parsed", "mode", "tripid", "catch_draw")]
  
  rm(sf_trip_data, scup_trip_data, bsb_trip_data, length_temp, keep_release_bsb, keep_release_scup,
     keep_release_sf, zero_catch_temp, base_outcomes, length_data)
  
  
  # compute utility/choice probabilites/welfare
  trip_data <-trip_data %>%
    dplyr::mutate(
      vA = beta_sqrt_sf_keep*sqrt(tot_keep_sf_new) +
        beta_sqrt_sf_release*sqrt(tot_rel_sf_new) +
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_new) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb_new) +
        beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf_new)*sqrt(tot_keep_bsb_new)) +
        beta_sqrt_scup_catch*sqrt(tot_cat_scup_new) +
        beta_cost*cost, 
      
      v0 = beta_sqrt_sf_keep*sqrt(tot_keep_sf_base) +
        beta_sqrt_sf_release*sqrt(tot_rel_sf_base) +
        beta_sqrt_bsb_keep*sqrt(tot_keep_bsb_base) +
        beta_sqrt_bsb_release*sqrt(tot_rel_bsb_base) +
        beta_sqrt_sf_bsb_keep*(sqrt(tot_keep_sf_base)*sqrt(tot_keep_bsb_base)) +
        beta_sqrt_scup_catch*sqrt(tot_cat_scup_base) +
        beta_cost*cost)
  
  
  mean_trip_data <- trip_data %>% data.table::data.table() %>% 
    .[, group_index := .GRP, by = .(date_parsed, mode, catch_draw, tripid)]
  
  # expand the data to create two alternatives, representing the alternatives available in choice survey
  mean_trip_data <- mean_trip_data %>%
    dplyr::mutate(n_alt = rep(2,nrow(.))) %>%
    tidyr::uncount(n_alt) %>%
    dplyr::mutate(alt = rep(1:2,nrow(.)/2),
                  opt_out = ifelse(alt == 2, 1, 0))
  
  #Calculate the expected utility of alts 2 parameters of the utility function,
  #put the two values in the same column, exponentiate, and calculate their sum (vA_col_sum)
  
  data.table::setDT(mean_trip_data)
  
  # Filter only alt == 2 once, and calculate vA and v0
  mean_trip_data[alt == 2, c("vA", "v0") := .(
    beta_opt_out * opt_out +
      beta_opt_out_age * (age * opt_out) +
      beta_opt_out_avidity * (total_trips_12 * opt_out) 
  )]
  
  # Pre-compute exponential terms
  mean_trip_data[, `:=`(exp_vA = exp(vA), exp_v0 = exp(v0))]
  
  # Group by group_index and calculate probabilities and log-sums
  mean_trip_data[, `:=`(
    probA = exp_vA / sum(exp_vA),
    prob0 = exp_v0 / sum(exp_v0),
    log_sum_base = log(sum(exp_vA)),
    log_sum_alt = log(sum(exp_v0))
  ), by = group_index]
  
  # Calculate consumer surplus 
  mean_trip_data[, `:=`(
    CS_base = log_sum_base / -beta_cost,
    CS_alt = log_sum_alt / -beta_cost
  )]
  
  # Calculate change consumer surplus 
  mean_trip_data[, `:=`(
    change_CS = CS_alt - CS_base
  )]
  
  
  mean(mean_trip_data$change_CS)
  
  # Get rid of things we don't need.
  mean_trip_data <- mean_trip_data %>% 
    dplyr::filter(alt==1) %>% 
    dplyr::select(-matches("beta")) %>% 
    dplyr::select(-"alt", -"opt_out", -"vA" , -"v0",-"exp_v0", -"exp_vA", 
                  -"cost", -"age", -"total_trips_12", -"catch_draw", -"group_index", 
                  -"log_sum_alt", -"log_sum_base") 
  
  all_vars<-c()
  all_vars <- names(mean_trip_data)[!names(mean_trip_data) %in% c("date_parsed","mode", "tripid")]
  all_vars
  
  # average outcomes across draws
  mean_trip_data<-mean_trip_data  %>% data.table::as.data.table() %>%
    .[,lapply(.SD, mean), by = c("date_parsed","mode", "tripid"), .SDcols = all_vars]
  
  # multiply the average trip probability in the new scenario (probA) by each catch variable to get probability-weighted catch
  list_names <- c("tot_keep_sf_new",   "tot_rel_sf_new",  "tot_cat_sf_new", 
                  "tot_keep_bsb_new",  "tot_rel_bsb_new", "tot_cat_bsb_new",  
                  "tot_keep_scup_new","tot_rel_scup_new",  "tot_cat_scup_new")
  
  pattern_vars <- grep("^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$", 
                       names(mean_trip_data), value = TRUE)
  
  # Combine
  all_vars <- c(list_names, pattern_vars)
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(all_vars) := lapply(.SD, function(x) x * probA), .SDcols = all_vars] %>%
    .[]
  
  # multiply the average trip probability in the base scenario (prob0) by each catch variable to get probability-weighted catch
  list_names <- c("tot_keep_sf_base",   "tot_rel_sf_base",  "tot_cat_sf_base", 
                  "tot_keep_bsb_base",  "tot_rel_bsb_base", "tot_cat_bsb_base",  
                  "tot_keep_scup_base","tot_rel_scup_base",  "tot_cat_scup_base")
  
  mean_trip_data <- mean_trip_data %>%
    data.table::as.data.table() %>%
    .[,as.vector(list_names) := lapply(.SD, function(x) x * probA), .SDcols = list_names] %>%
    .[]
  
  # select the same number of choice occasions in the prediction year as in the calibration year
  mean_trip_data<-mean_trip_data %>% 
    dplyr::left_join(n_choice_occasions, by = c("mode", "date_parsed"))
  
  mean_trip_data <-mean_trip_data %>% 
    dplyr::mutate(n_sim_choices=1) %>% 
    dplyr::group_by(date_parsed, mode) %>% 
    dplyr::mutate(sum_n_sim_choices=sum(n_sim_choices)) %>% 
    dplyr::ungroup()
  
  mean_trip_data <- mean_trip_data %>% 
    dplyr::mutate(uniform=runif(dplyr::n(), min=0, max=1)) %>% 
    dplyr::arrange(date_parsed, mode, uniform)
  
  mean_trip_data1 <- mean_trip_data %>% 
    dplyr::group_by(date_parsed, mode) %>%
    dplyr::mutate(id_within_group = dplyr::row_number()) %>% 
    dplyr::filter(n_choice_occasions<=sum_n_sim_choices & id_within_group<=n_choice_occasions) %>% 
    dplyr::ungroup()
  
  mean_trip_data2 <- mean_trip_data %>% 
    dplyr::filter(n_choice_occasions>sum_n_sim_choices)  %>% 
    dplyr::mutate(expand=round(n_choice_occasions/sum_n_sim_choices)+1)
  
  row_inds <- seq_len(nrow(mean_trip_data2))
  
  mean_trip_data2<-mean_trip_data2 %>% 
    dplyr::slice(rep(row_inds,expand))  
  
  mean_trip_data2 <- mean_trip_data2 %>%
    dplyr::mutate(uniform=runif(dplyr::n(), min=0, max=1)) %>% 
    dplyr::arrange(date_parsed, mode, uniform) %>% 
    dplyr::group_by(date_parsed, mode) %>%
    dplyr::mutate(id_within_group = dplyr::row_number()) %>% 
    dplyr::filter(id_within_group<=n_choice_occasions)
  
  results<-mean_trip_data1 %>% 
    dplyr::bind_rows(mean_trip_data2) %>% 
    dplyr::rename(n_trips_base=prob0, 
                  n_trips_alt=probA) %>% 
    dplyr::select(-uniform, id_within_group, -expand)
  
  rm(mean_trip_data1, mean_trip_data2, trip_data)
  
  
  # aggregate welfare and fishing effort estimates
  
  list_names = c("change_CS",  "n_trips_alt")
  
  aggregate_trip_data_mode <- results %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum),  by = c("mode"), .SDcols = list_names]
  
  aggregate_trip_data_allmodes <- results %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, sum),  .SDcols = list_names] %>% 
    dplyr::mutate(mode="all modes")
  
  model_output1<-aggregate_trip_data_mode %>% 
    dplyr::bind_rows(aggregate_trip_data_allmodes) %>% 
    dplyr::select(change_CS, n_trips_alt, mode) %>% 
    dplyr::rename(CV=change_CS, ntrips=n_trips_alt)
  
  
  # model_output1 contains CV and ntrips estimates
  model_output1<- model_output1 %>%
    tidyr::pivot_longer(!c(mode), names_to = "var", values_to = "value") %>%
    dplyr::mutate(category=dplyr::case_when(var=="CV"~"CV",TRUE ~ "predicted trips"), 
                  keep_release="N/A", param="N/A", 
                  number_weight=dplyr::case_when(var=="CV"~"dollars",TRUE ~ "trips") ) %>% 
    dplyr::select(-var)
  
  
  # use the lengths of fish harvested and released to compute weights
  pattern_vars <- grep("^keep_(sf_|bsb_|scup_)[0-9.]*$|^release_(sf_|bsb_|scup_)[0-9.]*$", 
                       names(results), value = TRUE)
  
  length_data1 <- results %>%
    dplyr::select(date_parsed, mode,  all_of(pattern_vars)) %>% 
    dplyr::mutate(month=lubridate::month(date_parsed))
  
  length_data1 <- length_data1 %>%
    data.table::as.data.table() %>%
    .[,lapply(.SD, base::sum), by = c("mode", "month"), .SDcols = pattern_vars]
  
  length_data1<- length_data1 %>%
    tidyr::pivot_longer(cols = !month & !mode,  names_to = "Var", values_to = "number_at_length") %>%
    tidyr::separate(Var, into = c("keep_release", "species", "length"), sep = "_") %>%
    dplyr::left_join(l_w_conversion, by = c("month", "species")) %>%
    dplyr::mutate(length_in = as.numeric(length),
                  ln_a = as.numeric(ln_a),
                  ln_b = as.numeric(ln_b),
                  a = as.numeric(a),
                  b = as.numeric(b),
                  length_cm = length_in*2.54)  %>%  #Convert to cm
    dplyr::mutate(ln_a = ifelse(is.na(ln_a), 0, ln_a)) %>% 
    dplyr::mutate(a = ifelse(is.na(a), 0, a)) %>%
    dplyr::mutate(b = ifelse(is.na(b), 0, b)) %>%
    dplyr::mutate(ln_b = ifelse(is.na(ln_b), 0, ln_b))  %>%   
    dplyr::mutate(weight = dplyr::case_when(species == "scup" ~ exp(ln_a + ln_b*log(length_cm)))) %>% 
    dplyr::mutate(weight = dplyr::case_when(species == "sf" ~ a*length_cm^b, TRUE ~ weight))  %>% 
    dplyr::mutate(weight = dplyr::case_when(species == "bsb" ~ a*length_cm^b, TRUE ~ weight),
                  weight = weight*2.20462262185, #convert to lbs
                  total_weight = number_at_length * weight,
                  discmort_weight = dplyr::case_when(keep_release == "release" & species == "sf" ~ (.1 * number_at_length * weight)),
                  discmort_weight = dplyr::case_when(keep_release == "release" & species == "scup" ~ (.15 * number_at_length * weight), TRUE ~ discmort_weight),
                  discmort_weight = dplyr::case_when(keep_release == "release" & species == "bsb" ~ (.15 * number_at_length * weight), TRUE ~ discmort_weight),
                  discmort_number = dplyr::case_when(keep_release == "release" & species == "sf" ~ (.1 * number_at_length)),
                  discmort_number = dplyr::case_when(keep_release == "release" & species == "scup" ~ (.15 * number_at_length), TRUE ~ discmort_number),
                  discmort_number = dplyr::case_when(keep_release == "release" & species == "bsb" ~ (.15 * number_at_length), TRUE ~ discmort_number))  %>%
    dplyr::group_by(species, mode, keep_release) %>%
    dplyr::summarise(total_numbers = sum(number_at_length),
                     total_weight = sum(total_weight),
                     discmort_weight = sum(discmort_weight),
                     discmort_number = sum(discmort_number),.groups = 'drop') %>%
    dplyr::ungroup() 
  
  length_data_long <- length_data1 %>%
    dplyr::mutate(var1 = paste0(species, "_", mode, "_", keep_release )) %>%
    dplyr::select(var1, total_numbers, total_weight, discmort_weight, discmort_number) %>%
    tidyr::pivot_longer(!var1, names_to = "var", values_to = "value") %>%
    dplyr::mutate(var = paste0(var1,"_",var)) %>%
    dplyr::select(!var1) %>%
    dplyr::filter(is.na(value)==FALSE) %>% 
    tidyr::separate(var, into = c("category","mode", "keep_release", "param", "number_weight")) 
  
  length_data_long_all<-length_data_long %>% 
    dplyr::group_by(category, keep_release,param, number_weight) %>% 
    dplyr::summarise(value=sum(value),.groups = 'drop') %>% 
    dplyr::mutate(mode="all modes")
  
  length_output<-length_data_long_all %>% 
    dplyr::bind_rows(length_data_long)
  
  # combine catch and welfare results 
  
  predictions <- plyr::rbind.fill(length_output, model_output1) %>%
    dplyr::select(category, mode, everything() ) %>% 
    dplyr::mutate(state = st, draw=dr)
      
  print("Finished predict_rec_catch")
      
  return(predictions) 
}


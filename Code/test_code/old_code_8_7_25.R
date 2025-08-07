
#test code for predict rec catch
iterative_input_data_cd="C:/Users/andrew.carr-harris/Desktop/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

#check

############# To Run Individual
# Variables to change 
dr<-1
st="MA"


library(magrittr)
############# To Run in Loop 

#for (st in c("MA", "RI")){
#  for (dr in 1:2){

# import necessary data

# For kim: We need to retain the SQ regulation variables. 
#          As of now, the SQ regulations variables are the ones without subscripts. 
#          I will copy these variables with the subscript _SQ to make this explicit.
#          The alternative regulations that will be adjusted by the users will be 
#          have subscripts _y2 (note this is slightly different from cod and haddock 2024)


directed_trips<-feather::read_feather(file.path(input_data_cd, paste0("directed_trips_calibration_", st, ".feather"))) %>% 
  tibble::tibble() %>%
  dplyr::filter(draw == dr) %>%
  dplyr::select(mode, date, 
                bsb_bag, bsb_min, bsb_bag_y2, bsb_min_y2, 
                fluke_bag, fluke_min, fluke_bag_y2,fluke_min_y2,
                scup_bag, scup_min, scup_bag_y2, scup_min_y2) %>% 
  dplyr::mutate(fluke_min_SQ=fluke_min, fluke_bag_SQ=fluke_bag, 
                bsb_min_SQ=bsb_min, bsb_bag_SQ=bsb_bag, 
                scup_min_SQ=scup_min, scup_bag_SQ=scup_bag)


catch_data <- feather::read_feather(file.path(iterative_input_data_cd, paste0("projected_catch_draws_",st, "_", dr,".feather"))) %>% 
  dplyr::left_join(directed_trips, by=c("mode", "date")) 

catch_data<-catch_data %>% 
  dplyr::select(-cost, -total_trips_12, -age, -bsb_keep_sim, -bsb_rel_sim, -day_i, -my_dom_id_string, 
                -scup_keep_sim, -scup_rel_sim, -sf_keep_sim, -sf_rel_sim, -wave)

l_w_conversion <- readr::read_csv(file.path("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Data", "L_W_Conversion.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state==st)


sf_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="sf", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

bsb_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="bsb" , draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state, fitted_prob, length)

scup_size_data <- read_csv(file.path(input_data_cd, "baseline_catch_at_length.csv"), show_col_types = FALSE)  %>% 
  dplyr::filter(state == st, species=="scup", draw==dr) %>% 
  dplyr::filter(!is.na(fitted_prob)) %>% 
  dplyr::select(state,  fitted_prob, length)

# base-year trip outcomes
base_outcomes0 <- list()
n_choice_occasions0 <- list()

mode_draw <- c("sh", "pr", "fh")
for (md in mode_draw) {
  
  # pull trip outcomes from the calibration year
  base_outcomes0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("base_outcomes_", st, "_", md, "_", dr, ".feather"))) %>% 
    data.table::as.data.table()
  
  base_outcomes0[[md]]<-base_outcomes0[[md]] %>% 
    dplyr::select(-domain2) %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
  # pull in data on the number of choice occasions per mode-day
  n_choice_occasions0[[md]]<-feather::read_feather(file.path(iterative_input_data_cd, paste0("n_choice_occasions_MA_", md, "_", dr, ".feather")))  
  n_choice_occasions0[[md]]<-n_choice_occasions0[[md]] %>% 
    dplyr::mutate(date_parsed=lubridate::dmy(date)) %>% 
    dplyr::select(-date)
  
}

base_outcomes <- bind_rows(base_outcomes0)
n_choice_occasions <- bind_rows(n_choice_occasions0) %>% 
  dplyr::arrange(date_parsed, mode)
rm(base_outcomes0, n_choice_occasions0)

base_outcomes<-base_outcomes %>% 
  dplyr::arrange(date_parsed, mode, tripid, catch_draw)




# Pull in calibration comparison information about trip-level harvest/discard re-allocations 
calib_comparison<-readRDS(file.path(iterative_input_data_cd, "calibrated_model_stats.rds")) %>% 
  dplyr::filter(state==st & draw==dr ) 

calib_comparison<-calib_comparison %>% 
  dplyr::rename(n_legal_rel_bsb=n_legal_bsb_rel, 
                n_legal_rel_scup=n_legal_scup_rel, 
                n_legal_rel_sf=n_legal_sf_rel, 
                n_sub_kept_bsb=n_sub_bsb_kept,
                n_sub_kept_sf=n_sub_sf_kept,
                n_sub_kept_scup=n_sub_scup_kept,
                prop_legal_rel_bsb=prop_legal_bsb_rel,
                prop_legal_rel_sf=prop_legal_sf_rel,
                prop_legal_rel_scup=prop_legal_scup_rel,
                prop_sub_kept_bsb=prop_sub_bsb_kept,
                prop_sub_kept_sf=prop_sub_sf_kept,
                prop_sub_kept_scup=prop_sub_scup_kept,
                convergence_sf=sf_convergence,
                convergence_bsb=bsb_convergence,
                convergence_scup=scup_convergence) 

##########
# List of species suffixes
species_suffixes <- c("sf", "bsb", "scup")

# Get all variable names
all_vars <- names(calib_comparison)

# Identify columns that are species-specific (contain _sf, _bsb, or _scup)
species_specific_vars <- all_vars[
  str_detect(all_vars, paste0("(_", species_suffixes, ")$", collapse = "|"))
]

id_vars <- setdiff(all_vars, species_specific_vars)

calib_comparison<-calib_comparison %>% 
  dplyr::select(mode, all_of(species_specific_vars))

# Extract base variable names (without _sf, _bsb, _scup)
base_names <- unique(str_replace(species_specific_vars, "_(sf|bsb|scup)$", ""))

# Pivot the data longer on the species-specific columns
calib_comparison <- calib_comparison %>%
  pivot_longer(
    cols = all_of(species_specific_vars),
    names_to = c(".value", "species"),
    names_pattern = "(.*)_(sf|bsb|scup)"
  ) %>% 
  dplyr::distinct()


#  }
#}



set.seed(12345)
# this is the "old" code 8/7/25

mode_list_sf<-list() 
keep_release_list_sf <-list()  
zero_catch_list_sf <-list()

floor_subl_sf_harv<-min(directed_trips$fluke_min)-3*2.54
floor_subl_bsb_harv<-min(directed_trips$bsb_min)-3*2.54
floor_subl_scup_harv<-min(directed_trips$scup_min)-3*2.54

mode_draw <- c("sh", "pr", "fh")

for (md in mode_draw) {
  
  calib_comparison_md<-calib_comparison %>% 
    dplyr::filter(mode==md) 
  
  for (p in 1:nrow(calib_comparison_md)) {
    sp <- calib_comparison_md$species[p]
    
    assign(paste0("rel_to_keep_", sp), calib_comparison_md$rel_to_keep[p])
    assign(paste0("keep_to_rel_", sp), calib_comparison_md$keep_to_rel[p])
    
    if (calib_comparison_md$rel_to_keep[p] == 1) {
      assign(paste0("p_rel_to_keep_", sp), calib_comparison_md$p_rel_to_keep[p])
      assign(paste0("p_keep_to_rel_", sp), 0)
      assign(paste0("prop_sublegal_kept_", sp), calib_comparison_md$prop_sub_kept[p])
      
    }
    
    if (calib_comparison_md$keep_to_rel[p] == 1) {
      assign(paste0("p_keep_to_rel_", sp), calib_comparison_md$p_keep_to_rel[p])
      assign(paste0("p_rel_to_keep_", sp), 0)
      assign(paste0("prop_legal_rel_", sp), calib_comparison_md$prop_legal_rel[p])
      
    }
  }
  
  all_keep_to_rel_sf<-case_when(p_keep_to_rel_sf==1~1, TRUE~0)
  all_keep_to_rel_bsb<-case_when(p_keep_to_rel_bsb==1~1, TRUE~0)
  all_keep_to_rel_scup<-case_when(p_keep_to_rel_scup==1~1, TRUE~0)
  
  ######  Begin trip simulation  ######  
  # filter out the catch data for mode==md
  
  catch_data_md<- catch_data %>% 
    dplyr::filter(mode==md)
  
  # subset trips with zero catch, as no size draws are required
  zero_catch_sf<- dplyr::filter(catch_data_md, sf_cat == 0)
  zero_catch_list_sf[[md]]<-zero_catch_sf
  
  # Check if there is zero catch for any species and if so, pipe code around keep/release determination
  sf_catch_check_md<-base::sum(catch_data_md$sf_cat) 
  
  ### Summer flounder trip simulation
  if (sf_catch_check_md!=0){
    
    # keep trips with positive sf catch
    sf_catch_data <- dplyr::filter(catch_data_md, sf_cat > 0)
    
    row_inds <- seq_len(nrow(sf_catch_data))
    
    sf_catch_data<-sf_catch_data %>%
      dplyr::slice(rep(row_inds, sf_cat))   %>%
      dplyr::mutate(fishid=dplyr::row_number())
    
    # generate lengths for each fish
    catch_size_data_proj <- sf_catch_data %>%
      dplyr::mutate(fitted_length = sample(sf_size_data$length,
                                           nrow(.),
                                           prob = sf_size_data$fitted_prob,
                                           replace = TRUE)) 
    
    # Compute keep and release under projection-year regulations
    
    catch_size_data_proj <- catch_size_data_proj %>%
      dplyr::mutate(posskeep = ifelse(fitted_length>=fluke_min_y2 ,1,0)) %>%
      dplyr::group_by(tripid, date, mode, catch_draw) %>%
      dplyr::mutate(csum_keep = cumsum(posskeep)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        keep_adj = dplyr::case_when(
          fluke_bag_y2 > 0 ~ ifelse(csum_keep<=fluke_bag_y2 & posskeep==1,1,0),
          TRUE ~ 0))
    
    catch_size_data_proj <- catch_size_data_proj %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0)
    
    catch_size_data_proj <- catch_size_data_proj %>%
      dplyr::mutate(keep = keep_adj,
                    release = ifelse(keep==0,1,0)) %>%
      dplyr::select(fishid, fitted_length, tripid, keep, release, date, catch_draw, mode) %>% 
      dplyr::mutate(subl_harv_indicator=case_when(release==1 & fitted_length>=floor_subl_sf_harv~1,TRUE~0)) 
    
    sum_sf_rel<-sum(catch_size_data_proj$release)
    sum_sf_keep<-sum(catch_size_data_proj$keep)
    
    
    # reallocate a portion of all releases as kept if needed 
    if (rel_to_keep_sf==1 & sum_sf_rel>0){
      
      catch_size_data_re_allocate<- catch_size_data_proj %>%
        dplyr::filter(subl_harv_indicator==1) 
      
      catch_size_data_re_allocate_base<- catch_size_data_proj %>%
        dplyr::filter(subl_harv_indicator==0) 
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(uniform=runif(n(), min=0, max=1)) %>% 
        dplyr::arrange(uniform) %>%
        dplyr::ungroup()
      
      n_row_re_allocate<-nrow(catch_size_data_re_allocate)
      
      n_sub_sf_kept=round(prop_sublegal_kept_sf*n_row_re_allocate)  
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(fishid2=1:n_row_re_allocate) %>% 
        dplyr::mutate(keep_new=case_when(fishid2<=n_sub_sf_kept~1, TRUE~ 0))
      
      catch_size_data_re_allocate <- catch_size_data_re_allocate %>% 
        dplyr::mutate(rel_new=case_when(keep_new==0~1, TRUE~ 0)) %>% 
        dplyr::select(-keep, -release, -uniform, -fishid2, -uniform) %>% 
        dplyr::rename(keep=keep_new, release=rel_new)
      
      catch_size_data_proj<- rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base) %>% 
        dplyr::select(-subl_harv_indicator)
      
      rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
      
    }
    
    # Now reallocate a portion of all keeps as releases if needed 
    if (keep_to_rel_sf==1 & sum_sf_keep>0){
      
      # If all kept must be release, p_keep_to_rel_sf==1
      if (all_keep_to_rel_sf==1){
        
        catch_size_data_proj<-catch_size_data_proj %>% 
          dplyr::mutate(rel_new = keep+release, 
                        keep_new = 0) %>% 
          dplyr::select(-keep, -release) %>% 
          dplyr::rename(release=rel_new,  keep=keep_new) 
        
      }
      
      #If not all kept must be release, p_keep_to_rel_sf<1
      if (all_keep_to_rel_sf!=1){
        
        catch_size_data_re_allocate<- catch_size_data_proj %>%
          dplyr::filter(keep==1)
        
        catch_size_data_re_allocate_base<- catch_size_data_proj %>%
          dplyr::filter(keep==0) 
        
        n_row_re_allocate<-nrow(catch_size_data_re_allocate)
        
        catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
          dplyr::mutate(uniform=runif(n_row_re_allocate)) %>%
          dplyr::arrange(uniform) %>% 
          dplyr::mutate(fishid2=1:n_row_re_allocate)
        
        n_kept_sf_rel=round(prop_legal_rel_sf*n_row_re_allocate)
        
        catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
          dplyr::mutate(rel_new=dplyr::case_when(fishid2<=n_kept_sf_rel~1, TRUE~ 0))
        
        catch_size_data_re_allocate<-catch_size_data_re_allocate %>% 
          dplyr::mutate(keep_new=dplyr::case_when(rel_new==0~1, TRUE~ 0)) %>% 
          dplyr::select(-keep, -release, -fishid2, -uniform) %>% 
          dplyr::rename(keep=keep_new, release=rel_new)
        
        catch_size_data_proj<-rbind.fill(catch_size_data_re_allocate,catch_size_data_re_allocate_base )
        
        rm(catch_size_data_re_allocate,catch_size_data_re_allocate_base)
        
        
      }
    }
    
    # length data for fluke 
    catch_size_data_proj <- data.table::as.data.table(catch_size_data_proj)
    
    new_size_data <- catch_size_data_proj[, .(
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
    
    # end length data for fluke 
    
    sf_trip_data <- catch_size_data_proj %>%
      dplyr::group_by(date, catch_draw, tripid, mode) %>%
      dplyr::summarize(tot_keep_sf_new = sum(keep),
                       tot_rel_sf_new = sum(release),
                       .groups = "drop") %>% dplyr::ungroup()
    
    sf_zero_catch<-zero_catch_sf %>%
      dplyr::filter(mode==md) %>% 
      dplyr::select(date, catch_draw, tripid, mode) %>%
      dplyr::mutate(tot_keep_sf_new=0,
                    tot_rel_sf_new=0)
    
    sf_trip_data <- dplyr::bind_rows(sf_trip_data, sf_zero_catch) %>%
      dplyr::mutate_if(is.numeric, tidyr::replace_na, replace = 0) %>%
      dplyr::select(c("date", "catch_draw","tripid","mode",
                      "tot_keep_sf_new","tot_rel_sf_new"))
    
    mode_list_sf[[md]]<- sf_trip_data %>% 
      dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))
  }
  
  if (sf_catch_check_md==0){
    
    sf_trip_data<-catch_data_md %>% 
      dplyr::select("date", "catch_draw","tripid","mode") %>% 
      dplyr::mutate(tot_keep_sf_new = 0, 
                    tot_rel_sf_new= 0, 
                    domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid)) 
    
    mode_list_sf[[md]]<- sf_trip_data %>% 
      dplyr::mutate(domain2 = paste0(date, "_", mode, "_", catch_draw, "_", tripid))
  }
  # remove unneccessary and large dataset
  rm_list <- c("catch_size_data_proj", "keep_size_data", "new_size_data","release_size_data", 
               "sf_catch_data", "sf_trip_data", "sf_zero_catch") 
  
  for (obj in rm_list) {
    if (exists(obj)) {
      rm(list = obj)
    }
  }  
}
  sf_trip_data <- dplyr::bind_rows(mode_list_sf)
  sf_trip_data<-data.table::as.data.table(sf_trip_data)
  
  data.table::setkey(sf_trip_data, "domain2")
  
  keep_release_sf <- dplyr::bind_rows(keep_release_list_sf)
  keep_release_sf<-data.table::as.data.table(keep_release_sf)
  zero_catch_sf<-dplyr::bind_rows(zero_catch_list_sf)
  
  
  
  
  sum(sf_trip_data$tot_keep_sf_new)
  sum(sf_trip_data$tot_rel_sf_new)
  nrow(zero_catch_sf)
  sum(keep_release_sf$keep_sf_42)
  sum(keep_release_sf$release_sf_45)  

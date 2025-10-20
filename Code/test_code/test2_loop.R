

#Lou's repos
iterative_input_data_cd="E:/Lou's projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"


# Status quo regs
predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){

  for (dr in 1:25){
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
}

prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_SQ_new2.csv"))


# Reduce the minimum size by two for all species
predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){

  for (dr in 1:25){

    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2_min_minus2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
}

prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_minus2_new2.csv"))



# Increase the minimum size by two for all species
predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){

  for (dr in 1:25){
    k<-k+1
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_data_read_test2_min_plus2.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/test_code/predict_rec_catch_test2.R")
    
    predictions_list[[k]]<-predictions
    k<-k+1
    
  }
}

prediction_draws <- dplyr::bind_rows(predictions_list)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))

write_csv(prediction_draws, file.path(input_data_cd, "test2output_plus2_new2.csv"))




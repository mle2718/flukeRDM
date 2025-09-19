

#Lou's repos
iterative_input_data_cd="E:/Lou's projects/flukeRDM/flukeRDM_iterative_data"
input_data_cd="C:/Users/andrew.carr-harris/Desktop/MRIP_data_2025"

predictions_list<-list()
k<-1
for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
#  for (st in c("MA", "RI")){
    
  for (dr in 1:2){
    k<-k+1
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_data_read.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch.R")
    
    predictions_list[[k]]<-predictions
    
  }
}

predictions_list2<-predictions_list[-1]   
prediction_draws <- dplyr::bind_rows(predictions_list2)
prediction_draws_check <- prediction_draws %>% 
  dplyr::filter(is.na(value))



## Run the model keeping everything the same
predictions_list2<-list()
k<-1
#for (st in c("MA")){
    for (st in c("MA", "RI", "CT", "NY", "NJ", "DE", "MD", "VA", "NC")){
  
  for (dr in 1:100){
    k<-k+1
    
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_data_read_test1.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch_functions_test1.R")
    source("C:/Users/andrew.carr-harris/Desktop/Git/flukeRDM/Code/sim/predict_rec_catch.R")
    
    predictions_list2[[k]]<-predictions
    
  }
}

predictions_list3<-predictions_list2[-1]   
prediction_draws2 <- dplyr::bind_rows(predictions_list3)
write_csv(prediction_draws2, file.path(iterative_input_data_cd, paste0("test_no_change_scenario_MA_NY.csv")))


output<-prediction_draws2 %>% 
  dplyr::filter(category=="CV" & mode=="all modes") %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(mean_cv=mean(value), sd_cv=sd(value))


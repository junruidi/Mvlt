##################################################################
##    Create Sample Data to Develop/Test Movelet Functions      ##
##                    JD 05/07/2019                             ##
## Only focus on 4008 visit 2 for now                           ##
##################################################################

rm(list = ls())
library(tidyverse)
library(lubridate)
# only work with subject 4002 for now

labs = read_csv("~/Documents/JDi/SQUAD/prediction/data/squad_subject_annotations.csv") %>% filter(visit == 2)



# 1. Prosess the Anotation Data -------------------------------------------
labs = read_csv("squad_subject_annotations.csv") %>% filter(subject_id == "100706184008")
tasks = unique(labs$annotation_type)
tasks_keep = c(tasks[which(grepl("^L",tasks))],"Restless")
labs = (labs %>% filter(annotation_type %in% tasks_keep))[,c(1,2,3,6)]
labs = labs[order(labs$annotation_start_time,decreasing = F),]
labs$annotation_start_time = labs$annotation_start_time - 60 * 60 * 4
labs$annotation_stop_time = labs$annotation_stop_time - 60 * 60 * 4


# 2. Process the Acceleration Data ----------------------------------------
accel = read_csv("/Volumes/PfIRe-SQUAD/downsampled_geneactiv_data/100706184008_02_leftwrist_downsampled_20hz.csv",col_names = c("Time","x","y","z","h")) %>% select(-h)




# 3. Find the Rough Sleep Period from PSG ---------------------------------
load("~/Documents/JDi/SQUAD/sleep/data/Sleep_PSG.rda")
PSG = PSG %>% filter(SBJ_ID == 100706184008)
st = PSG$Lights_Off
end = PSG$Lights_on


labs = labs %>% filter(annotation_start_time >= st & annotation_stop_time <= end)
accel = accel %>% filter(Time >= st & Time <= end)



# 4. Create the Final Sample Data to Use ----------------------------------

accel$Label = "Sleep"

for(i in 1:nrow(labs)){
  accel$Label = ifelse(accel$Time <= labs$annotation_stop_time[i] & accel$Time >= labs$annotation_start_time[i], labs$annotation_type[i], accel$Label)
}
names(accel)[1] = "Timestamp"
accel = accel[-nrow(accel),]
save(labs, accel, file = "~/Documents/JDi/Mvlt/data.rda")



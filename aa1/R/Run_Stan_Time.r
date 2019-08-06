# Stan estimation script for real survey data

############## Customization Part begins #####################

path <-        # Enter your full working directory path, in the example "./DEEP {Your Name}"
project_name <-   # If you are running Stan estimation on survey data sets, enter the unique name 
					   # for this project as a string of characters, eg: 'StudyNo1'. 
num_question <-   # How many questions asked for each subject
num_question_Est <-    # How many questions each subject asked are used for parameter estimation 
type_theta <- 'Hier'    # Either 'Global', 'Individual' or 'Hier'. Which version of scaling parameter setting
                   # do you want in this estimation
############## Customization Part ends #####################


#### Data Processig Part begins #############################
library(ggplot2)
library(reshape2)
library(rstan)
library(dplyr)
library(stringr)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()-1)

source(paste0(path, '/Functions_Time_temp.r'))

if (!is.null(project_name)){
  Time_data_prepare(project_name = project_name, path = path, num_question = num_question)
}

###### Data Processig Part ends #############################

#####  Estimation Part begins #################################
# Stan Time Estimaiton
Stan_Time_Estimation(project_name = project_name, 
                                 num_question_Est = num_question_Est, 
                                 num_question = num_question, 
                                 type_theta = type_theta, 
                                 path = path)

##### Save the estimates out from stanfit object into csv files
Time_save_stantocsv(project_name = project_name,
                    num_question_Est = num_question_Est,
                    type_theta = type_theta,
                    path = path)

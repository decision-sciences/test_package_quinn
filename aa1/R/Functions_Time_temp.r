#' Read, Clean and Denoise Time Survey Data
#' @description Read and denoise the original suvery output data. Only complete rows would be reserved. In addition, this function automatically drops those "unengaged" responses (noisy ones with only random choices, not involving utility consideration).
#' Once you obtain the original output csv file from DEEP survey under directory \code{path}, rename it to be "DEEP_\{\code{model}\}_surveydata_\{\code{project_name}\}.csv", where "\code{model}" is either "Time" or "Risk".

#' @param project_name The name of this study.
#' @param path Full path for working directory.
#' @param num_question How many questions are asked for each subject in this suvery.
#' @param each_seconds How many seconds spent on each question on average so that the observation would be considered as a reasonable response.
#' @param clean_time Logical value. Default is \code{TRUE} means you want to denoise the original data by dropping all responses which are completed within \code{num_question} times \code{each_seconds} seconds.
#' @param clean_side Logical value. Default is \code{TRUE} means you want to denoise the original data by dropping all responses which only contain one-side options, namely, only left or right options chosen.
#'
#' @import dplyr
#' 
#' @return Three csv files will be saved under directory indicated by \code{path} and used in estimation function \code{\link{Stan_Time_Estimation}}:
#' \itemize{
#'   \item "\{\code{project_name}\}_Time_options_chosen.csv" contains amounts of rewards (first column) and delayed days (second column) of all chosen options. The number of rows is \code{num_question} times \code{num_subjects}, where \code{num_subjects} is the number of subjects in this survey after denoising.
#'   \item "\{\code{project_name}\}_Time_options_notchosen.csv" is same as the above one except this contains data of unchosen options among all questions.
#'   \item "\{\code{project_name}\}_Time_serials.csv" contains serial number for each subject as identification.
#' }
#' @export
#'
#' @examples
#' Time_data_prepare(project_name = 'test', path = '/Users/ap/Desktop', num_question = 12)
Time_data_prepare <- function(project_name, 
                              path, 
                              num_question,
                              clean_time = T, 
                              clean_side = T, 
                              each_seconds = 3
){
  
  original <- read.csv(paste0(path, '/DEEP_Time_surveydata_', project_name, '.csv'), as.is = T, header = T)
  # Keep only complete rows
  original <- original[complete.cases(original),]
  rownames(original) <- seq(nrow(original))
  
  if (clean_time == T){
    # Drop all responses with random choices defined by using time less than each_seconds seconds on average
    # This argument controls how we regard responses as noisy responses completed just randomly by subjects.
    #During data cleaning, we will first drop responses which are finished within (each_seconds * num_questions) seconds in total.
    used_time <- original %>% group_by(participant) %>% summarise(sum = sum(timesincepresentation))
    original <- original %>% filter(participant %in% used_time$participant[used_time$sum > num_question * each_seconds])
    rownames(original) <- seq(nrow(original))
  }
  
  if (clean_side == T){
    # It also drops all responses which contain only one-side options (only left or right chose options).
    # Min seconds spent on each question on average so that the responses would be considered as a normal response,  
    # otherwise, they will be considered as noisy ones and dropped. 
    all_oneside <- original %>% group_by(participant) %>% summarise(one_side = all(chosenside == 1) | all(chosenside == 0))
    original <- original %>% filter(participant %in% all_oneside$participant[all_oneside$one_side == FALSE])
    rownames(original) <- seq(nrow(original))
  }
  
  subjectNumber <- length(unique(original$participant))
  
  # Attention, as default gambles1 will contain the actual options made by subjects
  
  gambles1 <- matrix(c(original$chosendollars, original$chosentime), ncol = 2)
  gambles2 <- matrix(c(original$nonchosendollars, original$nonchosentime), ncol = 2)
  
  write.table(x = gambles1, paste0(path, '/', project_name, '_Time_options_chosen.csv'), col.names = F, row.names = F, sep = ',')
  write.table(x = gambles2, paste0(path, '/', project_name, '_Time_options_notchosen.csv'), col.names = F, row.names = F, sep = ',')
  write.table(unique(original$serial), paste0(path, '/', project_name, '_Time_serials.csv'), row.names = F, col.names = F, sep = ',')
}


#' Stan Estimation Function for DEEP Time
#' @description The main function to do estimation on DEEP Time data. This function will automatically call stan models.
#' @param project_name The name of this study. 
#' @param num_question_Est How many questions you want to use in estimation.
#' @param num_question How many questions are asked for each subject.
#' @param type_theta Type of scaling response noise parameter used in estimation, specify either "Global", "Individual" or "Hier".
#' @param path Full path for working directory.
#' @param save_out Whether save the stanfit object as a rdata file. The default is "TRUE".
#' @param chains A positive integer specifying the number of Markov chains. The default is 3.
#' @param iter A positive integer specifying the number of iterations for each chain (including warmup). The default is 1000.
#' @param thin A positive integer specifying the period for saving samples. The default is 3. For details, refer to help page for function \code{\link[rstan]{stan}}.
#' @param adapt_delta A double value between 0 and 1 controlling the accept probability in sampling. The default is 0.9. For details, refer to help page for function \code{\link[rstan]{stan}}.
#' @param max_treedepth A positive integer specifying how deep in tree exploration. The default is 12. For details, refer to help page for function \code{\link[rstan]{stan}}.
#' @param stepsize A double and positive value controlling sampler's behavior. The default is 1. For details, refer to help page for function \code{\link[rstan]{stan}}.
#' 
#' @return Return a large stanfit object called "hier_time" if \code{save_out=FALSE}. Otherwise, return nothing but save the stanfit object into a local RData file named 
#' "Stan_Time_\{\code{project_name}\}_Est\{\code{type_theta}\}\{\code{num_question_Est}\}questions.RData" under directory \code{path}.
#' @export
#'
#' @importFrom rstan sampling
#' @importFrom rstan rstan_options
#' @importFrom rstan options
#' @examples
#' Stan_Time_Estimation(project_name = 'Test', num_question_Est = 12, num_question = 12, 
#' type_theta = 'Hier', path = '/Users/ap/Desktop')
#' @references 
#' Toubia, O., Johnson, E., Evgeniou, T., & Delquié, P. (2013). Dynamic experiments for 
#' estimating preferences: An adaptive method of eliciting time and risk parameters. 
#' Management Science, 59(3), 613-640.
#' \url{https://pubsonline.informs.org/doi/abs/10.1287/mnsc.1120.1570}
#' 

Stan_Time_Estimation <- function(project_name, 
                                 num_question_Est, 
                                 num_question, 
                                 type_theta, 
                                 path,
                                 save_out = T,
                                 chains=3, 
                                 iter=1000, 
                                 thin=3, 
                                 adapt_delta=.9, 
                                 max_treedepth=12, 
                                 stepsize=1) {
  
  rstan_options(auto_write = TRUE)
  options(mc.cores = parallel::detectCores()-1)
  
  # Make sure the type of delta used is correct
  stopifnot(type_theta %in% c('Global', 'Individual', 'Hier'))
  
  gambles1 <- read.csv(paste0(path, '/', project_name, '_Time_options_chosen.csv')
                       , header = F, col.names = c("ssamount","ssdelay"))
  gambles2 <- read.csv(paste0(path, '/', project_name, '_Time_options_notchosen.csv')
                       , header = F, col.names = c("llamount","lldelay"))
  serials <- read.csv(paste0(path, '/', project_name, '_Time_serials.csv'), header = F)
  
  subjectNumber <- nrow(serials)
  
  all_Stan_data <- data.frame(SubjectID = NA, QuestionNum = NA, SS_Amount = gambles1$ssamount, SS_Delay = gambles1$ssdelay,
                              LL_Amount = gambles2$llamount, LL_Delay = gambles2$lldelay, Choice = 0)
  
  all_Stan_data <- Time_index_Questions(all_Stan_data, subjectNumber, num_question)
  
  # create data list for running Stan
  Time_prepare_Stan(all_Stan_data, subjectNumber, num_question_Est)
  
  # Set initial values to begin MCMC sampling based on empirical knowledge
  chain <- list('beta' = rep(0.8, subjectNumber), 'r' = rep(0.002, subjectNumber),
                'delta' = rep(exp(-365*0.002), subjectNumber), 'theta' = rep(0.5, subjectNumber),
                'delta_phi' = rep(0, subjectNumber),
                'mubeta' = 0.8, 'sigmabeta' = 0.2,
                'mudelta_phi' = 0, 'sigmadelta_phi' = 0.2,
                'mutheta' = -1, 'sigmatheta' = 1)
  
  initial_chains <- list(chain, chain, chain)
  
  # run stan model with indiviual or global or hierarchical scaling response noise parameter
  if (type_theta == 'Global') {
    hier_time <- stan(paste0(path, '/Stan_Time_Global.stan'),
                          data = moddat,
                          chains = chains, 
                          iter = iter,
                          init = initial_chains,
                          thin = thin,
                          control = list(adapt_delta = adapt_delta,
                                         max_treedepth = max_treedepth,
                                         stepsize = stepsize))
  } else {
    if (type_theta == 'Individual'){
      hier_time <- stan(paste0(path, '/Stan_Time_Individual.stan'),
                            data = moddat,
                            chains = chains, 
                            iter = iter,
                            init = initial_chains,
                            thin = thin,
                            control = list(adapt_delta = adapt_delta,
                                           max_treedepth = max_treedepth,
                                           stepsize = stepsize))
    } else {
      hier_time <- stan(paste0(path, '/Stan_Time_Hier.stan'),
                            data = moddat,
                            chains = chains, 
                            iter = iter,
                            init = initial_chains,
                            thin = thin,
                            control = list(adapt_delta = adapt_delta,
                                           max_treedepth = max_treedepth,
                                           stepsize = stepsize))
    }
  }
  
  if (save_out){
    # Save out the Stan fit subject as a RData file in your working path
    filename_stan   <- paste0(path, "/Stan_Time_", project_name, "_Est", type_theta,
                              num_question_Est, "questions.RData")
    save(hier_time, file = filename_stan)
  } else {
    # If not save out, return the subject
    return(hier_time)
  }
}


#' Save Time Preferences Estimates to csv Files
#' @description Save posterior point estimates for time preferences from stanfit object to local csv files. 
#' Only run this function when stanfit object for study \code{project_name} is saved under directory \code{path} after estimation.
#' 
#' @param project_name The name of this study. 
#' @param num_question_Est How many questions you want to use in estimation.
#' @param type_theta Type of scaling response noise parameter used in estimation, specify either "Global", "Individual" or "Hier".
#' @param path Full path for working directory.
#'
#' @return Two csv files will be saved under directory indicated by \code{path}:
#' \itemize{
#'   \item "StanEstimates_Time_parameters_\{\code{project_name}\}_Est\{\code{type_theta}\}\{\code{num_question_Est}\}questions.csv" contains all estimates of individual \eqn{\beta} (present-bias parameter), \eqn{r} (daily discount rate) and \eqn{\delta} (yearly discount factor) respectively as in three columns. The number of rows is the number of subjects in the survey. 
#'   \item "StanEstimates_Time_theta_\{\code{project_name}\}_Est\{\code{type_theta}\}\{\code{num_question_Est}\}questions.csv" contains estimates of either global or individual scaling response error parameter \eqn{\theta}.
#' }
#' 
#' @export
#' @importFrom rstan extract
#' @examples
#' Time_save_stantocsv(project_name = 'test', num_question_Est = 12, type_theta = 'Hier', path = path)
#' @references 
#' Toubia, O., Johnson, E., Evgeniou, T., & Delquié, P. (2013). Dynamic experiments for 
#' estimating preferences: An adaptive method of eliciting time and risk parameters. 
#' Management Science, 59(3), 613-640.
#' \url{https://pubsonline.informs.org/doi/abs/10.1287/mnsc.1120.1570}
#' 
Time_save_stantocsv <- function(project_name,
                                num_question_Est, 
                                type_theta, 
                                path){
  
  # Make sure the type of theta used is correct
  stopifnot(type_theta %in% c('Global', 'Individual', 'Hier'))
  
  load(paste0(path, "/Stan_Time_", project_name, "_Est", type_theta, num_question_Est, "questions.RData"))
  stan <- rstan::extract(hier_time)
  serials <- read.csv(paste0(path, '/', project_name, '_Time_serials.csv'), header = F)$V1
  filename_wts <- paste0(path, '/StanEstimates_Time_parameters_', project_name, '_Est', 
                         type_theta, num_question_Est, "questions.csv")
  filename_theta <- paste0(path, '/StanEstimates_Time_theta_', project_name, '_Est', 
                           type_theta, num_question_Est, "questions.csv")
  
  stan_beta <- colMeans(stan$beta)
  stan_r <- colMeans(stan$r)
  stan_delta <- colMeans(stan$delta)
  
  if (type_theta == 'Global'){
    stan_theta <- matrix(c(1, mean(stan$global_theta)), nrow = 1, byrow = T)
    colnames(stan_delta) <- c('serial', 'global theta')
  } else {
    stan_theta <- matrix(c(serials, colMeans(stan$theta)), ncol = 2)
    colnames(stan_theta) <- c('serial', 'individual theta')
  }
  stan <- matrix(c(serials, stan_beta, stan_r, stan_delta), ncol = 4)
  colnames(stan) <- c('serial', 'beta', 'r', 'delta')
  
  write.table(stan, filename_wts, col.names = T, row.names = F, sep = ',')
  write.table(stan_theta, filename_theta, col.names = T, row.names = F, sep = ',')
}


#' Index and Arrange Time Questions
#' @description Index and arrange questions so that smaller-sooner and larger-later options would be arranged as expected. 
#' Called by function \code{\link{Stan_Time_Estimation}}.
#' @param stan_data Data needed to be processed.
#' @param subjectNumber Number of subjects in this survey.
#' @param num_question How many questions are asked for each subject in this survey.
#'
#' @return Return the indexed and arranged data.
#' @export
#'
#' @examples
#' Time_Index_Questions(stan_data = stan_data, subjectNumber = 200, num_question = 20)
Time_index_Questions <- function(stan_data, 
                                 subjectNumber,
                                 num_question)
{
  for(subjN in 1:subjectNumber)
  {
    for(questN in 1:num_question)
    {
      rowIndex <- (subjN - 1) * num_question + questN
      stan_data$SubjectID[rowIndex] <- subjN
      stan_data$QuestionNum[rowIndex] <- questN
      
      # ensure that we have ss and ll in the right order
      if(stan_data$LL_Delay[rowIndex] < stan_data$SS_Delay[rowIndex])
      {
        temp_lldelay <- stan_data$SS_Delay[rowIndex]
        temp_llamount <- stan_data$SS_Amount[rowIndex]
        
        stan_data$SS_Delay[rowIndex] <- stan_data$LL_Delay[rowIndex]
        stan_data$SS_Amount[rowIndex] <- stan_data$LL_Amount[rowIndex]
        
        stan_data$LL_Delay[rowIndex] <- temp_lldelay
        stan_data$LL_Amount[rowIndex] <- temp_llamount
        
        stan_data$Choice[rowIndex] <- 1
      }
    }
  }
  return(stan_data)
}

#' Prepare Data for Stan Time Estimation
#' @description After reading, processing and denoising necessary data. This function will create an R list object containing all data to run estimation for DEEP Time.
#' Called by function \code{\link{Stan_Time_Estimation}}.
#' @param all_Stan_data Data needed to be processed.
#' @param subjectNumber Number of subjects in this survey.
#' @param num_question_Est How many questions are going to be used in this estimation.
#'
#' @import dplyr
#' @importFrom reshape2 acast
#' 
#' @return Several objects will be returned into global enviornment:
#' \itemize{
#'   \item ss_amnt: a dataframe contains amounts of rewards in all smaller-sooner options.
#'   \item ll_amnt: a dataframe contains amounts of rewards in all larger-later options.
#'   \item ss_delay: a dataframe contains delayed days of rewards in all smaller-sooner options.
#'   \item ll_delay: a dataframe contains delayed days of rewards in all larger-later options.
#'   \item choices: a dataframe contains all choices made by subjects with "0" means smaller-sooner option is chosen and "1" means larger-later option is chosen.
#'   \item moddat: a list object contains all these data needed in estimation model.
#' }
#' 
#' @export
#'
#' @examples
#' Time_prepare_Stan(all_Stan_data = all_Stan_data, num_question_Est = 12, subjectNumber = 200)
Time_prepare_Stan <- function(all_Stan_data, 
                              subjectNumber,
                              num_question_Est){
  
  all_Stan_data <- all_Stan_data %>% filter(QuestionNum <= num_question_Est)
  
  ss_amnt <- all_Stan_data %>%
    select(SubjectID, QuestionNum, SS_Amount) %>%
    acast(SubjectID ~ QuestionNum, value.var = "SS_Amount")
  
  ll_amnt <- all_Stan_data %>%
    select(SubjectID, QuestionNum, LL_Amount) %>%
    acast(SubjectID ~ QuestionNum, value.var = "LL_Amount")
  
  ss_delay <- all_Stan_data %>%
    select(SubjectID, QuestionNum, SS_Delay)  %>%
    acast(SubjectID ~ QuestionNum, value.var = "SS_Delay")
  ss_delay <- ss_delay
  
  ll_delay <- all_Stan_data %>%
    select(SubjectID, QuestionNum, LL_Delay)  %>%
    acast(SubjectID ~ QuestionNum, value.var = "LL_Delay")
  ll_delay <- ll_delay
  
  choices <- all_Stan_data %>%
    select(SubjectID, QuestionNum, Choice)  %>%
    acast(SubjectID ~ QuestionNum, value.var = "Choice")
  
  assign("ss_amnt", ss_amnt, envir = .GlobalEnv)
  assign("ll_amnt", ll_amnt, envir = .GlobalEnv)
  assign("ss_delay", ss_delay, envir = .GlobalEnv)
  assign("ll_delay", ll_delay, envir = .GlobalEnv)
  assign("choices", choices, envir = .GlobalEnv)
  
  moddat <- list("npart" = subjectNumber, 
                 "nchoice" = num_question_Est,
                 "choices" = choices,
                 "ss_amnt" = ss_amnt,
                 "ss_delay" = ss_delay,
                 "ll_amnt" = ll_amnt,
                 "ll_delay" = ll_delay)
  assign("moddat", moddat, envir = .GlobalEnv)
}

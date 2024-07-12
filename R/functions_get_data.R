library(here)
library(tidyverse)

# to read the data. it is an rda object so we need a nifty trick, see more:
# https://stackoverflow.com/a/5577647/12296038
read_data <- function(data_name){
    env <- new.env()
    nm <- load(here("data", data_name), env)[1]
    env[[nm]]
}

filter_select_data <- function(data){
  dat <- data |> 
    # we only want primary respondents
    dplyr::filter(
      RESP_TYPE == "PRIMARY"
    ) |> 
    
    # these are the items we'll use in the analysis
    dplyr::select(
      Q4, Q7
    )
  
  return(dat)
}
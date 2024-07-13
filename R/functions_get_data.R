library(here)
library(tidyverse)

# to read the data. it is an rda object so we need a nifty trick, see more:
# https://stackoverflow.com/a/5577647/12296038
read_data <- function(data_name){
    env <- new.env()
    nm <- load(here("data", data_name), env)[1]
    env[[nm]]
}

# data cleaning steps
clean_data <- function(data){
  dat <- data |> 
    # we only want primary respondents
    dplyr::filter(
      RESP_TYPE == "PRIMARY"
    ) |> 
    
    # these are the items we'll use in the analysis
    dplyr::select(
      D2, # gender they identify with
      Q129D # partner has kept them from job, money, fin resources
    ) |> 
    
    dplyr::mutate(
      # we take only those who identify as cis male or female, as the data does
      # not distinguish between transgender men and women (even though the 
      # questionnaire does) and there are only 28 people classified as 
      # "transgender"
      gender = dplyr::case_when(
        stringr::str_detect(D2, "(1)") ~ "Male",
        stringr::str_detect(D2, "(2)") ~ "Female",
        .default = NA
      ) |> 
        forcats::as_factor(),
      fin_abuse = dplyr::case_when(
        stringr::str_detect(Q129D, "(1)") ~ "Never",
        stringr::str_detect(Q129D, "(2)") ~ "Rarely",
        stringr::str_detect(Q129D, "(3)") ~ "Sometimes",
        stringr::str_detect(Q129D, "(4)") ~ "Often",
        stringr::str_detect(Q129D, "(5)") ~ "Very often",
      ) |> 
        forcats::as_factor() |> 
        forcats::fct_relevel(
          "Never", "Rarely", "Sometimes", "Often", "Very often"
        ), 
      # discard columns not created in this mutate() call
      .keep = "none"
    )
  
  return(dat)
}
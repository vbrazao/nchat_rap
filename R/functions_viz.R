library(tidyverse)

# functions to display data (tables, plots)

# create a table with all the values by gender
make_table <- function(data){
  data |> 
    dplyr::count(fin_abuse, gender) |> 
    tidyr::pivot_wider(
      names_from = fin_abuse,
      values_from = n,
      names_prefix = "Prevents Access."
    ) |> 
    dplyr::mutate(
      gender = dplyr::if_else(is.na(gender), true = "NA", false = gender)
    ) |> 
    dplyr::rename(
      Gender = gender
    ) |> 
    rempsyc::nice_table(
      separate.header = TRUE
    )
}


library(tidyverse)

# functions to display data (tables, plots)

# create a table with all the values by gender
make_table <- function(data){
  data |> 
    dplyr::count(fin_abuse, gender) |> 
    tidyr::pivot_wider(
      names_from = fin_abuse,
      values_from = n,
      names_prefix = "Partner Prevents Access."
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

# create plot of fin_abuse by gender

make_plot <- function(data){
  data |>
    dplyr::count(fin_abuse, gender) |> 
    ggplot(aes(x = gender, y = fin_abuse, fill = n)) +
    geom_tile(color = "lightgrey", linewidth = 1) +
    scale_fill_gradient(low = "white", high = "purple", name = "Frequency") +
    labs(x = "Gender", y = "Partner Prevents Access") + 
    theme_light() +
    theme(
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_rect(fill = "lightgrey")
    )
}




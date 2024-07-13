library(ordinal)

# ordinal model for the data

make_model <- function(data){
  ordinal::clm(
    formula = fin_abuse ~ gender,
    data = data
  ) 
}

# to be summarized with gtsummary::tbl_regression(exp = TRUE)

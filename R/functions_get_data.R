
# to read the data. it is an rda object so we need a nifty trick, see more:
# https://stackoverflow.com/a/5577647/12296038
read_data <- function(data_name){
    env <- new.env()
    nm <- load(here("data", data_name), env)[1]
    env[[nm]]
}


#importation's function of  Ph Grosjean #####


microO2_import <- function(file, O2 = "Ch1", T = "('C)", P = "(mbar)",
                           time = "Time (s)", date = "Date", date.format = "%d.%m.%Y",
                           daytime = "Time (HH:MM:SS)", skip = 13, ...) {
  dat <- readr::read_tsv(file, skip = skip, ...)
  # Extract the data we need, and make a tibble from it
  
  res <- data.frame(
    O2   = dat[[O2]],
    T    = dat[[T]],
    P    = dat[[P]],
    time = dat[[time]],
    date = as.POSIXct(as.Date(dat[[date]], format = date.format)) + dat[[daytime]]
  )
  
  # Return a subclassed tbl_df object
  res <- dplyr::as_data_frame(res)
  structure(res, class = c("microO2", class(res)))
}


# importation's by guyliann first version

microO2_import2  <- function(file){
  X <- read.table(file = file, skip = 13, sep = '\t', header = TRUE)
  X <- X[ , c(1:3,5, 13, 15)]
  X <- tidyr::unite(X, Date, c(1,2), sep = " ")
  X$Date <- as.POSIXct(X$Date, format = "%d.%m.%Y %H:%M:%S")
  X <- dplyr::rename( X, temps = Time..s., O2 = Ch1, P = "X.mbar.", T = "X.C...C.")
}



microO2_import3  <- function(file){
  read.table(file = file, skip = 13, sep = '\t', header = TRUE) ->.; # import rawdata with 13 lines skip
  .[ , c(1:3,5, 13, 15)] ->.; # select only interesting columns
  tidyr::unite(., Date, c(1,2), sep = " ") ->.; #combine date and  hour
  as.POSIXct(.$Date, format = "%d.%m.%Y %H:%M:%S") ->.$Date 
  dplyr::rename(.,date = "Date",  time= Time..s., O2 = Ch1, pressure= "X.mbar.", temperature = "X.C...C.") ->.
}

#example to check the function microO2_import3
#test <-microO2_import3(file = "data/T40/T40.txt")

#Function to create a dataframe that convert time by distance
distance_by_time <-function(N, distance_step, distance_start, time_step, time_start){
  n <-c(0:N) #N is the number of distance 's step
  time <- (n*time_step)+time_start #distance_step is the time between each step, time_start is the time's beginning
  dist <- (n*distance_step)+distance_start #distance_step is the distance between each step, the distance must be regular, the distance_start is the distance's beginning
  data.frame(n = n, time = time, distance = dist) #create a data frame with 3 vectors
}

#example to check the function distance_by_time
#test <- distance_by_time(N = 40, distance_step = 10, distance_start = 0, time_step = 15, time_start = 0)




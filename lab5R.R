library("tidyverse")
library("jsonlite")
library("stringr")

#get Allentown data
allen.data <- read_csv("data/essentia.data.allentown.csv")

#get other data
other.data <- read_csv("data/essentia.data.csv")

#function to extract data about how each band compares to Allentown
checker <- function(feature = feature,
                    allen.data = allen.data,
                    other.data = other.data){
  
  summarize.data <- other.data |>
    mutate(feature = as.numeric(get(feature)))|>
    group_by(artist) |>
    summarize(
      minimum = min(feature, na.rm = TRUE),
      maximum = max(feature, na.rm = TRUE),
      low.bound = quantile(feature, 0.25, na.rm = TRUE) - 1.5 * IQR(feature, na.rm = TRUE),
      upper.bound = quantile(feature, 0.75, na.rm = TRUE) + 1.5 * IQR(feature, na.rm = TRUE)
    )|>
    mutate(
      allen.data.feature = allen.data[[feature]],
      out.of.range = ifelse((minimum > allen.data.feature | maximum < allen.data.feature), 
                            TRUE, 
                            FALSE),
      unusual = ifelse((low.bound > allen.data.feature | upper.bound < allen.data.feature), 
                        TRUE, 
                        FALSE)
    )|>
    mutate(
      description = ifelse((unusual == TRUE),
                            ifelse((out.of.range == TRUE),
                                   "Out Of Range",
                                   "Outlying"),
                            "Within Range")
    )|>
    
    select(-allen.data.feature)
    
  return(summarize.data)
}

#remove all columns which are not numeric
col.classes.type <- sapply(other.data, class)
cols.numeric.names <- names(other.data[which(col.classes.type == "numeric")])

list.descriptions <- c()

for(col in 1:length(cols.numeric.names)){
#col = 196
  data <- checker(cols.numeric.names[col], allen.data, other.data)
  list.descriptions <- cbind(list.descriptions, data[[8]])
}

view(list.descriptions)




#Counting code
as <- 0
ms <- 0
ts <- 0

for(i in 1:length(list.descriptions)){
  if(list.descriptions[i] == "Within Range" & (i%%3 == 2)){
    as = as + 1
  }else if(list.descriptions[i] == "Within Range" & (i%%3 == 1)){
    ms = ms + 1
  }else if(list.descriptions[i] == "Within Range" & (i%%3 == 0)){
    ts = ts + 1
  }else if(list.descriptions[i] == "Outlying" & (i%%3 == 2)){
    as = as + 0.5
  }else if(list.descriptions[i] == "Outlying" & (i%%3 == 1)){
    ms = ms + 0.5
  }else if(list.descriptions[i] == "Outlying" & (i%%3 == 0)){
    ts = ts + 0.5
  }
}



#min(...) provides the minimum for a vector
#quantile(...) provides the specified percentile for a vector
#median(...) provides the median for a vector
#max(...) provides the max for a vector
#IQR(...) provides the interquartile rang for a vector
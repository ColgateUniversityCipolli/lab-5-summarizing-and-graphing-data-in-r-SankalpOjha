library("tidyverse")
library("jsonlite")
library("stringr")

#get Allentown data
allen.data <- read_csv("data/essentia.data.allentown.csv")

#get other data
other.data <- read_csv("data/essentia.data.csv")

checker <- function(allen.data = allen.data,
                    other.data = other.data,
                    feature = "overall_loudness"){
  
  summarize.data <- other.data |>
    mutate(feature = as.numeric(get(feature)))|>
    group_by(artist) |>
    summarize(
      minimum = min(feature, na.rm = TRUE),
      maximum = max(feature, na.rm = TRUE),
      low.bound = quantile(feature, 0.25, na.rm = FALSE) - 1.5 * IQR(feature, na.rm = TRUE),
      upper.bound = quantile(feature, 0.75, na.rm = FALSE) + 1.5 * IQR(feature, na.rm = TRUE)
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
    )
  
  return(summarize.data)
}

x <- checker(allen.data = allen.data,
             other.data = other.data,
             feature = "overall_loudness")

#min(...) provides the minimum for a vector
#quantile(...) provides the specified percentile for a vector
#median(...) provides the median for a vector
#max(...) provides the max for a vector
#IQR(...) provides the interquartile rang for a vector
#' @rdname target_encoding_methods
#' @export
fe_target_encoding_mean <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    noise = 0,
    seed = 1,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_mean"
  )

  #aggregate by groups
  df.map <- tapply(
    X = data[[dependent.variable.name]],
    INDEX = data[[categorical.variable.name]],
    FUN = mean,
    na.rm = TRUE
  )
  df.map <- data.frame(names(df.map), df.map)
  names(df.map) <- c(
    categorical.variable.name,
    categorical.variable.new.name
  )

  #join
  data <- dplyr::inner_join(
    x = data,
    y = df.map,
    by = categorical.variable.name
  )

  #add noise if any
  data <- fe_target_encoding_noise(
    data = data,
    categorical.variable.name = categorical.variable.new.name,
    noise = noise,
    seed = seed
  )

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}


#' @rdname target_encoding_methods
#' @export
fe_target_encoding_rnorm <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    sd.width = 0.1,
    seed = 1,
    verbose = TRUE
){

  if(sd.width <= 0){
    sd.width <- 0.0001
  }
  if(sd.width > 1){
    sd.width <- 1
  }

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_rnorm"
  )

  set.seed(seed)
  data <- data %>%
    dplyr::group_by(.data[[categorical.variable.name]]) %>%
    dplyr::mutate(
      !!categorical.variable.new.name := stats::rnorm(
        n = dplyr::n(),
        mean = mean(
          get(dependent.variable.name),
          na.rm = TRUE
        ),
        sd = sd(
          get(dependent.variable.name),
          na.rm = TRUE
        ) * sd.width
      )
    ) %>%
    dplyr::ungroup()

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}


#' @rdname target_encoding_methods
#' @export
fe_target_encoding_rank <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    noise = 0,
    seed = 1,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_rank"
  )

  #aggregate by groups
  df.map <- tapply(
    X = data[[dependent.variable.name]],
    INDEX = data[[categorical.variable.name]],
    FUN = mean,
    na.rm = TRUE
  ) %>%
    sort()

  df.map <- data.frame(
    names(df.map),
    1:length(df.map)
  )
  names(df.map) <- c(categorical.variable.name, categorical.variable.new.name)

  #merge
  data <- dplyr::inner_join(
    x = data,
    y = df.map,
    by = categorical.variable.name
  )

  #add noise if any
  data <- fe_target_encoding_noise(
    data = data,
    categorical.variable.name = categorical.variable.new.name,
    noise = noise,
    seed = seed
  )

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}

#' @rdname target_encoding_methods
#' @export
fe_target_encoding_loo <- function(
    data,
    dependent.variable.name,
    categorical.variable.name,
    verbose = TRUE
){

  #new variable name
  categorical.variable.new.name <- paste0(
    categorical.variable.name,
    "__encoded_loo"
  )

  #leave one out
  #by group, sum all cases of the response, subtract the value of the current row, and divide by n-1
  data <- data %>%
    dplyr::group_by_at(categorical.variable.name) %>%
    dplyr::mutate(
      !!categorical.variable.new.name := (
        sum(
          get(dependent.variable.name),
          na.rm = TRUE
        ) -
          get(dependent.variable.name)
      ) /
        (dplyr::n() - 1)
    )

  if(verbose == TRUE){
    message(
      "New encoded predictor: '",
      categorical.variable.new.name,
      "'"
    )
  }

  data

}


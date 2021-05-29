
get_plot_data <- function(cf, X.vars.cate){
  # initializing the necessary variables
  V=NULL
  variable = NULL
  level = NULL
  i = 1
  results <- vector("list", 36)
  # this loops iterates all non-na levels of the variables and calculates the ATE for each one
  for (variable in names(X.vars.cate)){
    for (level in na.exclude(unique(X.vars.cate[[variable]]))){
      DATA <- X.vars.cate[[variable]] == as.character(level)
      variable_name <- variable
      level_name <- level
      estimate <- round((average_treatment_effect(cf, subset=DATA))[1],2)
      se <- round((average_treatment_effect(cf, subset=DATA))[2], 2)
      upper <- round(estimate + 1.96*se, 2)
      lower <- round(estimate - 1.96*se, 2)
      results[[i]] <- list("name" = variable_name, 
                           "level" = level_name, 
                           "estimate" = estimate, 
                           "se" = se, 
                           "upperb" = upper, 
                           "lowerb" = lower)
      i = i + 1
    }
  }
  
  # this part is a little messy, but it basically converts a list of lists into a data frame to ggplot
  name = lapply(results, function(l) l[[1]])
  level = lapply(results, function(l) l[[2]])
  
  name <- t(matrix(unlist(name), nrow=length(unlist(name[1]))))
  level <- t(matrix(unlist(level), nrow=length(unlist(level[1]))))
  char <- data.frame(name, level)
  
  num <- as.data.frame(t(matrix(as.numeric(unlist(results)), nrow=length(unlist(results[1]))))) %>% 
    rename(estimate = V3,
           se = V4,
           upper = V5,
           lower = V6) %>%
    select(-V1, -V2)
  
  PLOT_DATA <- cbind(char, num )
  return(PLOT_DATA)
}


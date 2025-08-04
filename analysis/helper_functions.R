
# Function to handle skipped values
handle_skipped <- function(x, skip_values) {
  x[x %in% skip_values] <- NA
  return(x)
}

# Function to flip scales (works for 1-5, 1-10, or any other scale)
flip_scale <- function(x, min_val, max_val) {
  return(max_val + min_val - x)
}

# source: 
# https://www.alexstephenson.me/post/2022-04-02-weighted-variance-in-r/
weighted.average <- function(x, w){
    ## Sum of the weights 
    sum.w <- sum(w, na.rm = T)
    ## Sum of the weighted $x_i$ 
    xw <- sum(w*x, na.rm = T)
    
    ## Return the weighted average 
    return(xw/sum.w)
}

weighted.se.mean <- function(x, w, na.rm = T){
    ## Remove NAs 
    if (na.rm) {
      i <- !is.na(x)
        w <- w[i]
        x <- x[i]
    }
    
    ## Calculate effective N and correction factor
    n_eff <- (sum(w))^2/(sum(w^2))
    correction = n_eff/(n_eff-1)
    
    ## Get weighted variance 
    numerator = sum(w*(x-weighted.average(x,w))^2)
    denominator = sum(w)
    
    ## get weighted standard error of the mean 
    se_x = sqrt((correction * (numerator/denominator))/n_eff)
    return(se_x)
}

# Function to create bins and labels
create_bins_and_labels <- function(rating_min, rating_max, total_questions) {
  bins <- seq(rating_min + rating_max - 1, rating_max * total_questions, by = rating_max)
  n_bins <- length(bins)
  bin_labels <- data.frame(start = bins[1:(n_bins-1)] + 1, end = bins[2:n_bins]) %>%
    mutate(label = paste(start, end, sep = "-")) %>% 
    pull(label)
  return(list(bins = bins, labels = bin_labels))
}


# Prepare data and create survey design
rename_variables <- function(data, label_list) {
  new_names <- sapply(names(data), function(x) {
    if (x %in% names(label_list)) label_list[[x]] else x
  })
  setNames(data, new_names)
}

# Function to run logistic regression with optional interactions
run_regression <- function(outcome, predictors, design, family=quasibinomial(link='logit'), interactions = NULL) {
  # 
  # Create the main formula
  main_formula <- paste(predictors, collapse = " + ")
  
  # Add interactions if specified
  if (!is.null(interactions)) {
    interaction_terms <- paste(interactions, collapse = " * ")
    formula <- as.formula(paste(outcome, "~", main_formula, "+", interaction_terms))
  } else {
    formula <- as.formula(paste(outcome, "~", main_formula))
  }
  
  # Run the model
  model <- svyglm(formula, design = design, family = family)
  return(model)
}


# Updated regression function
run_regression_for_table <- function(outcome, predictors, design, family=gaussian()) {
  formula <- as.formula(paste(outcome, "~", paste(predictors, collapse = " + ")))
  model <- svyglm(formula, design = design, family = family)
  return(model)
}

# https://github.com/labreumaia/longtable.stargazer/blob/master/longtable.stargazer.R
##### Function to make stargazer compatible with longtable #####
#### Lucas de Abreu Maia ####
#### Department of Political Science ####
#### UCSD ####
#### lucasamaia.com ####
#### labreumaia@gmail.com ####

## Description
# This function simply makes stargazer compatible with the longtable 
# LaTeX environment
## Arguments
# ... - any argument to be passed to stargazer.
# float - logical. Whether or not the output of stargazer should be in 
# a float environment. This is useful if you want your table to have a 
# title and label associated with it.
# longtable.float - logical. Whether or not you want the longtable 
# itself to be  within a float environment.
# longtable.head - logical. If you want column headers to appear at 
# the top of every page.
# filename - character. An optional file path for the printed output. 
# If abcent, the output is printed to the console.

longtable.stargazer = function(..., float = T, longtable.float = F, 
  longtable.head = T, filename = NULL){
  # Capturing stargazer to hack it
  require(stargazer)
  res = capture.output(
    stargazer(..., float = float)
  )
  # Changing tabulare environment for longtable
    res = gsub("tabular", "longtable", res)
  # removing floating environment
  if(float == T & longtable.float == F){
    res[grep("table", res)[1]] = res[grep("longtable", res)[1]]
    # Removing extra longtable commands
    res = res[-grep("longtable", res)[2]]
    res = res[-length(res)]
  }
  # Adding page headings
  if(longtable.head == T){
    res = c(res[1:which(res == "\\hline \\\\[-1.8ex] ")[1] - 1], "\\endhead", res[which(res == "\\hline \\\\[-1.8ex] ")[1]:length(res)])
  }
  # Exporting
  cat(res, sep = "\n")
  # Exporting
  if(!is.null(filename)){
    cat(res, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ", 
      collapse = ""))
  }else{
    cat(res, sep = "\n")
  }
}


create_ols_summary_table <- function(models, 
                                     title = "OLS Regression Results", 
                                     column.labels = NULL,
                                     dep.var.labels = NULL,
                                     covariate.labels = NULL,
                                     out.file = NULL,
                                     label="",
                                     omit = NULL,
                                     longtable = FALSE) {
  
  # Extract coefficients and standard errors
  extract_coef_se <- function(model) {
    coef_summary <- summary(model)$coefficients
    coef <- coef_summary[, "Estimate"]
    se <- coef_summary[, "Std. Error"]
    return(list(coef = coef, se = se))
  }
  
  model_data <- lapply(models, extract_coef_se)
  
  if (longtable==FALSE){
    # Create the LaTeX table
    stargazer_output <- stargazer(
      models,
      title = title,
      column.labels = column.labels,
      dep.var.labels = dep.var.labels,
      dep.var.labels.include = FALSE,
      model.names = FALSE,
      model.numbers = FALSE,
      dep.var.caption = "",
      covariate.labels = covariate.labels,
      coef = lapply(model_data, function(x) x$coef),
      se = lapply(model_data, function(x) x$se),
      type = "latex",
      style = "default",
      font.size = "footnotesize",
      single.row = TRUE,
      no.space = TRUE,
      table.placement = "h",
      digits = 3,
      column.sep.width = "-10pt",
      star.cutoffs = c(0.05, 0.01, 0.001),
      ci = FALSE,
      header = FALSE, 
      label = label,
      omit = omit,
      out = out.file
    )    
  } else {
    # Create the LaTeX table
    stargazer_output <- longtable.stargazer(
      models,
      title = title,
      column.labels = column.labels,
      dep.var.labels = dep.var.labels,
      dep.var.labels.include = FALSE,
      model.names = FALSE,
      model.numbers = FALSE,
      dep.var.caption = "",
      covariate.labels = covariate.labels,
      coef = lapply(model_data, function(x) x$coef),
      se = lapply(model_data, function(x) x$se),
      type = "latex",
      style = "default",
      font.size = "scriptsize",
      single.row = FALSE,
      no.space = TRUE,
      table.placement = "!htbp",
      digits = 3,
      star.cutoffs = c(0.05, 0.01, 0.001),
      ci = FALSE,
      header = FALSE, 
      label = label,
      omit = omit,
      float.env = "sidewaystable",  # Use sidewaystable environment
      out = out.file
    )
    
  }
  
  if (!is.null(out.file)) {
    # cat(stargazer_output, file = out.file, sep = "\n")
    cat("LaTeX table has been written to", out.file, "\n")
  }
  
  return(stargazer_output)
}

generate_summary_stats <- function(dt, vars_to_summarize, weight_var = NULL, 
                                   group_var = NULL, 
                                   placebo_var = "Treatment",
                                   include_factors = FALSE) {
  
  dt <- as.data.table(dt)
  weight_var <- enquo(weight_var)
  group_var <- enquo(group_var)
  placebo_var <- enquo(placebo_var)
  
  weight_var_name <- if (!quo_is_null(weight_var)) quo_name(weight_var) else NULL
  group_var_name <- if (!quo_is_null(group_var)) quo_name(group_var) else NULL
  placebo_var_name <- quo_name(placebo_var)
  
  summary_stats <- lapply(vars_to_summarize, function(var) {
    if (is.factor(dt[[var]]) || is.character(dt[[var]])) {
      if (!include_factors) return(NULL)
      
      stats <- dt[, .(
        N = as.integer(.N),
        Levels = as.double(length(unique(get(var)))),
        MostFrequent = names(which.max(table(get(var)))),
        Proportion = as.double(max(prop.table(table(get(var)))))
      ), by = c(group_var_name, placebo_var_name)]
    } else {
      if (is.null(weight_var_name)) {
        stats <- dt[, .(
          N = as.integer(sum(!is.na(get(var)))),
          Mean = as.double(mean(get(var), na.rm = TRUE)),
          Var = as.double(var(get(var), na.rm = TRUE)),
          Min = as.double(min(get(var), na.rm = TRUE)),
          Q10 = as.double(quantile(get(var), probs = 0.1, na.rm = TRUE)),
          Q25 = as.double(quantile(get(var), probs = 0.25, na.rm = TRUE)),
          Median = as.double(median(get(var), na.rm = TRUE)),
          Q75 = as.double(quantile(get(var), probs = 0.75, na.rm = TRUE)),
          Q90 = as.double(quantile(get(var), probs = 0.9, na.rm = TRUE)),
          Max = as.double(max(get(var), na.rm = TRUE))
        ), by = c(group_var_name, placebo_var_name)]
      } else {
        stats <- dt[, .(
          N = as.integer(sum(!is.na(get(var)))),
          Mean = as.double(tryCatch(weighted.mean(get(var), w = get(weight_var_name), na.rm = TRUE), error = function(e) NA_real_)),
          Var = as.double(tryCatch(wtd.var(get(var), weights = get(weight_var_name), na.rm = TRUE), error = function(e) NA_real_)),
          Min = as.double(min(get(var), na.rm = TRUE)),
          Q10 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.1, na.rm = TRUE), error = function(e) NA_real_)),
          Q25 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.25, na.rm = TRUE), error = function(e) NA_real_)),
          Median = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.5, na.rm = TRUE), error = function(e) NA_real_)),
          Q75 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.75, na.rm = TRUE), error = function(e) NA_real_)),
          Q90 = as.double(tryCatch(wtd.quantile(get(var), weights = get(weight_var_name), probs = 0.9, na.rm = TRUE), error = function(e) NA_real_)),
          Max = as.double(max(get(var), na.rm = TRUE))
        ), by = c(group_var_name, placebo_var_name)]
      }
    }
    
    if (!is.null(stats)) {
      stats[, Variable := var]
    }
    
    return(stats)
  })
  
  # Remove NULL entries and ensure all columns are of the same type
  summary_stats <- Filter(Negate(is.null), summary_stats)
  
  # Identify all unique columns across all data.tables
  all_columns <- unique(unlist(lapply(summary_stats, names)))
  
  # Ensure each data.table has all columns, filling with NA where necessary
  summary_stats <- lapply(summary_stats, function(dt) {
    for (col in all_columns) {
      if (!(col %in% names(dt))) {
        dt[, (col) := NA_real_]
      }
    }
    # Ensure all columns (except character and factor columns) are double
    for (col in names(dt)) {
      if (!is.character(dt[[col]]) && !is.factor(dt[[col]])) {
        set(dt, j = col, value = as.double(dt[[col]]))
      }
    }
    return(dt[, ..all_columns])
  })
  
  summary_table <- rbindlist(summary_stats, fill = TRUE, use.names = TRUE)
  
  if (!is.null(group_var_name)) {
    setnames(summary_table, group_var_name, "Group")
    if (is.factor(dt[[group_var_name]])) {
      summary_table[, Group := factor(Group, levels = levels(dt[[group_var_name]]), labels = levels(dt[[group_var_name]]))]
    }
  } else {
    summary_table[, Group := "All"]
  }
  
  setnames(summary_table, placebo_var_name, "Placebo")
  if (is.factor(dt[[placebo_var_name]])) {
    summary_table[, Placebo := factor(Placebo, levels = levels(dt[[placebo_var_name]]), labels = levels(dt[[placebo_var_name]]))]
  }
  
  # Calculate total N for each variable
  total_N <- summary_table[, .(Total_N = sum(N)), by = .(Variable, Placebo)]
  
  # Add "All" group with total N
  all_stats <- summary_table[, lapply(.SD, function(x) if(is.numeric(x)) mean(x, na.rm = TRUE) else if (is.integer(x)) sum(x, na.rm = TRUE) else x[1]), 
                             by = .(Variable, Placebo), 
                             .SDcols = setdiff(names(summary_table), c("Group", "Variable", "Placebo", "N"))]
  all_stats[, Group := "All"]
  
  # Merge total N into all_stats
  all_stats <- merge(all_stats, total_N, by = c("Variable", "Placebo"))
  setnames(all_stats, "Total_N", "N")
  
  summary_table <- rbindlist(list(summary_table, all_stats), use.names = TRUE, fill = TRUE)
  
  # Determine columns dynamically based on the data
  all_cols <- names(summary_table)
  fixed_cols <- c("Group", "Placebo", "Variable")
  stat_cols <- setdiff(all_cols, fixed_cols)
  
  setcolorder(summary_table, c(fixed_cols, stat_cols))

  summary_table[, `:=`(Group = str_replace_all(Group, "_", " "), 
                       Placebo = str_replace_all(Placebo, "_", " "), 
                       Variable = str_replace_all(Variable, "_", " "))]
  
  return(summary_table)
}

generate_factor_summary <- function(dt, factor_vars, group_var, placebo_var) {
  dt <- as.data.table(dt)
  
  summary_list <- lapply(factor_vars, function(var) {
    full <- data.table()
    levels_group <- levels(dt[[group_var]])
    
    for (lvl in levels_group) {
      sub <- dt[get(group_var) == lvl]
      temp <- sub[, .N, by = c(placebo_var, var)]
      condition_total <- sub[, .(cond_N = .N), by = placebo_var]
      
      temp <- merge(temp, condition_total, by = placebo_var)
      temp[, Proportion := N / cond_N]
      
      temp_wide <- dcast(temp, as.formula(paste(placebo_var, "~", var)), value.var = "Proportion")
      temp_wide[, Group := lvl]
      full <- rbindlist(list(full, temp_wide), use.names = TRUE, fill = TRUE)
    }
    
    # Rename columns
    setnames(full, old = placebo_var, new = "Treatment Status")
    setnames(full, old = "Group", new = "Assigned Rumor")
    
    full[, Variable := var]
    return(full)
  })
  
  factor_summary_table <- rbindlist(summary_list, fill = TRUE, use.names = TRUE)
  
  # Reorder columns
  desired_order <- c("Treatment Status", "Assigned Rumor", "Variable")
  remaining_cols <- setdiff(names(factor_summary_table), desired_order)
  final_order <- c(desired_order, remaining_cols)
  
  setcolorder(factor_summary_table, final_order)
  
  return(factor_summary_table)
}

generate_factor_summary <- function(dt, factor_vars, placebo_var, group_var=NULL, weight_var = NULL) {
  require(data.table)
  require(Hmisc)
  
  dt <- as.data.table(dt)
  
  summary_list <- lapply(factor_vars, function(var) {
    full <- data.table()
    if (is.null(group_var)) {
      levels_group <- "All"
    } else {
      levels_group <- levels(dt[[group_var]])
    }
    
    for (lvl in levels_group) {
      if (is.null(group_var)) {
        sub <- dt
      } else {
        sub <- dt[get(group_var) == lvl]
      }
      
      if (is.null(weight_var)) {
        temp <- sub[, .N, by = c(placebo_var, var)]
        condition_total <- sub[, .(cond_N = .N), by = placebo_var]
        temp <- merge(temp, condition_total, by = placebo_var)
        temp[, Proportion := N / cond_N]
      } else {
        temp <- sub[, .(N = sum(get(weight_var))), by = c(placebo_var, var)]
        condition_total <- sub[, .(cond_N = sum(get(weight_var))), by = placebo_var]
        temp <- merge(temp, condition_total, by = placebo_var)
        temp[, Proportion := N / cond_N]
      }
      
      temp_wide <- dcast(temp, as.formula(paste(placebo_var, "~", var)), value.var = "Proportion")
      temp_wide[, Group := lvl]
      full <- rbindlist(list(full, temp_wide), use.names = TRUE, fill = TRUE)
    }
    
    # Rename columns
    setnames(full, old = placebo_var, new = "Treatment Status")
    if (!is.null(group_var)) {
      setnames(full, old = "Group", new = "Assigned Rumor")
    } else {
      full[, Group := NULL]
    }
    
    full[, Variable := var]
    return(full)
  })
  
  factor_summary_table <- rbindlist(summary_list, fill = TRUE, use.names = TRUE)
  
  # Reorder columns
  if (!is.null(group_var)) {
    desired_order <- c("Treatment Status", "Assigned Rumor", "Variable")
  } else {
    desired_order <- c("Treatment Status", "Variable")
  }
  remaining_cols <- setdiff(names(factor_summary_table), desired_order)
  final_order <- c(desired_order, remaining_cols)
  
  setcolorder(factor_summary_table, final_order)
  
  return(factor_summary_table)
}

create_latex_table <- function(dt, filename, title = NULL, caption = NULL, label = NULL, note = NULL, digits = 2) {
  if (is.null(caption)) {
    caption <- title
  }
  if (is.null(label)) {
    base_filename <- tools::file_path_sans_ext(basename(filename))
    label <- paste0("tab:", gsub("\\s+", "_", tolower(base_filename)))
  }
  
  # Format numeric columns
  numeric_cols <- sapply(dt, is.numeric)
  dt_formatted <- copy(dt)
  dt_formatted[, (names(numeric_cols)[numeric_cols]) := lapply(.SD, function(x) sprintf(paste0("%.", digits, "f"), x)), 
               .SDcols = names(numeric_cols)[numeric_cols]]
  
  # Format character and factor columns
  char_factor_cols <- sapply(dt, function(x) is.character(x) || is.factor(x))
  dt_formatted[, (names(char_factor_cols)[char_factor_cols]) := lapply(.SD, function(x) gsub("_", " ", x)), 
               .SDcols = names(char_factor_cols)[char_factor_cols]]
  
  # Format 'N' column separately
  if ("N" %in% names(dt_formatted)) {
    dt_formatted[, N := format(as.numeric(N), big.mark = ",")]
  }
  
  # Create table WITHOUT label parameter
  latex_table <- kable(dt_formatted, format = "latex", booktabs = TRUE, 
                       caption = caption,
                       align = "l",
                       escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position")) %>%
    row_spec(0, bold = TRUE)
  
  if (!is.null(note)) {
    latex_table <- latex_table %>% 
      footnote(general = note, general_title = "Note:", footnote_as_chunk = TRUE, threeparttable = FALSE)
  }
  
  # Convert to plain character string
  latex_string <- as.character(latex_table)
  
  # Find the caption line and add label after it
  latex_string <- sub("\\\\caption\\{([^}]*)\\}", 
                      paste0("\\\\caption{\\1}\n\\\\label{", label, "}"), 
                      latex_string)
  
  # Write the file directly since we're working with a string now
  writeLines(latex_string, filename)
  
  cat("Table saved to:", filename, "\n")
  
  return(latex_string)
}

calculate_stats <- function(data, variable, groups) {
  data %>%
    group_by(across(all_of(groups))) %>%
    dplyr::summarise(
      mean = mean({{variable}}, na.rm = TRUE),
      sd = sd({{variable}}, na.rm = TRUE),
      skewness = skewness({{variable}}, na.rm = TRUE),
      kurtosis = kurtosis({{variable}}, na.rm = TRUE),
      q10 = quantile({{variable}}, 0.10, na.rm = TRUE),
      q25 = quantile({{variable}}, 0.25, na.rm = TRUE),
      median = median({{variable}}, na.rm = TRUE),
      q75 = quantile({{variable}}, 0.75, na.rm = TRUE),
      q90 = quantile({{variable}}, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
}

calculate_treatment_effects <- function(data, variable, treatment_var, by_var = NULL, rumor_var = NULL) {
  # Convert arguments to quosures (quoted expressions)
  variable <- enquo(variable)
  treatment_var <- enquo(treatment_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  
  # Prepare grouping variables
  grouping_vars <- quos(!!treatment_var)
  if (!quo_is_null(by_var)) grouping_vars <- c(grouping_vars, by_var)
  if (!quo_is_null(rumor_var)) grouping_vars <- c(grouping_vars, rumor_var)
  
  # Calculate basic statistics for each group
  stats <- data %>%
    group_by(!!!grouping_vars) %>%
    dplyr::summarise(
      mean = mean(!!variable, na.rm = TRUE),
      sd = sd(!!variable, na.rm = TRUE),
      n = n(),
      .groups = "drop"
    )
  
  # Calculate treatment effects
  effects <- stats %>%
    group_by(across(-c(!!treatment_var, mean, sd, n))) %>%
    dplyr::summarise(
      mean_diff = diff(mean),
      pooled_sd = sqrt(sum((n - 1) * sd^2) / sum(n - 2)),
      cohens_d = mean_diff / pooled_sd,
      t_stat = mean_diff / (pooled_sd * sqrt(sum(1/n))),
      df = sum(n) - 2,
      p_value = 2 * pt(-abs(t_stat), df),
      treatment_var = quo_name(treatment_var),  # Add this line
      .groups = "drop"
    )
  
  return(effects)
}


plot_cumulative_density <- function(data, variable, by_var, treatment_var, rumor_var,
                                    title = "Cumulative Distribution of Changes",
                                    subtitle = "By Treatment Status",
                                    x_label = "Change",
                                    y_label = "Cumulative Proportion",
                                    output_file = NULL,
                                    weight_var = NULL) {
  
  # Ensure variables are quoted
  by_var <- enquo(by_var)
  treatment_var <- enquo(treatment_var)
  rumor_var <- enquo(rumor_var)
  variable <- enquo(variable)
  weight_var <- enquo(weight_var)
  
  # Calculate statistics
  stats <- data %>%
    group_by(!!treatment_var, !!rumor_var, !!by_var) %>%
    dplyr::summarise(
      q10 = Hmisc::wtd.quantile(!!variable, weights = !!weight_var, probs = 0.10, na.rm = TRUE),
      median = Hmisc::wtd.quantile(!!variable, weights = !!weight_var, probs = 0.50, na.rm = TRUE),
      q90 = Hmisc::wtd.quantile(!!variable, weights = !!weight_var, probs = 0.90, na.rm = TRUE),
      # q10 = quantile(!!variable, 0.10, na.rm = TRUE),
      # median = median(!!variable, na.rm = TRUE),
      # q90 = quantile(!!variable, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create the plot
  plot <- data %>%
    group_by(!!treatment_var, !!rumor_var, !!by_var) %>%
    arrange(!!variable) %>%
    mutate(cum_prop = row_number() / n()) %>%
    ggplot(aes(x = !!variable, y = cum_prop, 
               color = !!treatment_var,
               group = !!treatment_var)) +
    stat_ecdf(geom = "step") +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         color = "Treatment") +
    theme_minimal() +
    # scale_color_brewer(palette = "Set4") +
    scale_color_manual(values = wes_palette("Royal1", 2)) +
    scale_y_continuous(labels = percent_format()) +
    facet_grid(rows = vars(!!rumor_var), cols = vars(!!by_var)) +
    geom_text_repel(data = stats, 
                    aes(x = median, y = 0.6,
                        label = sprintf("%s\n10th %%: %.2f\n50th %%: %.2f\n90th %%: %.2f", 
                                        !!treatment_var, q10, median, q90),
                        color = !!treatment_var),
                    size = 3, show.legend = FALSE,
                    box.padding = 1.5, point.padding = 1.5,
                    min.segment.length = 0, seed = 123) +
    coord_cartesian(clip = "off") + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(size = 24),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15),
      strip.text = element_text(size = 20)
    )
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    ggsave(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}




plot_probability_density <- function(data, variable, by_var = NULL, treatment_var, rumor_var = NULL,
                                     title = "Distribution of Changes",
                                     subtitle = "By Treatment Status",
                                     x_label = "Change",
                                     y_label = "Count",
                                     output_file = NULL,
                                     na.rm=TRUE,
                                     weight_var = NULL) {
  
  # Ensure variables are quoted
  by_var <- enquo(by_var)
  treatment_var <- enquo(treatment_var)
  rumor_var <- enquo(rumor_var)
  variable <- enquo(variable)
  weight_var <- enquo(weight_var)
  
  data <- data %>% 
    dplyr::filter(!is.na(!!variable)) %>%
    {if (!quo_is_null(by_var)) dplyr::filter(., !is.na(!!by_var)) else .} %>%
    {if (!quo_is_null(weight_var)) dplyr::filter(., !is.na(!!weight_var)) else .} %>%
    droplevels()
  # Pre-calculate statistics
  stats <- data %>%
    group_by(!!treatment_var, !!rumor_var, !!by_var) %>%
    dplyr::summarise(
      count = n(),
      mean = Hmisc::wtd.mean(!!variable, na.rm = TRUE),
      sd = sd(!!variable, na.rm = TRUE),
      se = sd / sqrt(count),
      skewness = moments::skewness(!!variable, na.rm = TRUE),
      kurtosis = moments::kurtosis(!!variable, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate treatment effects (assuming this function exists)
  effects <- calculate_treatment_effects(data, !!variable, !!treatment_var, !!by_var, !!rumor_var)
  
  bar_position <- position_dodge2(preserve = "single")
  # Create the plot
  plot <- ggplot(data, aes(x = !!variable,
                           fill = !!treatment_var,
                           group = !!treatment_var)) +
    geom_histogram(aes(y = after_stat(count)), 
                   alpha = 0.7, 
                   position = bar_position,
                   bins = 30) +
    geom_errorbar(stat = "bin",
                  aes(y = after_stat(count),
                      ymin = after_stat(count - 1.96 * sqrt(count)),
                      ymax = after_stat(count + 1.96 * sqrt(count))),
                  position = bar_position,
                  color = "grey30",
                  width = 0.25) +
    # geom_rug(alpha = 0.2) +
    geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         fill = "Treatment") +
    theme_minimal() +
    scale_fill_manual(values = wes_palette("Royal1", 2)) +
    facet_grid(rows = vars(!!rumor_var), cols = vars(!!by_var)) +
    geom_text_repel(data = stats, 
                    aes(x = mean, y = Inf,
                        label = sprintf("%s\nMean: %.2f\nSD: %.2f\nSE: %.2f\nSkew: %.2f\nKurt: %.2f", 
                                        !!treatment_var, mean, sd, se, skewness, kurtosis),
                        color = !!treatment_var),
                    size = 3, show.legend = FALSE,
                    box.padding = 0.5, point.padding = 0.5,
                    min.segment.length = 0, seed = 123,
                    vjust = 1) +
                    
    geom_text(data = effects,
              aes(x = -Inf, y = Inf,
                  label = sprintf(
                    "Mean Diff: %.2f\nCohen's d: %.2f\np-value: %.3f",
                    mean_diff, cohens_d, p_value),
                    #  bold if p_value<0.05
                    fontface = ifelse(p_value < 0.05, 2, 1)
                  ),
              hjust = -0.1, vjust = 1.1, size = 5.0,
              inherit.aes = FALSE) +
    coord_cartesian(clip = "off") + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      plot.title = element_text(size = 24),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 15),
      strip.text = element_text(size = 16)
    )
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    ggsave(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}


# Function to run models for a specific rumor
run_rumor_models <- function(rumor_value, var_labels=post_y_vars_labels) {
  # rumor_label <- levels(dat_final$Topic)[rumor_value]
  # rumor_data <- subset(dat_final, Topic == rumor_label)
  rumor_data <- subset(dat_final, Topic == rumor_value)
  rumor_design <- svydesign(data = rumor_data, weights = ~weight, id = ~1)
  
  models <- lapply(var_labels, function(outcome) {
    predictors <- c(paste0(sub("Post_", "Pre_", outcome)), "Treatment * AI_Type", variables_in_model_labels)
    run_regression_for_table(outcome, predictors, rumor_design, family = gaussian())
  })
  
  names(models) <- var_labels
  return(models)
}


# Function to create tables for individual rumor models
create_rumor_tables <- function(rumor_models, rumor_names, table_label = "") {
  for (i in seq_along(rumor_names)) {
    rumor_table <- create_ols_summary_table(
      models = rumor_models[[i]],
      title = paste("OLS Regression Results for", rumor_names[i]),
      dep.var.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
      covariate.labels = c(
        pre_y_vars_labels %>% gsub("_", " ", .), 
        treatment_label, 
        covariate_labels),
      out.file = paste0("rumor_", i, "_table", table_label, output_label, ".tex"),
      label = paste0("tab:rumor_", i)
    )
    # print(paste("Table for", rumor_names[i], "has been created."))
  }
}

pre_post_plot <- function(data, pre_var, post_var, treatment_var, by_var, rumor_var,
                          title = "Pre vs Post Plot",
                          subtitle = "By Treatment and Group",
                          x_label = "Pre Score",
                          y_label = "Post Score",
                          output_file = NULL,
                          weight_var = NULL) {
  
  # Ensure variables are quoted
  pre_var <- enquo(pre_var)
  post_var <- enquo(post_var)
  treatment_var <- enquo(treatment_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  weight_var <- enquo(weight_var)

  if (!quo_is_null(weight_var)) {
    data <- data %>% filter(!is.na(!!weight_var))
  }
  
  # Function to calculate summary statistics
  calculate_summary_stats <- function(data) {
    data %>%
      dplyr::group_by(!!rumor_var, !!by_var, !!treatment_var) %>%
      dplyr::summarise(
        mean_x = Hmisc::wtd.mean(!!pre_var, weights = !!weight_var, na.rm = TRUE),
        mean_y = Hmisc::wtd.mean(!!post_var, weights = !!weight_var, na.rm = TRUE),
        sd_x = weighted.se.mean(!!pre_var, w = !!weight_var, na.rm = TRUE),
        sd_y = weighted.se.mean(!!post_var, w = !!weight_var, na.rm = TRUE),
        # sd_x = sd(!!pre_var, na.rm = TRUE),
        # sd_y = sd(!!post_var, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
  }
  
  # Calculate summary statistics
  summary_stats <- calculate_summary_stats(data)
  
  # Function to calculate effect sizes
  calculate_effect_sizes <- function(data) {
    data %>%
      dplyr::group_by(!!rumor_var, !!by_var) %>%
      dplyr::summarise(
        effect_size = -1 * effsize::cohen.d(
          formula = as.formula(paste("I(", quo_name(post_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$estimate,
        p_value = t.test(
          formula = as.formula(paste("I(", quo_name(post_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$p.value,
        .groups = "drop"
      )
  }

  # Calculate effect sizes
  effect_sizes <- calculate_effect_sizes(data)
  # aggregate data
  agg_data <- data %>%
    dplyr::group_by(!!rumor_var, !!by_var, !!treatment_var, !!pre_var, !!post_var) %>%
    dplyr::summarise(
      n = n(),
      .groups = "drop"
    )
    # print(agg_data)
  # scaleFUN <- function(x) sprintf("%.2f", x)
  if (quo_is_null(rumor_var) & quo_is_null(by_var)) {
    text_size <- 9  
  } else if (quo_is_null(rumor_var) | quo_is_null(by_var)) {
    text_size <- 6
  } else {
    text_size <- 3
  }
  
  # Create the plot
  plot <- ggplot(agg_data, aes(x = !!pre_var, y = !!post_var, color = !!treatment_var)) +
    # geom_point(aes(size = n), position = position_jitter(width = 0.1, height = 0.1)) +
    geom_point(aes(size=n, alpha=n), position=position_jitter(width=0.1, height=0.1)) +
    scale_alpha(range = c(0.3, 1.0)) + 
    # geom_jitter(aes(size=n)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 5, linetype = "dotted", color = "grey40") + 
    geom_smooth(method = "lm", se = FALSE, aes(weight = n)) +
    geom_point(data = summary_stats, aes(x = mean_x, y = mean_y), size = 5, shape = 23, fill = "white") +
    ggrepel::geom_text_repel(
      data = summary_stats,
      aes(x = mean_x, y = mean_y, 
          label = sprintf("%s\nMean (x, y): (%.2f, %.2f)\nSD (x, y): (%.2f, %.2f)\nn: %d", 
                          !!treatment_var, mean_x, mean_y, sd_x, sd_y, n)),
      inherit.aes = FALSE,
      size = text_size,
      box.padding = 2.5,
      point.padding = 2.5,
      min.segment.length = 0,
      direction = 'both',
      position = position_nudge_line(
        x = c(2, -3), 
        y = c(1, -3),
        direction = "split"
      )
    ) +
    geom_text(
      data = effect_sizes,
      aes(x = -Inf, y = Inf, 
          label = sprintf("ATT Cohen's d: %.2f\np-value: %.3f", 
                          effect_size, p_value),
          #  bold if p_value<0.05
          fontface = ifelse(p_value < 0.05, 2, 1)
      ),
      hjust = -0.1, vjust = 1.1, size = text_size,
      inherit.aes = FALSE
    ) +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         color = "Treatment") +
    coord_cartesian(xlim = c(0,10), ylim = c(0,10)) +
    facet_grid(vars(!!rumor_var), vars(!!by_var)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 24),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      strip.text = element_text(size = 20)
    ) +
    # scale_y_continuous(labels=scales::label_number(accuracy=1)) + 
    # scale_x_continuous(labels=scales::label_number(accuracy=1)) +
    scale_y_continuous(breaks = 0:10, labels = 0:10) + 
    scale_x_continuous(breaks = 0:10, labels = 0:10) +
    scale_color_manual(values = wesanderson::wes_palette("Royal1", 2)) + 
    # Add arrows and labels for "persuaded for" and "persuaded against"
    annotate("segment", x = 0, xend = 0, y = 0, yend = 4, 
             arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
             color = "blue") +
    annotate("text", x = 1, y = 5, label = "increased\nconfidence", color = "blue", size = text_size) +
    annotate("segment", x = 0, xend = 4, y = 0, yend = 0, 
             arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
             color = "blue") +
    annotate("text", x = 5, y = 1, label = "decreased\nconfidence", color = "blue", size = text_size)
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}



pre_post_diff_plot <- function(data, pre_var, diff_var, treatment_var, by_var, rumor_var,
                          title = "Pre vs Diff Plot",
                          subtitle = "By Treatment and Group",
                          x_label = "Pre Score",
                          y_label = "Post-Pre Diff Score",
                          output_file = NULL,
                          weight_var = NULL) {
  
  # Ensure variables are quoted
  pre_var <- enquo(pre_var)
  diff_var <- enquo(diff_var)
  treatment_var <- enquo(treatment_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  weight_var <- enquo(weight_var)

  if (!quo_is_null(weight_var)) {
    data <- data %>% filter(!is.na(!!weight_var))
  }
  
  # Function to calculate summary statistics
  calculate_summary_stats <- function(data) {
    data %>%
      dplyr::group_by(!!rumor_var, !!by_var, !!treatment_var) %>%
      dplyr::summarise(
        mean_x = Hmisc::wtd.mean(!!pre_var, weights = !!weight_var, na.rm = TRUE),
        mean_y = Hmisc::wtd.mean(!!diff_var, weights = !!weight_var, na.rm = TRUE),
        sd_x = weighted.se.mean(!!pre_var, w = !!weight_var, na.rm = TRUE),
        sd_y = weighted.se.mean(!!diff_var, w = !!weight_var, na.rm = TRUE),
        # mean_x = mean(!!pre_var, na.rm = TRUE),
        # mean_y = mean(!!diff_var, na.rm = TRUE),
        # sd_x = sd(!!pre_var, na.rm = TRUE),
        # sd_y = sd(!!diff_var, na.rm = TRUE),
        n = dplyr::n(),
        .groups = "drop"
      )
  }
  
  # Calculate summary statistics
  # summary_stats <- calculate_summary_stats(data)
  summary_stats <- calculate_summary_stats(data) %>%
    mutate(
      label_nudge_y = ifelse(!!treatment_var == levels(!!treatment_var)[1], 8, -8),
      label_nudge_x = ifelse(!!treatment_var == levels(!!treatment_var)[1], 1, -3)
    )

  # Function to calculate effect sizes
  calculate_effect_sizes <- function(data) {
    data %>%
      dplyr::group_by(!!rumor_var, !!by_var) %>%
      dplyr::summarise(
        effect_size = -1 * effsize::cohen.d(
          formula = as.formula(paste("I(", quo_name(diff_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$estimate,
        p_value = t.test(
          formula = as.formula(paste("I(", quo_name(diff_var), "-", quo_name(pre_var), ") ~", quo_name(treatment_var))),
          data = cur_data()
        )$p.value,
        .groups = "drop"
      )
  }
  
  # Calculate effect sizes
  effect_sizes <- calculate_effect_sizes(data)
  # aggregate data
  agg_data <- data %>%
    dplyr::group_by(!!rumor_var, !!by_var, !!treatment_var, !!pre_var, !!diff_var) %>%
    dplyr::summarise(
      n = n(),
      .groups = "drop"
    )
  # scaleFUN <- function(x) sprintf("%.2f", x)
  if (quo_is_null(rumor_var) & quo_is_null(by_var)) {
    text_size <- 9  
  } else if (quo_is_null(rumor_var) | quo_is_null(by_var)) {
    text_size <- 6
  } else {
    text_size <- 3
  }
  # Create the plot
  plot <- ggplot(agg_data, aes(x = !!pre_var, y = !!diff_var, color = !!treatment_var)) +
    # geom_jitter(alpha = 0.2) +
    geom_point(aes(size=n, alpha=n), position=position_jitter(width=0.1, height=0.1)) +
    scale_alpha(range = c(0.3, 1.0)) + 
    # geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 5, linetype = "dotted", color = "grey40") + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
    geom_smooth(method = "lm", se = FALSE, aes(weight=n)) +
    geom_point(data = summary_stats, aes(x = mean_x, y = mean_y), size = 5, shape = 23, fill = "white") +
    ggrepel::geom_text_repel(
      data = summary_stats,
      aes(x = mean_x, y = mean_y, 
          label = sprintf("%s\nMean (x, y): (%.2f, %.2f)\nSD (x, y): (%.2f, %.2f)\nn: %d", 
                          !!treatment_var, mean_x, mean_y, sd_x, sd_y, n),
          segment.square = FALSE,
          segment.inflect = TRUE),
      inherit.aes = FALSE,
      size = text_size,
      box.padding = 0.5,
      point.padding = 0.5,
      min.segment.length = 0,
      direction = 'y',
      hjust = 0,
      nudge_x = summary_stats$label_nudge_x,
      nudge_y = summary_stats$label_nudge_y
    ) +
    geom_text(
      data = effect_sizes,
      aes(x = -Inf, y = Inf, 
          label = sprintf("ATT Cohen's d: %.2f\np-value: %.3f", 
                          effect_size, p_value),
          #  bold if p_value<0.05
          fontface = ifelse(p_value < 0.05, 2, 1)
      ),
      hjust = -0.1, vjust = 1.1, size = text_size,
      inherit.aes = FALSE
    ) +
    labs(title = title,
         subtitle = subtitle,
         x = x_label,
         y = y_label,
         color = "Treatment") +
    coord_cartesian(xlim = c(0,10), ylim = c(-10,10)) +
    facet_grid(vars(!!rumor_var), vars(!!by_var)) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 24),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      strip.text = element_text(size = 20)
    ) +
    # scale_y_continuous(labels=scales::label_number(accuracy=1)) + 
    # scale_x_continuous(labels=scales::label_number(accuracy=1)) +
    scale_y_continuous(
      breaks = c(-10, -8, -5, -2, 0, 2, 5, 8, 10),
      labels = c(-10, -8, -5, -2, 0, 2, 5, 8, 10),
      minor_breaks = -10:10
    ) +
    scale_x_continuous(
      breaks = c(0, 2, 5, 8, 10),
      labels = c(0, 2, 5, 8, 10),
      minor_breaks = 0:10
    ) +
    scale_color_manual(values = wesanderson::wes_palette("Royal1", 2))
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, plot, width = 14, height = 16)
  }
  
  return(plot)
}

# Function to create CISA time series plot
create_time_series_plot <- function(data, treatment_var, pre_var, post_var, recontact_var, by_var, rumor_var, ylab="Score", title="Time Series Plot", subtitle = "By Treatment Condition", weight_var = NULL) {
  treatment_var <- enquo(treatment_var)
  pre_var <- enquo(pre_var)
  post_var <- enquo(post_var)
  recontact_var <- enquo(recontact_var)
  by_var <- enquo(by_var)
  rumor_var <- enquo(rumor_var)
  weight_var <- enquo(weight_var)

  # Prepare the data
  plot_data <- data %>% 
    dplyr::select(
      !!treatment_var, 
      !!pre_var, 
      !!post_var,
      !!recontact_var,
      !!by_var, 
      !!rumor_var,
      !!weight_var
    )

  if (!quo_is_null(weight_var)) {
    plot_data <- plot_data %>% 
      dplyr::filter(!is.na(!!weight_var))
  }

  plot_data_long <- plot_data %>% 
    pivot_longer(cols = c(!!pre_var, !!post_var, !!recontact_var),
                 names_to = "time", 
                 values_to = "score") %>%
      mutate(time = case_when(
        time == quo_name(pre_var) ~ "Pre",
        time == quo_name(post_var) ~ "Post",
        time == quo_name(recontact_var) ~ "Recontact",
        TRUE ~ time
      ) %>% 
    factor(levels = c("Pre", "Post", "Recontact")))
    #               %>%
    # mutate(time = factor(time, levels = c(!!pre_var, !!post_var, !!recontact_var),
    #                      labels = c("Pre", "Post", "Recontact")))

  # aggregate data
  plot_data_agg <- plot_data_long %>%
    group_by(!!treatment_var, !!by_var, !!rumor_var, time, score) %>%
    summarise(
      n = n(),
      .groups = 'drop'
    ) 
  # Calculate means and standard errors
  dat_summary <- plot_data_long %>%
    group_by(time, !!treatment_var, !!by_var, !!rumor_var) %>%
    summarise(
      # weighted version of mean and se if weight_var is provided
      mean_score = ifelse(quo_is_null(weight_var), mean(score, na.rm = TRUE), weighted.mean(score, w = !!weight_var, na.rm = TRUE)),
      # mean_score = mean(score, na.rm = TRUE),
      se = ifelse(quo_is_null(weight_var), sd(score, na.rm = TRUE) / sqrt(n()), weighted.se.mean(score, w = !!weight_var, na.rm = TRUE)),
      # se = sd(score, na.rm = TRUE) / sqrt(n()),
      .groups = 'drop'
    )
  dodge_width <- 0.2
  # Create the plot
  p <- ggplot() +
    # Individual points
    # geom_jitter(data = plot_data_long, 
    #             aes(x = time, y = score, color = !!treatment_var),
    #             alpha = 0.2, width = 0.2) +
    geom_point(data = plot_data_agg, 
               aes(x = time, y = score, color = !!treatment_var, size = n),
               position = position_jitter(width=0.1, height=0.1)) +
    geom_violinhalf(data = plot_data_long, 
                aes(x = time, y = score, color = !!treatment_var, fill = !!treatment_var),
                alpha = 0.4, position = "identity", flip = TRUE, width = 0.5
                ) +
    # Means and error bars
    geom_line(data = dat_summary,
              aes(x = time, y = mean_score, color = !!treatment_var, group = !!treatment_var),
              # position = position_dodge(width = dodge_width),
              position = position_nudge(x = dodge_width),
              size = 1) +
    geom_point(data = dat_summary,
               aes(x = time, y = mean_score, color = !!treatment_var),
              #  position = position_dodge(width = dodge_width),
              position = position_nudge(x = dodge_width),
               size = 5) +
    geom_errorbar(data = dat_summary,
                  aes(x = time, y = mean_score, 
                      ymin = mean_score - 1.96 * se, 
                      ymax = mean_score + 1.96 * se,
                      color = !!treatment_var),
                  # position = position_dodge(width = dodge_width),
                  position = position_nudge(x = dodge_width),
                  width = 0.3) +
    coord_cartesian(ylim = c(0, 10)) +
    facet_grid(vars(!!rumor_var), vars(!!by_var)) +
    labs(title = title,
        subtitle = subtitle,
         x = "Time",
         y = ylab,
         color = "Treatment") + 
    theme_minimal() + 
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 24),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 20),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 18),
      strip.text = element_text(size = 20)
    ) +
    guides(fill = "none") +
    # theme(
    #   legend.position = "bottom",
    #   axis.text.x = element_text(angle = 45, hjust = 1),
    #   strip.text = element_text(face = "bold"),
    #   panel.spacing = unit(1, "lines")
    # ) + 
    scale_color_manual(values = wes_palette("Royal1", 2), 
                       labels = c("Control", "Treatment")) + 
    scale_fill_manual(values = wes_palette("Royal1", 2),
                      labels = c("Control", "Treatment"))

  return(p)
}


plot_coefficients <- function(model_lists, 
                                       var_name = "TreatmentTreatment",
                                       title = "Estimated Treatment Effects for OLS Models",
                                       subtitle = "Across Different Outcomes",
                                       x_label = "Estimated Treatment Effect",
                                       y_label = "Model",
                                       point_size = 3,
                                       error_bar_height = 0.2,
                                       text_size = 12,
                                       wrap_width = 30,
                                       debug = FALSE) {
  
  extract_coefs <- function(model, var_name, model_name, is_cisa = FALSE) {
    if (is.null(model) || !inherits(model, "lm")) {
      if(debug) cat("Skipping", model_name, "- not an lm object\n")
      return(NULL)
    }
    
    coef_data <- tidy(model) %>%
      filter(term == var_name) %>%
      select(estimate, std.error) %>%
      mutate(
        model = if (is_cisa) {
          parts <- str_split(model_name, ": ", n = 3)[[1]]
          paste(parts[1:min(2, length(parts))], collapse = ": ")
        } else model_name,
        significance = case_when(
          estimate - 1.96 * std.error > 0 ~ "Positive",
          estimate + 1.96 * std.error < 0 ~ "Negative",
          TRUE ~ "Insignificant"
        ),
        type = if_else(str_detect(model_name, "Recontact|Followup"), "Recontact", "Post-treatment")
      )
    
    if (nrow(coef_data) == 0) {
      if(debug) cat("No coefficients found for", model_name, "\n")
      return(NULL)
    }
    if(debug) cat("Extracted coefficients for", model_name, "\n")
    coef_data
  }
  
  process_model_list <- function(models, prefix = "") {
    if(debug) cat("Processing", prefix, "\n")
    map_dfr(names(models), function(model_name) {
      full_name <- paste(prefix, model_name, sep = if(prefix == "") "" else ": ")
      if (is.list(models[[model_name]]) && !inherits(models[[model_name]], "lm")) {
        if(debug) cat("Recursing into", full_name, "\n")
        process_model_list(models[[model_name]], full_name)
      } else {
        if(debug) cat("Extracting coefficients for", full_name, "\n")
        extract_coefs(models[[model_name]], var_name, full_name, is_cisa = grepl("CISA", model_name))
      }
    })
  }
  
  all_coefs <- process_model_list(model_lists)
  
  if (nrow(all_coefs) == 0) {
    stop("No coefficients found for the specified variable name.")
  }
  
  if(debug) {
    print(str(all_coefs))
    print(head(all_coefs))
  }
  
  all_coefs <- all_coefs %>%
    mutate(model = factor(model, levels = rev(unique(model))))
  
  ggplot(all_coefs, aes(x = estimate, y = model, color = significance, shape = type)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = point_size) +
    geom_errorbarh(aes(xmin = estimate - 1.96 * std.error, 
                       xmax = estimate + 1.96 * std.error), 
                   height = error_bar_height) +
    labs(x = x_label, y = y_label, title = title, subtitle = subtitle) +
    scale_color_manual(values = c("Positive" = "blue", "Negative" = "red", "Insignificant" = "gray50")) +
    scale_shape_manual(values = c("Post-treatment" = 16, "Recontact" = 17)) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title = element_text(face = "bold", size = text_size),
      plot.title = element_text(face = "bold", hjust = 0.5, size = text_size * 1.2),
      plot.subtitle = element_text(hjust = 0.5, size = text_size),
      axis.text = element_text(size = text_size * 0.8),
      legend.position = "bottom",
      plot.title.position = "plot"
    ) +
    scale_y_discrete(labels = function(x) str_wrap(x, width = wrap_width))
}

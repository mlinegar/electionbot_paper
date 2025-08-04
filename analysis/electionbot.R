#### Load libraries ####
# install.packages(c(
#   "survey", "forcats", "haven", "tidyverse", "margins", "effects", "svyVGAM", "stargazer",
#   "broom", "scales", "moments", "kableExtra", "rlang", "effsize", "wesanderson", "stringr",
#   "ggrepel", "ggpp", "broom", "purrr", "see", "xtable"
# ))
# https://stackoverflow.com/questions/69790523/installing-systemfonts-package-from-cran-and-github-fails
# withr::with_makevars(c(OBJCXX = "gcc"), install.packages('systemfonts'))

start_fresh <- FALSE
rumors <- c("Voter Rolls", "Non-Citizen Voting")


library(survey)
library(forcats)
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(margins)
library(effects)
library(svyVGAM)
library(stargazer)
library(broom)
library(scales)
library(moments)
library(kableExtra)
library(rlang)
library(effsize)
library(wesanderson)
library(stringr)
library(data.table)
library(effects)
library(ggrepel)
library(ggpp)
library(broom)
library(purrr)
library(see)
library(xtable)
library(Hmisc)

'%!in%' <- function(x,y)!('%in%'(x,y))

#### Define key fixed variables ####
# data_dir <- "."
data_dir <- "/Users/mlinegar/code/electionbot"
output_dir <- paste0(data_dir, "/output")
data_file <- "washu_generativeai_recontact_octnov24.sav"
# recontact_file <- "caltech_elections_augustrecontact24.sav"

int_var <- "Party_Identification"
# int_var <- "Conspiracy_Score"

#### Prepare data ####

# Define constants
total_populism_questions <- 6
total_conspiracy_questions <- 6
rating_max <- 5
rating_min <- 1

output_label <- ""
# output_label <- "_pid"

# Define variables in model and their labels
y_vars_post <- c("ballotcount_scale_2", "ballotcounty_scale_2", "ballotcountry_scale_2")
y_vars_pre <- c("ballotcount_scale", "ballotcounty_scale", "ballotcountry_scale")
y_vars_diff <- c("ballotcount_diff", "ballotcounty_diff", "ballotcountry_diff")
y_vars_recontact <- c("ballotcount_scale_recontact", "ballotcounty_scale_recontact", "ballotcountry_scale_recontact")
y_vars_diff_recontact <- c("ballotcount_diff_recontact", "ballotcounty_diff_recontact", "ballotcountry_diff_recontact")
cisa_y_vars_diff_recontact <- c("cisa_rel_diff", "cisa_rel_diff_recontact", "cisa_other_diff", "cisa_other_diff_recontact")
# cisa_y_vars_diff_recontact <- c("cisa_rel_diff", "cisa_rel_diff_recontact", "cisa_fake_diff_recontact", "cisa_true_diff_recontact")
# motivated_y_vars_recontact <- c("changemind_recontact", "election1_scale_recontact", "election2_scale_recontact", "election3_scale_recontact")

variables_in_model <- c("age4", "gender", "race4", "educ4", "pid3", "ideo3", 
                        "region", "urbancity", "newsint", "conspiracy") #, "populism"

treatment_label <- "Treatment (vs. Placebo)"
ai_type_label <- "Chatbot (vs. Article)"
# Create a list of nice labels for variables
variable_labels <- list(
  age4 = "Age_Group",
  gender = "Gender",
  race4 = "Race_Ethnicity",
  educ4 = "Education_Level",
  pid3 = "Party_Identification",
  ideo3 = "Ideology",
  region = "Region",
  urbancity = "Urban_Rural",
  newsint = "Political_Interest",
  populism = "Populism_Score",
  conspiracy = "Conspiracy_Score",
  mist = "MIST_Correct",
  anes = "ANES_Representation",
  anes_recontact = "ANES_Representation_Recontact",
  anes_1_diff = "ANES_Representation_Diff_1",
  anes_2_diff = "ANES_Representation_Diff_2",
  populism_bin = "Populism_Bin",
  conspiracy_bin = "Conspiracy_Bin",
  topic = "Topic",
  treatment = "Treatment",
  ai_type = "AI_Type",
  ballotcount_scale = "Pre_Confidence_Own_Ballot",
  ballotcounty_scale = "Pre_Confidence_County_Ballots",
  ballotcountry_scale = "Pre_Confidence_Country_Ballots",
  ballotcount_scale_2 = "Post_Confidence_Own_Ballot",
  ballotcounty_scale_2 = "Post_Confidence_County_Ballots",
  ballotcountry_scale_2 = "Post_Confidence_Country_Ballots",
  ballotcount_scale_recontact = "Recontact_Confidence_Own_Ballot",
  ballotcounty_scale_recontact = "Recontact_Confidence_County_Ballots",
  ballotcountry_scale_recontact = "Recontact_Confidence_Country_Ballots",
  ballotcount_diff = "Confidence_Own_Ballot_Diff",
  ballotcounty_diff = "Confidence_County_Ballots_Diff",
  ballotcountry_diff = "Confidence_Country_Ballots_Diff",
  ballotcount_diff_recontact = "Recontact_Confidence_Own_Ballot_Diff",
  ballotcounty_diff_recontact = "Recontact_Confidence_County_Ballots_Diff",
  ballotcountry_diff_recontact = "Recontact_Confidence_Country_Ballots_Diff",
  cisa_rel = "Rumor",
  cisa_rel_post = "Rumor_Post",
  cisa_rel_diff = "Rumor_Diff",
  cisa_rel_recontact = "Rumor_Recontact",
  cisa_rel_diff_recontact = "Rumor_Diff_Recontact",
  cisa_other = "Rumor_Other",
  cisa_other_post = "Rumor_Other_Post",
  cisa_other_diff = "Rumor_Other_Diff",
  cisa_other_recontact = "Rumor_Other_Recontact",
  cisa_other_diff_recontact = "Rumor_Other_Diff_Recontact",
  cisa_fake = "All_Rumors",
  cisa_fake_recontact = "All_Rumors_Recontact",
  cisa_fake_diff_recontact = "All_Rumors_Diff_Recontact",
  cisa_true = "All_Facts",
  cisa_true_recontact = "All_Facts_Recontact",
  cisa_true_diff_recontact = "All_Facts_Diff_Recontact",
  article_recalled = "Article_Recalled_Recontact",
  changemind_recontact = "Tried_To_Change_Friends_Mind",
  election1_scale_recontact = "Motivated_To_Debunk",
  election2_scale_recontact = "Equipped_To_Push_Back",
  election3_scale_recontact = "Willing_To_Argue_Against",
  harris_vote = "Vote_Harris_2024"
)


variables_in_model_labels <- variable_labels[variables_in_model] %>% unlist() %>% as.character()
post_y_vars_labels <- variable_labels[y_vars_post] %>% unlist() %>% as.character()
pre_y_vars_labels <- variable_labels[y_vars_pre] %>% unlist() %>% as.character()
diff_y_vars_labels <- variable_labels[y_vars_diff] %>% unlist() %>% as.character()
recontact_y_var_labels <- variable_labels[y_vars_recontact] %>% unlist() %>% as.character()
recontact_diff_y_var_labels <- variable_labels[y_vars_diff_recontact] %>% unlist() %>% as.character()
recontact_cisa_diff_y_var_labels <- variable_labels[cisa_y_vars_diff_recontact] %>% unlist() %>% as.character()

all_var_labels <- c(pre_y_vars_labels, post_y_vars_labels, diff_y_vars_labels, variables_in_model_labels)


### DEFINE FUNCTIONS ###
source("./helper_functions.R")

#### DATA PROCESSING ####

if (start_fresh){
  print("Starting from scratch...")
  washu_generativeai_octnov24 <- read_sav(file.path(data_dir, data_file))
  # washu_generativeai_octnov24_recontact <- read_sav(file.path(data_dir, recontact_file))

  # Process the data
  dat <- washu_generativeai_octnov24 %>%
    mutate(
      # Handle skipped values (8 for most variables, 998 for some scale variables)
      across(matches("^(conspiracy|information|populism|attention|mist)_\\d+$"), ~handle_skipped(., 8)),
      across(matches("^(ballotco|cisa)\\w+_scale"), ~handle_skipped(., 998)),
      across(matches("^(ballotco|cisa)\\w+_scale"), ~handle_skipped(., -1)),
      across(matches("^newsint$"), ~handle_skipped(., 7)),
      across(matches("^urbancity$"), ~handle_skipped(., 5)),
      across(matches("^anes$"), ~handle_skipped(., 8)),

      # Flip scales for variables where low values indicate agreement
      across(matches("^conspiracy_\\d+$"), ~flip_scale(., rating_min, rating_max)),
      across(matches("^populism_\\d+$"), ~flip_scale(., rating_min, rating_max)),
      across(matches("^anes_\\d+$"), ~flip_scale(., rating_min, rating_max)),
      anes = anes_1 + anes_2,
      anes_recontact = anes_1_2 + anes_2_2,
      anes_1_diff = anes_1_2 - anes_1,
      anes_2_diff = anes_2_2 - anes_2,
      cisa_rel = case_when(
        topic == 1 ~ cisa1_scale,
        topic == 2 ~ cisa2_scale
        ),
      cisa_rel_post = case_when(
          topic == 1 ~ cisa1_scale_2,
          topic == 2 ~ cisa2_scale_2
      ),
      cisa_other_post = case_when(
          topic == 1 ~ cisa2_scale_2 + cisa3_scale_2,
          topic == 2 ~ cisa1_scale_2 + cisa3_scale_2
      ),
      cisa_rel_recontact = case_when(
        topic == 1 ~ cisa1_scale_recontact,
        topic == 2 ~ cisa2_scale_recontact
        ),
      cisa_other = case_when(
          topic == 1 ~ cisa2_scale + cisa3_scale,
          topic == 2 ~ cisa1_scale + cisa3_scale
      ),
      cisa_other_recontact = case_when(
          topic == 1 ~ cisa2_scale_recontact + cisa3_scale_recontact,
          topic == 2 ~ cisa1_scale_recontact + cisa3_scale_recontact
      ),
      # Recode information questions
      across(matches("^information_\\d+$"), ~factor(5 - ., levels = 1:4, labels = c("Never", "Rarely", "Sometimes", "Often"))),
      
      # Calculate MIST, populism, and conspiracy scores
      populism = rowSums(select(., matches("^populism_\\d+$")), na.rm = TRUE),
      conspiracy = rowSums(select(., matches("^conspiracy_\\d+$")), na.rm = TRUE),
      # mist = rowSums(select(., matches("^correct_mist_\\d+$")), na.rm = TRUE),

      # Recode attention check questions
      attention_1_correct = attention_1 == 3,
      attention_2_correct = attention_2 == 2,
      
      # Recode demographic variables
      gender2 = case_when(
        gender4 %in% c(1, 2) ~ gender4,
        TRUE ~ NA_real_
      ),
      
      urbancity4 = case_when(
        urbancity %in% 1:4 ~ urbancity,
        TRUE ~ NA_real_
      ),
      
      newsint4 = case_when(
        newsint %in% 1:4 ~ newsint,
        TRUE ~ NA_real_
      ),
      
      pid3 = case_when(
        pid3 %in% 1:2 ~ pid3,
        pid3 %in% 3:5 ~ 3,
        TRUE ~ NA_real_
      ),
      
      ideo3 = case_when(
        ideo3 %in% 1:3 ~ ideo3,
        TRUE ~ NA_real_
      ),

      # Convert variables to factors and label them
      across(c(gender, gender4, urbancity, age4, race, race4, educ, educ4, pid3, pid7, ideo3, ideo5, region, newsint,
              topic, 
              treatment,
              ai_type
              #  , electionrumor_pipe
              ), as.factor),
      
      # Add labels to rumor variables
      topic = fct_recode(topic, 
                                      "Non-Citizen Voting" = "1", 
                                      "Voter Rolls" = "2"),
      
      # Recode and relevel treatment
      treatment = fct_recode(treatment, "Treatment" = "1", "Placebo" = "2"),
      treatment = fct_relevel(treatment, "Placebo"),
      ai_type = fct_recode(ai_type, "AI Chatbot" = "1", "AI Article" = "2"),

      gender = fct_recode(gender, "Male" = "1", "Female" = "2"),
      age4 = fct_recode(age4, "Under 30" = "1", "30-44" = "2", "45-64" = "3", "65+" = "4"),
      age4 = fct_relevel(age4, "Under 30", "30-44", "45-64", "65+"),
      race4 = fct_recode(race4, "White" = "1", "Black" = "2", "Hispanic" = "3", "Other" = "4"),
      educ4 = fct_recode(educ4, "HS or less" = "1", "Some college" = "2", "College grad" = "3", "Postgrad" = "4"),
      educ4 = fct_relevel(educ4, "HS or less", "Some college", "College grad", "Postgrad"),
      pid3 = fct_recode(pid3, "Democrat" = "1", "Republican" = "2", "Independent" = "3"),
      pid3 = fct_relevel(pid3, "Democrat", "Independent", "Republican"),
      ideo3 = fct_recode(ideo3, "Liberal" = "1", "Moderate" = "2", "Conservative" = "3"),
      ideo3 = fct_relevel(ideo3, "Liberal", "Moderate", "Conservative"),
      region = fct_recode(region, "Northeast" = "1", "Midwest" = "2", "South" = "3", "West" = "4"),
      urbancity = fct_recode(urbancity, "City" = "1", "Suburb" = "2", "Town" = "3", "Rural area" = "4"),
      newsint = fct_recode(newsint, "Pol Interest: Most of the time" = "1", "Pol Interest: Some of the time" = "2", 
                          "Pol Interest: Only now and then" = "3", "Pol Interest: Hardly at all" = "4")
    )

  # Create bins for populism and conspiracy scores
  populism_bins_and_labels <- create_bins_and_labels(rating_min, rating_max, total_populism_questions)
  conspiracy_bins_and_labels <- create_bins_and_labels(rating_min, rating_max, total_conspiracy_questions)

  dat <- dat %>%
    mutate(
      populism_bin = cut(populism, breaks = populism_bins_and_labels$bins, labels = populism_bins_and_labels$labels),
      conspiracy_bin = cut(conspiracy, breaks = conspiracy_bins_and_labels$bins, labels = conspiracy_bins_and_labels$labels),
      mist = rowSums(select(., matches("^correct_mist_\\d+$")), na.rm = TRUE),
      ballotcount_diff = ballotcount_scale_2 - ballotcount_scale,
      ballotcounty_diff = ballotcounty_scale_2 - ballotcounty_scale,
      ballotcountry_diff = ballotcountry_scale_2 - ballotcountry_scale,
      cisa_fake = rowSums(select(., matches("^cisa[1-5]_scale$")), na.rm = TRUE),
      cisa_true = rowSums(select(., matches("^cisa[6-9]_scale$|^cisa_10_scale$")), na.rm = TRUE),
      cisa_rel_diff = cisa_rel_post - cisa_rel,
      cisa_rel_diff_recontact = cisa_rel_recontact - cisa_rel,
      cisa_other_diff = cisa_other_recontact - cisa_other,
      # cisa_other_recontact # defined above
      cisa_other_diff_recontact = cisa_other_recontact - cisa_other,
      harris_vote = presvote24post_recontact == 1
    )

  # cisa_rel_recontact = "Rumor_Recontact",
  #   cisa_rel_diff_recontact = "Rumor_Diff_Recontact",
  #   cisa_other_diff = "Rumor_Other_Diff",
  #   cisa_other_recontact = "Rumor_Other_Recontact",
  #   cisa_other_diff_recontact = "Rumor_Other_Diff_Recontact",

  #### MERGE RECONTACT DATA ####
  dat_final <- copy(dat)

  dat_final <- dat_final %>%
    mutate(
      ballotcount_diff_recontact = ballotcount_scale_recontact - ballotcount_scale,
      ballotcounty_diff_recontact = ballotcounty_scale_recontact - ballotcounty_scale,
      ballotcountry_diff_recontact = ballotcountry_scale_recontact - ballotcountry_scale,
      cisa_diff = cisa_rel_post - cisa_rel,
      cisa_other_diff = cisa_other_post - cisa_other,
      cisa_rel_diff_recontact = cisa_rel_recontact - cisa_rel,
      cisa_other_diff_recontact = cisa_other_recontact - cisa_other
      )


  #### FILTERING ####
  # Remove rows with missing values in any of the variables used in the model
  complete_cases <- complete.cases(dat_final[,variables_in_model])

  # Rename the variables in dat_final
  dat_final <- rename_variables(dat_final, variable_labels)
  setDT(dat_final)
  dat_final <- dat_final[chatbot_completeness_level>0 | AI_Type=="AI Article"]

  fwrite(dat_final, file.path(data_dir, "prebunk_full.csv"))
} else {
  print("Reading in prebunk data...")
  dat_final <- fread(file.path(data_dir, "prebunk_full.csv"))
}

svy_design <- svydesign(data = dat_final, weights = ~weight, id = ~1)


covariate_labels <- c()
for (var in variables_in_model_labels) {
  var_levels <- levels(dat_final[[var]])[-1]
  if (!is.null(var_levels)){
    covariate_labels <- c(covariate_labels, var_levels)
    } else {
    covariate_labels <- c(covariate_labels, gsub("_", " ", var))
    }
}

#### SUMMARY STATISTICS ####
dt <- as.data.table(dat_final)
cor(dt[!is.na(weight_recontact),weight], dt[!is.na(weight_recontact), weight_recontact])

# For weighted statistics
summary_stats_weighted <- generate_summary_stats(dat_final, 
                                                 vars_to_summarize = all_var_labels, 
                                                 weight_var = weight,
                                                 group_var = Topic,
                                                 placebo_var = Treatment,
                                                 include_factors = FALSE)

# For unweighted statistics
summary_stats_unweighted <- generate_summary_stats(dat_final, 
                                                   vars_to_summarize = all_var_labels, 
                                                   weight_var = NULL,
                                                   group_var = Topic,
                                                   placebo_var = Treatment,
                                                   include_factors = FALSE)

# For weighted statistics (factors)
summary_stats_weighted_factor <- generate_summary_stats(dat_final, 
                                                 vars_to_summarize = all_var_labels, 
                                                 weight_var = weight,
                                                 group_var = Topic,
                                                 placebo_var = Treatment,
                                                 include_factors = TRUE)

# For unweighted statistics (factors)
summary_stats_unweighted_factor <- generate_summary_stats(dat_final, 
                                                   vars_to_summarize = all_var_labels, 
                                                   weight_var = NULL,
                                                   group_var = Topic,
                                                   placebo_var = Treatment,
                                                   include_factors = TRUE)

# make table comparing pre and post, weighted and unweighted summary statistics
post_unweighted_factor <- generate_summary_stats(
  dat_final %>% dplyr::filter(!is.na(weight_recontact)),
  vars_to_summarize = all_var_labels,
  weight_var = NULL,
  group_var = Topic,
  placebo_var = Treatment,
  include_factors = TRUE
)

post_weighted_factor <- generate_summary_stats(
  dat_final %>% dplyr::filter(!is.na(weight_recontact)),
  vars_to_summarize = all_var_labels,
  weight_var = weight_recontact,
  group_var = Topic,
  placebo_var = Treatment,
  include_factors = TRUE
)



# Create separate tables for each group
groups <- unique(summary_stats_unweighted$Group)
for (group in groups) {
  group_stats <- summary_stats_unweighted[Group == group]
  latex_table <- create_latex_table(group_stats, 
                                    filename = file.path(output_dir, paste0("summary_statistics_", tolower(group), ".tex")), 
                                    title = paste("Summary Statistics -", stringr::str_replace_all(group, "_", " ")), 
                                    note = "")
}

groups <- unique(summary_stats_weighted$Group)
for (group in groups) {
  group_stats <- summary_stats_weighted[Group == group]
  latex_table <- create_latex_table(group_stats, 
                                    filename = file.path(output_dir, paste0("summary_statistics_", tolower(group), "_weighted", ".tex")), 
                                    title = paste("Summary Statistics -", stringr::str_replace_all(group, "_", " "), "(Weighted)"), 
                                    note = "")
}

# Factor variables
factor_var_labels <- all_var_labels[sapply(as.data.table(dat_final)[, ..all_var_labels], is.factor)]

# Generating factor summaries
for (var in factor_var_labels) {
  print(var)
  factor_summary_stats <- generate_factor_summary(dat_final, var, 
                                                  group_var = "Topic", 
                                                  placebo_var = "Treatment")
  latex_table <- create_latex_table(factor_summary_stats, 
                                    filename = file.path(output_dir, paste0("summary_statistics_", tolower(var), ".tex")), 
                                    title = paste("Summary Statistics -", stringr::str_replace_all(var, "_", " ")), 
                                    note = "")                                              
}

# make ind for post treatment missingness, regress vs party and treatment
dat_final[, followup_missing := is.na(weight_recontact)]
dropoff <- lm(followup_missing ~ Party_Identification + Treatment * AI_Type, data = dat_final)
summary(dropoff)
# FIXME: save this to output

create_ols_summary_table(
    models = list(dropoff),
    title = paste("OLS Regression Test for Differential Dropoff"),
    column.labels = "Missing in Follow-up",
  #   covariate.labels = c(treatment_label),
    out.file = file.path(output_dir, "dropoff.tex"),
    label = paste0("tab:dropoff")
  )


# Factor variables
is.character_or_factor <- function(x) {
  is.character(x) || is.factor(x)
}
factor_var_labels <- all_var_labels[sapply(as.data.table(dat_final)[, ..all_var_labels], is.character_or_factor)]

# Generating original and followup weighted factor summaries
for (var in factor_var_labels) {
  print(var)
  
# Generate summaries for different scenarios
pre_unweighted <- generate_factor_summary(dat_final, var, 
                                          placebo_var = "Treatment")
pre_unweighted[, Scenario := "Pre-treatment Unweighted"]

pre_weighted <- generate_factor_summary(dat_final, var, 
                                        placebo_var = "Treatment",
                                        weight_var = "weight")
pre_weighted[, Scenario := "Pre-treatment Weighted"]

post_unweighted <- generate_factor_summary(dat_final %>% dplyr::filter(!is.na(weight_recontact)), var, 
                                           placebo_var = "Treatment")
post_unweighted[, Scenario := "Follow-up Unweighted"]

post_weighted <- generate_factor_summary(dat_final %>% dplyr::filter(!is.na(weight_recontact)), var, 
                                         placebo_var = "Treatment",
                                         weight_var = "weight_recontact")
post_weighted[, Scenario := "Follow-up Weighted"]

# Combine all summaries
combined_summary <- rbindlist(list(pre_unweighted, pre_weighted, post_unweighted, post_weighted), use.names = TRUE, fill = TRUE)

setnames(combined_summary, "V1", "NA", skip_absent = TRUE)
# Create latex table
latex_table <- create_latex_table(combined_summary, 
                                  filename = file.path(output_dir, paste0("cross_wave_factor_summary_statistics_", tolower(var), ".tex")), 
                                  title = paste("Cross-Wave Summary Statistics -", stringr::str_replace_all(var, "_", " ")), 
                                  note = "")
                                            
}

### REGRESSIONS ###


### TESTING: MINI REGRESSIONS TO DOUBLE CHECK THINGS ####

tiny_lm_own <- lm(Confidence_Own_Ballot_Diff~Treatment * AI_Type, data = dat_final)
tiny_lm_county <- lm(Confidence_County_Ballots_Diff~Treatment * AI_Type, data = dat_final)
tiny_lm_country <- lm(Confidence_Country_Ballots_Diff~Treatment * AI_Type, data = dat_final)

create_ols_summary_table(
      models = list(tiny_lm_own, tiny_lm_county, tiny_lm_country),
      title = paste("OLS Regression Results for Smallest Pooled Model"),
      column.labels = paste0("Diff ", c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence")),
    #   covariate.labels = c(treatment_label),
      out.file = file.path(output_dir, "tiny_diff.tex"),
      label = paste0("tab:tiny")
    )

# recontact test
tiny_lm_own_recontact <- lm(Recontact_Confidence_Own_Ballot ~ Pre_Confidence_Own_Ballot + Treatment * AI_Type, data = dat_final)
tiny_lm_county_recontact <- lm(Recontact_Confidence_County_Ballots~ Pre_Confidence_County_Ballots + Treatment * AI_Type, data = dat_final)
tiny_lm_country_recontact <- lm(Recontact_Confidence_Country_Ballots~ Pre_Confidence_Country_Ballots + Treatment * AI_Type, data = dat_final)


create_ols_summary_table(
      models = list(tiny_lm_own_recontact, tiny_lm_county_recontact, tiny_lm_country_recontact),
      title = paste("OLS Regression Results for Smallest Pooled Model"),
      column.labels = paste0("Diff ", c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence")),
    #   covariate.labels = c(treatment_label, paste("Rumor: ", rumors)),
      out.file = file.path(output_dir, "mini_diff.tex"),
      label = paste0("tab:tiny_recontact")
    )

mini_lm_own <- lm(Confidence_Own_Ballot_Diff~Treatment * AI_Type + Topic, data = dat_final)
mini_lm_county <- lm(Confidence_County_Ballots_Diff~Treatment * AI_Type + Topic, data = dat_final)
mini_lm_country <- lm(Confidence_Country_Ballots_Diff~Treatment * AI_Type + Topic, data = dat_final)

mini_lm_own_recontact <- lm(Recontact_Confidence_Own_Ballot_Diff~Treatment * AI_Type + Topic, data = dat_final)
mini_lm_county_recontact <- lm(Recontact_Confidence_Own_Ballot_Diff~Treatment * AI_Type + Topic, data = dat_final)
mini_lm_country_recontact <- lm(Recontact_Confidence_Own_Ballot_Diff~Treatment * AI_Type + Topic, data = dat_final)


create_ols_summary_table(
      models = list(mini_lm_own, mini_lm_county, mini_lm_country),
      title = paste("OLS Regression Results for Second Smallest Pooled Model"),
      column.labels = paste0("Diff ", c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence")),
      covariate.labels = c(treatment_label, paste("Rumor: ", rumors)),
      out.file = file.path(output_dir, "mini_diff.tex"),
      label = paste0("tab:mini")
    )

create_ols_summary_table(
      models = list(mini_lm_own_recontact, mini_lm_county_recontact, mini_lm_country_recontact),
      title = paste("OLS Regression Results for Second Smallest Recontact Pooled Model"),
      column.labels = paste0("Diff ", c("Own Ballot Confidence (Recontact)", "County Ballots Confidence (Recontact)", "Country Ballots Confidence (Recontact)")),
      covariate.labels = c(treatment_label, paste("Rumor: ", rumors)),
      out.file = file.path(output_dir, "mini_diff_recontact.tex"),
      label = paste0("tab:mini_recontact")
    )


mini_lm_cisa <- lm(Rumor_Diff~Treatment * AI_Type + Topic, data = dat_final)
summary(mini_lm_cisa)

mini_lm_cisa_recontact <- lm(Rumor_Diff_Recontact~Treatment * AI_Type + Topic, data = dat_final)
summary(mini_lm_cisa_recontact)

mini_lm_cisa_other <- lm(Rumor_Other_Diff~Treatment * AI_Type + Topic, data = dat_final)
summary(mini_lm_cisa_other)

mini_lm_cisa_other_recontact <- lm(Rumor_Other_Diff_Recontact~Treatment * AI_Type + Topic, data = dat_final)
summary(mini_lm_cisa_other_recontact)

# Run Main Regressions

# Run models for each rumor
rumor_models <- lapply(rumors, run_rumor_models)
names(rumor_models) <- rumors

recontact_rumor_models <- lapply(rumors, run_rumor_models, var_labels = recontact_y_var_labels)
names(rumor_models) <- rumors

# cisa_rumor_models <- lapply(seq_along(rumors), run_rumor_models, var_labels = c("Rumor_Diff", "Rumor_Diff_Recontact"))
# names(cisa_rumor_models) <- rumors


# Create tables for individual rumor models
create_rumor_tables(rumor_models, rumors)
create_rumor_tables(recontact_rumor_models, rumors, table_label = "recontact")

# cisa_rumor_names <- paste0("CISA ", rumors)
# create_rumor_tables(cisa_rumor_models, cisa_rumor_names)

# Run pooled models
pooled_models <- lapply(post_y_vars_labels, function(outcome) {
  predictors <- c(paste0(sub("Post_", "Pre_", outcome)), "Treatment * AI_Type", "Topic", variables_in_model_labels)
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(pooled_models) <- paste0("Pooled_", post_y_vars_labels)


# Run recontact pooled models
pooled_followup_models <- lapply(recontact_y_var_labels, function(outcome) {
  predictors <- c(paste0(sub("Recontact_", "Pre_", outcome)), "Treatment * AI_Type", "Topic", variables_in_model_labels)
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(pooled_followup_models) <- paste0("Pooled_", recontact_y_var_labels)


# Create table for pooled models
key_covars <- c(
    pre_y_vars_labels %>% gsub("_", " ", .), 
    treatment_label, 
    ai_type_label,
    paste0("Rumor: ", rumors)
    )
covar_labels <- c(
    key_covars,
    covariate_labels)

pooled_table <- create_ols_summary_table(
  models = pooled_models,
  title = "OLS Regression Results for Pooled Models",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("pooled_models_table%s.tex", output_label)),
    label = "tab:pooled_models",
    longtable = FALSE
)
print("Table for pooled models has been created.")


pooled_followup_table <- create_ols_summary_table(
  models = pooled_followup_models,
  title = "OLS Regression Results for Followup Pooled Models",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("pooled_models_table%s.tex", output_label)),
    label = "tab:pooled_followup_models",
    longtable = FALSE
)
print("Table for pooled models has been created.")

# CISA TABLES 
cisa_rumor_models <- lapply(rumors, function(rumor){
  print(rumor)
  dat_rumor <- dat_final %>% dplyr::filter(Topic==rumor)
  svy_design_rumor <- svydesign(data = dat_rumor, weights = ~weight, id = ~1)
  predictors <- c("Treatment * AI_Type", variables_in_model_labels)
  run_regression_for_table("Rumor_Diff", predictors, svy_design_rumor, family = gaussian())
})
names(cisa_rumor_models) <- rumors

cisa_rumors_table <- create_ols_summary_table(
  models = cisa_rumor_models,
  title = "OLS Regression Results for Individual Rumor CISA Models",
  column.labels = rumors,
  # covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("cisa_rumor_models_table%s.tex", output_label)),
    label = "tab:cisa_rumors",
    longtable = FALSE
    
)
print("Table for cisa rumor models has been created.")

cisa_rumor_followup_models <- lapply(rumors, function(rumor){
  dat_rumor <- dat_final %>% dplyr::filter(Topic==rumor & !is.na(weight_recontact))
  svy_design_rumor <- svydesign(data = dat_rumor, weights = ~weight_recontact, id = ~1)
  predictors <- c("Treatment * AI_Type", variables_in_model_labels)
  run_regression_for_table("Rumor_Diff_Recontact", predictors, svy_design_rumor, family = gaussian())
})

names(cisa_rumor_followup_models) <- paste0("Recontact ", rumors)

cisa_rumors_followup_table <- create_ols_summary_table(
  models = cisa_rumor_followup_models,
  title = "OLS Regression Results for Individual CISA Recontact Models",
  column.labels = rumors,
  # covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("cisa_rumors_followup_models_table%s.tex", output_label)),
    label = "tab:cisa_rumors_followup",
    longtable = FALSE
    
)
print("Table for cisa rumors followup models has been created.")

# Run pooled models 
cisa_models <- lapply(recontact_cisa_diff_y_var_labels, function(outcome) {
  predictors <- c("Treatment * AI_Type", "Topic", variables_in_model_labels)
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(cisa_models) <- recontact_cisa_diff_y_var_labels

cisa_table <- create_ols_summary_table(
  models = cisa_models,
  title = "OLS Regression Results for CISA Models",
  column.labels = c("Post Rumor Diff", "Recontact Rumor Diff", "Post Other CISA Diff", "Recontact Other CISA Diff"),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("cisa_models_table%s.tex", output_label)),
    label = "tab:cisa",
    longtable = FALSE
    
)
print("Table for cisa models has been created.")


# Run pooled models with interactions with treatment
# Run interacted pooled models
int_pooled_models <- lapply(post_y_vars_labels, function(outcome) {
  predictors <- c(paste0(sub("Post_", "Pre_", outcome)), "Treatment", "Topic", 
  paste0("Treatment * AI_Type * ", int_var),
  variables_in_model_labels)
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(int_pooled_models) <- paste0("Pooled_", post_y_vars_labels)

# Create table for pooled models
int_key_covars <- c(
    pre_y_vars_labels %>% gsub("_", " ", .), 
    treatment_label, 
    # paste0("Rumor: ", rumors), "Rumor: Blue Shift", "Rumor: Voting Machines"
    rumors
    )
int_covar_labels <- c(
    int_key_covars,
    covariate_labels)

int_pooled_table <- create_ols_summary_table(
  models = int_pooled_models,
  title = "OLS Regression Results for Pooled Models",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
#   covariate.labels = int_covar_labels,
  out.file = file.path(output_dir, sprintf("int_pooled_models_table%s.tex", output_label)),
    label = "tab:int_pooled_models",
    longtable = FALSE
)
print("Table for interacted pooled models has been created.")


# Run recontact pooled models
recontact_pooled_models <- lapply(recontact_y_var_labels, function(outcome) {
  predictors <- c(paste0(sub("Recontact_", "Pre_", outcome)), "Treatment * AI_Type", variables_in_model_labels)
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(recontact_pooled_models) <- paste0("Recontact_", recontact_y_var_labels)

# Create table for pooled models
recontact_pooled_table <- create_ols_summary_table(
  models = recontact_pooled_models,
  title = "OLS Regression Results for Pooled Recontact Models",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("recontact_pooled_models_table%s.tex", output_label)),
    label = "tab:recontact_pooled_models",
    longtable = FALSE
    # omit = (length(key_covars)+1):length(covar_labels)
)
print("Table for pooled models has been created.")


# Run recontact interacted pooled models
int_recontact_pooled_models <- lapply(recontact_y_var_labels, function(outcome) {
  predictors <- c(paste0(sub("Recontact_", "Pre_", outcome)), 
    "Topic", 
    paste0("Treatment * AI_Type * ", int_var),
    variables_in_model_labels)
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(int_recontact_pooled_models) <- paste0("Recontact_", recontact_y_var_labels)

# Create table for recontact interacted pooled models
int_recontact_pooled_table <- create_ols_summary_table(
  models = int_recontact_pooled_models,
  title = "OLS Regression Results for Pooled Interacted Recontact Models",
  column.labels = c("Own Ballot Confidence", "County Ballots Confidence", "Country Ballots Confidence"),
#   covariate.labels = int_covar_labels,
  out.file = file.path(output_dir, sprintf("int_recontact_pooled_models_table%s.tex", output_label)),
    label = "tab:int_recontact_pooled_models",
    longtable = FALSE
    # omit = (length(key_covars)+1):length(covar_labels)
)
print("Table for interacted pooled models has been created.")


#### OTHER REGRESSION MODELS ####
# models included here:
# cisa models (relevant, other)

# Run CISA models

key_covars <- c(
  treatment_label, 
  paste0("Rumor: ", rumors)
  )

covar_labels <- c(
    key_covars, 
    covariate_labels
    )

cisa_models <- lapply(recontact_cisa_diff_y_var_labels, function(outcome) {
  predictors <- c(
    "Treatment * AI_Type", 
    "Topic", 
    variables_in_model_labels
    )
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(cisa_models) <- recontact_cisa_diff_y_var_labels

# Create table for CISA models
cisa_table <- create_ols_summary_table(
  models = cisa_models,
  title = "OLS Regression Results for CISA Models",
  column.labels = stringr::str_replace_all(recontact_cisa_diff_y_var_labels, "_", " "),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("cisa_models_table%s.tex", output_label)),
  label = "tab:cisa_models",
    longtable = FALSE
  # omit = (length(key_covars)+1):length(covar_labels)
)
print("Table for CISA models has been created.")



# Run CISA models with interactions
key_covars <- c(
  treatment_label, 
  paste0("Rumor: ", rumors)
  )

covar_labels <- c(
    key_covars, 
    covariate_labels
    )

int_cisa_models <- lapply(recontact_cisa_diff_y_var_labels, function(outcome) {
  predictors <- c(
    "Treatment", 
    "Topic", 
    # paste0("Treatment * ", "Ideology"),
    paste0("Treatment * AI_Type * Topic * ", int_var),
    variables_in_model_labels)
  run_regression_for_table(outcome, predictors, svy_design, family = gaussian())
})
names(int_cisa_models) <- recontact_cisa_diff_y_var_labels

# Create table for CISA models with interactions
int_cisa_table <- create_ols_summary_table(
  models = int_cisa_models,
  title = "OLS Regression Results for CISA Models",
  column.labels = stringr::str_replace_all(recontact_cisa_diff_y_var_labels, "_", " "),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("int_cisa_models_table%s.tex", output_label)),
  label = "tab:int_cisa_models",
    longtable = FALSE
  # omit = (length(key_covars)+1):length(covar_labels)
)
print("Table for interacted CISA models has been created.")


#### PLOT GROUP LEVEL TREATMENT EFFECTS ####

# For probability density plot
plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density_collapsed.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_collapsed.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density.pdf"), width = 12, height = 8)


plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  by_var = AI_Type,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density_party.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_aitype.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  by_var = Party_Identification,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density_party.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_party.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Confidence_Country_Ballots_Diff,
  by_var = Conspiracy_Bin,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and Conspiracy Beliefs",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "confidence_country_change_density_conspiracy_bin.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_conspiracy_bin.pdf"), width = 12, height = 8)

# Recontact Versions

# For probability density plot
plot_probability_density(
  data = dat_final,
  variable = Recontact_Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  title = "Distribution of Changes in Confidence in Country Ballots (Recontact)",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Recontact - Pre)",
  output_file = "confidence_country_change_density_collapsed_recontact.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_collapsed_recontact.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Recontact_Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Recontact - Pre)",
  output_file = "confidence_country_change_density_recontact.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_recontact.pdf"), width = 12, height = 8)


plot_probability_density(
  data = dat_final,
  variable = Recontact_Confidence_Country_Ballots_Diff,
  by_var = AI_Type,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and AI Type",
  x_label = "Change in Confidence (Recontact - Pre)",
  output_file = "confidence_country_change_density_aitype_recontact.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_aitype_recontact.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Recontact_Confidence_Country_Ballots_Diff,
  by_var = Party_Identification,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Recontact - Pre)",
  output_file = "confidence_country_change_density_party_recontact.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_party_recontact.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Recontact_Confidence_Country_Ballots_Diff,
  by_var = Conspiracy_Bin,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Confidence in Country Ballots",
  subtitle = "By Treatment Status and Conspiracy Beliefs",
  x_label = "Change in Confidence (Recontact - Pre)",
  output_file = "confidence_country_change_density_conspiracy_bin_recontact.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "confidence_country_change_density_conspiracy_bin_recontact.pdf"), width = 12, height = 8)


# Repeat for relevant CISA questions

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff,
  treatment_var = Treatment,
  # rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "cisa_change_density_collapsed.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "cisa_change_density_collapsed.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "cisa_change_density.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "cisa_change_density.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff,
  by_var = AI_Type,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and AI Type",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "cisa_change_density_ai_type.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "cisa_change_density_ai_type.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff,
  by_var = Party_Identification,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Post - Pre)",
  output_file = "cisa_change_density_party.pdf",
  weight_var = weight
)
ggsave(file.path(output_dir, "cisa_change_density_party.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  # rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Followup - Pre)",
  output_file = "cisa_change_density_collapsed_recontact.pdf",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "cisa_change_density_collapsed_recontact.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status",
  x_label = "Change in Confidence (Followup - Pre)",
  output_file = "cisa_change_density_recontact.pdf",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "cisa_change_density_recontact.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff_Recontact,
  by_var = AI_Type,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and AI Type",
  x_label = "Change in Confidence (Followup - Pre)",
  output_file = "cisa_change_density_ai_type_recontact.pdf",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "cisa_change_density_ai_type_recontact.pdf"), width = 12, height = 8)

plot_probability_density(
  data = dat_final,
  variable = Rumor_Diff_Recontact,
  by_var = Party_Identification,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Distribution of Changes in Belief in Specific Election Rumors",
  subtitle = "By Treatment Status and Party Identification",
  x_label = "Change in Confidence (Followup - Pre)",
  output_file = "cisa_change_density_party_recontact.pdf",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "cisa_change_density_party_recontact.pdf"), width = 12, height = 8)

#### PLOT PRE POST ####
pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Treatment,
  title = "Confidence in Assigned Election Rumor:  
Pre-Treatment vs. Post-Treatment",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence.pdf"), width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor:  
Pre-Treatment vs. Post-Treatment",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence_rumor.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Treatment,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor:  
Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence_party.pdf"), width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Post,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor:  
Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Post-Treatment Assigned Rumor Confidence Score",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence_party_rumor.pdf"), width = 12, height = 14)



pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Treatment,
  title = "Confidence in Assigned Election Rumor: 
  Pre-Treatment vs Followup",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence_recontact.pdf"), width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
  Pre-Treatment vs Followup",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence_rumor_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor: 
  Pre-Treatment vs Followup",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence_party_recontact.pdf"), width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Rumor,
  post_var = Rumor_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
  Pre-Treatment vs Followup",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Assigned Rumor Confidence Score",
  y_label = "Followup Rumor Confidence Score",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_cisa_confidence_party_rumor_recontact.pdf"), width = 12, height = 14)



pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  # rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs. Post-Treatment",
  # subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots.pdf"), width = 12, height = 14)

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_rumor.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  # rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_party.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_rumor_party.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  title = "Confidence in National Election: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in National Election: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_party_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_rumor_ai_type.pdf"), width = 12, height = 14)


##### AI RESULTS #####

# Own Ballot Confidence

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Post_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Post-Treatment Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Post_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Post-Treatment Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_party_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Post_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Post-Treatment Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_rumor_ai_type.pdf"), width = 12, height = 14)


## Own ballot AI RECONTACT

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Recontact_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Followup",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Followup Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Recontact_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Followup Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_party_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Recontact_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Followup Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_rumor_ai_type_recontact.pdf"), width = 12, height = 14)


# County Ballots Confidence AI TYPE

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Post_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  title = "Confidence in County Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Ballots",
  y_label = "Post-Treatment Confidence in County Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Post_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in County Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in County Ballots",
  y_label = "Post-Treatment Confidence in County Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_party_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Post_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in County Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in County Ballots",
  y_label = "Post-Treatment Confidence in County Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_rumor_ai_type.pdf"), width = 12, height = 14)


## County Ballots AI RECONTACT

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Recontact_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  title = "Confidence in County Ballots: 
  Pre-Treatment vs. Followup",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Ballots",
  y_label = "Followup Confidence in County Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Recontact_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in County Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in County Ballots",
  y_label = "Followup Confidence in County Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_party_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Recontact_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Followup Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_rumor_ai_type_recontact.pdf"), width = 12, height = 14)


# Country Ballots Confidence

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  title = "Confidence in Country Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Country Ballots",
  y_label = "Post-Treatment Confidence in Country Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in Country Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in Country Ballots",
  y_label = "Post-Treatment Confidence in Country Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_party_ai_type.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in Country Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in Country Ballots",
  y_label = "Post-Treatment Confidence in Country Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_rumor_ai_type.pdf"), width = 12, height = 14)


## Country Ballots AI RECONTACT

pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Recontact_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  title = "Confidence in Country Ballots: 
  Pre-Treatment vs. Followup",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Country Ballots",
  y_label = "Followup Confidence in Country Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Recontact_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in Country Ballots: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in Country Ballots",
  y_label = "Followup Confidence in Country Ballots",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_party_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Recontact_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in Own Ballot: 
  Pre-Treatment vs. Post-Treatment",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Followup Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_rumor_ai_type_recontact.pdf"), width = 12, height = 14)


### RUMOR AI CONFIDENCE ###

# Rumor 1

pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Post_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Post-Treatment Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_rumor1.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Post_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Post-Treatment Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_party_rumor1.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Post_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Post-Treatment Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_rumor1.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Post_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Post-Treatment Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_party_rumor1.pdf"), width = 12, height = 14)



pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_rumor1.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_party_rumor1.pdf"), width = 12, height = 14)




# recontact

pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Recontact_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Recontact Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_rumor1_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Recontact_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Recontact Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_party_rumor1_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Recontact_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Recontact Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_rumor1_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Recontact_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Recontact Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_party_rumor1_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Recontact_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Recontact Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_rumor1_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Recontact_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Followup\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Followup Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_rumor1_recontact.pdf"), width = 12, height = 14)


### REPEAT FOR RUMOR 2 #####


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Post_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Post-Treatment Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_rumor2.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Post_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Post-Treatment Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_party_rumor2.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Post_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Post-Treatment Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_rumor2.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Post_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Post-Treatment Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_party_rumor2.pdf"), width = 12, height = 14)



pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_rumor2.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Post-Treatment\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Post-Treatment Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_party_rumor2.pdf"), width = 12, height = 14)




# recontact

pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Recontact_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Recontact Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_rumor2_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Own_Ballot,
  post_var = Recontact_Confidence_Own_Ballot,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Own Ballot: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Own Ballot",
  y_label = "Recontact Confidence in Own Ballot",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_own_ballot_ai_type_party_rumor2_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Recontact_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Recontact Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_rumor2_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_County_Ballots,
  post_var = Recontact_Confidence_County_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in County Election: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in County Election",
  y_label = "Recontact Confidence in County Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_county_ballots_ai_type_party_rumor2_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Recontact_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Recontact\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Recontact Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_rumor2_recontact.pdf"), width = 12, height = 14)


pre_post_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Recontact_Confidence_Country_Ballots,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Followup\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Followup Confidence in National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_confidence_country_ballots_ai_type_rumor2_recontact.pdf"), width = 12, height = 14)



#### PLOT PRE POST DIFF PLOTS ####

pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  title = "Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Treatment Status",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_rumor.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  title = "Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_party.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_rumor_party.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_rumor_ai_type.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_party.pdf"), width = 12, height = 14)

# Repeat for Rumor 1 

pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_rumor1.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
title = sprintf("Confidence in National Election: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_party_rumor1.pdf"), width = 12, height = 14)

# Repeat for Rumor 2

pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_rumor2.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
title = sprintf("Confidence in National Election: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_party_rumor2.pdf"), width = 12, height = 14)

# Individual Rumor Confidence

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  title = "Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Treatment Status",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_rumor.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_party.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_rumor_party.pdf"), width = 12, height = 14)

# herehere


# Repeat for Rumor 1 

pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_rumor1.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_party_rumor1.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_rumor1_recontact.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_party_rumor1_recontact.pdf"), width = 12, height = 14)

# Repeat for Rumor 2

pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_rumor2.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Rumor,
  diff_var = Rumor_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Post-Treatment - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_party_rumor2.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_rumor2_recontact.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Assigned Election Rumor: 
Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_party_rumor2_recontact.pdf"), width = 12, height = 14)



# Recontact Versions

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  title = "Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Treatment Status",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_recontact.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_rumor_recontact.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Party Identification",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_party_recontact.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Party Identification and Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_rumor_party_recontact.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = AI_Type,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By AI and Rumor Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_rumor_ai_type_recontact.pdf"), width = 12, height = 14)


pre_post_diff_plot(
  data = dat_final,
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = "Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)",
  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_cisa_confidence_ai_type_party_recontact.pdf"), width = 12, height = 14)

# By Rumor
# Repeat for Rumor 1
pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)

ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_rumor1_recontact.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_party_rumor1_recontact.pdf"), width = 12, height = 14)

# Repeat for Rumor 2
pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)

ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_rumor2_recontact.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Rumor,
  diff_var = Rumor_Diff_Recontact,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
  title = sprintf("Confidence in Assigned Election Rumor: 
  Pre vs Change (Follow-up - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in Assigned Election Rumor",
  y_label = "Change in Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_cisa_ai_type_party_rumor2_recontact.pdf"), width = 12, height = 14)


# National Election Confidence by Rumor

# Repeat for Rumor 1 
# Recontact_Confidence_Own_Ballot_Diff

pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Recontact_Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Recontact Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_rumor1_recontact.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final[Topic==rumors[1]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Recontact_Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
title = sprintf("Confidence in National Election: 
Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[1]),  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Recontact Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_party_rumor1_recontact.pdf"), width = 12, height = 14)

# Repeat for Rumor 2

pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Recontact_Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = AI_Type,
  # rumor_var = Topic,
  title = sprintf("Confidence in National Election: 
  Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),
  subtitle = "By AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Recontact Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_rumor2_recontact.pdf"), width = 12, height = 14)

pre_post_diff_plot(
  data = dat_final[Topic==rumors[2]],
  pre_var = Pre_Confidence_Country_Ballots,
  diff_var = Recontact_Confidence_Country_Ballots_Diff,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = AI_Type,
title = sprintf("Confidence in National Election: 
Pre-Treatment vs Change (Recontact - Pre-Treatment)\n (Only Rumor: %s)", rumors[2]),  subtitle = "By Party Identification and AI Type",
  x_label = "Pre-Treatment Confidence in National Election",
  y_label = "Recontact Change in Confidence National Election",
  weight_var = weight
)
ggsave(file.path(output_dir, "pre_post_diff_confidence_country_ballots_ai_type_party_rumor2_recontact.pdf"), width = 12, height = 14)




#### TIME SERIES PLOTS ####


create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_country.pdf"), width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  rumor_var = Topic,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_country_rumor.pdf"), width = 12, height = 8, units = "in")


create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  by_var = Party_Identification,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_country_party.pdf"), width = 12, height = 8, units = "in")


create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  by_var = Party_Identification,
  rumor_var = Topic,
  pre_var = Pre_Confidence_Country_Ballots,
  post_var = Post_Confidence_Country_Ballots,
  recontact_var = Recontact_Confidence_Country_Ballots,
  title = "Confidence in National Election: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in National Election",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_country_rumor_party.pdf"), width = 12, height = 8, units = "in")



create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_cisa.pdf"), width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status and Rumor",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_cisa_rumor.pdf"), width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  by_var = Party_Identification,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status and Party Identification",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_cisa_party.pdf"), width = 12, height = 8, units = "in")

create_time_series_plot(
  data = dat_final,
  treatment_var = Treatment,
  pre_var = Rumor,
  post_var = Rumor_Post,
  recontact_var = Rumor_Recontact,
  by_var = Party_Identification,
  rumor_var = Topic,
  title = "Confidence in Assigned Election Rumor: Pre-Treatment, Post-Treatment, and Follow-up",
  subtitle = "By Treatment Status, Rumor, and Party Identification",
  ylab = "Confidence in Assigned Election Rumor",
  weight_var = weight_recontact
)
ggsave(file.path(output_dir, "time_series_plot_cisa_rumor_party.pdf"), width = 12, height = 8, units = "in")

#### PLOTS MARGINAL EFFECTS #### 
# allEffects(cisa_models[[1]]) %>% plot()


#### RANDOM ASSORTED STATISTICS IN PAPER ####
t.test(
  dt[Treatment=="Placebo",Post_Confidence_Country_Ballots],
  dt[Treatment=="Placebo",Pre_Confidence_Country_Ballots], 
  na.rm=TRUE, paired=TRUE)

# calculate cohen's d for pre/post confidence in country ballots (placebo only)
cohen.d(
  dt[Treatment=="Placebo",Post_Confidence_Country_Ballots],
  dt[Treatment=="Placebo",Pre_Confidence_Country_Ballots],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dt[Treatment=="Treatment",Post_Confidence_Country_Ballots],
  dt[Treatment=="Treatment",Pre_Confidence_Country_Ballots],
  na.rm=TRUE, paired=TRUE
)

party <- "Republican"

t.test(
  dt[Treatment=="Placebo" & Party_Identification==party,Post_Confidence_Country_Ballots],
  dt[Treatment=="Placebo" & Party_Identification==party,Pre_Confidence_Country_Ballots], 
  na.rm=TRUE, paired=TRUE)

t.test(
  dt[Treatment=="Treatment" & Party_Identification==party,Post_Confidence_Country_Ballots],
  dt[Treatment=="Treatment" & Party_Identification==party,Pre_Confidence_Country_Ballots], 
  na.rm=TRUE, paired=TRUE)

cohen.d(
  dt[Treatment=="Placebo" & Party_Identification==party,Post_Confidence_Country_Ballots],
  dt[Treatment=="Placebo" & Party_Identification==party,Pre_Confidence_Country_Ballots],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dt[Treatment=="Treatment" & Party_Identification==party,Post_Confidence_Country_Ballots],
  dt[Treatment=="Treatment" & Party_Identification==party,Pre_Confidence_Country_Ballots],
  na.rm=TRUE, paired=TRUE
)

# CISA questions
t.test(
  dt[Treatment=="Placebo",Rumor_Recontact],
  dt[Treatment=="Placebo",Rumor], 
  na.rm=TRUE, paired=TRUE)

# calculate cohen's d for pre/post confidence in relevant cisa questions (placebo only)
cohen.d(
  dt[Treatment=="Placebo",Rumor_Recontact],
  dt[Treatment=="Placebo",Rumor],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dt[Treatment=="Treatment",Rumor_Recontact],
  dt[Treatment=="Treatment",Rumor],
  na.rm=TRUE, paired=TRUE
)

party <- "Democrat"

t.test(
  dt[Treatment=="Placebo" & Party_Identification==party,Rumor_Recontact],
  dt[Treatment=="Placebo" & Party_Identification==party,Rumor], 
  na.rm=TRUE, paired=TRUE)

t.test(
  dt[Treatment=="Treatment" & Party_Identification==party,Rumor_Recontact],
  dt[Treatment=="Treatment" & Party_Identification==party,Rumor], 
  na.rm=TRUE, paired=TRUE)

cohen.d(
  dt[Treatment=="Placebo" & Party_Identification==party,Rumor_Recontact],
  dt[Treatment=="Placebo" & Party_Identification==party,Rumor],
  na.rm=TRUE, paired=TRUE
)
cohen.d(
  dt[Treatment=="Treatment" & Party_Identification==party,Rumor_Recontact],
  dt[Treatment=="Treatment" & Party_Identification==party,Rumor],
  na.rm=TRUE, paired=TRUE
)


# Crude Proportion Confident Plots and Tables
# Function to calculate CI
calculate_ci <- function(p, n) {
  se <- sqrt(p * (1 - p) / n)
  margin <- 1.96 * se
  list(lower = pmax(0, p - margin), upper = pmin(1, p + margin))
}

make_props_data <- function(dat_final, weighted=FALSE){
  dt <- as.data.table(dat_final)

  if (!weighted){

  props_all <- dt[,.(
    Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
    Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
    Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
    Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
    Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
    Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
  ), by = .(Treatment)][
    order(Treatment)]
  props_all[, Party_Identification := "Any"]

  props <- dt[,.(
    Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
    Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
    Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
    Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
    Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
    Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
  ), by = .(Treatment, Party_Identification)]

  props_full <- rbindlist(list(props, props_all), use.names = TRUE)

  props_full[,`:=`(
    Election_Confidence_Difference = Pre_Election_Confidence-Post_Election_Confidence,
    Rumor_Difference = Pre_Rumor_Confidence-Post_Rumor_Confidence
    )]
  props_full <- props_full[order(as.character(Party_Identification), Treatment)]

  } else {

  props_all <- dt[,.(
    Pre_Election_Confidence = weighted.mean(Pre_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
    Post_Election_Confidence = weighted.mean(Post_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
    Recontact_Election_Confidence = weighted.mean(Recontact_Confidence_Country_Ballots>5, weight_recontact, na.rm = TRUE),
    Pre_Rumor_Confidence = weighted.mean(Rumor>5, weight, na.rm = TRUE),
    Post_Rumor_Confidence = weighted.mean(Rumor_Post>5, weight, na.rm = TRUE),
    Recontact_Rumor_Confidence = weighted.mean(Rumor_Recontact>5, weight_recontact, na.rm = TRUE)
  ), by = .(Treatment)][
    order(Treatment)]
  props_all[, Party_Identification := "Any"]

  props <- dt[,.(
    Pre_Election_Confidence = weighted.mean(Pre_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
    Post_Election_Confidence = weighted.mean(Post_Confidence_Country_Ballots>5, weight, na.rm = TRUE),
    Recontact_Election_Confidence = weighted.mean(Recontact_Confidence_Country_Ballots>5, weight_recontact, na.rm = TRUE),
    Pre_Rumor_Confidence = weighted.mean(Rumor>5, weight, na.rm = TRUE),
    Post_Rumor_Confidence = weighted.mean(Rumor_Post>5, weight, na.rm = TRUE),
    Recontact_Rumor_Confidence = weighted.mean(Rumor_Recontact>5, weight_recontact, na.rm = TRUE)
  ), by = .(Treatment, Party_Identification)]

  props_full <- rbindlist(list(props, props_all), use.names = TRUE)

  props_full[,`:=`(
    Election_Confidence_Difference = Pre_Election_Confidence-Post_Election_Confidence,
    Rumor_Difference = Pre_Rumor_Confidence-Post_Rumor_Confidence
    )]
  props_full <- props_full[order(as.character(Party_Identification), Treatment)]
  }


  # Calculate sample sizes
  sample_sizes <- dt[, .(
    n_election = sum(!is.na(Pre_Confidence_Country_Ballots)),
    n_rumor = sum(!is.na(Rumor))
  ), by = .(Treatment, Party_Identification)]

  sample_sizes_all <- dt[, .(
    n_election = sum(!is.na(Pre_Confidence_Country_Ballots)),
    n_rumor = sum(!is.na(Rumor))
  ), by = .(Treatment)]
  sample_sizes_all[, Party_Identification := "Any"]

  sample_sizes <- rbindlist(list(sample_sizes, sample_sizes_all), use.names = TRUE)

  # Merge sample sizes with props_full
  props_full <- merge(props_full, sample_sizes, by = c("Treatment", "Party_Identification"))

  # Calculate CIs
  ci_columns <- c("Pre_Election_Confidence", "Post_Election_Confidence", "Recontact_Election_Confidence",
                  "Pre_Rumor_Confidence", "Post_Rumor_Confidence", "Recontact_Rumor_Confidence")

  for (col in ci_columns) {
    ci_lower <- paste0(col, "_CI_Lower")
    ci_upper <- paste0(col, "_CI_Upper")
    n_col <- ifelse(grepl("Election", col), "n_election", "n_rumor")
    
    props_full[, c(ci_lower, ci_upper) := calculate_ci(get(col), get(n_col))]
  }

  # order party, with "Any" first
  props_full[, Party_Identification := fct_relevel(Party_Identification, "Any")]

  return(props_full)
}

plot_props <- function(props_full, title_str="Confidence in Election Integrity and Election Rumors", output_file=""){

  # Reshape the data
  plot_data <- props_full %>%
    select(Treatment, Party_Identification, 
          matches("(Pre|Post|Recontact)_(Election|Rumor)_Confidence($|_CI)")) %>%
    pivot_longer(
      cols = -c(Treatment, Party_Identification),
      names_to = c("time", "measure", "stat_type"),
      names_pattern = "(Pre|Post|Recontact)_(Election|Rumor)_(Confidence(?:_CI_(?:Lower|Upper))?)"
    ) %>%
    mutate(
      time = factor(time, levels = c("Pre", "Post", "Recontact")),
      measure = factor(measure, levels = c("Election", "Rumor"))
    ) %>%
    pivot_wider(
      names_from = stat_type,
      values_from = value
    ) %>%
    rename(CI_Lower = Confidence_CI_Lower,
          CI_Upper = Confidence_CI_Upper)

  # Create the plot
  gg <- ggplot(plot_data, aes(x = time, y = Confidence, color = Treatment, 
                        group = Treatment)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
    geom_label_repel(aes(label = sprintf("%.1f%%", Confidence * 100), 
                y = Confidence,
                # box.padding = unit(0.35, "lines"),
                # point.padding = unit(0.5, "lines"),
                color = Treatment),
            size = 3, fontface = "bold") +
    facet_grid(measure ~ Party_Identification) +
    labs(
      title = title_str,
      subtitle = "By Party Identification Over Time",
      x = "Time",
      y = "Proportion with Confidence > 5 (out of 10)",
      color = "Treatment"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    # scale_color_manual(values = c("Placebo" = "#3B9AB2", "Treatment" = "#E1AF00")) +
    scale_color_manual(values = wesanderson::wes_palette("Royal1", 2)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 18),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      strip.text = element_text(size = 14, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(1, "lines"),
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45)
    )
    if (output_file != ""){
      print(gg)
      ggsave(output_file, width = 12, height = 8, dpi = 300)
    } else {
      print(gg)
    }
}

props_full <- make_props_data(dat_final, weighted=FALSE)
props_full_wtd <- make_props_data(dat_final, weighted=TRUE)

plot_props(props_full, title_str="Confidence in Election Integrity and Election Rumors", output_file=file.path(output_dir, "crude_confidence_props.pdf"))
plot_props(props_full_wtd, title_str="Confidence in Election Integrity and Election Rumors, Weighted by Sampling Weights", output_file=file.path(output_dir, "crude_confidence_props_wtd.pdf"))

props_rumor1 <- make_props_data(dat_final[Topic==rumors[1]], weighted=FALSE)
props_rumor2 <- make_props_data(dat_final[Topic==rumors[2]], weighted=FALSE)
plot_props(props_rumor1, title_str=sprintf("Confidence in Election Integrity and Election Rumors \n(Only Rumor: %s)", rumors[1]), output_file=file.path(output_dir, "crude_confidence_props_rumor1.pdf"))
plot_props(props_rumor2, title_str=sprintf("Confidence in Election Integrity and Election Rumors \n(Only Rumor: %s)", rumors[2]), output_file=file.path(output_dir, "crude_confidence_props_rumor2.pdf"))

# Reshape the data
plot_data <- props_full %>%
  select(Treatment, Party_Identification, 
         matches("(Pre|Post|Recontact)_(Election|Rumor)_Confidence($|_CI)")) %>%
  pivot_longer(
    cols = -c(Treatment, Party_Identification),
    names_to = c("time", "measure", "stat_type"),
    names_pattern = "(Pre|Post|Recontact)_(Election|Rumor)_(Confidence(?:_CI_(?:Lower|Upper))?)"
  ) %>%
  mutate(
    time = factor(time, levels = c("Pre", "Post", "Recontact")),
    measure = factor(measure, levels = c("Election", "Rumor"))
  ) %>%
  pivot_wider(
    names_from = stat_type,
    values_from = value
  ) %>%
  rename(CI_Lower = Confidence_CI_Lower,
         CI_Upper = Confidence_CI_Upper)

# Create the plot
ggplot(plot_data, aes(x = time, y = Confidence, color = Treatment, 
                      group = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_label_repel(aes(label = sprintf("%.1f%%", Confidence * 100), 
              y = Confidence,
              # box.padding = unit(0.35, "lines"),
              # point.padding = unit(0.5, "lines"),
              color = Treatment),
          size = 3, fontface = "bold") +
  facet_grid(measure ~ Party_Identification) +
  labs(
    title = "Confidence in Election Integrity and Election Rumors",
    subtitle = "By Party Identification Over Time",
    x = "Time",
    y = "Proportion with Confidence > 5 (out of 10)",
    color = "Treatment"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  # scale_color_manual(values = c("Placebo" = "#3B9AB2", "Treatment" = "#E1AF00")) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1", 2)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45)
  )
ggsave(file.path(output_dir, "crude_confidence_props.pdf"), width = 12, height = 8, dpi = 300)

# Reshape the data
plot_data_wtd <- props_full_wtd %>%
  select(Treatment, Party_Identification, 
         matches("(Pre|Post|Recontact)_(Election|Rumor)_Confidence($|_CI)")) %>%
  pivot_longer(
    cols = -c(Treatment, Party_Identification),
    names_to = c("time", "measure", "stat_type"),
    names_pattern = "(Pre|Post|Recontact)_(Election|Rumor)_(Confidence(?:_CI_(?:Lower|Upper))?)"
  ) %>%
  mutate(
    time = factor(time, levels = c("Pre", "Post", "Recontact")),
    measure = factor(measure, levels = c("Election", "Rumor"))
  ) %>%
  pivot_wider(
    names_from = stat_type,
    values_from = value
  ) %>%
  rename(CI_Lower = Confidence_CI_Lower,
         CI_Upper = Confidence_CI_Upper)

# Create the plot
ggplot(plot_data_wtd, aes(x = time, y = Confidence, color = Treatment, 
                      group = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  geom_label_repel(aes(label = sprintf("%.1f%%", Confidence * 100), 
              y = Confidence,
              # box.padding = unit(0.35, "lines"),
              # point.padding = unit(0.5, "lines"),
              color = Treatment),
          size = 3, fontface = "bold") +
  facet_grid(measure ~ Party_Identification) +
  labs(
    title = "Confidence in Election Integrity and Election Rumors",
    subtitle = "By Party Identification Over Time, Weighted by Sampling Weights",
    x = "Time",
    y = "Proportion with Confidence > 5 (out of 10)",
    color = "Treatment"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  # scale_color_manual(values = c("Placebo" = "#3B9AB2", "Treatment" = "#E1AF00")) +
  scale_color_manual(values = wesanderson::wes_palette("Royal1", 2)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 18),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45)
  )

ggsave(file.path(output_dir, "confidence_intervals_plot_wtd.png"), width = 12, height = 8, dpi = 300)

# repeat by each rumor
props_all_rumor <- dt[,.(
  Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
  Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
  Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
  Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
  Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
  Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
), by = .(Treatment, Topic)][
  order(Treatment, Topic)]
props_all_rumor[, Party_Identification := "Any"]

props_rumor <- dt[,.(
  Pre_Election_Confidence = mean(Pre_Confidence_Country_Ballots>5, na.rm = TRUE),
  Post_Election_Confidence = mean(Post_Confidence_Country_Ballots>5, na.rm = TRUE),
  Recontact_Election_Confidence = mean(Recontact_Confidence_Country_Ballots>5, na.rm = TRUE),
  Pre_Rumor_Confidence = mean(Rumor>5, na.rm = TRUE),
  Post_Rumor_Confidence = mean(Rumor_Post>5, na.rm = TRUE),
  Recontact_Rumor_Confidence = mean(Rumor_Recontact>5, na.rm = TRUE)
), by = .(Topic, Treatment, Party_Identification)]

props_full_rumor <- rbindlist(list(props_rumor, props_all_rumor), use.names = TRUE)

props_full_rumor[,`:=`(
  Election_Confidence_Difference = Pre_Election_Confidence-Post_Election_Confidence,
  Rumor_Difference = Pre_Rumor_Confidence-Post_Rumor_Confidence
  )]
props_full_rumor <- props_full_rumor[order(as.character(Party_Identification), Treatment, Topic)]

# Create the election confidence table
election_table <- props_full[Party_Identification == "Any", .(
  Treatment = Treatment,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)]

# Create the rumor confidence table
rumor_table <- props_full[Party_Identification == "Any", .(
  Treatment = Treatment,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)]

rumor_table_party <- props_full[Party_Identification != "Any", .(
  Treatment = Treatment,
  Party = Party_Identification,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)]

election_table_party <- props_full[Party_Identification != "Any", .(
  Treatment = Treatment,
  Party = Party_Identification,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)]

# Create the election confidence table
election_table_rumor <- props_full_rumor[Party_Identification == "Any", .(
  Rumor = Topic,
  Treatment = Treatment,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)][order(Rumor, Treatment)]

# Create the rumor confidence table
rumor_table_rumor <- props_full_rumor[Party_Identification == "Any", .(
  Rumor = Topic,
  Treatment = Treatment,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)][order(Rumor, Treatment)]

election_table_rumor_party <- props_full_rumor[Party_Identification != "Any", .(
  Rumor = Topic,
  Treatment = Treatment,
  Party = Party_Identification,
  `Pre-Election` = Pre_Election_Confidence,
  `Post-Election` = Post_Election_Confidence,
  `Recontact-Election` = Recontact_Election_Confidence
)][order(Rumor, Party, Treatment)]

rumor_table_rumor_party <- props_full_rumor[Party_Identification != "Any", .(
  Rumor = Topic,
  Treatment = Treatment,
  Party = Party_Identification,
  `Pre-Rumor` = Pre_Rumor_Confidence,
  `Post-Rumor` = Post_Rumor_Confidence,
  `Recontact-Rumor` = Recontact_Rumor_Confidence
)][order(Rumor, Party, Treatment)]

# Function to format percentages
format_pct <- function(x) sprintf("%.1f%%", x * 100)

# Apply formatting to all tables
election_table[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]
election_table_rumor[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table_rumor[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]
election_table_party[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table_party[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]
election_table_rumor_party[, c("Pre-Election", "Post-Election", "Recontact-Election") := lapply(.SD, format_pct), .SDcols = c("Pre-Election", "Post-Election", "Recontact-Election")]
rumor_table_rumor_party[, c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor") := lapply(.SD, format_pct), .SDcols = c("Pre-Rumor", "Post-Rumor", "Recontact-Rumor")]

# Create LaTeX tables
election_latex <- xtable(election_table,
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact",
                         size = "small",
                         label = "tab:prop_election")

rumor_latex <- xtable(rumor_table, 
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact",
                      size = "small",
                      label = "tab:prop_rumor")

election_rumor_latex <- xtable(election_table_rumor, 
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor",
                         size = "small",
                         label = "tab:prop_election_rumor")

rumor_indiv_rumor_latex <- xtable(rumor_table_rumor, 
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor",
                      size = "small",
                      label = "tab:prop_indiv_rumor")

election_party_latex <- xtable(election_table_party, 
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact, by party identification",
                         size = "small",
                         label = "tab:prop_election_party")

rumor_party_latex <- xtable(rumor_table_party,
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact, by party identification",
                      size = "small",
                      label = "tab:prop_rumor_party")

election_rumor_party_latex <- xtable(election_table_rumor_party, 
                         caption = "Proportion of participants confident in the election integrity (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor and party identification",
                         size = "small",
                         label = "tab:prop_election_indiv_rumor_party")

rumor_indiv_rumor_party_latex <- xtable(rumor_table_rumor_party, 
                      caption = "Proportion of participants confident election rumors is true (score $>$ 5 out of 10) before, immediately after, and at recontact, by assigned rumor and party identification",
                      size = "small",
                      label = "tab:prop_rumor_indiv_rumor_party")

                      

# Print LaTeX code
print(election_latex, file=file.path(output_dir, 'election_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_latex, file=file.path(output_dir, 'rumor_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")
print(election_rumor_latex, file=file.path(output_dir, 'election_rumor_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_indiv_rumor_latex, file=file.path(output_dir, 'rumor_indiv_rumor_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")
print(election_party_latex, file=file.path(output_dir, 'election_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_party_latex, file=file.path(output_dir, 'rumor_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")
print(election_rumor_party_latex, file=file.path(output_dir, 'election_rumor_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = TRUE, size="\\small")
print(rumor_indiv_rumor_party_latex, file=file.path(output_dir, 'rumor_indiv_rumor_party_prop.tex'), compress=FALSE, include.rownames = FALSE, floating = TRUE, table.placement = "h", caption.placement = "top", escape = FALSE, size="\\small")

# Combine all models
# names(pooled_models) <- c("Own Ballot", "County Ballots", "Country Ballots")
# names(pooled_followup_models) <- c("Own Ballot (Followup)", "County Ballots (Followup)", "Country Ballots (Followup)")
# names(cisa_models) <- c("Confidence", "Confidence (Followup)", 
#                         "Confidence (Followup)", "Confidence (Followup)")
# names(cisa_rumor_models) <- rumors
# names(cisa_rumor_followup_models) <- rumors
# herehere: fix labels
all_models <- list(
  # "Election Confidence" = pooled_models,
  "Country Ballots" = list(
    "Confidence" = pooled_models[[3]],
    "Confidence (Recontact)" = pooled_followup_models[[3]]
  ),
  "County Ballots" = list(
    "Confidence" = pooled_models[[2]],
    "Confidence (Recontact)" = pooled_followup_models[[2]]
  ),
  "Own Ballot" = list(
    "Confidence" = pooled_models[[1]],
    "Confidence (Recontact)" = pooled_followup_models[[1]]
  ),
  "Assigned Rumor (Pooled)" = cisa_models[1:2],
  "Other Rumors (Pooled)" = cisa_models[3:4],
  # "Election Facts (Recontact)" = cisa_models[4],
  "Voter Rolls" = list(
    "Confidence" = cisa_rumor_models[[1]],
    "Confidence (Recontact)" = cisa_rumor_followup_models[[1]]
  ),
  "Non-Citizen Voting" = list(
    "Confidence" = cisa_rumor_models[[2]],
    "Confidence (Recontact)" = cisa_rumor_followup_models[[2]]
  )
)

cisa_plot <- plot_coefficients(
  all_models,
  title = "Treatment Effect Estimates for All OLS Models",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Model",
  text_size = 18
)

print(cisa_plot)

ggsave(file.path(output_dir, "treatment_effects_plot_all.pdf"), cisa_plot, width = 15, height = 12, units = "in")



individual_rumors <- list(
  "Voter Rolls" = list(
    "Confidence" = cisa_rumor_models[["Voter Rolls"]],
    "Confidence (Recontact)" = cisa_rumor_followup_models[["Recontact Voter Rolls"]]
  ),
  "Non-Citizen Voting" = list(
    "Confidence" = cisa_rumor_models[["Non-Citizen Voting"]],
    "Confidence (Recontact)" = cisa_rumor_followup_models[["Recontact Non-Citizen Voting"]]
  )
)

individual_rumors_plot <- plot_coefficients(
  individual_rumors,
  title = "Treatment Effect Estimates
    Confidence that: Election Rumor is True",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Rumor Model",
  text_size = 18,
  debug = TRUE
)

print(individual_rumors_plot)
ggsave(file.path(output_dir, "treatment_effects_plot_rumors.pdf"), individual_rumors_plot, width = 15, height = 12, units = "in")

main_models <- list(
  "Assigned Rumor (Pooled)" = list(
    "Confidence" = cisa_models[1],
    "Confidence (Recontact)" = cisa_models[2]
  ),
  "Other Rumor (Pooled)" = list(
    "Confidence" = cisa_models[3],
    "Confidence (Recontact)" = cisa_models[4]
  ),
  "Country Ballots" = list(
    "Confidence" = pooled_models[[3]],
    "Confidence (Recontact)" = pooled_followup_models[[3]]
  )
)


main_models_plot <- plot_coefficients(
  main_models,
  title = "Treatment Effect Estimates
  Confidence that: Election Rumors are True, Ballots are Accurately Counted Nationally",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Model",
  text_size = 18,
  debug = TRUE
)

print(main_models_plot)
ggsave(file.path(output_dir, "treatment_effects_plot_main.pdf"), main_models_plot, width = 15, height = 12, units = "in")


election_models <- list(
  "Country Ballots" = list(
    "Confidence" = pooled_models[[3]],
    "Confidence (Recontact)" = pooled_followup_models[[3]]
  ),
  "County Ballots" = list(
    "Confidence" = pooled_models[[2]],
    "Confidence (Recontact)" = pooled_followup_models[[2]]
  ),
  "Own Ballot" = list(
    "Confidence" = pooled_models[[1]],
    "Confidence (Recontact)" = pooled_followup_models[[1]]
  )
)


election_models_plot <- plot_coefficients(
  election_models,
  title = "Treatment Effect Estimates
    Confidence that: Ballots are Accurately Counted",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Model",
  text_size = 18,
  debug = TRUE
)

print(election_models_plot)
ggsave(file.path(output_dir, "treatment_effects_plot_election.pdf"), election_models_plot, width = 15, height = 12, units = "in")

## Repeat for interacted models with AI Type

individual_rumors_plot_ai <- plot_coefficients(
  individual_rumors,
  var_name = "TreatmentTreatment:AI_TypeAI Chatbot",
  title = "Treatment * Chatbot Effect Estimates
    Confidence that: Election Rumor is True",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Rumor Model",
  text_size = 18,
  debug = TRUE
)

print(individual_rumors_plot_ai)
ggsave(file.path(output_dir, "treatment_effects_plot_rumors_ai.pdf"), individual_rumors_plot_ai, width = 15, height = 12, units = "in")


main_models_plot_ai <- plot_coefficients(
  main_models,
  var_name = "TreatmentTreatment:AI_TypeAI Chatbot",
  title = "Treatment * Chatbot Effect Estimates
  Confidence that: Election Rumors are True, Ballots are Accurately Counted Nationally",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Model",
  text_size = 18,
  debug = TRUE
)

print(main_models_plot_ai)
ggsave(file.path(output_dir, "treatment_effects_plot_main_ai.pdf"), main_models_plot_ai, width = 15, height = 12, units = "in")


election_models_plot_ai <- plot_coefficients(
  election_models,
  var_name = "TreatmentTreatment:AI_TypeAI Chatbot",
  title = "Treatment * Chatbot Effect Estimates
    Confidence that: Ballots are Accurately Counted",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Model",
  text_size = 18,
  debug = TRUE
)

print(election_models_plot_ai)
ggsave(file.path(output_dir, "treatment_effects_plot_election_ai.pdf"), election_models_plot_ai, width = 15, height = 12, units = "in")

# Political party * period
# herehere

int_election_models <- list(
  "Country Ballots" = list(
    "Confidence" = int_pooled_models[[3]],
    "Confidence (Recontact)" = int_recontact_pooled_models[[3]]
  ),
  "County Ballots" = list(
    "Confidence" = int_pooled_models[[2]],
    "Confidence (Recontact)" = int_recontact_pooled_models[[2]]
  ),
  "Own Ballot" = list(
    "Confidence" = int_pooled_models[[1]],
    "Confidence (Recontact)" = int_recontact_pooled_models[[1]]
  )
)


int_election_models_plot <- plot_coefficients(
  int_election_models,
  var_name = c("TreatmentTreatment", "Party_Identification"),
  title = "Treatment Effect Estimates
    Confidence that: Ballots are Accurately Counted",
  subtitle = "Measured Immediately Post-Treatment and After One Week (Post-Election)",
  y_label = "Model",
  text_size = 18,
  debug = TRUE
)

print(int_election_models_plot)
ggsave(file.path(output_dir, "treatment_effects_plot_election.pdf"), election_models_plot, width = 15, height = 12, units = "in")

## Models to Add ##
# Presidential 2024 vote
# presvote24post_recontact

predictors <- c("Treatment * AI_Type", "Topic", variables_in_model_labels)
pooled_pres_vote_model <- run_regression_for_table("Vote_Harris_2024", predictors, svy_design, family = gaussian())
predictors_no_topic <- predictors[predictors %!in% "Topic"]

# ANES
# anes_labels <- c("ANES 1", "ANES 2")
anes_labels <- c("Officials Don't Care", "No Say in Gov't")

# ANES by rumor
rumor_1_design <- svydesign(data = dat_final[Topic==rumors[1]], weights = ~weight, id = ~1)
rumor_2_design <- svydesign(data = dat_final[Topic==rumors[2]], weights = ~weight, id = ~1)

rumor_1_anes_1_model <- run_regression_for_table("ANES_Representation_Diff_1", predictors_no_topic, rumor_1_design, family = gaussian())
rumor_1_anes_2_model <- run_regression_for_table("ANES_Representation_Diff_2", predictors_no_topic, rumor_1_design, family = gaussian())

rumor_2_anes_1_model <- run_regression_for_table("ANES_Representation_Diff_1", predictors_no_topic, rumor_2_design, family = gaussian())
rumor_2_anes_2_model <- run_regression_for_table("ANES_Representation_Diff_2", predictors_no_topic, rumor_2_design, family = gaussian())


rumor_anes_table <- create_ols_summary_table(
  models = list(rumor_1_anes_1_model, rumor_1_anes_2_model, rumor_2_anes_1_model, rumor_2_anes_2_model),
  title = "OLS Regression Results for Individual Rumor ANES Representation Models",
  column.labels = paste0("Rumor: ", rep(rumors, each = 2), anes_labels),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("rumor_anes_models_table%s.tex", output_label)),
    label = "tab:rumor_anes_models",
    longtable = FALSE
)
print("Table for rumor-level ANES models has been created.")

# ANES interacted
predictors <- c(paste0("Treatment * AI_Type * ", int_var), "Topic", variables_in_model_labels)

predictors_no_topic <- predictors[predictors %!in% "Topic"]
predictors_no_int <- predictors_no_topic[predictors_no_topic %!in% int_var]

rumor_1_anes_1_int_model <- run_regression_for_table("ANES_Representation_Diff_1", predictors_no_int, rumor_1_design, family = gaussian())
rumor_1_anes_2_int_model <- run_regression_for_table("ANES_Representation_Diff_2", predictors_no_int, rumor_1_design, family = gaussian())

rumor_2_anes_1_int_model <- run_regression_for_table("ANES_Representation_Diff_1", predictors_no_int, rumor_2_design, family = gaussian())
rumor_2_anes_2_int_model <- run_regression_for_table("ANES_Representation_Diff_2", predictors_no_int, rumor_2_design, family = gaussian())


rumor_anes_table <- create_ols_summary_table(
  models = list(rumor_1_anes_1_int_model, rumor_1_anes_2_int_model, rumor_2_anes_1_int_model, rumor_2_anes_2_int_model),
  title = "OLS Regression Results for Individual Rumor Interacted ANES Representation Models",
  column.labels = paste("Rumor:", rep(rumors, each = 2), anes_labels),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("rumor_anes_int_models_table%s.tex", output_label)),
    label = "tab:rumor_anes_int_models",
    longtable = FALSE
)
print("Table for rumor-level Interacted ANES models has been created.")


# ANES pooled
pooled_anes_1_model <- run_regression_for_table("ANES_Representation_Diff_1", predictors, svy_design, family = gaussian())
pooled_anes_2_model <- run_regression_for_table("ANES_Representation_Diff_2", predictors, svy_design, family = gaussian())

pooled_anes_table <- create_ols_summary_table(
  models = list(pooled_anes_1_model, pooled_anes_2_model),
  title = "OLS Regression Results for Pooled ANES Representation Models",
  column.labels = rep(anes_labels, times = 2),
#   covariate.labels = covar_labels,
  out.file = file.path(output_dir, sprintf("pooled_anes_models_table%s.tex", output_label)),
    label = "tab:pooled_anes_models",
    longtable = FALSE
)
print("Table for pooled ANES models has been created.")

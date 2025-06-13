#--------------------------------------------#
#                                            #
####          Robustness Checks           ####
#          Kim & Zauberman (2024)            #
#                                            #
#--------------------------------------------#

#--------------------------------------------#
#                                            #
####            Used packages             ####
#                                            #
#--------------------------------------------#

if(!require(emmeans)){install.packages('emmeans')}
if(!require(gtsummary)){install.packages('gtsummary')}
if(!require(kim)){install.packages('kim')}
if(!require(quantreg)){install.packages('quantreg')}
if(!require(tidyverse)){install.packages('tidyverse')}
if(!require(interactions)){install.packages('interactions')}
if(!require(boot)){install.packages('boot')}
if(!require(ggplot2)){install.packages('ggplot2')}
if(!require(ggeffects)){install.packages('ggeffects')}
if(!require(patchwork)){install.packages('patchwork')}
if(!require(paletteer)){install.packages('paletteer')}
if(!require(kableExtra)){install.packages('kableExtra')}
if(!require(rlang)){install.packages('rlang')}


start_kim()


#--------------------------------------------#
#                                            #
####             Load data                ####
#                                            #
#--------------------------------------------#

# Please set the the path to your repository !!!

setwd("/Users/david/Box Sync/R/rep_kim_zaubermann/scripts/")
setwd("C:/Users/Clemens Lindner/Documents/github/rep_kim_zaubermann/data")

# Use the original authors' object names
d001 <- read_csv("bias and ideology data v010", na.strings = "")
d001 <- read_csv("bias and ideology data v013.csv", na.strings = "")


#--------------------------------------------#
#                                            #
####         Prepare data sets            ####
#                                            #
#--------------------------------------------#

# convert the binary bias target variables to factors
d001[, bias_target_men_vs_women := factor(
  bias_target_men_vs_women, levels = c("Men", "Women"))]
d001[, bias_target_whites_vs_blacks := factor(
  bias_target_whites_vs_blacks, levels = c("Whites", "Blacks"))]
d001[, bias_target_men_vs_unknown := factor(
  bias_target_men_vs_unknown, levels = c("Men", "Unknown"))]
d001[, bias_target_women_vs_unknown := factor(
  bias_target_women_vs_unknown, levels = c("Women", "Unknown"))]
d001[, bias_target_whites_vs_unknown := factor(
  bias_target_whites_vs_unknown, levels = c("Whites", "Unknown"))]
d001[, bias_target_blacks_vs_unknown := factor(
  bias_target_blacks_vs_unknown, levels = c("Blacks", "Unknown"))]
d001[, bias_target_liberals_vs_conservatives := factor(
  bias_target_liberals_vs_conservatives,
  levels = c("Liberals", "Conservatives"),
  labels = c("Liberal\nTarget", "Conserv.\nTarget"))]
d001[, bias_target_liberals_vs_unknown := factor(
  bias_target_liberals_vs_unknown, levels = c("Liberals", "Unknown"),
  labels = c("Liberal\nTarget", "Unknown\nTarget"))]
d001[, bias_target_conservatives_vs_unknown := factor(
  bias_target_conservatives_vs_unknown,
  levels = c("Conservatives", "Unknown"),
  labels = c("Conserv.\nTarget", "Unknown\nTarget"))]



###########################################################################
#
# Abstract and Intro ------------------------------------------------------
#
###########################################################################

# Check entities ----------------------------------------------------------
tv(d001[, entity], na.rm = FALSE)

# Subset data for main text -----------------------------------------------
d002 <- d001[entity %in% c(
  "Employees", "Police Brutality Victims", 
  "University Students", "Monkeys") & 
    !is.na(study_id_for_paper) &
    !is.na(bias_threshold) &
    completed_survey == "Yes" &
    (!is.na(conservatism_7pt_merged) | 
       !is.na(conservatism_slider_merged)) &
    passed_prereg_or_similar_criteria == "Yes"]

# Count of studies --------------------------------------------------------
tv(d002[, study_id_for_paper]) # 26 studies

# Total sample size across the 26 studies ---------------------------------
d002[, .N] # N = 14,925


#--------------------------------------------#
#                                            #
####               Set up                 ####
#                                            #
#--------------------------------------------#


### Set a color palette ####
# Colors for plots
my_pal <- c(OLS = "#5e4fa2",
            Q10 = "#3288bd",
            Q25 = "#abdda4",
            Q50 = "#66c2a5",
            Q75 = "#f46d43",
            Q90 = "#9e0142")

### Functions ####

# Set up a function for simple linear models
fun_mod_lin <- function(ds, dv, iv, targets) {
  
  # Create an empty list for the output
  mods <- list()
  
  # Loop over targets
  for (i in seq_along(targets)) {
    
    # Set the target value
    target <- targets[i]
    
    # Set up formula
    formula = as.formula(paste(my_dv, "~", my_iv))
    
    # Create sub data frames for each model
    filtered_ds <- ds |> 
      subset(bias_target == target)
    
    # Run model
    model <- lm(formula, data = filtered_ds)
    
    # Save model outputs in the list from the beginning
    mods[[target]] <- model
  }
  
  return(mods)
}

# Set up a function for simple linear models WITH interaction
fun_mod_lin_wi <- function(ds, target_cols, dv, iv) {
  
  # Create an empty list to store models
  mods <- list()
  
  # Loop over targets
  for (i in seq_along(target_cols)) {
    
    # Set the target value
    target <- target_cols[i]
    
    # Set up formula using the provided dv (dependent variable) and iv (independent variable)
    formula <- as.formula(paste(dv, "~", target, "*", iv))
    
    # Subset the data for the current target
    filtered_ds <- ds |> 
      dplyr::select(all_of(target), # all_of is needed for "external" vector
                    {{dv}},
                    {{iv}}) |> 
      
      drop_na(all_of(target)) # Get rid of NAs
    
    model <- lm(formula, 
                data = filtered_ds)
    
    
    # Store all tau models for the current target in the mods list
    mods[[target]] <- model
  }
  
  return(mods)
}


# Set up a function for quantile models (no interaction)
# Saves the models for EACH tau
fun_mod_qr_ni <- function(ds, dv, iv, targets, taus) {
  
  # Create an empty list to store models
  mods <- list()
  
  # Loop over targets
  for (i in seq_along(targets)) {
    
    # Set the target value
    target <- targets[i]
    
    # Set up formula using the provided dv (dependent variable) and iv (independent variable)
    formula <- as.formula(paste(dv, "~", iv))
    
    # Subset the data for the current target
    filtered_ds <- ds |> 
      subset(bias_target == target)
    
    # Create an empty list to store models for each tau
    tau_models <- list()
    
    # Loop over taus
    for (mt in seq_along(taus)) {
      
      # Set the tau value
      tau <- taus[mt]
      
      # Run the quantile regression model for the current tau
      model <- quantreg::rq(formula, 
                            data = filtered_ds, 
                            tau = tau)
      
      # Store the model for the current tau in the list
      tau_models[[paste0("t_", tau)]] <- model
    }
    
    # Store all tau models for the current target in the mods list
    mods[[target]] <- tau_models
  }
  
  return(mods)
}

# Set up a function for quantile models WITH interaction
# Saves the models for EACH tau
fun_mod_qr_wi_st <- function(ds, target_cols, dv, iv, taus) {
  
  # Create an empty list to store models
  mods <- list()
  
  # Loop over targets
  for (i in seq_along(target_cols)) {
    
    # Set the target value
    target <- target_cols[i]
    
    # Set up formula using the provided dv (dependent variable) and iv (independent variable)
    formula <- as.formula(paste(dv, "~", target, "*", iv))
    
    # Subset the data for the current target
    filtered_ds <- ds |> 
      dplyr::select(all_of(target), # all_of is needed for "external" vector
                    {{dv}},
                    {{iv}}) |> 
      
      drop_na(all_of(target)) # Get rid of NAs
    
    # Create an empty list to store models for each tau
    tau_models <- list()
    
    # Loop over taus
    for (mt in seq_along(taus)) {
      
      # Set the tau value
      tau <- taus[mt]
      # Run the quantile regression model for the current tau
      model <- quantreg::rq(formula, 
                            data = filtered_ds, 
                            tau = tau)
      
      # Store the model for the current tau in the list
      tau_models[[paste0("t_", tau)]] <- model
    }
    
    # Store all tau models for the current target in the mods list
    mods[[target]] <- tau_models
  }
  
  return(mods)
}

# Set up a function for quantile models WITH interaction
# Saves one model for ALL taus
fun_mod_qr_wi <- function(ds, target_cols, dv, iv, taus) {
  
  # Create an empty list to store models
  mods <- list()
  
  # Loop over targets
  for (i in seq_along(target_cols)) {
    
    # Set the target value
    target <- target_cols[i]
    
    # Set up formula using the provided dv (dependent variable) and iv (independent variable)
    formula <- as.formula(paste(dv, "~", target, "*", iv))
    
    # Subset the data for the current target
    filtered_ds <- ds |> 
      dplyr::select(all_of(target), # all_of is needed for "external" vector
                    {{dv}},
                    {{iv}}) |> 
      
      drop_na(all_of(target)) # Get rid of NAs
    
    
    
    
    model <- quantreg::rq(formula, 
                          data = filtered_ds, 
                          tau = taus)
    
    
    # Store all tau models for the current target in the mods list
    mods[[target]] <- model
  }
  
  return(mods)
}


# Set up a function for plotting a linear model with accompanying quantile models
fun_plot_lm_rq <- function(ds, y_var, x_var, colors = NULL) {
  
  ds |> 
    ggplot2::ggplot(aes(x = {{x_var}}, 
                        y = {{y_var}})) +
    geom_smooth(method = "lm", se = FALSE, aes(color = "OLS"),
                linewidth = 1.5) +
    
    geom_quantile(quantiles = 0.1, aes(color = "Q10"), alpha = 0.8) +
    geom_quantile(quantiles = 0.25, aes(color = "Q25"), alpha = 0.8) +
    geom_quantile(quantiles = 0.5, aes(color = "Q50"), alpha = 0.8) +
    geom_quantile(quantiles = 0.75, aes(color = "Q75"), alpha = 0.8) +
    geom_quantile(quantiles = 0.9, aes(color = "Q90"), alpha = 0.8) +
    
    scale_color_manual(values = colors) +
    
    labs(x = "Conservatism",
         y = "Bias threshold") +
    
    theme_bw() +
    theme(legend.title=element_blank()) 
}

# Set up a function for plotting quantile models WITH interactions
fun_plot_qr_wi <- function(ds, target_cols, y_var, x_var, colors = NULL) {
  
  # Create an empty list for the output
  plots <- list()
  
  # Loop over targets using seq_along
  for (i in seq_along(target_cols)) {
    
    target <- target_cols[i]
    
    # Create plot
    p <- ds |> 
      dplyr::select(all_of(target), # all_of is needed for "external" vector
                    {{y_var}},
                    {{x_var}}) |> 
      
      drop_na(all_of(target)) # Get rid of NAs
    
    # Save model outputs in the list from the beginning
    plots[[target]] <- p |> 
      
      ggplot(aes(x = {{x_var}}, 
                 y = {{y_var}})) +
      
      geom_jitter(alpha = .3) +
      
      geom_quantile(quantiles = 0.1, aes(color = "Q10"), alpha = 0.8) +
      geom_quantile(quantiles = 0.25, aes(color = "Q25"), alpha = 0.8) +
      geom_quantile(quantiles = 0.5, aes(color = "Q50"), alpha = 0.8) +
      geom_quantile(quantiles = 0.75, aes(color = "Q75"), alpha = 0.8) +
      geom_quantile(quantiles = 0.9, aes(color = "Q90"), alpha = 0.8) +
      
      geom_smooth(method = "lm", se = FALSE, aes(color = "OLS"),
                  linewidth = 1.5) +
      
      facet_wrap(as.formula(paste("~", target))) + # as.formula solves the problem, because (~ target), does not work
      
      
      scale_color_manual(values = my_pal) +
      
      labs(x = "Conservatism - centered",
           y = "Bias threshold") + 
      
      theme_bw() +
      theme(legend.title=element_blank())
    
  }
  
  return(plots)
}


# Set up a function for model summaries for quantile regression
fun_mod_summaries <- function(model_list) {
  
  lapply(model_list, function(model) {
    summary(model, se = "boot")
  })
}


# Set up a function for computing emmeans
fun_em <- function(ds, dv, ivs, moderator, steps = NULL) {
  
  # List to store the results
  emms_mod_linear <- list()
  
  for (i in seq_along(ivs)) {
    
    # Set the iv value
    iv <- ivs[i]
    
    # This creates the formulas for the linear models based on the function input
    formulars <- as.formula(paste(dv, "~", iv, "*", moderator))
    
    mod_linear <- lm(formulars, data = ds)
    
    # This creates the specs argument for the emmeans function
    pw_form <- as.formula(paste("pairwise", "~", iv, "|", moderator))
    
    # Run emmeans on the linear model with dynamic moderator
    if (is.null(steps)) {
      emms_mod_linear[[iv]] <- emmeans(
        mod_linear,
        pw_form,
        infer = TRUE
      )
    } else {
      # Use the user-specified 'steps' if provided
      emms_mod_linear[[iv]] <- emmeans(
        mod_linear,
        pw_form,
        at = setNames(list(steps), moderator),
        infer = TRUE
      )
    }
  }
  
  return(emms_mod_linear)
}

# Set up a function for plotting emmeans
fun_em_plot <- function(ds, dv, ivs, moderator, steps = NULL) {
  
  # List to store the results
  emms_mod_plot <- list()
  
  for (i in seq_along(ivs)) {
    
    # Set the iv value
    iv <- ivs[i]
    
    # This creates the formulas for the linear models based on the function input
    formulars <- as.formula(paste(dv, "~", iv, "*", moderator))
    
    mod_linear <- lm(formulars, data = ds)
    
    # This creates the specs argument for the emmip function
    pw_form <- as.formula(paste(iv, "~", moderator))
    
    # Run emmeans on the linear model with dynamic moderator
    if (is.null(steps)) {
      emms_mod_plot[[iv]] <- emmip(
        mod_linear,
        pw_form,
        dodge = 0, 
        CI = TRUE
      )
    } else {
      # Use the user-specified 'steps' if provided
      emms_mod_plot[[iv]] <- emmip(
        mod_linear,
        pw_form,
        at = setNames(list(steps), moderator),
        dodge = 0, 
        CI = TRUE
      )
    }
  }
  
  return(emms_mod_plot)
}


### For summary tables  ####

# Set table spanners
my_tb_sp <- c("OLS", "QR 10", "QR 25", "QR 50", "QR 75", "QR 90")

# Set up a function for summary tables - use with caution !!!

fun_table_simple_models <- function(group) {
  tbl_merge(
    tbls = list(
      tbl_regression(sim_lms[[group]]) |> bold_p(),
      tbl_regression(sim_qrs[[group]]$t_0.1) |> bold_p(),
      tbl_regression(sim_qrs[[group]]$t_0.25) |> bold_p(),
      tbl_regression(sim_qrs[[group]]$t_0.5) |> bold_p(),
      tbl_regression(sim_qrs[[group]]$t_0.75) |> bold_p(),
      tbl_regression(sim_qrs[[group]]$t_0.9) |> bold_p()
    ),
    tab_spanner = my_tb_sp
  )
}

fun_table_inter_models <- function(group) {
  tbl_merge(
    tbls = list(
      tbl_regression(int_lms[[group]]) |> bold_p(),
      tbl_regression(int_qrs[[group]]$t_0.1) |> bold_p(),
      tbl_regression(int_qrs[[group]]$t_0.25) |> bold_p(),
      tbl_regression(int_qrs[[group]]$t_0.5) |> bold_p(),
      tbl_regression(int_qrs[[group]]$t_0.75) |> bold_p(),
      tbl_regression(int_qrs[[group]]$t_0.9) |> bold_p()
    ),
    tab_spanner = my_tb_sp
  )
}


colorize_comparison_table <- function(tbl, metrics,
                                      digits = 0,
                                      color_ok = "forestgreen",
                                      color_fail = "firebrick",
                                      color_na = "lightgray") {
  
  tbl_colored <- tbl
  
  for (metric in metrics) {
    rep_col <- paste0(metric, "_reported")
    repro_col <- paste0(metric, "_reproduced")
    
    # Validierung
    if (!(rep_col %in% names(tbl)) || !(repro_col %in% names(tbl))) {
      warning(paste("Spalten fehlen:", rep_col, "oder", repro_col))
      next
    }
    
    reported_vals <- tbl[[rep_col]]
    reproduced_vals <- round(tbl[[repro_col]],4)
    
    # Vergleichslogik vorbereiten
    comparison <- rep(NA, length(reported_vals))
    
    if (metric == "p") {
      # p-Werte benötigen spezielle Behandlung
      for (j in seq_along(reported_vals)) {
        rep_val <- reported_vals[j]
        repro_val <- reproduced_vals[j]
        
        if (is.na(repro_val)) next
        
        # Fall 1: "<.001" oder Varianten
        if (grepl("^<\\s*0*\\.?0*1", rep_val)) {
          comparison[j] <- repro_val < 0.001
        }
        # Fall 2: numerisch
        else if (!is.na(suppressWarnings(as.numeric(rep_val)))) {
          comparison[j] <- round(as.numeric(rep_val), digits) == round(repro_val, digits)
        }
        # Fall 3: unklar (z.B. "n.s.")
        else {
          comparison[j] <- NA
        }
      }
      
    } else if (metric == "n") {
      # exakter Vergleich für "n"
      comparison <- reported_vals == reproduced_vals
      
    } else {
      # gerundeter Vergleich für numerische Maße
      comparison <- round(as.numeric(reported_vals), digits) == round(as.numeric(reproduced_vals), digits)
    }
    
    # Farbige Darstellung in Reproduced-Spalte
    tbl_colored[[repro_col]] <- cell_spec(
      reproduced_vals,
      color = "white",
      background = ifelse(is.na(comparison), color_na,
                          ifelse(comparison, color_ok, color_fail))
    )
  }
  
  return(tbl_colored)
}

### For Bootstrap ####

boot_jn_fn <- function(data, indices) {
  d <- data[indices, ]
  model <- tryCatch(
    lm(formula, data = d),
    error = function(e) return(c(NA, NA))
  )
  jn <- tryCatch(
    johnson_neyman(model,
                   pred = !!rlang::sym(pred),
                   modx = "conservatism_7pt_merged",
                   alpha = 0.05,
                   plot = FALSE),
    error = function(e) return(c(NA, NA))
  )
  if (is.list(jn)) {
    bounds <- jn$bounds
    return(c(bounds[1], bounds[length(bounds)]))
  } else {
    return(c(NA, NA))
  }
}

#---------------------------------------------#
#                                             #
####Subset Data for Studies (same like K&Z)####
#                                             #
#---------------------------------------------#

##* Study 1 ####
d101 <- d002[study_id_for_paper == "01"]


##* Study 2 ####
d201 <- d002[study_id_for_paper == "02"]

##* Meta Analysis ####
d003 <- d001[entity %in% c(
  "Employees", "Police Brutality Victims", 
  "University Students", "Monkeys") & 
    !is.na(study_id_for_paper) &
    !is.na(bias_threshold) &
    completed_survey == "Yes" &
    (!is.na(conservatism_7pt_merged) | 
       !is.na(conservatism_slider_merged))]



#--------------------------------------------#
#                                            #
####        Study 1 - Descriptives        ####
#                                            #
#--------------------------------------------#

tbl1_reported <- 
  read.csv("tbl1_reported.csv", encoding = "UTF-8", header = T, text = "",na.strings = "NA") %>%
  tibble::as_tibble() %>%
  janitor::clean_names()


tbl1_reproduced <- 
  d101 %>%
  group_by(bias_target) %>%
  summarise(n = n(),
            mean = mean(bias_threshold,na.rm = T),
            sd = sd(bias_threshold,na.rm = T),
            median = median(bias_threshold,na.rm = T),
            r = cor.test(conservatism_7pt_merged, bias_threshold)$estimate,
            ci_low = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[1],
            ci_upp = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[2],
            p = cor.test(conservatism_7pt_merged, bias_threshold)$p.value) %>%
  rename("target" = "bias_target")


metrics <- c("n", "mean", "sd", "median", "r", "ci_low", "ci_upp","p")

tbl1_descriptives <- 
  tbl1_reported %>%
  select(target,all_of(metrics)) %>%
  filter(!is.na(n)) %>%
  full_join(.,
            tbl1_reproduced,
            by = "target") %>%
  rename_with(.fn = ~ gsub("\\.x$", "_reported", .x), .cols = ends_with(".x")) %>%
  rename_with(.fn = ~ gsub("\\.y$", "_reproduced", .x), .cols = ends_with(".y")) %>%
  select(
    target,
    starts_with("n_"),
    starts_with("mean_"),
    starts_with("sd_"),
    starts_with("median_"),
    starts_with("r_"),
    starts_with("ci_low_"),
    starts_with("ci_upp_"),
    starts_with("p_")
  )


tbl1_descriptives %>%
  kable("html", escape = FALSE,
        col.names = c("Target", rep(c("Reported", "Reproduced"), times = length(metrics)))) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c(" " = 1, setNames(rep(2, length(metrics)), metrics)))


tbl_colored <- colorize_comparison_table(tbl1_descriptives, metrics)

tbl_colored %>%
  kable("html", escape = FALSE,
        col.names = c("Target", rep(c("Reported", "Reproduced"), times = length(metrics)))) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c(" " = 1, setNames(rep(2, length(metrics)), metrics)))



#--------------------------------------------#
#                                            #
####        Study 1 - Correlations        ####
#                                            #
#--------------------------------------------#

##* Use the same data set as K & Z ####
d101 <- d002[study_id_for_paper == "01"]



# Set parameters for the function (fun_mod_lin)

my_iv <- "conservatism_7pt_merged"

my_targets <-  c("women",
                 "men",
                 "whites",
                 "blacks",
                 "unknown")

my_dv <-  "bias_threshold"

my_taus <- c(0.10, 0.25, 0.5, 0.75, 0.9)


#* Run linear models ####
sim_lms <- fun_mod_lin(d101,
                       my_dv,
                       my_iv,
                       my_targets)

#* Run quantile models as robustness check ####
sim_qrs <- fun_mod_qr_ni(d101,
                         my_dv,
                         my_iv,
                         my_targets,
                         my_taus)


#* Women as targets ####

sim_lms$women 

# Check assumptions
performance::check_outliers(sim_lms$women)
performance::check_normality(sim_lms$women)
performance::check_heteroscedasticity(sim_lms$women)


# Quantile regressions 

# Plot 
d101 |>
  filter(bias_target == "women") |> 
  fun_plot_lm_rq(y_var = bias_threshold,
                 x_var = conservatism_7pt_merged,
                 colors = my_pal)


fun_mod_summaries(sim_qrs$women)


#  Compare Model fits 
performance::compare_performance(sim_lms$women,
                                 sim_qrs$women$t_0.1,
                                 sim_qrs$women$t_0.25,
                                 sim_qrs$women$t_0.5,
                                 sim_qrs$women$t_0.75,
                                 sim_qrs$women$t_0.9) #|> plot()

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_simple_models("women")



#* Men as targets ####

sim_lms$men 

# Check assumptions
performance::check_outliers(sim_lms$men)
performance::check_normality(sim_lms$men)
performance::check_heteroscedasticity(sim_lms$men)


# Quantile regressions 

# Plot 
d101 |>
  filter(bias_target == "men") |> 
  fun_plot_lm_rq(y_var = bias_threshold,
                 x_var = conservatism_7pt_merged,
                 colors = my_pal)


fun_mod_summaries(sim_qrs$men)


#  Compare Model fits 
performance::compare_performance(sim_lms$men,
                                 sim_qrs$men$t_0.1,
                                 sim_qrs$men$t_0.25,
                                 sim_qrs$men$t_0.5,
                                 sim_qrs$men$t_0.75,
                                 sim_qrs$men$t_0.9) 

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_simple_models("men")

#* Whites as targets ####

sim_lms$whites 

# Check assumptions
performance::check_outliers(sim_lms$whites)
performance::check_normality(sim_lms$whites)
performance::check_heteroscedasticity(sim_lms$whites)


# Quantile regressions 

# Plot 
d101 |>
  filter(bias_target == "whites") |> 
  fun_plot_lm_rq(y_var = bias_threshold,
                 x_var = conservatism_7pt_merged,
                 colors = my_pal)


fun_mod_summaries(sim_qrs$whites)


#  Compare Model fits 
performance::compare_performance(sim_lms$whites,
                                 sim_qrs$whites$t_0.1,
                                 sim_qrs$whites$t_0.25,
                                 sim_qrs$whites$t_0.5,
                                 sim_qrs$whites$t_0.75,
                                 sim_qrs$whites$t_0.9) 

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_simple_models("whites")

#* Blacks as targets ####

sim_lms$blacks 

# Check assumptions
performance::check_outliers(sim_lms$blacks)
performance::check_normality(sim_lms$blacks)
performance::check_heteroscedasticity(sim_lms$blacks)


# Quantile regressions 

# Plot 
d101 |>
  filter(bias_target == "blacks") |> 
  fun_plot_lm_rq(y_var = bias_threshold,
                 x_var = conservatism_7pt_merged,
                 colors = my_pal)


fun_mod_summaries(sim_qrs$blacks)


#  Compare Model fits 
performance::compare_performance(sim_lms$blacks,
                                 sim_qrs$blacks$t_0.1,
                                 sim_qrs$blacks$t_0.25,
                                 sim_qrs$blacks$t_0.5,
                                 sim_qrs$blacks$t_0.75,
                                 sim_qrs$blacks$t_0.9) 

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_simple_models("blacks")


#* Unknown as targets ####

sim_lms$unknown 

# Check assumptions
performance::check_outliers(sim_lms$unknown)
performance::check_normality(sim_lms$unknown)
performance::check_heteroscedasticity(sim_lms$unknown)


# Quantile regressions 

# Plot 
d101 |>
  filter(bias_target == "unknown") |> 
  fun_plot_lm_rq(y_var = bias_threshold,
                 x_var = conservatism_7pt_merged,
                 colors = my_pal)


fun_mod_summaries(sim_qrs$unknown)


#  Compare Model fits 
performance::compare_performance(sim_lms$unknown,
                                 sim_qrs$unknown$t_0.1,
                                 sim_qrs$unknown$t_0.25,
                                 sim_qrs$unknown$t_0.5,
                                 sim_qrs$unknown$t_0.75,
                                 sim_qrs$unknown$t_0.9) 

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_simple_models("unknown")


#--------------------------------------------#
#                                            #
####        Study 1 - Regressions         ####
#                                            #
#--------------------------------------------#

# Set parameters for the function (fun_mod_lin)

# Set variables for the functions
my_dv <- "bias_threshold"

my_ivs <- c(
  "bias_target_men_vs_women",
  "bias_target_whites_vs_blacks",
  "bias_target_men_vs_unknown",
  "bias_target_women_vs_unknown",
  "bias_target_whites_vs_unknown",
  "bias_target_blacks_vs_unknown")

my_mod <- "conservatism_7pt_merged_mean_ctrd"


my_taus <- c(0.10, 0.25, 0.5, 0.75, 0.9)



#* Run linear models  with interaction ####
int_lms <- fun_mod_lin_wi(d101,
                          my_ivs,
                          my_dv,
                          my_mod)

#* Run quantile models  with interaction as robustness check ####

# Get an output for each tau separately - this is needed for summary table
int_qrs <- fun_mod_qr_wi_st(d101,
                            my_ivs,
                            my_dv,
                            my_mod,
                            my_taus)

int_qrs_summary <- fun_mod_qr_wi(d101,
                                 my_ivs,
                                 my_dv,
                                 my_mod,
                                 my_taus)

#* Create plots for quantile models ####

# Use the column names from the data set, and my ivs only from the vector !!!
int_qrs_plots <- fun_plot_qr_wi(d101,
                                my_ivs,
                                bias_threshold,
                                conservatism_7pt_merged_mean_ctrd,
                                my_pal)



#* Women vs. Men ####

summary(int_lms$bias_target_men_vs_women)

# Check Assumptions
performance::check_outliers(int_lms$bias_target_men_vs_women)
performance::check_normality(int_lms$bias_target_men_vs_women)
performance::check_heteroscedasticity(int_lms$bias_target_men_vs_women)

## Quantile regression ##

# Plot 
int_qrs_plots$bias_target_men_vs_women

# Model
int_qrs_summary$bias_target_men_vs_women |> 
  summary(se = "boot")


#  Compare Model fits 
performance::compare_performance(int_lms$bias_target_men_vs_women,
                                 int_qrs$bias_target_men_vs_women$t_0.1,
                                 int_qrs$bias_target_men_vs_women$t_0.25,
                                 int_qrs$bias_target_men_vs_women$t_0.5,
                                 int_qrs$bias_target_men_vs_women$t_0.75,
                                 int_qrs$bias_target_men_vs_women$t_0.9) |> plot()

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_inter_models("bias_target_men_vs_women")


#* Blacks vs. Whites ####

summary(int_lms$bias_target_whites_vs_blacks)

# Check Assumptions
performance::check_outliers(int_lms$bias_target_whites_vs_blacks)
performance::check_normality(int_lms$bias_target_whites_vs_blacks)
performance::check_heteroscedasticity(int_lms$bias_target_whites_vs_blacks)

## Quantile regression ##

# Plot 
int_qrs_plots$bias_target_whites_vs_blacks

# Model
int_qrs_summary$bias_target_whites_vs_blacks |> 
  summary(se = "boot")


#  Compare Model fits 
performance::compare_performance(int_lms$bias_target_whites_vs_blacks,
                                 int_qrs$bias_target_whites_vs_blacks$t_0.1,
                                 int_qrs$bias_target_whites_vs_blacks$t_0.25,
                                 int_qrs$bias_target_whites_vs_blacks$t_0.5,
                                 int_qrs$bias_target_whites_vs_blacks$t_0.75,
                                 int_qrs$bias_target_whites_vs_blacks$t_0.9) |> plot()

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_inter_models("bias_target_whites_vs_blacks")

#* Men vs. Unknown ####

summary(int_lms$bias_target_men_vs_unknown)

# Check Assumptions
performance::check_outliers(int_lms$bias_target_men_vs_unknown)
performance::check_normality(int_lms$bias_target_men_vs_unknown)
performance::check_heteroscedasticity(int_lms$bias_target_men_vs_unknown)

## Quantile regression ##

# Plot 
int_qrs_plots$bias_target_men_vs_unknown

# Model
int_qrs_summary$bias_target_men_vs_unknown |> 
  summary(se = "boot")


#  Compare Model fits 
performance::compare_performance(int_lms$bias_target_men_vs_unknown,
                                 int_qrs$bias_target_men_vs_unknown$t_0.1,
                                 int_qrs$bias_target_men_vs_unknown$t_0.25,
                                 int_qrs$bias_target_men_vs_unknown$t_0.5,
                                 int_qrs$bias_target_men_vs_unknown$t_0.75,
                                 int_qrs$bias_target_men_vs_unknown$t_0.9) |> plot()

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_inter_models("bias_target_men_vs_unknown")


#* Women vs. Unknown ####

summary(int_lms$bias_target_women_vs_unknown)

# Check Assumptions
performance::check_outliers(int_lms$bias_target_women_vs_unknown)
performance::check_normality(int_lms$bias_target_women_vs_unknown)
performance::check_heteroscedasticity(int_lms$bias_target_women_vs_unknown)

## Quantile regression ##

# Plot 
int_qrs_plots$bias_target_women_vs_unknown

# Model
int_qrs_summary$bias_target_women_vs_unknown |> 
  summary(se = "boot")


#  Compare Model fits 
performance::compare_performance(int_lms$bias_target_women_vs_unknown,
                                 int_qrs$bias_target_women_vs_unknown$t_0.1,
                                 int_qrs$bias_target_women_vs_unknown$t_0.25,
                                 int_qrs$bias_target_women_vs_unknown$t_0.5,
                                 int_qrs$bias_target_women_vs_unknown$t_0.75,
                                 int_qrs$bias_target_women_vs_unknown$t_0.9) |> plot()

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_inter_models("bias_target_women_vs_unknown")


#* Whites vs. Unknown ####

summary(int_lms$bias_target_whites_vs_unknown)

# Check Assumptions
performance::check_outliers(int_lms$bias_target_whites_vs_unknown)
performance::check_normality(int_lms$bias_target_whites_vs_unknown)
performance::check_heteroscedasticity(int_lms$bias_target_whites_vs_unknown)

## Quantile regression ##

# Plot 
int_qrs_plots$bias_target_whites_vs_unknown

# Model
int_qrs_summary$bias_target_whites_vs_unknown |> 
  summary(se = "boot")


#  Compare Model fits 
performance::compare_performance(int_lms$bias_target_whites_vs_unknown,
                                 int_qrs$bias_target_whites_vs_unknown$t_0.1,
                                 int_qrs$bias_target_whites_vs_unknown$t_0.25,
                                 int_qrs$bias_target_whites_vs_unknown$t_0.5,
                                 int_qrs$bias_target_whites_vs_unknown$t_0.75,
                                 int_qrs$bias_target_whites_vs_unknown$t_0.9) |> plot()

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_inter_models("bias_target_whites_vs_unknown")

#* Blacks vs. Unknown ####

summary(int_lms$bias_target_blacks_vs_unknown)

# Check Assumptions
performance::check_outliers(int_lms$bias_target_blacks_vs_unknown)
performance::check_normality(int_lms$bias_target_blacks_vs_unknown)
performance::check_heteroscedasticity(int_lms$bias_target_blacks_vs_unknown)

## Quantile regression ##

# Plot 
int_qrs_plots$bias_target_blacks_vs_unknown

# Model
int_qrs_summary$bias_target_blacks_vs_unknown |> 
  summary(se = "boot")


#  Compare Model fits 
performance::compare_performance(int_lms$bias_target_blacks_vs_unknown,
                                 int_qrs$bias_target_blacks_vs_unknown$t_0.1,
                                 int_qrs$bias_target_blacks_vs_unknown$t_0.25,
                                 int_qrs$bias_target_blacks_vs_unknown$t_0.5,
                                 int_qrs$bias_target_blacks_vs_unknown$t_0.75,
                                 int_qrs$bias_target_blacks_vs_unknown$t_0.9) |> plot()

#  Create a table summary for ols, q10, q25, q50, q75, q90 
fun_table_inter_models("bias_target_blacks_vs_unknown")

#* Reproduce Regressions Table 1 #####

# all predictors
predictors <- c(
  "bias_target_men_0_vs_women_1",
  "bias_target_whites_0_vs_blacks_1",
  "bias_target_men_0_vs_unknown_1",
  "bias_target_women_0_vs_unknown_1",
  "bias_target_whites_0_vs_unknown_1",
  "bias_target_blacks_0_vs_unknown_1"
)

# prepare data frame Table 1
models_stats <- c(
  "target1_contrast",   # e.g. "men"
  "target2_contrast",   # e.g. "women"
  "moderator",          # e.g. "conservatism_7pt_merged"
  "rsq_no_int",         # R² without Interaction
  "rsq_int",            # R² with Interaction
  "rsq_chnge",          # R²-Diff
  "f",                  # F-value model comparison
  "p_comp"              # p-value model comparison
)

# empty data frame
tbl1_mod <- data.frame(matrix(nrow = 0, ncol = length(models_stats)))
colnames(tbl1_mod) <- models_stats

# loop across all predictors
for (i in predictors) {
  
  # define model
  formula_no_int <- as.formula(paste("bias_threshold ~", i, "+ conservatism_7pt_merged"))
  formula_int    <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  
  # run linear model
  m_no_int <- lm(formula_no_int, data = d101)
  m_int    <- lm(formula_int, data = d101)
  
  # compare model (ANOVA)
  model_comp <- anova(m_no_int, m_int)
  
  # get targets
  parts <- strsplit(i, "_0_vs_")[[1]]
  target1 <- gsub("bias_target_", "", parts[1])
  target2 <- gsub("_1$", "", parts[2])
  
  # write results in a row
  row <- data.frame(
    target1_contrast = target1,
    target2_contrast = target2,
    moderator        = "conservatism_7pt_merged",
    rsq_no_int       = summary(m_no_int)$r.squared,
    rsq_int          = summary(m_int)$r.squared,
    rsq_chnge        = summary(m_int)$r.squared - summary(m_no_int)$r.squared,
    f                = model_comp$F[2],
    p_comp           = model_comp$`Pr(>F)`[2],
    stringsAsFactors = FALSE
  )
  
  # add row to data frame
  tbl1_mod <- rbind(tbl1_mod, row)
}


#merge reproduced data frame with reported data frame
tbl1_regressions <- 
  tbl1_reported %>%
  select(12:ncol(.)) %>%
  full_join(.,
            tbl1_mod %>%
              mutate(moderator = stringr::str_remove_all(moderator,"_7pt_merged")),
            by = c("target1_contrast","target2_contrast","moderator")) %>%
  rename_with(.fn = ~ gsub("\\.x$", "_reported", .x), .cols = ends_with(".x")) %>%
  rename_with(.fn = ~ gsub("\\.y$", "_reproduced", .x), .cols = ends_with(".y")) %>%
  select(
    starts_with("target"),
    starts_with("moderator"),
    starts_with("rsq_no_"),
    starts_with("rsq_int"),
    starts_with("rsq_chn"),
    starts_with("f_"),
    starts_with("p_")
  )

reg_stats <- c("rsq_no_int","rsq_int","rsq_chnge","f","p_comp")

# create a readable html table
tbl1_regressions %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Moderator", rep(c("Reported", "Reproduced"), 
                                                             times = length(reg_stats)))) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2,
                     " " = 1, 
                     setNames(rep(2, length(reg_stats)), 
                              c("R2 No Interaction","R2 Interaction","R2 Change","F","p"))))

# colorize the results such that green values indicate same results
tbl1_reg_colored <- colorize_comparison_table(as.data.frame(tbl1_regressions), reg_stats)

#html table
tbl1_reg_colored %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Moderator", rep(c("Reported", "Reproduced"), times = length(reg_stats)))) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2," " = 1, setNames(rep(2, length(reg_stats)), reg_stats)))

#--------------------------------------------#
#                                            #
####         Study 1 - Floodlight         ####
#                                            #
#--------------------------------------------#


# Set variables for the function
my_dv <- "bias_threshold"

my_ivs <- c(
  "bias_target_men_vs_women",
  "bias_target_whites_vs_blacks",
  "bias_target_men_vs_unknown",
  "bias_target_women_vs_unknown",
  "bias_target_whites_vs_unknown",
  "bias_target_blacks_vs_unknown")

my_mod <- "conservatism_7pt_merged"


my_steps <- seq(from = 1, to = 7, by = 0.01)

my_steps_plot <- seq(from = 1, to = 7, by = 0.1)


# Create a list with the estimated marginal mean models
emm_models <- fun_em(d101, my_dv, my_ivs, my_mod, my_steps)

# Create a list with the estimated marginal mean model plots
emm_model_plots <- fun_em_plot(d101, my_dv, my_ivs, my_mod, my_steps_plot)


#* Women vs. Men ####

# Extract contrasts for significance region

# Use only single values, otherwise contrast will be adjusted

# If you want the contrast for 4.79 from the conservatism scale
# you have to use 380 -
# 3xx because the values start from 1
# x80 because the values from the seq start with 1.00, 

emm_models$bias_target_men_vs_women$contrasts[379]
emm_models$bias_target_men_vs_women$contrasts[380]
emm_models$bias_target_men_vs_women$contrasts[381]

emm_model_plots$bias_target_men_vs_women + 
  geom_vline(xintercept = 4.79,
             alpha = .7) 


#* Blacks vs. Whites ####

# Extract contrasts for significance region
emm_models$bias_target_whites_vs_blacks$contrasts[242]
emm_models$bias_target_whites_vs_blacks$contrasts[243]
emm_models$bias_target_whites_vs_blacks$contrasts[244]

emm_models$bias_target_whites_vs_blacks$contrasts[392]
emm_models$bias_target_whites_vs_blacks$contrasts[393]
emm_models$bias_target_whites_vs_blacks$contrasts[394]

emm_model_plots$bias_target_whites_vs_blacks + 
  geom_vline(xintercept = 3.42,
             alpha = .7) +
  geom_vline(xintercept = 4.92,
             alpha = .7)

#* Men vs. Unknown ####

# Extract contrasts for significance region
emm_models$bias_target_men_vs_unknown$contrasts[293]
emm_models$bias_target_men_vs_unknown$contrasts[294]
emm_models$bias_target_men_vs_unknown$contrasts[295]

emm_model_plots$bias_target_men_vs_unknown + 
  geom_vline(xintercept = 3.93,
             alpha = .7) 


#* Women vs. Unknown ####

# Extract contrasts for significance region
emm_models$bias_target_women_vs_unknown$contrasts[303]
emm_models$bias_target_women_vs_unknown$contrasts[304]
emm_models$bias_target_women_vs_unknown$contrasts[305]

emm_model_plots$bias_target_women_vs_unknown + 
  geom_vline(xintercept = 4.03,
             alpha = .7) 


#* Whites vs. Unknown ####

# Extract contrasts for significance region
emm_models$bias_target_whites_vs_unknown$contrasts[262]
emm_models$bias_target_whites_vs_unknown$contrasts[263]
emm_models$bias_target_whites_vs_unknown$contrasts[264]

emm_model_plots$bias_target_whites_vs_unknown + 
  geom_vline(xintercept = 3.62,
             alpha = .7) 


#* Blacks vs. Unknown ####

# Extract contrasts for significance region
emm_models$bias_target_blacks_vs_unknown$contrasts[383]
emm_models$bias_target_blacks_vs_unknown$contrasts[384]
emm_models$bias_target_blacks_vs_unknown$contrasts[385]

emm_model_plots$bias_target_blacks_vs_unknown + 
  geom_vline(xintercept = 4.83,
             alpha = .7) 

#* Reproduce Plot with ggplot and ggeffects ####

#empty plot list 
plot_list <- list()

for (i in predictors) {
  
  # regression with interaction
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  
  # run model
  m_int <- lm(formula_int, data = d101)
  
  # ggpredict
  sslop <- ggpredict(m_int, terms = c("conservatism_7pt_merged", i))
  
  ros <- interactions::johnson_neyman(
    model = m_int,
    pred = !!rlang::sym(i),
    modx = conservatism_7pt_merged)
  
  vline_low <- ifelse(ros$bounds[1] >= 1 & ros$bounds[1] <= 7,ros$bounds[1],NA)
  vline_high <- ifelse(ros$bounds[2] >= 1 & ros$bounds[2] <= 7,ros$bounds[2],NA)
  
  # create Plot
  p <- ggplot(sslop, aes(x = x, y = predicted, color = group)) +
    geom_line(size = 1.6) +
    scale_color_manual(
      name = "ID Gruppen",
      values = c("deeppink3", "deepskyblue4")
    ) +
    labs(
      title = paste("Simple Slopes:", i),
      x = "Conservatism",
      y = "Bias threshold"
    ) +
    scale_x_continuous(breaks = seq(0, 7, by = 1), limits = c(0, 7)) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  if(!is.na(vline_low[[1]])) {
    p <- p + geom_vline(xintercept = vline_low[[1]])
  } else {
    p
  }
  
  if(!is.na(vline_high[[1]])) {
    p <- p + geom_vline(xintercept = vline_high[[1]])
  } else {
    p
  }
  
  # save Plot
  plot_list[[i]] <- p
}

# Grid plot
wrap_plots(plot_list, ncol = 2)

#* Reproduce Johnson-Neyman Bounds ####

#empty data frame
fig3_mod <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(fig3_mod) <- c("target1_contrast", "target2_contrast", "jn_low_raw", "jn_upp_raw")

for (i in predictors) {
  
  # model with interaction
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  m_int       <- lm(formula_int, data = d101)
  
  # get targes
  parts   <- strsplit(i, "_0_vs_")[[1]]
  target1 <- gsub("bias_target_", "", parts[1])
  target2 <- gsub("_1$", "", parts[2])
  
  # Johnson-Neyman Intervall
  ros <- interactions::johnson_neyman(
    model = m_int,
    pred = !!rlang::sym(i),
    modx = conservatism_7pt_merged)
  
  # Neue Zeile
  row <- data.frame(
    target1_contrast = target1,
    target2_contrast = target2,
    jn_low_raw       = ros$bounds[[1]],
    jn_upp_raw       = ros$bounds[[2]],
    stringsAsFactors = FALSE
  )
  
  # Zeile anhängen
  fig3_mod <- rbind(fig3_mod, row)
}

# Result table

fig3_mod %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Lower Bound","Upper Bound")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2,"Johnson-Neyman" = 2))

#* Johnson-Neyman with other raw conservatism measures as robustness check ####

# Predictor contrasts
predictors <- c(
  "bias_target_men_0_vs_women_1",
  "bias_target_whites_0_vs_blacks_1",
  "bias_target_men_0_vs_unknown_1",
  "bias_target_women_0_vs_unknown_1",
  "bias_target_whites_0_vs_unknown_1",
  "bias_target_blacks_0_vs_unknown_1"
)

# other conservatism measures as moderators
moderators <- c(
  "conservatism_7pt_merged",
  "conservatism_econ",
  "conservatism_slider_v3",
  "conservatism_social"
)

fig3_mod_long <- data.frame(
  target1_contrast = character(),
  target2_contrast = character(),
  moderator        = character(),
  interaction_b    = numeric(),
  interaction_p    = numeric(),
  jn_low_raw       = numeric(),
  jn_upp_raw       = numeric(),
  stringsAsFactors = FALSE
)


# Loop across different moderators and all target combination
for (pred in predictors) {
  for (mod in moderators) {
    
    # defina and run model
    formula_int <- as.formula(paste("bias_threshold ~", pred, "*", mod))
    m_int <- lm(formula_int, data = d101)
    
    # get target from the model
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod)
    )
    
    # write results in one row
    row <- data.frame(
      target1_contrast = target1,
      target2_contrast = target2,
      moderator        = mod,
      interaction_b    = coefficients(m_int)[[4]],
      interaction_p    = summary(m_int)$coefficients[4,4],
      jn_low_raw       = ros$bounds[[1]],
      jn_upp_raw       = ros$bounds[[2]],
      stringsAsFactors = FALSE
    )
    
    # add row to the data frame
    fig3_mod_long <- rbind(fig3_mod_long, row)
  }
}

# print results
print(fig3_mod_long)


#* Johnson-Neyman with other standardized conservatism measures as robustness check ####

d101 <- 
  d101 %>%
  mutate(across(all_of(moderators), ~as.numeric(scale(.)),.names = "{.col}_scl"))

moderators_scl <-
  paste0(moderators,"_scl")


fig3_mod_long_scl <- 
  data.frame(
    target1_contrast = character(),
    target2_contrast = character(),
    moderator        = character(),
    interaction_b_scl= numeric(),
    interaction_p_scl= numeric(),
    jn_low_scl       = numeric(),
    jn_upp_scl       = numeric(),
    stringsAsFactors = FALSE
  )


# loop across all targets and conservatism measures (z-score)
for (pred in predictors) {
  for (mod_scl in moderators_scl) {
    
    # define and run model
    formula_int_scl <- as.formula(paste("bias_threshold ~", pred, "*", mod_scl))
    m_int_scl <- lm(formula_int_scl, data = d101)
    
    # get targets from the model
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int_scl,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod_scl)
    )
    
    # write results in a row
    row <- data.frame(
      target1_contrast = target1,
      target2_contrast = target2,
      moderator        = mod_scl,
      interaction_b_scl= coefficients(m_int_scl)[[4]],
      interaction_p_scl= summary(m_int_scl)$coefficients[4,4],
      jn_low_scl       = ros$bounds[[1]],
      jn_upp_scl       = ros$bounds[[2]],
      stringsAsFactors = FALSE
    )
    
    # add row to data frame
    fig3_mod_long_scl <- rbind(fig3_mod_long_scl, row)
  }
}

# results
print(fig3_mod_long_scl)

stdy1_all_scales <- 
  fig3_mod_long_scl %>%
  mutate(moderator = stringr::str_remove_all(moderator,"_scl")) %>%
  full_join(fig3_mod_long,.)

#write a readable html table with standardized and unstandardized estimates
stdy1_all_scales %>%
  mutate(across(where(is.numeric),round,4)) %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Moderator Scale",
                      "Beta","p","Lower","Upper",
                      "Beta","p","Lower","Upper")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2," "= 1,
                     "Interaction" = 2,"Johnson-Neyman" = 2,
                     "Interaction" = 2,"Johnson-Neyman" = 2)) %>%
  add_header_above(c(" " = 3,"Unstandardized Moderator" = 4,
                     "Standardized Moderator" = 4))


#* Bootstrap JN-Bounds as Robustness check ####

# empty list for results
results_list <- list()

#loop across all predictors
for (pred in predictors) {
  cat("\n====== Predictor:", pred, "======\n")
  
  formula <- as.formula(paste("bias_threshold ~", pred, "* conservatism_7pt_merged"))
  model_orig <- lm(formula, data = d101)
  
  jn_orig <- tryCatch(
    johnson_neyman(model_orig,
                   pred = !!rlang::sym(pred),
                   modx = "conservatism_7pt_merged",
                   plot = FALSE),
    error = function(e) return(NULL)
  )
  
  if (is.null(jn_orig) || is.null(jn_orig$bounds)) {
    cat("J-N analysis failed for predictor:", pred, "\n")
    next
  }
  
  jn_bounds_orig <- jn_orig$bounds
  orig_lower <- jn_bounds_orig[1]
  orig_upper <- jn_bounds_orig[length(jn_bounds_orig)]
  cat("Original J-N bounds:", round(orig_lower, 3), "to", round(orig_upper, 3), "\n")
  
  boot_jn_fn <- function(data, indices) {
    d <- data[indices, ]
    model <- tryCatch(
      lm(formula, data = d),
      error = function(e) return(c(NA, NA))
    )
    jn <- tryCatch(
      johnson_neyman(model,
                     pred = !!rlang::sym(pred),
                     modx = "conservatism_7pt_merged",
                     alpha = 0.05,
                     plot = FALSE),
      error = function(e) return(c(NA, NA))
    )
    if (is.list(jn)) {
      bounds <- jn$bounds
      return(c(bounds[1], bounds[length(bounds)]))
    } else {
      return(c(NA, NA))
    }
  }
  
  set.seed(123)
  boot_jn <- boot(data = d101, statistic = boot_jn_fn, R = 1000)
  boot_bounds <- boot_jn$t
  boot_bounds <- boot_bounds[complete.cases(boot_bounds), ]
  
  if (nrow(boot_bounds) == 0) {
    cat("No valid bootstrap samples for predictor:", pred, "\n")
    next
  }
  
  ci_lower <- quantile(boot_bounds[, 1], probs = c(0.025, 0.975), na.rm = TRUE)
  ci_upper <- quantile(boot_bounds[, 2], probs = c(0.025, 0.975), na.rm = TRUE)
  
  cat("Bootstrapped 95% CI for lower bound:", round(ci_lower[1], 3), "to", round(ci_lower[2], 3), "\n")
  cat("Bootstrapped 95% CI for upper bound:", round(ci_upper[1], 3), "to", round(ci_upper[2], 3), "\n")
  
  results_list[[pred]] <- data.frame(
    predictor = pred,
    orig_lower = orig_lower,
    orig_upper = orig_upper,
    ci_lower_2.5 = ci_lower[1],
    ci_lower_97.5 = ci_lower[2],
    ci_upper_2.5 = ci_upper[1],
    ci_upper_97.5 = ci_upper[2],
    valid_bootstraps = nrow(boot_bounds)
  )
}

# Combine all rows into one data frame
results_df <- do.call(rbind, results_list)

# Print or view the final results
print(results_df)


results_df %>%
  mutate(across(where(is.numeric),round,4),
         predictor = stringr::str_replace_all(predictor,"_"," "),
         predictor = stringr::str_remove_all(predictor,"0 | 1"),
         predictor = stringr::str_to_title(predictor)) %>%
  select(predictor,orig_lower,ci_lower_2.5,ci_lower_97.5,
         orig_upper,ci_upper_2.5, ci_upper_97.5, valid_bootstraps) %>%
  kable("html", escape = FALSE,row.names = F,
        col.names = c("Contrast",
                      "Lower JN-Bound","Lower","Upper",
                      "Upper JN-Bound","Lower","Upper","N Valid Bootstraps")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c(" " = 1,
                     "Original" = 1,
                     "Bootstraped 95% CI"= 2,
                     "Original" = 1,
                     "Bootstraped 95% CI"= 2,
                     " " = 1))


#--------------------------------------------#
#                                            #
####        Study 2 - Correlations        ####
#                                            #
#--------------------------------------------#

#reproduce descriptives and correlations from study 2
stdy2_desc_tbl <- 
  d201 %>%
  group_by(bias_target) %>%
  summarise(n = n(),
            mean = mean(bias_threshold,na.rm = T),
            sd = sd(bias_threshold,na.rm = T),
            median = median(bias_threshold,na.rm = T),
            r = cor.test(conservatism_7pt_merged, bias_threshold)$estimate,
            ci_low = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[1],
            ci_upp = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[2],
            p = cor.test(conservatism_7pt_merged, bias_threshold)$p.value)


#write a readable html table
stdy2_desc_tbl %>%
  mutate(reported_r = case_when(bias_target == "conservatives" ~ −0.21,
                                bias_target == "liberals" ~ 0.30,
                                bias_target == "unknown" ~ 0.04),
         reported_p = case_when(bias_target == "conservatives" ~ "0.014",
                                bias_target == "liberals" ~ "<0.001",
                                bias_target == "unknown" ~ "0.680"),
         across(where(is.numeric),round,4)) %>%
  kable("html", escape = FALSE,row.names = F) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped"))



#--------------------------------------------#
#                                            #
####        Study 2 - Regressions         ####
#                                            #
#--------------------------------------------#


# Set variables for the function
my_dv <- "bias_threshold"

my_ivs <- c(
  "bias_target_liberals_vs_conservatives",
  "bias_target_liberals_vs_unknown",
  "bias_target_conservatives_vs_unknown")

my_mod <- "conservatism_7pt_merged"

# Create a list with the estimated marginal mean models
emm_models_s2 <- fun_em(d201, my_dv, my_ivs, my_mod, my_steps)

# Create a list with the estimated marginal mean model plots
emm_model_plots_s2 <- fun_em_plot(d201, my_dv, my_ivs, my_mod, my_steps_plot)


#* Liberals vs. conservatives ####

# Extract contrasts for significance region
emm_models_s2$bias_target_liberals_vs_conservatives$contrasts[255]
emm_models_s2$bias_target_liberals_vs_conservatives$contrasts[256]
emm_models_s2$bias_target_liberals_vs_conservatives$contrasts[257]

emm_models_s2$bias_target_liberals_vs_conservatives$contrasts[255]
emm_models_s2$bias_target_liberals_vs_conservatives$contrasts[256]
emm_models_s2$bias_target_liberals_vs_conservatives$contrasts[257]

# Plot
emm_model_plots_s2$bias_target_liberals_vs_conservatives + 
  geom_vline(xintercept = 3.55,
             alpha = .7) +
  geom_vline(xintercept = 5.69,
             alpha = .7)


#* Liberals vs. unknown ####


# Extract contrasts for significance region
emm_models_s2$bias_target_liberals_vs_unknown$contrasts[228]
emm_models_s2$bias_target_liberals_vs_unknown$contrasts[229]
emm_models_s2$bias_target_liberals_vs_unknown$contrasts[230]


# Plot
emm_model_plots_s2$bias_target_liberals_vs_unknown + 
  geom_vline(xintercept = 3.28,
             alpha = .7) 


#* Conservatives vs. unknown ####

# Extract contrasts for significance region
emm_models_s2$bias_target_conservatives_vs_unknown$contrasts[09]
emm_models_s2$bias_target_conservatives_vs_unknown$contrasts[10]
emm_models_s2$bias_target_conservatives_vs_unknown$contrasts[11]


# Plot
emm_model_plots_s2$bias_target_conservatives_vs_unknown + 
  geom_vline(xintercept = 1.09,
             alpha = .7) 


#* Reproduce JN-Bounds ####

predictors_stdy2 <-
  c("bias_target_liberals_0_vs_conservatives_1",
    "bias_target_liberals_0_vs_unknown_1",
    "bias_target_conservatives_0_vs_unknown_1")

fig4_mod <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(fig4_mod) <- c("target1_contrast", "target2_contrast", "jn_low_raw", "jn_upp_raw")

for (i in predictors_stdy2) {
  
  # define and run model
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  m_int       <- lm(formula_int, data = d201)
  
  # get targets
  parts   <- strsplit(i, "_0_vs_")[[1]]
  target1 <- gsub("bias_target_", "", parts[1])
  target2 <- gsub("_1$", "", parts[2])
  
  # Johnson-Neyman Intervall
  ros <- interactions::johnson_neyman(
    model = m_int,
    pred = !!rlang::sym(i),
    modx = conservatism_7pt_merged)
  
  # write resuts in a row
  row <- data.frame(
    target1_contrast = target1,
    target2_contrast = target2,
    jn_low_raw       = ros$bounds[[1]],
    jn_upp_raw       = ros$bounds[[2]],
    stringsAsFactors = FALSE
  )
  
  # bind row to data frame
  fig4_mod <- rbind(fig4_mod, row)
}

# results

fig4_mod %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Lower Bound","Upper Bound")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2,"Johnson-Neyman" = 2))

#* Reproduce Figure 4 with ggplot ----

#empty list
plot_list <- list()

#loop across all predictors / target groups
for (i in predictors_stdy2) {
  
  # define model
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  
  # run model
  m_int <- lm(formula_int, data = d201)
  
  # ggpredict
  sslop <- ggpredict(m_int, terms = c("conservatism_7pt_merged", i))
  
  ros <- interactions::johnson_neyman(
    model = m_int,
    pred = !!rlang::sym(i),
    modx = conservatism_7pt_merged)
  
  vline_low <- ifelse(ros$bounds[1] >= 1 & ros$bounds[1] <= 7,ros$bounds[1],NA)
  vline_high <- ifelse(ros$bounds[2] >= 1 & ros$bounds[2] <= 7,ros$bounds[2],NA)
  
  # create plot
  p <- ggplot(sslop, aes(x = x, y = predicted, color = group)) +
    geom_line(size = 1.6) +
    scale_color_manual(
      name = "ID Gruppen",
      values = c("deeppink3", "deepskyblue4")
    ) +
    labs(
      title = paste("Simple Slopes:", i),
      x = "Conservatism",
      y = "Bias threshold"
    ) +
    scale_x_continuous(breaks = seq(0, 7, by = 1), limits = c(0, 7)) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  if(!is.na(vline_low[[1]])) {
    p <- p + geom_vline(xintercept = vline_low[[1]])
  } else {
    p
  }
  
  if(!is.na(vline_high[[1]])) {
    p <- p + geom_vline(xintercept = vline_high[[1]])
  } else {
    p
  }
  
  # save plot
  plot_list[[i]] <- p
}

# grid plot
wrap_plots(plot_list, ncol = 2)

#* Other Conservtism Scales as Robustness check -----

# 2 conservatism measures in study 2
moderators_stdy2 <-
  c("conservatism_7pt_merged",
    "conservatism_slider_merged")


# create empty data frame
fig4_mod_long <- data.frame(
  target1_contrast = character(),
  target2_contrast = character(),
  moderator        = character(),
  interaction_b    = numeric(),
  interaction_p    = numeric(),
  jn_low_raw       = numeric(),
  jn_upp_raw       = numeric(),
  stringsAsFactors = FALSE
)


# loop across targets and conservatism measures
for (pred in predictors_stdy2) {
  for (mod in moderators_stdy2) {
    
    # define and run model
    formula_int <- as.formula(paste("bias_threshold ~", pred, "*", mod))
    m_int <- lm(formula_int, data = d201)
    
    # get targets
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod)
    )
    
    # write results in a row
    row <- data.frame(
      target1_contrast = target1,
      target2_contrast = target2,
      moderator        = mod,
      interaction_b    = coefficients(m_int)[[4]],
      interaction_p    = summary(m_int)$coefficients[4,4],
      jn_low_raw       = ros$bounds[[1]],
      jn_upp_raw       = ros$bounds[[2]],
      stringsAsFactors = FALSE
    )
    
    # add row to data frame
    fig4_mod_long <- rbind(fig4_mod_long, row)
  }
}

# print results
print(fig4_mod_long)

#* Other Conservtism Scales (standardized) as Robustness check -----

d201 <- 
  d201 %>%
  mutate(across(all_of(moderators_stdy2), ~as.numeric(scale(.)),.names = "{.col}_scl"))

moderators_scl_stdy2 <-
  paste0(moderators_stdy2,"_scl")


fig4_mod_long_scl <- 
  data.frame(
    target1_contrast = character(),
    target2_contrast = character(),
    moderator        = character(),
    interaction_b_scl= numeric(),
    interaction_p_scl= numeric(),
    jn_low_scl       = numeric(),
    jn_upp_scl       = numeric(),
    stringsAsFactors = FALSE
  )


# loop across targets and conservatism measures
for (pred in predictors_stdy2) {
  for (mod_scl in moderators_scl_stdy2) {
    
    # define and run model
    formula_int_scl <- as.formula(paste("bias_threshold ~", pred, "*", mod_scl))
    m_int_scl <- lm(formula_int_scl, data = d201)
    
    # get targets
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int_scl,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod_scl)
    )
    
    # new row with results
    row <- data.frame(
      target1_contrast = target1,
      target2_contrast = target2,
      moderator        = mod_scl,
      interaction_b_scl= coefficients(m_int_scl)[[4]],
      interaction_p_scl= summary(m_int_scl)$coefficients[4,4],
      jn_low_scl       = ros$bounds[[1]],
      jn_upp_scl       = ros$bounds[[2]],
      stringsAsFactors = FALSE
    )
    
    # add row to data frame
    fig4_mod_long_scl <- rbind(fig4_mod_long_scl, row)
  }
}

# print results
print(fig4_mod_long_scl)

stdy2_all_scales <- 
  fig4_mod_long_scl %>%
  mutate(moderator = stringr::str_remove_all(moderator,"_scl")) %>%
  full_join(fig4_mod_long,.)

# write a readable html table with standardized and unstandardized moderators
stdy2_all_scales %>%
  mutate(across(where(is.numeric),round,4)) %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Moderator Scale",
                      "Beta","p","Lower","Upper",
                      "Beta","p","Lower","Upper")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2," "= 1,
                     "Interaction" = 2,"Johnson-Neyman" = 2,
                     "Interaction" = 2,"Johnson-Neyman" = 2)) %>%
  add_header_above(c(" " = 3,"Unstandardized Moderator" = 4,
                     "Standardized Moderator" = 4))


#* Bootstrap JN-Bounds as Robustness check ####

results_stdy2_boot_list <- list()

for (pred in predictors_stdy2) {
  cat("\n====== Predictor:", pred, "======\n")
  
  formula <- as.formula(paste("bias_threshold ~", pred, "* conservatism_7pt_merged"))
  model_orig <- lm(formula, data = d201)
  
  jn_orig <- tryCatch(
    johnson_neyman(model_orig,
                   pred = !!rlang::sym(pred),
                   modx = "conservatism_7pt_merged",
                   plot = FALSE),
    error = function(e) return(NULL)
  )
  
  if (is.null(jn_orig) || is.null(jn_orig$bounds)) {
    cat("J-N analysis failed for predictor:", pred, "\n")
    next
  }
  
  jn_bounds_orig <- jn_orig$bounds
  orig_lower <- jn_bounds_orig[1]
  orig_upper <- jn_bounds_orig[length(jn_bounds_orig)]
  cat("Original J-N bounds:", round(orig_lower, 3), "to", round(orig_upper, 3), "\n")
  
  boot_jn_fn <- function(data, indices) {
    d <- data[indices, ]
    model <- tryCatch(
      lm(formula, data = d),
      error = function(e) return(c(NA, NA))
    )
    jn <- tryCatch(
      johnson_neyman(model,
                     pred = !!rlang::sym(pred),
                     modx = "conservatism_7pt_merged",
                     alpha = 0.05,
                     plot = FALSE),
      error = function(e) return(c(NA, NA))
    )
    if (is.list(jn)) {
      bounds <- jn$bounds
      return(c(bounds[1], bounds[length(bounds)]))
    } else {
      return(c(NA, NA))
    }
  }
  
  set.seed(123)
  boot_jn <- boot(data = d201, statistic = boot_jn_fn, R = 1000)
  boot_bounds <- boot_jn$t
  boot_bounds <- boot_bounds[complete.cases(boot_bounds), ]
  
  if (nrow(boot_bounds) == 0) {
    cat("No valid bootstrap samples for predictor:", pred, "\n")
    next
  }
  
  ci_lower <- quantile(boot_bounds[, 1], probs = c(0.025, 0.975), na.rm = TRUE)
  ci_upper <- quantile(boot_bounds[, 2], probs = c(0.025, 0.975), na.rm = TRUE)
  
  cat("Bootstrapped 95% CI for lower bound:", round(ci_lower[1], 3), "to", round(ci_lower[2], 3), "\n")
  cat("Bootstrapped 95% CI for upper bound:", round(ci_upper[1], 3), "to", round(ci_upper[2], 3), "\n")
  
  results_stdy2_boot_list[[pred]] <- data.frame(
    predictor = pred,
    orig_lower = orig_lower,
    orig_upper = orig_upper,
    ci_lower_2.5 = ci_lower[1],
    ci_lower_97.5 = ci_lower[2],
    ci_upper_2.5 = ci_upper[1],
    ci_upper_97.5 = ci_upper[2],
    valid_bootstraps = nrow(boot_bounds)
  )
}

# Combine all rows into one data frame
results_stdy2_df <- do.call(rbind, results_stdy2_boot_list)

# Print or view the final results
print(results_stdy2_df)


results_stdy2_df %>%
  mutate(across(where(is.numeric),round,4),
         predictor = stringr::str_replace_all(predictor,"_"," "),
         predictor = stringr::str_remove_all(predictor,"0 | 1"),
         predictor = stringr::str_to_title(predictor)) %>%
  select(predictor,orig_lower,ci_lower_2.5,ci_lower_97.5,
         orig_upper,ci_upper_2.5, ci_upper_97.5, valid_bootstraps) %>%
  kable("html", escape = FALSE,row.names = F,
        col.names = c("Contrast",
                      "Lower JN-Bound","Lower","Upper",
                      "Upper JN-Bound","Lower","Upper","N Valid Bootstraps")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c(" " = 1,
                     "Original" = 1,
                     "Bootstraped 95% CI"= 2,
                     "Original" = 1,
                     "Bootstraped 95% CI"= 2,
                     " " = 1))


#--------------------------------------------#
#                                            #
####             Meta-Analysis            ####
#                                            #
#--------------------------------------------#

#* Grouped Un-weighted Correlations for Participants who passed / didn't pass inclusion criteria ####
d003 %>%
  group_by(bias_target,passed_prereg_or_similar_criteria) %>%
  summarise(n = n(),
            mean = mean(bias_threshold,na.rm = T),
            sd = sd(bias_threshold,na.rm = T),
            median = median(bias_threshold,na.rm = T),
            r = cor.test(conservatism_7pt_merged, bias_threshold)$estimate,
            ci_low = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[1],
            ci_upp = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[2],
            p = cor.test(conservatism_7pt_merged, bias_threshold)$p.value) %>%
  mutate(across(where(is.numeric),round,4)) %>%
  rename("target" = "bias_target","passed prereg or similar criteria"="passed_prereg_or_similar_criteria") %>%
  kable("html", escape = FALSE) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  column_spec(2,width = "10em")


#*random subgroups from whole sample ####

set.seed(123)

# Parameter
n_iter <- 1000
sample_frac <- 0.5

# Subsample-Correlationen
compute_subsample_corrs <- function(df, n_iter, sample_frac) {
  replicate(n_iter, {
    subsample <- df %>% sample_frac(sample_frac, replace = FALSE)
    cor(subsample$conservatism_7pt_merged, subsample$bias_threshold, use = "complete.obs")
  })
}

# get distribution ob correlation coeffs across subsamples 
cor_distributions <- d003 %>%
  group_by(bias_target) %>%
  group_modify(~ {
    corrs <- compute_subsample_corrs(.x, n_iter, sample_frac)
    tibble(r = corrs)
  }) %>%
  ungroup()

# Raw values
originals <- d003 %>%
  group_by(bias_target) %>%
  summarise(
    r_original = cor(conservatism_7pt_merged, bias_threshold, use = "complete.obs"),
    .groups = "drop"
  )

# Combine for plot
cor_distributions_plot <- cor_distributions %>%
  left_join(originals, by = "bias_target")


ggplot(cor_distributions_plot, aes(x = r)) +
  geom_density(fill = "steelblue", alpha = 0.6) +
  geom_vline(aes(xintercept = r_original), color = "red", linetype = "dashed", linewidth = 0.8) +
  facet_wrap(~ bias_target) +
  labs(title = "Robustheit der Korrelationen in Subsamples",
       x = "Korrelationskoeffizient", y = "Dichte") +
  theme_minimal()


cor_distributions_plot  %>%
  group_by(bias_target) %>%
  reframe(r_original = r_original,
      r_mean = mean(r, na.rm = TRUE),
      r_sd = sd(r, na.rm = TRUE),
      r_ci_low = quantile(r, 0.025, na.rm = TRUE),
      r_ci_upp = quantile(r, 0.975, na.rm = TRUE)
    ) %>%
  distinct()



#--------------------------------------------#
#                                            #
####     Meta-Analysis - Forest Plot      ####
#                                            #
#--------------------------------------------#


forest_data <- 
  d003 %>%
  filter(passed_prereg_or_similar_criteria == "Yes") %>%
  group_by(bias_target, study_id_for_paper) %>%
  summarise(
    n = n(),
    r = cor.test(conservatism_7pt_merged, bias_threshold)$estimate,
    ci_low = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[1],
    ci_upp = cor.test(conservatism_7pt_merged, bias_threshold)$conf.int[2],
    .groups = "drop"
  ) %>%
  group_by(bias_target) %>%
  mutate(mean_r = mean(r,na.rm = T)) %>%
  ungroup() %>%
  arrange(mean_r, r) %>%
  mutate(row_order = row_number())

# Plot
ggplot(forest_data, aes(x = r, y = -row_order, color = bias_target)) +
  geom_point(aes(size = n), shape = 15) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_upp), height = 0.4) +
  scale_size(range = c(1, 4)) +
  scale_y_continuous(breaks = NULL) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    x = "Correlation (r)",
    y = NULL,
    size = "n",
    color = "Target"
  ) +
  scale_color_paletteer_d("MoMAColors::Doughton") +
  theme_minimal()

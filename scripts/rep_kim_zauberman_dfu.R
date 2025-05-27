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

start_kim()


#--------------------------------------------#
#                                            #
####             Load data                ####
#                                            #
#--------------------------------------------#

# Please set the the path to your repository !!!

setwd("/Users/david/Box Sync/R/rep_kim_zaubermann/scripts/")

# Use the original authors' object names
d001 <- read_csv("bias and ideology data v010", na.strings = "")

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
            Q20 = "#fee08b",
            Q30 = "#e6f598",
            Q40 = "#abdda4",
            Q50 = "#66c2a5",
            Q60 = "#fdae61",
            Q70 = "#f46d43",
            Q80 = "#d53e4f",
            Q90 = "#9e0142")

### Functions ####

# Set up a function for simple linear models
fun_mod_lin <- function(ds, dv, iv, targets) {
  
  # Create an empty list for the output
  mods <- list()
  
  # Loop over targets
  for (target in targets) {
    
    # Set up formular
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

# Set up a function for quantile regressions (no interaction)
fun_mod_qr_ni <- function(ds, dv, iv, targets, taus) {
  
  # Create an empty list to store models
  mods <- list()
  
  # Loop over targets
  for (target in targets) {
    
    # Set up formula using the provided dv (dependent variable) and iv (independent variable)
    formula <- as.formula(paste(dv, "~", iv))
    
    # Subset the data for the current target
    filtered_ds <- ds |> 
      subset(bias_target == target)
    
    # Create an empty list to store models for each tau
    tau_models <- list()
    
    # Loop over taus
    for (tau in taus) {
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

# Set up a function for plotting a linear model with accompanying quantile models
fun_plot_lm_rq <- function(ds, y_var, x_var, colors = NULL) {
  
  ds |> 
    ggplot2::ggplot(aes(x = {{x_var}}, 
                        y = {{y_var}})) +
    geom_smooth(method = "lm", se = FALSE, aes(color = "OLS"),
                linewidth = 1.5) +
    
    geom_quantile(quantiles = 0.1, aes(color = "Q10"), alpha = 0.8) +
    geom_quantile(quantiles = 0.2, aes(color = "Q20"), alpha = 0.8) +
    geom_quantile(quantiles = 0.3, aes(color = "Q30"), alpha = 0.8) +
    geom_quantile(quantiles = 0.4, aes(color = "Q40"), alpha = 0.8) +
    geom_quantile(quantiles = 0.5, aes(color = "Q50"), alpha = 0.8) +
    geom_quantile(quantiles = 0.6, aes(color = "Q60"), alpha = 0.8) +
    geom_quantile(quantiles = 0.7, aes(color = "Q70"), alpha = 0.8) +
    geom_quantile(quantiles = 0.8, aes(color = "Q80"), alpha = 0.8) +
    geom_quantile(quantiles = 0.9, aes(color = "Q90"), alpha = 0.8) +

    scale_color_manual(values = colors) +
    
    theme_bw() +
    theme(legend.title=element_blank()) 
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
  
  for (iv in ivs) {
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
  
  for (iv in ivs) {
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


#--------------------------------------------#
#                                            #
####        Study 1 - Correlations        ####
#                                            #
#--------------------------------------------#

##* Use the same data set as K & Z ####
d101 <- d002[study_id_for_paper == "01"]



# Set parameters for the function (fun_mod_lin)

my_iv = "conservatism_7pt_merged"

my_targets = c("women",
               "men",
               "whites",
               "blacks",
               "unknown")

my_dv = "bias_threshold"

# Set table spanners
my_tb_sp <- c("OLS", "QR 10%", "QR 30%", "QR 50", "QR 70%", "QR90")

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
                         seq(from = 0.1,
                             to = 0.9,
                             by = 0.1))


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
                                 sim_qrs$women$t_0.2,
                                 sim_qrs$women$t_0.5,
                                 sim_qrs$women$t_0.7,
                                 sim_qrs$women$t_0.9) #|> plot()

#  Create a table summary for ols, q10, q30, q50, q70, q90 
tbl_merge(
  tbls = list(
    tbl_regression(sim_lms$women) |> 
      bold_p(),
    
    tbl_regression(sim_qrs$women$t_0.1) |> 
      bold_p(),
    
    tbl_regression(sim_qrs$women$t_0.3) |> 
      bold_p(),
    
    tbl_regression(sim_qrs$women$t_0.5) |> 
      bold_p(),
    
    tbl_regression(sim_qrs$women$t_0.7) |> 
      bold_p(),
    
    tbl_regression(sim_qrs$women$t_0.9) |> 
      bold_p()
  ),
  
  tab_spanner = my_tb_sp
)



##* Men as targets ####

#**  Create the data set ####
d_01_men <- d101 |> 
  filter(bias_target == "men")


#** Check model assumptions ####

s1_t1_t_men <- lm(bias_threshold ~ conservatism_7pt_merged,
                  d_01_men)

summary(s1_t1_t_men)

d_01_men |> 
  ggplot(aes(x = conservatism_7pt_merged)) +
  geom_density()

d_01_men |> 
  ggplot(aes(x = bias_threshold)) +
  geom_density()

performance::check_outliers(s1_t1_t_men)
performance::check_normality(s1_t1_t_men)
performance::check_heteroscedasticity(s1_t1_t_men)

#**  Run quantile regression as robustness check ####

d_01_men |> 
  ggplot(aes(x = conservatism_7pt_merged,
             y = bias_threshold)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_quantile(quantiles = seq(0.1, 0.9, by = 0.1)) 



s1_t1_t_men_qr <- rq(bias_threshold ~ conservatism_7pt_merged,
                     tau = seq(from = 0.1, 
                               to = 0.9, 
                               by = 0.1),
                     data = d_01_men)


summary(s1_t1_t_men_qr) %>% 
  plot(parm = "conservatism_7pt_merged")


s1_t1_t_men_qr10 <- rq(bias_threshold ~ conservatism_7pt_merged,
                       tau = 0.1,
                       data = d_01_men)

s1_t1_t_men_qr30 <- rq(bias_threshold ~ conservatism_7pt_merged,
                       tau = 0.3,
                       data = d_01_men)

s1_t1_t_men_qr50 <- rq(bias_threshold ~ conservatism_7pt_merged,
                       tau = 0.5,
                       data = d_01_men)

s1_t1_t_men_qr70 <- rq(bias_threshold ~ conservatism_7pt_merged,
                       tau = 0.7,
                       data = d_01_men)

s1_t1_t_men_qr90 <- rq(bias_threshold ~ conservatism_7pt_merged,
                       tau = 0.9,
                       data = d_01_men)





#**  Create a table summary for ols, q10, q30, q50, q70, q90 ####
tbl_merge(
  tbls = list(
    tbl_regression(s1_t1_t_men) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_men_qr10) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_men_qr30) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_men_qr50) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_men_qr70) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_men_qr90) |> 
      bold_p()
  ),
  
  tab_spanner = c("OLS", "QR 10%", "QR 30%", "QR 50", "QR 70%", "QR90")
)


#**  Compare Model fits ####
performance::compare_performance(s1_t1_t_men,
                                 s1_t1_t_men_qr10,
                                 s1_t1_t_men_qr30,
                                 s1_t1_t_men_qr50,
                                 s1_t1_t_men_qr90,
                                 s1_t1_t_men_qr90) |> plot()


##* Whites as targets ####

#**Create the data set ####
d_01_whites <- d101 |> 
  filter(bias_target == "whites")


#** Check model assumptions ####

s1_t1_t_whites <- lm(bias_threshold ~ conservatism_7pt_merged,
                     d_01_whites)

summary(s1_t1_t_whites)

d_01_whites |> 
  ggplot(aes(x = conservatism_7pt_merged)) +
  geom_density()

d_01_whites |> 
  ggplot(aes(x = bias_threshold)) +
  geom_density()

performance::check_outliers(s1_t1_t_whites)
performance::check_normality(s1_t1_t_whites)
performance::check_heteroscedasticity(s1_t1_t_whites)

#** Run quantile regression as robustness check ####

d_01_whites |> 
  ggplot(aes(x = conservatism_7pt_merged,
             y = bias_threshold)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_quantile(quantiles = seq(0.1, 0.9, by = 0.1)) 



s1_t1_t_whites_qr <- rq(bias_threshold ~ conservatism_7pt_merged,
                        tau = seq(from = 0.1, 
                                  to = 0.9, 
                                  by = 0.1),
                        data = d_01_whites)

summary(s1_t1_t_whites_qr) %>% 
  plot(parm = "conservatism_7pt_merged")


s1_t1_t_whites_qr10 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.1,
                          data = d_01_whites)

s1_t1_t_whites_qr30 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.3,
                          data = d_01_whites)

s1_t1_t_whites_qr50 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.5,
                          data = d_01_whites)

s1_t1_t_whites_qr70 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.7,
                          data = d_01_whites)

s1_t1_t_whites_qr90 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.9,
                          data = d_01_whites)

#** Create a table summary for ols, q10, q30, q50, q70, q90 ####
tbl_merge(
  tbls = list(
    tbl_regression(s1_t1_t_whites) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_whites_qr10) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_whites_qr30) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_whites_qr50) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_whites_qr70) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_whites_qr90) |> 
      bold_p()
  ),
  
  tab_spanner = c("OLS", "QR 10%", "QR 30%", "QR 50", "QR 70%", "QR90")
)

#** Compare Model fits ####
performance::compare_performance(s1_t1_t_whites,
                                 s1_t1_t_whites_qr10,
                                 s1_t1_t_whites_qr30,
                                 s1_t1_t_whites_qr50,
                                 s1_t1_t_whites_qr90,
                                 s1_t1_t_whites_qr90) |> plot()


##* Blacks as targets ####

#** Create the data set ####
d_01_blacks <- d101 |> 
  filter(bias_target == "blacks")


#** Check model assumptions ####

s1_t1_t_blacks <- lm(bias_threshold ~ conservatism_7pt_merged,
                     d_01_blacks)

summary(s1_t1_t_blacks)

d_01_blacks |> 
  ggplot(aes(x = conservatism_7pt_merged)) +
  geom_density()

d_01_blacks |> 
  ggplot(aes(x = bias_threshold)) +
  geom_density()

performance::check_outliers(s1_t1_t_blacks)
performance::check_normality(s1_t1_t_blacks)
performance::check_heteroscedasticity(s1_t1_t_blacks)

#** Run quantile regression as robustness check ####

d_01_blacks |> 
  ggplot(aes(x = conservatism_7pt_merged,
             y = bias_threshold)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_quantile(quantiles = seq(0.1, 0.9, by = 0.1)) 



s1_t1_t_blacks_qr <- rq(bias_threshold ~ conservatism_7pt_merged,
                        tau = seq(from = 0.1, 
                                  to = 0.9, 
                                  by = 0.1),
                        data = d_01_blacks)

summary(s1_t1_t_blacks_qr) %>% 
  plot(parm = "conservatism_7pt_merged")


s1_t1_t_blacks_qr10 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.1,
                          data = d_01_blacks)

s1_t1_t_blacks_qr30 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.3,
                          data = d_01_blacks)

s1_t1_t_blacks_qr50 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.5,
                          data = d_01_blacks)

s1_t1_t_blacks_qr70 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.7,
                          data = d_01_blacks)

s1_t1_t_blacks_qr90 <- rq(bias_threshold ~ conservatism_7pt_merged,
                          tau = 0.9,
                          data = d_01_blacks)


#**  Create a table summary for ols, q10, q30, q50, q70, q90 ####
tbl_merge(
  tbls = list(
    tbl_regression(s1_t1_t_blacks) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_blacks_qr10) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_blacks_qr30) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_blacks_qr50) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_blacks_qr70) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_blacks_qr90) |> 
      bold_p()
  ),
  
  tab_spanner = c("OLS", "QR 10%", "QR 30%", "QR 50", "QR 70%", "QR90")
)



#** Compare Model fits ####
performance::compare_performance(s1_t1_t_blacks,
                                 s1_t1_t_blacks_qr10,
                                 s1_t1_t_blacks_qr30,
                                 s1_t1_t_blacks_qr50,
                                 s1_t1_t_blacks_qr90,
                                 s1_t1_t_blacks_qr90) |> plot()


##* Unknown as targets ####

###* Create the data set ####
d_01_unknown<- d101 |> 
  filter(bias_target == "unknown")


#**Check model assumptions ####

s1_t1_t_unknown<- lm(bias_threshold ~ conservatism_7pt_merged,
                     d_01_unknown)

summary(s1_t1_t_unknown)

d_01_unknown|> 
  ggplot(aes(x = conservatism_7pt_merged)) +
  geom_density()

d_01_unknown|> 
  ggplot(aes(x = bias_threshold)) +
  geom_density()

performance::check_outliers(s1_t1_t_unknown)
performance::check_normality(s1_t1_t_unknown)
performance::check_heteroscedasticity(s1_t1_t_unknown)

#** Run quantile regression as robustness check ####

d_01_unknown|> 
  ggplot(aes(x = conservatism_7pt_merged,
             y = bias_threshold)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_quantile(quantiles = seq(0.1, 0.9, by = 0.1)) 



s1_t1_t_unknown_qr <- rq(bias_threshold ~ conservatism_7pt_merged,
                         tau = seq(from = 0.1, 
                                   to = 0.9, 
                                   by = 0.1),
                         data = d_01_unknown)

summary(s1_t1_t_unknown_qr) %>% 
  plot(parm = "conservatism_7pt_merged")


s1_t1_t_unknown_qr10 <- rq(bias_threshold ~ conservatism_7pt_merged,
                           tau = 0.1,
                           data = d_01_unknown)

s1_t1_t_unknown_qr30 <- rq(bias_threshold ~ conservatism_7pt_merged,
                           tau = 0.3,
                           data = d_01_unknown)

s1_t1_t_unknown_qr50 <- rq(bias_threshold ~ conservatism_7pt_merged,
                           tau = 0.5,
                           data = d_01_unknown)

s1_t1_t_unknown_qr70 <- rq(bias_threshold ~ conservatism_7pt_merged,
                           tau = 0.7,
                           data = d_01_unknown)

s1_t1_t_unknown_qr90 <- rq(bias_threshold ~ conservatism_7pt_merged,
                           tau = 0.9,
                           data = d_01_unknown)


#**  Create a table summary for ols, q10, q30, q50, q70, q90 ####
tbl_merge(
  tbls = list(
    tbl_regression(s1_t1_t_unknown) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_unknown_qr10) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_unknown_qr30) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_unknown_qr50) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_unknown_qr70) |> 
      bold_p(),
    
    tbl_regression(s1_t1_t_unknown_qr90) |> 
      bold_p()
  ),
  
  tab_spanner = c("OLS", "QR 10%", "QR 30%", "QR 50", "QR 70%", "QR90")
)


#** Compare Model fits ####
performance::compare_performance(s1_t1_t_unknown,
                                 s1_t1_t_unknown_qr10,
                                 s1_t1_t_unknown_qr30,
                                 s1_t1_t_unknown_qr50,
                                 s1_t1_t_unknown_qr90,
                                 s1_t1_t_unknown_qr90) |> plot()



#--------------------------------------------#
#                                            #
####        Study 1 - Regressions         ####
#                                            #
#--------------------------------------------#

#* Women vs Men ####

## Simple regression ## 
wm_lm <- lm(bias_threshold ~ bias_target_men_vs_women * conservatism_7pt_merged,
            d101)

summary(wm_lm)

# Check Assumptions
performance::check_model(wm_lm)

performance::check_outliers(wm_lm)
performance::check_normality(wm_lm)
performance::check_heteroscedasticity(wm_lm)

## Quantile regression ##

# Run quantile regression from 0.1 to 0.9 quantile
wm_qr <- rq(bias_threshold ~ bias_target_men_vs_women * conservatism_7pt_merged,
            tau = seq(from = 0.1, 
                      to = 0.9, 
                      by = 0.1),
            data = d101)

summary(wm_qr,
        se = "boot") 


wm_qr10 <- rq(bias_threshold ~ bias_target_men_vs_women * conservatism_7pt_merged,
              tau = 0.1,
              data = d101)

wm_qr30 <- rq(bias_threshold ~ bias_target_men_vs_women * conservatism_7pt_merged,
              tau = 0.3,
              data = d101)

wm_qr50 <- rq(bias_threshold ~ bias_target_men_vs_women * conservatism_7pt_merged,
              tau = 0.5,
              data = d101)

wm_qr70 <- rq(bias_threshold ~ bias_target_men_vs_women * conservatism_7pt_merged,
              tau = 0.7,
              data = d101)

wm_qr90 <- rq(bias_threshold ~ bias_target_men_vs_women * conservatism_7pt_merged,
              tau = 0.9,
              data = d101)


#**  Create a table summary for ols, q10, q30, q50, q70, q90 ####
tbl_merge(
  tbls = list(
    tbl_regression(wm_lm) |> 
      bold_p(),
    
    tbl_regression(wm_qr10) |> 
      bold_p(),
    
    tbl_regression(wm_qr30) |> 
      bold_p(),
    
    tbl_regression(wm_qr50) |> 
      bold_p(),
    
    tbl_regression(wm_qr70) |> 
      bold_p(),
    
    tbl_regression(wm_qr90) |> 
      bold_p()
  ),
  
  tab_spanner = c("OLS", "QR 10%", "QR 30%", "QR 50", "QR 70%", "QR90")
)


#** Compare Model fits ####
performance::compare_performance(wm_lm,
                                 wm_qr10,
                                 wm_qr30,
                                 wm_qr50,
                                 wm_qr90,
                                 wm_qr90) 


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


#* Women vs Men ####

emm_models$bias_target_men_vs_women

emm_models$bias_target_men_vs_women$contrasts[599]

emm_model_plots$bias_target_men_vs_women + 
  geom_vline(xintercept = 4.79,
             alpha = .7) 


#* Whites vs. Blacks ####

emm_models$bias_target_whites_vs_blacks

emm_model_plots$bias_target_whites_vs_blacks + 
  geom_vline(xintercept = 3.42,
             alpha = .7) +
  geom_vline(xintercept = 4.92,
             alpha = .7)

#* Men vs. Unknown ####

emm_models$bias_target_men_vs_unknown

emm_model_plots$bias_target_men_vs_unknown + 
  geom_vline(xintercept = 3.93,
             alpha = .7) 


#* Women vs. Unknown ####

emm_models$bias_target_women_vs_unknown

emm_model_plots$bias_target_women_vs_unknown + 
  geom_vline(xintercept = 4.03,
             alpha = .7) 

#* Whites vs. Unknown ####

emm_models$bias_target_whites_vs_unknown

emm_model_plots$bias_target_whites_vs_unknown + 
  geom_vline(xintercept = 3.62,
             alpha = .7) 

#* Blacks vs. Unknown ####

emm_models$bias_target_blacks_vs_unknown

emm_model_plots$bias_target_blacks_vs_unknown + 
  geom_vline(xintercept = 4.83,
             alpha = .7) 

#--------------------------------------------#
#                                            #
####        Study 2 - Regressions         ####
#                                            #
#--------------------------------------------#

##* Use the same data set as K & Z ####
d201 <- d002[study_id_for_paper == "02"]

###########################################################################
#
# R code to accompany the paper below.
# Written by Jin Kim (jin.m.kim@yale.edu)
# Please email Jin Kim if you notice any errors or have any questions.
# Last updated on Jul 29, 2023
#
# Kim, J. & Zauberman, G. (2023).
# When Does Unequal Representation Reflect Bias?
# The Role of Political Ideology in Judgments of Distributional Outcomes
# (Under Review, 2nd Round)
# 
###########################################################################

# Install Package 'kim' (v0.5.357) ----------------------------------------
# The package's version must be higher than v0.5.357
if (require(kim) == FALSE) install.packages("kim")

# Update Package 'kim' ----------------------------------------------------
kim::update_kim(force = FALSE)

# Install all dependencies (recommended but may not be necessary) ---------
# kim::install_all_dependencies()

# Attach and start using the Package 'kim' --------------------------------
# The code below will clear the console and global environment ------------
library(kim); start_kim()

# Install or attach required packages -------------------------------------
prep(gridExtra, grid, metafor)

# Read data ---------------------------------------------------------------
d001 <- read_csv("bias and ideology data v013", na.strings = "")

# Edit data ---------------------------------------------------------------
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


###########################################################################
#
# Study 1 ("s1") ----------------------------------------------------------
#
###########################################################################

# Subset of data for s1 ---------------------------------------------------
# Only the ps who passed preregistered exclusion criteria -----------------
d101 <- d002[study_id_for_paper == "01"]

# Sample size -------------------------------------------------------------
d101[, .N] # N = 1,108

###########################################################################
# Table 1, top panel ------------------------------------------------------
###########################################################################

# Descriptive statistics for bias threshold  ------------------------------
# Column 1: sample size by condition --------------------------------------
tv(d101[, bias_target])
# Columns 2-4: mean, SD, and median by condition --------------------------
desc_stats(d101[bias_target == "men", bias_threshold], sigfigs = 2)
desc_stats(d101[bias_target == "women", bias_threshold], sigfigs = 2)
desc_stats(d101[bias_target == "whites", bias_threshold], sigfigs = 2)
desc_stats(d101[bias_target == "blacks", bias_threshold], sigfigs = 2)
desc_stats(d101[bias_target == "unknown", bias_threshold], sigfigs = 2)
# Column 5: correlation by condition --------------------------------------
correlation_kim(
  data = d101[bias_target == "men"],
  x_var_name = "conservatism_7pt_merged",
  y_var_name = "bias_threshold")
correlation_kim(
  data = d101[bias_target == "women"],
  x_var_name = "conservatism_7pt_merged",
  y_var_name = "bias_threshold")
correlation_kim(
  data = d101[bias_target == "whites"],
  x_var_name = "conservatism_7pt_merged",
  y_var_name = "bias_threshold")
correlation_kim(
  data = d101[bias_target == "blacks"],
  x_var_name = "conservatism_7pt_merged",
  y_var_name = "bias_threshold")
correlation_kim(
  data = d101[bias_target == "unknown"],
  x_var_name = "conservatism_7pt_merged",
  y_var_name = "bias_threshold")

###########################################################################
# Table 1, bottom panel ---------------------------------------------------
###########################################################################

# Nested model test -------------------------------------------------------
names(d101)
iv_names <- c(
  "bias_target_men_vs_women",
  "bias_target_whites_vs_blacks",
  "bias_target_men_vs_unknown",
  "bias_target_women_vs_unknown",
  "bias_target_whites_vs_unknown",
  "bias_target_blacks_vs_unknown")
int_effect_size_results <- do.call(rbind, lapply(iv_names, function(x) {
  temp_iv <- x
  temp_dt <- d101[
    !is.na(get(temp_iv)) & !is.na(bias_threshold) &
      !is.na(conservatism_7pt_merged)]
  lm_1 <- lm(formula = as.formula(paste0(
    "bias_threshold ~ conservatism_7pt_merged + ",
    temp_iv)),
    data = temp_dt)
  lm_2 <- lm(formula = as.formula(paste0(
    "bias_threshold ~ conservatism_7pt_merged * ",
    temp_iv)),
    data = temp_dt)
  nested_model_anova <- anova(lm_1, lm_2)
  # elements for the table
  num_of_levels_in_iv <- 2
  lm_2_df_residual <- stats::df.residual(lm_2)
  lm_2_r_squared <- summary(lm_2)$r.squared
  lm_1_r_squared <- summary(lm_1)$r.squared
  r_squared_increase <- lm_2_r_squared - lm_1_r_squared
  f_stat_for_model_fit_diff <- lm_2_df_residual *
    (lm_2_r_squared - lm_1_r_squared) /
    ((num_of_levels_in_iv - 1) * (1 - lm_2_r_squared))
  p_for_f_stat_for_model_fit_diff <- stats::pf(
    q = f_stat_for_model_fit_diff,
    df1 = num_of_levels_in_iv - 1,
    df2 = lm_2_df_residual,
    lower.tail = FALSE)
  output <- data.table(
    iv = temp_iv,
    lm_1_r_squared, lm_2_r_squared,
    r_squared_increase, 
    f_stat_for_model_fit_diff,
    # nested_model_f = nested_model_anova[2, "F"],
    p_for_f_stat_for_model_fit_diff
    # nested_model_p = nested_model_anova[2, "Pr(>F)"]
  )
  return(output)
}))
int_effect_size_results

# save as csv -------------------------------------------------------------
write_csv(int_effect_size_results, "int effect size s01 v01")

###########################################################################
# Fig. 2 ------------------------------------------------------------------
###########################################################################

# Correlation estimates appear on the final plot output -------------------
# bias targets
su(d101[, bias_target])
s1_bias_target <- c("women", "men", "blacks", "whites", "unknown")
s1_bias_target_label <- c("Women", "Men", "Blacks", "Whites", "Unknown")
color <- rep("black", 5)
# create correlation plots
correlation_plots <- lapply(seq_along(s1_bias_target), function(i) {
  g1 <- scatterplot(
    data = d101[bias_target == s1_bias_target[i]],
    x_var_name = "conservatism_7pt_merged",
    y_var_name = "bias_threshold",
    jitter_x_percent = 3,
    jitter_y_percent = 3,
    annotate_y_pos = 15,
    annotated_stats_color = color[i],
    annotated_stats_font_size = 10,
    dot_color = color[i],
    line_of_fit_color = color[i],
    dot_size = 5,
    alpha = 0.3)
  g1 <- g1 + ggtitle(s1_bias_target_label[i])
  g1 <- g1 + scale_x_continuous(breaks = 1:7)
  g1 <- g1 + theme_kim(cap_axis_lines = TRUE)
  g1 <- g1 + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(
      size = 24, face = "bold", color = color[i]),
    plot.margin = unit(c(1, 0, 2, 1), "cm"))
  return(g1)
})
# save as svg -------------------------------------------------------------
svg(filename = "fig 2 s1 correlation 5 panels v001.svg",
    height = 22, width = 18)
corr_plots_grid_layout <- rbind(
  c(1, 1, 2, 2),
  c(3, 3, 4, 4),
  c(NA, 5, 5, NA))
grid.arrange(
  grobs = correlation_plots,
  layout_matrix = corr_plots_grid_layout,
  bottom = textGrob(paste0(
    "Political Ideology\n",
    "(1 = Extremely Liberal", "; 4 = Moderate; ",
    "7 = Extremely Conservative)"),
    gp = gpar(fontsize = 32, fontface = "bold")),
  left = textGrob(
    paste0("Bias\nThreshold", paste0(rep("\n", 12), collapse = "")),
    gp = gpar(fontsize = 32, fontface = "bold"),
    rot = 0))
dev.off()

###########################################################################
# Study 1, interaction and floodlight analyses ----------------------------
###########################################################################

# Subset of ps for men vs women -------------------------------------------
d101[bias_target %in% c("men", "women"), .N] # 456

# Interaction for men vs women --------------------------------------------
multiple_regression(
  data = d101,
  formula = bias_threshold ~ bias_target_men_vs_women *
    conservatism_7pt_merged)

# Floodlight for men vs women ---------------------------------------------
# For a refined plot, see the code in the next section
floodlight_2_by_continuous(
  data = d101,
  iv_name = "bias_target_men_vs_women",
  dv_name = "bias_threshold",
  mod_name = "conservatism_7pt_merged",
  jitter_x_percent = 2,
  jitter_y_percent = 2,
  dot_alpha = 0.3,
  legend_title = "Bias Target",
  interaction_p_value_font_size = 8,
  jn_point_font_size = 8)

###########################################################################
# Fig. 3 ------------------------------------------------------------------
###########################################################################

# Fig 3: all 6 floodlight analyses ---------------------------------------
s1_floodlight_panel_label <- capitalize(letters[1:6])
s1_binary_vars <- c(
  "bias_target_men_vs_women",
  "bias_target_whites_vs_blacks",
  "bias_target_men_vs_unknown",
  "bias_target_women_vs_unknown",
  "bias_target_whites_vs_unknown",
  "bias_target_blacks_vs_unknown")
floodlight_analysis_plots <- lapply(
  seq_along(s1_binary_vars), function(i) {
    g1 <- floodlight_2_by_continuous(
      data = d101,
      iv_name = s1_binary_vars[i],
      dv_name = "bias_threshold",
      mod_name = "conservatism_7pt_merged",
      jitter_x_percent = 2,
      jitter_y_percent = 2,
      dot_alpha = 0.3,
      legend_title = "Bias Target",
      interaction_p_value_font_size = 8,
      jn_point_font_size = 8,
      line_of_fit_thickness = 1.5,
      dot_size = 5)
    # set colors of dots for the floodlight plot
    two_targets <- su(d101[, get(s1_binary_vars[i])], na.last = NA)
    dot_colors <- vapply(two_targets, function(target) {
      color <- fcase(
        target %in% c("Men", "Whites"), "blue",
        target %in% c("Women", "Blacks"), "red",
        target == "Unknown", "gray30")
      return(color)
    }, character(1L))
    g1 <- g1 + scale_color_manual(values = dot_colors)
    g1 <- g1 + annotation_custom(
      grob = textGrob(
        label = s1_floodlight_panel_label[i],
        hjust = 0.5, vjust = 0.5,
        gp = gpar(fontsize = 32, fontface = "bold")),
      ymin = 114,
      ymax = 114,
      xmin = -1,
      xmax = -1)
    g1 <- g1 + annotation_custom(
      grob = rectGrob(
        height = 1, width = 1, gp = gpar(lwd = 3, fill = NA),
        hjust = 0.5, vjust = 0.5),
      ymin = 109,
      ymax = 119,
      xmin = -1.55,
      xmax = -0.45)
    g1 <- g1 + scale_x_continuous(breaks = 1:7)
    g1 <- g1 + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(3.2, 0.5, 2, 0.5), "cm"),
      legend.key.height = unit(3, "line"),
      legend.key.size = unit(3, "line"))
    return(g1)
  })
# save as svg -------------------------------------------------------------
svg(filename = "fig 3 s1 floodlight 6 panels v001.svg",
    height = 20, width = 18)
grid.arrange(
  grobs = floodlight_analysis_plots, ncol = 2,
  bottom = textGrob(paste0(
    "Political Ideology\n",
    "(1 = Extremely Liberal", "; 4 = Moderate; ",
    "7 = Extremely Conservative)"),
    gp = gpar(fontsize = 32, fontface = "bold")),
  left = textGrob(
    label = "Bias\nThreshold",
    vjust = 0.02,
    gp = gpar(fontsize = 32, fontface = "bold"),
    rot = 0))
dev.off()

# Subset of ps for whites vs blacks ---------------------------------------
d101[bias_target %in% c("whites", "blacks"), .N] # 444

# Interaction for whites vs blacks ----------------------------------------
multiple_regression(
  data = d101,
  formula = bias_threshold ~ bias_target_whites_vs_blacks *
    conservatism_7pt_merged)

# Floodlight for whites vs blacks -----------------------------------------
# for a refined plot, see the code in the section above, titled
# "Fig 3: all 6 floodlight analyses"
floodlight_2_by_continuous(
  data = d101,
  iv_name = "bias_target_whites_vs_blacks",
  dv_name = "bias_threshold",
  mod_name = "conservatism_7pt_merged",
  jitter_x_percent = 2,
  jitter_y_percent = 2,
  dot_alpha = 0.3,
  legend_title = "Bias Target",
  interaction_p_value_font_size = 8,
  jn_point_font_size = 8)

# Known targets versus unknown target -------------------------------------
# binary target variables
s1_known_vs_unknown_target_vars <- c(
  "bias_target_men_vs_unknown",
  "bias_target_women_vs_unknown",
  "bias_target_whites_vs_unknown",
  "bias_target_blacks_vs_unknown")
# Interaction analyses: see the results summary in the console ------------
lapply(seq_along(s1_known_vs_unknown_target_vars), function(i) {
  multiple_regression(
    data = d101,
    formula = as.formula(paste0(
      "bias_threshold ~ ", s1_known_vs_unknown_target_vars[i],
      " * conservatism_7pt_merged")))
})
# Floodlight analysis: see the results in the plot output viewer ----------
lapply(seq_along(s1_known_vs_unknown_target_vars), function(i) {
  floodlight_2_by_continuous(
    data = d101,
    iv_name = s1_known_vs_unknown_target_vars[i],
    dv_name = "bias_threshold",
    mod_name = "conservatism_7pt_merged",
    jitter_x_percent = 2,
    jitter_y_percent = 2,
    dot_alpha = 0.3,
    legend_title = "Bias Target",
    interaction_p_value_font_size = 8,
    jn_point_font_size = 8)
})

###########################################################################
#
# Study 2 -----------------------------------------------------------------
#
###########################################################################

# Subset of data for s2 ---------------------------------------------------
d201 <- d002[study_id_for_paper == "02"]
# Sample size -------------------------------------------------------------
d201[, .N] # N = 394

# Correlation -------------------------------------------------------------
# correlation by target
d201[, .(r_and_p = und(
  corr_text, x = .SD[, conservatism_7pt_merged],
  y = .SD[, bias_threshold])), keyby = bias_target]

###########################################################################
# Fig. 4 ------------------------------------------------------------------
###########################################################################

# Fig 4: all 3 floodlight analyses ---------------------------------------
s2_floodlight_panel_label <- capitalize(letters[1:3])
s2_binary_vars <- c(
  "bias_target_liberals_vs_conservatives",
  "bias_target_liberals_vs_unknown",
  "bias_target_conservatives_vs_unknown")
floodlight_analysis_plots <- lapply(
  seq_along(s2_binary_vars), function(i) {
    if (i == 3) {
      jn_point_label_hjust_temp <- c(1, 0)
    } else {
      jn_point_label_hjust_temp <- NULL
    }
    g1 <- floodlight_2_by_continuous(
      data = d201,
      iv_name = s2_binary_vars[i],
      dv_name = "bias_threshold",
      mod_name = "conservatism_7pt_merged",
      jitter_x_percent = 2,
      jitter_y_percent = 2,
      dot_alpha = 0.3,
      legend_title = "Bias Target",
      interaction_p_value_font_size = 8,
      jn_point_font_size = 8,
      jn_point_label_hjust = jn_point_label_hjust_temp,
      line_of_fit_thickness = 1.5,
      dot_size = 5)
    # set colors of dots for the floodlight plot
    two_targets <- su(d201[, get(s2_binary_vars[i])], na.last = NA)
    dot_colors <- vapply(two_targets, function(target) {
      color <- fcase(
        target == "Liberal\nTarget", "chocolate4",
        target == "Conserv.\nTarget", "purple",
        target == "Unknown\nTarget", "gray30")
      return(color)
    }, character(1L))
    g1 <- g1 + scale_color_manual(values = dot_colors)
    g1 <- g1 + annotation_custom(
      grob = textGrob(
        label = s2_floodlight_panel_label[i],
        hjust = 0.5, vjust = 0.5,
        gp = gpar(fontsize = 32, fontface = "bold")),
      ymin = 113,
      ymax = 113,
      xmin = -0.7,
      xmax = -0.7)
    g1 <- g1 + annotation_custom(
      grob = rectGrob(
        height = 1, width = 1, gp = gpar(lwd = 3, fill = NA),
        hjust = 0.5, vjust = 0.5),
      ymin = 108,
      ymax = 118,
      xmin = -1.2,
      xmax = -0.2)
    g1 <- g1 + scale_x_continuous(breaks = 1:7)
    g1 <- g1 + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.margin = unit(c(3.2, 0.5, 2, 0.5), "cm"),
      legend.key.height = unit(5, "line"),
      legend.key.size = unit(3, "line"))
    return(g1)
  })
# save as svg
svg(filename = "fig 4 s2 floodlight 3 panels v001.svg",
    height = 14, width = 19)
grid.arrange(
  grobs = floodlight_analysis_plots, 
  layout_matrix = rbind(
    c(NA, 1, 1, NA),
    c(2, 2, 3, 3)),
  bottom = textGrob(paste0(
    "Political Ideology\n",
    "(1 = Extremely Liberal", "; 4 = Moderate; ",
    "7 = Extremely Conservative)"),
    gp = gpar(fontsize = 32, fontface = "bold")),
  left = textGrob(
    label = "Bias\nThreshold", 
    vjust = -3.5,
    gp = gpar(fontsize = 32, fontface = "bold"),
    rot = 0))
dev.off()

# Interaction analyses: see the results summary in the console ------------
lapply(seq_along(s2_binary_vars), function(i) {
  multiple_regression(
    data = d201,
    formula = as.formula(paste0(
      "bias_threshold ~ ", s2_binary_vars[i],
      " * conservatism_7pt_merged")))
})

# Floodlight analyses: see the results in the plot output viewer ----------
lapply(seq_along(s2_binary_vars), function(i) {
  floodlight_2_by_continuous(
    data = d201,
    iv_name = s2_binary_vars[i],
    dv_name = "bias_threshold",
    mod_name = "conservatism_7pt_merged",
    jitter_x_percent = 2,
    jitter_y_percent = 2,
    dot_alpha = 0.3,
    legend_title = "Bias Target",
    interaction_p_value_font_size = 8,
    jn_point_font_size = 8)
})


###########################################################################
#
# Results across studies --------------------------------------------------
#
###########################################################################

# Take a subset of data for the 26 studies, but include -------------------
# all participants including those who did not pass the
# preregistered or similar exclusion criteria
d003 <- d001[entity %in% c(
  "Employees", "Police Brutality Victims", 
  "University Students", "Monkeys") & 
    !is.na(study_id_for_paper) &
    !is.na(bias_threshold) &
    completed_survey == "Yes" &
    (!is.na(conservatism_7pt_merged) | 
       !is.na(conservatism_slider_merged))]

# Count of the studies in this subset -------------------------------------
tv(d003[!is.na(study_id_for_paper), study_id_for_paper]) # 26

# Total sample size across the 26 studies ---------------------------------
d003[, .N] # N = 17,011

# Sample size the subset of ps who passed preregistered or similar --------
# exclusion criteria
d003[passed_prereg_or_similar_criteria == "Yes", .N]

# Correlation by condition and study --------------------------------------
corr_dt <- d003[
  , .(n = .N,
      r = cor.test(bias_threshold, conservatism_7pt_merged)$estimate,
      p = cor.test(bias_threshold, conservatism_7pt_merged)$p.value,
      r_ci_95_ll = cor.test(
        bias_threshold, conservatism_7pt_merged)$conf.int[1],
      r_ci_95_ul = cor.test(
        bias_threshold, conservatism_7pt_merged)$conf.int[2]),  
  keyby = c(
    "study_id_for_paper", "condition_name_detailed", "bias_target",
    "target_nondom_vs_dom_vs_unknown")]
corr_dt[, pretty_r := sub("^(-?)0.", "\\1.", sprintf("%.2f", r))]
corr_dt[, r_95_ci := paste0(
  "[", sub("^(-?)0.", "\\1.", sprintf("%.2f", r_ci_95_ll)),  ", ", 
  sub("^(-?)0.", "\\1.", sprintf("%.2f", r_ci_95_ul)), "]")]
corr_dt[, pretty_p := pretty_round_p_value(p)]
corr_dt
# Confirm total n
sum(corr_dt$n) # should be 17011
# write to csv
# write_csv(corr_dt, "all ps meta analysis all correlations v12")

# Count of all estimated correlations -------------------------------------
nrow(corr_dt) # 96

###########################################################################
# Table 2 -----------------------------------------------------------------
###########################################################################

# Weighted mean correlation table by target type --------------------------
target_type <- c("Nondominant", "Unknown", "Dominant")
temp_01 <- lapply(seq_along(target_type), function(i) {
  temp <- corr_dt[target_nondom_vs_dom_vs_unknown == target_type[i]]
  target <- target_type[i]
  weighted_mean_r_result <- weighted_mean_r(temp$r, temp$n)
  if (length(weighted_mean_r_result) == 1 & 
      is.numeric(weighted_mean_r_result)) {
    r <- temp$r
    p <- temp$p
    ci_95_ll <- temp$r
    ci_95_ul <- temp$r
  } else {
    r <- weighted_mean_r_result[["weighted_mean_r"]]
    p <- weighted_mean_r_result[["p_value"]]
    ci_95_ll <- weighted_mean_r_result[["ci_ll"]]
    ci_95_ul <- weighted_mean_r_result[["ci_ul"]]
  }
  k <- nrow(temp)
  n <- sum(temp$n)
  output <- data.table(
    target, r, ci_95_ll, ci_95_ul, p, 
    number_of_conditions = k, n = n)
  return(output)
})
t01 <- do.call(rbind, temp_01)
# Weighted mean correlation table by target ---------------------------
target <- su(corr_dt[, bias_target])
# Weighted mean by target
temp_02 <- lapply(seq_along(target), function(i) {
  temp <- corr_dt[bias_target == target[i]]
  temp_target <- target[i]
  if (nrow(temp) == 1) {
    r <- temp$r
    p <- temp$p
    ci_95_ll <- temp$r_ci_95_ll
    ci_95_ul <- temp$r_ci_95_ul
  } else {
    weighted_mean_r_result <- weighted_mean_r(temp$r, temp$n)
    r <- weighted_mean_r_result[["weighted_mean_r"]]
    p <- weighted_mean_r_result[["p_value"]]
    ci_95_ll <- weighted_mean_r_result[["ci_ll"]]
    ci_95_ul <- weighted_mean_r_result[["ci_ul"]]
  }
  k <- nrow(temp)
  n <- sum(temp$n)
  output <- data.table(
    target = temp_target, 
    r, ci_95_ll, ci_95_ul, p, 
    number_of_conditions = k, n = n)
  return(output)
})
t02 <- do.call(rbind, temp_02)
setorder(t02, -r)

# Construct a table of weighted mean correlation --------------------------
# sort rows for the target section of the table
table_1_new_row_order <- c(
  setdiff(t02[, target], c("liberals", "conservatives")),
  c("liberals", "conservatives"))
t02 <- order_rows_specifically_in_dt(
  t02, "target", table_1_new_row_order)
# combine the sections
t03 <- rbind(t01, t02)
# clean table
t04 <- copy(t03)
t04[, p := pretty_round_p_value(p)]
t04[, weighted_mean_ci := paste0(
  "[", pretty_round_r(ci_95_ll, 2), ", ", 
  pretty_round_r(ci_95_ul, 2), "]")]
t04[, r := pretty_round_r(r)]
t04[, target := capitalize(target)]
t04[, r_and_ci := paste0(r, " ", weighted_mean_ci)]
t05 <- t04[, c("target", "r_and_ci", "p", 
                 "number_of_conditions", "n")]
t05
# write to csv
# write_csv(t05, "table 2 in main text v001")

# Confirm total n
# Sum all n except for the first 3 rows
t05
sum(tail(t05[, n], -3))

###########################################################################
# Fig. 5 ------------------------------------------------------------------
###########################################################################

# Sort targets in ascending order the correlations ------------------------
t06 <- copy(t03)
t06[, target := tolower(target)]
t06 <- unique(t06)
setorder(t06, r)
t06
t06[, target]

# Sort the 96 correlations by target and correlation ----------------------
temp_03 <- copy(corr_dt)
temp_03[, bias_target_factor := factor(
  bias_target, levels = t06[, target])]
setorder(temp_03, bias_target_factor, r)
temp_03

# Forest plot -------------------------------------------------------------
temp_03[, order_for_forest_plot := (nrow(temp_03):1) + 14]
# Data table for plotting the diamonds in the forest plot -----------------
t07 <- t06[target %in% c(
  "dominant", "irrelevant", "unknown", "nondominant")]
setorder(t07, r)
draw_diamonds_dt <- data.table(target = tolower(c(
  as.character(unique(temp_03[, bias_target_factor])),
  t07[, target])))
for (i in seq_len(nrow(draw_diamonds_dt))) {
  set(draw_diamonds_dt, i = i, j = "r", value = t06[
    target == draw_diamonds_dt[i, target], r])
  set(draw_diamonds_dt, i = i, j = "r_ci_95_ll", value = t06[
    target == draw_diamonds_dt[i, target], ci_95_ll])
  set(draw_diamonds_dt, i = i, j = "r_ci_95_ul", value = t06[
    target == draw_diamonds_dt[i, target], ci_95_ul])
}
number_of_diamonds <- nrow(draw_diamonds_dt)
diamond_y_coordinate <- rev(seq(
  -number_of_diamonds + 1, number_of_diamonds - 1, 2))
draw_diamonds_dt[, diamond_y2 := diamond_y_coordinate + 0.7]
draw_diamonds_dt[, diamond_y1 := diamond_y_coordinate - 0.7]
draw_diamonds_dt[, diamond_y_coordinate := diamond_y_coordinate]
draw_diamonds_dt[, diamond_color := c(
  "red",
       "cyan4",
       "darkorange",
       "gold3",
       "gray50",
       "gray30",
       "blue",
       "coral",
       "green3",
       "magenta",
       "purple",
       "gray50",
       "gray30",
       "seagreen"
)]
# Begin plotting
g1 <- ggplot(
  data = temp_03,
  aes(x = r, y = order_for_forest_plot))
g1 <- g1 + xlab(
  "Correlation Between Political Ideology (Conservatism) and Bias Threshold")
# Vertical lines
for (x in c(-0.4, -0.2, 0.2, 0.4)) {
  g1 <- g1 + geom_vline(
    xintercept = x, linetype = "dotted",
    size = 0.32)
}
g1 <- g1 + geom_vline(
  xintercept = 0, linetype = "dotted", size = 1)
# Add error bars
g1 <- g1 + geom_errorbarh(
  aes(color = bias_target_factor),
  xmin = temp_03$r_ci_95_ll,
  height = 0,
  xmax = temp_03$r_ci_95_ul,
  size = 1.5)
# Add correlations from individual conditions
g1 <- g1 + geom_point(data = temp_03, shape = 15, aes(
  size = n, color = bias_target_factor))
g1 <- g1 + scale_size(range = c(3, 7))
g1 <- g1 + scale_color_manual(
  values = c(draw_diamonds_dt[, diamond_color]))
# Draw diamonds
g2 <- g1
for (i in seq_len(number_of_diamonds)) {
  temp_dt <- data.table(
    x = c(
      draw_diamonds_dt[i, r_ci_95_ll],
      draw_diamonds_dt[i, r],
      draw_diamonds_dt[i, r_ci_95_ul],
      draw_diamonds_dt[i, r]),
    y = c(
      draw_diamonds_dt[i, diamond_y_coordinate],
      draw_diamonds_dt[i, diamond_y2],
      draw_diamonds_dt[i, diamond_y_coordinate],
      draw_diamonds_dt[i, diamond_y1]))
  g2 <- g2 + geom_polygon(
    data = temp_dt, aes(x = x, y = y),
    fill = draw_diamonds_dt[i, diamond_color])
}
# Add labels for target groups
temp_04 <- temp_03[, .(
  target_label_x_coordinate = max(r_ci_95_ul) + 0.05), by = bias_target]
temp_04[, label_color := vlookup(
  bias_target, unique(draw_diamonds_dt[, c(
    "target", "diamond_color"), with = FALSE]), 
  "target", "diamond_color")]
# Manually edit the label x coordinates
temp_04[bias_target == "whites", target_label_x_coordinate := 0.05]
temp_04[bias_target == "men", target_label_x_coordinate := 0.1]
temp_04[bias_target == "unknown", target_label_x_coordinate := 0.35]
temp_04[bias_target == "women", target_label_x_coordinate := 0.44]
temp_04[bias_target == "blacks", target_label_x_coordinate := 0.46]
# Y coordinate of the label by target -------------------------------------
temp_05 <- temp_03[, .(target_label_y_coordinate = mean(
  order_for_forest_plot)), by = bias_target_factor]
temp_06 <- temp_04[temp_05, on = .(bias_target = bias_target_factor)]
# Place the labels
for (i in seq_len(nrow(temp_06))) {
  g2 <- g2 + annotate(
    geom = "text",
    x = temp_06[i, target_label_x_coordinate],
    y = temp_06[i, target_label_y_coordinate],
    label = capitalize(temp_06[i, bias_target]), 
    color = temp_06[i, label_color],
    fontface = "bold",
    hjust = 0,
    size = 7)
}
# Add labels for diamonds
for (i in seq_along(draw_diamonds_dt[, diamond_y_coordinate])) {
  g2 <- g2 + annotate(
    geom = "text",
    x = draw_diamonds_dt[i, r_ci_95_ul] + 0.02,
    y = draw_diamonds_dt[i, diamond_y_coordinate],
    label = capitalize(draw_diamonds_dt[i, target]), 
    color = draw_diamonds_dt[i, diamond_color],
    fontface = "bold",
    hjust = 0,
    size = 7)
}
# Add section dividers
g2 <- g2 + geom_segment(x = -0.6, y = 14, xend = 0.6, yend = 14)
g2 <- g2 + geom_segment(x = -0.6, y = -6, xend = 0.6, yend = -6)
# Further edit the plot
g2 <- g2 + scale_x_continuous(
  limits = c(-0.7, 0.7),
  breaks = seq(-0.6, 0.6, 0.2),
  labels = round(seq(-0.6, 0.6, 0.2), 1))
g2 <- g2 + scale_y_continuous(
  limits = c(
    min(draw_diamonds_dt[, diamond_y1]) - 0.5, 
    max(temp_03[, order_for_forest_plot])), 
  expand = c(0.01, 0))
# Apply theme manually
base_size <- 20
axis_tick_font_size <- 20
axis_title_font_size <- 24
y_axis_title_vjust <- 0.85
axis_title_margin_size <- 24
legend_position <- "none"
g3 <- g2 + theme_classic(base_size = base_size) + ggplot2::theme(
  plot.title = ggplot2::element_text(hjust = 0.5),
  legend.position = legend_position,
  axis.title.x = ggplot2::element_text(margin = ggplot2::margin(
    t = axis_title_margin_size)),
  axis.title = ggplot2::element_text(
    face = "bold", color = "black", size = axis_title_font_size),
  axis.text = ggplot2::element_text(
    face = "bold", color= "black", size = axis_tick_font_size),
  legend.title = ggplot2::element_text(
    face = "bold", color = "black", size = axis_title_font_size),
  legend.text = ggplot2::element_text(
    face = "bold", color= "black", size = axis_tick_font_size)
)
g3 <- g3 + theme(
  axis.title.y = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.line.y = element_blank())
g3 <- g3 + lemon::coord_capped_cart(bottom = "both")
g3
ggsave_quick("fig 5 forest plot v001", width = 13, height = 22)

###########################################################################
# Political ideology by bias target interaction: dominant, ----------------
# nondominant, and unknown
###########################################################################

# The code for these analyses is presented in the code for ----------------
# SI, Section 3


###########################################################################
# Fig. 6 ------------------------------------------------------------------
###########################################################################

# Use the subset with all participants' data (N = 17011), but -------------
# exclude bias threshold values less than 50 ------------------------------
d003[, .N] # N = 17,011
d004 <- d003[bias_threshold >= 50]
d004[, .N] # N = 16,830

# Sample sizes for the notes to be included in the figure -----------------
d004[!is.na(target_nondom_vs_dom) & !is.na(bias_threshold) &
       !is.na(conservatism_7pt_merged), .N]
d004[!is.na(target_dom_vs_unknown) & !is.na(bias_threshold) &
       !is.na(conservatism_7pt_merged), .N]
d004[!is.na(target_nondom_vs_unknown) & !is.na(bias_threshold) &
       !is.na(conservatism_7pt_merged), .N]

# Floodlight analyses -----------------------------------------------------
# Check the values in the three binary variables --------------------------
names(d004)
tv(d004[, target_nondom_vs_dom])
tv(d004[, target_dom_vs_unknown])
tv(d004[, target_nondom_vs_unknown])
# Convert the binary variables to factors ---------------------------------
d004[, target_nondom_vs_dom := factor(
  target_nondom_vs_dom, 
  levels = c("Dominant", "Nondominant"))]
d004[, target_dom_vs_unknown := factor(
  target_dom_vs_unknown, 
  levels = c("Dominant", "Unknown"))]
d004[, target_nondom_vs_unknown := factor(
  target_nondom_vs_unknown, 
  levels = c("Nondominant", "Unknown"))]
# Parameters for plotting -------------------------------------------------
ivs <- c(
  "target_nondom_vs_dom",
  "target_dom_vs_unknown",
  "target_nondom_vs_unknown")
iv_colors <- list(
  c("blue", "red"),
  c("blue", "gray30"),
  c("red", "gray30"))
floodlight_plots <- vector("list", 3L)
# Begin plotting ----------------------------------------------------------
for (i in seq_along(ivs)) {
  g1 <- NULL
  g1 <- floodlight_2_by_continuous(
    data = d004,
    iv_name = ivs[i],
    dv_name = "bias_threshold",
    mod_name = "conservatism_7pt_merged",
    jitter_x_percent = 3, 
    jitter_y_percent = 3,
    dot_alpha = 0.2,
    dot_size = 3.5,
    x_axis_title = FALSE,
    y_axis_title = FALSE,
    legend_title = FALSE,
    jn_point_label_hjust = c(1, 0),
    jn_point_font_size = 8,
    line_of_fit_thickness = 3)
  g1 <- g1 + scale_x_continuous(
    breaks = 1:7)
  g1 <- g1 + scale_y_continuous(
    breaks = c(50, 75, 100))
  g1 <- g1 + scale_color_manual(values = iv_colors[[i]])
  g1 <- g1 + theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank())
  floodlight_plots[[i]] <- g1
}

# Put the plots together --------------------------------------------------
# Save as png -------------------------------------------------------------
floodlight_plots_final <- arrangeGrob(
  grobs = floodlight_plots, 
  layout_matrix = rbind(
    c(NA, 1, 1, NA),
    c(2, 2, 3, 3)),
  bottom = textGrob(paste0(
    "Political Ideology\n",
    "(1 = Extremely Liberal", "; 4 = Moderate; ",
    "7 = Extremely Conservative)"),
    gp = gpar(fontsize = 32, fontface = "bold")),
  left = textGrob(
    label = "Bias\nThreshold", 
    vjust = -3.5,
    gp = gpar(fontsize = 32, fontface = "bold"),
    rot = 0),
  padding = unit(2, "line"))
ggsave(
  filename = paste0(
    "fig 6 floodlight 3 panels v001.png"), 
  plot = floodlight_plots_final,
  height = 14, width = 22, dpi = 200)


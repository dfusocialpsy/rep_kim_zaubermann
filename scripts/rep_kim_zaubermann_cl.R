library(kim); start_kim()

# Install or attach required packages -------------------------------------
prep(gridExtra, grid, metafor)

# Read data ---------------------------------------------------------------
setwd("C:/Users/Clemens Lindner/Documents/github/rep_kim_zaubermann/data")
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


# TABLE 1 ----
library(dplyr)
d101 %>%
  filter(bias_target == "men") %>%
  rstatix::cor_test(conservatism_7pt_merged,bias_threshold)


# FLOODLIGHT 1 ####
m1 <- lm(bias_threshold ~ bias_target_men_0_vs_women_1*conservatism_7pt_merged, data = d101)

summary(m1)

m1_flood <- floodlight_2_by_continuous(dv_name = "bias_threshold", 
                                       mod_name = "conservatism_7pt_merged",
                                       iv_name = "bias_target_men_0_vs_women_1", 
                                       data = d101)

jn_result <- interactions::johnson_neyman(model = m1, 
                                          pred = conservatism_7pt_merged, 
                                          modx = bias_target_men_0_vs_women_1)


library(boot)

boot_interaction <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(bias_threshold ~ bias_target_men_0_vs_women_1 * conservatism_7pt_merged, data = d)
  return(coef(model)["bias_target_men_0_vs_women_1:conservatism_7pt_merged"])
}

# 2. Run the bootstrap
set.seed(123)
boot_result <- boot(data = d101, statistic = boot_interaction, R = 1000)

# 3. View results
print(boot_result)
boot.ci(boot_result, type = "perc")

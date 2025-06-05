library(interactions)
library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(kableExtra)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(patchwork)
library(paletteer)

## function ----

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


# GET DATA -----
setwd("C:/Users/Clemens Lindner/Documents/github/rep_kim_zaubermann/data")
d001 <- read.csv("bias and ideology data v013.csv", na.strings = "")

d001 <-
  data.table::as.data.table(d001)

# convert the binary bias target variables to factors
d001 <- d001 %>% 
  mutate(across(c(bias_target_men_vs_women,
                  bias_target_whites_vs_blacks,
                  bias_target_men_vs_unknown,
                  bias_target_women_vs_unknown,
                  bias_target_whites_vs_unknown,
                  bias_target_blacks_vs_unknown,
                  bias_target_liberals_vs_conservatives,
                  bias_target_liberals_vs_unknown,
                  bias_target_conservatives_vs_unknown), as.factor))

## Subset data for main text ------
d002 <- d001[entity %in% c(
  "Employees", "Police Brutality Victims", 
  "University Students", "Monkeys") & 
    !is.na(study_id_for_paper) &
    !is.na(bias_threshold) &
    completed_survey == "Yes" &
    (!is.na(conservatism_7pt_merged) | 
       !is.na(conservatism_slider_merged)) &
    passed_prereg_or_similar_criteria == "Yes"]

janitor::tabyl(d001$entity)


## Count of studies -----
janitor::tabyl(d002$study_id_for_paper) %>%
  janitor::adorn_totals() %>%
  tibble::as_tibble() %>%
  print(n = 30)

# STUDY 1 ------

# Subset of data for s1
# Only the ps who passed preregistered exclusion criteria
d101 <- d002[study_id_for_paper == "01"]

# Sample size
d101[, .N] # N = 1,108

## Robustness Check 1: Own Script ----

### TABLE 1 ----

#### Descriptives & Correlations -----

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


#### Regressions ----

predictors <- c(
  "bias_target_men_0_vs_women_1",
  "bias_target_whites_0_vs_blacks_1",
  "bias_target_men_0_vs_unknown_1",
  "bias_target_women_0_vs_unknown_1",
  "bias_target_whites_0_vs_unknown_1",
  "bias_target_blacks_0_vs_unknown_1"
)

# Spaltennamen für Ergebnis-DataFrame
models_stats <- c(
  "target1_contrast",   # e.g. "men"
  "target2_contrast",   # e.g. "women"
  "moderator",          # e.g. "conservatism_7pt_merged"
  "rsq_no_int",         # R² ohne Interaktion
  "rsq_int",            # R² mit Interaktion
  "rsq_chnge",          # R²-Differenz
  "f",                  # F-Wert aus Modellvergleich
  "p_comp"              # p-Wert aus Modellvergleich
)

# Leerer Ergebnis-DataFrame
tbl1_mod <- data.frame(matrix(nrow = 0, ncol = length(models_stats)))
colnames(tbl1_mod) <- models_stats

# Schleife über alle Prädiktoren
for (i in predictors) {
  
  # Modelle definieren
  formula_no_int <- as.formula(paste("bias_threshold ~", i, "+ conservatism_7pt_merged"))
  formula_int    <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  
  # Modelle schätzen
  m_no_int <- lm(formula_no_int, data = d101)
  m_int    <- lm(formula_int, data = d101)
  
  # Modellvergleiche (ANOVA)
  model_comp <- anova(m_no_int, m_int)
  
  # Kontrast extrahieren
  parts <- strsplit(i, "_0_vs_")[[1]]
  target1 <- gsub("bias_target_", "", parts[1])
  target2 <- gsub("_1$", "", parts[2])
  
  # Zeile für Ergebnis-DataFrame erstellen
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
  
  # Zeile an DataFrame anhängen
  tbl1_mod <- rbind(tbl1_mod, row)
}

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

tbl1_regressions %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Moderator", rep(c("Reported", "Reproduced"), 
                                                             times = length(reg_stats)))) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2,
                     " " = 1, 
                     setNames(rep(2, length(reg_stats)), 
                                                      c("R2 No Interaction","R2 Interaction","R2 Change","F","p"))))

tbl1_reg_colored <- colorize_comparison_table(as.data.frame(tbl1_regressions), reg_stats)

tbl1_reg_colored %>%
  kable("html", escape = FALSE,
      col.names = c("Target 1","Target 2","Moderator", rep(c("Reported", "Reproduced"), times = length(reg_stats)))) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2," " = 1, setNames(rep(2, length(reg_stats)), reg_stats)))


### FIGURE 3 -----

#### ggpredict & johnson-neyman -----

plot_list <- list()

for (i in predictors) {
  
  # Formel
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  
  # Modell schätzen
  m_int <- lm(formula_int, data = d101)
  
  # Vorhersagen
  sslop <- ggpredict(m_int, terms = c("conservatism_7pt_merged", i))
  
  ros <- interactions::johnson_neyman(
    model = m_int,
    pred = !!rlang::sym(i),
    modx = conservatism_7pt_merged)
  
  vline_low <- ifelse(ros$bounds[1] >= 1 & ros$bounds[1] <= 7,ros$bounds[1],NA)
  vline_high <- ifelse(ros$bounds[2] >= 1 & ros$bounds[2] <= 7,ros$bounds[2],NA)
  
  # Plot erstellen
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
  
  # Plot speichern
  plot_list[[i]] <- p
}

# Plots in Grid anzeigen (z.B. 2 Spalten)
wrap_plots(plot_list, ncol = 2)

#### Interactions: Johnson-Neyman Bounds ----

fig3_mod <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(fig3_mod) <- c("target1_contrast", "target2_contrast", "jn_low_raw", "jn_upp_raw")

for (i in predictors) {
  
  # Formel und Modell
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  m_int       <- lm(formula_int, data = d101)
  
  # Kontrast extrahieren
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

# Ergebnis

fig3_mod %>%
  kable("html", escape = FALSE,
      col.names = c("Target 1","Target 2","Lower Bound","Upper Bound")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2,"Johnson-Neyman" = 2))

## Robustness Check 2: Other conservatism scales ----

library(rlang)

# Predictor contrasts
predictors <- c(
  "bias_target_men_0_vs_women_1",
  "bias_target_whites_0_vs_blacks_1",
  "bias_target_men_0_vs_unknown_1",
  "bias_target_women_0_vs_unknown_1",
  "bias_target_whites_0_vs_unknown_1",
  "bias_target_blacks_0_vs_unknown_1"
)

# Verschiedene Konservatismus-Moderatoren
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


# Doppelte Schleife über alle Kombinationen
for (pred in predictors) {
  for (mod in moderators) {
    
    # Formel und Modell
    formula_int <- as.formula(paste("bias_threshold ~", pred, "*", mod))
    m_int <- lm(formula_int, data = d101)
    
    # Kontrast extrahieren
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod)
    )
    
    # Neue Zeile für Ergebnis-Tabelle
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
    
    # Zeile anhängen
    fig3_mod_long <- rbind(fig3_mod_long, row)
  }
}

# Ergebnis prüfen
print(fig3_mod_long)


d101 <- 
  d101 %>%
  mutate(across(moderators, ~as.numeric(scale(.)),.names = "{.col}_scl"))

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


# Doppelte Schleife über alle Kombinationen
for (pred in predictors) {
  for (mod_scl in moderators_scl) {
    
    # Formel und Modell
    formula_int_scl <- as.formula(paste("bias_threshold ~", pred, "*", mod_scl))
    m_int_scl <- lm(formula_int_scl, data = d101)
    
    # Kontrast extrahieren
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int_scl,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod_scl)
    )
    
    # Neue Zeile für Ergebnis-Tabelle
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
    
    # Zeile anhängen
    fig3_mod_long_scl <- rbind(fig3_mod_long_scl, row)
  }
}

# Ergebnis prüfen
print(fig3_mod_long_scl)

stdy1_all_scales <- 
  fig3_mod_long_scl %>%
  mutate(moderator = stringr::str_remove_all(moderator,"_scl")) %>%
  full_join(fig3_mod_long,.) #%>%
  #arrange(moderator)

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

## Robustness Check 3: Bootstrap JN-Bounds ----


results_list <- list()

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

# STUDY 2 ----

d201 <- d002[study_id_for_paper == "02"]

## Robustness CHeck 1: Own script -----

#### Correlations -----

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


#### Interactions ----

predictors_stdy2 <-
  c("bias_target_liberals_0_vs_conservatives_1",
    "bias_target_liberals_0_vs_unknown_1",
    "bias_target_conservatives_0_vs_unknown_1")

fig4_mod <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(fig4_mod) <- c("target1_contrast", "target2_contrast", "jn_low_raw", "jn_upp_raw")

for (i in predictors_stdy2) {
  
  # Formel und Modell
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  m_int       <- lm(formula_int, data = d201)
  
  # Kontrast extrahieren
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
  fig4_mod <- rbind(fig4_mod, row)
}

# Ergebnis

fig4_mod %>%
  kable("html", escape = FALSE,
        col.names = c("Target 1","Target 2","Lower Bound","Upper Bound")) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("condensed", "striped")) %>%
  add_header_above(c("Contrast" = 2,"Johnson-Neyman" = 2))

### FIGURE 4 ----

plot_list <- list()

for (i in predictors_stdy2) {
  
  # Formel
  formula_int <- as.formula(paste("bias_threshold ~", i, "* conservatism_7pt_merged"))
  
  # Modell schätzen
  m_int <- lm(formula_int, data = d201)
  
  # Vorhersagen
  sslop <- ggpredict(m_int, terms = c("conservatism_7pt_merged", i))
  
  ros <- interactions::johnson_neyman(
    model = m_int,
    pred = !!rlang::sym(i),
    modx = conservatism_7pt_merged)
  
  vline_low <- ifelse(ros$bounds[1] >= 1 & ros$bounds[1] <= 7,ros$bounds[1],NA)
  vline_high <- ifelse(ros$bounds[2] >= 1 & ros$bounds[2] <= 7,ros$bounds[2],NA)
  
  # Plot erstellen
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
  
  # Plot speichern
  plot_list[[i]] <- p
}

# Plots in Grid anzeigen (z.B. 2 Spalten)
wrap_plots(plot_list, ncol = 2)


## Robustness Check 2: Other Conservtism Scales -----

moderators_stdy2 <-
  c("conservatism_7pt_merged",
    "conservatism_slider_merged")


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


# Doppelte Schleife über alle Kombinationen
for (pred in predictors_stdy2) {
  for (mod in moderators_stdy2) {
    
    # Formel und Modell
    formula_int <- as.formula(paste("bias_threshold ~", pred, "*", mod))
    m_int <- lm(formula_int, data = d201)
    
    # Kontrast extrahieren
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod)
    )
    
    # Neue Zeile für Ergebnis-Tabelle
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
    
    # Zeile anhängen
    fig4_mod_long <- rbind(fig4_mod_long, row)
  }
}

# Ergebnis prüfen
print(fig4_mod_long)


d201 <- 
  d201 %>%
  mutate(across(moderators_stdy2, ~as.numeric(scale(.)),.names = "{.col}_scl"))

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


# Doppelte Schleife über alle Kombinationen
for (pred in predictors_stdy2) {
  for (mod_scl in moderators_scl_stdy2) {
    
    # Formel und Modell
    formula_int_scl <- as.formula(paste("bias_threshold ~", pred, "*", mod_scl))
    m_int_scl <- lm(formula_int_scl, data = d201)
    
    # Kontrast extrahieren
    parts   <- strsplit(pred, "_0_vs_")[[1]]
    target1 <- gsub("bias_target_", "", parts[1])
    target2 <- gsub("_1$", "", parts[2])
    
    # Johnson-Neyman Intervall
    ros <- interactions::johnson_neyman(
      model = m_int_scl,
      pred  = !!rlang::sym(pred),
      modx  = !!rlang::sym(mod_scl)
    )
    
    # Neue Zeile für Ergebnis-Tabelle
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
    
    # Zeile anhängen
    fig4_mod_long_scl <- rbind(fig4_mod_long_scl, row)
  }
}

# Ergebnis prüfen
print(fig4_mod_long_scl)

stdy2_all_scales <- 
  fig4_mod_long_scl %>%
  mutate(moderator = stringr::str_remove_all(moderator,"_scl")) %>%
  full_join(fig4_mod_long,.) #%>%
#arrange(moderator)

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


## Robustness Check 3: Bootstrap JN-Bounds ----

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

# STUDY 3 ----

## TABLE 2: Included and Excluded participants no weigths ----

d003 <- d001[entity %in% c(
  "Employees", "Police Brutality Victims", 
  "University Students", "Monkeys") & 
    !is.na(study_id_for_paper) &
    !is.na(bias_threshold) &
    completed_survey == "Yes" &
    (!is.na(conservatism_7pt_merged) | 
       !is.na(conservatism_slider_merged))]

d003$passed_prereg_or_similar_criteria

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

## FIGURE 5 ----


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

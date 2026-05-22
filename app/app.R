library(shiny)
library(bslib)
library(tidyverse)
library(glmmTMB)
library(pROC)
library(broom.mixed)
library(leaflet)
library(sf)
library(scales)
library(DT)
library(cowplot)
library(patchwork)


# ── shared helpers ────────────────────────────────────────────────────────────
custom_colors  <- c("#E69F00","#56B4E9","#009E73","#F0E442","#0072B2","#D55E00","#CC79A7")
channel_colors <- c(HFC = "#899DA4", LFC = "#02401B")
percent_threshold <- 20

metrics_from_cm <- function(cm, model_name, auc_val) {
  TN <- cm["0","0"]; FN <- cm["0","1"]
  FP <- cm["1","0"]; TP <- cm["1","1"]
  n  <- TN + FN + FP + TP
  po <- (TP + TN) / n
  pe <- ((TP+FP)*(TP+FN) + (TN+FN)*(TN+FP)) / n^2
  tibble(
    Model = model_name, N = n,
    TP=TP, FP=FP, TN=TN, FN=FN,
    Accuracy    = (TP+TN)/n,
    Sensitivity = TP/(TP+FN),
    Specificity = TN/(TN+FP),
    Precision   = TP/(TP+FP),
    NPV         = TN/(TN+FN),
    `Bal. Acc.` = ((TP/(TP+FN))+(TN/(TN+FP)))/2,
    Kappa       = (po-pe)/(1-pe),
    Prevalence  = (TP+FN)/n,
    AUC         = auc_val
  ) |> mutate(across(where(is.numeric), \(x) round(x,3)))
}

mini_locations_raw     <- read_csv("mini_locations_raw.csv",  show_col_types=FALSE)
mini_fish_raw          <- read_csv("mini_fish_raw.csv",       show_col_types=FALSE)
steelhead_redd_summary <- read_rds("steelhead_redd_summary.rds")
chinook_redd_summary   <- read_rds("chinook_redd_summary.rds")
raw_chinook_redds      <- read_rds("raw_redd_location_chinook.rds")   |> sf::st_cast("POINT")
raw_steelhead_redds    <- read_rds("raw_redd_location_steelhead.rds") |> sf::st_cast("POINT")

prepare_species_data <- function(species_choice) {
  redd_summary <- if (species_choice=="chinook salmon") chinook_redd_summary else steelhead_redd_summary
  raw <- mini_fish_raw |>
    left_join(mini_locations_raw |> distinct()) |>
    mutate(
      count         = ifelse(is.na(count),0,count),
      fish_presence = as.factor(ifelse(count<1,"0","1")),
      month         = lubridate::month(date)
    ) |>
    filter(
      if (species_choice=="chinook salmon")
        species=="chinook salmon" | count==0
      else
        species %in% c("steelhead trout (wild)","steelhead trout (clipped)") | count==0
    )
  model_data <- raw |>
    select(count, location, channel_location, depth, velocity,
           contains("inchannel"), contains("overhead"),
           percent_cobble_substrate, percent_boulder_substrate,
           percent_undercut_bank, month, fl_mm,
           channel_geomorphic_unit, reach_length, reach_width, channel_type,
           any_of("surface_turbidity")) |>
    mutate(
      small_woody       = percent_small_woody_cover_inchannel,
      large_woody       = percent_large_woody_cover_inchannel,
      boulder_substrate = percent_boulder_substrate,
      cobble_substrate  = percent_cobble_substrate,
      undercut_bank     = percent_undercut_bank,
      aquatic_veg       = percent_submerged_aquatic_veg_inchannel,
      overhanging_veg   = percent_cover_half_meter_overhead + percent_cover_more_than_half_meter_overhead
    ) |>
    mutate(
      across(c(cobble_substrate,boulder_substrate,small_woody,large_woody,
               aquatic_veg,undercut_bank,overhanging_veg),
             \(x) ifelse(x>=percent_threshold,1,0)),
      no_cover_overhead = ifelse(percent_no_cover_overhead>=percent_threshold,1,0)
    ) |>
    select(-contains("no_cover")) |>
    distinct() |>
    mutate(fl_mm=ifelse(is.na(fl_mm),0,fl_mm)) |>
    na.omit() |>
    select(-fl_mm) |>
    left_join(redd_summary, by="location") |>
    mutate(
      redd_total    = replace_na(redd_total,0),
      redd_presence = replace_na(redd_presence,0),
      month         = as.factor(month)
    )
  log_reg_data <- model_data |> mutate(presence=as.integer(count>0))
  list(raw=raw, model_data=model_data, log_reg_data=log_reg_data)
}

build_formula <- function(species_choice, random_effects=NULL) {
  extra  <- if (species_choice=="steelhead trout" && "surface_turbidity" %in% names(mini_fish_raw))
    "+ surface_turbidity" else ""
  re_str <- if (!is.null(random_effects)) paste("+",random_effects) else ""
  as.formula(paste(
    "presence ~ small_woody + depth + velocity + large_woody +",
    "aquatic_veg + overhanging_veg + cobble_substrate +",
    "boulder_substrate + undercut_bank + redd_total + redd_presence",
    extra, re_str
  ))
}

cover_vars_raw <- c(
  "percent_small_woody_cover_inchannel"     = "Small woody",
  "percent_large_woody_cover_inchannel"     = "Large woody",
  "percent_submerged_aquatic_veg_inchannel" = "Aquatic veg",
  "percent_undercut_bank"                   = "Undercut bank",
  "percent_cobble_substrate"                = "Cobble",
  "percent_boulder_substrate"               = "Boulder"
)

term_labels <- c(
  small_woody="Small woody debris", large_woody="Large woody debris",
  overhanging_veg="Overhanging vegetation", aquatic_veg="Aquatic vegetation",
  cobble_substrate="Cobble substrate", boulder_substrate="Boulder substrate",
  undercut_bank="Undercut bank", depth="Depth", velocity="Velocity",
  redd_total="Total redds nearby", redd_presence="Redd present",
  surface_turbidity="Surface turbidity"
)

# ── standalone model-building helper ─────────────────────────────────────────
build_model_rows <- function(sp, lrd) {
  threshold <- 0.5
  lrd_sub   <- if (sp == "steelhead trout")
    lrd |> filter(month %in% c("3","4","5","6")) else lrd

  make_row <- function(m, dat, label) {
    pp      <- predict(m, type = "response")
    obs     <- dat$presence
    roc_obj <- roc(obs, pp, quiet = TRUE)
    cm      <- table(Predicted = ifelse(pp > threshold, 1, 0), Observed = obs)
    list(model = m, roc = roc_obj, cm = cm, label = label, dat = dat)
  }

  m1 <- glm(build_formula(sp), data = lrd, family = binomial())
  m2 <- glmmTMB(build_formula(sp, "(1|location)"), data = lrd, family = binomial())
  m3 <- glmmTMB(build_formula(sp, "(1|location)+(1|month)"), data = lrd, family = binomial())
  m4 <- if (sp == "steelhead trout")
    glmmTMB(build_formula(sp, "(1|location)+(1|month)"), data = lrd_sub, family = binomial())
  else NULL

  rows <- list(
    make_row(m1, lrd, "Simple logistic regression"),
    make_row(m2, lrd, "RE: location"),
    make_row(m3, lrd, "RE: location + month")
  )
  if (!is.null(m4)) rows[[4]] <- make_row(m4, lrd_sub, "RE: location + month (Mar–May)")
  rows
}

# ── standalone plot helpers ───────────────────────────────────────────────────
plot_effects <- function(best, sp_label_txt) {
  effects_df <- broom.mixed::tidy(best$model, conf.int = TRUE) |>
    filter(!term %in% c("(Intercept)","sd__(Intercept)")) |>
    mutate(sig        = !is.na(p.value) & p.value < 0.05,
           term_label = recode(term, !!!term_labels))
  ggplot(effects_df, aes(x = estimate, y = reorder(term_label, estimate),
                         color = sig, shape = sig)) +
    geom_vline(xintercept = 0, linetype = 2, color = "gray50") +
    geom_point(size = 2.8) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.25) +
    scale_color_manual(values = c(`FALSE` = "gray60", `TRUE` = "#D55E00"),
                       labels = c("p >= 0.05","p < 0.05"), name = NULL) +
    scale_shape_manual(values = c(`FALSE` = 1, `TRUE` = 16),
                       labels = c("p >= 0.05","p < 0.05"), name = NULL) +
    labs(title   = paste("Effect sizes —", sp_label_txt),
         caption = paste("Best model:", best$label, "| filled = p < 0.05"),
         x = "Log odds ratio", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.margin     = margin(t = 10, r = 20, b = 10, l = 140))
}

plot_roc <- function(mods, sp_label_txt) {
  roc_to_df <- function(m) data.frame(
    FPR   = 1 - m$roc$specificities,
    TPR   = m$roc$sensitivities,
    model = paste0(m$label, " (AUC = ", round(auc(m$roc), 3), ")")
  )
  df <- bind_rows(lapply(mods, roc_to_df))
  ggplot(df, aes(x = FPR, y = TPR, color = model)) +
    geom_line(linewidth = 1) +
    geom_abline(linetype = 2, color = "gray60") +
    scale_color_manual(values = custom_colors[seq_along(mods)]) +
    labs(title   = paste("ROC curves —", sp_label_txt),
         x       = "False positive rate (1 - Specificity)",
         y       = "True positive rate (Sensitivity)",
         color   = NULL,
         caption = "Dashed diagonal = chance (AUC = 0.5).") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.text = element_text(size = 8))
}

plot_month <- function(best, sp_label_txt) {
  month_df <- broom.mixed::tidy(best$model, effects = "ran_vals", conf.int = TRUE) |>
    filter(group == "month") |>
    rename(unit = level, re = estimate) |>
    mutate(odds_ratio = exp(re), or_low = exp(conf.low), or_high = exp(conf.high),
           month_num = as.integer(unit),
           unit = factor(paste("Month", unit),
                         levels = rev(paste("Month", sort(as.integer(unique(unit))))))) |>
    arrange(month_num)
  ggplot(month_df, aes(x = odds_ratio, y = unit)) +
    geom_vline(xintercept = 1, linetype = 2, color = "gray50") +
    geom_point(size = 2.5, color = "#0072B2") +
    geom_errorbarh(aes(xmin = or_low, xmax = or_high), height = 0.2, color = "#0072B2") +
    scale_x_log10() +
    labs(title   = paste("Month effects —", sp_label_txt),
         caption = "OR > 1: higher presence probability relative to global mean",
         x = "Random-effect odds ratio (log scale)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(plot.margin = margin(t = 10, r = 20, b = 10, l = 100))
}

plot_sites <- function(best, raw_data, sp_label_txt) {
  re_loc <- broom.mixed::tidy(best$model, effects = "ran_vals", conf.int = TRUE) |>
    filter(group == "location") |>
    rename(unit = level, re = estimate) |>
    left_join(raw_data |> select(unit = location, channel_location) |> distinct(), by = "unit") |>
    mutate(odds_ratio = exp(re), or_low = exp(conf.low), or_high = exp(conf.high)) |>
    arrange(odds_ratio) |>
    distinct(unit, .keep_all = TRUE) |>
    mutate(unit = factor(unit, levels = unit))
  ggplot(re_loc, aes(x = odds_ratio, y = unit, color = channel_location)) +
    geom_vline(xintercept = 1, linetype = 2, color = "gray50") +
    geom_point(size = 2.2) +
    geom_errorbarh(aes(xmin = or_low, xmax = or_high), height = 0.2) +
    scale_color_manual(values = channel_colors,
                       labels = c(HFC = "High-flow channel", LFC = "Low-flow channel"),
                       name   = "Channel type") +
    scale_x_log10() +
    labs(title   = paste("Site effects —", sp_label_txt),
         caption = "OR > 1: higher baseline presence probability",
         x = "Random-effect odds ratio (log scale)", y = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom",
          axis.text.y     = element_text(size = 7),
          plot.margin     = margin(t = 10, r = 20, b = 10, l = 120))
}

plot_hsi_df <- function(log_reg_data) {
  base_vars <- c("small_woody","large_woody","overhanging_veg",
                 "undercut_bank","aquatic_veg","boulder_substrate","cobble_substrate")
  all_vars  <- c(base_vars, intersect("surface_turbidity", names(log_reg_data)))
  df_long <- log_reg_data |>
    select(channel_location, presence, all_of(all_vars)) |>
    mutate(across(all_of(all_vars), \(x) as.integer(x > 0))) |>
    pivot_longer(all_of(all_vars), names_to = "feature", values_to = "feature_present")
  feature_summary <- df_long |>
    group_by(channel_location, feature) |>
    summarise(n_all       = n(),
              n_feat      = sum(feature_present == 1, na.rm = TRUE),
              n_fish      = sum(presence == 1,        na.rm = TRUE),
              n_fish_feat = sum(presence == 1 & feature_present == 1, na.rm = TRUE),
              .groups = "drop") |>
    group_by(channel_location) |>
    mutate(HA      = n_feat / n_all,
           HU      = ifelse(n_fish > 0, n_fish_feat / n_fish, NA_real_),
           P       = HU / HA,
           HSI_raw = ifelse(is.finite(P), P / max(P, na.rm = TRUE), NA_real_)) |>
    ungroup()
  plot_df <- feature_summary |>
    select(channel_location, feature, HA, HU, HSI_raw) |>
    pivot_longer(c(HA, HU, HSI_raw), names_to = "panel", values_to = "value") |>
    mutate(panel   = recode(panel, HA = "HA (availability)",
                            HU = "HU (utilization)", HSI_raw = "HSI (preference)"),
           feature = str_replace_all(feature, "_", " "),
           feature = recode(feature, "surface turbidity" = "surface turbulence"))
  plot_df
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title = span(
    style = "font-family: 'Roboto', serif; font-size: 1.05rem; letter-spacing: 0.02em; color: #333232;",
    "Feather River — Logistic Regression Analysis"
  ),
  theme = bs_theme(
    bootswatch   = "flatly",
    base_font    = font_google("Roboto"),
    heading_font = font_google("Roboto"),
    primary      = "black",
    secondary    = "#7294D4",
    success      = "#3A3A3C"
  ),
  bg      = "#9A8822",
  inverse = TRUE,
  fillable = FALSE,
  header = tags$head(tags$style(HTML("
    /* prevent bslib cards from clipping plot content */
    .card { overflow: visible !important; }
    .card-body { overflow: visible !important; }
    .bslib-card { overflow: visible !important; }
    .shiny-plot-output {
      overflow: visible !important;
    }
    /* leaflet must clip its own tiles */
    .leaflet-container { overflow: hidden !important; }
    .leaflet { overflow: hidden !important; }
  "))),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 1 · OVERVIEW
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel("Overview",
    br(),
    navset_card_tab(
      nav_panel("Objective & Approach",
        card(full_screen = FALSE, uiOutput("both_overview_objective")),
        br(),
        card(
          div(style = "padding: 4px 8px; font-size: 0.85rem; color: #666;",
            p(em("Data: ",
              tags$a("Feather River Mini Snorkel Survey (EDI, 2001–2002)",
                     href   = "https://portal.edirepository.org/nis/metadataviewer?packageid=edi.1705.3",
                     target = "_blank",
                     style  = "color:#666;")
            ))
          )
        )
      ),
      nav_panel("Raw Data Glimpse",
        card(padding = 0, plotOutput("cmp_overview_plot", height = "700px", width = "100%"))
      ),
      nav_panel("Dataset Summary",
        card(uiOutput("cmp_dataset_summary"))
      ),
      nav_panel("Variables of Interest",
        card(uiOutput("both_overview_vars"))
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 2 · DATA EXPLORATION
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel("Data Exploration",
    br(),
    navset_card_tab(
      nav_panel("HFC / LFC",
        card(padding = 0, plotOutput("cmp_hfc", height = "700px", width = "100%"))
      ),
      nav_panel("Redd Density Map",
        card(leafletOutput("cmp_map", height = "540px"))
      ),
      nav_panel("Site Explorer",
        card(uiOutput("cmp_site_selector_ui")),
        br(),
        card(padding = 0, plotOutput("cmp_site_explorer", height = "560px", width = "100%"))
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 3 · MODEL PERFORMANCE
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel("Model Performance",
    br(),
    navset_card_tab(
      nav_panel("Model Comparison",
        card(
          div(style = "padding: 4px 8px;",
            h5("Comparing candidate models"),
            p("Four candidate models are evaluated — from a simple logistic regression
               to mixed-effects models with random intercepts for transect location
               and month. All models use a classification threshold of 0.5 to convert
               predicted probabilities to presence/absence."),
            p(strong("Fish presence records are rare (< 30% of observations)."),
              "This class imbalance means overall", strong("Accuracy"), "is misleading
               — a model predicting all absences would score high. Focus instead on:"),
            tags$ul(
              tags$li(strong("AUC"), "— area under the ROC curve; 0.5 = chance, 1.0 = perfect discrimination."),
              tags$li(strong("Balanced Accuracy"), "— average of Sensitivity and Specificity; robust to class imbalance."),
              tags$li(strong("Sensitivity"), "— proportion of true presences correctly predicted (true positive rate)."),
              tags$li(strong("Specificity"), "— proportion of true absences correctly predicted (true negative rate)."),
              tags$li(strong("Kappa"), "— agreement above chance; accounts for prevalence.")
            )
          )
        ),
        uiOutput("cmp_model_comparison")
      ),
      nav_panel("Effect Sizes",
        card(
          div(style = "padding: 4px 8px;",
            h5("How to read this plot"),
            p("Each point shows the estimated", strong("log odds ratio"), "for a predictor
               in the best-fitting model, with 95% confidence intervals. A log odds ratio
               of zero (dashed line) means no association with fish presence."),
            tags$ul(
              tags$li(strong("Positive values"), "— the predictor increases the probability of fish presence."),
              tags$li(strong("Negative values"), "— the predictor decreases the probability of fish presence."),
              tags$li(strong("Filled/orange points"), "— statistically significant (p < 0.05)."),
              tags$li(strong("Open/grey points"), "— confidence interval overlaps zero; effect is uncertain.")
            ),
            p("Binary cover and substrate variables (e.g., cobble, small woody) are coded
               1 if >= 20% cover at the transect, 0 otherwise. Continuous predictors
               (depth, velocity) are on their original scale.")
          )
        ),
        card(padding = 0, plotOutput("cmp_effects", height = "560px", width = "100%"))
      ),
      nav_panel("Month Effects",
        card(
          div(style = "padding: 4px 8px;",
            h5("Seasonal random effects"),
            p("The mixed-effects model includes a random intercept for survey month,
               allowing each month to have its own baseline fish-presence probability
               relative to the global mean. The plot shows these month-level departures
               expressed as", strong("odds ratios (OR)."), "The x-axis is on a log scale."),
            tags$ul(
              tags$li(strong("OR > 1"), "— fish presence probability is higher than average in that month."),
              tags$li(strong("OR < 1"), "— fish presence probability is lower than average in that month."),
              tags$li(strong("OR = 1 (dashed line)"), "— no departure from the global mean.")
            ),
            p("Wide confidence intervals indicate months with fewer survey observations
               or high within-month variability.")
          )
        ),
        card(padding = 0, plotOutput("cmp_month", height = "480px", width = "100%"))
      ),
      nav_panel("Site Effects",
        card(
          div(style = "padding: 4px 8px;",
            h5("Site-level random effects"),
            p("The model includes a random intercept for each transect location,
               capturing site-specific differences in baseline fish presence that
               are not explained by the fixed predictors (depth, velocity, cover, etc.).
               Points are colored by channel type."),
            tags$ul(
              tags$li(strong("OR > 1"), "— site has a higher baseline presence probability than the average site."),
              tags$li(strong("OR < 1"), "— site has a lower baseline presence probability."),
              tags$li(strong("HFC (High-flow channel)"), "— main channel with higher discharge and velocity."),
              tags$li(strong("LFC (Low-flow channel)"), "— side channel with lower, more stable flows.")
            ),
            p("Sites are sorted by their estimated OR. A wide spread across sites
               confirms that location-level heterogeneity is a substantial source
               of variation — justifying the random effects structure.")
          )
        ),
        card(padding = 0, plotOutput("cmp_sites", height = "700px", width = "100%"))
      ),
      nav_panel("Habitat Preference (HSI)",
        card(
          div(style = "padding: 4px 8px;",
            h5("Habitat availability, utilization, and preference (HSI)"),
            p("This panel summarizes how each cover/substrate feature is used relative
               to how often it is available — following the HSI framework of",
              em("Conallin et al. 2014"), ". All values are scaled 0–1."),
            fluidRow(
              column(4,
                tags$dl(
                  tags$dt("HA — Habitat Availability"),
                  tags$dd("Proportion of all survey transects where the feature is present
                           (>= 20% cover). Reflects how common a habitat feature is in the study area.")
                )
              ),
              column(4,
                tags$dl(
                  tags$dt("HU — Habitat Utilization"),
                  tags$dd("Proportion of fish-presence transects where the feature is present.
                           Reflects how often fish were observed where the feature occurred.")
                )
              ),
              column(4,
                tags$dl(
                  tags$dt("HSI — Habitat Preference"),
                  tags$dd("HU / HA, normalized to the highest-scoring feature.
                           Values near 1 indicate strong preference; values near 0 indicate
                           avoidance or indifference relative to availability.")
                )
              )
            ),
            p("Compare HA and HU side-by-side: if HU is much higher than HA for a feature,
               fish are using it more than expected by chance — resulting in a high HSI score.")
          )
        ),
        card(padding = 0, plotOutput("cmp_hsi", height = "600px", width = "100%"))
      )
    )
  ),

  # ══════════════════════════════════════════════════════════════════════════
  # TAB 4 · RESULTS / DISCUSSION
  # ══════════════════════════════════════════════════════════════════════════
  nav_panel("Results & Discussion",
    br(),
    uiOutput("cmp_discussion")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── reactives (both species) ──────────────────────────────────────────────
  cmp_data_ck <- reactive({ prepare_species_data("chinook salmon") })
  cmp_data_st <- reactive({ prepare_species_data("steelhead trout") })
  cmp_mods_ck <- reactive({ build_model_rows("chinook salmon",  cmp_data_ck()$log_reg_data) })
  cmp_mods_st <- reactive({ build_model_rows("steelhead trout", cmp_data_st()$log_reg_data) })
  cmp_best_ck <- reactive({ m <- cmp_mods_ck(); m[[length(m)]] })
  cmp_best_st <- reactive({ m <- cmp_mods_st(); m[[length(m)]] })

  # ── OVERVIEW ───────────────────────────────────────────────────────────────

  output$both_overview_objective <- renderUI({
    tagList(
      h4("Objective"),
      p("Develop a model that reflects the significance of cover, substrate, depth, and
         velocity on juvenile fish presence and absence in the Feather River using Mini
         Snorkel Survey data (EDI, 2001–2002). Models were fit separately for Chinook
         Salmon and Steelhead Trout."),
      hr(),
      h4("Modeling approach"),
      tags$ol(
        tags$li(strong("Initial: Hurdle model"),
                "— evaluated to separate zero/non-zero counts. Count component performed
                 poorly due to extreme zero-inflation and high variability. Analysis
                 refocused on presence/absence."),
        tags$li(strong("Refined: Logistic regression"),
                "— binary response (fish present/absent). Cover and substrate variables
                 binarized at a", strong(paste0(percent_threshold, "% threshold")), "."),
        tags$li(strong("Mixed-effects structure"),
                "— glmmTMB with random intercepts for transect location and month to
                 capture spatial heterogeneity and seasonal dynamics.")
      ),
      hr(),
      h4("Best model results"),
      fluidRow(
        column(6,
               h5("Chinook Salmon"),
               tags$ol(
                 tags$li("Simple logistic regression (no random effects)"),
                 tags$li("RE: transect location"),
                 tags$li("RE: location + month (all survey months, March–August)",
                         strong(" ← best"), " — AUC ≈ 0.91")
               )
        ),
        column(6,
               h5("Steelhead Trout"),
               tags$ol(
                 tags$li("Simple logistic regression (no random effects)"),
                 tags$li("RE: transect location"),
                 tags$li("RE: location + month"),
                 tags$li("RE: location + month (March–May only)",
                         strong(" ← best"), " — AUC ≈ 0.91")
               )
        )
      )
    )
  })

  output$both_overview_vars <- renderUI({
    tagList(
      h4("Predictor variables"),
      p("Cover and substrate variables are measured as percentages and converted to binary
         presence/absence using a", strong(paste0(percent_threshold, "% threshold")), ".
         Overhanging vegetation categories were combined."),
      hr(),
      fluidRow(
        column(4,
          h5("Continuous"),
          tags$ul(
            tags$li(strong("Depth"), " — numeric (m)"),
            tags$li(strong("Velocity"), " — numeric (m/s)"),
            tags$li(strong("Total redds nearby"), " — count"),
            tags$li(strong("Surface turbulence"), " — numeric (Steelhead only)")
          )
        ),
        column(4,
          h5("Binary"),
          tags$ul(
            tags$li(strong("Overhanging vegetation")),
            tags$li(strong("Small woody debris")),
            tags$li(strong("Large woody debris")),
            tags$li(strong("Aquatic vegetation")),
            tags$li(strong("Undercut bank")),
            tags$li(strong("Boulder substrate")),
            tags$li(strong("Cobble substrate")),
            tags$li(strong("Redd present"), " — 0/1")
          )
        ),
        column(4,
          h5("Random effects"),
          tags$ul(
            tags$li(strong("Location"), " — site-level spatial heterogeneity"),
            tags$li(strong("Month"), " — seasonal dynamics")
          ),
          br(), h5("Response"),
          tags$ul(tags$li(strong("Fish presence"), " — 1 if count > 0, else 0"))
        )
      )
    )
  })

  # ── Raw Data Glimpse ──────────────────────────────────────────────────────
  output$cmp_overview_plot <- renderPlot(res = 96, {
    ck <- cmp_data_ck()
    st <- cmp_data_st()

    make_overview_plots <- function(d, sp_lbl) {
      p1 <- d$raw |> filter(count > 0) |>
        ggplot(aes(count, fill = channel_location)) +
        geom_histogram(binwidth = 50, color = "white") +
        scale_fill_manual(values = channel_colors, name = "Channel") +
        facet_grid(~channel_location) +
        labs(title = paste("Count distribution —", sp_lbl, "(non-zero)"),
             x = "Fish count", y = "Frequency") +
        theme_minimal(base_size = 12) + theme(legend.position = "none")

      p2 <- d$raw |>
        group_by(channel_location, month = lubridate::month(date)) |>
        tally(count) |>
        ggplot(aes(x = as.factor(month), y = n, fill = channel_location)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = channel_colors, name = "Channel") +
        labs(title = paste(sp_lbl, "— total counts by month and channel"),
             x = "Month", y = "Total count") +
        theme_minimal(base_size = 12)

      p1 + p2
    }

    ck_plots <- make_overview_plots(ck, "Chinook Salmon")
    st_plots <- make_overview_plots(st, "Steelhead Trout")

    ck_plots / st_plots
  })

  # ── Dataset Summary ───────────────────────────────────────────────────────
  output$cmp_dataset_summary <- renderUI({
    make_summary_tbl <- function(d, sp_lbl) {
      n_obs      <- nrow(d$log_reg_data)
      n_present  <- sum(d$log_reg_data$presence)
      prevalence <- round(n_present / n_obs * 100, 1)
      n_sites    <- length(unique(d$log_reg_data$location))
      months     <- sort(unique(as.integer(as.character(d$log_reg_data$month))))
      tagList(
        h5(sp_lbl),
        tags$table(
          class = "table table-condensed table-bordered",
          tags$tbody(
            tags$tr(tags$th("Total observations"), tags$td(format(n_obs, big.mark = ","))),
            tags$tr(tags$th("Presence records"),   tags$td(format(n_present, big.mark = ","))),
            tags$tr(tags$th("Prevalence"),         tags$td(paste0(prevalence, "%"))),
            tags$tr(tags$th("Sampling sites"),     tags$td(n_sites)),
            tags$tr(tags$th("Survey months"),      tags$td(paste(month.abb[months], collapse = ", ")))
          )
        )
      )
    }
    fluidRow(
      column(6, make_summary_tbl(cmp_data_ck(), "Chinook Salmon")),
      column(6, make_summary_tbl(cmp_data_st(), "Steelhead Trout"))
    )
  })

  # ── HFC / LFC ─────────────────────────────────────────────────────────────
  output$cmp_hfc <- renderPlot(res = 96, {
    make_hfc_plots <- function(d, sp_lbl) {
      p1 <- d$raw |>
        mutate(month = factor(lubridate::month(date), levels = 1:12, labels = month.abb)) |>
        group_by(channel_location, month) |>
        tally(count) |>
        ggplot(aes(x = month, y = n, fill = channel_location)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = channel_colors, name = "Channel") +
        labs(title = paste(sp_lbl, "— total counts by month and channel"),
             x = NULL, y = "Total count") +
        theme_minimal(base_size = 12)

      p2 <- d$raw |>
        mutate(month    = factor(lubridate::month(date), levels = 1:12, labels = month.abb),
               presence = as.integer(count > 0)) |>
        group_by(channel_location, month) |>
        summarise(pct_present = mean(presence) * 100, .groups = "drop") |>
        ggplot(aes(x = month, y = pct_present, fill = channel_location)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = channel_colors, name = "Channel") +
        labs(title = paste(sp_lbl, "— % transects with fish present"),
             x = NULL, y = "% observations with fish") +
        theme_minimal(base_size = 12)

      p1 + p2
    }

    ck_plt <- make_hfc_plots(cmp_data_ck(), "Chinook Salmon")
    st_plt <- make_hfc_plots(cmp_data_st(), "Steelhead Trout")
    ck_plt / st_plt
  })

  # ── Redd Density Map (combined) ───────────────────────────────────────────
  output$cmp_map <- renderLeaflet({
    location_coords <- mini_locations_raw |>
      distinct(location, longitude, latitude) |>
      filter(!is.na(longitude), !is.na(latitude)) |>
      group_by(location) |> slice(1) |> ungroup()

    locations_sf <- location_coords |>
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

    # shared scale so circle sizes are comparable across species
    global_max <- max(c(chinook_redd_summary$redd_total,
                        steelhead_redd_summary$redd_total), na.rm = TRUE)

    ck_sf <- chinook_redd_summary |>
      left_join(location_coords, by = "location") |>
      filter(!is.na(longitude), !is.na(latitude)) |>
      mutate(radius = rescale(redd_total, from = c(0, global_max), to = c(3, 18))) |>
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

    st_sf <- steelhead_redd_summary |>
      left_join(location_coords, by = "location") |>
      filter(!is.na(longitude), !is.na(latitude)) |>
      mutate(radius = rescale(redd_total, from = c(0, global_max), to = c(3, 18))) |>
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)

    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Aerial Imagery") |>
      addProviderTiles(providers$OpenStreetMap,     group = "Street Map") |>
      addCircleMarkers(data = locations_sf, color = "#0072B2", radius = 5,
                       fillOpacity = 0.3, opacity = 0.7,
                       popup = ~paste0("Location: ", location),
                       group = "Snorkel transects") |>
      addCircleMarkers(data = ck_sf, color = "#E69F00",
                       radius = ~radius,
                       fillOpacity = 0.85, opacity = 1,
                       popup = ~paste0("Location: ", location, "<br>Chinook Redds: ", redd_total),
                       group = "Chinook redds") |>
      addCircleMarkers(data = st_sf, color = "#56B4E9",
                       radius = ~radius,
                       fillOpacity = 0.85, opacity = 1,
                       popup = ~paste0("Location: ", location, "<br>Steelhead Redds: ", redd_total),
                       group = "Steelhead redds") |>
      addLayersControl(
        baseGroups   = c("Aerial Imagery","Street Map"),
        overlayGroups = c("Snorkel transects","Chinook redds","Steelhead redds"),
        position     = "topleft",
        options      = layersControlOptions(collapsed = FALSE)
      ) |>
      addLegend(
        colors = c("#0072B2","#E69F00","#56B4E9"),
        labels = c("Snorkel transects","Chinook redds (size ∝ count)","Steelhead redds (size ∝ count)")
      )
  })

  # ── Site Explorer (both species) ──────────────────────────────────────────
  output$cmp_site_selector_ui <- renderUI({
    sites <- sort(unique(cmp_data_ck()$raw$location))
    fluidRow(
      column(4, selectInput("cmp_selected_site", "Select a site:", choices = sites, selected = sites[1])),
      column(8, br(), p(em("Showing both species' presence rates and cover at the selected site.")))
    )
  })

  output$cmp_site_explorer <- renderPlot(res = 96, {
    req(input$cmp_selected_site)

    make_site_plots <- function(d, sp_lbl, bar_color) {
      site_monthly <- d$raw |>
        filter(location == input$cmp_selected_site) |>
        mutate(month    = factor(lubridate::month(date), levels = 1:12, labels = month.abb),
               presence = as.integer(count > 0)) |>
        group_by(month) |>
        summarise(pct_present = mean(presence) * 100, total_count = sum(count),
                  n_obs = n(), .groups = "drop")

      p1 <- ggplot(site_monthly, aes(x = month, y = pct_present)) +
        geom_col(fill = bar_color, alpha = 0.85) +
        scale_y_continuous(limits = c(0, max(site_monthly$pct_present, 5) * 1.2)) +
        labs(title = paste(sp_lbl, "presence rate —", tools::toTitleCase(input$cmp_selected_site)),
             x = NULL, y = "% observations with fish") +
        theme_minimal(base_size = 12) +
        theme(plot.margin = margin(t = 10, r = 10, b = 20, l = 10))

      cover_df <- d$raw |>
        filter(location == input$cmp_selected_site) |>
        mutate(overhanging_veg = percent_cover_half_meter_overhead +
                 percent_cover_more_than_half_meter_overhead) |>
        select(all_of(names(cover_vars_raw)), overhanging_veg) |>
        rename(!!!setNames(names(cover_vars_raw), unname(cover_vars_raw)),
               `Overhanging veg` = overhanging_veg) |>
        pivot_longer(everything(), names_to = "feature", values_to = "pct") |>
        group_by(feature) |>
        summarise(mean_pct = mean(pct, na.rm = TRUE), .groups = "drop")

      p2 <- ggplot(cover_df, aes(x = mean_pct, y = reorder(feature, mean_pct))) +
        geom_col(fill = "#9A8822", alpha = 0.8) +
        labs(title = paste("Mean % cover —", tools::toTitleCase(input$cmp_selected_site)),
             x = "Mean % cover", y = NULL) +
        theme_minimal(base_size = 12)

      p1 + p2
    }

    ck_plt <- make_site_plots(cmp_data_ck(), "Chinook Salmon",  "#E69F00")
    st_plt <- make_site_plots(cmp_data_st(), "Steelhead Trout", "#56B4E9")
    ck_plt / st_plt
  })

  # ── MODEL PERFORMANCE ──────────────────────────────────────────────────────

  output$cmp_model_comparison <- renderUI({
    tagList(
      fluidRow(
        column(6, h5("Chinook Salmon"), DTOutput("cmp_perf_table_ck")),
        column(6, h5("Steelhead Trout"), DTOutput("cmp_perf_table_st"))
      ),
      br(),
      card(plotOutput("cmp_roc_plot", height = "460px"))
    )
  })

  output$cmp_perf_table_ck <- renderDT({
    mods <- cmp_mods_ck()
    rows <- bind_rows(lapply(mods, function(m) metrics_from_cm(m$cm, m$label, round(auc(m$roc), 3))))
    datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
              caption = "Chinook Salmon — classification threshold = 0.5")
  })

  output$cmp_perf_table_st <- renderDT({
    mods <- cmp_mods_st()
    rows <- bind_rows(lapply(mods, function(m) metrics_from_cm(m$cm, m$label, round(auc(m$roc), 3))))
    datatable(rows, rownames = FALSE, options = list(scrollX = TRUE, dom = "t"),
              caption = "Steelhead Trout — classification threshold = 0.5")
  })

  output$cmp_roc_plot <- renderPlot({
    ck_plt <- plot_roc(cmp_mods_ck(), "Chinook Salmon")
    st_plt <- plot_roc(cmp_mods_st(), "Steelhead Trout")
    ck_plt | st_plt
  })

  # ── Effect Sizes ──────────────────────────────────────────────────────────
  output$cmp_effects <- renderPlot(res = 96, {
    ck_plt <- plot_effects(cmp_best_ck(), "Chinook Salmon")
    st_plt <- plot_effects(cmp_best_st(), "Steelhead Trout")
    ck_plt | st_plt
  })

  # ── Month Effects ─────────────────────────────────────────────────────────
  output$cmp_month <- renderPlot(res = 96, {
    ck_plt <- plot_month(cmp_best_ck(), "Chinook Salmon")
    st_plt <- plot_month(cmp_best_st(), "Steelhead Trout")
    ck_plt | st_plt
  })

  # ── Site Effects ──────────────────────────────────────────────────────────
  output$cmp_sites <- renderPlot(res = 96, {
    ck_plt <- plot_sites(cmp_best_ck(), cmp_data_ck()$raw, "Chinook Salmon")
    st_plt <- plot_sites(cmp_best_st(), cmp_data_st()$raw, "Steelhead Trout")
    ck_plt | st_plt
  })

  # ── Habitat Preference (HSI) ──────────────────────────────────────────────
  output$cmp_hsi <- renderPlot(res = 96, {
    ck_df <- plot_hsi_df(cmp_data_ck()$log_reg_data) |> mutate(species = "Chinook Salmon")
    st_df <- plot_hsi_df(cmp_data_st()$log_reg_data) |> mutate(species = "Steelhead Trout")
    combined <- bind_rows(ck_df, st_df)

    ggplot(combined, aes(x = feature, y = value, fill = species)) +
      geom_col(position = "dodge", alpha = 0.85) +
      facet_grid(panel ~ channel_location, scales = "free_y") +
      scale_fill_manual(values = c("Chinook Salmon" = "#C47D2B", "Steelhead Trout" = "#2C7BB6"),
                        name = "Species") +
      labs(x       = NULL,
           y       = "Value (0–1)",
           title   = "Species comparison — habitat availability, utilization, and preference (HSI)",
           caption = "Figure based off of Conallin et al. 2014, Figure 5") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x  = element_text(angle = 45, hjust = 1),
            strip.text.y = element_text(size = 9),
            legend.position = "bottom",
            plot.margin  = margin(t = 10, r = 20, b = 40, l = 100))
  })

  # ── Results & Discussion ──────────────────────────────────────────────────
  output$cmp_discussion <- renderUI({
    "work in progress.... "
  })
  # output$cmp_discussion <- renderUI({
  #   cover_finding_ck <- tagList(
  #     p(strong("Overhanging vegetation"), "was the strongest cover predictor (OR > 2, p < 0.05),
  #        consistent across all months and sites. Riparian structure is a primary habitat
  #        driver for juvenile Chinook in the Feather River."),
  #     p(strong("Small woody debris"), "showed a moderate positive association.",
  #       strong("Velocity"), "was strongly negative — fish avoid high-velocity microhabitats.",
  #       strong("Aquatic vegetation"), "was negatively associated, possibly reflecting
  #        reduced hydraulic complexity in vegetation-dominated areas."),
  #     p("Substrate types, large woody debris, and undercut banks were not significant
  #        independent predictors once overhanging cover and hydraulics were included.")
  #   )
  # 
  #   cover_finding_st <- tagList(
  #     p(strong("Overhanging vegetation"), ",", strong("aquatic vegetation"), ", and",
  #       strong("small woody debris"), "were all positively associated with juvenile steelhead
  #        presence — consistent with use of structurally complex microhabitats for predation
  #        risk reduction."),
  #     p(strong("Velocity"), "was negative. Multiple cover types contribute, rather than a
  #        single dominant predictor, suggesting steelhead use a broader range of cover features
  #        than Chinook.")
  #   )
  # 
  #   ck <- cmp_data_ck()
  #   st <- cmp_data_st()
  # 
  #   ck_n_obs      <- nrow(ck$log_reg_data)
  #   ck_n_present  <- sum(ck$log_reg_data$presence)
  #   ck_prevalence <- round(ck_n_present / ck_n_obs * 100, 1)
  # 
  #   st_n_obs      <- nrow(st$log_reg_data)
  #   st_n_present  <- sum(st$log_reg_data$presence)
  #   st_prevalence <- round(st_n_present / st_n_obs * 100, 1)
  # 
  #   div(style = "max-width:1100px;",
  # 
  #     # 1. Cover Is Important
  #     h4("Cover Is Important"),
  #     p(class = "text-muted fst-italic",
  #       "Finding: Cover increases the probability of juvenile fish presence,
  #        but its effect is modulated by spatial and temporal context."),
  #     fluidRow(
  #       column(6,
  #              h5("Chinook Salmon"),
  #              cover_finding_ck
  #       ),
  #       column(6,
  #              h5("Steelhead Trout"),
  #              cover_finding_st
  #       )
  #     ),
  #     hr(),
  # 
  #     # 2. Study Design & Data Skewness
  #     h4("Study Design & Data Skewness"),
  #     fluidRow(
  #       column(6,
  #              h5("Zero-inflation and class imbalance"),
  #              fluidRow(
  #                column(6,
  #                       p(strong("Chinook Salmon")),
  #                       p(paste0("Fish presence records comprise only ", ck_prevalence,
  #                                "% of observations (", format(ck_n_present, big.mark = ","),
  #                                " of ", format(ck_n_obs, big.mark = ","), " total)."))
  #                ),
  #                column(6,
  #                       p(strong("Steelhead Trout")),
  #                       p(paste0("Fish presence records comprise only ", st_prevalence,
  #                                "% of observations (", format(st_n_present, big.mark = ","),
  #                                " of ", format(st_n_obs, big.mark = ","), " total)."))
  #                )
  #              ),
  #              p("This extreme class imbalance means overall accuracy is misleading — a model
  #                 that predicts all absences would be highly accurate but useless."),
  #              tags$ul(
  #                tags$li("AUC and balanced accuracy are the preferred performance metrics."),
  #                tags$li("Sensitivity (correctly detecting true presences) is low at the
  #                  default 0.5 classification threshold."),
  #                tags$li("A hurdle model was evaluated for count data but performed poorly —
  #                  count variability was too high and zero-inflation too severe.")
  #              )
  #       ),
  #       column(6,
  #              h5("Temporal mismatch"),
  #              p("Mini Snorkel surveys were conducted in 2001–2002. Redd data span 2014–2023
  #                 (Chinook) and ongoing (Steelhead). The redd-fish spatial correlation is
  #                 interpreted as a", em("spatial pattern only"),
  #                "— not a contemporaneous relationship."),
  #              hr(),
  #              h5("Binary cover variables"),
  #              p("Cover and substrate variables measured as percentages were binarized at a",
  #                strong(paste0(percent_threshold, "% threshold")), "for modeling. Key predictors
  #                 remain consistent across threshold values tested."),
  #              hr(),
  #              h5("Survey design"),
  #              p("Fixed transect snorkel surveys across March–August. Not all sites were surveyed
  #                 in all months — random effects for month help account for this unbalanced
  #                 sampling structure. Steelhead models are restricted to March–May surveys.")
  #       )
  #     ),
  #     hr(),
  # 
  #     # 3. Spatial Coupling with Redds
  #     h4("Spatial Coupling with Redds"),
  #     p(class = "text-muted fst-italic",
  #       "Finding: Juvenile fish presence is spatially associated with redd locations,
  #        but this coupling is not uniform across all sites and months."),
  #     fluidRow(
  #       column(6,
  #              h5("What the models show"),
  #              tags$ul(
  #                tags$li(strong("Total redd count (redd_total):"),
  #                        "positive and significant — reaches with greater cumulative redd
  #                  activity tend to have higher juvenile presence probability."),
  #                tags$li(strong("Binary redd presence (redd_presence):"),
  #                        "positive but not always significant — redd", em("intensity"),
  #                        "(count) is more informative than simple presence/absence of spawning.")
  #              ),
  #              br(), h5("Mechanisms"),
  #              tags$ol(
  #                tags$li(strong("Parent-offspring proximity:"),
  #                        "Juveniles emerge and rear near where adults spawned. Reaches with
  #                  higher spawning intensity produce more juveniles locally."),
  #                tags$li(strong("Shared habitat quality:"),
  #                        "Reaches that attract spawning adults may also provide good rearing
  #                  habitat — the correlation partly reflects shared suitability rather
  #                  than a strict parent-offspring link.")
  #              )
  #       ),
  #       column(6,
  #              h5("When the coupling breaks down"),
  #              p("The redd-juvenile coupling is not universal. Site random effects show high
  #                 variability in baseline presence probabilities", em("independent"),
  #                "of redd counts."),
  #              tags$ul(
  #                tags$li("Thermal conditions (summer warming) may limit rearing suitability
  #                  in reaches that support cold-water spawning."),
  #                tags$li("Downstream connectivity — juveniles may disperse from natal reaches
  #                  to better rearing habitat."),
  #                tags$li("Competition and density dependence — high spawning-area density may
  #                  not translate proportionally to higher juvenile counts."),
  #                tags$li("River size and project footprint — larger reaches have greater spatial
  #                  heterogeneity in redd-to-rearing habitat proximity.")
  #              )
  #       )
  #     )
  #   )
  # })

}

shinyApp(ui, server)

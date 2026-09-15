# ============================================================
# NORMAL FOREST PLOT FOR LEAVE-ONE-OUT ANALYSIS
# 
# RECOMMENDED REPRESENTATION:
#   - Each point = pooled estimate after omitting one study
#   - Horizontal line = 95% CI of the LOO estimate
#   - Vertical solid line = full-data REML pooled estimate
#   - Vertical dashed line = theoretical/reference value
#   - Diamond = FULL REML MODEL ONLY
# ============================================================


# ------------------------------------------------------------
# 1. Prepare LOO slope results
# ------------------------------------------------------------

loo.slope.df <- data.frame(
  Omitted_Study = dat$Study,
  Location = dat$Location,
  Estimate = loo.slope$estimate,
  SE = loo.slope$se
)

loo.slope.df$LCL <- (
  loo.slope.df$Estimate -
    1.96 * loo.slope.df$SE
)

loo.slope.df$UCL <- (
  loo.slope.df$Estimate +
    1.96 * loo.slope.df$SE
)


# ------------------------------------------------------------
# 2. Prepare LOO intercept results
# ------------------------------------------------------------

loo.int.df <- data.frame(
  Omitted_Study = dat$Study,
  Location = dat$Location,
  Estimate = loo.int$estimate,
  SE = loo.int$se
)

loo.int.df$LCL <- (
  loo.int.df$Estimate -
    1.96 * loo.int.df$SE
)

loo.int.df$UCL <- (
  loo.int.df$Estimate +
    1.96 * loo.int.df$SE
)


# ============================================================
# 3. NORMAL LOO FOREST-PLOT FUNCTION
# ============================================================

normal_loo_forest <- function(
    df,
    full_model,
    reference_line,
    title,
    xlab
) {
  
  # ----------------------------------------------------------
  # Order studies
  # ----------------------------------------------------------
  
  df$Row <- rev(seq_len(nrow(df)))
  
  
  # ----------------------------------------------------------
  # FULL REML MODEL
  # ----------------------------------------------------------
  
  pooled <- as.numeric(coef(full_model)[1])
  
  pooled_lcl <- full_model$ci.lb
  pooled_ucl <- full_model$ci.ub
  
  
  # ----------------------------------------------------------
  # Determine plotting range
  # ----------------------------------------------------------
  
  xr <- max(df$UCL) - min(df$LCL)
  
  xmin <- min(df$LCL)
  xmax <- max(df$UCL)
  
  
  # ----------------------------------------------------------
  # Positions for text columns
  # ----------------------------------------------------------
  
  study.col <- xmin - 1.10 * xr
  loc.col   <- xmin - 0.40 * xr
  
  est.col   <- xmax + 0.10 * xr
  
  
  # ==========================================================
  # FULL REML DIAMOND
  # ==========================================================
  
  # Put diamond below all LOO rows
  diamond.y <- -1.0
  
  diamond <- data.frame(
    x = c(
      pooled_lcl,
      pooled,
      pooled_ucl,
      pooled
    ),
    
    y = c(
      diamond.y,
      diamond.y + 0.35,
      diamond.y,
      diamond.y - 0.35
    )
  )
  
  
  # ==========================================================
  # PLOT
  # ==========================================================
  
  ggplot(
    df,
    aes(
      x = Estimate,
      y = Row
    )
  ) +
    
    # --------------------------------------------------------
  # LOO 95% confidence intervals
  # --------------------------------------------------------
  
  geom_errorbarh(
    aes(
      xmin = LCL,
      xmax = UCL
    ),
    height = 0.15,
    linewidth = 0.5
  ) +
    
    
    # --------------------------------------------------------
  # LOO estimates
  # --------------------------------------------------------
  
  geom_point(
    size = 3,
    shape = 21,
    fill = "navyblue"
  ) +
    
    
    # --------------------------------------------------------
  # THEORETICAL REFERENCE LINE
  # --------------------------------------------------------
  
  #geom_vline(
  #  xintercept = reference_line,
  #  linetype = "dashed",
   # linewidth = 0.8
  #) +
    
    
    # --------------------------------------------------------
  # FULL REML ESTIMATE
  # --------------------------------------------------------
  
  geom_vline(
    xintercept = pooled,
    linewidth = 1.1
  ) +
    
    
    # ========================================================
  # FULL REML DIAMOND
  # ========================================================
  
  geom_polygon(
    data = diamond,
    aes(
      x = x,
      y = y
    ),
    inherit.aes = FALSE,
    fill = "navyblue"
  ) +
    
    
    # --------------------------------------------------------
  # STUDY LABELS
  # --------------------------------------------------------
  
  geom_text(
    aes(
      x = study.col,
      label = Omitted_Study
    ),
    hjust = 0,
    size = 3.1
  ) +
    
    
    # --------------------------------------------------------
  # LOCATION LABELS
  # --------------------------------------------------------
  
  geom_text(
    aes(
      x = loc.col,
      label = Location
    ),
    hjust = 0,
    size = 3.1
  ) +
    
    
    # --------------------------------------------------------
  # LOO EFFECT + 95% CI
  # --------------------------------------------------------
  
  geom_text(
    aes(
      x = est.col,
      label = sprintf(
        "%.2f [%.2f, %.2f]",
        Estimate,
        LCL,
        UCL
      )
    ),
    hjust = 0,
    size = 3.0
  ) +
    
    
    # ========================================================
  # HEADERS
  # ========================================================
  
  annotate(
    "text",
    x = study.col,
    y = nrow(df) + 1.8,
    label = "Study omitted",
    fontface = "bold",
    hjust = 0,
    size = 3.5
  ) +
    
    annotate(
      "text",
      x = loc.col,
      y = nrow(df) + 1.8,
      label = "Location",
      fontface = "bold",
      hjust = 0,
      size = 3.5
    ) +
    
    annotate(
      "text",
      x = est.col,
      y = nrow(df) + 1.8,
      label = "LOO estimate [95% CI]",
      fontface = "bold",
      hjust = 0,
      size = 3.5
    ) +
    
    
    # ========================================================
  # FULL REML LABEL
  # ========================================================
  
  annotate(
    "text",
    x = study.col,
    y = diamond.y,
    label = "Overall REML",
    fontface = "bold",
    hjust = 0,
    size = 3.4
  ) +
    
    annotate(
      "text",
      x = est.col,
      y = diamond.y,
      label = sprintf(
        "%.2f [%.2f, %.2f]",
        pooled,
        pooled_lcl,
        pooled_ucl
      ),
      fontface = "bold",
      hjust = 0,
      size = 3.2
    ) +
    
    
    # ========================================================
  # AXIS AND LABELS
  # ========================================================
  
  labs(
    title = title,
    x = xlab,
    y = NULL
  ) +
    
    
    # --------------------------------------------------------
  # Plot limits
  # --------------------------------------------------------
  
  coord_cartesian(
    xlim = c(
      study.col - 0.05 * xr,
      est.col + 0.35 * xr
    ),
    
    ylim = c(
      diamond.y - 0.8,
      nrow(df) + 2.5
    ),
    
    clip = "off"
  ) +
    
    
    # --------------------------------------------------------
  # Theme
  # --------------------------------------------------------
  
  theme_bw(base_size = 12) +
    
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      
      panel.border = element_blank(),
      
      panel.background = element_blank(),
      plot.background = element_blank(),
      
      plot.margin = margin(
        10,
        130,
        10,
        180
      )
    )
}


# ============================================================
# 4. SLOPE LOO FOREST PLOT
# ============================================================

p.loo.slope <- normal_loo_forest(
  df = loo.slope.df,
  
  full_model = res.slope,
  
  reference_line = 8,
  
  title =
    "(a) Leave-One-Out Sensitivity: LMWL Slope",
  
  xlab =
    expression(
      "LMWL Slope (" * beta * ")"
    )
)


# ============================================================
# 5. INTERCEPT LOO FOREST PLOT
# ============================================================

p.loo.int <- normal_loo_forest(
  df = loo.int.df,
  
  full_model = res.int,
  
 # reference_line = 10,
  
  title =
    "(b) Leave-One-Out Sensitivity: LMWL Intercept",
  
  xlab =
    expression(
      "LMWL Intercept (" * beta[1] * ")"
    )
)


# ============================================================
# 6. DISPLAY INDIVIDUAL FIGURES
# ============================================================

p.loo.slope
p.loo.int


# ============================================================
# 7. COMBINE SLOPE + INTERCEPT
# ============================================================

loo.fig <- p.loo.slope / p.loo.int

loo.fig


# ============================================================
# 8. EXPORT PUBLICATION-QUALITY FIGURE
# ============================================================

ggsave(
  "LOO_Normal_Forest_Full_REML_Diamond.tiff",
  
  loo.fig,
  
  width = 14,
  height = 16,
  
  units = "in",
  
  dpi = 1200,
  
  compression = "lzw"
)


# ============================================================
# COMPACT PUBLICATION-QUALITY LOO FOREST PLOT
# Full REML estimate shown as the diamond
# ============================================================

normal_loo_forest <- function(
    df,
    full_model,
    reference_line,
    title,
    xlab
) {
  
  # ----------------------------------------------------------
  # Order
  # ----------------------------------------------------------
  
  df$Row <- rev(seq_len(nrow(df)))
  
  
  # ----------------------------------------------------------
  # Full REML estimate and CI
  # ----------------------------------------------------------
  
  pooled <- as.numeric(coef(full_model)[1])
  pooled_lcl <- full_model$ci.lb
  pooled_ucl <- full_model$ci.ub
  
  
  # ----------------------------------------------------------
  # Plot range
  # ----------------------------------------------------------
  
  xr <- max(df$UCL) - min(df$LCL)
  
  xmin <- min(df$LCL)
  xmax <- max(df$UCL)
  
  
  # ----------------------------------------------------------
  # Compact column positions
  # ----------------------------------------------------------
  
  study.col <- xmin - 0.75 * xr
  loc.col   <- xmin - 0.30 * xr
  est.col   <- xmax + 0.08 * xr
  
  
  # ----------------------------------------------------------
  # Overall REML diamond
  # ----------------------------------------------------------
  
  diamond.y <- -1.0
  
  diamond <- data.frame(
    x = c(
      pooled_lcl,
      pooled,
      pooled_ucl,
      pooled
    ),
    
    y = c(
      diamond.y,
      diamond.y + 0.30,
      diamond.y,
      diamond.y - 0.30
    )
  )
  
  
  # ==========================================================
  # FOREST PLOT
  # ==========================================================
  
  ggplot(
    df,
    aes(
      x = Estimate,
      y = Row
    )
  ) +
    
    # --------------------------------------------------------
  # LOO confidence intervals
  # --------------------------------------------------------
  
  geom_errorbarh(
    aes(
      xmin = LCL,
      xmax = UCL
    ),
    height = 0.12,
    linewidth = 0.45
  ) +
    
    
    # --------------------------------------------------------
  # LOO estimates
  # --------------------------------------------------------
  
  geom_point(
    size = 2.4,
    shape = 21,
    fill = "gray50",
    stroke = 0.4
  ) +
    
    
    # --------------------------------------------------------
  # Reference value
  # --------------------------------------------------------
  
  #geom_vline(
  #  xintercept = reference_line,
  #  linetype = "dashed",
  #  linewidth = 0.6
  #) +
    
    
    # --------------------------------------------------------
  # Full REML estimate
  # --------------------------------------------------------
  
  geom_vline(
    xintercept = pooled,
    linewidth = 0.8
  ) +
    
    
    # --------------------------------------------------------
  # Full REML diamond
  # --------------------------------------------------------
  
  geom_polygon(
    data = diamond,
    aes(
      x = x,
      y = y
    ),
    inherit.aes = FALSE,
    fill = "navyblue"
  ) +
    
    
    # ========================================================
  # LEFT-HAND INFORMATION
  # ========================================================
  
  geom_text(
    aes(
      x = study.col,
      label = Omitted_Study
    ),
    hjust = 0,
    size = 2.55
  ) +
    
    geom_text(
      aes(
        x = loc.col,
        label = Location
      ),
      hjust = 0,
      size = 2.55
    ) +
    
    
    # ========================================================
  # NUMERICAL RESULTS
  # ========================================================
  
  geom_text(
    aes(
      x = est.col,
      label = sprintf(
        "%.2f [%.2f, %.2f]",
        Estimate,
        LCL,
        UCL
      )
    ),
    hjust = 0,
    size = 2.45
  ) +
    
    
    # ========================================================
  # HEADERS
  # ========================================================
  
  annotate(
    "text",
    x = study.col,
    y = nrow(df) + 1.35,
    label = "Study omitted",
    fontface = "bold",
    hjust = 0,
    size = 2.8
  ) +
    
    annotate(
      "text",
      x = loc.col,
      y = nrow(df) + 1.35,
      label = "Location",
      fontface = "bold",
      hjust = 0,
      size = 2.8
    ) +
    
    annotate(
      "text",
      x = est.col,
      y = nrow(df) + 1.35,
      label = "LOO estimate [95% CI]",
      fontface = "bold",
      hjust = 0,
      size = 2.8
    ) +
    
    
    # ========================================================
  # OVERALL REML LABEL
  # ========================================================
  
  annotate(
    "text",
    x = study.col,
    y = diamond.y,
    label = "Overall DL",
    fontface = "bold",
    hjust = 0,
    size = 2.8
  ) +
    
    annotate(
      "text",
      x = est.col,
      y = diamond.y,
      label = sprintf(
        "%.2f [%.2f, %.2f]",
        pooled,
        pooled_lcl,
        pooled_ucl
      ),
      fontface = "bold",
      hjust = 0,
      size = 2.7
    ) +
    
    
    # ========================================================
  # LABELS
  # ========================================================
  
  labs(
    title = title,
    x = xlab,
    y = NULL
  ) +
    
    
    # ========================================================
  # COMPACT COORDINATES
  # ========================================================
  
  coord_cartesian(
    xlim = c(
      study.col - 0.03 * xr,
      est.col + 0.28 * xr
    ),
    
    ylim = c(
      diamond.y - 0.65,
      nrow(df) + 2.0
    ),
    
    clip = "off"
  ) +
    
    
    # ========================================================
  # PUBLICATION THEME
  # ========================================================
  
  theme_classic(
    base_size = 9.5
  ) +
    
    theme(
      
      # Remove unnecessary y-axis
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      
      # Clean grid
      panel.grid = element_blank(),
      
      # Title
      plot.title = element_text(
        size = 10.5,
        face = "bold",
        hjust = 0
      ),
      
      # X-axis
      axis.title.x = element_text(
        size = 9.5,
        margin = margin(t = 6)
      ),
      
      axis.text.x = element_text(
        size = 8.5
      ),
      
      # Compact margins
      plot.margin = margin(
        5,
        8,
        5,
        8
      )
    )
}


# ============================================================
# SLOPE
# ============================================================

p.loo.slope <- normal_loo_forest(
  
  df = loo.slope.df,
  
  full_model = res.slope,
  
#reference_line = 8,
  
  title = "(a) LMWL slope",
  
  xlab = expression(
    "Slope (" * beta * ")"
  )
)


# ============================================================
# INTERCEPT
# ============================================================

p.loo.int <- normal_loo_forest(
  
  df = loo.int.df,
  
  full_model = res.int,
  
  #reference_line = 10,
  
  title = "(b) LMWL intercept",
  
  xlab = expression(
    "Intercept (" * beta[1] * ")"
  )
)


# ============================================================
# COMBINE
# ============================================================

loo.fig <- p.loo.slope / p.loo.int

loo.fig


# ============================================================
# EXPORT — NORMAL JOURNAL SIZE
# ============================================================

ggsave(
  "LOO_Forest_Publication_Size.tiff",
  
  loo.fig,
  
  width = 7.2,
  height = 8.8,
  
  units = "in",
  
  dpi = 600,
  
  compression = "lzw"
)





##Prepare data
library(metafor)
library(ggplot2)
library(dplyr)
library(patchwork)
# Load libraries

library(metafor)
library(dplyr)

# Create dataset

dat <- data.frame(
  Study = c(
    "Pelig-Ba 2009","Addai et al 2016","Freitaga et al 2008",
    "Ganyaglo et al 2015","Aheampong and Hess 2000","Fynn et al 2015",
    "Yidana et al 2014","Mensah et al 2014","Ganyaglo et al 2015",
    "Afrifa Yamoah 2013","Freitaga et al 2008","Kretchy 2013",
    "Kretchy 2013","Nyarko et al 2010","Freitaga et al 2008",
    "GAEC report","Nyarko et al 2010","Kretchy 2013",
    "Adomako et al 2010","Adomako et al 2011",
    "Akiti 1986","Akiti 1986"
  ),
  
  Location = c(
    "Tamale","Nasia Basin","Axim","Twifo Praso","Afram Plains",
    "Nabogo","West Mamprusi","Ying-Savelugu","Saltpond",
    "Gushiegu","Ejura","Amedzofe","Akatsi","Tindama",
    "Tamale","Atomic","Pwalugu","Kpando",
    "Weija/Nsawam","Koforidua","Aburi","Bolgatanga"
  ),
  
  Latitude = c(
    9.40,10.45,4.87,5.61,6.80,9.55,10.35,9.62,5.21,
    9.92,7.39,6.90,6.11,10.97,9.40,5.80,10.74,
    6.99,5.80,6.09,5.85,10.79
  ),
  
  Slope = c(
    6.788740637,7.079334170,5.787605940,6.862570508,7.024921466,
    7.282675870,7.266346910,7.403514030,6.486435014,7.474410921,
    7.697400875,7.116736782,7.091673770,7.621921714,8.035528500,
    6.397570231,6.787267568,7.450612144,7.692032115,7.800512697,
    7.568612019,8.406638871
  ),
  
  SE_Slope = c(
    0.754558178,0.236730092,0.358544493,0.320969067,0.496972838,
    0.293011431,0.311301502,0.586738725,0.304529499,0.793469147,
    0.384097884,0.333602414,0.372358193,0.026584201,0.408990182,
    0.377540421,0.775802857,0.292005309,0.088279414,0.044786696,
    0.227751057,1.044462192
  ),
  
  Intercept = c(
    0.398473145,3.486498264,3.759578051,3.840855217,4.304125654,
    4.770302097,4.779371550,5.029379703,5.453963819,5.473688361,
    5.584400748,7.115151000,7.472422377,7.869228921,7.928564102,
    8.634115130,9.502174595,10.323681130,10.546412280,11.218665510,
    11.618931090,17.775154950
  ),
  
  SE_Intercept = c(
    1.996953967,1.435593447,0.992699998,0.567258505,2.484988430,
    2.138950498,2.297765511,4.826790681,0.662778565,6.774486899,
    1.368268479,1.152365241,0.846691819,0.087128843,1.627092279,
    1.038591198,2.076491575,0.941729704,0.357307558,0.221117041,
    1.263802453,5.660364776
  ),
  
  Elevation = c(
    200,219,50,100,150,200,166,200,50,180,228,
    455,50,200,200,50,165,156.58,50,238,447,177
  )
)

# Sampling variances

dat$vi_slope <- dat$SE_Slope^2
dat$vi_intercept <- dat$SE_Intercept^2

#---------------------------------------------------


#-----------------------------------------
# Random-effects models
#-----------------------------------------

res.slope <- rma(
  yi = Slope,
  vi = vi_slope,
  method = "DL",
  data = dat
)
tau2.s <- round(res.slope$tau2,3)
I2.s   <- round(res.slope$I2,1)
QEp.s  <- signif(res.slope$QEp,3)

res.int <- rma(
  yi = Intercept,
  vi = vi_intercept,
  method = "DL",
  data = dat
)
tau2.i <- round(res.int$tau2,3)
I2.i   <- round(res.int$I2,1)
QEp.i  <- signif(res.int$QEp,3)
#-----------------------------------------
# Weights
#-----------------------------------------

dat$WeightSlope <-
  round(
    100 * weights(res.slope) /
      sum(weights(res.slope)),
    1
  )

dat$WeightInt <-
  round(
    100 * weights(res.int) /
      sum(weights(res.int)),
    1
  )

#-----------------------------------------
# Confidence intervals
#-----------------------------------------

dat$Slope_LCL <- dat$Slope - 1.96*dat$SE_Slope
dat$Slope_UCL <- dat$Slope + 1.96*dat$SE_Slope

dat$Int_LCL <- dat$Intercept - 1.96*dat$SE_Intercept
dat$Int_UCL <- dat$Intercept + 1.96*dat$SE_Intercept
##2. Forest plot function
forest_gg <- function(df,
                      estimate,
                      lcl,
                      ucl,
                      weight,
                      pooled,
                      pooled.lcl,
                      pooled.ucl,
                      refline,
                      title,
                      xlab){
  
  n <- nrow(df)
  
  df <- df %>%
    arrange(.data[[estimate]])
  
  df$Row <- rev(seq_len(n))
  
  #---------------------------------
  # Overall effect row
  #---------------------------------
  
  overall.row <- 0
  
  diamond <- data.frame(
    x = c(
      pooled.lcl,
      pooled,
      pooled.ucl,
      pooled
    ),
    y = c(
      overall.row,
      overall.row + 0.45,
      overall.row,
      overall.row - 0.45
    )
  )
  
  #---------------------------------
  # Text column positions
  #---------------------------------
  xr <- max(df[[ucl]]) - min(df[[lcl]])
  
  xmin <- min(df[[lcl]])
  xmax <- max(df[[ucl]])
  
  # ---- compressed layout (key change) ----
  study.col <- xmin - 1.55 * xr
  loc.col   <- xmin - 0.55 * xr
  lat.col   <- xmin - 0.05 * xr
  
  wt.col    <- xmax + 0.20 * xr
  beta.col  <- xmax + 0.45 * xr
  
   # xr <- max(df[[ucl]]) - min(df[[lcl]])
  
  #xmin <- min(df[[lcl]])
  #xmax <- max(df[[ucl]])
  
  #study.col <- xmin - 1.50*xr
  #loc.col   <- xmin - 0.80*xr
  #lat.col   <- xmin - 0.25*xr
  
  #wt.col    <- xmax + 0.20*xr
 # beta.col  <- xmax + 0.60*xr
 
  if(title == "(b) LMWL Intercept"){
    
    study.col <- xmin - 1.35*xr
    loc.col   <- xmin - 0.55*xr
    lat.col   <- xmin - 0.4*xr
    
  }
  #p.int <- p.int +
  # coord_cartesian(
  #   xlim = c(-35, 35),
  #   ylim = c(-2, nrow(dat)+3),
  #   clip = "off"
  #  )
  #---------------------------------
  # Plot
  #---------------------------------
  
  ggplot(df,
         aes(
           x = .data[[estimate]],
           y = Row
         )) +
    labs(y = NULL) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )+
    geom_vline(
      xintercept = refline,
      linetype = 2,
      linewidth = 0.9,
      colour = "red"
    ) +
    
    geom_vline(
      xintercept = pooled,
      linewidth = 1.1,
      colour = "navyblue"
    )+
    
    geom_segment(
      aes(
        x = .data[[lcl]],
        xend = .data[[ucl]],
        yend = Row
      ),
      linewidth = 0.5
    ) +
    
    geom_point(
      aes(size = .data[[weight]]),
      shape = 21,
      fill = "gray50"
    ) +
    
    geom_polygon(
      data = diamond,
      aes(x, y),
      inherit.aes = FALSE,
      fill = "navyblue"
    ) +
    annotate(
      "text",
      x = beta.col,
      y = overall.row,
      label = sprintf(
        "%.2f [%.2f, %.2f]",
        pooled,
        pooled.lcl,
        pooled.ucl
      ),
      hjust = 0,
      fontface = "bold",
      size = 3.5
    )+
    geom_text(
      aes(
        x = study.col,
        label = Study
      ),
      hjust = 0,
      size = 3.2
    ) +
    
    geom_text(
      aes(
        x = loc.col,
        label = Location
      ),
      hjust = 0,
      size = 3.2
    ) +
    
    geom_text(
      aes(
        x = wt.col,
        label = sprintf("%.1f", .data[[weight]])
      ),
      size = 3.2
    ) +
    
    geom_text(
      aes(
        x = beta.col,
        label = sprintf(
          "%.2f [%.2f, %.2f]",
          .data[[estimate]],
          .data[[lcl]],
          .data[[ucl]]
        )
      ),
      hjust = 0,
      size = 3.2
    ) +
    
    annotate(
      "text",
      x = study.col,
      y = n + 2,
      label = "Author-Year",
      fontface = "bold",
      hjust = 0
    ) +
    
    annotate(
      "text",
      x = loc.col,
      y = n + 2,
      label = "Location",
      fontface = "bold",
      hjust = 0
    ) +
    
    annotate(
      "text",
      x = wt.col,
      y = n + 2,
      label = "Weight (%)",
      fontface = "bold"
    ) +
    
    annotate(
      "text",
      x = beta.col,
      y = n + 2,
      label = "Effect Size [95% CI]",
      fontface = "bold",
      hjust = 0
    ) +
    
    annotate(
      "text",
      x = study.col,
      y = overall.row,
      label = "Overall DL",
      fontface = "bold",
      hjust = 0
    ) +
    
    scale_size(
      range = c(2,8),
      guide = "none"
    ) +
    
    coord_cartesian(
      xlim = c(
        study.col - 0.05*xr,
        beta.col + 0.40*xr
      ),
      ylim = c(-2, n + 3),
      clip = "off"
    ) +
    
    theme_bw(base_size = 12) +
    
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(10, 70, 10, 90)
    
    ) +
    
    labs(
      title = title,
      x = xlab
    )
}
##3. Create slope panel
p.slope <- forest_gg(
  df = dat,
  estimate = "Slope",
  lcl = "Slope_LCL",
  ucl = "Slope_UCL",
  weight = "WeightSlope",
  pooled = coef(res.slope)[1],
  pooled.lcl = res.slope$ci.lb,
  pooled.ucl = res.slope$ci.ub,
  refline = 8,
  title = "(a) LMWL Slope",
  xlab = expression(
    paste(
      "LMWL Slope (", beta, ")"
    )
  ))
  #xlab = expression(beta))
##4. Create intercept panel
p.int <- forest_gg(
  df = dat,
  estimate = "Intercept",
  lcl = "Int_LCL",
  ucl = "Int_UCL",
  weight = "WeightInt",
  pooled = coef(res.int)[1],
  pooled.lcl = res.int$ci.lb,
  pooled.ucl = res.int$ci.ub,
  refline = 10,
  title = "(b) LMWL Intercept",
  xlab = expression(
    paste(
      "LMWL Intercept (", beta[1], ")"
    )
  ))
  #xlab = expression(beta[1]))

##5. Combine figure
fig <- p.slope / p.int

fig
##6. Export
ggsave(
  "Ghana_LMWL_Forest_MetaAnalysis.tiff",
  fig,
  width = 14,
  height = 16,
  units = "in",
  dpi = 1200,
  compression = "lzw"
)






################################################################################


##1. Run Leave-One-Out Meta-analysis
library(metafor)
library(dplyr)
library(ggplot2)
library(patchwork)
# Create dataset

dat <- data.frame(
  Study = c(
    "Pelig-Ba 2009","Addai et al 2016","Freitaga et al 2008",
    "Ganyaglo et al 2015","Aheampong and Hess 2000","Fynn et al 2015",
    "Yidana et al 2014","Mensah et al 2014","Ganyaglo et al 2015",
    "Afrifa Yamoah 2013","Freitaga et al 2008","Kretchy 2013",
    "Kretchy 2013","Nyarko et al 2010","Freitaga et al 2008",
    "GAEC report","Nyarko et al 2010","Kretchy 2013",
    "Adomako et al 2010","Adomako et al 2011",
    "Akiti 1986","Akiti 1986"
  ),
  
  Location = c(
    "Tamale","Nasia Basin","Axim","Twifo Praso","Afram Plains",
    "Nabogo","West Mamprusi","Ying-Savelugu","Saltpond",
    "Gushiegu","Ejura","Amedzofe","Akatsi","Tindama",
    "Tamale","Atomic","Pwalugu","Kpando",
    "Weija/Nsawam","Koforidua","Aburi","Bolgatanga"
  ),
  
  Latitude = c(
    9.40,10.45,4.87,5.61,6.80,9.55,10.35,9.62,5.21,
    9.92,7.39,6.90,6.11,10.97,9.40,5.80,10.74,
    6.99,5.80,6.09,5.85,10.79
  ),
  
  Slope = c(
    6.788740637,7.079334170,5.787605940,6.862570508,7.024921466,
    7.282675870,7.266346910,7.403514030,6.486435014,7.474410921,
    7.697400875,7.116736782,7.091673770,7.621921714,8.035528500,
    6.397570231,6.787267568,7.450612144,7.692032115,7.800512697,
    7.568612019,8.406638871
  ),
  
  SE_Slope = c(
    0.754558178,0.236730092,0.358544493,0.320969067,0.496972838,
    0.293011431,0.311301502,0.586738725,0.304529499,0.793469147,
    0.384097884,0.333602414,0.372358193,0.026584201,0.408990182,
    0.377540421,0.775802857,0.292005309,0.088279414,0.044786696,
    0.227751057,1.044462192
  ),
  
  Intercept = c(
    0.398473145,3.486498264,3.759578051,3.840855217,4.304125654,
    4.770302097,4.779371550,5.029379703,5.453963819,5.473688361,
    5.584400748,7.115151000,7.472422377,7.869228921,7.928564102,
    8.634115130,9.502174595,10.323681130,10.546412280,11.218665510,
    11.618931090,17.775154950
  ),
  
  SE_Intercept = c(
    1.996953967,1.435593447,0.992699998,0.567258505,2.484988430,
    2.138950498,2.297765511,4.826790681,0.662778565,6.774486899,
    1.368268479,1.152365241,0.846691819,0.087128843,1.627092279,
    1.038591198,2.076491575,0.941729704,0.357307558,0.221117041,
    1.263802453,5.660364776
  ),
  
  Elevation = c(
    200,219,50,100,150,200,166,200,50,180,228,
    455,50,200,200,50,165,156.58,50,238,447,177
  )
)

# Sampling variances

dat$vi_slope <- dat$SE_Slope^2
dat$vi_intercept <- dat$SE_Intercept^2

# Full models
library(metafor)
library(dplyr)
library(ggplot2)
library(patchwork)

res.slope <- rma(Slope, vi = vi_slope, method="REML", data=dat)
res.int   <- rma(Intercept, vi = vi_intercept, method="REML", data=dat)

loo.slope <- leave1out(res.slope)
loo.int   <- leave1out(res.int)

#Build LOO datasets (FIXED & robust)
df.slope <- data.frame(
  Study = dat$Study,
  Location = dat$Location,
  Estimate = loo.slope$estimate,
  SE = loo.slope$se
)

df.int <- data.frame(
  Study = dat$Study,
  Location = dat$Location,
  Estimate = loo.int$estimate,
  SE = loo.int$se
)

df.slope$LCL <- df.slope$Estimate - 1.96*df.slope$SE
df.slope$UCL <- df.slope$Estimate + 1.96*df.slope$SE

df.int$LCL <- df.int$Estimate - 1.96*df.int$SE
df.int$UCL <- df.int$Estimate + 1.96*df.int$SE


#TEP 1 — compute LOO pooled summary
# LOO pooled estimate (average of leave-one-out estimates)
loo.pool.slope <- mean(df.slope$Estimate, na.rm = TRUE)
loo.pool.int   <- mean(df.int$Estimate, na.rm = TRUE)

# LOO variability (for diamond CI)
loo.se.slope <- sd(df.slope$Estimate, na.rm = TRUE)
loo.se.int   <- sd(df.int$Estimate, na.rm = TRUE)

# CI for LOO pooled effect
loo.slope.lcl <- loo.pool.slope - 1.96 * loo.se.slope
loo.slope.ucl <- loo.pool.slope + 1.96 * loo.se.slope

loo.int.lcl <- loo.pool.int - 1.96 * loo.se.int
loo.int.ucl <- loo.pool.int + 1.96 * loo.se.int
#3. Weights + pooled estimates
df.slope$Weight <- round(100*weights(res.slope)/sum(weights(res.slope)),1)
df.int$Weight   <- round(100*weights(res.int)/sum(weights(res.int)),1)

ref.slope <- coef(res.slope)[1]
ref.int   <- coef(res.int)[1]

loo_forest <- function(df,
                       ref_full,
                       ref_global,
                       loo_pool,
                       loo_lcl,
                       loo_ucl,
                       title,
                       xlab){
  
  df$Row <- rev(seq_len(nrow(df)))
  
  xr <- max(df$UCL) - min(df$LCL)
  
  xmin <- min(df$LCL)
  xmax <- max(df$UCL)
  
  study.col <- xmin - 1*xr
  loc.col   <- xmin - 0.35*xr
  wt.col    <- xmax + 0.15*xr
  est.col   <- xmax + 0.50*xr
  
  # pooled row position
  pooled.y <- -0.8
  
  # LOO diamond data
  diamond <- data.frame(
    x = c(loo_lcl, loo_pool, loo_ucl, loo_pool),
    y = c(0, 0.35, 0, -0.35)
  )
  
  ggplot(df, aes(x = Estimate, y = Row)) +
    
    # CI
    geom_errorbarh(
      aes(xmin = LCL, xmax = UCL),
      height = 0.15
    ) +
    
    # points
    geom_point(
      aes(size = Weight),
      shape = 21,
      fill = "gray60"
    ) +
    
    # =========================
  # FULL MODEL (NO LOO)
  # =========================
  geom_vline(
    xintercept = ref_full,
    linewidth = 1.3
  ) +
    
    
    # =========================
  # LOO DIAMOND
  # =========================
  geom_polygon(
    data = diamond,
    aes(x, y),
    inherit.aes = FALSE,
    fill = "navyblue"
  ) +
    
    # STUDY
    geom_text(aes(x = study.col, label = Study),
              hjust = 0, size = 3.2) +
    
    # LOCATION
    geom_text(aes(x = loc.col, label = Location),
              hjust = 0, size = 3.2) +
    
    # WEIGHT
    geom_text(aes(x = wt.col, label = sprintf("%.1f", Weight)),
              size = 3.2) +
    
    # EFFECT
    geom_text(aes(x = est.col,
                  label = sprintf("%.2f [%.2f, %.2f]", Estimate, LCL, UCL)),
              hjust = 0, size = 3.2) +
    
    # HEADERS
    annotate("text", x = study.col, y = nrow(df)+2,
             label = "Author-Year", fontface="bold", hjust=0) +
    
    annotate("text", x = loc.col, y = nrow(df)+2,
             label = "Location", fontface="bold", hjust=0) +
    
    annotate("text", x = wt.col, y = nrow(df)+2,
             label = "Weight (%)", fontface="bold") +
    
    annotate("text", x = est.col, y = nrow(df)+2,
             label = "Estimate [95% CI]", fontface="bold", hjust=0) +
    
    # =========================
  # POOL LABELS
  # =========================
      annotate("text",
             x = loo_pool,
             y = pooled.y - 0.5,
             label = paste0("LOO pooled = ", round(loo_pool,2)),
             fontface="bold",
             size=3.5) +
    
    scale_size(range=c(2,7), guide="none") +
    
    coord_cartesian(
      xlim = c(study.col - 0.05*xr, est.col + 0.25*xr),
      ylim = c(-1.5, nrow(df)+3),
      clip = "off"
    ) +
    
    theme_bw(base_size = 12) +
    
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(10, 70, 10, 90)
        ) +
    
    labs(title = title, x = xlab, y = NULL)
}

p.slope <- loo_forest(
  df.slope,
  ref_full = ref.slope,
  ref_global = 8,
  loo_pool = loo.pool.slope,
  loo_lcl = loo.slope.lcl,
  loo_ucl = loo.slope.ucl,
  title = "(a) LOO Sensitivity: LMWL Slope",
  xlab = expression(beta)
)

p.int <- loo_forest(
  df.int,
  ref_full = ref.int,
  ref_global = 10,
  loo_pool = loo.pool.int,
  loo_lcl = loo.int.lcl,
  loo_ucl = loo.int.ucl,
  title = "(b) LOO Sensitivity: LMWL Intercept",
  xlab = expression(beta[1])
)


#3. Combine
fig <- p.slope / p.int
fig
#4. Export (journal-ready)
ggsave(
  "LOO_Forest_Final_Publication2.tiff",
  fig,
  width = 14,
  height = 16,
  dpi = 1200,
  compression = "lzw"
)

#########################################################################











































###############################################################
## Figure 6
## Nature-quality Meta-regression
##
## Part I
## Data preparation
## Model fitting
## Prediction datasets
###############################################################

##============================================================
## Packages
##============================================================

library(tidyverse)

library(metafor)

library(ggplot2)

library(ggrepel)

library(patchwork)
#install.packages("patchwork")
library(cowplot)

library(scales)

library(glue)

library(grid)

library(gridExtra)

library(viridis)

theme_set(theme_bw())

##============================================================
## Study data
##============================================================

data <- data.frame(
  
  Study=c(
    "Pelig-Ba 2009",
    "Addai et al 2016",
    "Freitaga et al 2008",
    "Ganyaglo et al 2015",
    "Aheampong and Hess 2000",
    "Fynn et al 2015",
    "Yidana et al 2014",
    "Mensah et al 2014",
    "Ganyaglo et al 2015",
    "Afrifa Yamoah 2013",
    "Freitaga et al 2008",
    "Kretchy 2013",
    "Kretchy 2013",
    "Nyarko et al 2010",
    "Freitaga et al 2008",
    "GAEC report",
    "Nyarko et al 2010",
    "Kretchy 2013",
    "Adomako et al 2010",
    "Adomako et al 2011",
    "Akiti 1986",
    "Akiti 1986"
  ),
  
  Lat=c(
    9.40,
    10.45,
    4.87,
    5.61,
    6.80,
    9.55,
    10.35,
    9.62,
    5.21,
    9.92,
    7.39,
    6.90,
    6.11,
    10.97,
    9.40,
    5.80,
    10.74,
    6.99,
    5.80,
    6.09,
    5.85,
    10.79
  ),
  
  Elevation=c(
    200,
    219,
    50,
    100,
    150,
    200,
    166,
    200,
    50,
    180,
    228,
    455,
    50,
    200,
    200,
    50,
    165,
    156.58,
    50,
    238,
    447,
    177
  ),
  
  Slope=c(
    6.788740637,
    7.079334170,
    5.787605940,
    6.862570508,
    7.024921466,
    7.282675870,
    7.266346910,
    7.403514030,
    6.486435014,
    7.474410921,
    7.697400875,
    7.116736782,
    7.091673770,
    7.621921714,
    8.035528500,
    6.397570231,
    6.787267568,
    7.450612144,
    7.692032115,
    7.800512697,
    7.568612019,
    8.406638871
  ),
  
  SE=c(
    0.754558178,
    0.236730092,
    0.358544493,
    0.320969067,
    0.496972838,
    0.293011431,
    0.311301502,
    0.586738725,
    0.304529499,
    0.793469147,
    0.384097884,
    0.333602414,
    0.372358193,
    0.026584201,
    0.408990182,
    0.377540421,
    0.775802857,
    0.292005309,
    0.088279414,
    0.044786696,
    0.227751057,
    1.044462192
  ),
  
  Location=c(
    "Tamale",
    "Nasia Basin",
    "Axim",
    "Twifo Praso",
    "Afram Plains",
    "Nabogo",
    "West Mamprusi",
    "Ying-Savelugu",
    "Saltpond",
    "Gushiegu",
    "Ejura",
    "Amedzofe",
    "Akatsi",
    "Tindama",
    "Tamale",
    "Atomic",
    "Pwalugu",
    "Kpando",
    "Weija/Nsawam",
    "Koforidua",
    "Aburi",
    "Bolgatanga"
  )
  
)

###############################################################
## Intercept dataset
###############################################################

intercept <- data.frame(
  
  Location=data$Location,
  
  Lat=data$Lat,
  
  Elevation=data$Elevation,
  
  Intercept=c(
    0.398473145,
    3.486498264,
    3.759578051,
    3.840855217,
    4.304125654,
    4.770302097,
    4.779371550,
    5.029379703,
    5.453963819,
    5.473688361,
    5.584400748,
    7.115151000,
    7.472422377,
    7.869228921,
    7.928564102,
    8.634115130,
    9.502174595,
    10.323681130,
    10.546412280,
    11.218665510,
    11.618931090,
    17.775154950
  ),
  
  SE=c(
    1.996953967,
    1.435593447,
    0.992699998,
    0.567258505,
    2.484988430,
    2.138950498,
    2.297765511,
    4.826790681,
    0.662778565,
    6.774486899,
    1.368268479,
    1.152365241,
    0.846691819,
    0.087128843,
    1.627092279,
    1.038591198,
    2.076491575,
    0.941729704,
    0.357307558,
    0.221117041,
    1.263802453,
    5.660364776
  )
  
)

###############################################################
## Meta-regression models
###############################################################

m.slope.lat <- rma(
  yi=Slope,
  sei=SE,
  mods=~Lat,
  data=data,
  method="DL"
)

m.slope.elev <- rma(
  yi=Slope,
  sei=SE,
  mods=~Elevation,
  data=data,
  method="DL"
)

m.int.lat <- rma(
  yi=Intercept,
  sei=SE,
  mods=~Lat,
  data=intercept,
  method="DL"
)

m.int.elev <- rma(
  yi=Intercept,
  sei=SE,
  mods=~Elevation,
  data=intercept,
  method="DL"
)

###############################################################
## Null models
###############################################################

null.slope <- rma(
  yi=data$Slope,
  sei=data$SE,
  method="DL"
)

null.int <- rma(
  yi=intercept$Intercept,
  sei=intercept$SE,
  method="DL"
)

###############################################################
## Pseudo-R2
###############################################################

R2.slope.lat <- 100 *
  (null.slope$tau2-m.slope.lat$tau2)/
  null.slope$tau2

R2.slope.elev <- 100 *
  (null.slope$tau2-m.slope.elev$tau2)/
  null.slope$tau2

R2.int.lat <- 100 *
  (null.int$tau2-m.int.lat$tau2)/
  null.int$tau2

R2.int.elev <- 100 *
  (null.int$tau2-m.int.elev$tau2)/
  null.int$tau2

###############################################################
## Study weights
###############################################################

data$Weight <- weights(m.slope.lat)

intercept$Weight <- weights(m.int.lat)

###############################################################
## Prediction grids
###############################################################

lat.grid <- tibble(
  
  Lat=seq(
    min(data$Lat),
    max(data$Lat),
    length.out=300)
  
)

elev.grid <- tibble(
  
  Elevation=seq(
    0,
    500,
    length.out=300)
  
)

###############################################################
## Predictions
###############################################################

pred.slope.lat <- predict(
  m.slope.lat,
  newmods=lat.grid$Lat
)

pred.slope.elev <- predict(
  m.slope.elev,
  newmods=elev.grid$Elevation
)

pred.int.lat <- predict(
  m.int.lat,
  newmods=lat.grid$Lat
)

pred.int.elev <- predict(
  m.int.elev,
  newmods=elev.grid$Elevation
)

###############################################################
## Plot datasets
###############################################################

plot.lat.slope <-
  
  bind_cols(
    lat.grid,
    as.data.frame(pred.slope.lat)
  )

plot.elev.slope <-
  
  bind_cols(
    elev.grid,
    as.data.frame(pred.slope.elev)
  )

plot.lat.int <-
  
  bind_cols(
    lat.grid,
    as.data.frame(pred.int.lat)
  )

plot.elev.int <-
  
  bind_cols(
    elev.grid,
    as.data.frame(pred.int.elev)
  )

###############################################################
## Helper functions
###############################################################

fmt_p <- function(p){
  
  if(p<0.001){
    
    return("P < 0.001")
    
  }
  
  paste0("P = ",sprintf("%.3f",p))
  
}

###############################################################
## Extract model statistics
###############################################################

stats <- tibble(
  
  Model=c(
    "SlopeLat",
    "SlopeElev",
    "IntLat",
    "IntElev"
  ),
  
  R2=c(
    R2.slope.lat,
    R2.slope.elev,
    R2.int.lat,
    R2.int.elev
  ),
  
  P=c(
    coef(summary(m.slope.lat))[2,4],
    coef(summary(m.slope.elev))[2,4],
    coef(summary(m.int.lat))[2,4],
    coef(summary(m.int.elev))[2,4]
  )
  
)

stats


###############################################################
## PART II
## Nature-style ggplot theme
## Panels A and B
###############################################################

library(ggrepel)
library(patchwork)

###############################################################
## Nature theme
###############################################################

theme_nature <- function(base_size = 9){
  
  theme_classic(base_size = base_size) +
    
    theme(
      
      text = element_text(
        family = "sans",
        colour = "black"
      ),
      
      axis.title = element_text(
        face = "bold",
        size = base_size + 1
      ),
      
      axis.text = element_text(
        size = base_size,
        colour = "black"
      ),
      
      axis.line = element_line(
        linewidth = 0.5,
        colour = "black"
      ),
      
      axis.ticks = element_line(
        linewidth = 0.5
      ),
      
      panel.border = element_blank(),
      
      legend.position = "bottom",
      
      legend.title = element_blank(),
      
      legend.key = element_blank(),
      
      plot.title = element_text(
        face = "bold",
        size = base_size + 2,
        hjust = 0
      ),
      
      plot.tag = element_text(
        face = "bold",
        size = base_size + 3
      )
      
    )
  
}

###############################################################
## Point sizes proportional to study weight
###############################################################

data$PointSize <-
  rescale(
    data$Weight,
    to = c(2.5,7)
  )

intercept$PointSize <-
  rescale(
    intercept$Weight,
    to = c(2.5,7)
  )

###############################################################
## Label only influential observations
###############################################################

pred <- predict(m.slope.lat)

residuals.lat <-
  abs(data$Slope - pred$pred)

data$Label <- ifelse(
  
  residuals.lat >
    quantile(residuals.lat,0.90),
  
  data$Location,
  
  ""
  
)

###############################################################
## Annotation helper
###############################################################

annot_text <- function(R2,P){
  
  glue(
    "italic(R)^2 == '{round(R2,1)}%'*','~~
italic(P)== '{ifelse(P<0.001,'<0.001',
sprintf('%.3f',P))}'"
  )
  
}

###############################################################
## Colour palette
###############################################################

colSlope <- "#C0392B"

fillSlope <- "#F5B7B1"

predictionFill <- "grey82"

###############################################################
## Panel A
###############################################################

panelA <-
  
  ggplot() +
  
  ## Prediction interval
  geom_ribbon(
    
    data = plot.lat.slope,
    
    aes(
      x = Lat,
      ymin = pi.lb,
      ymax = pi.ub
    ),
    
    fill = predictionFill,
    
    alpha = 0.35
    
  ) +
  
  ## Confidence interval
  geom_ribbon(
    
    data = plot.lat.slope,
    
    aes(
      x = Lat,
      ymin = ci.lb,
      ymax = ci.ub
    ),
    
    fill = fillSlope,
    
    alpha = 0.35
    
  ) +
  
  ## Regression line
  geom_line(
    
    data = plot.lat.slope,
    
    aes(
      x = Lat,
      y = pred
    ),
    
    colour = colSlope,
    
    linewidth = 1.3
    
  ) +
  
  ## Data points
  geom_point(
    
    data = data,
    
    aes(
      x = Lat,
      y = Slope,
      size = Weight
    ),
    
    shape = 21,
    
    fill = "white",
    
    colour = "black",
    
    stroke = 0.5
    
  ) +
  
  scale_size_continuous(
    
    range = c(2.5,7),
    
    name = "Study weight"
    
  ) +
  
  ## Labels
  geom_text_repel(
    
    data = subset(data, Label != ""),
    
    aes(
      x = Lat,
      y = Slope,
      label = Label
    ),
    
    size = 3,
    
    box.padding = 0.35,
    
    point.padding = 0.25,
    
    segment.size = 0.25,
    
    max.overlaps = Inf
    
  ) +
  
  ## Statistics
  annotate(
    
    "text",
    
    x = 4.25,
    
    y = 8.55,
    
    label = annot_text(
      
      R2.slope.lat,
      
      coef(summary(m.slope.lat))[2,4]
      
    ),
    
    parse = TRUE,
    
    hjust = 0,
    
    size = 3.5
    
  ) +
  
  coord_cartesian(
    
    xlim = c(4,12),
    
    ylim = c(5.7,8.7)
    
  ) +
  
  labs(
    
    x = "Absolute latitude (°N)",
    
    y = "LMWL slope",
    
    tag = "(a)"
    
  ) +
  
  ############################################################
## Nature-style publication theme
############################################################

theme_bw(base_size = 12) +
  
  theme(
    
    text = element_text(family = "sans"),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.border = element_blank(),
    
    ## Grid lines
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.35
    ),
    
    panel.grid.minor = element_line(
      colour = "grey94",
      linewidth = 0.20
    ),
    
    ## Axes
    axis.line = element_line(
      colour = "black",
      linewidth = 0.9
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.8
    ),
    
    axis.ticks.length = unit(2.5, "mm"),
    
    ## Axis labels
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      size = 10,
      colour = "black"
    ),
    
    ## Panel tag
    plot.tag = element_text(
      face = "bold",
      size = 14
    ),
    
    ## Legend
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold",
      size = 10
    ),
    
    legend.text = element_text(
      size = 9
    )
    
  )

  ###############################################################
  ## Panel B
  ###############################################################
  
  pred2 <- predict(m.slope.elev)
  
  residuals.elev <-
    abs(data$Slope - pred2$pred)
  
  data$Label2 <- ifelse(
    
    residuals.elev >
      quantile(residuals.elev, 0.90),
    
    data$Location,
    
    ""
    
  )
  
  panelB <-
    
    ggplot() +
    
    ## Prediction interval
    geom_ribbon(
      
      data = plot.elev.slope,
      
      aes(
        x = Elevation,
        ymin = pi.lb,
        ymax = pi.ub
      ),
      
      fill = predictionFill,
      
      alpha = 0.35
      
    ) +
    
    ## Confidence interval
    geom_ribbon(
      
      data = plot.elev.slope,
      
      aes(
        x = Elevation,
        ymin = ci.lb,
        ymax = ci.ub
      ),
      
      fill = fillSlope,
      
      alpha = 0.35
      
    ) +
    
    ## Regression line
    geom_line(
      
      data = plot.elev.slope,
      
      aes(
        x = Elevation,
        y = pred
      ),
      
      colour = colSlope,
      
      linewidth = 1.3
      
    ) +
    
    ## Observations
    geom_point(
      
      data = data,
      
      aes(
        x = Elevation,
        y = Slope,
        size = Weight
      ),
      
      shape = 21,
      
      fill = "white",
      
      colour = "black",
      
      stroke = 0.5
      
    ) +
    
    scale_size_continuous(
      
      range = c(2.5, 7),
      
      name = "Study weight"
      
    ) +
    
    ## Labels
    geom_text_repel(
      
      data = subset(data, Label2 != ""),
      
      aes(
        x = Elevation,
        y = Slope,
        label = Label2
      ),
      
      size = 3,
      
      box.padding = 0.35,
      
      point.padding = 0.25,
      
      segment.size = 0.25,
      
      max.overlaps = Inf
      
    ) +
    
    ## Statistics
    annotate(
      
      "text",
      
      x = 15,
      
      y = 8.55,
      
      label = annot_text(
        
        R2.slope.elev,
        
        coef(summary(m.slope.elev))[2,4]
        
      ),
      
      parse = TRUE,
      
      hjust = 0,
      
      size = 3.5
      
    ) +
    
    coord_cartesian(
      
      xlim = c(0, 500),
      
      ylim = c(5.7, 8.7)
      
    ) +
    
    labs(
      
      x = "Elevation (m a.s.l.)",
      
      y = NULL,
      
      tag = "(b)"
      
    ) +
    
    ############################################################
  ## Nature-style publication theme
  ############################################################
  
  theme_bw(base_size = 12) +
    
    theme(
      
      text = element_text(family = "sans"),
      
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      panel.border = element_blank(),
      
      ## Grid lines
      panel.grid.major = element_line(
        colour = "grey85",
        linewidth = 0.35
      ),
      
      panel.grid.minor = element_line(
        colour = "grey94",
        linewidth = 0.20
      ),
      
      ## Axes
      axis.line = element_line(
        colour = "black",
        linewidth = 0.9
      ),
      
      axis.ticks = element_line(
        colour = "black",
        linewidth = 0.8
      ),
      
      axis.title = element_text(
        face = "bold",
        size = 12
      ),
      
      axis.text = element_text(
        size = 10,
        colour = "black"
      ),
      
      ## Panel label
      plot.tag = element_text(
        face = "bold",
        size = 14
      ),
      
      ## Legend
      legend.position = "bottom",
      
      legend.title = element_text(
        face = "bold",
        size = 10
      ),
      
      legend.text = element_text(
        size = 9
      )
      
    )


###############################################################
## PART III
## Panels C and D
## Assemble figure
## Export publication-quality files
###############################################################

###############################################################
## Colour palette
###############################################################

colIntercept <- "#2166AC"

fillIntercept <- "#C6DBEF"

###############################################################
## Labels for intercept models
###############################################################

pred3 <- predict(m.int.lat)

intercept$Label <- ifelse(
  
  abs(intercept$Intercept-pred3$pred) >
    quantile(abs(intercept$Intercept-pred3$pred),0.90),
  
  intercept$Location,
  
  ""
  
)
###############################################################
## Panel C
###############################################################

panelC <-
  
  ggplot() +
  
  ## Prediction interval
  geom_ribbon(
    data = plot.lat.int,
    aes(
      x = Lat,
      ymin = pi.lb,
      ymax = pi.ub
    ),
    fill = "grey85",
    alpha = 0.35
  ) +
  
  ## Confidence interval
  geom_ribbon(
    data = plot.lat.int,
    aes(
      x = Lat,
      ymin = ci.lb,
      ymax = ci.ub
    ),
    fill = fillIntercept,
    alpha = 0.35
  ) +
  
  ## Regression line
  geom_line(
    data = plot.lat.int,
    aes(
      x = Lat,
      y = pred
    ),
    colour = colIntercept,
    linewidth = 1.3
  ) +
  
  ## Observations
  geom_point(
    data = intercept,
    aes(
      x = Lat,
      y = Intercept,
      size = Weight
    ),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.5
  ) +
  
  scale_size_continuous(
    range = c(2.5,7),
    name = "Study weight"
  ) +
  
  ## Labels
  geom_text_repel(
    data = subset(intercept, Label != ""),
    aes(
      x = Lat,
      y = Intercept,
      label = Label
    ),
    size = 3,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.size = 0.25,
    max.overlaps = Inf
  ) +
  
  ## Statistics
  annotate(
    "text",
    x = 4.25,
    y = 19,
    label = annot_text(
      R2.int.lat,
      coef(summary(m.int.lat))[2,4]
    ),
    parse = TRUE,
    hjust = 0,
    size = 3.5
  ) +
  
  coord_cartesian(
    xlim = c(4,12),
    ylim = c(-1,20)
  ) +
  
  labs(
    x = "Absolute latitude (°N)",
    y = "LMWL intercept (‰)",
    tag = "(c)"
  ) +
  
  ############################################################
## Nature-style theme with grid lines
############################################################

theme_bw(base_size = 12) +
  
  theme(
    
    text = element_text(family = "sans"),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    panel.border = element_blank(),
    
    ## Grid lines
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.35
    ),
    
    panel.grid.minor = element_line(
      colour = "grey94",
      linewidth = 0.20
    ),
    
    ## Axes
    axis.line = element_line(
      colour = "black",
      linewidth = 0.9
    ),
    
    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.8
    ),
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      size = 10,
      colour = "black"
    ),
    
    ## Panel tag
    plot.tag = element_text(
      face = "bold",
      size = 14
    ),
    
    legend.position = "bottom"
  )
###############################################################
## Labels for elevation model
###############################################################

pred4 <- predict(m.int.elev)

intercept$Label2 <- ifelse(
  
  abs(intercept$Intercept-pred4$pred) >
    quantile(abs(intercept$Intercept-pred4$pred),0.90),
  
  intercept$Location,
  
  ""
  
)

###############################################################
## Panel D
###############################################################

panelD <-
  
  ggplot() +
  
  geom_ribbon(
    
    data=plot.elev.int,
    
    aes(
      
      Elevation,
      
      ymin=pi.lb,
      
      ymax=pi.ub
      
    ),
    
    fill="grey82",
    
    alpha=.45
    
  )+
  
  geom_ribbon(
    
    data=plot.elev.int,
    
    aes(
      
      Elevation,
      
      ymin=ci.lb,
      
      ymax=ci.ub
      
    ),
    
    fill=fillIntercept,
    
    alpha=.45
    
  )+
  
  geom_line(
    
    data=plot.elev.int,
    
    aes(
      
      Elevation,
      
      pred
      
    ),
    
    linewidth=1.2,
    
    colour=colIntercept
    
  )+
  
  geom_point(
    
    data=intercept,
    
    aes(
      
      Elevation,
      
      Intercept,
      
      size=Weight
      
    ),
    
    shape=21,
    
    fill="white",
    
    colour="black",
    
    stroke=.45
    
  )+
  
  scale_size_continuous(
    
    range=c(2.5,7),
    
    name="Study weight"
    
  )+
  
  geom_text_repel(
    
    data=subset(intercept,Label2!=""),
    
    aes(
      
      Elevation,
      
      Intercept,
      
      label=Label2
      
    ),
    
    size=3,
    
    box.padding=.35,
    
    point.padding=.25,
    
    segment.size=.25,
    
    max.overlaps=Inf
    
  )+
  
  annotate(
    
    "text",
    
    x=15,
    
    y=19,
    
    label=annot_text(
      
      R2.int.elev,
      
      coef(summary(m.int.elev))[2,4]
      
    ),
    
    parse=TRUE,
    
    hjust=0,
    
    size=3.5
    
  )+
  
  coord_cartesian(
    
    xlim=c(0,500),
    
    ylim=c(-1,20)
    
  )+
  
  labs(
    
    x="Elevation (m a.s.l.)",
    
    y=NULL
    
  )+
  
  theme_nature()

  
  labs(tag = "(d)")
  ###############################################################
## Panel D
###############################################################

panelD <-

ggplot() +

  ## Prediction interval
  geom_ribbon(
    data = plot.elev.int,
    aes(
      x = Elevation,
      ymin = pi.lb,
      ymax = pi.ub
    ),
    fill = "grey85",
    alpha = 0.35
  ) +

  ## Confidence interval
  geom_ribbon(
    data = plot.elev.int,
    aes(
      x = Elevation,
      ymin = ci.lb,
      ymax = ci.ub
    ),
    fill = fillIntercept,
    alpha = 0.35
  ) +

  ## Regression line
  geom_line(
    data = plot.elev.int,
    aes(
      x = Elevation,
      y = pred
    ),
    colour = colIntercept,
    linewidth = 1.3
  ) +

  ## Points
  geom_point(
    data = intercept,
    aes(
      x = Elevation,
      y = Intercept,
      size = Weight
    ),
    shape = 21,
    fill = "white",
    colour = "black",
    stroke = 0.5
  ) +

  scale_size_continuous(
    range = c(2.5,7),
    name = "Study weight"
  ) +

  ## Labels
  geom_text_repel(
    data = subset(intercept, Label2 != ""),
    aes(
      x = Elevation,
      y = Intercept,
      label = Label2
    ),
    size = 3,
    box.padding = 0.35,
    point.padding = 0.25,
    segment.size = 0.25,
    max.overlaps = Inf
  ) +

  ## Statistics
  annotate(
    "text",
    x = 15,
    y = 19,
    label = annot_text(
      R2.int.elev,
      coef(summary(m.int.elev))[2,4]
    ),
    parse = TRUE,
    hjust = 0,
    size = 3.5
  ) +

  coord_cartesian(
    xlim = c(0,500),
    ylim = c(-1,20)
  ) +

  labs(
    x = "Elevation (m a.s.l.)",
    y = NULL,
    tag = "(d)"
  ) +

  ############################################################
  ## Nature-style theme with gridlines
  ############################################################

  theme_bw(base_size = 12) +

  theme(

    text = element_text(family = "sans"),

    panel.border = element_blank(),

    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),

    ## GRID LINES
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.35
    ),

    panel.grid.minor = element_line(
      colour = "grey94",
      linewidth = 0.20
    ),

    ## AXES
    axis.line = element_line(
      colour = "black",
      linewidth = 0.9
    ),

    axis.ticks = element_line(
      colour = "black",
      linewidth = 0.8
    ),

    axis.title = element_text(
      face = "bold",
      size = 12
    ),

    axis.text = element_text(
      size = 10,
      colour = "black"
    ),

    ## PANEL LABEL
    plot.tag = element_text(
      face = "bold",
      size = 14
    ),

    legend.position = "bottom"
  )

###############################################################
## Shared legend
###############################################################

legend_plot <-
  
  ggplot(
    
    data.frame(
      
      x=1,
      
      y=1,
      
      w=c(1,3,5)
      
    ),
    
    aes(x,y)
    
  )+
  
  geom_point(
    
    aes(size=w),
    
    shape=21,
    
    fill="white",
    
    colour="black"
    
  )+
  
  scale_size_continuous(
    
    range=c(2.5,7),
    
    name="Study weight"
    
  )+
  
  theme_void()


legend <- cowplot::get_legend(legend_plot)

###############################################################
## Assemble figure
###############################################################

figure6 <-
  (
    panelA + panelB
  ) /
  (
    panelC + panelD
  ) +
  plot_layout(guides = "collect")

figure6 <- figure6 &
  theme(
    legend.position = "bottom"
  )

###############################################################
## Display
###############################################################

figure6

###############################################################
## Export
###############################################################

ggsave(
  
  filename="Figure6_MetaRegression_Nature.pdf",
  
  plot=figure6,
  
  device=cairo_pdf,
  
  width=180,
  
  height=180,
  
  units="mm"
  
)

ggsave(
  
  filename="Figure6_MetaRegression_Nature.png",
  
  plot=figure6,
  
  device="png",
  
  width=180,
  
  height=180,
  
  units="mm"
  
)

ggsave(
  
  filename="Figure6_MetaRegression_Nature.tiff",
  
  plot=figure6,
  
  dpi=600,
  
  compression="lzw",
  
  width=180,
  
  height=180,
  
  units="mm"
  
)

ggsave(
  
  filename="Figure6_MetaRegression_Nature2.tiff",
  
  plot=figure6,
  
  dpi=600,
  
  compression="lzw",
  
  width=180,
  
  height=180,
  
  units="mm"
  
)


###############################################################
## End
###############################################################

#########################################################################

## GMWL
######################################################################

library(tidyverse)
library(patchwork)

############################################################
## Data
############################################################

coef <- tribble(
  ~Method, ~Slope, ~SlopeSE, ~Intercept, ~InterceptSE,
  "OLSR", 7.12,0.07,6.16,0.26,
  "RMA", 7.35,0.07,6.70,0.26,
  "MA", 7.57,0.08,7.30,0.27,
  "PWSLR",7.31,0.07,7.24,0.31,
  "PWRMA",7.53,0.07,8.00,0.31,
  "PWMA",7.75,0.08,8.76,0.31
)

############################################################
## 95% CI
############################################################

coef <- coef |>
  mutate(
    
    slope.low = Slope - 1.96 * SlopeSE,
    slope.up  = Slope + 1.96 * SlopeSE,
    
    int.low = Intercept - 1.96 * InterceptSE,
    int.up  = Intercept + 1.96 * InterceptSE,
    
    Colour = case_when(
      Method == "PWSLR" ~ "PWSLR",
      Method == "OLSR"  ~ "OLSR",
      TRUE              ~ "Other"
    )
    
  )

############################################################
## Theme
############################################################

themeNature <- function(){
  
  theme_classic(base_size = 12)+
    
    theme(
      
      text = element_text(family="sans"),
      
      axis.line = element_line(
        colour="black",
        linewidth=0.8),
      
      axis.ticks = element_line(
        linewidth=0.7),
      
      axis.ticks.length = unit(2.5,"mm"),
      
      axis.title = element_text(
        face="bold",
        size=12),
      
      axis.text = element_text(
        size=10,
        colour="black"),
      
      panel.grid.major.x =
        element_line(
          colour="grey88",
          linewidth=0.35),
      
      panel.grid.major.y =
        element_blank(),
      
      panel.grid.minor =
        element_blank(),
      
      plot.tag =
        element_text(
          face="bold",
          size=14),
      
      legend.position="none"
      
    )
  
}

############################################################
## Slope plot
############################################################

p1 <-
  
  ggplot(
    coef,
    aes(
      Slope,
      reorder(Method,Slope),
      colour=Colour)
  )+
  
  geom_errorbarh(
    
    aes(
      xmin=slope.low,
      xmax=slope.up),
    
    height=.16,
    linewidth=.9,
    colour="black"
    
  )+
  
  geom_point(
    size=4
  )+
  
  scale_colour_manual(
    
    values=c(
      
      OLSR="#009E73",
      
      PWSLR="#D55E00",
      
      Other="black"
      
    )
    
  )+
  
  labs(
    
    x=expression("LMWL slope"),
    
    y=NULL,
    
    tag="(a)"
    
  )+
  
  themeNature()

############################################################
## Intercept plot
############################################################

p2 <-
  
  ggplot(
    coef,
    aes(
      Intercept,
      reorder(Method,Intercept),
      colour=Colour)
  )+
  
  geom_errorbarh(
    
    aes(
      xmin=int.low,
      xmax=int.up),
    
    height=.16,
    linewidth=.9,
    colour="black"
    
  )+
  
  geom_point(
    size=4
  )+
  
  scale_colour_manual(
    
    values=c(
      
      OLSR="#009E73",
      
      PWSLR="#D55E00",
      
      Other="black"
      
    )
    
  )+
  
  labs(
    
    x=expression("LMWL intercept ("*"\u2030"*")"),
    
    y=NULL,
    
    tag="(b)"
    
  )+
  
  themeNature()
############################################################
## Final figure
############################################################

figure <- p1 | p2

figure

############################################################
## Save
############################################################

ggsave(
  "Forest_coefficients_Nature.tiff",
  figure,
  dpi=600,
  compression="lzw",
  width=180,
  height=90,
  units="mm"
)

ggsave(
  "Forest_coefficients_Nature.pdf",
  figure,
  width=180,
  height=90,
  units="mm"
)
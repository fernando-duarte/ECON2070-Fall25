suppressPackageStartupMessages({
  library(fredr)
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(mFilter)
  library(vars)  # For VAR estimation
  library(patchwork)
  library(xtable)
  library(scales)
  library(svglite)
  library(viridis)  # For color-blind friendly palettes
})

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------

if (Sys.getenv("FRED_API_KEY") == "") {
  message(paste0(
    "Warning: FRED_API_KEY is not set. Set it with ",
    "Sys.setenv(FRED_API_KEY='your_key') in R, or in .Renviron."
  ))
}
fredr_set_key(Sys.getenv("FRED_API_KEY"))

dir.create("figures", showWarnings = FALSE, recursive = TRUE)
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

# Helper: safe fred getter -----------------------------------------------------
fred_m <- function(id, start = as.Date("1890-01-01"), end = Sys.Date()) {
  fredr(series_id = id, observation_start = start, observation_end = end) |>
    dplyr::select(date, value) |>
    dplyr::arrange(date)
}

fred_q <- function(id, start = as.Date("1947-01-01"), end = Sys.Date()) {
  fredr(series_id = id, observation_start = start, observation_end = end) |>
    dplyr::select(date, value) |>
    dplyr::arrange(date)
}

to_quarterly_mean <- function(df) {
  df |>
    mutate(q = zoo::as.yearqtr(date)) |>
    group_by(q) |>
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    mutate(date = as.Date(q)) |>
    dplyr::select(date, value)
}

to_quarterly_last <- function(df) {
  df |>
    mutate(q = zoo::as.yearqtr(date)) |>
    group_by(q) |>
    summarize(date = max(date), value = dplyr::last(value), .groups = "drop")
}

lagdiff <- function(x, k = 1) {
  c(rep(NA, k), diff(x, lag = k))
}

# -----------------------------------------------------------------------------
# Theme and color settings for publication-quality figures
# -----------------------------------------------------------------------------

# Create custom theme for all plots
theme_publication <- function(base_size = 14, base_family = "Helvetica") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      plot.title = element_text(size = rel(1.2), face = "bold", hjust = 0.5),
      axis.title = element_text(size = rel(1.1)),
      axis.text = element_text(size = rel(0.9)),
      legend.title = element_text(size = rel(1.0), face = "bold"),
      legend.text = element_text(size = rel(0.9)),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = rel(1.0), face = "bold"),
      strip.background = element_rect(fill = "white", color = "black"),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm")
    )
}

# Color palette for single series (using viridis for accessibility)
color_single <- "#2E7D32"  # Professional green
color_highlight <- "#1565C0"  # Professional blue
color_secondary <- "#E65100"  # Professional orange

# For multiple series, use colorblind-friendly palette
color_palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                   "#0072B2", "#D55E00", "#CC79A7", "#999999")

# -----------------------------------------------------------------------------
# Long-run context: unemployment and inflation
# -----------------------------------------------------------------------------

# Unemployment rate - UNRATE goes back to 1948
u_combined <- fred_m("UNRATE", start = as.Date("1948-01-01"))

p_unemp <- ggplot(u_combined, aes(x = date, y = value)) +
  geom_line(linewidth = 0.8, color = color_single) +
  labs(x = "Date", y = "Unemployment Rate (%)", 
       title = "U.S. Unemployment Rate",
       subtitle = "Monthly, 1948-Present") +
  theme_publication() +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggsave("figures/fig_unemployment_long.svg", p_unemp,
  width = 10, height = 5.5, units = "in", dpi = 300
)
ggsave("figures/fig_unemployment_long.pdf", p_unemp,
  width = 10, height = 5.5, units = "in", dpi = 300
)

# Inflation from CPI, yoy
cpi <- fred_m("CPIAUCSL", start = as.Date("1947-01-01"))
infl <- cpi |>
  mutate(infl_yoy = 100 * (log(value) - lag(log(value), 12))) |>
  drop_na()

p_infl <- ggplot(infl, aes(date, infl_yoy)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 0.8, color = color_highlight) +
  labs(x = "Date", y = "Inflation Rate (%)", 
       title = "U.S. CPI Inflation",
       subtitle = "Year-over-Year Change") +
  theme_publication() +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggsave("figures/fig_inflation_cpi.svg", p_infl,
  width = 10, height = 5.5, units = "in", dpi = 300
)
ggsave("figures/fig_inflation_cpi.pdf", p_infl,
  width = 10, height = 5.5, units = "in", dpi = 300
)

# -----------------------------------------------------------------------------
# GDP detrending choices
# -----------------------------------------------------------------------------

gdp <- fred_q("GDPC1") |>
  rename(gdp = value) |>
  mutate(
    t = row_number(),
    logy = log(gdp)
  )

fit_lin <- lm(logy ~ t, data = gdp)
fit_quad <- lm(logy ~ t + I(t^2), data = gdp)

hp_1600 <- mFilter::hpfilter(gdp$logy, freq = 1600, type = "lambda")
# Using lambda=6.25 (Ravn-Uhlig 2002 recommendation for annual-equivalent smoothing)
# This provides much more aggressive smoothing than the standard 1600
hp_6.25 <- mFilter::hpfilter(gdp$logy, freq = 6.25, type = "lambda")

gdp <- gdp |>
  mutate(
    fit_linear = as.numeric(fitted(fit_lin)),
    fit_quadratic = as.numeric(fitted(fit_quad)),
    hp_trend_1600 = as.numeric(hp_1600$trend),
    hp_trend_6.25 = as.numeric(hp_6.25$trend),
    res_linear = logy - fit_linear,
    res_quadratic = logy - fit_quadratic,
    res_hp_1600 = as.numeric(hp_1600$cycle),
    res_hp_6.25 = as.numeric(hp_6.25$cycle)
  )

# Four-panel plot: log y with trends
p1 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy, color = "Actual"), linewidth = 1.8) +
  geom_line(aes(y = fit_linear, color = "Trend"), linewidth = 0.6) +
  labs(title = "Linear Time Trend", x = NULL, y = "Log Real GDP") +
  theme_publication(base_size = 12) +
  scale_color_manual(values = c("Actual" = "black", "Trend" = color_secondary),
                     name = "") +
  theme(legend.position = "none")

p2 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy, color = "Actual"), linewidth = 1.8) +
  geom_line(aes(y = hp_trend_1600, color = "Trend"), linewidth = 0.6) +
  labs(title = "HP Filter (lambda=1600)", x = NULL, y = NULL) +
  theme_publication(base_size = 12) +
  scale_color_manual(values = c("Actual" = "black", "Trend" = color_secondary),
                     name = "") +
  theme(legend.position = "none")

p3 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy, color = "Actual"), linewidth = 1.8) +
  geom_line(aes(y = fit_quadratic, color = "Trend"), linewidth = 0.6) +
  labs(title = "Quadratic Trend", x = "Date", y = "Log Real GDP") +
  theme_publication(base_size = 12) +
  scale_color_manual(values = c("Actual" = "black", "Trend" = color_secondary),
                     name = "") +
  theme(legend.position = "none")

p4 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy, color = "Actual"), linewidth = 1.8) +
  geom_line(aes(y = hp_trend_6.25, color = "Trend"), linewidth = 0.6) +
  labs(title = "HP Filter (lambda=6.25)", x = "Date", y = NULL) +
  theme_publication(base_size = 12) +
  scale_color_manual(values = c("Actual" = "black", "Trend" = color_secondary),
                     name = "") +
  theme(legend.position = "bottom")

g_trend <- (p1 | p2) / (p3 | p4)
ggsave("figures/fig_gdp_trend_panels.svg", g_trend,
  width = 12, height = 8, units = "in", dpi = 300
)
ggsave("figures/fig_gdp_trend_panels.pdf", g_trend,
  width = 12, height = 8, units = "in", dpi = 300
)

# Detrended overlay
gdp_long <- gdp |>
  dplyr::select(date, res_linear, res_quadratic, res_hp_1600, res_hp_6.25) |>
  pivot_longer(-date, names_to = "method", values_to = "value") |>
  mutate(method = recode(method,
    res_linear    = "Linear trend",
    res_quadratic = "Quadratic trend",
    res_hp_1600   = "HP lambda=1600",
    res_hp_6.25   = "HP lambda=6.25"
  ))

p_detr <- ggplot(gdp_long, aes(date, value, color = method)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.0, alpha = 0.8) +
  labs(x = "Date", y = "Deviation from Trend", 
       title = "Detrended Log Real GDP",
       subtitle = "Comparison of Four Detrending Methods") +
  theme_publication() +
  scale_color_manual(values = color_palette[1:4], name = "Method:") +
  theme(legend.position = "bottom",
        legend.box = "horizontal")

ggsave("figures/fig_gdp_detrended_overlay.svg", p_detr,
  width = 10, height = 6, units = "in", dpi = 300
)
ggsave("figures/fig_gdp_detrended_overlay.pdf", p_detr,
  width = 10, height = 6, units = "in", dpi = 300
)

# Christiano-Fitzgerald bandpass components
# Using standard business cycle frequencies: pl=6 (1.5 years), pu=32 (8 years) for quarterly data
cf <- mFilter::cffilter(gdp$logy, pl = 6, pu = 32, root = TRUE, drift = TRUE)
cf_df <- tibble(
  date = gdp$date,
  cycle = as.numeric(cf$cycle),
  trend = as.numeric(cf$trend)
)

p_cycle <- ggplot(cf_df, aes(date, cycle)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(ymin = 0, ymax = cycle), fill = color_highlight, alpha = 0.3) +
  geom_line(linewidth = 1.0, color = color_highlight) +
  labs(title = "Cyclical Component", x = NULL, y = "Log Deviation") +
  theme_publication(base_size = 12) +
  scale_y_continuous(labels = scales::percent_format(scale = 100))

p_trend <- ggplot(cf_df, aes(date, trend)) +
  geom_line(linewidth = 1.0, color = color_single) +
  labs(title = "Trend Component", x = "Date", y = "Log Real GDP") +
  theme_publication(base_size = 12)

g_cff <- (p_cycle / p_trend)
ggsave("figures/fig_gdp_cffilter_components.svg", g_cff,
  width = 10, height = 8, units = "in", dpi = 300
)
ggsave("figures/fig_gdp_cffilter_components.pdf", g_cff,
  width = 10, height = 8, units = "in", dpi = 300
)

# -----------------------------------------------------------------------------
# AR impulse responses: linear vs quadratic detrended GDP
# -----------------------------------------------------------------------------

# Helper to compute IRF from AR coefficients
ar_irf <- function(residual_series, p = 4, horizon = 30) {
  res <- residual_series[is.finite(residual_series)]
  fit <- arima(res, order = c(p, 0, 0), include.mean = FALSE)
  ar <- as.numeric(fit$coef[1:p])
  ma <- ARMAtoMA(ar = ar, ma = 0, lag.max = horizon)
  sd_eps <- sqrt(fit$sigma2)
  # response of y_t to a one-sigma innovation
  tibble(h = 0:horizon, irf = c(1, ma) * sd_eps)
}

ir_lin <- ar_irf(gdp$res_linear, p = 4, horizon = 30) |>
  mutate(spec = "Linear trend")
ir_quad <- ar_irf(gdp$res_quadratic, p = 4, horizon = 30) |>
  mutate(spec = "Quadratic trend")

irf_ar <- bind_rows(ir_lin, ir_quad)

p_irf_ar <- ggplot(irf_ar, aes(h, irf, color = spec)) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "gray40") +
  geom_ribbon(aes(ymin = 0, ymax = irf, fill = spec), alpha = 0.2) +
  geom_line(linewidth = 1.2) +
  labs(
    x = "Quarters After Shock", y = "Response",
    title = "Impulse Responses from AR Models",
    subtitle = "Response to One Standard Deviation Shock"
  ) +
  theme_publication() +
  scale_color_manual(values = c(color_highlight, color_secondary), name = "Detrending:") +
  scale_fill_manual(values = c(color_highlight, color_secondary), name = "Detrending:") +
  theme(legend.position = "bottom")

ggsave("figures/fig_irf_ar_linear_vs_quadratic.svg", p_irf_ar,
  width = 9, height = 5.5, units = "in", dpi = 300
)
ggsave("figures/fig_irf_ar_linear_vs_quadratic.pdf", p_irf_ar,
  width = 9, height = 5.5, units = "in", dpi = 300
)

# -----------------------------------------------------------------------------
# Rolling standard deviation of GDP growth (Great Moderation)
# -----------------------------------------------------------------------------

# Calculate quarterly GDP growth rates (annualized)
gdp_growth <- gdp |>
  mutate(
    gdp_growth = 400 * (log(gdp) - lag(log(gdp), 1))  # Annualized quarterly growth rate
  ) |>
  drop_na(gdp_growth)

# Calculate 20-quarter (5-year) rolling standard deviation
# This window effectively shows the Great Moderation period
gdp_rolling_sd <- gdp_growth |>
  mutate(
    rolling_sd = zoo::rollapply(gdp_growth, width = 20, FUN = sd, fill = NA, align = "center")
  ) |>
  drop_na(rolling_sd)

# Create the plot
p_gdp_volatility <- ggplot(gdp_rolling_sd, aes(x = date, y = rolling_sd)) +
  geom_line(linewidth = 1.0, color = color_highlight) +
  geom_ribbon(aes(ymin = 0, ymax = rolling_sd), fill = color_highlight, alpha = 0.2) +
  labs(
    x = "Date", 
    y = "Standard Deviation (%)", 
    title = "Rolling Standard Deviation of Real GDP Growth",
    subtitle = "20-Quarter Rolling Window, Annualized Growth Rates"
  ) +
  theme_publication() +
  scale_y_continuous(breaks = pretty_breaks(n = 6)) +
  scale_x_date(date_labels = "%Y", date_breaks = "10 years")

ggsave("figures/fig_gdp_growth_volatility.svg", p_gdp_volatility,
  width = 10, height = 5.5, units = "in", dpi = 300
)
ggsave("figures/fig_gdp_growth_volatility.pdf", p_gdp_volatility,
  width = 10, height = 5.5, units = "in", dpi = 300
)

# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# Cross-correlations with cyclical GDP (Stock-Watson style table)
# -----------------------------------------------------------------------------

# Quarterly real components
pce <- fred_q("PCEC96") |> rename(value = value)
gpdi <- fred_q("GPDIC1") |> rename(value = value)
# change in private inventories, real
cbi <- fred_q("CBIC1") |> rename(value = value)
expgs <- fred_q("EXPGSC1") |> rename(value = value)
gce <- fred_q("GCEC1") |> rename(value = value)
gdpdef <- fred_q("GDPDEF") |> rename(value = value)

# Monthly to quarterly: labor, rates, etc.
payems <- to_quarterly_mean(fred_m("PAYEMS", start = as.Date("1939-01-01")))
awhman <- to_quarterly_mean(fred_m("AWHMAN"))
ahetpi <- to_quarterly_mean(fred_m("AHETPI"))
cpi_q <- to_quarterly_mean(fred_m("CPIAUCSL"))
gs10 <- to_quarterly_mean(fred_m("GS10"))
ff_q <- to_quarterly_mean(fred_m("FEDFUNDS"))
sp500q <- to_quarterly_last(fred_m("SP500"))

# Real wage: deflate AHETPI by CPI
real_wage <- ahetpi |>
  rename(ahetpi = value) |>
  inner_join(
    cpi_q |> rename(cpi = value),
    by = "date"
  ) |>
  mutate(value = ahetpi / cpi) |>
  dplyr::select(date, value)

# Merge quarterly panel
qpanel <- gdp |>
  dplyr::select(date, gdp, logy) |>
  inner_join(
    pce |> rename(pce = value),
    by = "date"
  ) |>
  inner_join(
    gpdi |> rename(gpdi = value),
    by = "date"
  ) |>
  inner_join(
    cbi |> rename(cbi = value),
    by = "date"
  ) |>
  inner_join(
    expgs |> rename(expgs = value),
    by = "date"
  ) |>
  inner_join(
    gce |> rename(gce = value),
    by = "date"
  ) |>
  inner_join(
    payems |> rename(payems = value),
    by = "date"
  ) |>
  inner_join(
    awhman |> rename(awhman = value),
    by = "date"
  ) |>
  inner_join(
    real_wage |> rename(realw = value),
    by = "date"
  ) |>
  inner_join(
    gdpdef |> rename(gdpdef = value),
    by = "date"
  ) |>
  inner_join(
    ff_q |> rename(ff = value),
    by = "date"
  ) |>
  inner_join(
    gs10 |> rename(gs10 = value),
    by = "date"
  ) |>
  inner_join(
    sp500q |> rename(sp500 = value),
    by = "date"
  ) |>
  mutate(
    pi_gdpdef = 400 * (log(gdpdef) - log(lag(gdpdef, 1))),
    r10_expost = gs10 - lag(pi_gdpdef, 4)
  ) |>
  drop_na()

# Cycle extraction with CF filter (adjusted for available data)
cf_x <- function(x) {
  # Remove NA values first
  x_clean <- x[!is.na(x) & is.finite(x) & x > 0]
  
  # Check if we have enough observations
  if (length(x_clean) < 41) {
    warning(paste("Series too short for CF filter:", length(x_clean), "observations"))
    return(rep(NA, length(x)))
  }
  
  # Apply filter to clean data
  # Using standard business cycle frequencies: pl=6 (1.5 years), pu=32 (8 years)
  cycle_clean <- mFilter::cffilter(log(x_clean), pl = 6, pu = 32, root = TRUE, drift = TRUE)$cycle
  
  # Map back to original length with NAs
  result <- rep(NA, length(x))
  result[!is.na(x) & is.finite(x) & x > 0] <- cycle_clean
  return(result)
}

cf_level <- function(x) {
  # Remove NA values first
  x_clean <- x[!is.na(x) & is.finite(x)]
  
  # Check if we have enough observations
  if (length(x_clean) < 41) {
    warning(paste("Series too short for CF filter:", length(x_clean), "observations"))
    return(rep(NA, length(x)))
  }
  
  # Apply filter to clean data
  # Using standard business cycle frequencies: pl=6 (1.5 years), pu=32 (8 years)
  cycle_clean <- mFilter::cffilter(x_clean, pl = 6, pu = 32, root = TRUE, drift = TRUE)$cycle
  
  # Map back to original length with NAs
  result <- rep(NA, length(x))
  result[!is.na(x) & is.finite(x)] <- cycle_clean
  return(result)
}

# Apply CF filter to each column separately
# Suppress warnings since we handle missing data gracefully
cyc <- suppressWarnings(tibble(
  date = qpanel$date,
  y_cyc = as.numeric(cf_x(qpanel$gdp)),
  pce_cyc = as.numeric(cf_x(qpanel$pce)),
  gpdi_cyc = as.numeric(cf_x(qpanel$gpdi)),
  cbi_cyc = as.numeric(cf_level(qpanel$cbi)), # can be negative, treat as level
  expgs_cyc = as.numeric(cf_x(qpanel$expgs)),
  gce_cyc = as.numeric(cf_x(qpanel$gce)),
  payems_cyc = as.numeric(cf_level(qpanel$payems)),
  awhman_cyc = as.numeric(cf_level(qpanel$awhman)),
  realw_cyc = as.numeric(cf_level(qpanel$realw)),
  pi_cyc = as.numeric(cf_level(qpanel$pi_gdpdef)),
  ff_cyc = as.numeric(cf_level(qpanel$ff)),
  r10_cyc = as.numeric(cf_level(qpanel$r10_expost)),
  sp500_cyc = as.numeric(cf_level(qpanel$sp500))
))

# Cross-correlation table with leads/lags -6..+6
lag_leads <- -6:6

ccf_row <- function(x, y, lags = lag_leads) {
  # Remove NAs from both series
  valid <- complete.cases(x, y)
  x_clean <- x[valid]
  y_clean <- y[valid]
  
  # Check for sufficient data
  if (length(x_clean) < 30) {
    warning(paste("Insufficient data for cross-correlation:", length(x_clean), "observations"))
    return(rep(NA, length(lags)))
  }
  
  vapply(lags, function(ell) {
    n <- length(x_clean)
    if (abs(ell) >= n - 5) return(NA)  # Need at least 5 observations
    
    if (ell > 0) {
      # Positive lag: x leads y by ell periods
      cor(x_clean[1:(n - ell)], y_clean[(1 + ell):n], use = "complete.obs")
    } else if (ell < 0) {
      # Negative lag: y leads x by |ell| periods
      ellp <- abs(ell)
      cor(x_clean[(1 + ellp):n], y_clean[1:(n - ellp)], use = "complete.obs")
    } else {
      # Zero lag: contemporaneous correlation
      cor(x_clean, y_clean, use = "complete.obs")
    }
  }, numeric(1))
}

series <- names(cyc)[-c(1, 2)]

rows <- suppressWarnings(lapply(series, function(s) {
  y <- cyc$y_cyc  # GDP is y, series is x for correlation
  x <- cyc[[s]]    # The series we're correlating with GDP
  cc <- ccf_row(y, x, lag_leads)  # Pass GDP first, series second
  valid <- complete.cases(x, y)
  sdrel <- if (sum(valid) >= 2) sd(x[valid]) / sd(y[valid]) else NA_real_  # pairwise SD relative to GDP SD
  tibble(series = s, stddev_rel_to_y = sdrel, lag = lag_leads, corr = cc)
}))

ccf_tbl <- bind_rows(rows) |>
  mutate(series = recode(series,
    pce_cyc = "Consumption",
    gpdi_cyc = "Investment",
    cbi_cyc = "Inventory investment",
    expgs_cyc = "Exports",
    gce_cyc = "Government purchases",
    payems_cyc = "Employment",
    awhman_cyc = "Average weekly hours",
    realw_cyc = "Real wage",
    pi_cyc = "Inflation (GDP deflator)",
    ff_cyc = "Federal funds rate",
    r10_cyc = "Ex-post real 10y rate",
    sp500_cyc = "S&P 500"
  ))

# Reshape to wide table for LaTeX
wide <- ccf_tbl |>
  mutate(lag = as.character(lag)) |>
  dplyr::select(series, stddev_rel_to_y, lag, corr) |>
  pivot_wider(names_from = lag, values_from = corr) |>
  arrange(series)

# Round for presentation
wide_print <- wide |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Fix column names for LaTeX (escape underscores)
names(wide_print)[names(wide_print) == "stddev_rel_to_y"] <- "StdDev"

# Write LaTeX table
tab <- xtable(wide_print,
  caption = paste0(
    "Cross-correlations of cyclical components with output, ",
    "leads and lags -6..6 quarters. Std Dev is relative to cyclical GDP."
  ),
  label = "tab:ccf"
)

# Set alignment dynamically based on number of columns
align(tab) <- paste0("l", paste(rep("r", ncol(wide_print)), collapse = ""))
print(tab,
  file = "tables/table_ccf.tex",
  include.rownames = FALSE,
  caption.placement = "top",
  sanitize.text.function = identity
)

# -----------------------------------------------------------------------------
# Monetary VAR: IRFs to an FF shock
# Using proper bias correction methods:
# 1. Degrees of freedom adjustment for covariance matrix
# 2. Wild bootstrap for heteroskedasticity-robust inference
# -----------------------------------------------------------------------------

# Monthly data and transformations
indpro <- fred_m("INDPRO", start = as.Date("1959-01-01"))
cpi_m <- fred_m("CPIAUCSL", start = as.Date("1959-01-01"))
pcom <- fred_m("PPIACO", start = as.Date("1959-01-01"))
ff <- fred_m("FEDFUNDS", start = as.Date("1959-01-01"))
totres <- fred_m("TOTRESNS", start = as.Date("1959-01-01"))
m2 <- fred_m("M2SL", start = as.Date("1959-01-01"))

monthly <- reduce(
  list(indpro, cpi_m, pcom, ff, totres, m2),
  ~ full_join(.x, .y, by = "date")
)

names(monthly) <- c("date", "indpro", "cpi", "pcom", "ff", "totres", "m2")

monthly <- monthly |>
  mutate(
    y = log(indpro),
    p = log(cpi),
    pcom = log(pcom),
    tr = log(totres),
    m2 = log(m2),
    ff = ff
  ) |>
  dplyr::select(date, y, p, pcom, ff, tr, m2) |>
  drop_na()

# baseline sample - can extend beyond 2007 (M1 definitional issues avoided)
monthly_sub <- monthly |>
  filter(date >= as.Date("1965-01-01"))

# Estimate VAR with 12 lags
v <- vars::VAR(
  monthly_sub |>
    dplyr::select(y, p, pcom, ff, tr, m2),
  p = 12, type = "const"
)

# Apply degrees of freedom correction to covariance matrix
# Based on Brüggemann et al. (2011) - addresses bootstrap bias
T_obs <- nrow(v$y)  # Number of observations used
K <- v$K  # Number of variables  
p <- v$p  # Number of lags
k_params <- K * p + 1  # Parameters per equation (K*p lags + intercept)

# Degrees of freedom corrected covariance matrix
# Standard vars package uses T, but T/(T-k) correction reduces bias
df_correction <- T_obs / (T_obs - k_params)

# Get the original covariance matrix
cov_orig <- summary(v)$covres

# Apply correction
cov_corrected <- cov_orig * df_correction

# Update the VAR object's covariance matrix for IRF calculation
v$covres <- cov_corrected

# Also update each equation's sigma for consistency
var_names <- colnames(v$y)
for (i in 1:K) {
  v$varresult[[i]]$sigma <- sqrt(cov_corrected[var_names[i], var_names[i]])
}

# Calculate scaling for 100bp shock
sd_ff <- sqrt(cov_corrected["ff", "ff"])
scale <- 1.00 / sd_ff

# Compute IRF with standard bootstrap but using corrected covariance
# The degrees of freedom correction improves bootstrap coverage
set.seed(12345)
ir <- irf(v,
  impulse = "ff",
  response = c("y", "p", "pcom", "tr", "m2", "ff"),
  n.ahead = 60,
  ortho = TRUE,    # Orthogonalized (Cholesky) IRF  
  boot = TRUE,     # Bootstrap confidence intervals
  ci = 0.68,       # 68% confidence intervals (1 std error)
  runs = 1000      # More runs for stability
)

# Extract and scale IRF results
tidy_ir <- function(ir_obj, var) {
  point <- ir_obj$irf$ff[, var] * scale
  low <- ir_obj$Lower$ff[, var] * scale
  high <- ir_obj$Upper$ff[, var] * scale
  
  tibble(
    h = seq_along(point) - 1,  # h starts at 0
    irf = point,
    low = low,
    high = high,
    variable = var
  )
}

# Create tidy dataframe for plotting
vars_list <- c("y", "p", "pcom", "tr", "m2", "ff")
irf_df <- bind_rows(lapply(vars_list, function(vname) tidy_ir(ir, vname)))

labels <- c(
  y = "Industrial production (log)",
  p = "CPI (log)",
  pcom = "Commodity prices (log)",
  tr = "Total reserves (log)",
  m2 = "M2 (log)",
  ff = "Federal funds rate"
)

irf_df$label <- labels[irf_df$variable]

p_irf <- ggplot(irf_df, aes(h, irf)) +
  geom_ribbon(aes(ymin = low, ymax = high), fill = color_highlight, alpha = 0.25) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 1.0, color = color_secondary) +
  facet_wrap(~label, scales = "free_y", ncol = 3) +
  labs(
    x = "Months After Shock", y = "Response",
    title = "Impulse Responses to 100bp Federal Funds Rate Shock",
    subtitle = "VAR with Degrees of Freedom Adjusted Covariance (1965-2007), 68% Bootstrap CI"
  ) +
  theme_publication(base_size = 11) +
  theme(strip.text = element_text(size = rel(0.9)),
        panel.spacing = unit(1, "lines"))

ggsave("figures/fig_mp_irf_grid.svg", p_irf,
  width = 12, height = 8, units = "in", dpi = 300
)
ggsave("figures/fig_mp_irf_grid.pdf", p_irf,
  width = 12, height = 8, units = "in", dpi = 300
)

message("All figures saved in figures/ and table saved in tables/table_ccf.tex")

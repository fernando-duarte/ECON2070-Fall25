suppressPackageStartupMessages({
  library(fredr)
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(mFilter)
  library(patchwork)
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

# Download NBER recession indicator (USREC: 1 = recession, 0 = expansion)
recessions <- fred_m("USREC", start = as.Date("1945-01-01")) |>
  rename(recession = value)

# Create recession periods dataframe for shading
# Group consecutive recession months into periods
recession_periods <- recessions |>
  mutate(
    # Create groups for consecutive recession periods
    recession_group = cumsum(recession == 1 & lag(recession, default = 0) == 0)
  ) |>
  filter(recession == 1) |>
  group_by(recession_group) |>
  summarize(
    start = min(date),
    end = max(date),
    .groups = "drop"
  )

# Unemployment rate - UNRATE goes back to 1948
u_combined <- fred_m("UNRATE", start = as.Date("1948-01-01"))

# Filter recession periods to match unemployment data range
recession_periods_unemp <- recession_periods |>
  filter(end >= min(u_combined$date, na.rm = TRUE))

p_unemp <- ggplot(u_combined, aes(x = date, y = value)) +
  # Add recession shading first (behind the line)
  geom_rect(data = recession_periods_unemp,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.2, inherit.aes = FALSE) +
  geom_line(linewidth = 0.8, color = color_single) +
  labs(x = "Date", y = "Unemployment Rate (%)", 
       title = "U.S. Unemployment Rate",
       subtitle = "Monthly, 1948-Present (shaded areas indicate NBER recessions)") +
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

# Filter recession periods to match inflation data range
recession_periods_infl <- recession_periods |>
  filter(end >= min(infl$date, na.rm = TRUE))

p_infl <- ggplot(infl, aes(date, infl_yoy)) +
  # Add recession shading first (behind the line)
  geom_rect(data = recession_periods_infl,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
            fill = "gray", alpha = 0.2, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linewidth = 0.5, linetype = "dashed", color = "gray40") +
  geom_line(linewidth = 0.8, color = color_highlight) +
  labs(x = "Date", y = "Inflation Rate (%)", 
       title = "U.S. CPI Inflation",
       subtitle = "Year-over-Year Change (shaded areas indicate NBER recessions)") +
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


message("All figures saved in figures/")

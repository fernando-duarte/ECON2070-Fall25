# Package management: auto-install if needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(fredr, tidyverse, lubridate, zoo, mFilter, vars, patchwork, xtable, scales)

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
    select(date, value) |>
    arrange(date)
}

fred_q <- function(id, start = as.Date("1947-01-01"), end = Sys.Date()) {
  fredr(series_id = id, observation_start = start, observation_end = end) |>
    select(date, value) |>
    arrange(date)
}

to_quarterly_mean <- function(df) {
  df |>
    mutate(q = yearquarter(date)) |>
    group_by(q) |>
    summarize(value = mean(value, na.rm = TRUE), .groups = "drop") |>
    transmute(date = as.Date(as.yearqtr(q)), value = value)
}

to_quarterly_last <- function(df) {
  df |>
    mutate(q = yearquarter(date)) |>
    group_by(q) |>
    summarize(date = max(date), value = last(value), .groups = "drop")
}

lagdiff <- function(x, k = 1) {
  c(rep(NA, k), diff(x, lag = k))
}

# -----------------------------------------------------------------------------
# Long-run context: unemployment and inflation
# -----------------------------------------------------------------------------

# Unemployment: stitch historical with BLS series
u_hist <- fred_m("M089URALLUSM156N") # historical unemployment rate
u_bls <- fred_m("UNRATE") # BLS unemployment rate, since 1948

u_combined <- full_join(
  u_hist |>
    rename(u_hist = value),
  u_bls |>
    rename(u_bls = value),
  by = "date"
) |>
  mutate(value = if_else(!is.na(u_bls), u_bls, u_hist)) |>
  select(date, value) |>
  drop_na()

p_unemp <- ggplot(u_combined, aes(x = date, y = value)) +
  geom_line(linewidth = 0.4) +
  labs(x = "Date", y = "Percent", title = "U.S. unemployment rate (monthly)") +
  theme_minimal()

ggsave("figures/fig_unemployment_long.pdf", p_unemp,
  width = 9, height = 4.8, units = "in"
)

# Inflation from CPI, yoy
cpi <- fred_m("CPIAUCSL", start = as.Date("1947-01-01"))
infl <- cpi |>
  mutate(infl_yoy = 100 * (log(value) - lag(log(value), 12))) |>
  drop_na()

p_infl <- ggplot(infl, aes(date, infl_yoy)) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(linewidth = 0.4) +
  labs(x = "Date", y = "Percent", title = "CPI inflation, year over year") +
  theme_minimal()

ggsave("figures/fig_inflation_cpi.pdf", p_infl,
  width = 9, height = 4.8, units = "in"
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
hp_16 <- mFilter::hpfilter(gdp$logy, freq = 16, type = "lambda")

gdp <- gdp |>
  mutate(
    fit_linear = as.numeric(fitted(fit_lin)),
    fit_quadratic = as.numeric(fitted(fit_quad)),
    hp_trend_1600 = as.numeric(hp_1600$trend),
    hp_trend_16 = as.numeric(hp_16$trend),
    res_linear = logy - fit_linear,
    res_quadratic = logy - fit_quadratic,
    res_hp_1600 = as.numeric(hp_1600$cycle),
    res_hp_16 = as.numeric(hp_16$cycle)
  )

# Four-panel plot: log y with trends
p1 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy), linewidth = 0.4) +
  geom_line(aes(y = fit_linear), linewidth = 0.4) +
  labs(title = "log Y and linear time trend", x = NULL, y = NULL) +
  theme_minimal()

p2 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy), linewidth = 0.4) +
  geom_line(aes(y = hp_trend_1600), linewidth = 0.4) +
  labs(title = "log Y and HP trend (lambda=1600)", x = NULL, y = NULL) +
  theme_minimal()

p3 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy), linewidth = 0.4) +
  geom_line(aes(y = fit_quadratic), linewidth = 0.4) +
  labs(title = "log Y and quadratic trend", x = NULL, y = NULL) +
  theme_minimal()

p4 <- ggplot(gdp, aes(date)) +
  geom_line(aes(y = logy), linewidth = 0.4) +
  geom_line(aes(y = hp_trend_16), linewidth = 0.4) +
  labs(title = "log Y and HP trend (lambda=16)", x = NULL, y = NULL) +
  theme_minimal()

g_trend <- (p1 | p2) / (p3 | p4)
ggsave("figures/fig_gdp_trend_panels.pdf", g_trend,
  width = 10, height = 7.2, units = "in"
)

# Detrended overlay
gdp_long <- gdp |>
  select(date, res_linear, res_quadratic, res_hp_1600, res_hp_16) |>
  pivot_longer(-date, names_to = "method", values_to = "value") |>
  mutate(method = recode(method,
    res_linear    = "Linear trend",
    res_quadratic = "Quadratic trend",
    res_hp_1600   = "HP lambda=1600",
    res_hp_16     = "HP lambda=16"
  ))

p_detr <- ggplot(gdp_long, aes(date, value, linetype = method)) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(linewidth = 0.4) +
  labs(x = NULL, y = NULL, title = "Detrended log Y under four methods") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("figures/fig_gdp_detrended_overlay.pdf", p_detr,
  width = 9, height = 4.8, units = "in"
)

# Christiano-Fitzgerald bandpass components
cf <- mFilter::cffilter(gdp$logy, pl = 6, pu = 32, root = TRUE, drift = TRUE)
cf_df <- tibble(
  date = gdp$date,
  cycle = as.numeric(cf$cycle),
  trend = as.numeric(cf$trend),
  irregular = as.numeric(cf$irregular)
)

p_cycle <- ggplot(cf_df, aes(date, cycle)) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(linewidth = 0.4) +
  labs(title = "Bandpass-Filtered GDP (Cycle)", x = NULL, y = NULL) +
  theme_minimal()

p_trend <- ggplot(cf_df, aes(date, trend)) +
  geom_line(linewidth = 0.4) +
  labs(title = "Bandpass-Filtered GDP (Trend)", x = NULL, y = NULL) +
  theme_minimal()

p_irreg <- ggplot(cf_df, aes(date, irregular)) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(linewidth = 0.4) +
  labs(title = "Bandpass-Filtered GDP (Irregular)", x = NULL, y = NULL) +
  theme_minimal()

g_cff <- (p_cycle / p_trend / p_irreg)
ggsave("figures/fig_gdp_cffilter_components.pdf", g_cff,
  width = 9, height = 8.5, units = "in"
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

p_irf_ar <- ggplot(irf_ar, aes(h, irf, linetype = spec)) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(linewidth = 0.5) +
  labs(
    x = "Quarters", y = NULL,
    title = "AR impulse responses on detrended GDP"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave("figures/fig_irf_ar_linear_vs_quadratic.pdf", p_irf_ar,
  width = 8, height = 4.5, units = "in"
)

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
  select(date, value)

# Merge quarterly panel
qpanel <- gdp |>
  select(date, gdp, logy) |>
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

# Cycle extraction with CF filter
cf_x <- function(x) {
  mFilter::cffilter(log(x), pl = 6, pu = 32, root = TRUE, drift = TRUE)$cycle
}
cf_level <- function(x) {
  mFilter::cffilter(x, pl = 6, pu = 32, root = TRUE, drift = TRUE)$cycle
}

cyc <- qpanel |>
  transmute(
    date = date,
    y_cyc = cf_x(gdp),
    pce_cyc = cf_x(pce),
    gpdi_cyc = cf_x(gpdi),
    cbi_cyc = cf_level(cbi), # can be negative, treat as level
    expgs_cyc = cf_x(expgs),
    gce_cyc = cf_x(gce),
    payems_cyc = cf_level(payems),
    awhman_cyc = cf_level(awhman),
    realw_cyc = cf_level(realw),
    pi_cyc = cf_level(pi_gdpdef),
    ff_cyc = cf_level(ff),
    r10_cyc = cf_level(r10_expost),
    sp500_cyc = cf_level(sp500)
  ) |>
  drop_na()

# Cross-correlation table with leads/lags -6..+6
lag_leads <- -6:6

ccf_row <- function(x, y, lags = lag_leads) {
  vapply(lags, function(ell) {
    if (ell >= 0) {
      stats::cor(
        x[(1 + ell):length(x)], y[1:(length(y) - ell)],
        use = "pairwise.complete.obs"
      )
    } else {
      ellp <- abs(ell)
      stats::cor(
        x[1:(length(x) - ellp)], y[(1 + ellp):length(y)],
        use = "pairwise.complete.obs"
      )
    }
  }, numeric(1))
}

series <- names(cyc)[-c(1, 2)]

rows <- lapply(series, function(s) {
  x <- cyc$y_cyc
  y <- cyc[[s]]
  cc <- ccf_row(x, y, lag_leads)
  sdrel <- sd(y, na.rm = TRUE) / sd(x, na.rm = TRUE)
  tibble(series = s, stddev_rel_to_y = sdrel, lag = lag_leads, corr = cc)
})

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
  select(series, stddev_rel_to_y, lag, corr) |>
  pivot_wider(names_from = lag, values_from = corr) |>
  arrange(series)

# Round for presentation
wide_print <- wide |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Write LaTeX table
tab <- xtable(wide_print,
  caption = paste0(
    "Cross-correlations of cyclical components with output, ",
    "leads and lags -6..6 quarters. Std Dev is relative to cyclical GDP."
  ),
  label = "tab:ccf"
)

align(tab) <- "lrrrrrrrrrrrrrrrr"
print(tab,
  file = "tables/table_ccf.tex",
  include.rownames = FALSE,
  caption.placement = "top",
  sanitize.text.function = identity
)

# -----------------------------------------------------------------------------
# Monetary VAR: IRFs to an FF shock
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
  select(date, y, p, pcom, ff, tr, m2) |>
  drop_na()

# baseline sample - can extend beyond 2007 (M1 definitional issues avoided)
monthly_sub <- monthly |>
  filter(date >= as.Date("1965-01-01"))

# estimate VAR with 12 lags
v <- vars::VAR(
  monthly_sub |>
    select(y, p, pcom, ff, tr, m2),
  p = 12, type = "const"
)

# scale to a 100bp shock
sd_ff <- sqrt(diag(v$covresid)["ff"])
scale <- 1.00 / sd_ff

ir <- irf(v,
  impulse = "ff",
  response = c("y", "p", "pcom", "tr", "m2", "ff"),
  n.ahead = 60, ortho = TRUE, boot = TRUE, ci = 0.68
)

tidy_ir <- function(ir_obj, var) {
  point <- ir_obj$irf$ff[, var] * scale
  low <- ir_obj$Lower$ff[, var] * scale
  high <- ir_obj$Upper$ff[, var] * scale
  tibble(
    h = seq_along(point), irf = point,
    low = low, high = high, variable = var
  )
}

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
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.15) +
  geom_hline(yintercept = 0, linewidth = 0.2) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~label, scales = "free_y", ncol = 3) +
  labs(
    x = "Months after shock", y = NULL,
    title = "Impulse responses to a 100bp federal funds rate innovation"
  ) +
  theme_minimal()

ggsave("figures/fig_mp_irf_grid.pdf", p_irf,
  width = 10, height = 8.5, units = "in"
)

message("All figures saved in figures/ and table saved in tables/table_ccf.tex")

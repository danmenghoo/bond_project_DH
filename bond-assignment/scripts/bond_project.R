# Bond Assignment: Data Load + Validation Only
# ------------------------------------------------------------
# This script:
# 1) loads required packages
# 2) reads five FactSet Excel files from data/
# 3) keeps Date, Price, Yield to Maturity
# 4) renames to date, price, ytm and adds ticker + CUSIP
# 5) combines all bonds and sorts by date, ticker
# 6) runs assignment validation checks with assertthat::assert_that()
# 7) prints summary outputs

# -----------------------------
# 1) Load required packages
# -----------------------------
required_packages <- c("readxl", "dplyr", "purrr", "assertthat")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing package(s): ", paste(missing_packages, collapse = ", "),
      ". Please install them first, e.g. install.packages(c(\"",
      paste(missing_packages, collapse = "\", \""),
      "\"))"
    )
  )
}

library(readxl)
library(dplyr)
library(purrr)
library(assertthat)

# -----------------------------
# 2) Define input files in data/
# -----------------------------
# Note: TRP in filename corresponds to ticker TPR (Tapestry).
file_map <- tibble::tribble(
  ~ticker, ~cusip,       ~file_name,
  "NKE",   "654106AK9", "NIKE - 654106AK9.xlsx",
  "RL",    "751212AD3", "RL - 751212AD3.xlsx",
  "TPR",   "876030AA5", "TRP - 876030AA5.xlsx",
  "PVH",   "693656AE0", "PVH - 693656AE0.xlsx",
  "VFC",   "918204AR9", "VFC - 918204AR9.xlsx"
)

data_dir <- "data"
assert_that(dir.exists(data_dir), msg = "Expected folder 'data/' was not found.")

full_paths <- file.path(data_dir, file_map$file_name)
assert_that(
  all(file.exists(full_paths)),
  msg = paste0(
    "One or more input files are missing in data/: ",
    paste(file_map$file_name[!file.exists(full_paths)], collapse = ", ")
  )
)

# -----------------------------
# 3-4) Read each file and clean
# -----------------------------
read_one_bond <- function(file_name, ticker, cusip, data_dir = "data") {
  path <- file.path(data_dir, file_name)

  raw <- read_excel(
    path = path,
    sheet = "Price History",
    skip = 15
  )

  # Keep only required columns, preserving original units/precision
  cleaned <- raw %>%
    select(Date, Price, `Yield to Maturity`) %>%
    rename(
      date = Date,
      price = Price,
      ytm = `Yield to Maturity`
    ) %>%
    mutate(
      date = as.Date(date),
      ticker = ticker,
      cusip = cusip
    )

  cleaned
}

bond_data <- pmap_dfr(
  .l = list(file_map$file_name, file_map$ticker, file_map$cusip),
  .f = ~ read_one_bond(file_name = ..1, ticker = ..2, cusip = ..3, data_dir = data_dir)
) %>%
  arrange(date, ticker)

# -----------------------------
# 5-6) Validation checks
# -----------------------------
# Required assignment range
expected_start <- as.Date("2025-08-29")
expected_end <- as.Date("2026-08-31")

# No missing values in key fields
assert_that(all(!is.na(bond_data$date)), msg = "Missing date values detected.")
assert_that(all(!is.na(bond_data$price)), msg = "Missing price values detected.")
assert_that(all(!is.na(bond_data$ytm)), msg = "Missing ytm values detected.")

# No duplicate ticker-date pairs
dup_count <- bond_data %>%
  count(ticker, date, name = "n") %>%
  filter(n > 1) %>%
  nrow()
assert_that(dup_count == 0, msg = "Duplicate ticker-date pairs detected.")

# Exactly five companies
n_companies <- bond_data %>% distinct(ticker) %>% nrow()
assert_that(n_companies == 5, msg = "Expected exactly five companies.")

# All companies have the same observation dates
company_dates <- bond_data %>%
  group_by(ticker) %>%
  summarise(dates = list(sort(unique(date))), .groups = "drop")

reference_dates <- company_dates$dates[[1]]
all_same_dates <- all(vapply(company_dates$dates, identical, logical(1), reference_dates))
assert_that(all_same_dates, msg = "Not all companies have the same observation dates.")

# Each company date range is exactly 2025-08-29 through 2026-08-31
date_ranges <- bond_data %>%
  group_by(ticker) %>%
  summarise(
    min_date = min(date),
    max_date = max(date),
    .groups = "drop"
  )

range_ok <- all(date_ranges$min_date == expected_start & date_ranges$max_date == expected_end)
assert_that(
  range_ok,
  msg = "At least one company does not match expected date range 2025-08-29 to 2026-08-31."
)

# -----------------------------
# 7) Print requested summaries
# -----------------------------
cat("\n=== Date range by company ===\n")
print(date_ranges)

cat("\n=== Row count per company ===\n")
row_counts <- bond_data %>% count(ticker, name = "row_count")
print(row_counts)

cat("\n=== First few rows (combined data) ===\n")
print(head(bond_data))

cat("\nAll validations passed.\n")

# ============================================================
# Question 1: Bond prices (table + chart)
# ============================================================
# This section uses the existing `bond_data` object from above.
# It creates a wide price table (one row per date, one column per ticker),
# keeps prices in original units (% of face value), saves outputs,
# and produces one line chart for all five bonds.

# Check additional packages needed for Q1 output/plotting
q1_packages <- c("tidyr", "ggplot2", "readr")
q1_missing <- q1_packages[!vapply(q1_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(q1_missing) > 0) {
  stop(
    paste0(
      "Missing package(s) for Question 1: ", paste(q1_missing, collapse = ", "),
      ". Please install them first, e.g. install.packages(c(\"",
      paste(q1_missing, collapse = "\", \""),
      "\"))"
    )
  )
}

# 1) Create wide price table: one row per date, one column per ticker
price_table <- bond_data %>%
  select(date, ticker, price) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = price) %>%
  arrange(date)

# 3) Create output/ if it does not exist
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 4) Save complete price table to CSV (no unit conversion; no rounding forced)
price_table_path <- file.path(output_dir, "q1_price_table.csv")
readr::write_csv(price_table, price_table_path)

# 5-6) Plot all five bond price series and save PNG at 300 dpi
price_plot <- ggplot2::ggplot(
  bond_data,
  ggplot2::aes(x = date, y = price, color = ticker)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::labs(
    title = "Corporate Bond Prices: August 2025–August 2026",
    x = "Date",
    y = "Price (% of face value)",
    color = "Ticker"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(price_plot)

plot_path <- file.path(output_dir, "q1_bond_prices.png")
ggplot2::ggsave(
  filename = plot_path,
  plot = price_plot,
  dpi = 300,
  width = 10,
  height = 6,
  units = "in"
)

# 7) Print first few rows of price table
cat("\n=== Question 1: First few rows of price table ===\n")
print(head(price_table))

# ============================================================
# Question 2: YTM (table + chart)
# ============================================================
# This section uses the existing `bond_data` object.
# It creates a wide YTM table (one row per date, one column per ticker),
# keeps YTM in original percentage units (e.g., 4.8 means 4.8%),
# saves outputs, and produces one line chart for all five bonds.

# 1) Create wide YTM table: one row per date, one column per ticker
ytm_table <- bond_data %>%
  select(date, ticker, ytm) %>%
  tidyr::pivot_wider(names_from = ticker, values_from = ytm) %>%
  arrange(date)

# 2) Save complete YTM table to CSV (no unit conversion; no rounding forced)
ytm_table_path <- file.path(output_dir, "q2_ytm_table.csv")
readr::write_csv(ytm_table, ytm_table_path)

# 3-7) Plot all five bond YTM series and save PNG at 300 dpi
# Use a fixed ticker order so color mapping is consistent with Q1.
ticker_levels <- sort(unique(bond_data$ticker))

ytm_plot_data <- bond_data %>%
  mutate(ticker = factor(ticker, levels = ticker_levels))

ytm_plot <- ggplot2::ggplot(
  ytm_plot_data,
  ggplot2::aes(x = date, y = ytm, color = ticker)
) +
  ggplot2::geom_line(linewidth = 0.8) +
  ggplot2::labs(
    title = "Corporate Bond Yields: August 2025–August 2026",
    x = "Date",
    y = "Yield to Maturity (%)",
    color = "Ticker"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(ytm_plot)

ytm_plot_path <- file.path(output_dir, "q2_bond_yields.png")
ggplot2::ggsave(
  filename = ytm_plot_path,
  plot = ytm_plot,
  dpi = 300,
  width = 10,
  height = 6,
  units = "in"
)

# 8) Print first few rows of YTM table
cat("\n=== Question 2: First few rows of YTM table ===\n")
print(head(ytm_table))

# ============================================================
# Question 3: Descriptive statistics preparation
# ============================================================
# This section computes period-start/period-end changes and
# the three largest absolute daily YTM moves for each ticker.

# Ensure data are sorted within ticker by date for first/last and lag logic
bond_data_sorted <- bond_data %>%
  arrange(ticker, date)

# 1-5) Period changes by ticker
q3_period_changes <- bond_data_sorted %>%
  group_by(ticker) %>%
  summarise(
    first_date = first(date),
    last_date = last(date),
    first_price = first(price),
    last_price = last(price),
    `price change` = 100 * (last_price / first_price - 1),
    first_ytm = first(ytm),
    last_ytm = last(ytm),
    ytm_change_bps = 100 * (last_ytm - first_ytm),
    .groups = "drop"
  )

# Largest absolute daily YTM changes (computed separately within ticker)
q3_largest_ytm_moves <- bond_data_sorted %>%
  group_by(ticker) %>%
  mutate(
    previous_date = lag(date),
    ytm_change_bps = 100 * (ytm - lag(ytm))
  ) %>%
  filter(!is.na(ytm_change_bps)) %>%
  mutate(abs_ytm_change_bps = abs(ytm_change_bps)) %>%
  slice_max(order_by = abs_ytm_change_bps, n = 3, with_ties = FALSE) %>%
  arrange(ticker, desc(abs_ytm_change_bps), date) %>%
  select(ticker, date, previous_date, ytm_change_bps) %>%
  ungroup()

# Print both tables
cat("\n=== Question 3: Period changes by ticker ===\n")
print(q3_period_changes)

cat("\n=== Question 3: Three largest absolute daily YTM changes by ticker ===\n")
print(q3_largest_ytm_moves)

# Save both tables
q3_period_path <- file.path(output_dir, "q3_period_changes.csv")
readr::write_csv(q3_period_changes, q3_period_path)

q3_moves_path <- file.path(output_dir, "q3_largest_ytm_moves.csv")
readr::write_csv(q3_largest_ytm_moves, q3_moves_path)

# ============================================================
# Question 4: Pricing verification (single-bond check)
# ============================================================
# Formula used (semiannual compounding):
# Price = sum_{t=1..N} [Coupon / (1 + y/2)^t] + Principal / (1 + y/2)^N
# where y is YTM in decimal terms, and N is the number of remaining
# semiannual periods after the valuation date coupon is excluded.

# Bond setup from prompt
q4_ticker <- "NKE"
q4_cusip <- "654106AK9"
coupon_rate_pct <- 2.85
valuation_date <- as.Date("2026-03-27")
principal_amount <- 2000
n_periods <- 8

# 1) Extract Nike observation on valuation date and assert unique match
nke_obs <- bond_data %>%
  filter(ticker == q4_ticker, cusip == q4_cusip, date == valuation_date)

assert_that(
  nrow(nke_obs) == 1,
  msg = "Expected exactly one NKE observation for 2026-03-27."
)

# 2-4) Inputs for pricing (valuation right after coupon payment date)
# Excluding coupon paid on valuation date means only future 8 coupons remain.
price_pct <- nke_obs$price[[1]]
ytm_pct <- nke_obs$ytm[[1]]

coupon_payment <- principal_amount * (coupon_rate_pct / 100) / 2
semiannual_yield <- (ytm_pct / 100) / 2

# 5) Present value of remaining coupons + principal repayment
period_index <- seq_len(n_periods)
cash_flow <- rep(coupon_payment, n_periods)
cash_flow[n_periods] <- cash_flow[n_periods] + principal_amount

discount_factor <- 1 / ((1 + semiannual_yield) ^ period_index)
present_value <- cash_flow * discount_factor
calculated_price <- sum(present_value)

# 6) Convert observed quote (% of principal amount) to dollars
observed_price <- price_pct / 100 * principal_amount

# 8) Cash-flow table
q4_cash_flows <- data.frame(
  period = period_index,
  cash_flow = cash_flow,
  discount_factor = discount_factor,
  present_value = present_value
)

# 7) Print inputs and comparison
cat("\n=== Question 4: Pricing verification inputs ===\n")
print(
  data.frame(
    ticker = q4_ticker,
    cusip = q4_cusip,
    valuation_date = valuation_date,
    principal_amount = principal_amount,
    coupon_rate_pct = coupon_rate_pct,
    coupon_payment = coupon_payment,
    ytm_pct = ytm_pct,
    semiannual_yield = semiannual_yield,
    remaining_periods = n_periods
  )
)

price_difference <- calculated_price - observed_price

cat("\n=== Question 4: Calculated vs observed price ===\n")
q4_pricing_comparison <- data.frame(
  ticker = q4_ticker,
  cusip = q4_cusip,
  valuation_date = valuation_date,
  calculated_price = calculated_price,
  observed_price = observed_price,
  difference_calculated_minus_observed = price_difference
)
print(q4_pricing_comparison)

cat("\n=== Question 4: Cash-flow table ===\n")
print(q4_cash_flows)

# 9) Save outputs
q4_comp_path <- file.path(output_dir, "q4_pricing_comparison.csv")
readr::write_csv(q4_pricing_comparison, q4_comp_path)

q4_cf_path <- file.path(output_dir, "q4_cash_flows.csv")
readr::write_csv(q4_cash_flows, q4_cf_path)

# 10) Par check: same formula should return par when YTM equals coupon rate
semiannual_yield_par <- (coupon_rate_pct / 100) / 2
discount_factor_par <- 1 / ((1 + semiannual_yield_par) ^ period_index)
present_value_par <- cash_flow * discount_factor_par
price_at_par_yield <- sum(present_value_par)

assert_that(
  isTRUE(all.equal(price_at_par_yield, principal_amount, tolerance = 1e-8)),
  msg = "Par check failed: pricing formula did not return principal when YTM equals coupon rate."
)

# ============================================================
# Question 5: Cross-sectional comparison on final date
# ============================================================
# This section uses the final observation date, joins bond terms,
# builds a summary table, creates a YTM comparison chart,
# and compares all maturity-date pairs.

# Bond terms provided in prompt + verified metadata from
# FactSet Bond Snapshot PDFs dated 2026-11-01.
# Note: ratings below are snapshot values as of 2026-11-01;
# effective rating dates were not independently verified.
q5_terms <- tibble::tribble(
  ~ticker, ~cusip,       ~annual_coupon_pct, ~maturity_date,      ~face_value_usd, ~minimum_denomination_usd, ~sp_snapshot_rating_2026_11_01,
  "NKE",   "654106AK9", 2.85,               as.Date("2030-03-27"), 1000,            2000,                      "A",
  "RL",    "751212AD3", 5.00,               as.Date("2032-06-15"), 1000,            2000,                      "A-",
  "TPR",   "876030AA5", 3.05,               as.Date("2032-03-15"), 1000,            2000,                      "BBB+",
  "PVH",   "693656AE0", 5.50,               as.Date("2030-06-13"), 1000,            2000,                      "BBB",
  "VFC",   "918204AR9", 6.00,               as.Date("2033-10-15"), 1000,            1000,                      "BB"
)

# 1) Final observation date checks
final_obs_date <- max(bond_data$date)
assert_that(final_obs_date == as.Date("2026-08-31"), msg = "Final observation date is not 2026-08-31.")

final_obs <- bond_data %>%
  filter(date == final_obs_date)

assert_that(nrow(final_obs) == 5, msg = "Final date must contain exactly five rows (one per ticker).")
assert_that(
  final_obs %>% count(ticker) %>% summarise(all_one = all(n == 1)) %>% pull(all_one),
  msg = "Final date does not contain exactly one row per ticker."
)

# 2-3) Join terms and create summary table
q5_selected_principal <- 2000

q5_summary <- final_obs %>%
  inner_join(q5_terms, by = c("ticker", "cusip")) %>%
  mutate(
    observation_date = date,
    remaining_years_to_maturity = as.numeric(maturity_date - observation_date) / 365.25,
    selected_principal_usd = q5_selected_principal,
    dollar_price_per_face_value = price / 100 * face_value_usd,
    dollar_price_for_selected_principal = price / 100 * selected_principal_usd
  ) %>%
  transmute(
    ticker,
    cusip,
    observation_date,
    annual_coupon_pct,
    maturity_date,
    remaining_years_to_maturity,
    face_value_usd,
    minimum_denomination_usd,
    selected_principal_usd,
    price,
    dollar_price_per_face_value,
    dollar_price_for_selected_principal,
    ytm,
    sp_snapshot_rating_2026_11_01
  ) %>%
  arrange(ytm)

assert_that(nrow(q5_summary) == 5, msg = "Summary table does not contain all five bonds after join.")

cat("\n=== Question 5: Final-date summary (sorted by YTM) ===\n")
print(q5_summary, n = Inf, width = Inf)

q5_summary_path <- file.path(output_dir, "q5_summary.csv")
readr::write_csv(q5_summary, q5_summary_path)

# 4) Vertical dot plot of final-date YTMs
# Keep ticker color mapping consistent with earlier charts.
ticker_levels <- sort(unique(bond_data$ticker))

q5_plot_data <- q5_summary %>%
  mutate(
    ticker = factor(ticker, levels = ticker),
    ticker_for_color = factor(as.character(ticker), levels = ticker_levels),
    ytm_label = sprintf("%.2f", ytm)
  )

ytm_gap_bps <- 100 * (max(q5_summary$ytm) - min(q5_summary$ytm))

q5_plot <- ggplot2::ggplot(
  q5_plot_data,
  ggplot2::aes(x = ticker, y = ytm, color = ticker_for_color)
) +
  ggplot2::geom_point(size = 3) +
  ggplot2::geom_text(ggplot2::aes(label = ytm_label), vjust = -0.8, size = 3.5, show.legend = FALSE) +
  ggplot2::labs(
    title = paste0("Corporate Bond YTMs on ", final_obs_date),
    subtitle = paste0("Highest-minus-lowest YTM gap: ", sprintf("%.2f", ytm_gap_bps), " bps"),
    x = "Ticker (ordered by increasing YTM)",
    y = "Yield to Maturity (%)",
    color = "Ticker"
  ) +
  ggplot2::theme_minimal(base_size = 12)

print(q5_plot)

q5_plot_path <- file.path(output_dir, "q5_ytm_comparison.png")
ggplot2::ggsave(
  filename = q5_plot_path,
  plot = q5_plot,
  dpi = 300,
  width = 10,
  height = 6,
  units = "in"
)

cat("\n=== Question 5: Highest-minus-lowest YTM gap (bps) ===\n")
print(ytm_gap_bps)

# 5) Compare all 10 unique maturity-date pairs
pair_index <- t(combn(seq_len(nrow(q5_summary)), 2))

q5_maturity_pairs <- tibble::tibble(
  ticker_1 = q5_summary$ticker[pair_index[, 1]],
  maturity_date_1 = q5_summary$maturity_date[pair_index[, 1]],
  ytm_1 = q5_summary$ytm[pair_index[, 1]],
  ticker_2 = q5_summary$ticker[pair_index[, 2]],
  maturity_date_2 = q5_summary$maturity_date[pair_index[, 2]],
  ytm_2 = q5_summary$ytm[pair_index[, 2]],
  maturity_gap_days = abs(as.numeric(q5_summary$maturity_date[pair_index[, 1]] - q5_summary$maturity_date[pair_index[, 2]])),
  abs_ytm_diff_bps = 100 * abs(q5_summary$ytm[pair_index[, 1]] - q5_summary$ytm[pair_index[, 2]])
) %>%
  arrange(maturity_gap_days, abs_ytm_diff_bps, ticker_1, ticker_2)

q5_pairs_path <- file.path(output_dir, "q5_maturity_pairs.csv")
readr::write_csv(q5_maturity_pairs, q5_pairs_path)

min_gap_days <- min(q5_maturity_pairs$maturity_gap_days)
closest_maturity_pairs <- q5_maturity_pairs %>%
  filter(maturity_gap_days == min_gap_days)

cat("\n=== Question 5: Pair(s) with smallest maturity-date difference ===\n")
print(closest_maturity_pairs, n = Inf, width = Inf)

# ============================================================
# Q3(a): VF Corporation price and YTM figure
# ============================================================
# This section only reads existing data and creates a figure.
# It does not modify earlier analysis objects.

# Inspect which dataset is available: `bond_data` (preferred) or `bond_panel`
q3a_data_name <- if (exists("bond_data")) {
  "bond_data"
} else if (exists("bond_panel")) {
  "bond_panel"
} else {
  stop("Neither bond_data nor bond_panel exists in the current session.")
}

q3a_data <- get(q3a_data_name)

cat("\n=== Q3(a): Using dataset and columns ===\n")
cat("Dataset:", q3a_data_name, "\n")
print(names(q3a_data))

# Required columns check
required_q3a_cols <- c("date", "price", "ytm", "ticker", "cusip")
assert_that(
  all(required_q3a_cols %in% names(q3a_data)),
  msg = "Q3(a) dataset is missing one or more required columns: date, price, ytm, ticker, cusip."
)

# Filter VF Corporation observations in the assignment date range
q3a_start <- as.Date("2025-08-29")
q3a_end <- as.Date("2026-08-31")
marker_date_1 <- as.Date("2025-08-29")
marker_date_2 <- as.Date("2026-02-27")

vfc_data <- q3a_data %>%
  mutate(date = as.Date(date)) %>%
  filter(
    ticker == "VFC",
    cusip == "918204AR9",
    date >= q3a_start,
    date <= q3a_end
  ) %>%
  arrange(date)

assert_that(nrow(vfc_data) > 0, msg = "No VFC observations found for Q3(a).")

# Validation checks at two marker dates (do not change data; check values only)
vfc_check_1 <- vfc_data %>% filter(date == marker_date_1)
vfc_check_2 <- vfc_data %>% filter(date == marker_date_2)

assert_that(nrow(vfc_check_1) == 1, msg = "Expected one VFC row on 2025-08-29.")
assert_that(nrow(vfc_check_2) == 1, msg = "Expected one VFC row on 2026-02-27.")

assert_that(
  isTRUE(all.equal(vfc_check_1$price[[1]], 94.876, tolerance = 1e-9)),
  msg = "VFC price check failed for 2025-08-29."
)
assert_that(
  isTRUE(all.equal(vfc_check_1$ytm[[1]], 6.83041344739851, tolerance = 1e-12)),
  msg = "VFC YTM check failed for 2025-08-29."
)
assert_that(
  isTRUE(all.equal(vfc_check_2$price[[1]], 102.198, tolerance = 1e-9)),
  msg = "VFC price check failed for 2026-02-27."
)
assert_that(
  isTRUE(all.equal(vfc_check_2$ytm[[1]], 5.64031761085499, tolerance = 1e-12)),
  msg = "VFC YTM check failed for 2026-02-27."
)

# Build long plotting data for two vertically stacked panels
vfc_plot_data <- vfc_data %>%
  select(date, price, ytm) %>%
  tidyr::pivot_longer(
    cols = c(price, ytm),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    panel = dplyr::case_when(
      series == "price" ~ "Bond Price (% of face value)",
      series == "ytm" ~ "Yield to Maturity (%)"
    )
  )

# Marker points and labels at the two requested dates
vfc_markers <- vfc_plot_data %>%
  filter(date %in% c(marker_date_1, marker_date_2)) %>%
  mutate(
    label = dplyr::case_when(
      series == "price" ~ sprintf("%.3f", value),
      series == "ytm" ~ sprintf("%.4f", value)
    )
  )

# Create stacked figure (shared date axis, separate y scales)
vfc_line_color <- "#C51B8A"  # thin magenta line

fig_q3a <- ggplot2::ggplot(vfc_plot_data, ggplot2::aes(x = date, y = value)) +
  ggplot2::geom_line(color = vfc_line_color, linewidth = 0.55) +
  ggplot2::geom_vline(xintercept = as.numeric(marker_date_2), linetype = "dashed", linewidth = 0.4, color = "black") +
  ggplot2::geom_point(
    data = vfc_markers,
    ggplot2::aes(x = date, y = value),
    color = "black",
    size = 1.7,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = vfc_markers,
    ggplot2::aes(x = date, y = value, label = label),
    nudge_y = 0,
    vjust = -0.8,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  ggplot2::facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  ggplot2::labs(
    title = "VF Corporation (VFC): Bond Price and Yield to Maturity",
    x = "Date",
    y = NULL
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "grey88", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_line(color = "grey94", linewidth = 0.2),
    strip.background = ggplot2::element_rect(fill = "white", color = "grey80"),
    strip.text = ggplot2::element_text(face = "plain"),
    plot.title = ggplot2::element_text(size = 11, face = "plain", hjust = 0)
  )

print(fig_q3a)

q3a_plot_path <- file.path(output_dir, "fig_q3a_vf_price_yield.png")
ggplot2::ggsave(
  filename = q3a_plot_path,
  plot = fig_q3a,
  dpi = 300,
  width = 9,
  height = 6.8,
  units = "in"
)

# ============================================================
# Q3(c): PVH short-window price and YTM figure
# ============================================================
# This section only reads existing data and creates a figure.
# It does not modify earlier analysis objects or input files.

# Inspect which dataset is available: `bond_data` (preferred) or `bond_panel`
q3c_data_name <- if (exists("bond_data")) {
  "bond_data"
} else if (exists("bond_panel")) {
  "bond_panel"
} else {
  stop("Neither bond_data nor bond_panel exists in the current session.")
}

q3c_data <- get(q3c_data_name)

cat("\n=== Q3(c): Using dataset and columns ===\n")
cat("Dataset:", q3c_data_name, "\n")
print(names(q3c_data))

# Required columns check
required_q3c_cols <- c("date", "price", "ytm", "ticker", "cusip")
assert_that(
  all(required_q3c_cols %in% names(q3c_data)),
  msg = "Q3(c) dataset is missing one or more required columns: date, price, ytm, ticker, cusip."
)

# Filter PVH observations for requested window
q3c_start <- as.Date("2026-05-26")
q3c_end <- as.Date("2026-06-12")
q3c_mark_1 <- as.Date("2026-06-03")
q3c_mark_2 <- as.Date("2026-06-05")
q3c_event_line <- as.Date("2026-06-04")

pvh_data <- q3c_data %>%
  mutate(date = as.Date(date)) %>%
  filter(
    ticker == "PVH",
    cusip == "693656AE0",
    date >= q3c_start,
    date <= q3c_end
  ) %>%
  arrange(date)

assert_that(nrow(pvh_data) > 0, msg = "No PVH observations found for Q3(c).")

# Data quality checks for plotted window
assert_that(all(!is.na(pvh_data$price)), msg = "Missing PVH price values in Q3(c) window.")
assert_that(all(!is.na(pvh_data$ytm)), msg = "Missing PVH YTM values in Q3(c) window.")
assert_that(
  pvh_data %>% count(date) %>% summarise(all_one = all(n == 1)) %>% pull(all_one),
  msg = "Duplicate PVH dates found in Q3(c) window."
)

# Validation checks at requested dates (read only; no data modification)
pvh_check_1 <- pvh_data %>% filter(date == q3c_mark_1)
pvh_check_2 <- pvh_data %>% filter(date == q3c_mark_2)

assert_that(nrow(pvh_check_1) == 1, msg = "Expected one PVH row on 2026-06-03.")
assert_that(nrow(pvh_check_2) == 1, msg = "Expected one PVH row on 2026-06-05.")

assert_that(
  isTRUE(all.equal(pvh_check_1$price[[1]], 101.125, tolerance = 1e-9)),
  msg = "PVH price check failed for 2026-06-03."
)
assert_that(
  isTRUE(all.equal(pvh_check_1$ytm[[1]], 5.18596843672099, tolerance = 1e-12)),
  msg = "PVH YTM check failed for 2026-06-03."
)
assert_that(
  isTRUE(all.equal(pvh_check_2$price[[1]], 100.084, tolerance = 1e-9)),
  msg = "PVH price check failed for 2026-06-05."
)
assert_that(
  isTRUE(all.equal(pvh_check_2$ytm[[1]], 5.47555143263216, tolerance = 1e-12)),
  msg = "PVH YTM check failed for 2026-06-05."
)

# Build long plotting data for vertically stacked panels
pvh_plot_data <- pvh_data %>%
  select(date, price, ytm) %>%
  tidyr::pivot_longer(
    cols = c(price, ytm),
    names_to = "series",
    values_to = "value"
  ) %>%
  mutate(
    panel = dplyr::case_when(
      series == "price" ~ "Bond Price (% of face value)",
      series == "ytm" ~ "Yield to Maturity (%)"
    )
  )

# Marker points and labels
pvh_markers <- pvh_plot_data %>%
  filter(date %in% c(q3c_mark_1, q3c_mark_2)) %>%
  mutate(
    label = dplyr::case_when(
      series == "price" ~ sprintf("%.3f", value),
      series == "ytm" ~ sprintf("%.4f", value)
    )
  )

# Date ticks requested in prompt
q3c_breaks <- as.Date(c("2026-05-26", "2026-06-03", "2026-06-05", "2026-06-12"))

# Create stacked figure (shared x-axis, separate y scales)
pvh_line_color <- "#6B8E23"  # olive

fig_q3c <- ggplot2::ggplot(pvh_plot_data, ggplot2::aes(x = date, y = value)) +
  ggplot2::geom_line(color = pvh_line_color, linewidth = 0.55) +
  ggplot2::geom_vline(xintercept = as.numeric(q3c_event_line), linetype = "dashed", linewidth = 0.4, color = "black") +
  ggplot2::geom_point(
    data = pvh_markers,
    ggplot2::aes(x = date, y = value),
    color = "black",
    size = 1.7,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_text(
    data = pvh_markers,
    ggplot2::aes(x = date, y = value, label = label),
    vjust = -0.8,
    size = 3,
    color = "black",
    inherit.aes = FALSE
  ) +
  ggplot2::facet_wrap(~ panel, ncol = 1, scales = "free_y") +
  ggplot2::scale_x_date(
    breaks = q3c_breaks,
    date_labels = "%d %b"
  ) +
  ggplot2::labs(
    title = "PVH (693656AE0): Bond Price and Yield to Maturity",
    x = "Date",
    y = NULL
  ) +
  ggplot2::theme_bw(base_size = 11) +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(color = "grey88", linewidth = 0.3),
    panel.grid.minor = ggplot2::element_line(color = "grey94", linewidth = 0.2),
    strip.background = ggplot2::element_rect(fill = "white", color = "grey80"),
    strip.text = ggplot2::element_text(face = "plain"),
    plot.title = ggplot2::element_text(size = 11, face = "plain", hjust = 0)
  )

print(fig_q3c)

q3c_plot_path <- file.path(output_dir, "fig_q3c_pvh_price_yield.png")
ggplot2::ggsave(
  filename = q3c_plot_path,
  plot = fig_q3c,
  dpi = 300,
  width = 9,
  height = 6.8,
  units = "in"
)

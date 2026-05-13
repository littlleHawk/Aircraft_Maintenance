# =============================================================================
# DATA CLEANING PIPELINE: AIRLINE MAINTENANCE COST TIME SERIES
# =============================================================================
# Source data  : BTS Form 41 Schedule P-5.2  (T_F41SCHEDULE_P52.csv)
#                BTS Aircraft Types lookup    (T_AIRCRAFT_TYPES.csv)
# Target var   : TOT_DIR_MAINT (total direct maintenance cost, $000)
# Series level : One quarterly series per manufacturer (Airbus vs Boeing)
# Negatives    : Left in place; flagged as anomalies for analyst review
# Period       : Q1 2004 – latest available quarter
# Manufacturers: AIRBUS INDUSTRIE, BOEING
# -----------------------------------------------------------------------------
# Downstream use:
#   - SARIMA      → ts objects exported in Section 18 feed auto.arima() directly
#   - Changepoint → same ts objects feed cpt.meanvar() (changepoint package)
# =============================================================================


# -----------------------------------------------------------------------------
# 0.  PACKAGE DEPENDENCIES
# -----------------------------------------------------------------------------

required_pkgs <- c(
  "tidyverse",   # core data manipulation (dplyr, tidyr, readr, ggplot2, purrr)
  "lubridate",   # quarter() / year() helpers for date arithmetic
  "imputeTS",    # na_interpolation(): time-series-aware gap filling
  "zoo",         # na.locf(): last-observation-carried-forward for edge NAs
  "tseries",     # adf.test() / kpss.test(): stationarity diagnostics
  "changepoint"  # cpt.mean() / cpt.var(): changepoint detection (downstream)
)

# Install any package that is not already present in the library
new_pkgs <- required_pkgs[!required_pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs, quiet = TRUE)

# Load all packages silently
invisible(lapply(required_pkgs, library, character.only = TRUE))


# -----------------------------------------------------------------------------
# 1.  FILE PATHS  (edit here if CSVs live in a different directory)
# -----------------------------------------------------------------------------

PATH_P52   <- "plane_data/T_F41SCHEDULE_P52.csv"   # Form 41 Schedule P-5.2 cost records
PATH_TYPES <- "plane_data/craft_codes_lookup/T_AIRCRAFT_TYPES.csv"    # Aircraft type / manufacturer lookup


# -----------------------------------------------------------------------------
# 2.  FIXED CONFIGURATION
#     These reflect your answered modelling decisions and should not need
#     changing.  Analyst-tunable thresholds are marked with (TUNE).
# -----------------------------------------------------------------------------

TARGET_VAR       <- "TOT_DIR_MAINT"                        # primary cost column
SERIES_LEVEL     <- "manufacturer"                          # one series per mfr
NEGATIVE_ACTION  <- "flag"                                  # leave negatives; flag
START_YEAR       <- 2004                                    # first year (post-2003)
MANUFACTURERS    <- c("AIRBUS INDUSTRIE", "BOEING")         # exact lookup strings
SEASONAL_PERIOD  <- 4L                                      # quarterly data -> S=4
OUTLIER_IQR_MULT <- 3.0   # (TUNE) IQR multiplier for outlier flag (3 = conservative)
MAX_CONSEC_NA    <- 4L    # (TUNE) consecutive NA quarters that trigger a warning


# =============================================================================
# SECTION A - DATA INGESTION & JOINING
# =============================================================================

# -----------------------------------------------------------------------------
# 3.  LOAD RAW DATA
# -----------------------------------------------------------------------------

message("\n[3] Loading raw data...")

# Read the Form 41 P-5.2 cost table (~66 k rows x 63 columns)
raw_p52 <- read_csv(
  PATH_P52,
  col_types    = cols(.default = col_guess()),  # let readr infer each column type
  show_col_types = FALSE                        # suppress the column-spec message
)
message(sprintf("    P52   : %d rows x %d columns", nrow(raw_p52), ncol(raw_p52)))

# Read the aircraft type lookup (447 rows x 8 columns)
raw_types <- read_csv(
  PATH_TYPES,
  col_types    = cols(.default = col_guess()),
  show_col_types = FALSE
)
message(sprintf("    Types : %d rows x %d columns", nrow(raw_types), ncol(raw_types)))


# -----------------------------------------------------------------------------
# 4.  BUILD AIRCRAFT-TYPE LOOKUP FOR AIRBUS & BOEING ONLY
# -----------------------------------------------------------------------------

message("\n[4] Filtering aircraft type lookup to selected manufacturers...")

# Keep only rows whose MANUFACTURER matches the two configured values
lookup_ab <- raw_types %>%
  filter(MANUFACTURER %in% MANUFACTURERS) %>%            # Airbus Industrie + Boeing
  select(AC_TYPEID, MANUFACTURER, SHORT_NAME, LONG_NAME) # retain join-relevant cols only

# Hard stop if either manufacturer is missing from the lookup entirely
stopifnot(
  "Configured manufacturer not found in T_AIRCRAFT_TYPES" =
    all(MANUFACTURERS %in% lookup_ab$MANUFACTURER)
)

# Integer vector of valid aircraft type IDs; used for an efficient filter below
ab_type_ids <- lookup_ab$AC_TYPEID

message(sprintf("    Aircraft types retained : %d total", nrow(lookup_ab)))
message(sprintf("      Airbus Industrie      : %d types",
                sum(lookup_ab$MANUFACTURER == "AIRBUS INDUSTRIE")))
message(sprintf("      Boeing                : %d types",
                sum(lookup_ab$MANUFACTURER == "BOEING")))


# -----------------------------------------------------------------------------
# 5.  FILTER P-5.2 TO POST-2003 AIRBUS / BOEING RECORDS
# -----------------------------------------------------------------------------

message("\n[5] Applying year and aircraft-type filters to P52...")

filt_p52 <- raw_p52 %>%
  filter(YEAR >= START_YEAR) %>%          # keep 2004 onward (strictly post-2003)
  filter(AIRCRAFT_TYPE %in% ab_type_ids)  # keep only Airbus & Boeing type codes

message(sprintf(
  "    Rows retained : %d / %d (%.1f%%)",
  nrow(filt_p52), nrow(raw_p52),
  100 * nrow(filt_p52) / nrow(raw_p52)
))


# -----------------------------------------------------------------------------
# 6.  JOIN MANUFACTURER LABELS ONTO COST RECORDS
# -----------------------------------------------------------------------------

message("\n[6] Joining manufacturer labels...")

# Left-join so every P52 row receives its MANUFACTURER and SHORT_NAME
joined <- filt_p52 %>%
  left_join(
    lookup_ab,                                    # bring in MANUFACTURER, SHORT_NAME
    by = c("AIRCRAFT_TYPE" = "AC_TYPEID")         # join key: P52 type -> lookup ID
  )

# Count rows that failed the join (should be zero given the earlier filter)
join_miss <- sum(is.na(joined$MANUFACTURER))
if (join_miss > 0) {
  warning(sprintf(
    "%d rows could not be matched to a manufacturer after join - inspect AIRCRAFT_TYPE values",
    join_miss
  ))
}

# Drop unmatched rows so the rest of the pipeline operates on clean data
joined <- joined %>% filter(!is.na(MANUFACTURER))

message(sprintf("    Matched rows : %d", nrow(joined)))


# =============================================================================
# SECTION B - VARIABLE CONSTRUCTION & COLUMN SELECTION
# =============================================================================

# -----------------------------------------------------------------------------
# 7.  CONSTRUCT COMPOSITE COST COLUMNS
#     These are always computed and are available as model covariates even
#     when TARGET_VAR remains TOT_DIR_MAINT.
# -----------------------------------------------------------------------------

joined <- joined %>%
  mutate(
    # Airframe direct cost: labour + repair labour + materials.
    # na.rm = TRUE treats an individual missing component as 0 in the sum.
    # If ALL three components are NA the result is corrected back to NA below.
    AIRFRAME_COST = rowSums(
      pick(AIRFRAME_LABOR, AIRFRAME_REPAIR, AIRFRAME_MATERIALS),
      na.rm = TRUE
    ),
    # Engine direct cost: labour + repair + materials
    ENGINE_COST = rowSums(
      pick(ENGINE_LABOR, ENGINE_REPAIRS, ENGINE_MATERIALS),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    # Revert computed zeros that arose from all-NA inputs back to NA
    AIRFRAME_COST = if_else(
      is.na(AIRFRAME_LABOR) & is.na(AIRFRAME_REPAIR) & is.na(AIRFRAME_MATERIALS),
      NA_real_, AIRFRAME_COST
    ),
    ENGINE_COST = if_else(
      is.na(ENGINE_LABOR) & is.na(ENGINE_REPAIRS) & is.na(ENGINE_MATERIALS),
      NA_real_, ENGINE_COST
    )
  )


# -----------------------------------------------------------------------------
# 8.  SELECT AND RENAME COLUMNS RELEVANT TO THE MODELLING PIPELINE
# -----------------------------------------------------------------------------

cost_data <- joined %>%
  select(
    # Time index
    YEAR, QUARTER,
    
    # Grouping keys
    MANUFACTURER,      # "AIRBUS INDUSTRIE" or "BOEING"
    SHORT_NAME,        # human-readable aircraft label (e.g. "B737", "A320")
    AIRCRAFT_TYPE,     # numeric BTS type code (useful for type-level drill-down)
    CARRIER,           # IATA carrier code (useful for future carrier-level work)
    
    # Aircraft metadata
    AIRCRAFT_CONFIG,   # 1 = narrow-body, 2 = wide-body, 3 = other
    
    # Operational exposure (used for normalisation if ever needed)
    TOTAL_AIR_HOURS,   # flight hours flown in the quarter
    
    # Target variable
    TOT_DIR_MAINT,     # total direct maintenance cost ($000) <- TARGET_VAR
    
    # Alternative / component costs (useful as SARIMA exogenous regressors)
    TOT_FLT_MAINT_MEMO,  # flight maintenance memo cost ($000)
    AIRFRAME_LABOR,      # airframe maintenance labour ($000)
    ENGINE_LABOR,        # engine maintenance labour ($000)
    AIRFRAME_REPAIR,     # airframe repair costs ($000)
    ENGINE_REPAIRS,      # engine repair costs ($000)
    AIRFRAME_MATERIALS,  # airframe parts and materials ($000)
    ENGINE_MATERIALS,    # engine parts and materials ($000)
    AIRFRAME_OVERHAULS,  # scheduled airframe overhaul costs ($000)
    ENGINE_OVERHAULS,    # scheduled engine overhaul costs ($000)
    AIRFRAME_COST,       # computed: airframe labour + repair + materials
    ENGINE_COST          # computed: engine labour + repairs + materials
  )


# =============================================================================
# SECTION C - DATA TYPE COERCION & DATE CONSTRUCTION
# =============================================================================

# -----------------------------------------------------------------------------
# 9.  COERCE COLUMN TYPES
# -----------------------------------------------------------------------------

message("\n[9] Coercing column types...")

cost_data <- cost_data %>%
  mutate(
    YEAR            = as.integer(YEAR),           # ensure integer, not double
    QUARTER         = as.integer(QUARTER),        # 1, 2, 3, or 4 only
    MANUFACTURER    = as.factor(MANUFACTURER),    # factor enables group_by efficiency
    AIRCRAFT_CONFIG = as.factor(AIRCRAFT_CONFIG)  # 1/2/3 are category labels
  )

# Remove records with a QUARTER value outside the valid 1-4 range
invalid_q <- cost_data %>% filter(!(QUARTER %in% 1:4))
if (nrow(invalid_q) > 0) {
  warning(sprintf(
    "%d rows have QUARTER outside 1-4 and will be removed: values seen = %s",
    nrow(invalid_q), paste(unique(invalid_q$QUARTER), collapse = ", ")
  ))
  cost_data <- cost_data %>% filter(QUARTER %in% 1:4)
}


# -----------------------------------------------------------------------------
# 10. BUILD A DATE COLUMN FROM YEAR + QUARTER
# -----------------------------------------------------------------------------
#
# Represent each quarterly record as the first calendar day of that quarter.
# Q1 -> 1 Jan, Q2 -> 1 Apr, Q3 -> 1 Jul, Q4 -> 1 Oct.
# A proper Date vector lets lubridate / zoo / ts() anchor the time axis exactly.

cost_data <- cost_data %>%
  mutate(
    QUARTER_MONTH = (QUARTER - 1L) * 3L + 1L,       # month: 1, 4, 7, or 10
    DATE = as.Date(
      sprintf("%04d-%02d-01", YEAR, QUARTER_MONTH)   # "YYYY-MM-01" format string
    )
  ) %>%
  select(-QUARTER_MONTH)                             # drop the intermediate helper

message(sprintf(
  "    Date range in filtered data: %s to %s",
  min(cost_data$DATE), max(cost_data$DATE)
))


# =============================================================================
# SECTION D - NEGATIVE VALUE HANDLING & ANOMALY FLAGGING
# =============================================================================

# -----------------------------------------------------------------------------
# 11. FLAG NEGATIVE MAINTENANCE COST VALUES (leave values in place)
# -----------------------------------------------------------------------------
#
# Decision: retain negatives and attach a binary flag.
#
# Negative values in Form 41 P-5.2 arise from accounting credits or reversals
# (e.g. warranty recoveries, insurance reimbursements, prior-period corrections).
# They are not necessarily data entry errors.  However, they distort a cost
# time series and should be reviewed before modelling.
#
# Strategy: attach NEGATIVE_FLAG at the carrier x aircraft-type row level
# BEFORE aggregation, so the quarterly aggregate records N_NEGATIVE (how many
# underlying rows contributed a negative value to the sum).  This lets the
# analyst inspect whether negatives are isolated credits or systemic issues.

message("\n[11] Flagging negative TOT_DIR_MAINT values...")

cost_data <- cost_data %>%
  mutate(
    # 1 if TOT_DIR_MAINT is strictly negative; 0 if non-negative; NA if missing
    NEGATIVE_FLAG = case_when(
      is.na(TOT_DIR_MAINT)  ~ NA_integer_,   # cost is missing: flag is unknown
      TOT_DIR_MAINT < 0     ~ 1L,            # confirmed negative credit/reversal
      TRUE                  ~ 0L             # zero or positive: no flag
    )
  )

# Report the scale of the problem for the analyst
neg_count <- sum(cost_data$NEGATIVE_FLAG == 1L, na.rm = TRUE)
message(sprintf(
  "    Negative TOT_DIR_MAINT records : %d (%.2f%% of non-missing rows)",
  neg_count,
  100 * neg_count / sum(!is.na(cost_data$TOT_DIR_MAINT))
))

# Print breakdown by manufacturer so the analyst can see if one is worse
cost_data %>%
  filter(NEGATIVE_FLAG == 1L) %>%
  group_by(MANUFACTURER) %>%
  summarise(
    N_NEGATIVE = n(),
    MIN_VAL    = round(min(TOT_DIR_MAINT, na.rm = TRUE), 2),
    MAX_VAL    = round(max(TOT_DIR_MAINT, na.rm = TRUE), 2),
    .groups    = "drop"
  ) %>%
  { message("    Breakdown of negatives by manufacturer:"); print(.) }


# =============================================================================
# SECTION E - QUARTERLY AGGREGATION
# =============================================================================

# -----------------------------------------------------------------------------
# 12. AGGREGATE CARRIER x AIRCRAFT-TYPE ROWS TO MANUFACTURER x QUARTER
# -----------------------------------------------------------------------------
#
# Each row in cost_data is one carrier x aircraft-type x quarter observation.
# SARIMA requires a single value per time period per series.
# Cost is additive across carriers and aircraft types, so we use SUM.
# The N_* columns document what went into each aggregate for audit purposes.

message("\n[12] Aggregating to manufacturer-level quarterly series...")

quarterly <- cost_data %>%
  group_by(MANUFACTURER, YEAR, QUARTER, DATE) %>%
  summarise(
    # Sum costs across all carriers and aircraft types in this manufacturer-quarter
    COST            = sum(TOT_DIR_MAINT,   na.rm = TRUE), # aggregate target ($000)
    AIRFRAME_COST_Q = sum(AIRFRAME_COST,   na.rm = TRUE), # airframe component total
    ENGINE_COST_Q   = sum(ENGINE_COST,     na.rm = TRUE), # engine component total
    AIR_HOURS_Q     = sum(TOTAL_AIR_HOURS, na.rm = TRUE), # total flight hours
    
    # Audit columns: how many underlying rows fed this quarterly aggregate
    N_RECORDS       = n(),                                  # total rows in group
    N_MISSING_COST  = sum(is.na(TOT_DIR_MAINT)),           # rows with missing cost
    N_NEGATIVE      = sum(NEGATIVE_FLAG == 1L, na.rm = TRUE), # rows flagged negative
    .groups = "drop"
  ) %>%
  # If every row in a group had missing cost, na.rm=TRUE produces 0; revert to NA
  mutate(
    COST = if_else(N_RECORDS == N_MISSING_COST, NA_real_, COST)
  ) %>%
  arrange(MANUFACTURER, DATE)  # sort chronologically within each series

message(sprintf(
  "    Quarterly rows produced : %d (%d manufacturers x quarters)",
  nrow(quarterly), nrow(quarterly)
))


# =============================================================================
# SECTION F - DATE SPINE & GAP FILLING
# =============================================================================

# -----------------------------------------------------------------------------
# 13. BUILD A COMPLETE QUARTERLY DATE SPINE AND FILL STRUCTURAL GAPS
# -----------------------------------------------------------------------------
#
# SARIMA requires a strictly regular, gap-free time index.
# We generate the full sequence of quarter-start dates from START_YEAR Q1
# to the latest observed quarter, then cross it with the two manufacturers.
# Any quarter that exists in the spine but not in the aggregated data becomes
# a row with COST = NA, handled by imputation in Section 14.

message("\n[13] Building complete quarterly date spine...")

# Full sequence of quarter-start dates from 2004-01-01 to last observed quarter
all_quarter_dates <- seq.Date(
  from = as.Date(sprintf("%04d-01-01", START_YEAR)),  # 2004-01-01 fixed start
  to   = max(quarterly$DATE, na.rm = TRUE),           # last quarter in the data
  by   = "quarter"                                    # step by exactly 3 months
)

message(sprintf(
  "    Spine : %s to %s -> %d quarters",
  min(all_quarter_dates), max(all_quarter_dates), length(all_quarter_dates)
))

# Cross-product: both manufacturers x every quarter -> complete panel skeleton
full_spine <- crossing(
  MANUFACTURER = factor(MANUFACTURERS, levels = MANUFACTURERS), # both mfrs
  DATE         = all_quarter_dates                              # every quarter
) %>%
  mutate(
    YEAR    = year(DATE),     # reconstruct YEAR from DATE for the join key
    QUARTER = quarter(DATE)   # reconstruct QUARTER using lubridate
  )

# Left-join aggregated data onto the spine; missing quarters get NA in cost cols
quarterly <- full_spine %>%
  left_join(
    quarterly,
    by = c("MANUFACTURER", "YEAR", "QUARTER", "DATE") # join on all time keys
  ) %>%
  arrange(MANUFACTURER, DATE)  # re-sort after join

message(sprintf(
  "    Rows after spine join : %d (spine had %d)",
  nrow(quarterly), nrow(full_spine)
))


# =============================================================================
# SECTION G - IMPUTATION
# =============================================================================

# -----------------------------------------------------------------------------
# 14. TIME-SERIES-AWARE IMPUTATION OF MISSING QUARTERLY COST VALUES
# -----------------------------------------------------------------------------
#
# Strategy (applied independently per manufacturer series):
#   1. Interior NAs   -> linear interpolation (imputeTS::na_interpolation)
#      Assumes a smooth trend between bracketing observations; suitable for
#      cost series where abrupt jumps between known values are unlikely.
#   2. Leading NAs    -> next-observation-carried-back (na.locf fromLast=TRUE)
#      Fills quarters before the first real observation with that first value.
#   3. Trailing NAs   -> last-observation-carried-forward (na.locf fromLast=FALSE)
#      Fills quarters after the last real observation with that last value.
#
# IMPUTED_FLAG = 1 marks every quarter whose raw COST was NA, so the analyst
# can distinguish observed data from estimates during model inspection.
#
# A warning fires if a series has > MAX_CONSEC_NA consecutive missing quarters
# (i.e. > 1 year gap), as interpolation quality degrades over long spans.

impute_quarterly_series <- function(cost_vec, series_label = "") {
  
  # Count the longest run of consecutive NAs in this series
  runs    <- rle(is.na(cost_vec))
  max_gap <- if (any(runs$values)) max(runs$lengths[runs$values]) else 0L
  
  if (max_gap > MAX_CONSEC_NA) {
    warning(sprintf(
      "Series '%s': run of %d consecutive missing quarters (threshold = %d) - interpolation may be unreliable",
      series_label, max_gap, MAX_CONSEC_NA
    ))
  }
  
  # Step 1: linear interpolation fills interior NAs between observed values
  x <- na_interpolation(cost_vec, option = "linear")
  
  # Step 2: next-obs-carried-back handles leading NAs (before first real value)
  x <- na.locf(x, fromLast = TRUE,  na.rm = FALSE)
  
  # Step 3: last-obs-carried-forward handles trailing NAs (after last real value)
  x <- na.locf(x, fromLast = FALSE, na.rm = FALSE)
  
  x
}

message("\n[14] Imputing missing quarterly values within each manufacturer series...")

quarterly <- quarterly %>%
  group_by(MANUFACTURER) %>%
  mutate(
    IMPUTED_FLAG = as.integer(is.na(COST)),  # 1 = this quarter will be imputed
    COST_IMPUTED = impute_quarterly_series(
      COST,
      series_label = as.character(cur_group()$MANUFACTURER)
    )
  ) %>%
  ungroup()

message(sprintf(
  "    Imputed quarters : %d of %d total (%.1f%%)",
  sum(quarterly$IMPUTED_FLAG, na.rm = TRUE),
  nrow(quarterly),
  100 * mean(quarterly$IMPUTED_FLAG, na.rm = TRUE)
))


# =============================================================================
# SECTION H - OUTLIER DETECTION
# =============================================================================

# -----------------------------------------------------------------------------
# 15. FLAG STATISTICAL OUTLIERS WITHIN EACH MANUFACTURER SERIES
# -----------------------------------------------------------------------------
#
# Method: Tukey IQR fence, computed independently per manufacturer series.
# A value is flagged if it falls more than OUTLIER_IQR_MULT x IQR below Q1
# or above Q3.  At the default multiplier of 3.0 this is the "extreme outlier"
# fence (Tukey's "mild" fence uses 1.5).
#
# Outliers are FLAGGED, not removed.  The analyst should inspect them:
#   - Genuine structural breaks -> strong candidates for changepoint seeds
#   - Data errors               -> consider Winsorising before SARIMA fitting
#
# OUTLIER_FLAG = 1 marks affected quarters; applied to COST_IMPUTED (the
# fully gap-filled series) so that imputed quarters can also be flagged.

flag_outliers_iqr <- function(x, mult = OUTLIER_IQR_MULT) {
  q25   <- quantile(x, 0.25, na.rm = TRUE)          # first quartile of series
  q75   <- quantile(x, 0.75, na.rm = TRUE)          # third quartile of series
  iqr   <- q75 - q25                                # interquartile range
  lower <- q25 - mult * iqr                         # lower extreme fence
  upper <- q75 + mult * iqr                         # upper extreme fence
  as.integer(!is.na(x) & (x < lower | x > upper))  # 1 = outside fence
}

message("\n[15] Flagging outliers (IQR method, multiplier = ", OUTLIER_IQR_MULT, ")...")

quarterly <- quarterly %>%
  group_by(MANUFACTURER) %>%
  mutate(OUTLIER_FLAG = flag_outliers_iqr(COST_IMPUTED)) %>%  # per-series fence
  ungroup()

# Summary for analyst review
quarterly %>%
  group_by(MANUFACTURER) %>%
  summarise(
    N_OUTLIERS   = sum(OUTLIER_FLAG, na.rm = TRUE),
    OUTLIER_PCT  = round(100 * mean(OUTLIER_FLAG, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  { message("    Outlier summary by manufacturer:"); print(.) }


# =============================================================================
# SECTION I - STATIONARITY & SEASONALITY DIAGNOSTICS
# =============================================================================

# -----------------------------------------------------------------------------
# 16. STATIONARITY TESTS (ADF + KPSS) PER MANUFACTURER SERIES
# -----------------------------------------------------------------------------
#
# SARIMA(p, d, q)(P, D, Q)[4] requires specifying d (non-seasonal differencing
# order) and D (seasonal differencing order).  We run two complementary tests:
#
# ADF test  : H0 = unit root present (series is non-stationary).
#             p < 0.05 -> reject H0 -> series is likely stationary.
# KPSS test : H0 = series is stationary.
#             p < 0.05 -> reject H0 -> series is likely non-stationary.
#
# Guidance for d:
#   ADF rejects + KPSS does not reject -> d = 0 (stationary as-is)
#   ADF does not reject + KPSS rejects -> d = 1 (one difference needed)
#   Tests disagree                      -> inspect ACF/PACF plots manually

run_stationarity_tests <- function(x, label = "") {
  x_clean <- na.omit(x)               # both tests require a complete series
  
  if (length(x_clean) < 8) {          # minimum length for reliable test results
    return(tibble(
      SERIES               = label,
      ADF_STATISTIC        = NA_real_,
      ADF_PVAL             = NA_real_,
      KPSS_STATISTIC       = NA_real_,
      KPSS_PVAL            = NA_real_,
      LIKELY_NONSTATIONARY = NA,
      SUGGESTED_D          = NA_integer_
    ))
  }
  
  adf  <- tryCatch(adf.test(x_clean),  error = function(e) NULL) # Augmented Dickey-Fuller
  kpss <- tryCatch(kpss.test(x_clean), error = function(e) NULL) # Kwiatkowski-Phillips-Schmidt-Shin
  
  adf_p  <- if (!is.null(adf))  adf$p.value  else NA_real_
  kpss_p <- if (!is.null(kpss)) kpss$p.value else NA_real_
  
  # Non-stationary if ADF fails to reject (p > 0.05) OR KPSS rejects (p < 0.05)
  nonstat <- (!is.na(adf_p)  & adf_p  > 0.05) |
    (!is.na(kpss_p) & kpss_p < 0.05)
  
  tibble(
    SERIES               = label,
    ADF_STATISTIC        = if (!is.null(adf))  round(as.numeric(adf$statistic),  4) else NA_real_,
    ADF_PVAL             = round(adf_p,  4),
    KPSS_STATISTIC       = if (!is.null(kpss)) round(as.numeric(kpss$statistic), 4) else NA_real_,
    KPSS_PVAL            = round(kpss_p, 4),
    LIKELY_NONSTATIONARY = nonstat,
    SUGGESTED_D          = as.integer(nonstat)  # 0 = stationary; 1 = difference once
  )
}

message("\n[16] Running stationarity tests per manufacturer series...")

stationarity_results <- quarterly %>%
  group_by(MANUFACTURER) %>%
  summarise(
    result = list(run_stationarity_tests(
      COST_IMPUTED,
      label = as.character(cur_group()$MANUFACTURER)
    )),
    .groups = "drop"
  ) %>%
  pull(result) %>%
  bind_rows()

message("    Stationarity diagnostics (use SUGGESTED_D as the d= argument in auto.arima):")
print(stationarity_results)


# -----------------------------------------------------------------------------
# 17. SEASONAL STRENGTH DIAGNOSTIC (guides SARIMA seasonal D parameter)
# -----------------------------------------------------------------------------
#
# We decompose each series using STL (Seasonal-Trend-Loess) and compute the
# seasonal strength metric (Wang, Smith & Hyndman 2006):
#
#   Fs = max(0, 1 - Var(remainder) / Var(seasonal + remainder))
#
# Fs close to 1 -> strong quarterly pattern -> use D = 1 in SARIMA
# Fs close to 0 -> negligible seasonality   -> D = 0; check ACF at lag 4
#
# robust = TRUE in stl() down-weights outlier influence on the decomposition.

compute_seasonal_strength <- function(x, freq = SEASONAL_PERIOD) {
  x_clean <- na.omit(x)
  if (length(x_clean) < 2 * freq) return(NA_real_)  # need at least 2 full cycles
  
  ts_obj  <- ts(x_clean, frequency = freq)  # quarterly ts object, no fixed start
  
  stl_fit <- tryCatch(
    stl(ts_obj, s.window = "periodic", robust = TRUE),  # robust STL decomposition
    error = function(e) NULL
  )
  if (is.null(stl_fit)) return(NA_real_)
  
  comp      <- stl_fit$time.series                         # columns: seasonal, trend, remainder
  var_rem   <- var(comp[, "remainder"],                 na.rm = TRUE)  # variance of noise
  var_s_rem <- var(comp[, "seasonal"] + comp[, "remainder"], na.rm = TRUE)  # variance of seasonal + noise
  
  if (var_s_rem == 0) return(0)
  max(0, 1 - var_rem / var_s_rem)  # bounded to [0, 1]
}

message("\n[17] Computing seasonal strength per manufacturer series...")

seasonal_diagnostics <- quarterly %>%
  group_by(MANUFACTURER) %>%
  summarise(
    SEASONAL_STRENGTH = round(compute_seasonal_strength(COST_IMPUTED), 4),
    .groups = "drop"
  ) %>%
  mutate(
    SUGGESTED_D_SEAS = as.integer(SEASONAL_STRENGTH > 0.6)  # 1 = use seasonal difference
  )

message("    Seasonal strength (> 0.6 -> SUGGESTED_D_SEAS = 1 for SARIMA seasonal order):")
print(seasonal_diagnostics)


# =============================================================================
# SECTION J - BUILD MODELLING-READY ts OBJECTS
# =============================================================================

# -----------------------------------------------------------------------------
# 18. CONSTRUCT ONE ts() OBJECT PER MANUFACTURER
# -----------------------------------------------------------------------------
#
# auto.arima() and changepoint::cpt.meanvar() both accept base-R ts objects.
# We produce a named list so the modelling script can reference each series
# directly by manufacturer name:
#
#   ts_list[["AIRBUS INDUSTRIE"]]  -> quarterly ts, 2004 Q1 onward
#   ts_list[["BOEING"]]            -> quarterly ts, 2004 Q1 onward
#
# The ts() start argument uses c(year, quarter_number) of the first observation.
# COST_IMPUTED is used as the series vector (fully gap-filled; flag columns
# are preserved in the `quarterly` data frame for post-modelling annotation).

message("\n[18] Building ts() objects for each manufacturer series...")

build_manufacturer_ts <- function(df_mfr) {
  df_sorted <- df_mfr %>% arrange(DATE)               # ensure chronological order
  
  start_yr <- year(min(df_sorted$DATE))               # first year in series
  start_q  <- quarter(min(df_sorted$DATE))            # first quarter in series
  
  ts(
    data      = df_sorted$COST_IMPUTED,               # gap-filled cost vector
    start     = c(start_yr, start_q),                  # e.g. c(2004, 1) for Q1 2004
    frequency = SEASONAL_PERIOD                        # 4 for quarterly data
  )
}

# Split quarterly data frame by manufacturer, apply ts() constructor, name list
ts_list <- quarterly %>%
  group_by(MANUFACTURER) %>%
  group_split() %>%                   # returns list of one tibble per manufacturer
  setNames(MANUFACTURERS) %>%         # name list elements by manufacturer string
  lapply(build_manufacturer_ts)       # apply ts() builder to each element

# Print a brief summary of each ts object to confirm structure
for (nm in names(ts_list)) {
  s <- ts_list[[nm]]
  message(sprintf(
    "    %-20s : %d quarters | start %d Q%d | end %d Q%d",
    nm, length(s),
    start(s)[1], start(s)[2],
    end(s)[1],   end(s)[2]
  ))
}


# =============================================================================
# SECTION K - FINAL QUALITY REPORT & OUTPUT
# =============================================================================

# -----------------------------------------------------------------------------
# 19. COMPILE COMPREHENSIVE PER-SERIES QUALITY SUMMARY
# -----------------------------------------------------------------------------

message("\n[19] Compiling quality summary...")

quality_summary <- quarterly %>%
  group_by(MANUFACTURER) %>%
  summarise(
    N_QUARTERS     = n(),                                     # total quarters in spine
    N_OBSERVED     = sum(IMPUTED_FLAG == 0, na.rm = TRUE),   # quarters with real data
    N_IMPUTED      = sum(IMPUTED_FLAG == 1, na.rm = TRUE),   # gap-filled quarters
    N_OUTLIER      = sum(OUTLIER_FLAG,      na.rm = TRUE),   # flagged outliers
    N_NEG_CONTRIB  = sum(N_NEGATIVE,        na.rm = TRUE),   # cumulative negative-row count
    COST_MEAN      = round(mean(COST_IMPUTED, na.rm = TRUE), 2),
    COST_SD        = round(sd(COST_IMPUTED,   na.rm = TRUE), 2),
    COST_MIN       = round(min(COST_IMPUTED,  na.rm = TRUE), 2),
    COST_MAX       = round(max(COST_IMPUTED,  na.rm = TRUE), 2),
    DATE_FIRST     = min(DATE),
    DATE_LAST      = max(DATE),
    .groups = "drop"
  )

# Attach stationarity and seasonal diagnostics to the summary
quality_summary <- quality_summary %>%
  left_join(
    stationarity_results %>%
      select(MANUFACTURER = SERIES, SUGGESTED_D, ADF_PVAL, KPSS_PVAL),
    by = "MANUFACTURER"
  ) %>%
  left_join(
    seasonal_diagnostics %>%
      mutate(MANUFACTURER = as.character(MANUFACTURER)),
    by = "MANUFACTURER"
  )

message("    Final quality summary:")
print(quality_summary)


# -----------------------------------------------------------------------------
# 20. SAVE OUTPUTS
# -----------------------------------------------------------------------------

message("\n[20] Saving outputs...")

# (a) Full cleaned quarterly panel with all flag columns (for audit / EDA)
output_panel <- "maintenance_quarterly_clean.csv"
write_csv(quarterly, output_panel)
message(sprintf("    Panel CSV   : %s  (%d rows x %d columns)",
                output_panel, nrow(quarterly), ncol(quarterly)))

# (b) Diagnostics summary (stationarity + seasonal + quality per manufacturer)
output_diag <- "maintenance_diagnostics.csv"
write_csv(quality_summary, output_diag)
message(sprintf("    Diagnostics : %s", output_diag))

# (c) Named list of ts() objects saved as an RDS file for direct use in modelling
output_rds <- "maintenance_ts_list.rds"
saveRDS(ts_list, output_rds)
message(sprintf("    ts list RDS : %s", output_rds))

message("\n================================================================")
message(" Pipeline complete.  Modelling-ready objects:")
message("   ts_list[[\"AIRBUS INDUSTRIE\"]]  ->  auto.arima() + cpt.meanvar()")
message("   ts_list[[\"BOEING\"]]            ->  auto.arima() + cpt.meanvar()")
message("   quarterly                      ->  full panel with IMPUTED_FLAG,")
message("                                      OUTLIER_FLAG, NEGATIVE_FLAG")
message("================================================================\n")


# =============================================================================
# APPENDIX: DOWNSTREAM MODELLING STUBS (not run by this script)
# =============================================================================
#
# These stubs show how the pipeline outputs connect directly to SARIMA fitting
# and changepoint detection.  Wrap them in a separate modelling script that
# calls readRDS("maintenance_ts_list.rds") to load the ts_list object.

if (FALSE) {
  
  library(forecast)
  library(changepoint)
  
  # Reload diagnostics if running modelling script standalone
  ts_list             <- readRDS("maintenance_ts_list.rds")
  stationarity_results <- read_csv("maintenance_diagnostics.csv")
  
  for (mfr in names(ts_list)) {
    
    # ── SARIMA fit ───────────────────────────────────────────────────────────
    # auto.arima() performs a grid search over (p,d,q)(P,D,Q)[4].
    # Supplying d and D from our diagnostics constrains the search space and
    # prevents the optimiser from choosing an inappropriate differencing order.
    
    d_val <- stationarity_results$SUGGESTED_D[stationarity_results$MANUFACTURER == mfr]
    D_val <- stationarity_results$SUGGESTED_D_SEAS[stationarity_results$MANUFACTURER == mfr]
    
    cat(sprintf("\n=== SARIMA: %s (d=%d, D=%d) ===\n", mfr, d_val, D_val))
    
    fit_sarima <- auto.arima(
      ts_list[[mfr]],
      d             = d_val,     # non-seasonal differencing order from ADF/KPSS
      D             = D_val,     # seasonal differencing order from STL strength
      stepwise      = FALSE,     # exhaustive search (slower but finds better model)
      approximation = FALSE,     # exact likelihood (more accurate AIC)
      seasonal      = TRUE,      # include seasonal component
      ic            = "aicc"     # AICc penalises small-sample overfitting
    )
    
    print(summary(fit_sarima))
    checkresiduals(fit_sarima)   # Ljung-Box test + residual ACF plot
    
    # ── Changepoint detection ─────────────────────────────────────────────────
    # cpt.meanvar() tests simultaneously for shifts in mean AND variance, which
    # is typical for maintenance cost series that jump after major events
    # (e.g. fleet grounding, COVID-19, new maintenance contracts).
    # PELT (Pruned Exact Linear Time) is O(n) and scales well to 80+ quarters.
    # BIC penalty is conservative: favours fewer, more credible changepoints.
    
    cat(sprintf("\n=== Changepoints: %s ===\n", mfr))
    
    fit_cpt <- cpt.meanvar(
      ts_list[[mfr]],
      method    = "PELT",  # efficient exact algorithm for many potential changepoints
      penalty   = "BIC",   # Bayesian Information Criterion: penalises extra changepoints
      minseglen = 4L       # minimum 4 quarters (1 year) between consecutive changepoints
    )
    
    cat("Changepoint locations (quarter index within series):\n")
    print(cpts(fit_cpt))   # integer indices where mean/variance shifts were detected
    plot(fit_cpt, main = paste("Maintenance Cost Changepoints -", mfr),
         ylab = "TOT_DIR_MAINT ($000)", xlab = "Quarter")
  }
  
}

# ── END OF SCRIPT ─────────────────────────────────────────────────────────────

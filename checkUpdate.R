library(tidyverse)
devtools::load_all(".")
googledrive::drive_auth(email = "nick.mckay2@gmail.com", cache = ".secret")
googlesheets4::gs4_auth(email = "nick.mckay2@gmail.com", cache = ".secret")

newQC <- read_sheet_retry("1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8", sheet = "QC")
oldQC <- read_sheet_retry("1FqMMdjH8qcwcrIt0bGXOxc3cYi4OtbBDJTi85re6sIg")

shared_vars <- setdiff(intersect(names(newQC), names(oldQC)), "TSid")
big <- inner_join(newQC, oldQC, by = "TSid")

coerce_col <- function(x) {
  if (is.list(x)) map_chr(x, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]]))
  else as.character(x)
}

# =============================================================
# Step 1: Check — summarize discrepancies between QC sheets
# =============================================================

check_summary <- map_dfr(shared_vars, function(v) {
  vx <- paste0(v, ".x")
  vy <- paste0(v, ".y")
  if (!all(c(vx, vy) %in% names(big))) return(NULL)
  col_x <- coerce_col(big[[vx]])
  col_y <- coerce_col(big[[vy]])
  tibble(
    variable   = v,
    n_missing  = sum( is.na(col_x) & !is.na(col_y), na.rm = TRUE),
    n_spurious = sum(!is.na(col_x) &  is.na(col_y), na.rm = TRUE),
    n_mismatch = sum(!is.na(col_x) & !is.na(col_y) & col_x != col_y, na.rm = TRUE)
  )
})

cat("=== Values present in old QC but missing in new ===\n")
missing_check <- filter(check_summary, n_missing > 0) %>% arrange(desc(n_missing))
if (nrow(missing_check) == 0) cat("None.\n") else print(missing_check %>% select(variable, n_missing))

cat("\n=== Interpretation field anomalies (spurious fills or wrong values) ===\n")
interp_check <- filter(check_summary, grepl("Interpretation", variable), n_spurious + n_mismatch > 0) %>%
  arrange(desc(n_spurious + n_mismatch))
if (nrow(interp_check) == 0) cat("None.\n") else print(interp_check %>% select(variable, n_spurious, n_mismatch))

# =============================================================
# Step 2: Restore — apply all fixes and write to "fixed" sheet
# =============================================================

newQC_fixed <- newQC

# Restore values lost to NA (all columns)
updates_log <- map_dfr(shared_vars, function(v) {
  vx <- paste0(v, ".x")
  vy <- paste0(v, ".y")
  if (!all(c(vx, vy) %in% names(big))) return(NULL)
  col_x <- coerce_col(big[[vx]])
  col_y <- coerce_col(big[[vy]])
  fill_idx <- which(is.na(col_x) & !is.na(col_y))
  if (length(fill_idx) == 0) return(NULL)
  match_idx <- match(big$TSid[fill_idx], newQC_fixed$TSid)
  valid <- !is.na(match_idx)
  if (is.list(newQC_fixed[[v]]) || (is.logical(newQC_fixed[[v]]) && all(is.na(newQC_fixed[[v]])))) {
    newQC_fixed[[v]] <<- rep(NA_character_, nrow(newQC_fixed))
  }
  newQC_fixed[[v]][match_idx[valid]] <<- col_y[fill_idx[valid]]
  tibble(TSid = big$TSid[fill_idx[valid]], variable = v, restored_value = col_y[fill_idx[valid]])
})

# Correct spurious/wrong interpretation values (interpretation columns only)
interp_vars <- grep("Interpretation", shared_vars, value = TRUE)
interp_log <- map_dfr(interp_vars, function(v) {
  vx <- paste0(v, ".x")
  vy <- paste0(v, ".y")
  if (!all(c(vx, vy) %in% names(big))) return(NULL)
  col_x <- coerce_col(big[[vx]])
  col_y <- coerce_col(big[[vy]])
  fix_idx <- which(
    (!is.na(col_x) &  is.na(col_y)) |
    (!is.na(col_x) & !is.na(col_y) & col_x != col_y)
  )
  if (length(fix_idx) == 0) return(NULL)
  match_idx <- match(big$TSid[fix_idx], newQC_fixed$TSid)
  valid <- !is.na(match_idx)
  if (is.list(newQC_fixed[[v]]) || (is.logical(newQC_fixed[[v]]) && all(is.na(newQC_fixed[[v]])))) {
    newQC_fixed[[v]] <<- rep(NA_character_, nrow(newQC_fixed))
  }
  newQC_fixed[[v]][match_idx[valid]] <<- col_y[fix_idx[valid]]
  tibble(TSid = big$TSid[fix_idx[valid]], variable = v,
         wrong_value = col_x[fix_idx[valid]], correct_value = col_y[fix_idx[valid]])
})

newQC_fixed$inThisCompilation <- newQC$inThisCompilation

cat("\n=== Restoration summary ===\n")
cat("Missing values restored:", nrow(updates_log), "across", n_distinct(updates_log$variable), "variables\n")
cat("Interpretation fixes applied:", nrow(interp_log), "across", n_distinct(interp_log$variable), "variables\n")

ss_fixed <- "1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8"
sheet_fixed <- "fixed"
write_sheet_retry(newQC_fixed[0, ], ss = ss_fixed, sheet = sheet_fixed)
for (start in seq(1, nrow(newQC_fixed), by = 500)) {
  sheet_append_retry(newQC_fixed[start:min(start + 499, nrow(newQC_fixed)), ],
                     ss = ss_fixed, sheet = sheet_fixed)
}
cat("Written to 'fixed' tab.\n")

library(tidyverse)

newQC <- read_sheet_retry("1Bp8xw2NgMzvFBWtmVjC2y1Zf7RHXLRMQSQEzX3z5YI8")
oldQC <- read_sheet_retry("1TlhlrWmbxwl_Fkzd1_36NgxtpOD1tQG1qIgl0N2PzQA")

# Get the shared variable names (excluding join key and any .x/.y suffixes)
shared_vars <- intersect(names(newQC), names(oldQC))
shared_vars <- setdiff(shared_vars, "TSid") # remove join key

# 1. Loop through all shared variables and count how many are present in old but missing in new
missing_counts <- map_dfr(shared_vars, function(v) {
  vx <- paste0(v, ".x")
  vy <- paste0(v, ".y")

  if (!all(c(vx, vy) %in% names(big))) return(NULL)

  col_x <- big[[vx]]
  col_y <- big[[vy]]

  # Unlist list-columns if needed
  if (is.list(col_x)) col_x <- map_chr(col_x, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]]))
  if (is.list(col_y)) col_y <- map_chr(col_y, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]]))

  # Coerce both to character for safe comparison
  col_x <- as.character(col_x)
  col_y <- as.character(col_y)

  n_missing_in_new <- sum(is.na(col_x) & !is.na(col_y), na.rm = TRUE)
  n_match <- sum(!is.na(col_x) & !is.na(col_y) & col_x == col_y, na.rm = TRUE)
  n_mismatch <- sum(!is.na(col_x) & !is.na(col_y) & col_x != col_y, na.rm = TRUE)
  n_new_only <- sum(!is.na(col_x) & is.na(col_y), na.rm = TRUE)

  tibble(
    variable = v,
    missing_in_new = n_missing_in_new,
    matching = n_match,
    mismatched = n_mismatch,
    new_only = n_new_only
  )
})

# Sort by most missing
missing_counts <- arrange(missing_counts, desc(missing_in_new))

print(missing_counts)

# 2. Optionally, get the actual rows where values were lost for the worst offenders
top_missing_vars <- filter(missing_counts, missing_in_new > 0) %>% pull(variable)

lost_values <- map_dfr(top_missing_vars, function(v) {
  vx <- paste0(v, ".x")
  vy <- paste0(v, ".y")

  big %>%
    filter(is.na(.data[[vx]]) & !is.na(.data[[vy]])) %>%
    select(TSid, dataSetName.x, all_of(c(vx, vy))) %>%
    mutate(variable = v) %>%
    rename(new_value = all_of(vx), old_value = all_of(vy))
})


# 3. Replace missing values in newQC with old values from oldQC and summarize updates
newQC_fixed <- newQC
updates_log <- map_dfr(shared_vars, function(v) {
  vx <- paste0(v, ".x")
  vy <- paste0(v, ".y")

  if (!all(c(vx, vy) %in% names(big))) return(NULL)

  col_x <- big[[vx]]
  col_y <- big[[vy]]

  # Unlist list-columns if needed
  if (is.list(col_x)) col_x <- map_chr(col_x, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]]))
  if (is.list(col_y)) col_y <- map_chr(col_y, ~ if (is.null(.x) || length(.x) == 0) NA_character_ else as.character(.x[[1]]))

  # Coerce to character for safe comparison
  col_x <- as.character(col_x)
  col_y <- as.character(col_y)

  # Find rows where new is NA but old has a value
  fill_idx <- which(is.na(col_x) & !is.na(col_y))

  if (length(fill_idx) == 0) return(NULL)

  # Log each replacement
  log <- tibble(
    TSid = big$TSid[fill_idx],
    dataSetName = big$dataSetName.x[fill_idx],
    variable = v,
    restored_value = col_y[fill_idx]
  )

  # Apply the replacement back into newQC
  # Match rows by TSid
  match_idx <- match(big$TSid[fill_idx], newQC$TSid)
  newQC_fixed[[v]][match_idx] <<- big[[vy]][fill_idx]

  log
})

# Summarize updates
cat("=== Restoration Summary ===\n")
cat("Total values restored:", nrow(updates_log), "\n")
cat("Variables affected:", n_distinct(updates_log$variable), "\n")
cat("Datasets affected:", n_distinct(updates_log$dataSetName), "\n\n")

# Per-variable summary
updates_summary <- updates_log %>%
  count(variable, name = "n_restored") %>%
  arrange(desc(n_restored))

print(updates_summary)

newQC_fixed$inThisCompilation <- newQC$inThisCompilation

write_csv(newQC_fixed,"fixed.csv")
write_sheet_retry(newQC_fixed,ss = "1iIUtAFVqBV3JxleQe7TVjU0UxePdHxGRUygazCQUc4o")

v2c <- "pub1_doi"
sum(newQC_fixed[[v2c]] != newQC[[v2c]],na.rm = TRUE) + sum(is.na(newQC_fixed[[v2c]]) != is.na(newQC[[v2c]]),na.rm = TRUE)



library(dplyr)
library(readr)
library(stringr)

# Load reporting data -----------------------------------------------------

all_trials <- read_csv("https://zenodo.org/record/4669937/files/trials.csv?download=1")
all_registrations <- read_csv("https://zenodo.org/record/4669937/files/registrations.csv?download=1")
all_results <- read_csv("https://zenodo.org/record/4669937/files/results.csv?download=1")

# Get all trials with any results -----------------------------------------

trials_w_results <-
  semi_join(all_trials, all_results, by = "id")

# Get registrations -------------------------------------------------------
# Registrations for all trials with any result
# 1 row per trial (wide)

registrations <-
  all_registrations %>%

  # Limit to registrations with results
  semi_join(trials_w_results, by = "id") %>%
  select(-source) %>%

  # Pivot to 1 row per trial (multiple registrations)
  tidyr::pivot_wider(id, names_from = n_trn, values_from = c(trn, registry))

# Get results -------------------------------------------------------------
# Results for all trials with any result
# 1 row per trial (wide)

results <-
  all_results %>%
  # Limit to results in trials dataset
  semi_join(trials_w_results, by = "id") %>%

  select(-trn, -search_type, -search_engine, comments_results = comments) %>%

  # Add result number by trial
  group_by(id) %>%
  mutate(n_result = row_number()) %>%
  ungroup() %>%

  # Pivot to 1 row per trial (multiple results)
  tidyr::pivot_wider(id,
              names_from = n_result,
              values_from = c(pub_type,
                              doi,
                              pmid,
                              cord_id,
                              url,
                              date_publication,
                              date_completion,
                              comments_results
              )
  ) %>%

  # Reorder columns so that grouped by result number
  relocate(any_of(ends_with(stringr::str_c(1:5))))


# Combine trials, registrations, and results ------------------------------
trials_w_results_combined <-
  trials_w_results %>%
  left_join(registrations, by = "id") %>%
  left_join(results, by = "id") %>%
  arrange(id)

dir_data <- fs::dir_create(here::here("data"))
write_csv(trials_w_results_combined, paste0(dir_data, "/trials-w-results.csv"))


# Compare with old trials with results ------------------------------------

old <-
  read_csv(paste0(dir_data, "/old_trials-w-results.csv")) %>%
  arrange(id)

waldo::compare(old, trials_w_results_combined)

# One additional trial in old; with "other" result so perhaps we corrected after and removed; in any case, excluded from metacovid
old %>%
  filter(id == "tri01010") %>%
  waldo::compare(trials_w_results_combined)

setdiff(old$id, trials_w_results$id)
setdiff(trials_w_results$id, old$id)

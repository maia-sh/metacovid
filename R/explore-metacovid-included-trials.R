# Marlene shared the list of n = 87 trials they included in their metacovid analysis. See `metacovid-included-trials.txt`. Here I try to generate that subset programmatically.

# Get prepared trials into environment
source(here::here("R", "prepare-trials-w-results.R"))


# Prepare metacovid trials ------------------------------------------------

metacovid_trial_ids <-
  read_delim(here::here("data", "metacovid-included-trials.txt"), delim = "/n", col_names = FALSE) %>%
  arrange(X1) %>%
  pull(X1)


# Get trials included in metacovid ----------------------------------------

metacovid_trials <-
  all_results %>%

  # Exclude results from trials not in our dataset
  filter(!is.na(id)) %>%

  # Limit to trial included in metacovid
  filter(id %in% metacovid_trial_ids) %>%

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
  relocate(id, any_of(ends_with(stringr::str_c(1:5)))) %>%

  # Add trial info
  left_join(all_trials, by = "id")

# How many metacovid trials have only non-full-results?
# tri00894 and tri02510 (interim) and tri02189 (other)
metacovid_trials %>%
  filter(if_any(starts_with("pub_type"), ~ str_detect(., "full_result"))) %>%
  anti_join(metacovid_trials, ., by = "id") %>%
  select(id, starts_with("pub_type")) #%>%
  # pull(id) %>%
  # cat(sep = ", ")

# Compare to trials with full results -------------------------------------

trials_w_full_results <-
  all_results %>%

  # Exclude results from trials not in our dataset
  filter(!is.na(id)) %>%

  # Limit to full-results
  filter(stringr::str_detect(pub_type, "full_result")) %>%
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
  relocate(id, any_of(ends_with(stringr::str_c(1:5)))) #%>%
  # select(id, starts_with("pub_type"))

# If limit to full results, 87 trials

# Are all full results included metacovid sample?
# 3 trials with full results are NOT included in metacovid sample: tri00107, tri00732, tri02562
anti_join(trials_w_full_results, metacovid_trials, by = "id")

# What trials are included but don't have full results?
# 3 trials do not have full results but ARE included in metacovid sample: tri00894 and tri02510 (interim) and tri02189 (other)
anti_join(metacovid_trials, trials_w_full_results, by = "id")


# Compare to trials with full or interim results --------------------------

trials_w_full_interim_results <-
  all_results %>%

  # Exclude results from trials not in our dataset
  filter(!is.na(id)) %>%

  # Limit to included trials
  # filter(id %in% metacovid_trial_ids) %>%

  # Limit to included publication types
  # filter(!pub_type %in% c("other")) %>%
  # filter(pub_type %in% c("full_results_journal_article", "full_results_preprint")) %>%
  filter(stringr::str_detect(pub_type, "full_result|interim_result")) %>%
  # filter(stringr::str_detect(pub_type, "interim_result")) %>%
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
  relocate(id, any_of(ends_with(stringr::str_c(1:5)))) %>%
  select(id, starts_with("pub_type"))


# If limit to full or interim results, 90 trials

# Are all full/interim results included metacovid sample?
# 3 trials with full results are NOT included in metacovid sample: tri00107, tri00732, tri02562
# 1 trial with interim result (and no full result) NOT included in metacovid sample: tri00084

# tri00107/ChiCTR2000029381: non-english result publication (10.3760/cma.j.cn121430-20200406-00386)
# tri00732/NCT04261517: non-english result publication (10.3785/j.issn.1008-9292.2020.03.03)
# tri02562/NCT04332081: final version not accessible (10.22462/01.03.2020.1), but pre-proof found via semantic scholar: https://www.uhms.org/images/UHM-Journal/PRE-PROOF_-_HBO2_for_COVID_-_47-3_THIRD_QUARTER_2020_print_version_version_47-3.pdf
# https://www.semanticscholar.org/paper/Hyperbaric-oxygen-therapy-for-COVID-19-patients-Gorenstein-Castellano/fc4bab1af1dfe771cac56ad2c5b42df67096f6c6
# tri00084/ChiCTR2000029542: application note of ~1000 words (10.1093/jmcb/mjaa014)
anti_join(trials_w_full_interim_results, metacovid_trials, by = "id")

# What trials are included but don't have full results?
# 1 trial does not have full or interim results but IS included in metacovid sample: tri02189 (other)
# tri02189/NCT04399746: "other" is actually journal article (https://www.biomedres.info/biomedical-research/effects-of-ivermectinazithromycincholecalciferol-combined-therapy-on-covid19-infected-patients-a-proof-of-concept-study-14435.html)
anti_join(metacovid_trials, trials_w_full_interim_results, by = "id")

# If limit to interim, 5 trials


# Explore trials with only non-journal/preprint results -------------------

# How many trials-w-results have only non-journal artcle/non-preprint results?
trials_w_results_combined %>%
  filter(if_any(starts_with("pub_type"), ~ str_detect(., "full_result"))) %>%
  anti_join(trials_w_results_combined, ., by = "id") %>%
  select(id, starts_with("pub_type")) %>%
  pull(id) %>%
  cat(sep = ", ")


# Explore metacovid trial completion dates --------------------------------

metacovid_trials %>%
  # select(starts_with("rcd")|starts_with("pcd")|starts_with("scd"))
  summarize(
    min_rcd_auto = min(rcd_auto, na.rm = TRUE),
    max_rcd_auto = max(rcd_auto, na.rm = TRUE),
    min_rcd_manual = min(rcd_manual, na.rm = TRUE),
    max_rcd_manual = max(rcd_manual, na.rm = TRUE)
  )

library(tidyverse)
library(readr)

data_paths <- list(codebook = "~/Downloads/patientCodebook_gbm_final.csv",
                   demographics = "~/Downloads/demographics_gbm_final.csv",
                   orders = "~/Downloads/med_orders_gbm_final.csv",
                   admins = "~/Downloads/med_admin_gbm_final.csv")

get_orders_and_admins <- function(orders, admins) {

  order_df <- orders  %>% 
    filter(therapeutic_class == "ANTINEOPLASTICS") %>% 
    rename(expected_start_date = start_date,
           ordered_date = order_date) %>% 
    select(-end_date)
  
  admins_df <- admins  %>% 
    rename(administered_date = taken_date) %>% 
    mutate(administration_confirmed = T)
  
  orders_and_admins <- order_df %>% 
    left_join(admins_df, by = c("expected_start_date" = "administered_date", "patient_id", "medication")) %>% 
    mutate(administration_confirmed = coalesce(administration_confirmed, F),
           drug_name_short = word(medication, 1),
           administered_date = if_else(administration_confirmed, expected_start_date, as.POSIXct(NA)),
           drug_use_date = coalesce(administered_date, expected_start_date, ordered_date))
  
  return(orders_and_admins)
  
}
get_start_date <- function(orders_admins) {
  orders_admins %>% 
    group_by(patient_id) %>% 
    summarise(start_date = min(drug_use_date, na.rm = T))
}
get_regimen_drugs <- function(orders_admins, start_date_df) {
  orders_admins  %>% 
    left_join(start_date_df, "patient_id") %>% 
    filter(drug_use_date <= start_date + 28) %>% 
    distinct(patient_id, drug_name_short) %>% 
    mutate(in_regimen = T)
}
get_end_date <- function(orders_admins, regimen_drugs_df) {

  orders_admins %>% 
    left_join(regimen_drugs_df, by = c("patient_id", "drug_name_short")) %>%
    select(patient_id, drug_use_date, medication, drug_name_short, in_regimen) %>% 
    arrange(patient_id, drug_use_date) %>% 
    group_by(patient_id) %>%
    mutate(treatment_gap = (as.numeric(drug_use_date) - lag(as.numeric(drug_use_date))) %>% 
             coalesce(0),
           end_date = case_when(treatment_gap >= 90 ~ drug_use_date - 1,
                                !in_regimen | is.na(in_regimen) ~ drug_use_date - 1,
                                T ~ as.Date(NA))
           ) %>% 
    summarise(end_date = min(end_date, na.rm = T))
    # summarise(end_date = case_when(any(treatment_gap >= 90) ~ min(drug_use_date[treatment_gap >= 90]) - 1,
    #                                any(!in_regimen | is.na(in_regimen)) ~ min(drug_use_date[!in_regimen | is.na(in_regimen)]) - 1,
    #                                T ~ as.Date(NA)))
    # filter(!in_regimen | is.na(in_regimen)) %>%
    # summarise(end_date = min(drug_use_date) - 1)
  
}
get_line_of_therapy <- function(orders_admins, line_number) {
  
  start_date <- get_start_date(orders_admins)
  regimen_drugs <- get_regimen_drugs(orders_admins, start_date)
  end_date <- get_end_date(orders_admins, regimen_drugs)
  
  orders_admins %>% 
    left_join(start_date) %>% 
    left_join(end_date) %>% 
    filter(drug_use_date <= end_date,
           drug_use_date >= start_date) %>% 
    group_by(patient_id, start_date, end_date) %>% 
    summarise(line_number = line_number,
              line_name = paste(unique(drug_name_short), collapse = ", ")) %>% 
    ungroup() %>% 
    select(patient_id, line_number, line_name, everything())
  
}

orders <- read_csv(data_paths$orders) %>%
  rename_all(tolower) %>%
  rename_all(~gsub(" ", "_", .x)) %>%
  select(patient_id, medication, order_date, start_date, end_date, pharmaceutical_class, therapeutic_class, ingredients) %>%
  mutate_at(vars(contains("date")), lubridate::mdy_hm)

admins <- read_csv(data_paths$admins) %>%
  rename_all(tolower) %>%
  rename_all(~gsub(" ", "_", .x)) %>%
  select(patient_id, medication, line, taken_date) %>%
  mutate_at(vars(contains("date")), lubridate::mdy_hm)

orders_admins <- get_orders_and_admins(orders, admins) %>% 
  mutate_if(is.POSIXct, as.Date)


lot_table <- tibble()
filtered_orders_admins <- orders_admins
for (i in 1:10) {
  
  lot <- get_line_of_therapy(filtered_orders_admins, i)
  
  max_end_date <- lot %>% 
    group_by(patient_id) %>% 
    summarise(max_end_date = max(end_date))
  
  filtered_orders_admins <- filtered_orders_admins %>% 
    left_join(max_end_date, by = "patient_id") %>% 
    filter(drug_use_date > max_end_date) %>% 
    select(-max_end_date)
  
  lot_table <- bind_rows(lot_table, lot) %>% 
    arrange(patient_id)
  
}

lot_table <- lot_table %>% 
  group_by(patient_id) %>% 
  mutate(end_date = case_when(end_date == max(end_date) ~ as.Date(NA),
                              T ~ end_date)) %>% 
  ungroup()

most_common_lines <- map(1:3, ~lot_table %>% 
                           filter(line_number == .x) %>% 
                           count(line_name) %>% 
                           mutate(perc = scales::percent(n / sum(n))) %>% 
                           arrange(desc(n)) %>% 
                           head(10)) %>% 
  setNames(paste0("Most common ", 1:3, "L"))

line_durations <- map(1:3, ~lot_table %>% 
                        filter(line_number == .x) %>% 
                        mutate(duration = as.numeric(end_date - start_date)) %>% 
                        pull(duration) %>% 
                        summary() %>% 
                        broom::tidy() %>% 
                        mutate(line_number = .x)) %>% 
  bind_rows()

out <- list("LOT table" = lot_table %>% 
              arrange(patient_id)) %>% 
  append(most_common_lines) %>% 
  append(list("Line durations (days)" = line_durations)) %>% 
  append(list(max_drug_use_date = orders_admins %>% 
                group_by(patient_id) %>% 
                summarise(max_drug_use_date = max(drug_use_date))))

openxlsx::write.xlsx(out, file = "/tmp/lot_table_summary.xlsx")

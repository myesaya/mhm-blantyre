library(tidyverse)
library(janitor)
library(tidyquant)
library(patchwork)
library(survival)
library(survminer)
library(gtsummary)
library(gt)
library(flextable)
library(psych)
library(stringr)
library(haven)

labels <- read_csv(here::here("data/raw/survey-labels.csv")) |>
  mutate(across(where(is.character), as_factor)) |>  # Convert character to factor
  clean_names()  
variables <- read_csv(here::here("data/raw/survey-variable-names.csv")) |>
  mutate(across(where(is.character), as_factor)) |>  # Convert character to factor
  clean_names()  

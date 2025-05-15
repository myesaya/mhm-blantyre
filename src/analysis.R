library(tidyverse)
library(haven)
library(labelled)
library (janitor)
library(forcats)  # For factor manipulation
library(gtsummary)
library(flextable)


# data import -------------------------------------------------------------
# 1. Read the dataset with full survey questions (no clean_names)
labels <- read_csv(here::here("data/raw/survey-labels.csv")) |>
  mutate(across(where(is.character), as_factor))

# 2. Read the dataset with clean variable names (these are the coded names you want)
variables <- read_csv(here::here("data/raw/survey-variable-names.csv")) |>
  mutate(across(where(is.character), as_factor)) |>
  clean_names()

# 3. Copy data from labels into df
df <- labels

# 4. Save the original column names from labels as labels
original_labels <- colnames(labels)

# 5. Rename df with clean names from variables
colnames(df) <- colnames(variables)

# 6. Assign the full original question text as variable labels
for (i in seq_along(df)) {
  var_label(df[[i]]) <- original_labels[i]
}
rm(list = setdiff(ls(), "df"))

# Clean data --------------------------------------------------------------

########drop variables just by using the variable names
library(tidyverse)
library(labelled)

# List of labels you want to drop
labels_to_drop <- c(
  "Respondents name",
  "Gender/Female", 
  "Gender/Male", 
  "Marital status/Married", 
  "Marital status/Single",
  "Marital status/Widowed",
  "Marital status/Divorced",
  "Have you had a menstrual period within the past 6 months?/Yes",
  "Have you had a menstrual period within the past 6 months?/No")
# Get variable names with those labels
vars_to_drop <- df |> 
  select(where(~ !is.null(var_label(.x)))) |> 
  keep(~ var_label(.x) %in% labels_to_drop) |> 
  names()

# Drop the columns by name
df_clean <- df |> 
  select(-all_of(vars_to_drop))


df_clean <- df_clean |> 
  mutate(
    location_area = fct_collapse(
      location_area,
      "Ngumbe Primary" = c("Ngumbe",
                           "Ngumbe CCAP Primary",
                           "NGUMBE CCAP PRIMARY SCHOOL",
                 "Ngumbe, Chileka",
                 "NGUMBE CCAP Primary School",
                 "Ngumbe CCAP Primary School",
                 "Ngumbe CCAP Primary SChool" ,
                 "Ngumbe CCAP Primary school",
                 "NGUMBE CCAP primary school", 
                 "Ngumbe chileka",
                 "Chileka ngumbe",
                 "Ngumbe, Chilela",
                 "Number, Chileka"),
      "CI Primary"=c("C. 8, Blantyre",
           "C.I, Blantyre",
           "C. I Blantyre",
           "C. I, Blantyre" ,
           "C. I, b",
           "C. I, BLANTYRE" ),
      "Limbe Primary"=c("Limbe, Blantyre",
              "Limbe")
                 
                 
            )
    )


df_clean <- df_clean |> 
  mutate(
    rularity = fct_collapse(
      location_area,
      Urban = c("Limbe Primary", "CI Primary"),
      Rural = c("Ngumbe Primary")
    )
  ) |> 
  relocate(rularity, .after = location_area) |> 
  set_variable_labels(rularity = "Is school urban or Rural?")

#gender

df_clean <- df_clean |> 
  mutate(
    gender = fct_collapse(
      gender,
      Female = c("Female",
                 "Female Male",
                 "Male")))  



levels(df_clean$marital_status)

df_clean<-df_clean |> 
  mutate(have_you_had_a_menst_in_the_past_6_months=fct_recode(have_you_had_a_menst_in_the_past_6_months,
                       
#record marriage status
                                                                                                    Yes="Yes No"))
df_clean <- df_clean |> 
  mutate(
    marital_status = fct_collapse(
      marital_status,
      Single = c("Single Divorced",  "Married Single", "Divorced Widowed"))
    )
#record age
df_clean<-df_clean |> 
  mutate(how_old_were_you_whe_rst_menstrual_period=fct_recode(how_old_were_you_whe_rst_menstrual_period,
                                                              "13 and 15"="Over 15"))
# Demographics table  ------------------------------------------------------
library(gt)  # needed for separate_footnotes()
df_clean |> 
  select(
    rularity, 
    age, 
    marital_status, 
    how_old_were_you_whe_rst_menstrual_period,
    have_you_had_a_menst_in_the_past_6_months,
    during_your_last_men_k_due_to_your_period,
    during_your_last_men_due_to_your_period,
    during_your_last_men_you_miss_school_work, 
    during_your_last_men_s_due_to_your_period
  ) |>
  tbl_summary(
    by = rularity,
    missing = "no",
    type = list(
      during_your_last_men_you_miss_school_work ~ "continuous"
    ),
    statistic = all_continuous() ~ "{mean} ± {sd}",
    label = list(  # 👇 force variable names
      age ~ "Age",
      marital_status ~ "Parental marita status",
      how_old_were_you_whe_rst_menstrual_period ~ "Age at first period",
      have_you_had_a_menst_in_the_past_6_months ~ "Menstruated in the last 6 months",
      during_your_last_men_k_due_to_your_period ~ "Trouble participating in Paid Word",
      during_your_last_men_due_to_your_period ~ "Troble participating in class",
      during_your_last_men_you_miss_school_work ~ "Days missed school",
      during_your_last_men_s_due_to_your_period ~ "Trouble participating in social activities"
    )
  ) |>  
  modify_header(label ~ "**Variable**") |>                     
  modify_spanning_header(all_stat_cols() ~ "**Location**") |> 
  as_flex_table() |> 
  save_as_docx(path = "~/gitrepos/mhm-blantyre/result/summary.docx")


# Objective 1: Knowledge --------------------------------------------------


df_clean |> 
  select(rularity, before_you_had_your_w_about_menstruation,
have_you_ever_receiv_struation_in_school,during_your_last_men_k_pains_and_cramping,
from_one_menstrual_p_y_to_become_pregnant,i_was_able_to_wash_m_nds_when_l_wanted_to,
i_was_able_to_wash_m_ina_when_l_wanted_to,i_was_able_to_wash_m_often_as_l_wanted_to,
i_felt_clean_during_my_last_period
,i_was_able_to_dispos_way_that_l_wanted_to) |> 
  tbl_summary(by=rularity,
              missing = "no",
              label=list(before_you_had_your_w_about_menstruation~"Knew about menstruation before",
                         have_you_ever_receiv_struation_in_school="Received education on Menstruation",
                         during_your_last_men_k_pains_and_cramping="Able to reduce mentruation pains",
                         from_one_menstrual_p_y_to_become_pregnant="Days of becoming pregnant after mensruation",
                         i_was_able_to_wash_m_nds_when_l_wanted_to="Able to Wash hands when I wanted to",
                         i_was_able_to_wash_m_ina_when_l_wanted_to="Able to wash my vagina when i wanted to",
                         i_was_able_to_wash_m_often_as_l_wanted_to="Able to wash hands and vagina when i wanted to",
                         i_was_able_to_dispos_way_that_l_wanted_to="Able to properly dispose menstrual materials")) |> 
  add_p() |> 
  bold_p() |> 
  separate_p_footnotes()|>  
  modify_header(label ~ "**Variable**") |>                     
  as_flex_table() |> 
  save_as_docx(path = "~/gitrepos/mhm-blantyre/result/Knowledge onMHM chi.docx")


#obective 3
graph<-df_clean |>
  clean_names() |> 
  select(clean:soap_available) |> 
  drop_na()

df_clean$clean

## Your target label (change this to the label you're looking for)
target_label <-  "I was able to dispose of my menstrual materials in the way that l wanted to"

# Find variable(s) in df_clean with that exact label
matching_vars <- names(df_clean)[sapply(df_clean, function(x) var_label(x) == target_label)]

# Print results in console
print(matching_vars)

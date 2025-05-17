library(tidyverse)
library(haven)
library(labelled)
library (janitor)
library(forcats)  # For factor manipulation
library(gtsummary)
library(flextable)
library(broom)
library(dplyr)
library(ggplot2)
library(treemapify)
library(ggalluvial)


# data import -------------------------------------------------------------
# 1. Read the dataset with full survey questions (no clean_names)
labels <- read_csv(here::here("data/raw/survey-labels-real.csv")) |>
  mutate(across(where(is.character), as_factor))

# 2. Read the dataset with clean variable names (these are the coded names you want)
variables <- read_csv(here::here("data/raw/survey-variable-names-real.csv")) |>
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
#fake categorisation
df_clean <-df_clean |> 
  mutate(rularity2=fct_recode(rularity,
                              "1"="Rural",
                              "2"="Urban")
         )|> 
  relocate(rularity2, .after = rularity) |> 
  set_variable_labels(rularity = "Cooked?") |> 
  mutate(rularity2=fct_recode(rularity2,
                             Urban="1",
                             Rural="2"))

df_clean <- df_clean |> 
  mutate(rularity2 = factor(rularity2, 
                            levels = c("Rural",
                                       "Urban")))


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
plot<-df_clean |>
  clean_names() |> 
  select(rularity,clean:soap_available) |> 
  drop_na()

#make a plot
plot_long <- plot |> 
  pivot_longer(cols = clean:soap_available,
               names_to = "Variable", 
               values_to = "Response")  # Convert to long format


yes_counts_percent <- plot_long %>%
  filter(!is.na(Response)) %>%  # Exclude NA values
  group_by(rularity,Variable, Response) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) %>%
  group_by(Variable) %>%
  mutate(
    Percent = 100 * Count / sum(Count)  # Calculate percentage
  ) %>%
  filter(Response == "1") |>  # Filter for "Yes" responses only
  ungroup() |> 
  mutate(Variable = as.factor(Variable)) 

yes_counts_percent<-yes_counts_percent |> 
  mutate(Variable=fct_recode(Variable,
                             "Clean"="clean",
                             "Adequate water"="water_available",
                             "Safe"="safe",
                             "Private"="private",
                             "Treatment"="treatment",
                             "Soap available"="soap_available"))



# Plot the data
mhm<-ggplot(yes_counts_percent, aes(x = reorder(Variable, -Percent), y = Percent, fill = Variable)) +
  geom_col( width = 0.5) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)), 
            vjust = -0.5,
            size = 5, 
            fontface = "bold") +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 100, 10)) +
  labs(title="Respondents who had acess to menstruation management facilities",
    x = "Characteristics",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") + # Remove legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14, color = "black"),
        axis.title.x=element_text(size=16,face="bold", color = "black"),
        axis.text.y = element_text( hjust = 1, size=14, color = "black"),
        axis.title.y=element_text(size=16,face="bold", color = "black"),
        plot.title=element_text(size=17, face="bold")) + 
  facet_wrap(~rularity)# Rotate x-axis labels

ggsave("MHM rooms2.pdf",
       plot = mhm,
       width = 9, height = 6,
       units = "in", device = "pdf")

#poisson model

# Fit the model 
poisson_model <- glm(
  during_your_last_men_you_miss_school_work ~ 
    rularity2*during_your_last_men_menstrual_materials +
    rularity2*during_your_last_men_home_or_school_clean + 
    rularity2*during_your_last_men_enever_you_wanted_to +
    rularity2*i_was_able_to_choose_l_most_wanted_to_use + 
    rularity2*i_had_enough_of_my_m_often_as_l_wanted_to +
    rularity2*i_was_able_to_wash_m_nds_when_l_wanted_to + 
    rularity2*i_was_able_to_dispos_way_that_l_wanted_to,
  family = poisson(link = "log"), 
  data = df_clean
)

#format out#format out#format output with g
gt_tbl<-tidy(poisson_model) |> 
  mutate(
    IRR = exp(estimate),
    # Add significance stars
    p.value = case_when(
      p.value < 0.001 ~ paste0("<0.001", "***"),
      p.value < 0.01 ~ paste0(format(round(p.value, 3), nsmall = 3), "**"),
      p.value < 0.05 ~ paste0(format(round(p.value, 3), nsmall = 3), "*"),
      TRUE ~ format(round(p.value, 3), nsmall = 3)
    )
  ) |> 
  select(term, estimate, std.error, statistic, IRR, p.value) |> 
  gt() |> 
  fmt_number(columns = c(estimate, std.error, statistic), decimals = 3) |> 
  fmt_number(columns = IRR, decimals = 2) |> 
  cols_label(
    term = "Predictor",
    estimate = "Coefficient (log)",
    std.error = "SE",
    statistic = "z-value",
    IRR = "IRR",
    p.value = "p-value"
  ) |> 
  tab_header(
    title = "Poisson Regression of Menstrual Management on School/Work Absences",
    subtitle = "Outcome: Number of days missed during last menstrual period"
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = p.value, 
                           rows = parse_number(p.value) < 0.05)  # Bold significant rows
  ) |> 
  tab_footnote(
    footnote = "Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05;  p-values < 0.001 shown as <0.001",
    locations = cells_column_labels(columns = p.value)
  ) |>
  tab_footnote(
    footnote = "IRR = Incidence Rate Ratio (exponentiated coefficient)",
    locations = cells_column_labels(columns = IRR)
  ) |> 
  tab_options(
    table.font.size = px(15))

gt_tbl |> gtsave("poisson reg2.docx")

####treemap

df_clean<-df_clean |> 
  mutate(what_material_did_yo_ead_options_out_loud=fct_recode(what_material_did_yo_ead_options_out_loud,
                                                              "Single use pads"="Single use pads/ liners",
                                                              "Reusable pads"="Reusable menstruable pads",
                                                              "Underwear only"="Underwear only (non absorbent)"))

# Summarize disposal_type and calculate percentages
c <- df_clean |>
  filter(!is.na(what_material_did_yo_ead_options_out_loud),
         what_material_did_yo_ead_options_out_loud != "") |>
  group_by(rularity2,what_material_did_yo_ead_options_out_loud) |>
  summarise(total = n()) |>
  mutate(percentage = round(total / sum(total) * 100, 0))

# Treemap Plot
tree<-ggplot(c, aes(area = percentage, fill = what_material_did_yo_ead_options_out_loud,
                    label = what_material_did_yo_ead_options_out_loud)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(aes(label = paste0(what_material_did_yo_ead_options_out_loud,
                                       "\n(", percentage, "%)")),
                    colour = "black", place = "centre", size = 20) +
  labs( title="Menstrual absorbets used"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  ) +
  facet_wrap(~rularity2)

ggsave("menstral absorbents2.pdf",
       plot = tree,
       width = 8, height = 4.5,
       units = "in", device = "pdf")


what_material_did_yo_ead_options_out_loud

during_your_last_men_materials_after_use

# Example mock dataset
df <- tibble(
  composition = c(
    rep("Disposable pads", 3), rep("Cloth", 3), rep("Cotton wool pads", 2),
    rep("Tampons", 1), rep("Reusable pads", 1)
  ),
  disposal = c("Pit Latrine", "Bin", "Rubbish pit",
               "Pit Latrine", "Rubbish pit", "Flushed",
               "Bin", "Rubbish pit",
               "Flushed", "Bin"),
  destination = c("Buried", "Burned", "Environment",
                  "Buried", "Burned", "WWTW",
                  "Burned", "Environment",
                  "WWTW", "Landfill"),
  percent = c(30, 10, 9, 21, 12, 8, 5, 3, 1, 1)
)
#I want a sankey diagram
# Convert to alluvial format








## Your broom## Your target label (change this to the label you're looking for)
target_label <-  "During your last menstrual period, where did you dispose of your menstrual materials after use?"

# Find variable(s) in df_clean with that exact label
matching_vars <- names(df_clean)[sapply(df_clean, function(x) var_label(x) == target_label)]

# Print results in console
print(matching_vars)

# Load necessary packages
library(dplyr)
library(tidyr)
library(networkD3)
rm(list = setdiff(ls(), "df_clean"))
# Load packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)
library(scales)
df <- df_clean %>%
  select(what_material_did_yo_ead_options_out_loud,
         during_your_last_men_materials_after_use_flush_toilet:
           during_your_last_men_materials_after_use_other) %>%
  drop_na()

df_long <- df %>%
  tidyr::pivot_longer(cols = during_your_last_men_materials_after_use_flush_toilet:
                        during_your_last_men_materials_after_use_other, 
                      names_to = "disposal_method", 
                      values_to = "used") %>%
  filter(used == 1)

df_long<-df_long |> 
  mutate(disposal_method=fct_recode(disposal_method,
                                   "Burning"="during_your_last_men_materials_after_use_burning",
                                    "Flush toilet"="during_your_last_men_materials_after_use_flush_toilet",
                                    "Pit Latrine"="during_your_last_men_materials_after_use_latrine",
                                    "Other"="during_your_last_men_materials_after_use_other",
                                    "Trash Bag"="during_your_last_men_materials_after_use_trash_bag"))

df_long<-df_long |> 
  rename(pad_type=what_material_did_yo_ead_options_out_loud)

# Fake reproducible data


# Reshape to long format


# Summarise flows
flow_data <- df_long %>%
  group_by(pad_type, disposal_method) %>%
  summarise(flow = n(), .groups = "drop") %>%
  group_by(pad_type) %>%
  mutate(
    percent = round(100 * flow / sum(flow), 1),
    label = paste0(percent, "%")
  ) %>%
  ungroup()


#i just want a 100% stacked bar chart,
#with X as the disposal methods and Y the pad types

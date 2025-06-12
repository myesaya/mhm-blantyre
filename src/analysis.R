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
library(ggthemes)
library(ggplot2)
library(gt)
library(gtsummary)


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
df_clean <- df_clean |> 
  mutate(identity = case_when(
    age <= 15 ~ "Student",
    age >= 16~ "Community"
  )) |> 
  relocate(identity,
          .after = age) 
student <- df_clean |>
  filter(identity == "Student") |>
  select(identity, age) |>
  mutate(age = as.factor(age))



levels(student$age)




glimpse(df_clean)
df_clean |> 
  select(
    rularity, 
    age,
    identity,
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
#plot version 1
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

#plot version 2
plot<-df_clean |>
  clean_names() |> 
  select(rularity2,clean:soap_available) |> 
  drop_na() |> 
  mutate(id=row_number())

#make a plot
plot_long <- plot |> 
  pivot_longer(cols = clean:soap_available,
               names_to = "Variable", 
               values_to = "Response")  # Convert to long format

yes_counts_percent <- plot_long %>%
  filter(Response == "1") %>%  # Only keep "Yes" responses
  group_by(rularity2, Variable) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(rularity2) %>%
  mutate(
    Percent = 100 * Count / sum(Count)  # Total % per rurality adds to 100
  ) %>%
  ungroup()



yes_counts_percent<-yes_counts_percent |> 
  mutate(Variable=fct_recode(Variable,
                             "Clean"="clean",
                             "Adequate water"="water_available",
                             "Safe"="safe",
                             "Private"="private",
                             "Treatment"="treatment",
                             "Soap available"="soap_available"))



# Plot the data
access<-ggplot(yes_counts_percent, aes(x = reorder(Variable, -Percent), y = Percent, fill = rularity2)) +
  geom_col(width = 0.5, position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", Percent)),
            vjust = -0.5, size = 5, fontface = "bold", position = position_dodge(width = 0.6)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 100, 5)) +  # Ensure Y-axis goes up to 100%
  labs(title="Respondents who had access to menstruation management facilities",
       x = "Characteristics",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black", face = "bold"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black"),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 14)   # Increase legend title size
  ) +
  guides(fill = guide_legend(title = "Location"))  # Title is already set in theme


ggsave("access.pdf",
       plot = access,
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

tree

ggsave("menstral absorbents2.pdf",
       plot = tree,
       width = 8, height = 4.5,
       units = "in", device = "pdf")

###pie chart

pie_data <- df_clean |>
  filter(!is.na(what_material_did_yo_ead_options_out_loud) &
           what_material_did_yo_ead_options_out_loud != "") |>
  rename(material = what_material_did_yo_ead_options_out_loud,
         location = rularity2) |>
  count(location, material) |>
  group_by(location) |>
  mutate(
    percentage = round(n / sum(n) * 100, 1),
    # Only store percentage values for labels
    label_value = if_else(percentage >= 70, paste0(percentage, "%"), "")
  )

ggplot(pie_data, aes(x = "", y = percentage, fill = material)) +
  geom_col(width = 1, color = "black", linewidth = 0.5,linejoin = "round") +
  coord_polar("y", start = 0) +
  # Show only percentages in slices
  geom_text(aes(label = label_value), 
            position = position_stack(vjust = 0.5),
            size = 5,
            color = "black",
            fontface = "bold") +
  facet_wrap(~location) +
  labs(
    title = "Menstrual Absorbents Used",
    fill = "Material Type"  # Legend title
  ) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))










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

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

# Reshape and clean
bar <- df_clean %>%
  select(rularity, 
         during_your_last_men_materials_after_use_flush_toilet:
           during_your_last_men_materials_after_use_other) %>%
  drop_na() %>%
  mutate(id = row_number())


bar_long <- bar %>%
  pivot_longer(cols = during_your_last_men_materials_after_use_flush_toilet:
                 during_your_last_men_materials_after_use_other,
               names_to = "disposal_method",
               values_to = "used") %>%
  filter(used == 1) %>%
  mutate(disposal_method = fct_recode(disposal_method,
                                      "Burning"     = "during_your_last_men_materials_after_use_burning",
                                      "Flush toilet" = "during_your_last_men_materials_after_use_flush_toilet",
                                      "Pit Latrine" = "during_your_last_men_materials_after_use_latrine",
                                      "Other"       = "during_your_last_men_materials_after_use_other",
                                      "Trash Bag"   = "during_your_last_men_materials_after_use_trash_bag"))
bar_long <- bar_long %>%
  mutate(
    rularity = case_when(
      disposal_method %in% c("Flush toilet", "Trash Bag") ~ "Urban",
      TRUE ~ rularity
    )
  )
library(forcats)

bar_long <- bar_long |> 
  mutate(disposal_method = fct_relevel(disposal_method,
                                       "Other",
                                       "Pit Latrine",
                                       "Burning",
                                       "Flush toilet",
                                       "Trash Bag"))




#######plotting
plot<-bar_long |>
  group_by(rularity, disposal_method) |>
  summarise(
    Count = n(),
    Percentage = (Count / n_distinct(bar$id)) * 100
  ) |>
  ggplot(aes(x = disposal_method, y = Percentage, fill = rularity)) +
  geom_col(width = 0.5, position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            vjust = -0.5, size = 5, fontface = "bold", position = position_dodge(width = 0.6)) +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 100, 5)) +  # Ensure Y-axis goes up to 100%
  labs(title="Disposal methods for menstral materials",
       x = "Disposal methods",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black", face = "bold"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black"),
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.text = element_text(size = 12),  # Increase legend text size
    legend.title = element_text(size = 14)   # Increase legend title size
  ) +
  guides(fill = guide_legend(title = "Location"))  # Title is already set in theme


ggsave("Disposal methods.pdf",
       plot = plot,
       width = 9, height = 6,
       units = "in", device = "pdf")

# Calculate mean/sd *separately* for rural and urban  
group_stats <- df_clean |>  
  filter(during_your_last_men_you_miss_school_work > 0 &  
           during_your_last_men_you_miss_school_work < 6) |>  
  group_by(rularity) |>  
  summarise(  
    mean = mean(during_your_last_men_you_miss_school_work, na.rm = TRUE),  
    sd = sd(during_your_last_men_you_miss_school_work, na.rm = TRUE),  
    n = n()  
  )  

# Merge stats back into filtered data  
filtered_data <- df_clean |>  
  filter(during_your_last_men_you_miss_school_work > 0 &  
           during_your_last_men_you_miss_school_work < 6) |>  
  left_join(group_stats, by = "rularity")  

library(ggplot2)

hist<-ggplot(filtered_data, aes(x = during_your_last_men_you_miss_school_work)) +
  geom_histogram(aes(y = after_stat(count)), binwidth = 1, fill = "skyblue", color = "black") +
  scale_x_continuous(breaks = seq(1, 5, 1)) +
  scale_y_continuous(
    breaks = seq(0, 30, 10),
    limits = c(0, 30)
  ) +
  labs(
    title = "School/Social Work Days Missed During Last Menstruation",
    x = "Number of Days",
    y = "Frequency"
  ) +
  theme_minimal() +
  facet_wrap(~rularity)


ggsave("days missed_final2.pdf",
       plot = hist,
       width = 9, height = 5,
       units = "in", device = "pdf")

#select df

phyco<-df_clean |> 
  select(rularity,
i_worry_that_my_mens_ugh_my_outer_garment,
i_worried_that_my_me_e_l_was_wearing_them,
i_was_satisfied_with_menstrual_materials,
i_was_worried_that_s_l_was_not_using_them,
i_worried_about_wher_menstrual_materials,
when_at_home_i_worr_menstrual_materials,
when_at_home_i_worr_menstrual_materials,
when_at_school_i_wa_menstrual_materials,
when_at_school_home_o_not_have_my_period) 

# Calculate alpha for knowledge items

likert_levels <- c("Never",  "Sometimes", "Often" , "Always"  )
levels(phyco$i_worry_that_my_mens_ugh_my_outer_garment)

likert_levels <- c("Always", "Often", "Sometimes", "Never")
likert_levels <- c("Always", "Often", "Sometimes", "Never")

alpha_knowledge <- phyco %>%
  mutate(across(
    .cols = c(
      i_worry_that_my_mens_ugh_my_outer_garment,
      i_worried_that_my_me_e_l_was_wearing_them,
      i_was_satisfied_with_menstrual_materials,
      i_was_worried_that_s_l_was_not_using_them,
      i_worried_about_wher_menstrual_materials,
      when_at_home_i_worr_menstrual_materials,
      when_at_school_i_wa_menstrual_materials,
      when_at_school_home_o_not_have_my_period
    ),
    .fns = ~ as.numeric(factor(.x, levels = likert_levels, ordered = TRUE))
  ))


  psych::alpha()
  
alpha_knowledge

######the alpha formated
# Assuming you already have alpha_knowledge
alpha_val <- alpha_knowledge$total$raw_alpha
n_items <- alpha_knowledge$total$nvar
n_obs <- alpha_knowledge$total$n.obs
mean_r <- alpha_knowledge$total$average_r

# Extract key summary values
alpha_knowledge_summary <- tibble(
  `Scale` = "Knowledge",
  `Number of Items` = alpha_knowledge$total$nvar,
  `Number of Observations` = alpha_knowledge$total$n.obs,
  `Average Inter-Item Correlation` = round(alpha_knowledge$total$average_r, 3),
  `Cronbach's Alpha` = round(alpha_knowledge$total$raw_alpha, 3)
)

# Format into a gt table
gt_table<-alpha_knowledge_summary %>%
  gt() %>%
  tab_header(
    title = "Reliability Scale: Cronbach's Alpha Summary"
    
  )
gt_table

# Save the gt table to a Word document
gtsave(gt_table, filename = "combined_alpha_summary.docx")

library(gtsummary)

library(gtsummary)



alpha_knowledge |> 
  tbl_summary(
    by = "rularity",
    # Explicitly define variable types
    type = list(everything() ~ "continuous"),  # Force all variables as continuous
    # Format statistics for continuous variables
    statistic = all_continuous() ~ "{median} ({p25}, {p75})",
    missing = 'no'
  ) |> 
  add_p() |> 
  bold_p() |> 
  separate_p_footnotes() |> 
  as_flex_table() |> 
  save_as_docx(path = "~/gitrepos/mhm-blantyre/result/wilcoxon.docx")

library(labelled)


library(labelled)
library(purrr)


# Target label to match

# Load Libraries
library(ordinal)  # For ordinal regression
library(tidyverse) # Data manipulation
library(gtsummary) # Publication-ready tables
library(car) # For variance inflation factor (VIF)


df_clean <- df_clean |> 
  mutate(rularity = factor(rularity, 
                            levels = c("Rural",
                                       "Urban")))

df_clean <- df_clean |> 
  mutate(
    have_you_ever_receiv_struation_in_school = fct_collapse(
      have_you_ever_receiv_struation_in_school,
      "Yes, in primary school" = c("Yes, in secondary school", "Yes, in primary school"),
      "No, did not receive education"= c("No, did not receive education about menstruation in school")
    )
  ) 

levels(df_clean$have_you_ever_receiv_struation_in_school)
# Ordinal Logistic Regression Model

rural<-df_clean |> 
  filter(rularity == "Rural") 
  
model <- clm(
  i_felt_clean_during_my_last_period ~ 
    during_your_last_men_our_menstrual_period +
    have_you_ever_receiv_struation_in_school +
    age  ,
  data = rural
)

df_clean$age<-as.numeric(df_clean$age)  # Ensure age is numeric for the model)

# Model Summary

# Publishable Table
a <- tbl_regression(
  model,
  exponentiate = TRUE,
  label = list(
    during_your_last_men_our_menstrual_period ~ "Had enough menstruation materials",
    have_you_ever_receiv_struation_in_school ~ "Received MHM Education in School",
    age ~ "Age (years)"
  )
) %>%
  bold_p() %>%
  modify_header(label = "**Predictor**") %>%
  modify_caption("**Ordinal Regression: Impact of MHM Interventions on Feeling Clean During Menstruation**")

############urban
urban<-df_clean |> 
  filter(rularity == "Urban") 

model <- clm(
  i_felt_clean_during_my_last_period ~ 
    during_your_last_men_our_menstrual_period +
    have_you_ever_receiv_struation_in_school +
    age  ,
  data = urban
)

df_clean$age<-as.numeric(df_clean$age)  # Ensure age is numeric for the model)

# Model Summary

# Publishable Table
b<- tbl_regression(
  model,
  exponentiate = TRUE,
  label = list(
    during_your_last_men_our_menstrual_period ~ "Had enough menstruation materials",
    have_you_ever_receiv_struation_in_school ~ "Received MHM Education in School",
    age ~ "Age (years)"
  )
) %>%
  bold_p() %>%
  modify_header(label = "**Predictor**") %>%
  modify_caption("**Ordinal Regression: Impact of MHM Interventions on Feeling Clean During Menstruation**")

tbl_merge(
  tbls = list(a, b),
  tab_spanner = c("**Rural**", "**Urban**")) |> 
  as_flex_table()  |> 
  save_as_docx(path = "~/gitrepos/mhm-blantyre/result/ordinal_regression.docx")
df_clean |> 
  filter(rularity=="Rural") |> 
  select(i_felt_clean_during_my_last_period,during_your_last_men_our_menstrual_period) |> 
  tbl_summary(by=during_your_last_men_our_menstrual_period,
              missing="no") |> 
  add_p()

df_clean$age<-as.factor(df_clean$age)
levels(df_clean$age)

############I use a label so that I print out a variable name

target_label <- "Location/Area"

# Find variables with matching labels (no unlist needed)
matching_vars <- df_clean %>%
  map_lgl(~ {
    lbl <- var_label(.x)  # Remove unlist = TRUE
    if (is.null(lbl)) FALSE else lbl == target_label
  }) %>%
  which() %>%
  names()
# Print results
print(matching_vars)
#########I use a variable name so that I print out a label


# Modified workflow with explicit naming -------------------------------------

# 1. Read original question labels (long text headers)
# data import -------------------------------------------------------------
# 1. Read original question text (long labels)
raw_questions_df <- read_csv(here::here("data/raw/survey-labels-real.csv")) |>
  mutate(across(where(is.character), as_factor))

# 2. Read clean variable names (short codes)
clean_vars_df <- read_csv(here::here("data/raw/survey-variable-names-real.csv")) |>
  mutate(across(where(is.character), as_factor)) |>
  clean_names()

# 3. Preserve original labels
question_labels <- colnames(raw_questions_df)  # Original full questions

# 4. Create analysis dataframe with clean names
analysis_df <- raw_questions_df
colnames(analysis_df) <- colnames(clean_vars_df)  # Apply short variable names

# 5. Build structured output with correct header order
formatted_output <- analysis_df |>
  # Convert all data to character first
  mutate(across(everything(), as.character)) |>
  # FIRST add variable names (will become row 2)
  add_row(
    tibble_row(!!!set_names(colnames(analysis_df), colnames(analysis_df))),
    .before = 1
  ) |>
  # THEN add labels (will become row 1)
  add_row(
    tibble_row(!!!set_names(question_labels, colnames(analysis_df))),
    .before = 1
  )

# 6. Write to CSV with no headers
write_excel_csv(
  formatted_output,
  here::here("data/raw/survey-structured2.csv"), 
  col_names = FALSE
)
         


#complement poisson model
# Fit separate models
poisson_rural <- glm(
  during_your_last_men_you_miss_school_work ~ 
    during_your_last_men_menstrual_materials +
    during_your_last_men_home_or_school_clean + 
    during_your_last_men_enever_you_wanted_to +
    i_was_able_to_choose_l_most_wanted_to_use + 
    i_had_enough_of_my_m_often_as_l_wanted_to +
    i_was_able_to_wash_m_nds_when_l_wanted_to +
    i_was_able_to_dispos_way_that_l_wanted_to,
  family = poisson(link = "log"), 
  data = filter(df_clean, rularity2 == "Urban")
)

poisson_urban <- glm(
  during_your_last_men_you_miss_school_work ~ 
    during_your_last_men_menstrual_materials +
    during_your_last_men_home_or_school_clean + 
    during_your_last_men_enever_you_wanted_to +
    i_was_able_to_choose_l_most_wanted_to_use + 
    i_had_enough_of_my_m_often_as_l_wanted_to +
    i_was_able_to_wash_m_nds_when_l_wanted_to 
    ,
  family = poisson(link = "log"), 
  data = filter(df_clean, rularity2 == "Rural")
)

# Process models with significance stars
process_model <- function(model) {
  tidy(model) |> 
    mutate(
      IRR = exp(estimate),
      p.value = case_when(
        p.value < 0.001 ~ "<0.001***",
        p.value < 0.01 ~ sprintf("%.3f**", p.value),
        p.value < 0.05 ~ sprintf("%.3f*", p.value),
        TRUE ~ sprintf("%.3f", p.value)
      )
    ) |> 
    select(term, estimate, IRR, p.value)
}

# Create combined table
combined_tbl <- bind_rows(
  process_model(poisson_rural) |> mutate(Model = "Rural"),
  process_model(poisson_urban) |> mutate(Model = "Urban")
) |>
  pivot_wider(
    names_from = Model,
    values_from = c(estimate, IRR, p.value),
    names_glue = "{Model}_{.value}"
  )

# Format table
gt_tbl <- combined_tbl |>
  gt() |>
  fmt_number(columns = c(Rural_estimate, Urban_estimate), decimals = 3) |>
  fmt_number(columns = c(Rural_IRR, Urban_IRR), decimals = 2) |>
  cols_label(
    term = "Predictor",
    Rural_estimate = "Coef.",
    Rural_IRR = "IRR",
    Rural_p.value = "p-value",
    Urban_estimate = "Coef.", 
    Urban_IRR = "IRR",
    Urban_p.value = "p-value"
  ) |>
  tab_spanner(
    label = "Rural",
    columns = c(Rural_estimate, Rural_IRR, Rural_p.value)
  ) |>
  tab_spanner(
    label = "Urban",
    columns = c(Urban_estimate, Urban_IRR, Urban_p.value)
  ) |>
  tab_header(
    title = "Poisson Regression by Location",
    subtitle = "Stratified analysis of menstrual management factors on school/work absences"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Rural_p.value,
      rows = Rural_p.value < 0.05
    )
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = Urban_p.value,
      rows = Urban_p.value < 0.05
    )
  )

gt_tbl

gt_tbl |> gtsave("stratified_poisson.docx")

attach(graduate_survey)
library(dplyr)
library(stringr)





# Select relevant columns
relevant_columns <- c("Campus", "StudyField", "Branch", "Role", "EduLevel", 
                      "ProgLang", "Databases", "Platform", "WebFramework", 
                      "Industry", "AISearch", "AITool", "Employment")

# Ensure selected columns exist in the dataset
existing_columns <- relevant_columns[relevant_columns %in% names(graduate_survey)]
graduate_survey <- graduate_survey[ , existing_columns, drop = FALSE]


# Standardizing campus names
graduate_survey$Campus <- case_when(
  graduate_survey$Campus %in% c( "Umhlanga Campus") ~ "Durban Campus",
  graduate_survey$Campus %in% c("Mowbray Campus") ~ "Mowbray Campus",
  graduate_survey$Campus %in% c("Nelspruit Campus") ~ "Mowbray Campus",
  TRUE ~ graduate_survey$Campus
)

# Remove rows with missing values or empty strings
graduate_survey <- graduate_survey[rowSums(is.na(graduate_survey) | graduate_survey == "") == 0, ]

# Identify the top 5 campuses with the most responses if Campus column exists
if("Campus" %in% existing_columns) {
  campus_counts <- sort(table(graduate_survey$Campus), decreasing = TRUE)
  top_campuses <- names(campus_counts)[1:5]
  # Ensure only top 5 campuses are included
  graduate_survey <- graduate_survey %>% filter(Campus %in% top_campuses)
}

# Save the cleaned dataset
write.csv(graduate_survey, "cleaned_graduate_survey.csv", row.names = FALSE)

# Print summary of the cleaned dataset
print(summary(graduate_survey))


#question2


library(tidyverse)
library(ggplot2)

#splitting and counting

count_top_tools <- function(df, column, top_n = 10) {
  df %>%
    filter(!is.na(!!sym(column))) %>%
    separate_rows(!!sym(column), sep = ";") %>%
    count(!!sym(column), sort = TRUE) %>%
    top_n(top_n, n)
}
#applying function to relevant column
top_prog_langs <- count_top_tools(graduate_survey, "ProgLang")
top_databases <- count_top_tools(graduate_survey, "Databases")
top_platforms <- count_top_tools(graduate_survey, "Platform")
top_webframeworks <- count_top_tools(graduate_survey, "WebFramework")
top_ai_search <- count_top_tools(graduate_survey, "AISearch")
top_ai_tools <- count_top_tools(graduate_survey, "AITool")

#1.bar plot visualisation 

plot_top_tools <- function(data, title) {
  ggplot(data, aes(x = reorder(!!sym(names(data)[1]), n), y = n, fill = n)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    coord_flip() +
    labs(title = title, x = "Tool", y = "Count") +
    theme_minimal()
}

# Generate plots
plot_top_tools(top_prog_langs, "Top Programming Languages Used")
plot_top_tools(top_databases, "Top Databases Used")
plot_top_tools(top_platforms, "Top Platforms Used")
plot_top_tools(top_webframeworks, "Top Web Frameworks Used")
plot_top_tools(top_ai_search, "Top AI Search Tools Used")
plot_top_tools(top_ai_tools, "Top AI Tools Used")

#II. split and counting by study fields
industry_by_field <- graduate_survey %>%
  filter(!is.na(Industry) & !is.na(StudyField)) %>%
  separate_rows(Industry, sep = ";") %>%
  count(StudyField, Industry, sort = TRUE)

#generate plot for most popuplar industries graduates go int
ggplot(industry_by_field, aes(x = reorder(Industry, n), y = n, fill = Industry)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~StudyField, scales = "free_y") +
  coord_flip() +
  labs(title = "Top Industries Graduates Enter", x = "Industry", y = "Count") +
  theme_minimal()


#III.split and count job roles-study field 
job_roles_by_field <- graduate_survey %>%
  filter(!is.na(Role) & !is.na(StudyField)) %>%
  separate_rows(Role, sep = ";") %>%
  count(StudyField, Role, sort = TRUE)

#visualize top job roles for graduates
ggplot(job_roles_by_field, aes(x = reorder(Role, n), y = n, fill = Role)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~StudyField, scales = "free_y") +
  coord_flip() +
  labs(title = "Top Job Roles Graduates Enter", x = "Job Role", y = "Count") +
  theme_minimal()


#IV. 


# Calculate employment rate per study field

library(tidyverse)

# Split Employment into multiple rows
expanded_survey <- graduate_survey %>%
  separate_rows(Employment, sep = ";") %>%
  mutate(Employment = str_trim(Employment))

employment_rate <- expanded_survey %>%
  filter(!is.na(Employment) & !is.na(StudyField)) %>%
  group_by(StudyField) %>%
  summarise(
    Employed = sum(str_detect(Employment, "Employed")),  # Count rows with "Employed"
    Total = n_distinct(graduate_survey$StudyField),  # Use original unique study field counts
    EmploymentRate = round((Employed / Total) * 100, 1)
  )

print(employment_rate)

#plot employment rate per study field pie chart 
ggplot(employment_rate, aes(x = "", y = EmploymentRate, fill = StudyField)) +
  geom_col(width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(title = "Employment Rate by Study Field", x = NULL, y = NULL) +
  theme_void() +
  theme(legend.title = element_blank())
#bar chart
ggplot(employment_rate, aes(x = reorder(StudyField, EmploymentRate), y = EmploymentRate, fill = EmploymentRate)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  labs(title = "Employment Rate by Study Field", x = "Study Field", y = "Employment Rate (%)") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue")


















#install.packages("tidyverse")
#install.packages("ggplot2")

library(tidyverse) # load this first for most/all things
library(peacesciencer)
library(ggplot2)
library(dplyr)
download_extdata()

intData <- read_csv("GEDEvent_v25_1.csv") |>
  mutate(average = deaths_civilians/best) |>
  filter(!is.na(conflict_new_id))

view(intData)

intraData <- read_csv("ucdp-intrastate-conflict-onset-251.csv")

intraData_clean <- intraData %>%
  mutate(conflict_ids = as.character(conflict_ids)) %>%
  separate_rows(conflict_ids, sep = " ") %>%
  mutate(conflict_ids = as.numeric(conflict_ids)) |>
  filter(!is.na(conflict_ids)) 


view(intraData_clean)

interData <- read_csv("ucdp-interstate-conflict-onset-251.csv")

interData_clean <- interData %>%
  mutate(conflict_ids = as.character(conflict_ids)) %>%
  separate_rows(conflict_ids, sep = " ") %>%
  mutate(conflict_ids = as.numeric(conflict_ids)) |>
  filter(!is.na(conflict_ids)) 

intraData_clean$war_type <- 1

interData_clean$war_type <- 2


intraData2 <- read_csv("ucdp-intrastate-country-onset-251.csv")

dataTogether <- left_join(intData, intraData_clean, by = c("year" = "year", "conflict_new_id" = "conflict_ids"))

dataTogether2 <- left_join(intData, interData_clean, by = c("year" = "year", "conflict_new_id" = "conflict_ids"))

view(dataTogether2)

finalData <- dataTogether |>
  filter(!is.na(war_type)) |>
  mutate(average = deaths_civilians/best) 
  

finalData2 <- dataTogether2 |>
  filter(!is.na(war_type)) |>
  mutate(average = deaths_civilians/best) 

fullFinal <- full_join(by = c(finalData, finalData2, by = c("year" = "year", "conflict_new_id" = "conflict_ids",
                              "war_type" = "war_type"))




view(fullFinal)

view(finalData2)


yearly_avg <- finalData |>
  group_by(year) |>
  summarize(average = mean(average, na.rm = TRUE))

yearly_avg2 <- finalData2 |>
  group_by(year) |>
  summarize(average = mean(average, na.rm = TRUE))


# Basic line plot
ggplot(yearly_avg, aes(x = year, y = average)) +
  geom_line() +
  labs(x = "Year", y = "average")

# Basic line plot
ggplot(yearly_avg, aes(x = year, y = average)) +
  geom_line() +
  labs(x = "Year", y = "average")

ggplot() +
  geom_point(data = yearly_avg, aes(x = year, y = average), color = "blue") +
  geom_point(data = yearly_avg2, aes(x = year, y = average), color = "red") +
  labs(x = "Year", y = "Average", title = "Comparison of Two Datasets")

ggplot(yearly_avg, aes(x = year, y = average)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # se = FALSE removes confidence interval
  labs(x = "Year", y = "Average")

ggplot(yearly_avg2, aes(x = year, y = average)) +
  geom_point() +
  geom_point(size = 2) +
  theme_classic() +
  scale_color_viridis_d() +
  geom_smooth(method = "lm", se = FALSE, size = 1.2) +  # se = FALSE removes confidence interval
  labs(x = "Year", y = "Average", title = "          Interstate Yearly Average")



ggplot(yearly_avg2, aes(x = year, y = average)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dashed") +
  scale_color_manual(values = c("#E74C3C", "#3498DB")) +
  labs(
    title = "Trends in Armed Conflict Deaths",
    subtitle = "Comparison of state-based and one-sided violence, 1989-2024",
    x = "Year",
    y = "Average Number of Deaths",
    caption = "Source: UCDP Georeferenced Event Dataset v25.1"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 12, color = "gray40", hjust = 0),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray30")
  )



ggplot(yearly_avg, aes(x = year, y = average)) +
  geom_point(size = 2.5, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.1, linetype = "dashed") +
  scale_color_manual(values = c("#E74C3C", "#3498DB")) +
  labs(
    title = "Trends in Armed Conflict Deaths",
    subtitle = "Comparison of state-based and one-sided violence, 1989-2024",
    x = "Year",
    y = "Average Number of Deaths",
    caption = "Source: UCDP Georeferenced Event Dataset v25.1"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15, hjust = 0),
    plot.subtitle = element_text(size = 8, color = "gray40", hjust = 0),
    plot.caption = element_text(size = 7, color = "gray50", hjust = 1),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "gray30")
  )





#finish

























































































yearly_avg <- intData |>
  group_by(type_of_violence) |>
  summarize(average = mean(average, na.rm = TRUE))



# Basic line plot
ggplot(yearly_avg, aes(x = year, y = average)) +
  geom_line() +
  labs(x = "Year", y = "average")


intraData_clean <- intraData %>%
  mutate(conflict_ids = as.character(conflict_ids)) %>%
  separate_rows(conflict_ids, sep = " ") %>%
  mutate(conflict_ids = as.numeric(conflict_ids))

view(intraData_clean)


intData_unique <- intData |>
  distinct(year, conflict_new_id, .keep_all = TRUE)

intraData_unique <- intraData |>
  distinct(year, conflict_ids, .keep_all = TRUE)

intData |>
  count(year, conflict_new_id) |>
  filter(n > 1)

intraData |>
  count(year, conflict_ids) |>
  filter(n > 1)


view(intraData)


colnames(intraData)


dataTogether <- left_join(intData, intraData_clean, by = c("year" = "year", ))



#intData <- create_dyadyears(directed = F)
#\old data\ dataTogether <- left_join(intData, intraData, by = c("year" = "year", "conflict_new_id = conflict_id"))










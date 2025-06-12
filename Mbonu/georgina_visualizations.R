#load in data
library(tidyverse)
prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/prescriptions.csv")
View(prescriptions)
prescriptions |>
  filter(OpioidFlag == 'Opioid') |>
  select(State)

#cleaning to find number of claims for opioids per state and specialty
library(dplyr)
opioid_data <- prescriptions |>
  filter(OpioidFlag == "Opioid")
opioid_summary <- opioid_data |>
  group_by(State, SpecialtyCateg) |>
  summarise(TotalClaims = sum(NumberClaims, na.rm = TRUE), .groups = "drop")

#top states for total claims for every specialty
top_states <- opioid_summary |>
  group_by(SpecialtyCateg) |>
  slice_max(TotalClaims, n = 5) |>
  ungroup() |>
  mutate(State = factor(State))

#make graph for top states for total claims for every specialty
ggplot(top_states, aes(x = State, y = TotalClaims, fill = SpecialtyCateg)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ SpecialtyCateg, scales = 'free_y') +
  theme_minimal() +
  labs(
    title = "Top 5 States by Opioid Claims Within Each Specialty",
    x = "State",
    y = "Total Opioid Claims"
  )

#total opioid cost by state, specialty
opioid_summary <- opioid_data |>
  group_by(State, SpecialtyCateg) |>
  summarise(TotalCost = sum(TotalDrugCost, na.rm = TRUE), .groups = "drop")

#top states for total opioid cost
top_states <- opioid_summary |>
  group_by(SpecialtyCateg) |>
  slice_max(TotalCost, n = 5) |>
  ungroup() |>
  mutate(State = factor(State))

#plot top states opioid cost by specialty
ggplot(top_states, aes(x = reorder(State, -TotalCost), y = TotalCost, fill = SpecialtyCateg)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ SpecialtyCateg, scales = 'free_y') +
  theme_minimal() +
  labs(
    title = "Top 5 States by Opioid Cost Within Each Specialty",
    x = "State",
    y = "Total Cost of Opioid prescriptions")+ 
    scale_y_continuous(labels = scales::label_comma())

ggplot(top_states, aes(x = reorder(State, -TotalCost), y = TotalCost, fill = SpecialtyCateg)) +
 geom_bar(stat = "identity") +
 coord_flip() +
 scale_y_continuous(labels = scales::label_comma()) +
 labs(
  x = "State",
  y = "Total Opioid Cost",
  fill = "Specialty Category",
  title = "Top States by Opioid Costs by Specialty"
   ) +
  theme_minimal()
                       
                       

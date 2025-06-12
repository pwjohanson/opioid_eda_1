library(tidyverse)
library(ggplot2)
library(forcats)

prescriptions <- read_csv("https://raw.githubusercontent.com/36-SURE/2025/main/data/prescriptions.csv")
View(prescriptions)

#Sorting Opioid Brands by total number of claims

top_brands <- prescriptions |> 
  filter(OpioidFlag == "Opioid", !is.na(BrandName)) |> 
  group_by(BrandName) |> 
  summarise(total_claims = sum(NumberClaims, na.rm = TRUE), .groups = "drop") |> 
  slice_max(total_claims, n = 10)

#Arranging in order from lowest number of claims to highest number of claims

brand_levels <- top_brands |>
  arrange(total_claims) |> 
  pull(BrandName)

#Grouping total number of claims per brand by Specialty Category

claims_data <- prescriptions |>
  filter(
    OpioidFlag == "Opioid",
    !is.na(SpecialtyCateg),
    !is.na(BrandName),
    BrandName %in% brand_levels
  ) |> 
  group_by(SpecialtyCateg, BrandName) |> 
  summarise(n = sum(NumberClaims, na.rm = TRUE), .groups = "drop") |>
  mutate(BrandName = factor(BrandName, levels = brand_levels))

#Plotting Number of Opioid Claims by Brand and Specialty

ggplot(claims_data, aes(x = fct_reorder(SpecialtyCateg, n), y = n, fill = BrandName)) +
  geom_col() +
  labs(title = "Number of Opioid Claims by Brand and Specialty", y = "Number of Claims", x = "Specialty Category") +
  coord_flip()+
  theme(plot.title = element_text(face = "bold"), 
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        axis.title.x = element_text(size = 14))


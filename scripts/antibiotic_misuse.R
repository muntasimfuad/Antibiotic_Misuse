# Install required packages
install.packages("tidyverse")
install.packages("gtsummary")
install.packages("gt")
install.packages("readxl")
install.packages("easystats")
install.packages("naniar")


# Load rtequired packages
library(tidyverse)
library(gtsummary)
library(gt)
library(readxl)
library(easystats)
library(naniar)


# Import/Load data

data <- read_xlsx("data/AMR_KAP_Data.xlsx")

# Check missing data

gg_miss_var(data)

# Check duplicated rows

sum(duplicated(data))

# Table 1 Demographic characteristics of study participants (N = 704)

data |>  select(1:11) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/Table 1.docx")


# Distribution of knowledge of antibiotic resistance among parents of school-going children (N = 704)

fig1 <- data |>  select(12:23)


#  Reshape the data

long_fi1 <- fig1 |> pivot_longer(cols = 1:12,
                                 names_to = "Questions",
                                 values_to = "Responses")

cont_response <- long_fi1 |>  group_by(Questions,Responses) |> 
  summarise(Count = n(), .groups = 'drop') |> 
  mutate(Percentage = Count / sum(Count)*100)
  

# Generate the figure 1
figure1 <- ggplot(cont_response, aes(x = Questions, y = Percentage, fill = Responses)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Yes" = "#31a354", "No" = "#de2d26", "Don't Know" = "#f7fcb9")) + 
  labs(
    title = "Distribution of knowledge of antibiotic resistance among parents of school-going children (N = 704)",
    x = "",
    y = "Percentage",
    fill = "Responses"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  ) +
  coord_flip()

# Save the figure
ggsave("figures/Figure1.png", plot = figure1, 
       width = 14, 
       height = 8, 
       dpi = 300)



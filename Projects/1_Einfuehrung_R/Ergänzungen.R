if(!require('knitr')) {
  install.packages('knitr')
}
library(knitr)

data <- read_csv("dataset.csv")

# kable aus knitr unterstützt die Umsetzung von Tibbles in
# Formate wie html, latex oder md.
data %>%
  mutate(age = round(age, -1)) %>%
  group_by(age) %>%
  summarise(total = n()) %>%
  kable("latex") %>%
  write("mutated_data.tex")

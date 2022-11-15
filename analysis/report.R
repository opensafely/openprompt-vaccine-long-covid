library('tidyverse')
df_dummy <- read_csv(here::here("output/dummy_data.csv"))
df_input <- read_csv(
  here::here("output", "dataset.csv"),
  col_types = cols(patient_id = col_integer(),year_of_birth = col_double())
)

df_input_select <- df_input %>% 
  filter(prefup_4mo == 1)

plot_age <- ggplot(data=df_input, aes(df_input$age, fill=factor(sex))) + 
  geom_histogram() + 
  theme_classic()

ggsave(
  plot= plot_age,
  filename="descriptive.png", path=here::here("output"),
)
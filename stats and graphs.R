# Aaron Smith
#
# Task: Run statistical tests and generate graphs for athlete data.
#
################################################################################
# Load data and libraries ----
library(janitor)
library(ggthemes)
library(knitr)
library(kableExtra)
library(hrbrthemes)
library(gplots)
library(car)
library(ggrepel)
library(tidyverse)
library(scales)
library(reshape2)
library(formattable)
library(directlabels)
################################################################################
# Create a dummy dataset ----
# Set seed for reproducibility
set.seed(42)

# Define variables
sports <- c('Basketball', 'Soccer', 'Baseball', 'Tennis', 'Volleyball', 'Football', 'Swimming', 'Golf', 'Softball', 'Track and Field')
sexes <- c('Male', 'Female')
years <- sample(2017:2022, 1000, replace=TRUE)
# Generate GPAs with higher likelihood between 2.5 and 4
gpas <- round(runif(1000, 2.5, 4), 2)
athlete_options <- c('Student-Athlete', 'Non-Student-Athlete')
athlete_data <- sample(athlete_options, 1000, replace=TRUE)
sports_data <- ifelse(athlete_data == 'Non-Student-Athlete', 'None', sample(sports, 1000, replace=TRUE))
sex_data <- sample(sexes, 1000, replace=TRUE)

# Create a data frame
mlr_data <- data.frame(
  Sport = sports_data,
  Sex = sex_data,
  GPA = gpas,
  Year = years,
  Athlete = athlete_data
)

# Run a Multiple Linear Regression on Athletes ----
  # Y = GPA
  # X = Sex, Sport, Year

athlete_data <- mlr_data %>%
  filter(Athlete %in% "Student-Athlete")

# Look at Relationships between variables
  # GPA ~ Sport
lattice::densityplot(~ GPA, 
                     groups = Sport, 
                     data = athlete_data,
                     auto.key = TRUE)

  # GPA ~ Sex 
lattice::densityplot(~ GPA, 
                     groups = Sex, 
                     data = athlete_data,
                     auto.key = TRUE) 

lattice::densityplot(~ GPA, 
                     groups = Year, 
                     data = athlete_data,
                     auto.key = TRUE)

# Build the Model
model <- lm(GPA ~ Sport + Sex + Year, 
            data = athlete_data) 

# Summarize (only show the values that are significant)
summary(model)
vif(model) # low VIF for each, little covariance


# 
# T-Tests and Graphs Showing Differences between Athletes and Students ----
# Run a two-sample t-test on athlete GPAs versus non-athlete GPAs
student_data <- mlr_data %>% filter(Athlete %in% "Non-Student-Athlete")

set.seed(0)


athlete_gpa <- rnorm(nrow(athlete_data), mean = mean(athlete_data$GPA), sd = sd(athlete_data$GPA))
student_gpa <- rnorm(nrow(student_data), mean = mean(student_data$GPA), sd = sd(student_data$GPA))
general_t_test <- t.test(athlete_gpa, student_gpa, var.equal = TRUE)
general_t_test  
  # suggests this is a statistically significant result

# Run the two-sample t-tests by Year
running_tests <- mlr_data %>%
  select(Year, Sport, GPA)

year_split <- split(running_tests, running_tests$Year)

year_t_test <- lapply(year_split, function(year_split) {
  
  year_athletes <- year_split %>%
    filter(!Sport %in% "None")
  count_athletes <- nrow(year_athletes) # pulls athletes
  
  year_students <- year_split %>%
    filter(Sport %in% "None")
  count_students <- nrow(year_students) # pulls non-athletes
  
  # Run the t-test
  set.seed(0)
  year_athlete_gpa <- rnorm(count_athletes, mean = mean(year_athletes$GPA), sd = sd(year_athletes$GPA))
  year_non_athlete_gpa <- rnorm(count_students, mean = mean(year_students$GPA), sd = sd(year_students$GPA))
  year_second_t_test <- t.test(year_athlete_gpa, year_non_athlete_gpa, var.equal = TRUE)
  
  # Remove years that aren't significant
  if(year_second_t_test$p.value <= 0.1) {
  year_second_t_test$p.value
  }
})
year_t_test # 2022 is significant

# Graphically model GPAs for each group over time
gpa_by_year <- mlr_data %>% 
  group_by(Athlete, Year) %>%
  summarise(GPA = mean(GPA)) %>%
  ungroup() %>%
  mutate(GPA = round(GPA, 2))

gpa_years_graph <- ggplot(data = gpa_by_year, 
       mapping = aes(x = Year, 
                     y = GPA, 
                     label = round(GPA, 2))) + 
  geom_line(aes(color = Athlete), linewidth = 1.5) +
  geom_point(size = 2) +
  geom_text(aes(label = digits(GPA, digits = 2)), 
            vjust = 1.8) + # labels above their points
  ylim(3.0, 3.5) +
  geom_rangeframe() +
  theme_clean() +
  ggtitle("") +
  scale_color_manual(values = c('#E69F00', '#56B4E9')) +
  labs(fill = "Athlete Type") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), 
        legend.position = "right")
gpa_years_graph

# Create a bar graph showing the avg. GPA for each sport by year by sex with a marker for avg. school GPA ----
sport_gpa <- mlr_data %>%
  filter(Athlete %in% "Student-Athlete") %>%
  group_by(Sex, Sport) %>%
  summarise(GPA = mean(GPA)) %>%
  ungroup() %>%
  mutate(GPA = round(GPA, 2)) %>%
  arrange(desc(GPA)) # make sure GPAs are in descending order

sport_gpa_split <- split(sport_gpa, sport_gpa$Sex)

sports_gender_graph <- lapply(sport_gpa_split, function(sport_gpa) {
  
  sport_gpa_graph <- ggplot(sport_gpa, aes(x = reorder(Sport, GPA), y = GPA)) +
    geom_bar(stat = 'identity', 
             position = 'dodge', 
             color = '#000000',
             fill = '#56B4E9',
             width = .75,) + 
    labs(x = "Sport") +
    theme_clean() +
    ggtitle(paste0("Average GPAs for ", sport_gpa$Sex, " Sports")) +
    theme(plot.caption = element_text(face = "italic", 
                                      hjust = 0.5)) +
    coord_flip()

})
sports_gender_graph


# Which sports are above the GPA avg.? ----
above_below_avg <- sport_gpa %>%
  mutate(`Relation to Average` = if_else(GPA > mean(mlr_data$GPA), "Above", "Below")) %>%
  select(Sport, Sex, GPA, `Relation to Average`)
above_below_avg


# Generate the table (include color coding scale)
overall_gpa <- mean(mlr_data$GPA)

above_below_table <- above_below_avg %>%
  rownames_to_column('Aaron') %>%
  mutate(across(where(is.numeric), 
                function(x){cell_spec(x, 
                                      color = case_when(overall_gpa - x > 0.1 | overall_gpa - x < -0.1 ~ "white", 
                                                        overall_gpa - x < 0.1 | overall_gpa - x > -0.1 ~ "black"),
                                      
                                      background = case_when(overall_gpa - x < 0.1 & overall_gpa - x > 0 ~ "salmon", 
                                                             overall_gpa - x > 0.1 & overall_gpa - x > 0 & overall_gpa - x < 0.2 ~ "red",
                                                             overall_gpa - x > 0.2 & overall_gpa - x > 0 ~ "firebrick",
                                                             overall_gpa - x > -0.1 & overall_gpa - x < 0 ~ "palegreen",
                                                             overall_gpa - x < -0.1 & overall_gpa - x < 0 & overall_gpa - x > -0.2 ~ "green",
                                                             overall_gpa - x < -0.2 & overall_gpa - x < 0 ~ "forestgreen",
                                                             overall_gpa == x ~ "white"))}
  )) %>% 

  column_to_rownames('Aaron') %>%
  kable(escape = F, booktabs = T, align = rep('c'), 
        caption = "GPAs by Sport") %>%
  kable_classic(html_font = "Times New Roman", "striped", 
                full_width = F, font_size = 20) %>%
  column_spec(1, bold = T, border_right = T, background = "gainsboro") %>%
  row_spec(seq(1, nrow(above_below_avg), 1), extra_css = "border-bottom: 1px solid;")
above_below_table
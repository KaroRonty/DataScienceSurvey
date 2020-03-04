library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(scales)
library(stringr)
library(ggplot2)
library(gghalves)
library(ggbeeswarm)

options(scipen = 1e9)

# Data: https://drive.google.com/open?id=1QOmVDpd8hcVYqqUXDXf68UMDWQZP0wQV
data <- read.csv("survey_results_public.csv")

# Filter to include only Data Scientists
clean <- data %>% 
  filter(str_detect(DevType, ";Data scientist|Data scientist"),
         str_detect(MainBranch, "I am a developer by profession|not primarily"),
         str_detect(Employment, "full-time|Independent"),
         !is.na(ConvertedComp),
         ConvertedComp > 0)

# Make data for plotting
to_plot <- clean %>% 
  select(Country, ConvertedComp) %>% 
  group_by(Country) %>% 
  summarise(MedianConvertedComp = median(ConvertedComp, na.rm = TRUE),
            ConvertedComp = list(ConvertedComp),
            n = n()) %>% 
  filter(n > 10,
         MedianConvertedComp > 50000) %>% 
  unnest()

# Plot Data Scientist salaries by country
to_plot %>% 
  ggplot(aes(x = reorder(Country, MedianConvertedComp), 
             y = ConvertedComp,
             color = Country)) + 
  geom_half_point(transformation = ggbeeswarm:::PositionQuasirandom,
                  transformation_params =
                    formals(ggbeeswarm::position_quasirandom)) +
  geom_half_boxplot(nudge = 0.15, outlier.color = NA, color = "black") +
  xlab("") +
  ylab("Annual salary (USD) assuming 50 work weeks per year") +
  coord_flip(ylim = c(0, 200000)) +
  scale_y_continuous(breaks = seq(0, 200000, by = 10000), 
                     labels = comma) +
  ggtitle("Salaries of Data Scientists by country",
          subtitle =
            "Countries with a median salary above 50,000 USD and more than 10 respondents") +
  theme_bw() +
  theme(legend.position = "none",
        plot.caption = element_text(hjust = 0, lineheight = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Feature engineering for regression
regression <- clean %>% 
  filter(Country %in% unique(to_plot$Country),
         !is.na(LanguageWorkedWith),
         !is.na(YearsCodePro),
         ConvertedComp < 200000) %>% 
  mutate(R = ifelse(str_detect(LanguageWorkedWith,
                               ";R;|R;|R$"), 1, 0),
         Python = ifelse(str_detect(LanguageWorkedWith,
                                    "Python"), 1, 0),
         # Get first specified gender
         GenderMale = ifelse(str_detect(Gender, "Man"),
                             1, 0),
         UndergradMajorCompSci =
           ifelse(str_detect(UndergradMajor, "Computer science"),
                  1, 0),
         UndergradMajorMathsStats = 
           ifelse(str_detect(UndergradMajor, "Mathematics"),
                  1, 0),
         UndergradMajorBusiness = 
           ifelse(str_detect(UndergradMajor, "business"),
                  1, 0),
         EdLevelBSc = ifelse(str_detect(EdLevel, "Bachelor"), 1, 0),
         EdLevelMSc = ifelse(str_detect(EdLevel, "Master"), 1, 0),
         EdLevelLower =
           ifelse(str_detect(EdLevel,
                             "Asociate|Some|Secondary|never|Primary"), 1, 0),
         EdLevelHigher = ifelse(str_detect(EdLevel,
                                           "doctoral|Professional"), 1, 0)) %>% 
  mutate(R_and_Python = R * Python,
         YearsCodePro = as.numeric(as.character(YearsCodePro)),
         N_languages = str_count(LanguageWorkedWith,
                                 ";") + 1,
         Less_than_one_years_code =
           ifelse(YearsCode == "Less than 1 year", 1, 0),
         More_than_50_years_code =
           ifelse(YearsCode == "More than 50 years", 1, 0)) %>% 
  mutate(YearsCode = case_when(Less_than_one_years_code == 1 ~ 1,
                               More_than_50_years_code == 1 ~ 50,
                               TRUE ~ as.numeric(YearsCode))) %>% 
  select(Respondent,
         ConvertedComp,
         R,
         Python,
         R_and_Python,
         N_languages,
         MainBranch,
         Hobbyist,
         Employment,
         Student,
         EdLevelBSc,
         EdLevelMSc,
         EdLevelLower,
         EdLevelHigher,
         UndergradMajorCompSci,
         UndergradMajorMathsStats,
         UndergradMajorBusiness,
         OrgSize,
         YearsCode,
         Less_than_one_years_code,
         More_than_50_years_code,
         YearsCodePro,
         WorkWeekHrs,
         Age,
         GenderMale,
         Country) %>% 
  na.omit()
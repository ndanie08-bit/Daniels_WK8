###Natalie Daniels
###Data wrangling WK 8
###Oct 28. 2026

library(tidyverse)
library(scales)

##dataset: Gallups most inmportant problems
getwd()
gall <- read_csv("US-Public-Gallups_Most_Important_Problem-21.3.csv")

##plot 1: 

topic_labels <- c(
  "1"  = "Macroeconomics",
  "16" = "Defense",
  "2"  = "Civil Rights",
  "3"  = "Health",
  "7"  = "Environment",
  "12" = "Law & Crime"
)

plot_gall <- gall %>%
  filter(majortopic %in% as.integer(names(topic_labels))) %>%
  mutate(topic = factor(topic_labels[as.character(majortopic)],
                        levels = unname(topic_labels)))

ggplot(plot_gall, aes(x = year, y = percent)) +
  geom_line(aes(color = topic), linewidth = 0.7, na.rm = TRUE) +
  geom_smooth(color = "black", se = FALSE, span = 0.4,
              linewidth = 0.5, linetype = "dashed", na.rm = TRUE) +
  facet_wrap(~topic, ncol = 2) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(1950, 2020, by = 20)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Share of Gallup \"Most Important Problem\" Responses by Topic",
    subtitle = "Annual averages of poll-normalized proportions, 1946–2023",
    x = NULL,
    y = "Share of all responses",
    caption = "Source: Policy Agendas Project, Gallup MIP. No data for 1953 and 1955."
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())

###NEXT 

##Data: Democratic party data 
dem <- read_csv("Dem_partyplatform_20.2.csv") %>%
  filter(!is.na(year), majortopic %in% c(1, 2, 3, 7, 12, 16)) 

##share of ALL coded sentences each year
totals <- read_csv("Dem_partyplatform_20.2.csv") %>%
  filter(!is.na(year), majortopic > 0) %>%
  count(year, name = "total")

topic_labels2 <- c("1"="Macroeconomics","16"="Defense","2"="Civil Rights",
                  "3"="Health","7"="Environment","12"="Law & Crime")

plat <- dem %>%
  count(year, majortopic) %>%
  left_join(totals, by = "year") %>%
  mutate(share = n / total,
         topic = factor(topic_labels[as.character(majortopic)],
                        levels = unname(topic_labels)))
##Plot 2: 
ggplot(plat, aes(year, share, color = topic)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~topic, ncol = 2) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Democratic Platform Attention by Topic, 1948–2024",
       x = NULL, y = "Share of platform sentences") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")


###NEXT 

##Data: republican data
rep_dat <- read_csv("US-Party-Rep_Platform_19.3_0.csv") 

topic_labels3 <- c("1"="Macroeconomics","16"="Defense","2"="Civil Rights",
                  "3"="Health","7"="Environment","12"="Law & Crime")

rep_plat <- rep_dat %>%
  filter(!is.na(year), majortopic > 0) %>%
  count(year, majortopic) %>%
  group_by(year) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  filter(majortopic %in% as.integer(names(topic_labels))) %>%
  mutate(topic = factor(topic_labels[as.character(majortopic)],
                        levels = unname(topic_labels)))

##Plot 3: 
ggplot(rep_plat, aes(year, share, color = topic)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~topic, ncol = 2) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Republican Platform Attention by Topic, 1948–2024",
       x = NULL, y = "Share of platform sentences") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")





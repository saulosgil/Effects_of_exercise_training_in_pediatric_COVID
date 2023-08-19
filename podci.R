# Pacotes
library(tidyverse)
library(patchwork)

df <- read.csv('RCT_all data_COVIDped.xlsx - QoL.csv')

# ajustes
df <-
  df |>
  mutate(group = if_else(group == 1, 'HBET', 'SC-CONTROL'),
         time = if_else(time == 1, '1-PRE', '2-POST'))

glimpse(df)

# PODCI_upper
g1 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PODCI_upper,
                        na.rm = TRUE),
            sd = sd(PODCI_upper,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = time,
                       y = mean,
                       fill = group)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "PODCI_upper (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  )

# PODCI_Transfer
g2 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PODCI_Transfer,
                        na.rm = TRUE),
            sd = sd(PODCI_Transfer,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = time,
                       y = mean,
                       fill = group)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "PODCI_Transfer (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  )

# PODCI_Sports
g3 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PODCI_Sports,
                        na.rm = TRUE),
            sd = sd(PODCI_Sports,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = time,
                       y = mean,
                       fill = group)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "PODCI_Sports (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 5))

# PODCI_Pain
g4 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PODCI_Pain,
                        na.rm = TRUE),
            sd = sd(PODCI_Pain,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = time,
                       y = mean,
                       fill = group)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "PODCI_Pain (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  )

# POCI_Happiness
g5 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(POCI_Happiness,
                        na.rm = TRUE),
            sd = sd(POCI_Happiness,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = time,
                       y = mean,
                       fill = group)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "POCI_Happiness (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  )

# PODCI_GLOBAL
g6 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PODCI_GLOBAL,
                        na.rm = TRUE),
            sd = sd(PODCI_GLOBAL,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = time,
                       y = mean,
                       fill = group)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = TRUE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "PODCI_GLOBAL (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  )
# layout
(g1+g2+g3)/(g4+g5+g6)

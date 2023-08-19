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

# PEDS_Physical
g1 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PEDS_Physical,
                        na.rm = TRUE),
            sd = sd(PEDS_Physical,
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
  ylab(label = "PEDS_Physical (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 5))

# PEDS_Emocional
g2 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PEDS_Emocional,
                        na.rm = TRUE),
            sd = sd(PEDS_Emocional,
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
  ylab(label = "PEDS_Emocional (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 5))

# PEDS_Social
g3 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PEDS_Social,
                        na.rm = TRUE),
            sd = sd(PEDS_Social,
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
  ylab(label = "PEDS_Social (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100, 5))

# PEDS_School
g4 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PEDS_School,
                        na.rm = TRUE),
            sd = sd(PEDS_School,
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
  ylab(label = "PEDS_School (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  )

  # PEDS_Pysicosocial
g5 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PEDS_Pysicosocial,
                        na.rm = TRUE),
            sd = sd(PEDS_Pysicosocial,
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
  ylab(label = "PEDS_Pysicosocial (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
    theme(
      axis.title.x = element_blank()
    )

# PEDSQL_global
g6 <-
  df |>
  group_by(group, time) |>
  summarise(mean = mean(PEDSQL_global,
                        na.rm = TRUE),
            sd = sd(PEDSQL_global,
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
  ylab(label = "PEDSQL_global (a.u.)") +
  scale_x_discrete(labels = c("PRE", "POST"))+
  theme(
    axis.title.x = element_blank()
  )
# layout
(g1+g2+g3)/(g4+g5+g6)

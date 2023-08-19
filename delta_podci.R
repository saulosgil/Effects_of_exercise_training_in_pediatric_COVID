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

# separando o grupo treino - PRE
podci_pre <-
  df |>
  filter(group == 'HBET',
         time == '1-PRE')

# separando o grupo treino - POST
podci_post <-
  df |>
  filter(group == 'HBET',
         time == '2-POST')

# Juntando
podci <- inner_join(x = podci_pre,
                   y = podci_post,
                   by = "ID")

glimpse(podci)

# Deltas
delta_hbet <-
  podci |>
  mutate(
    delta_podci_upper    = PODCI_upper.y     - PODCI_upper.x,
    delta_podci_tranfer  = PODCI_Transfer.y  - PODCI_Transfer.x,
    delta_podci_sport    = PODCI_Sports.y    - PODCI_Sports.x,
    delta_podci_pain     = PODCI_Pain.y      - PODCI_Pain.x,
    delta_podci_hapiness = POCI_Happiness.y - POCI_Happiness.x,
    delta_podci_global   = PODCI_GLOBAL.y    - PODCI_GLOBAL.x,
  ) |>
  select(
    ID,
    group.x,
    delta_podci_upper,
    delta_podci_tranfer,
    delta_podci_sport,
    delta_podci_pain,
    delta_podci_hapiness,
    delta_podci_global
  )

glimpse(delta_hbet)

# separando o grupo controle - PRE
ctrl_pre <-
  df |>
  filter(group == 'SC-CONTROL',
         time == '1-PRE')

# separando o grupo treino - POST
ctrl_post <-
  df |>
  filter(group == 'SC-CONTROL',
         time == '2-POST')

# Juntando
ctrl <- inner_join(x = ctrl_pre,
                   y = ctrl_post,
                   by = "ID") |>
  select(-name.y,
         -group.y,
         -time.y)

glimpse(ctrl)

# Deltas
delta_ctrl <-
  ctrl |>
  mutate(
    delta_podci_upper    = PODCI_upper.y     - PODCI_upper.x,
    delta_podci_tranfer  = PODCI_Transfer.y  - PODCI_Transfer.x,
    delta_podci_sport    = PODCI_Sports.y    - PODCI_Sports.x,
    delta_podci_pain     = PODCI_Pain.y      - PODCI_Pain.x,
    delta_podci_hapiness = POCI_Happiness.y - POCI_Happiness.x,
    delta_podci_global   = PODCI_GLOBAL.y    - PODCI_GLOBAL.x,
  ) |>
  select(
    ID,
    group.x,
    delta_podci_upper,
    delta_podci_tranfer,
    delta_podci_sport,
    delta_podci_pain,
    delta_podci_hapiness,
    delta_podci_global
  )

glimpse(delta_ctrl)

# Juntado os deltas dos grupos treino e controle
delta_podci <- bind_rows(delta_hbet, delta_ctrl)

glimpse(delta_podci)

# delta_podci_upper
g1 <-
  delta_podci |>
  group_by(group.x) |>
  summarise(mean = mean(delta_podci_upper,
                        na.rm = TRUE),
            sd = sd(delta_podci_upper,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = group.x,
                       y = mean,
                       fill = group.x)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "delta_podci_upper (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 20, 5))

# delta_podci_emo
g2 <-
  delta_podci |>
  group_by(group.x) |>
  summarise(mean = mean(delta_podci_tranfer,
                        na.rm = TRUE),
            sd = sd(delta_podci_tranfer,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = group.x,
                       y = mean,
                       fill = group.x)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "delta_podci_tranfer (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# delta_podci_sport
g3 <-
  delta_podci |>
  group_by(group.x) |>
  summarise(mean = mean(delta_podci_sport,
                        na.rm = TRUE),
            sd = sd(delta_podci_sport,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = group.x,
                       y = mean,
                       fill = group.x)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "delta_podci_sport (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# delta_podci_pain
g4 <-
  delta_podci |>
  group_by(group.x) |>
  summarise(mean = mean(delta_podci_pain,
                        na.rm = TRUE),
            sd = sd(delta_podci_pain,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = group.x,
                       y = mean,
                       fill = group.x)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "delta_podci_pain (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# delta_podci_hapiness
g5 <-
  delta_podci |>
  group_by(group.x) |>
  summarise(mean = mean(delta_podci_hapiness,
                        na.rm = TRUE),
            sd = sd(delta_podci_hapiness,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = group.x,
                       y = mean,
                       fill = group.x)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "delta_podci_hapiness (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# delta_podci_hapiness
g6 <-
  delta_podci |>
  group_by(group.x) |>
  summarise(mean = mean(delta_podci_hapiness,
                        na.rm = TRUE),
            sd = sd(delta_podci_hapiness,
                    na.rm = TRUE)) |>
  ggplot(mapping = aes(x = group.x,
                       y = mean,
                       fill = group.x)) +
  geom_bar(position = position_dodge(),
           stat = "identity",
           colour = "black",
           show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean,
                    ymax = mean + sd),
                width=.2,
                position=position_dodge(0.9)) +
  theme_classic() +
  ylab(label = "delta_podci_hapiness (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# layout
(g1+g2+g3)/(g4+g5+g6)

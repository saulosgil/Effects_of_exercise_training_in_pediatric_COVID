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
hbet_pre <-
  df |>
  filter(group == 'HBET',
         time == '1-PRE')

# separando o grupo treino - POST
hbet_post <-
  df |>
  filter(group == 'HBET',
         time == '2-POST')

# Juntando
hbet <- inner_join(x = hbet_pre,
           y = hbet_post,
           by = "ID")

glimpse(hbet)

# Deltas
delta_hbet <-
  hbet |>
  mutate(
    delta_peds_phy  = PEDS_Physical.y - PEDS_Physical.x,
    delta_peds_emo  = PEDS_Emocional.y - PEDS_Emocional.x,
    delta_peds_soc  = PEDS_Social.y - PEDS_Social.x,
    delta_peds_sch  = PEDS_School.y - PEDS_School.x,
    delta_peds_pys  = PEDS_Pysicosocial.y - PEDS_Pysicosocial.x,
    delta_peds_glob = PEDSQL_global.y - PEDSQL_global.x,
  ) |>
  select(
    ID,
    group.x,
    delta_peds_phy,
    delta_peds_emo,
    delta_peds_soc,
    delta_peds_sch,
    delta_peds_pys,
    delta_peds_glob,
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
    delta_peds_phy  = PEDS_Physical.y - PEDS_Physical.x,
    delta_peds_emo  = PEDS_Emocional.y - PEDS_Emocional.x,
    delta_peds_soc  = PEDS_Social.y - PEDS_Social.x,
    delta_peds_sch  = PEDS_School.y - PEDS_School.x,
    delta_peds_pys  = PEDS_Pysicosocial.y - PEDS_Pysicosocial.x,
    delta_peds_glob = PEDSQL_global.y - PEDSQL_global.x,
  ) |>
  select(
    ID,
    group.x,
    delta_peds_phy,
    delta_peds_emo,
    delta_peds_soc,
    delta_peds_sch,
    delta_peds_pys,
    delta_peds_glob,
  )

glimpse(delta_ctrl)

# Juntado os deltas dos grupos treino e controle
delta_peds <- bind_rows(delta_hbet, delta_ctrl)

glimpse(delta_peds)

# delta_peds_phy
g1 <-
  delta_peds |>
  group_by(group.x) |>
  summarise(mean = mean(delta_peds_phy,
                        na.rm = TRUE),
            sd = sd(delta_peds_phy,
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
  ylab(label = "delta_peds_phy (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 50, 5))

# delta_peds_emo
g2 <-
  delta_peds |>
  group_by(group.x) |>
  summarise(mean = mean(delta_peds_emo,
                        na.rm = TRUE),
            sd = sd(delta_peds_emo,
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
  ylab(label = "delta_peds_emo (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 50, 5))

# delta_peds_soc
g3 <-
  delta_peds |>
  group_by(group.x) |>
  summarise(mean = mean(delta_peds_soc,
                        na.rm = TRUE),
            sd = sd(delta_peds_soc,
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
  ylab(label = "delta_peds_soc (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 50, 5))

# delta_peds_sch
g4 <-
  delta_peds |>
  group_by(group.x) |>
  summarise(mean = mean(delta_peds_sch,
                        na.rm = TRUE),
            sd = sd(delta_peds_sch,
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
  ylab(label = "delta_peds_sch (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# delta_peds_pys
g5 <-
  delta_peds |>
  group_by(group.x) |>
  summarise(mean = mean(delta_peds_pys,
                        na.rm = TRUE),
            sd = sd(delta_peds_pys,
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
  ylab(label = "delta_peds_pys (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# delta_peds_glob
g6 <-
  delta_peds |>
  group_by(group.x) |>
  summarise(mean = mean(delta_peds_glob,
                        na.rm = TRUE),
            sd = sd(delta_peds_glob,
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
  ylab(label = "delta_peds_glob (a.u.)") +
  scale_x_discrete(labels = c("HBET", "SC_CONTROL"))+
  theme(
    axis.title.x = element_blank()
  )

# layout
(g1+g2+g3)/(g4+g5+g6)

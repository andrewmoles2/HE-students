library(tidyverse)
library(ggtext)

location_14 <- read_csv("data/location/he_student_loc_1415.csv", skip = 14)
location_21 <- read_csv("data/location/he_student_loc_2122.csv", skip = 14)


l14_long <- location_14 %>%
  pivot_longer(cols = !c(UKPRN, `HE Provider`),
               names_to = "student_from",
               values_to = "n_students") %>%
  mutate(academic_year = "14/15")

l21_long <- location_21 %>%
  pivot_longer(cols = !c(UKPRN, `HE Provider`),
               names_to = "student_from",
               values_to = "n_students") %>%
  mutate(academic_year = "21/22")

students_from <- rbind(l14_long, l21_long)

uni_to_explore <- c("The University of Cambridge", "The University of Oxford",
                    "University College London", "The University of York",
                    "The University of Manchester", "The University of Bristol")

# Just looking at totals
df <- students_from %>%
  filter(`HE Provider` %in% uni_to_explore)

col_pal <- MetBrewer::met.brewer("Egypt", n = 2)
names(col_pal) <- c("14/15", "21/22")

title_text <- glue::glue(
  "Comparison of total number of students admitted to<br>given universities between <span style = 'color:{col_pal['14/15']}'>2014/2015</span> and <span style = 'color:{col_pal['21/22']}'>2021/2022</span>"
)

df %>%
  filter(student_from == "Total") %>%
  ggplot(aes(x = n_students, y = `HE Provider`, colour = academic_year)) +
  geom_point(size = 4, show.legend = FALSE) +
  scale_color_manual(values = col_pal) +
  labs(
    x = 'Total Students Admitted',
    y = element_blank(),
    title = title_text
  ) +
  theme_minimal(base_family = "Avenir", base_size = 16) +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown()
  )

# more elaborate 

camb <- df %>%
  filter(`HE Provider` == uni_to_explore[1]) %>%
  filter(student_from %in% c("Total", "Cambridgeshire")) %>%
  pivot_wider(names_from = student_from, values_from = n_students) %>%
  rename(home = Cambridgeshire) %>%
  mutate(perc_home = Total/home)
oxf <- df %>%
  filter(`HE Provider` == uni_to_explore[2]) %>%
  filter(student_from %in% c("Total", "Oxfordshire")) %>%
  pivot_wider(names_from = student_from, values_from = n_students) %>%
  rename(home = Oxfordshire) %>%
  mutate(perc_home = Total/home)
london <- df %>%
  filter(`HE Provider` == uni_to_explore[3]) %>%
  filter(student_from %in% c("Total", "Greater London")) %>%
  pivot_wider(names_from = student_from, values_from = n_students) %>%
  rename(home = `Greater London`) %>%
  mutate(perc_home = Total/home)
york <- df %>%
  filter(`HE Provider` == uni_to_explore[4]) %>%
  filter(student_from == "Total" | str_detect(student_from, "York")) %>%
  mutate(student_from = case_when(
    str_detect(student_from, "York") ~ "York",
    .default = "Total"
  )) %>%
  group_by(student_from, academic_year) %>%
  summarise(n_students = sum(n_students, na.rm = T)) %>%
  pivot_wider(names_from = student_from, values_from = n_students) %>%
  rename(home = "York") %>%
  mutate(perc_home = Total/home) %>%
  mutate(UKPRN = 10007167,
         `HE Provider` = "The University of York") %>%
  select(UKPRN, `HE Provider`, academic_year, home, Total, perc_home)
manc <- df %>%
  filter(`HE Provider` == uni_to_explore[5]) %>%
  filter(student_from %in% c("Total", "Greater Manchester")) %>%
  pivot_wider(names_from = student_from, values_from = n_students) %>%
  rename(home = `Greater Manchester`) %>%
  mutate(perc_home = Total/home)
brist <- df %>%
  filter(`HE Provider` == uni_to_explore[6]) %>%
  filter(student_from %in% c("Total", "City of Bristol")) %>%
  pivot_wider(names_from = student_from, values_from = n_students) %>%
  rename(home = `City of Bristol`) %>%
  mutate(perc_home = Total/home)

home_bind <- dplyr::bind_rows(camb, oxf, london, york, manc, brist)

home_df <- home_bind %>%
  pivot_wider(names_from = academic_year, values_from = c(home, Total, perc_home)) %>%
  janitor::clean_names() %>%
  mutate(change_perc = perc_home_21_22 - perc_home_14_15,
         change_home = home_21_22 - home_14_15,
         change_total = total_21_22 - total_14_15,
         he_provider = fct_reorder(he_provider, perc_home_14_15))


col_pal <- MetBrewer::met.brewer("Egypt", n = 4)
col_pal <- col_pal[3:4]
names(col_pal) <- c("14/15", "21/22")

title_text <- glue::glue(
  "Comparision of percent of students admitted to given universities<br>who are from the same town/city between <span style = 'color:{col_pal['14/15']}'>2014/2015</span> and <span style = 'color:{col_pal['21/22']}'>2021/2022</span>"
)

plot_home <- home_df %>%
  ggplot() + 
  geom_segment(aes(x = perc_home_14_15, y = he_provider, yend = he_provider, xend = perc_home_21_22),
               linewidth = 2.5, colour = "grey30") +
  geom_point(aes(x = perc_home_14_15, y = he_provider), 
             colour = col_pal["14/15"], size = 10) +
  geom_point(aes(x = perc_home_21_22, y = he_provider), 
             colour = col_pal['21/22'], size = 10) + 
  geom_text(aes(x = perc_home_21_22, y = he_provider, label = round(perc_home_21_22, 1)),
            size = 2.75, colour = "grey5") +
  geom_text(aes(x = perc_home_14_15, y = he_provider, label = round(perc_home_14_15, 1)),
            size = 2.75, colour = "grey5") +
  scale_x_continuous(labels = scales::label_percent(scale = 1),
                     breaks = seq(0, 20, by = 5),
                     limits = c(0, 20)) +
  labs(
    x = '% students from university city/town admitted',
    y = element_blank(),
    subtitle = title_text,
    title = "Who is going to university in their home town/city?"
  ) +
  theme_minimal(base_family = "Avenir", base_size = 16) +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(),
    plot.subtitle = element_markdown(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )
plot_home

ggsave(filename = "outputs/home_uni.png", plot_home, device = ragg::agg_png,
       dpi = 300, width = 13.5, height = 9, bg = "#F2F1F6")


# save cleaned up datasets
write_csv(students_from, "data/location/he_student_loc_1415_2122.csv")
write_csv(home_df, "data/home_loc_14_22.csv")
home_bind <- home_bind %>%
  mutate(perc_home = perc_home/100)
write_csv(home_bind, "data/home_loc_14_22_clean.csv")

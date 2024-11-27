#load packages
library(tidyverse)
library(nflplotR)
library(tidymodels)

# Database setup ---- 
## Load Datasets ----
#load and join datasets to main "draft" data frame
draft_prospects <- read_csv("data/nfl_draft_prospects.csv", guess_max = 13000)|>
  select(
    player_id, player_name, overall, school_name, school_abbr, pick, link, traded, trade_note, pick, weight, height, pos_rk, ovr_rk, draft_year, grade
  ) |> rename(
    pick_rd = pick, college_mascot = school_name, college_abbr = school_abbr 
  )

draft_fbr_11_20 <- readxl::read_xlsx("data/nfldraft_11_20.xlsx")
coaches_fbr_11_20 <- readxl::read_xlsx("data/nfl_coaches_11_20.xlsx")

#need to set team name equal to team drafted
#draft_contracts <- read_csv("data/combined_data_2000-2023.csv") |>
# filter(year_signed == draft_year & draft_year >= 2011 & draft_year <= 2020) |>
#filter()
#select(pick, draft_year, value, value_norm, gtd, gtd_norm) 

# view(draft_contracts)

## Join Datasets ----
#official join statement
draft <- draft_fbr_11_20 |>
  inner_join(draft_prospects, join_by(pick == overall, year == draft_year)) |>
  left_join(coaches_fbr_11_20, join_by(year, team))



view(draft)
skimr::skim_without_charts(draft)


## Evaluate Missingness ----
naniar::gg_miss_var(draft)


draft |>
  filter(is.na(w_av)) |>
  arrange(desc(gp)) |>
  select(player, gp, w_av)


# what does pos_rk NA mean?
draft |>
  filter(is.na(pos_rk)) |>
  arrange(desc(gp)) |>
  select(player, pick, year, pos_rk, ovr_rk)


draft <- draft |>
  complete(fill = list(w_av = 0, dr_av = 0, gp = 0)) 

## Clean/Add Variables  ----

# ignore relocation for Chargers, Raiders and Rams strings (they're the same team for our purposes)
draft <- draft |>
  mutate(
    team = fct_recode(team,
                      "LAC" = "SDG",
                      "LAR" = "STL",
                      "LVR" = "OAK"
    )
  ) 

#simplify position groups to standard groupings
std_pos_order <- c("QB", "RB", "WR", "TE", "OL", "IDL", "EDGE", "LB", "DB", "ST")
length(std_pos_order)
draft <- draft |>
  mutate(
    pos_group = fct_collapse(pos,
                             "RB" = c("RB", "FB"),
                             "OL" = c("C", "G", "T", "OL"),
                             "IDL" = c("DL", "DT", "NT"),
                             "EDGE" = c("DE", "OLB"),
                             "LB" = c("ILB", "LB"),
                             "DB" = c("DB", "CB","S"),
                             "ST" = c("K", "LS", "P")
    ),
    pos_group = fct_relevel(pos_group, std_pos_order)
  )

draft |>
  ggplot(aes(x = as.factor(round), y = pos_group)) +
  geom_tile(aes(fill = n))


# add helpful variables
draft <- draft |>
  mutate(
    career_length = played_to - year, 
    active = if_else(played_to == 2024, TRUE, FALSE)
  ) 

draft |>
  filter( w_av > 0) |>
  group_by(year) |>
  summarize(
    mean = mean(w_av, na.rm = TRUE),
    median = median(w_av, na.rm = TRUE),
    sd = sd(w_av, na.rm = TRUE),
    IQR = IQR(w_av, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = mean)) +
  geom_point() +
  geom_smooth(method="lm", formula= (mean ~ log(2024 - year)), se=FALSE, color=2)

#seems to be linear model of expected value
w_av_fit <- linear_reg() |>
  fit(w_av ~ year, data = draft)
w_av_fit

#seems to be exponential model of expected value
draft |>
  mutate (year_since = 2024 - year) |>
  lm(formula = w_av ~ log(year_since))

# weight player value relative to linear expectation (for draft year)
draft <- draft |>
  mutate(
    rel_w_av = w_av / (2.275 + 7.054 * log(2024 - year)),
    avg_w_av = if_else(career_length > 0, w_av / career_length, 0)
  ) 
draft |> 
  arrange(desc(rel_w_av)) |>
  select(player, rel_w_av)

draft |>
  group_by(year) |>
  summarize(
    mean = mean(w_av)
  )

# Initial Analysis ----
draft |>
  group_by(year) |>
  summarize(mean = mean(w_av, na.rm = TRUE)) |>
  ggplot(aes(x = as.factor(year), y = mean)) +
  geom_point() +
  labs(
    title = "Expected value (mean w_av) by draft class", 
    x = "draft class", 
    y = "expected value"
  )
ggsave(filename = "exp_w_av_1120.png")
## Univariate analysis ----

#distribution of expected value in draft
draft |>
  ggplot(aes(x = rel_w_av)) +
  geom_density()

#distribution of expected value in draft by round, and year
draft |>
  ggplot(aes(x = as.factor(round), y = rel_w_av)) +
  geom_boxplot()

draft |>
  ggplot(aes(x = rel_w_av)) +
  geom_density() +
  facet_grid(year ~ round)

## Bivariate Analysis ----
# by position and round
draft |>
  ggplot(aes(x = as.factor(round), y = rel_w_av)) +
  geom_boxplot() +
  facet_wrap(~pos_group)








# scratch work ----

draft |>
  ggplot(aes(x = dr_av, y = w_av)) +
  geom_point(alpha = 0.3)

draft |>
  ggplot(aes(x = pos_group, y = rel_w_av)) +
  geom_boxplot()

# mutate w_av by yr
draft |>
  mutate(
    rel_w_av = w_av / (2024 + 10 - year),
    .by = year
  ) |> arrange(desc(rel_w_av)) |> 
  select(player, rel_w_av)

draft |>
  mutate(
    rel_w_av = w_av / (2024 + 10 - year),
    .by = year
  ) |> ggplot(aes(x = rel_w_av)) +
  geom_de

# summary statistics

draft |>
  group_by(year) |>
  summarize(
    mean = mean(w_av),
    median = median(w_av),
    sd = sd(w_av),
    IQR = IQR(w_av)
  )

draft |>
  group_by(year) |>
  summarize(
    mean = mean(rel_w_av),
    median = median(rel_w_av),
    sd = sd(rel_w_av),
    IQR = IQR(rel_w_av)
  )


# let's do the basics
draft |>
  ggplot(aes(x = pick, y = w_av)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~year)

draft |>
  ggplot(aes(x = as.factor(round), y = w_av)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~team)








draft |>
  summarize(
    started = sum(start_yrs > 3),
    prop_start = started /n()
  )





draft |>
  summarize(
    avg_rel_w_av = sum(rel_w_av, na.rm = TRUE) / n(),
    .by = team
  ) |> arrange(desc(avg_rel_w_av)) |>
  ggplot(aes(x = team, y= avg_rel_w_av)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x = team, 
                   xend = team, 
                   y = min(avg_rel_w_av), 
                   yend = max(avg_rel_w_av)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()






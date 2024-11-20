### Goal: Get the data that we need to build the scatter plot on rushing yards/ weights by position
### Need: the data (ESPN and PSU)
# Steps:
# 1. Load packages ({tidyverse}, {rvest})
# 2. Get the Rushing Data
#   a. Read in the ESPN Page
#   b. Select the appropriate elements
#   c. Construct the tables
#   d. Identify the list elements we need
#   e. Clean data
# 3. Get Roster Data
#   a. Read in the PSU Roster Page
#   b. Select the appropriate elements
#   c. Construct the tables
#   d. Identify the list elements we need
#   e. Clean data
# 4. Combine the appropriate data frames



# Load Packages
library(tidyverse)
library(rvest)

# Get the rushing data
#2 a-c

espnRawList <-read_html(x = "https://www.espn.com/college-football/team/stats/_/id/213/penn-state-nittany-lions") %>%
              html_elements(css = "table") %>%
              html_table()
#2 d

espnRushingRaw <- bind_cols(espnRawList[[3]],espnRawList[[4]])


# Get roster data

#3 a-c

psuRawList <- read_html(x = "https://gopsusports.com/sports/football/roster?view=table") %>%
              html_elements(css = "table") %>%
              html_table()

#3 d 

psuRosterRaw <- psuRawList[[1]]


# Clean data

#2 e (espn data)
# drop "total" row
# separate name from position of player

espnRushing <- espnRushingRaw %>%
              filter(Name != "Total") %>%
              separate_wider_delim(
                  cols = Name,
                  delim = " ",
                  names = c("First", "Last", "Position1"),
                  too_many = "merge"
              ) %>%
              separate_wider_delim(
                  cols = Position1,
                  delim = " ",
                  names = c("last1", "Position"),
                  too_few = "align_end"
                  ) %>%
              unite(
                col = "Name",
                First,
                Last,
                last1,
                sep = " ",
                na.rm = TRUE
              )




# espnRushingCombineSeparate <- espnRushingRaw %>%
#   filter(Name != "Total") %>%
#   separate_wider_delim(
#     cols = Name,
#     delim = " ",
#     names = c("name1", "name2", "name3", "Position"),
#     too_few = "align_end"
#   ) %>%
#   unite(
#     col = "Name",
#     name1,
#     name2,
#     name3,
#     sep = " ",
#     na.rm = TRUE
#   )

#3 e (psu data)
psuRoster <- psuRosterRaw %>% 
              dplyr::select("#", Name, Position, Weight) %>%
              rename("Number" = "#") %>%
              mutate(
                Number = paste0("#", Number),
                Weight = readr::parse_number(Weight),
                Name = str_squish(Name)
              )
#4
psuRushing <- left_join(
              x = espnRushing,
              y = psuRoster,
              by = join_by(Name == Name, Position == Position)
)







# Future Work ----
psuPalette <- c(
  "#1E407C", "#BC204B", "#3EA39E", "#E98300",
  "#999999", "#AC8DCE", "#F2665E", "#99CC00"
)
## Make Plot ----
library(ggplot2)

ggplot(
  data = psuRushing,
  mapping = aes(
    x = Weight,
    y = YDS,
    shape = Position,
    color = Position,
    label = Number
  )
) +
  geom_point(size = 3) +
  geom_text(
    color = "black",
    nudge_x = 2.5,
    nudge_y = 0,
    size = 4,
    na.rm = TRUE
  ) +
  labs(
    title = "Total Rushing Yards by PSU Player Weight and Position",
    x = "Player weight (lbs)",
    y = "Total rushing yards",
    shape = "Position",
    color = "Position"
  ) +
  scale_color_manual(
    values = psuPalette
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 14)
  )




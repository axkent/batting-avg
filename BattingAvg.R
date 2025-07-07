library(baseballr)
library(dplyr)
library(slider)
library(plotly)
schedule <- mlb_schedule(season = 2025, level_ids = "1")
schedule <- schedule |> 
  filter(teams_home_team_name == "San Diego Padres" | teams_away_team_name == "San Diego Padres", 
         status_detailed_state == "Final", 
         series_description != "Spring Training") |> 
  select(date, game_pk, teams_home_team_name, teams_away_team_name)
batter.stats <- list()  
for (i in seq_along(schedule$game_pk)){
  pk <- schedule$game_pk[i]
  date <- schedule$date[i]
  print(pk)
  print(date)
  batter.stats[[as.character(pk)]] <- mlb_player_game_stats(665487, pk) |> 
    filter(group == "hitting") |> 
    mutate(date = date)
}
df <- bind_rows(batter.stats, .id = "game_pk")
df <- df |> 
  select(date, game_pk, summary, hits, at_bats) |> 
  #remove NAs/days off
  filter(summary != is.na(summary)) |> 
  mutate(cumulative_hits = cumsum(hits),
         cumulative_at_bats = cumsum(at_bats),
         batting_avg_season = round(cumulative_hits/cumulative_at_bats,3),
         last_ten_hits = slide_dbl(hits, sum, .before = 9, .complete = TRUE),
         last_ten_at_bats = slide_dbl(at_bats, sum, .before = 9, .complete = TRUE),
         batting_avg_last_ten = round(last_ten_hits / last_ten_at_bats, 3),
         date = as.Date(date)) |> 
  arrange(date) |> 
  slice(-(1:10))



p <- ggplot(df, aes(x = date)) +
  geom_line(aes(y = batting_avg_season, color = "Season Average"), size = 1.2) +
  geom_line(aes(y = batting_avg_last_ten, color = "Last 10 Games"), size = 1.2) +
  geom_point(aes(y = batting_avg_season, color = "Season Average", 
                 text = paste("Date:", date, 
                              "<br>Season Avg:", sprintf("%.3f", batting_avg_season))), 
             size = 1.5) +
  geom_point(aes(y = batting_avg_last_ten, color = "Last 10 Games",
                 text = paste("Date:", date, 
                              "<br>Last 10 Avg:", sprintf("%.3f", batting_avg_last_ten))), 
             size = 1.5) +
  scale_color_manual(values = c("Season Average" = "blue", "Last 10 Games" = "red")) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(
    title = "Batting Average Over Time",
    x = "Date",
    y = "Batting Average",
    color = "Average Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_x_date(date_labels = "%m/%d", date_breaks = "3 days") +
  scale_y_continuous(labels = function(x) sprintf("%.3f", x))

# Convert to interactive plotly plot
ggplotly(p, tooltip = "text")

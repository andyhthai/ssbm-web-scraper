# vs previous-year ranked -------------------------------------------------

# get_lastyear requires
table2015 <- get_table(3)

table2015$year <- 2015

temp_tables <- rbindlist(list(table2015, table2016, table2017, table2018, table2019, table2022, table2023, table2024))

temp_tables <- temp_tables |>
  mutate(startgg = case_match(player,
    "La Luna" ~ "The Moon",
    "Captain Faceroll" ~ "Faceroll",
    "Drunksloth" ~ "woof woof bark bark",
    "MikeHaze" ~ "mikehaze (but on b0xx)",
    "Ka-Master" ~ "Luigi Ka-Master",
    "FatGoku" ~ "Zoey",
    "Prince Abu" ~ "Prince F. Abu",
    "Rik" ~ "Fraggin&Laggin",
    "NMW" ~ "Nickemwit",
    "Smashdaddy" ~ "Smash Papi",
    "Cal" ~ "Essy",
    "Jakenshaken" ~ "Jake",
    "Flipsy" ~ "Darling",
    "MilkMan" ~ "ZODD-01",
    "Trulliam" ~ "HongKong97",
    "Mango" ~ "Mang0",
    "Hax" ~ "Hax$",
    "iBDW" ~ "Cody Schwab",
    "JOJI" ~ "ZODD-01",
    "Kürv" ~ "Matt, Vegas",
    "Logan" ~ "Faith",
    "Raz" ~ "Faith",
    "Suf" ~ "Android 0",
    "bobby big ballz" ~ "Chickenman400",
    "MoG" ~ "mvlvchi",
    "lint" ~ "LINT LICKA",
    "Druggedfox" ~ "Sami",
    "KirbyKaze" ~ "PSI $wagnet",
    "Santiago" ~ "Santi",
    "Trifasia" ~ "Trif",
    "Connor" ~ "Updog",
    "Infinite Numbers" ~ "Arkouda",
    "Mafia" ~ "gamerjim",
    "SmashG0D" ~ "Rishi",
    "Kage the Warrior" ~ "Kage",
    "Frootloop" ~ "Frank",
    "HomeMadeWaffles" ~ "YungWaff",
    "Milkman" ~ "ZODD-01",
    "Mojo" ~ "Uncle Mojo",
    "Drunk Sloth" ~ "woof woof bark bark",
    "KPAN" ~ "itspancaketime",
    "Nanami" ~ "Namzerra",
    "Moky" ~ "moky",
    "Vegas Matt" ~ "Matt, Vegas",
    .default = startgg
  ), .keep = "all")

get_lastyear <- function(player_code, edition) {
  tryCatch(
    {
      if (edition == 2022) {
        players <- temp_tables |> filter(year == 2019)
      } else {
        players <- temp_tables |> filter(year == edition - 1)
      }
      players <- players |> select(rank, startgg)
      playerslist <- paste(players$startgg, collapse = "|")

      firefox$navigate(paste0(player_code, "&tab=events&expand=true&t=", edition, "&offline"))
      Sys.sleep(8)
      page_source <- read_html(firefox$getPageSource() |> unlist())

      opponents <- page_source |>
        html_elements(".bg-gradient-to-r") |>
        html_elements("a") |>
        html_text2() |>
        tibble()

      names(opponents) <- "startgg"
      opponents <- opponents |> filter(grepl(playerslist, startgg))
      opponents <- left_join(opponents, players, by = "startgg")
      opponents <- opponents |> filter(!is.na(opponents$rank))


      matches_played <- nrow(opponents)

      # Wins
      opponents_wins <- page_source |>
        html_elements(".border-r-dark-neutral-bg-4.bg-gradient-to-r") |>
        html_elements("a") |>
        html_text2() |>
        tibble()
      names(opponents_wins) <- "startgg"
      opponents_wins <- opponents_wins |> filter(grepl(playerslist, startgg))
      opponents_wins <- left_join(opponents_wins, players, by = "startgg")
      opponents_wins <- opponents_wins |> filter(!is.na(opponents_wins$rank))


      wins <- nrow(opponents_wins)
      winrate <- wins / matches_played
      avg_opponents_wins <- mean(opponents_wins$rank, na.rm = TRUE)

      # Losses
      opponents_losses <- page_source |>
        html_elements(".border-l-dark-neutral-bg-4.bg-gradient-to-r") |>
        html_elements("a") |>
        html_text2() |>
        tibble()
      names(opponents_losses) <- "startgg"

      opponents_losses <- opponents_losses |> filter(grepl(playerslist, startgg))
      opponents_losses <- left_join(opponents_losses, players, by = "startgg")
      opponents_losses <- opponents_losses |> filter(!is.na(opponents_losses$rank))

      avg_opponents_losses <- mean(opponents_losses$rank, na.rm = TRUE)

      # matches and winrate against top 10

      opponents_top10 <- opponents |> filter(rank <= 10)
      wins_top10 <- opponents_wins |> filter(rank <= 10)
      winrate_top10 <- nrow(wins_top10) / nrow(opponents_top10)

      if (is.nan(winrate_top10)) {
        winrate_top10 <- NA
      }

      # matches and winrate against top 20

      opponents_top20 <- opponents |> filter(rank <= 20)

      wins_top20 <- opponents_wins |> filter(rank <= 20)

      winrate_top20 <- nrow(wins_top20) / nrow(opponents_top20)

      if (is.nan(winrate_top20)) {
        winrate_top20 <- NA
      }


      last_year_variables <- as.list(c(
        nrow(opponents_wins), avg_opponents_wins, winrate, nrow(opponents_losses),
        avg_opponents_losses, nrow(opponents_top10),
        winrate_top10, nrow(opponents_top20), winrate_top20
      ))

      return(last_year_variables)
    },
    error = function(e) {
      cat("Error for ", player_name, ": ", e$message)
      return(list(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_))
    }
  )
}

all_lastyear <- function(player_group) {
  tryCatch(
    {
      last_year_wins <- numeric(length(player_group$player))
      avg_last_year_wins <- numeric(length(player_group$player))
      last_year_winrate <- numeric(length(player_group$player))
      last_year_losses <- numeric(length(player_group$player))
      avg_last_year_losses <- numeric(length(player_group$player))
      last_year_top10_opponents <- numeric(length(player_group$player))
      last_year_winrate_top10 <- numeric(length(player_group$player))
      last_year_top20_opponents <- numeric(length(player_group$player))
      last_year_winrate_top20 <- numeric(length(player_group$player))

      for (i in seq_along(player_group$player)) {
        cat("Processing", player_group$player[i], player_group$year[i], "\n")

        attempts <- 0
        last_year_variables <- NA_real_

        while (any(is.na(last_year_variables[-c(7, 9)])) && attempts < 3) {
          if (attempts > 0) {
            cat("Got NA, retrying attempt", attempts + 1, "\n")
          }
          last_year_variables <- get_lastyear(player_group$player_code[i], player_group$year[i])
          attempts <- attempts + 1
        }

        last_year_wins[i] <- last_year_variables[[1]]
        avg_last_year_wins[i] <- last_year_variables[[2]]
        last_year_winrate[i] <- last_year_variables[[3]]
        last_year_losses[i] <- last_year_variables[[4]]
        avg_last_year_losses[i] <- last_year_variables[[5]]
        last_year_top10_opponents[i] <- last_year_variables[[6]]
        last_year_winrate_top10[i] <- last_year_variables[[7]]
        last_year_top20_opponents[i] <- last_year_variables[[8]]
        last_year_winrate_top20[i] <- last_year_variables[[9]]
      }

      player_group$last_year_wins <- last_year_wins
      player_group$avg_last_year_wins <- avg_last_year_wins
      player_group$last_year_winrate <- last_year_winrate
      player_group$last_year_losses <- last_year_losses
      player_group$avg_last_year_losses <- avg_last_year_losses
      player_group$last_year_top10_opponents <- last_year_top10_opponents
      player_group$last_year_winrate_top10 <- last_year_winrate_top10
      player_group$last_year_top20_opponents <- last_year_top20_opponents
      player_group$last_year_winrate_top20 <- last_year_winrate_top20
      return(player_group)
    },
    error = function(e) {
      cat("Unsuccessful web scrape", e$message, "\n")
    }
  )
}

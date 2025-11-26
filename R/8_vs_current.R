# ranked (current year) ---------------------------------------------------


get_ranked_variables <- function(player_code, edition) {
  tryCatch(
    {
      firefox$navigate(paste0(player_code, "&tab=events&offline&t=", edition, "&expand=true&t100"))

      # add this give the browser enough time to load
      Sys.sleep(8)

      # We'll collect the html of the newly loaded page
      page_source <- read_html(firefox$getPageSource() |> unlist())

      matches <- page_source |>
        html_elements(".bg-gradient-to-r:not(.hidden)") |>
        html_elements(".px-0\\.5") |>
        html_text2()

      matches_played <- length(matches)

      wins <- page_source |>
        html_elements(".border-r-dark-neutral-bg-4.bg-gradient-to-r:not(.hidden)") |>
        html_elements(".px-0\\.5") |>
        html_text2()

      wins <- as.numeric(gsub("#", "", wins))

      ranked_wins <- length(wins)

      avg_rank_win <- mean(wins)


      losses <- page_source |>
        html_elements(".border-l-dark-neutral-bg-4.bg-gradient-to-r:not(.hidden)") |>
        html_elements(".px-0\\.5") |>
        html_text2()

      losses <- as.numeric(gsub("#", "", losses))

      avg_rank_loss <- mean(losses)

      ranked_winrate <- ranked_wins / matches_played

      variables <- as.list(c(ranked_winrate, ranked_wins, avg_rank_win, avg_rank_loss))

      return(variables)
    },
    error = function(e) {
      message("Error for player ", player_name, ": ", e$message)
      return(list(NA_real_, NA_real_, NA_real_, NA_real_))
    }
  )
}

all_ranked_variables <- function(player_group) {
  tryCatch(
    {
      top100_winrate <- numeric(length(player_group$player))
      ranked_wins <- numeric(length(player_group$player))
      avg_rank_win <- numeric(length(player_group$player))
      avg_rank_loss <- numeric(length(player_group$player))

      for (i in seq_along(player_group$player)) {
        cat("Processing", player_group$player[i], player_group$year[i], "\n")

        attempts <- 0
        ranked_variables <- NA_real_

        while (any(is.na(ranked_variables)) && attempts < 3) {
          if (attempts > 0) {
            cat("Got NA, retrying attempt", attempts + 1, "\n")
          }
          ranked_variables <- get_ranked_variables(player_group$player_code[i], player_group$year[i])
          attempts <- attempts + 1
        }

        top100_winrate[i] <- ranked_variables[[1]]
        ranked_wins[i] <- ranked_variables[[2]]
        avg_rank_win[i] <- ranked_variables[[3]]
        avg_rank_loss[i] <- ranked_variables[[4]]
      }

      player_group$top100_winrate <- top100_winrate
      player_group$ranked_wins <- ranked_wins
      player_group$avg_rank_win <- avg_rank_win
      player_group$avg_rank_loss <- avg_rank_loss
      return(player_group)
    },
    error = function(e) {
      cat("Unsuccessful web scrape", e$message, "\n")
    }
  )
}

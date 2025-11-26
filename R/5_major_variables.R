# majors ------------------------------------------------------------------



get_major_variables <- function(player_code, edition) {
  tryCatch(
    {
      firefox$navigate(paste0(player_code, "&tab=events&offline&t=", edition, "&expand=true&majors"))
      Sys.sleep(8)
      # Get the page source, check to see if it has the correct amount of tournaments for Zain (13)
      page_source <- read_html(firefox$getPageSource() |> unlist())

      tournaments <- page_source |> html_elements(".w-full.p-1\\.5:not(.hidden)")

      tournament_names <- tournaments |> html_text2()

      filtered_tournaments <- tournaments[!grepl("Eggdog Invitational M\nMelee Singles\nAug 30-2", tournament_names, fixed = TRUE)]

      # Get tournaments where placement is 1
      major_placings <- as.numeric(filtered_tournaments |> html_elements(".text-lg.md\\:text-2xl") |> html_text2())

      avg_major_placement <- mean(major_placings)

      major_wins <- length(major_placings[major_placings == 1])

      major_attendance <- length(major_placings)

      major_top8 <- length(major_placings[major_placings < 8])

      major_variables <- as.list(c(avg_major_placement, major_top8, major_wins, major_attendance))

      return(major_variables)
    },
    error = function(e) {
      message("Error for", player_name, ": ", e$message)
      return(list(NA_real_, NA_real_, NA_real_, NA_real_))
    }
  )
}

# Test
get_major_variables("https://www.supermajor.gg/melee/player/Zain?id=S6126", 2024)

all_major_variables <- function(player_group) {
  tryCatch(
    {
      avg_major_placement <- numeric(length(player_group$player))
      major_top8 <- numeric(length(player_group$player))
      major_wins <- numeric(length(player_group$player))
      major_attendance <- numeric(length(player_group$player))

      for (i in seq_along(player_group$player)) {
        cat("Processing", player_group$player[i], player_group$year[i], "\n")

        attempts <- 0
        major_variables <- NA_real_

        while (any(is.na(major_variables)) && attempts < 3) {
          if (attempts > 0) {
            cat("Got NA, retrying attempt", attempts + 1, "\n")
          }
          major_variables <- get_major_variables(player_group$player_code[i], player_group$year[i])
          attempts <- attempts + 1
        }

        avg_major_placement[i] <- major_variables[[1]]
        major_top8[i] <- major_variables[[2]]
        major_wins[i] <- major_variables[[3]]
        major_attendance[i] <- major_variables[[4]]
      }

      player_group$avg_major_placement <- avg_major_placement
      player_group$major_top8 <- major_top8
      player_group$major_wins <- major_wins
      player_group$major_attendance <- major_attendance
      return(player_group)
    },
    error = function(e) {
      cat("Unsuccessful web scrape", e$message, "\n")
    }
  )
}

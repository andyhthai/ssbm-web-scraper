# supermajors prep -------------------------------------------------------------

supermajor2016 <- c("GENESIS 3", "Get On My Level 2016", "EVO 2016", "The Big House 6", "UGC Smash Open")
supermajor2016 <- paste(supermajor2016, collapse = "|")

supermajor2017 <- c("GENESIS 4", "Splash 3", "EVO 2017", "Super Smash Con 2017", "Shine 2017", "The Big House 7")
supermajor2017 <- paste(supermajor2017, collapse = "|")

supermajor2018 <- c("GENESIS 5", "Splash 4", "EVO 2018", "Super Smash Con 2018", "The Big House 8")
supermajor2018 <- paste(supermajor2018, collapse = "|")

supermajor2019 <- c("GENESIS 6", "Get On My Level 2019", "Splash 5", "Super Smash Con 2019", "The Big House 9")
supermajor2019 <- paste(supermajor2019, collapse = "|")

supermajor2022 <- c("GENESIS 8", "The Big House 10", "Ludwig")
supermajor2022 <- paste(supermajor2022, collapse = "|")

supermajor2023 <- c("GENESIS 9", "Battle of BC 5", "Get On My Level 2023", "The Big House 11")
supermajor2023 <- paste(supermajor2023, collapse = "|")

supermajor2024 <- c("GENESIS X", "Get On My Level X", "Tipped Off 15", "Don't Park on the Grass 2024")
supermajor2024 <- paste(supermajor2024, collapse = "|")

supermajors <- as.list(c(supermajor2016, supermajor2017, supermajor2018, supermajor2019, supermajor2022, supermajor2023, supermajor2024))

illegal <- c("Sound", "Pre", "Ladder", "Local", "Rollers", "Reevo", "Lot", "Warm", "Showdown", "Guildhouse", "LCQ", "Amateur", "Foundry")
illegal <- paste(illegal, collapse = "|")

# supermajors -------------------------------------------------------------

get_supermajor_variables <- function(player_code, edition, list_edition) {
  tryCatch(
    {
      firefox$navigate(paste0(player_code, "&tab=events&t=", edition, "&offline"))

      Sys.sleep(10)

      page_source <- read_html(firefox$getPageSource() |> unlist())

      tournaments <- page_source |> html_elements(".w-full.p-1\\.5:not(.hidden)")

      tournament_names <- tournaments |> html_text2()

      filtered_tournaments <- tournaments[grepl(supermajors[[list_edition]], tournament_names, ignore.case = TRUE)]

      filtered_tournaments <- filtered_tournaments[grepl("Melee", filtered_tournaments, ignore.case = TRUE)]

      filtered_tournaments <- filtered_tournaments[!grepl(illegal, filtered_tournaments, ignore.case = TRUE)]

      supermajor_placings <- as.numeric(filtered_tournaments |> html_elements(".text-lg.md\\:text-2xl") |> html_text2())

      avg_supermajor_placement <- mean(supermajor_placings)

      supermajors_won <- length(supermajor_placings[supermajor_placings == 1])

      supermajor_attendance <- length(supermajor_placings)

      supermajor_top8 <- length(supermajor_placings[supermajor_placings < 8])

      supermajor_variables <- as.list(c(avg_supermajor_placement, supermajor_top8, supermajors_won, supermajor_attendance))

      return(supermajor_variables)
    },
    error = function(e) {
      cat("Error for ", player_name, ": ", e$message)
      return(list(NA_real_, NA_real_, NA_real_, NA_real_))
    }
  )
}

all_supermajor_variables <- function(player_group) {
  tryCatch(
    {
      avg_supermajor_placement <- numeric(length(player_group$player))
      supermajor_top8 <- numeric(length(player_group$player))
      supermajors_won <- numeric(length(player_group$player))
      supermajor_attendance <- numeric(length(player_group$player))

      for (i in seq_along(player_group$player)) {
        cat("Processing", player_group$player[i], player_group$year[i], "\n")

        attempts <- 0
        supermajor_variables <- NA_real_

        while (any(is.na(supermajor_variables)) && attempts < 3) {
          if (attempts > 0) {
            cat("Got NA, retrying attempt", attempts + 1, "\n")
          }
          supermajor_variables <- get_supermajor_variables(player_group$player_code[i], player_group$year[i], player_group$list_edition[i])
          attempts <- attempts + 1
        }

        avg_supermajor_placement[i] <- supermajor_variables[[1]]
        supermajor_top8[i] <- supermajor_variables[[2]]
        supermajors_won[i] <- supermajor_variables[[3]]
        supermajor_attendance[i] <- supermajor_variables[[4]]
      }

      player_group$avg_supermajor_placement <- avg_supermajor_placement
      player_group$supermajor_top8 <- supermajor_top8
      player_group$supermajors_won <- supermajors_won
      player_group$supermajor_attendance <- supermajor_attendance
      return(player_group)
    },
    error = function(e) {
      cat("Unsuccessful web scrape", e$message, "\n")
    }
  )
}

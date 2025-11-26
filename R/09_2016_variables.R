# 2016 majors ----------------------------------------------------------

pattern <- "Don't Park on the Grass|UGC Smash Open|DreamHack Winter 2016|Smash Summit 3|Canada Cup 2016|The Big House 6|Shine 2016|Super Smash Con 2016|EVO 2016|WTFox 2|CEO 2016|Smash 'N' Splash 2|Get On My Level 2016|DreamHack Austin 2016|Enthusiast Gaming Live Expo|Smash Summit 2|Pound 2016|Battle of the Five Gods|PAX Arena|GENESIS 3"

get_2016_major_variables <- function(player_name) {
  tryCatch(
    {
      firefox$navigate(paste0("https://liquipedia.net/smash/", player_name, "/Results"))

      Sys.sleep(5)
      page_source <- firefox$getPageSource() |>
        unlist() |>
        read_html()

      for (n in 1:5) {
        tab_css <- paste0(".tab", n)

        # If tab is not found, it must be a player that only plays melee
        tab <- tryCatch(
          {
            firefox$findElement(using = "css selector", tab_css)
          },
          error = function(e) {
            cat("Tab", n, "not found\n")
            return(NULL)
          }
        )
        if (is.null(tab)) {
          table_index <- page_source |>
            html_elements(".wikitable-striped.sortable") |>
            html_table()

          player_results <- table_index[[1]] |>
            select(Date, Place, Event)

          names(player_results) <- tolower(names(player_results))

          player_results <- player_results |>
            filter(grepl("2016", date)) |>
            filter(grepl(pattern, event))

          player_results <- player_results |> mutate(
            place = case_when(
              grepl("3165", place) ~ "65",
              grepl("313", place) ~ "13",
              grepl("14th", place) ~ "4",
              grepl("11st", place) ~ "1",
              grepl("13rd", place) ~ "3",
              grepl("12nd", place) ~ "2",
              grepl("1533", place) ~ "33",
              grepl("1549", place) ~ "49",
              grepl("192nd", place) ~ "129",
              grepl("256th", place) ~ "193",
              grepl("E15", place) ~ "5",
              grepl("Q717", place) ~ "17",
              grepl("I39", place) ~ "9",
              grepl("G17", place) ~ "7",
              grepl("R725", place) ~ "25",
              grepl("K411", place) ~ "11",
              grepl("P416", place) ~ "16",
              grepl("I19th", place) ~ "9",
              grepl("S3197", place) ~ "97",
            )
          )

          player_results$place <- as.numeric(player_results$place)

          avg_major_placement <- mean(player_results$place)

          major_wins <- length(player_results$place[player_results$place == 1])

          major_top8 <- length(player_results$place[player_results$place <= 7])

          major_attendance <- length(player_results$place)

          major_variables <- as.list(c(avg_major_placement, major_wins, major_top8, major_attendance))

          check_table <- TRUE

          return(major_variables)
        } else {
          check_tab <- page_source |>
            html_element(tab_css) |>
            html_text2()

          check_table <- FALSE

          if (check_tab == "Melee") {
            check_table <- TRUE
            break
          } else {
            cat("Melee not found in table", n, "\n")
          }
        }
      }

      if (!check_table) {
        cat("Melee table not found")
        return(list(NA_real_, NA_real_, NA_real_, NA_real_))
      }

      table_index <- page_source |>
        html_elements(".wikitable-striped.sortable") |>
        html_table()

      player_results <- table_index[[n]] |>
        select(Date, Place, Event)

      names(player_results) <- tolower(names(player_results))

      player_results <- player_results |>
        filter(grepl("2016", date)) |>
        filter(grepl(pattern, event))

      player_results <- player_results |> mutate(
        place = case_when(
          grepl("3165", place) ~ "65",
          grepl("313", place) ~ "13",
          grepl("14th", place) ~ "4",
          grepl("11st", place) ~ "1",
          grepl("13rd", place) ~ "3",
          grepl("12nd", place) ~ "2",
          grepl("1533", place) ~ "33",
          grepl("1549", place) ~ "49",
          grepl("192nd", place) ~ "129",
          grepl("256th", place) ~ "193",
          grepl("E15", place) ~ "5",
          grepl("Q717", place) ~ "17",
          grepl("I39", place) ~ "9",
          grepl("G17", place) ~ "7",
          grepl("R725", place) ~ "25",
          grepl("K411", place) ~ "11",
          grepl("P416", place) ~ "16",
          grepl("I19th", place) ~ "9",
          grepl("S3197", place) ~ "97",
        )
      )

      player_results$place <- as.numeric(player_results$place)

      avg_major_placement <- mean(player_results$place)

      major_wins <- length(player_results$place[player_results$place == 1])

      major_top8 <- length(player_results$place[player_results$place <= 7])

      major_attendance <- length(player_results$place)

      major_variables <- as.list(c(avg_major_placement, major_wins, major_top8, major_attendance))

      return(major_variables)
    },
    error = function(e) {
      message("Error for", player_name, ": ", e$message)
      return((list(NA_real_, NA_real_, NA_real_, NA_real_)))
    }
  )
}

all_2016_major_variables <- function(player_group) {
  tryCatch(
    {
      avg_major_placement <- numeric(length(player_group$player))
      major_wins <- numeric(length(player_group$player))
      major_top8 <- numeric(length(player_group$player))
      major_attendance <- numeric(length(player_group$player))


      for (i in seq_along(player_group$player)) {
        cat("Processing", player_group$player[i], "\n")

        attempts <- 0
        major_variables <- NA_real_

        while (any(is.na(major_variables)) && attempts < 3) {
          if (attempts > 0) {
            cat("Got NA, retrying attempt", attempts + 1, "\n")
          }
          major_variables <- get_2016_major_variables(player_group$player[i])
          attempts <- attempts + 1
        }


        avg_major_placement[i] <- major_variables[[1]]
        major_wins[i] <- major_variables[[2]]
        major_top8[i] <- major_variables[[3]]
        major_attendance[i] <- major_variables[[4]]
      }

      player_group$avg_major_placement <- avg_major_placement
      player_group$major_wins <- major_wins
      player_group$major_top8 <- major_top8
      player_group$major_attendance <- major_attendance
      return(player_group)
    },
    error = function(e) {
      cat("Unsuccessful web scrape", e$message)
    }
  )
}

# 2016 supermajors --------------------------------------------------------

get_2016_supermajor_variables <- function(player_name) {
  tryCatch(
    {
      firefox$navigate(paste0("https://liquipedia.net/smash/", player_name, "/Results"))

      Sys.sleep(5)
      page_source <- firefox$getPageSource() |>
        unlist() |>
        read_html()

      for (n in 1:5) {
        tab_css <- paste0(".tab", n)

        # If tab is not found, it must be a player that only plays melee
        tab <- tryCatch(
          {
            firefox$findElement(using = "css selector", tab_css)
          },
          error = function(e) {
            cat("Tab", n, "not found\n")
            return(NULL)
          }
        )
        if (is.null(tab)) {
          table_index <- page_source |>
            html_elements(".wikitable-striped.sortable") |>
            html_table()

          player_results <- table_index[[1]] |>
            select(Date, Place, Event)

          names(player_results) <- tolower(names(player_results))

          player_results <- player_results |>
            filter(grepl("2016", date)) |>
            filter(grepl(supermajors[[1]], event))

          player_results <- player_results |> mutate(
            place = case_when(
              grepl("3165", place) ~ "65",
              grepl("313", place) ~ "13",
              grepl("14th", place) ~ "4",
              grepl("11st", place) ~ "1",
              grepl("13rd", place) ~ "3",
              grepl("12nd", place) ~ "2",
              grepl("1533", place) ~ "33",
              grepl("1549", place) ~ "49",
              grepl("192nd", place) ~ "129",
              grepl("256th", place) ~ "193",
              grepl("E15", place) ~ "5",
              grepl("Q717", place) ~ "17",
              grepl("I39", place) ~ "9",
              grepl("G17", place) ~ "7",
              grepl("R725", place) ~ "25",
              grepl("K411", place) ~ "11",
              grepl("P416", place) ~ "16",
              grepl("I19th", place) ~ "9",
              grepl("S3197", place) ~ "97",
            )
          )

          player_results$place <- as.numeric(player_results$place)

          avg_supermajor_placement <- mean(player_results$place)

          supermajor_wins <- length(player_results$place[player_results$place == 1])

          supermajor_top8 <- length(player_results$place[player_results$place <= 7])

          supermajor_attendance <- length(player_results$place)

          supermajor_variables <- as.list(c(avg_supermajor_placement, supermajor_top8, supermajor_wins, supermajor_attendance))

          check_table <- TRUE

          return(supermajor_variables)
        } else {
          check_tab <- page_source |>
            html_element(tab_css) |>
            html_text2()

          check_table <- FALSE

          if (check_tab == "Melee") {
            check_table <- TRUE
            break
          } else {
            cat("Melee not found in table", n, "\n")
          }
        }
      }

      if (!check_table) {
        cat("Melee table not found")
        return(list(NA_real_, NA_real_, NA_real_))
      }

      table_index <- page_source |>
        html_elements(".wikitable-striped.sortable") |>
        html_table()

      player_results <- table_index[[n]] |>
        select(Date, Place, Event)

      names(player_results) <- tolower(names(player_results))

      player_results <- player_results |>
        filter(grepl("2016", date)) |>
        filter(grepl(supermajors[[1]], event))

      player_results <- player_results |> mutate(
        place = case_when(
          grepl("3165", place) ~ "65",
          grepl("313", place) ~ "13",
          grepl("14th", place) ~ "4",
          grepl("11st", place) ~ "1",
          grepl("13rd", place) ~ "3",
          grepl("12nd", place) ~ "2",
          grepl("1533", place) ~ "33",
          grepl("1549", place) ~ "49",
          grepl("192nd", place) ~ "129",
          grepl("256th", place) ~ "193",
          grepl("E15", place) ~ "5",
          grepl("Q717", place) ~ "17",
          grepl("I39", place) ~ "9",
          grepl("G17", place) ~ "7",
          grepl("R725", place) ~ "25",
          grepl("K411", place) ~ "11",
          grepl("P416", place) ~ "16",
          grepl("I19th", place) ~ "9",
          grepl("S3197", place) ~ "97",
        )
      )

      player_results$place <- as.numeric(player_results$place)

      avg_supermajor_placement <- mean(player_results$place)

      supermajor_wins <- length(player_results$place[player_results$place == 1])

      supermajor_top8 <- length(player_results$place[player_results$place <= 7])

      supermajor_attendance <- length(player_results$place)

      supermajor_variables <- as.list(c(avg_supermajor_placement, supermajor_top8, supermajor_wins, supermajor_attendance))

      return(supermajor_variables)
    },
    error = function(e) {
      message("Error for", player_name, ": ", e$message)
      return((list(NA_real_, NA_real_, NA_real_, NA_real_)))
    }
  )
}

all_2016_supermajor_variables <- function(player_group) {
  tryCatch(
    {
      avg_supermajor_placement <- numeric(length(player_group$player))
      supermajor_wins <- numeric(length(player_group$player))
      supermajor_top8 <- numeric(length(player_group$player))
      supermajor_attendance <- numeric(length(player_group$player))


      for (i in seq_along(player_group$player)) {
        cat("Processing", player_group$player[i], "\n")

        attempts <- 0
        supermajor_variables <- NA_real_

        while (any(is.na(supermajor_variables)) && attempts < 3) {
          if (attempts > 0) {
            cat("Got NA, retrying attempt", attempts + 1, "\n")
          }
          supermajor_variables <- get_2016_supermajor_variables(player_group$player[i])
          attempts <- attempts + 1
        }


        avg_supermajor_placement[i] <- supermajor_variables[[1]]
        supermajor_top8[i] <- supermajor_variables[[2]]
        supermajor_wins[i] <- supermajor_variables[[3]]
        supermajor_attendance[i] <- supermajor_variables[[4]]
      }

      player_group$avg_supermajor_placement <- avg_supermajor_placement
      player_group$supermajors_won <- supermajor_wins
      player_group$supermajor_top8 <- supermajor_top8
      player_group$supermajor_attendance <- supermajor_attendance
      return(player_group)
    },
    error = function(e) {
      cat("Unsuccessful web scrape", e$message)
    }
  )
}

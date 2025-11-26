# attendance, winrate, upset factor ---------------------------------------

all_awu <- function(player_group) {
  tryCatch(
    {
      test_inner <- function(player_code, edition) {
        tryCatch(
          {
            firefox$navigate(paste0(player_code, "&tab=events&offline&t=", edition, "&expand=true"))

            # add this give the browser enough time to load
            Sys.sleep(8)

            # We'll collect the html of the newly loaded page
            page_source <- read_html(firefox$getPageSource() |> unlist())

            # Get the number of tournaments attended in a year
            event_number <- page_source |>
              html_element("div.mr-1") |>
              html_text2()

            # This is how many tournaments a player has attended that year
            tournaments <- str_extract(event_number, "\\d+")

            # We'll get the names of the tournaments attended in only the year we specified so we can
            # check if Smash the Record is there. Some weird matches occurred here, so we'll
            # exclude it from data collecting.
            tournament_names <- page_source |> html_elements(".w-full.p-1\\.5.pr-0.mb-4:not(.hidden)")


            # "In tournament_names, give me everything that doesn't have "Smash The Record" in it"
            filtered_tournaments <- tournament_names[!grepl("Smash The Record|Smashtherecord", tournament_names |> html_text2(), ignore.case = TRUE)]

            # The amount of losses are taken from filtered_tournaments
            losses <- filtered_tournaments |>
              html_elements(".border-l-dark-neutral-bg-4.bg-gradient-to-r") |>
              html_elements(".flex.gap-2.pl-1.items-center.justify-between") |>
              html_elements(".bg-black.text-white") |>
              html_text2()

            upsets_loss <- as.numeric(filtered_tournaments |> html_elements(".border-l-dark-neutral-bg-4.bg-gradient-to-r") |>
              html_elements(".circle.rounded-full") |>
              html_text2())

            # This compares upsets and scores, if "L" is in scores, it will turn the corresponding
            # number in upsets into NA. We do this because there was one case where Armada lost to
            # Toph at Dreamhack Austin/Atlanta(?) because of a DQ, but the website had the element of
            # an actual match. Normally, DQs wouldn't be counted because they have a separate element,
            # but it looks like the website made a mistake.
            upsets_loss[losses == "L"] <- NA

            # The above code could be removed, as it is probably a rare case that a DQ was coded
            # as a loss. Armada's inaccurate worst_uf for that year could simply be replaced.
            # It could be possible that a tournament that reported the outcome of a legitimate match
            # but not the score. There could be a big upset ignored if we leave the code as is.

            upsets_loss <- max(upsets_loss, na.rm = TRUE)

            # losses was previously a list of scores, now make it its length
            losses <- as.numeric(length(na.omit(losses)))

            # We have it as totalmatches_score because we used to remove matches
            # where scores are reported as Ws or Ls so it would ignore potential DQs
            # that are inappropriately identified as an actual match.
            # We decided to remove that since the element .bg-gradient-to-r is already
            # unique to actual matches, and the chances of DQ being coded as an
            # actual match seems like a rare mistake.
            totalmatches_score <- filtered_tournaments |>
              html_elements(".bg-gradient-to-r") |>
              html_elements(".flex.gap-2.pl-1.items-center.justify-between") |>
              html_elements(".text-black") |>
              html_text2()

            totalmatches <- as.numeric(length(na.omit(totalmatches_score)))

            wins <- totalmatches - losses

            winrate <- wins / totalmatches

            upsets_win <- as.numeric(filtered_tournaments |> html_elements(".border-r-dark-neutral-bg-4.bg-gradient-to-r") |>
              html_elements(".circle.rounded-full") |> html_text2())

            upsets_win <- max(upsets_win, na.rm = TRUE)

            variables <- as.numeric(list(tournaments, upsets_loss, upsets_win, winrate))


            return(variables)
          },
          error = function(e) {
            message("Error for player ", player_name, ": ", e$message)
            return(list(NA_real_, NA_real_, NA_real_, NA_real_))
          }
        )
      }

      attendance <- numeric(length(player_group$startgg))
      worst_uf <- numeric(length(player_group$startgg))
      best_uf <- numeric(length(player_group$startgg))
      winrates <- numeric(length(player_group$startgg))


      for (i in seq_along(player_group$startgg)) {
        cat("Processing", player_group$startgg[i], player_group$year[i], "\n")

        attempts <- 0
        test_results <- NA_real_

        while ((any(is.na(test_results)) || any(test_results == 0) || is.infinite(test_results[2])) && attempts < 3) {
          if (attempts > 0) {
            cat("Got NA, 0, or -Inf, retrying", attempts + 1, "\n")
            Sys.sleep(2)
          }

          test_results <- test_inner(player_group$player_code[i], player_group$year[i])
          attempts <- attempts + 1
        }

        attendance[i] <- test_results[1]
        worst_uf[i] <- test_results[2]
        best_uf[i] <- test_results[3]
        winrates[i] <- test_results[4]
      }


      player_group$attendance <- attendance
      player_group$worst_uf <- worst_uf
      player_group$best_uf <- best_uf
      player_group$winrate <- winrates
      return(player_group)
    },
    error = function(e) {
      cat("Unsuccessful web scrape", e$message)
    }
  )
}
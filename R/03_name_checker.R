# Check which players need to have their urls manually entered ------------

firefox$open()

# Check if startgg is valid on supermajor.gg, returns TRUE or FALSE, and then retrieves URL
checker <- function(df) {
  tryCatch(
    {
      valid <- logical(length(df$startgg))

      player_code <- character(length(df$startgg))

      for (i in seq_along(df$startgg)) {
        valid_player <- FALSE


        for (n in 1:5) {
          inner_test <- tryCatch(
            {
              firefox$navigate("https://supermajor.gg/")
              cat("Navigating to", df$startgg[i], "\n")
              Sys.sleep(5)

              # Find search bar
              search_bar <- firefox$findElement(using = "css selector", "input.w-full")
              search_bar$clickElement()
              Sys.sleep(1)
              search_bar$sendKeysToElement(list(df$startgg[i]))

              # Find and click the icon for melee
              melee_icon <- firefox$findElement(using = "css selector", ".object-center")
              melee_icon$clickElement()
              Sys.sleep(3)

              result_css <- paste0("div.md\\:min-w-\\[400px\\]:nth-child(", n, ")")
              test_result <- firefox$findElement(using = "css selector", result_css)
              test_result$clickElement()
              Sys.sleep(5)

              # Check to see if player is ranked
              player_source <- firefox$getPageSource() |>
                unlist() |>
                read_html()

              ranked <- player_source |>
                html_element("span.font-inter:nth-child(1)") |>
                html_text2()

              # Check to see if it's the correct player name
              true_name <- player_source |>
                html_elements(".text-center.z-10") |>
                html_text2()

              if (ranked == "Rankings" & any(grepl(tolower(df$startgg[i]), tolower(true_name), fixed = TRUE))) {
                cat("Found player at result", n, "\n")
                TRUE
              } else {
                cat("Not ranked at result", n, "\n")
                FALSE
              }
            },
            error = function(e) {
              cat("Error at result", n, "for ", df$startgg[i], "\n")
              FALSE
            }
          )
          if (inner_test) {
            valid_player <- TRUE
            code <- firefox$getCurrentUrl() |> unlist()
            break
          }
        }

        valid[i] <- valid_player
        player_code[i] <- code
      }
      df$real <- valid
      df$player_code <- player_code
      return(df)
    },
    error = function(e) {
      message("Error for ", df$startgg[i], ": ", e$message, "\n")
      return(NA_real_)
    }
  )
}

unique_players <- ssbmtables |> count(startgg, player)

# Before you run the checker, replace values in startgg columns with this code:
# These people have startgg tags that are different from the player tags listed
# on the rankings as of November 2025.

unique_players <- unique_players |>
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




unique_players <- checker(unique_players)

# If you get FALSE for column "real", you have to manually check and enter player's real startgg and
# url. This happens if a player changes their account name on startgg.

# 4% is guaranteed to receive FALSE for real because their name doesn't show up
# on the first five in supermajor.gg's search bar, so we'll change his manually as an example

# Now the real thing

unique_players[3, 5] <- "https://www.supermajor.gg/melee/player/4%25?id=S6422"
unique_players[3, 4] <- TRUE

ssbmtables <- left_join(ssbmtables |> select(!startgg), unique_players |> select(player, startgg, player_code), by = "player")

ssbmtables <- ssbmtables |> relocate(startgg, .after = player)

ssbmtables$id <- 1:nrow(ssbmtables)

ssbmtables <- ssbmtables |> relocate(id, .before = rank)

ssbmtables <- ssbmtables |> mutate(list_edition = case_match(
  year,
  2016 ~ 1,
  2017 ~ 2,
  2018 ~ 3,
  2019 ~ 4,
  2022 ~ 5,
  2023 ~ 6,
  2024 ~ 7
))

# Some more data cleaning which will matter when grabbing variables for 2016
ssbmtables <- ssbmtables |> mutate(
  player = case_match(
    player,
    "Mango" ~ "Mang0",
    .default = player
  ),
  .keep = "all"
)

ssbmtables2 <- ssbmtables |> filter(year != 2016)
tables2016 <- ssbmtables |> filter(year == 2016)

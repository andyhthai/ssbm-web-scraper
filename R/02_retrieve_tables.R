# Retrieve tables from liquipedia -----------------------------------------


smashhtml <- read_html("https://liquipedia.net/smash/SSBMRank")

tables <- smashhtml |> html_elements("table.sortable")

filtered_tables <- tables[grepl("Rank|MPGR", tables |> html_text2())]

filtered_tables <- tables[!grepl("Summer", filtered_tables |> html_text2())]

tables <- filtered_tables |> html_table()

# Possible values are 1-10 (2013 = 1, 2024 = 10) for table_index as of Nov 2025.
get_table <- function(table_index) {
  # Extract the tables from html
  tableshtml <- smashhtml |> html_elements("table.sortable")

  filtered_tables <- tableshtml[grepl("Rank|MPGR", tableshtml |> html_text2())]

  filtered_tables <- tableshtml[!grepl("Summer", tableshtml |> html_text2())]

  tables <- filtered_tables |> html_table()

  # Get one table of your choosing
  table <- tables[[table_index]]

  # Make first row of that table to names
  table <- table |> row_to_names(row_number = 1)

  # Make the first column name "rank"
  colnames(table)[1] <- "rank"

  # Select only these columns if using 2013
  if (table_index == 1) {
    table <- table |> select(rank, Player, Score)
  }

  # Select only these columns for all else. Turn NEW into NA for ±
  else {
    table <- table |> select(rank, Player, Score, `±`)

    table <- table |> mutate(`±` = case_when(
      `±` == "9000NEW" ~ NA,
      .default = as.numeric(`±`)
    ))
  }

  # Get the mains
  mains <- filtered_tables[[table_index]] |>
    html_elements("td:nth-child(3) > img:nth-child(1)") |>
    html_attr("alt")

  table <- table |> mutate(main = mains)

  table <- clean_names(table)

  table$startgg <- table$player

  table <- table |> relocate(startgg, .after = player)

  return(table)
}

# Example:

table2016 <- get_table(4)
table2017 <- get_table(5)
table2018 <- get_table(6)
table2019 <- get_table(7)
table2022 <- get_table(8)
table2023 <- get_table(9)
table2024 <- get_table(10)

table2016$year <- 2016
table2017$year <- 2017
table2018$year <- 2018
table2019$year <- 2019
table2022$year <- 2022
table2023$year <- 2023
table2024$year <- 2024

ssbmtables <- rbindlist(list(table2016, table2017, table2018, table2019, table2022, table2023, table2024))

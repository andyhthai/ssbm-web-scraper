# Retrieve tables from liquipedia -----------------------------------------


smashhtml <- read_html("https://liquipedia.net/smash/SSBMRank")

# Enter the year you want for table_year
get_table <- function(table_year) {
  # Extract the tables from html
  tables <- smashhtml |> html_elements("table.sortable")

  filtered_tables <- tables[grepl("Rank|MPGR", tables |> html_text2())]

  filtered_tables <- filtered_tables[!grepl("Summer", filtered_tables |> html_text2())]

  if (table_year %in% c("2018", "2019")) {
    extracted_table_html <- filtered_tables[grepl(paste("MPGR", table_year), filtered_tables |> html_text2())]
  } else {
    extracted_table_html <- filtered_tables[grepl(paste("SSBMRank", table_year), filtered_tables |> html_text2())]
  }

  extracted_table <- extracted_table_html |> html_table()

  extracted_table <- extracted_table[[1]]

  # Make first row of that table to names
  extracted_table <- extracted_table |> row_to_names(row_number = 1)

  # Make the first column name "rank"
  colnames(extracted_table)[1] <- "rank"

  # Select only these columns if using 2013
  if (table_year == "2013") {
    extracted_table <- extracted_table |> select(rank, Player, Score)
  }

  # Select only these columns for all else. Turn NEW into NA for ±
  else {
    extracted_table <- extracted_table |> select(rank, Player, Score, `±`)

    extracted_table <- extracted_table |> mutate(`±` = case_when(
      `±` == "9000NEW" ~ NA,
      .default = as.numeric(`±`)
    ))
  }

  # Get the mains
  mains <- extracted_table_html |>
    html_elements("td:nth-child(3) > img:nth-child(1)") |>
    html_attr("alt")

  extracted_table <- extracted_table |> mutate(main = mains)

  extracted_table <- clean_names(extracted_table)

  extracted_table$startgg <- extracted_table$player

  extracted_table <- extracted_table |> relocate(startgg, .after = player)

  extracted_table$year <- table_year

  return(extracted_table)
}
# Example:

table2016 <- get_table("2016")
table2017 <- get_table("2017")
table2018 <- get_table("2018")
table2019 <- get_table("2019")
table2022 <- get_table("2022")
table2023 <- get_table("2023")
table2024 <- get_table("2024")
table2025 <- get_table("2025")


ssbmtables <- rbindlist(list(table2016, table2017, table2018, table2019, table2022, table2023, table2024))

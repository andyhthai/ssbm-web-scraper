# making the dataset ------------------------------------------------------

tables2016 <- all_awu(tables2016)
tables2016 <- all_lastyear(tables2016)
tables2016 <- all_ranked_variables(tables2016)
tables2016 <- all_2016_major_variables(tables2016)
tables2016 <- all_2016_supermajor_variables(tables2016)



ssbmtables2 <- all_awu(ssbmtables2)
ssbmtables2 <- all_lastyear(ssbmtables2)
ssbmtables2 <- all_ranked_variables(ssbmtables2)
ssbmtables2 <- all_major_variables(ssbmtables2)
ssbmtables2 <- all_supermajor_variables(ssbmtables2)

ssbmtables3 <- rbind(ssbmtables2, tables2016)

ssbmtables3 <- ssbmtables3 |> rename(change = x)

ssbmtables3 <- ssbmtables3 |> arrange(year, rank)

# Except for the column "change", NA values typically mean that the player has the wrong
# player_code. This is most likely because a player changed their startgg handle.

# It could also mean that there was trouble accessing a liquidpedia page if
# their year is 2016. For example: Javi

# NA values will have to be changed through manual data cleaning

# NaN values in columns with "avg" means that getting an average is impossible
# because the player didn't attend at least 1 major/supermajor or didn't face
# against at least 1 opponent of the specific criteria.
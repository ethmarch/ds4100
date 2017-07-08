# this is a script to scrape soccer league tables from http://www.soccerstats.com
# the user can specify the country and tier of the league table they wish to receive

library(rvest)


## Function: scrapeTable
###### scrapes the league table of the given league from www.soccerstats.com and inserts it into a data frame
## Args:
###### country - the country of the desired league
###### tier - the tier number of the desired leauge (ex. for England, tier=1 gives you the table for the Premier League)
## Returns: a the league table as a dataframe
scrapeTable <- function(country, tier) {
  base_url <- "http://www.soccerstats.com/latest.asp?league="
  if (tier == 1) {
    league <- country
  } else {
    league <- paste(country, tier, sep="")
  }
  url <- paste(base_url, league, sep="")
  standings <- url %>%
    read_html() %>%
    html_nodes(xpath="//*[@id=\"btable\"]") %>%
    html_table(fill=TRUE)
  standings <- standings[[5]]
  standings <- standings[-c(1),]
  names(standings) <- c("Position", "Team", "Played", "Points")
  return(standings)
}

## Tests:
# 1
prem <- scrapeTable("england", 1)
if (nrow(prem) == 20 && ncol(prem) == 4) {
  print("Test 1 passed")
} else {
  print("Test 1 falied")
}

# 2
segunda <- scrapeTable("spain", 2)
if (nrow(segunda) == 22 && ncol(segunda) == 4) {
  print("Test 2 passed")
} else {
  print("Test 2 failed")
}

# 3
divizia <- scrapeTable("moldova", 1)
if (nrow(divizia) == 11 && ncol(divizia) == 4) {
  print("Test 3 passed")
} else {
  print("Test 3 failed")
}

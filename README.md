# ncaaStats
'ncaaStats' is an R package for gathering NCAA Men's Basketball data. The various functions within can efficiently produce dataframes of play-by-play information (ESPN), box scores (ESPN), betting-odds (ESPN), and up-to-date ratings/rankings (KenPom). 

## Installation
You can install `collegehoops` from GitHub with:

``` r
# install.packages("devtools")
install_github("dhutexas/collegehoops")
```

## Obtaining Data
* ```get_game_ids(team, season)```:  Returns a vector of all gameIds for a team in a given season. These act as the inputs for additional functions, allowing the scraping of data for particular games.
* ```get_season_schedule(team, season)```:  Returns the full season schedule of games for a particular team. Again, the gameIds can be used to obtain data for each game using the additional functions herein.
* ```get_game_data(espn_game_id)```:  Returns play-by-play data (if available) along with shot location data (if available) and basic facts about the game (home, away, final score, etc.).
* ```get_box_score_team(espn_game_id)```:  Returns team-oriented box-score data for an individual game.
* ```get_box_score_players(espn_game_id)```:  Returns player-oriented box-score data for an individual game.
* ```get_kenpom_data(season)```:  Returns end-of-season or most recent (if current seson) Ken Pomeroy ratings and rankings in a tidy format.


The `season` parameter can be written either in the format "2020-21" or "21", where the final number refers to the year in which the NCAA Tournament is played.

The `team` parameter must match the name used by ESPN.com. A complete list of current names is available in the built-in dataset `ids`, with additional details described in the __Datasets__ section below.

The `espn_game_id` parameter is the unique identifier for each game (past and future). To obtain a game, or games of interest to add as a parameter, please try some of the included functions in this package, such as ```get_game_ids(team, season)``` or ```get_season_schedule(team, season)```.


## Datasets

```ids``` A data frame for converting between team names from various sites.
 
 * ```team```: team name, as used by ESPN
 * ```id```: team id; numeric team id used by ESPN
 * ```link```: link; url link for ESPN website

This dataset can be loaded by typing ```ids```, after installing and loading the package.

## Acknowledgements and Thanks

This package owes a special debt of gratitude to Luke Benz and those over at `ncaahoopR`, including Saiem Gilani. If you are looking for a different way to gather this data, or some alternative functions, please check out their work.

# NBAapi

An R package that documents the NBA API, providing functions to pull data from [stats.nba.com](http://stats.nba.com).

Disclaimer: This R package is in no way affiliated with the NBA.

### Installation

This package is not available on CRAN and needs to be installed from this GitHub repository. To do this you need to run the following:
```
devtools::install_github("imadmali/NBAapi")
```

### Brief Overview

The functions to pull data have the `get_*` naming convention. For example if we want to access shot chart data from the "shot chart detail" endpoint we can use `get_shotchart(...)` (use the documentation to understand the appropriate arguments). Similarly, if we want play-by-play data we can use the `get_pbp(...)` function.

### Tasks

- [x] Box Scores (boxscoretraditionalv2, boxscorescoringv2)
- [x] Play by Play (playbyplay, playbyplayv2)
- [x] Shot Charts (shotchartdetail)
- [ ] Draft Combine
- [x] Player Info (commonallplayers, commonteamroster)
- [x] Team Info (commonTeamYears)
- [x] Player/team ID/maps
- [ ] Graphics for shot charts (scatter, hex, heat, lattice)
- [x] Light unit tests
- [ ] ...

Details on some of the active (and seemingly inactive) endpoints are available [here](https://github.com/seemethere/nba_py/wiki/stats.nba.com-Endpoint-Documentation).

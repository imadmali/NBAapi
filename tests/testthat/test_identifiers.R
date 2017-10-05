context("test 'identifiers' functions")

test_that("team_ids runs", {
  tmp <- team_ids()
  expect_equal(colnames(tmp), c("TEAM_ID", "ABBREVIATION"))
})

test_that("player_ids runs", {
  tmp <- player_ids(Season = "2016-17", IsOnlyCurrentSeason = "1")
  expect_equal(colnames(tmp), c("PERSON_ID",
                                "DISPLAY_FIRST_LAST",
                                "TEAM_ID",
                                "TEAM_ABBREVIATION"))
})

test_that("game_id runs correctly", {
  tmp <- expect_equal(game_id("2017", "1"), "0021600001")
})

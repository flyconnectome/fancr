test_that("changelog works", {
  skip_if_offline()
  skip_if_not(fanc_token_available(),
              message="Unable to obtain a Zetta / FANC access token")
  expect_type(res <- fanc_change_log("648518346473954669"), "list")
  expect_named(res, c("n_splits", "n_mergers", "user_info", "operations_ids",
                      "past_ids"))
})

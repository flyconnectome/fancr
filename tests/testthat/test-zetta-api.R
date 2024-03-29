test_that("changelog works", {
  skip_if_offline()
  skip_if_not(fanc_token_available(),
              message="Unable to obtain a FANC access token")
  expect_s3_class(res <- fanc_change_log("648518346473954669"), "data.frame")
  expect_named(res, c("operation_id", "timestamp", "user_id", "before_root_ids",
                      "after_root_ids", "is_merge", "user_name", "user_affiliation"))
})

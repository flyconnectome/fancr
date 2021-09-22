test_that("fanc_partner_summary works", {
  expect_s3_class(fanc_partner_summary("648518346494405175", partners = 'outputs'),
                  "data.frame")
})

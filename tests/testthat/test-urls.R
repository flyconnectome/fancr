test_that("fanc_scene works", {
  expect_type(sc <- fanc_scene("648518346498932033"), 'character')
})

test_that("switching between banc and fanc works", {
  op=choose_fanc()
  on.exit(options(op))
  expect_match(fanc_cloudvolume_url(), "mar2021_prod")
  expect_match(with_banc(fanc_cloudvolume_url()), "wclee_fly_cns_001")
  choose_banc()
  expect_match(with_fanc(fanc_cloudvolume_url()), "mar2021_prod")
  expect_match(with_banc(fanc_cloudvolume_url()), "wclee_fly_cns_001")
  choose_fanc()
  expect_match(fanc_cloudvolume_url(), "mar2021_prod")
  expect_match(with_banc(fanc_cloudvolume_url()), "wclee_fly_cns_001")
})

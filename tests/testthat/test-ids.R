test_that("fanc_xyz2id works", {
  expect_equal(fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE),
               "648518346499897667")

  expect_equal(
    fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE, root=F),
    "73186243730767724")
})


test_that("fanc_islatest works", {
  expect_false(fanc_islatest("648518346473954669"))
  expect_false(isTRUE(all.equal(
    fanc_latestid("648518346473954669"), "648518346473954669")))
})


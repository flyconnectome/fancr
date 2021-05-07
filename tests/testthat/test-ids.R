test_that("fanc_xyz2id works", {
  expect_equal(fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE),
               "648518346479013777")

  expect_equal(
    fanc_xyz2id(cbind(34495, 82783, 1954), rawcoords=TRUE, root=F),
    "73186243730767724")
})

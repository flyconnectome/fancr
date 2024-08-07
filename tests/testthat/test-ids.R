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


test_that("fanc_ids works", {
  expect_equal(fanc_ids("648518346473954669"), "648518346473954669")
  expect_equal(fanc_ids("648518346473954669", integer64 = T), bit64::as.integer64("648518346473954669"))

  df1=data.frame(pt_root_id=bit64::as.integer64("648518346473954669"))
  df2=data.frame(id=bit64::as.integer64("648518346473954669"))

  expect_equal(fanc_ids(df1, integer64 = F), "648518346473954669")
  expect_equal(fanc_ids(df1), df1$pt_root_id)
  expect_equal(fanc_ids(df2, integer64 = F), "648518346473954669")
})


test_that("fanc_cellid_from_segid", {
  rid=fanc_latestid("648518346486614449")
  expect_equal(fanc_cellid_from_segid(rid),12967L)
  expect_s3_class(df <- fanc_cellid_from_segid(rid, rval = 'dat'), 'data.frame')
  expect_equal(df$id, 12967L)

  expect_type(allids <- fanc_cellid_from_segid(), 'integer')
  expect_true(length(allids)>20000)

  # skip this test because we can't be sure it will work
  # expect_equal(
  #   fanc_cellid_from_segid("648518346486614449", "2023-07-29 06:04:28 UTC"),
  #   12967L)
})

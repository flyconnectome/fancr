test_that("fanc4to3 works", {
  # https://radagast.hms.harvard.edu/catmaidvnc/?pid=13&zp=117630&yp=470101.3&xp=194569.2&tool=tracingtool&active_skeleton_id=1377198&active_node_id=18180879&sid0=10&s0=-2

    # nb xyzmatrix sets column names
  fanc3=xyzmatrix(cbind(194569.2, 470101.3, 117630))
  fanc4=cbind(45224, 109317, 2614)*c(4.3,4.3,45)
  expect_equal(fanc4to3(fanc4), fanc3, tolerance = 1e-5)
})



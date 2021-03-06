test_that('funor_funom validates input arguments', {
  expect_warning(funor_funom(matrix(c('a', 1:3), nrow = 2)), 'argument "x" must be convertable to a numeric matrix')
  expect_warning(funor_funom(table_1), 'argument "x" must have at least 2 rows and columns')
  expect_warning(funor_funom(table_2, A_r = 'a'), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, A_m = 'a'), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_r = 'a'), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_m = 'a'), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, A_r = c(1, 2)), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, A_m = c(1, 2)), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_r = c(1, 2)), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_m = c(1, 2)), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, A_r = NA), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, A_m = NA), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_r = NA), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_m = NA), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, A_r = NULL), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, A_m = NULL), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_r = NULL), 'arguments "A" and "B" must be single numeric values')
  expect_warning(funor_funom(table_2, B_m = NULL), 'arguments "A" and "B" must be single numeric values')
})

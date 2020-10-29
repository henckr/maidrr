context('Variable grid')
library(maidrr)

data('mtpl_be')
data('mtpl_fr')

test_that('output is of the expected format for one feature', {

  expect_is(get_grid('ageph', mtpl_be), 'tbl_df')
  expect_equal(ncol(get_grid('ageph', mtpl_be)), 1)
  expect_equal(nrow(get_grid('ageph', mtpl_be)), length(unique(mtpl_be$ageph)))
  expect_match(names(get_grid('ageph', mtpl_be)), 'ageph')
  expect_is(get_grid('ageph', mtpl_be)$ageph, 'integer')
  expect_is(get_grid('fuel', mtpl_be)$fuel, 'factor')
  expect_is(get_grid('power', mtpl_fr)$power, 'ordered')
})


test_that('output is of the expected format for two features', {

  expect_is(get_grid(c('ageph', 'power'), mtpl_be), 'tbl_df')
  expect_equal(ncol(get_grid(c('ageph', 'power'), mtpl_be)), 2)
  expect_equal(names(get_grid(c('ageph', 'power'), mtpl_be)), c('ageph', 'power'))
})


test_that('an error is produced when a feature is not present in the data', {

  expect_error(get_grid('license', mtpl_be),
               'The following variable(s) could not be found in the supplied data: license', fixed = TRUE)
  expect_error(get_grid(c('ageph', 'nope', 'fuel', 'nein'), mtpl_be),
               'The following variable(s) could not be found in the supplied data: nope nein', fixed = TRUE)
})


test_that('extension to interaction effects is feasible', {
  grid <- tidyr::expand_grid(get_grid(var ='ageph', data = mtpl_be),
                             get_grid(var ='coverage', data = mtpl_be))

  expect_is(grid, 'tbl_df')
  expect_equal(ncol(grid), 2)
  expect_equal(nrow(grid), length(unique(mtpl_be$ageph)) * length(unique(mtpl_be$coverage)))
  expect_true(all(c('ageph', 'coverage') %in% names(grid)))
  expect_is(grid$ageph, 'integer')
  expect_is(grid$coverage, 'factor')
})


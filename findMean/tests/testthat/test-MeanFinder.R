test1 = dplyr::mutate(gapminder::gapminder, mean.lifeExp = mean(lifeExp, na.rm = TRUE))
test_that("Test find_mean with input Vector that has no NAs and no optional argument",
          expect_equal(find_mean(gapminder::gapminder, lifeExp), test1))
rm("test1")

test2 = dplyr:: mutate(datateachr::vancouver_trees, mean.latitude = mean(latitude, na.rm= TRUE))
test_that("Test find_mean with input Vector that has NAs and no optional argument",
          expect_equal(find_mean(datateachr::vancouver_trees, latitude), test2))
rm("test2")

test3 = datateachr::vancouver_trees %>%
  dplyr::group_by(cultivar_name) %>%
  dplyr::mutate(mean.diameter = mean(diameter, na.rm=TRUE))
test_that("Test find_mean with input Vector that has NAs and an optional argument",
          expect_equal(find_mean(datateachr::vancouver_trees, diameter, cultivar_name), test3))

rm("test3")

test_that("Test find_mean with input Vector that ONLY has NAs and an optional argument",
          expect_error(find_mean(NA, new_col, cultivar_name)))

test_that("Test find_mean with input Vector of non-numeric type and an optional argument",
          expect_error(find_mean(datateachr::vancouver_trees, species_name, cultivar_name)))

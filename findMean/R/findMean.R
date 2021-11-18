#' @title Find mean of columns according to groups and add it to original tibble.
#'
#' @description Add a mean column to data-frame after grouping by another column. The new mean column name will add a "mean." prefix to the
#'original column name ("mean.col_name"). This function uses the mutate [dplyr::mutate()] function and group_by [dplyr::group_by()] from the dplyr package.
#'
#' @param tib A tibble. Since the input should be a tibble I named it "tib"
#' @param col_name A column name to find mean of. To make it evident I named it "col_name". Function will remove NA from this column
#' @param group_col_name Optional parameter. A column name. If provided, data-set will be grouped by this column before finding mean. Since the data is going to be grouped according to this I named it "group_col_name". Function will not remove any NA from this column.
#' @return A tibble
#' @examples
#' find_mean(gapminder::gapminder, lifeExp)
#' find_mean(gapminder::gapminder, lifeExp, country)
#' @export

find_mean = function(tib, col_name, group_col_name) {
  calculations = dplyr::summarise(tib,
                                  is_numeric = is.numeric({{ col_name }}),
                                  class = class({{ col_name }}))
  if (!calculations$is_numeric) {
    stop("Selected column is not numeric. Column is ", calculations$class)
  }
  if (missing(group_col_name)) {
    mean_tib <- dplyr::mutate(tib, "mean.{{ col_name }}" := mean({{ col_name }}, na.rm = TRUE))
  } else {
    mean_tib <- tib %>%
      dplyr::group_by({{ group_col_name }}) %>%
      dplyr::mutate("mean.{{ col_name }}" := mean({{ col_name }}, na.rm = TRUE))
  }
}

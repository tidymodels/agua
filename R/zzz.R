.onLoad <- function(libname, pkgname) {
  add_logistic_reg_h2o()
  add_rand_forest_h2o()
  add_boost_tree_h2o()
}


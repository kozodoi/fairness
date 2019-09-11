.onLoad <- function(libname, pkgname)
{
  # make data set names global to avoid CHECK notes
  utils::globalVariables("relevel")
  utils::globalVariables("groupst")
  utils::globalVariables("grouproc_1")
  utils::globalVariables("grouproc_2")
  utils::globalVariables("grouproc_3")
  utils::globalVariables("grouproc_4")
  utils::globalVariables("grouproc_5")
  utils::globalVariables("grouproc_6")

  invisible()
}

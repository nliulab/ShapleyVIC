# The following code resolves the "no visible binding for global variable 'i'"
# in checks for `foreach`
utils::globalVariables(c("i"))

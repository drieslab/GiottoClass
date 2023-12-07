# Use this function internal to this package
# .n should be increased to 3L when within a generic method
.gstop <- function(...,
                   sep = " ",
                   strWidth = 100,
                   errWidth = FALSE,
                   .module = 'GiottoClass',
                   .prefix = ' ',
                   .initial = '',
                   .n = 2L) {
  GiottoUtils::gstop(
    ...,
    sep = sep,
    strWidth = strWidth,
    errWidth = errWidth,
    .module = .module,
    .prefix = .prefix,
    .initial = .initial,
    .n = .n
  )
}

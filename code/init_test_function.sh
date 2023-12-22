#!/bin/bash

# Write the content to hello.R
cat <<EOF >./R/hello.R
#' Hello Function
#'
#' This function prints 'Hello, World!' to the console.
#'
#' @return None
#' @export
#'
#' @examples
#' hello()
hello_world <- function() {
  message('Hello, World!')
}
EOF

#!/bin/bash

# Write the content to hello.R
cat <<EOF >./R/hello.R
#' Hello World Function
#'
#' This function prints 'Hello, World!' to the console.
#'
#' @return None
#' @export
#'
#' @examples
#' hello_world()
hello_world <- function() {
  message('Hello, World!')
}
EOF

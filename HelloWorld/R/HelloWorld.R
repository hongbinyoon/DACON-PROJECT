HelloWorld <- function() {
  appDir <- system.file("HelloWorld",
                        package = "HelloWorld")

  shiny::runApp(
    appDir,
    launch.browser = TRUE,
    display.mode = "normal"
  )
}


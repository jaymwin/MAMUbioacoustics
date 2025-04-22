
#' Shiny app to convert WAVs to FLACs
#'
#' @return
#' @export
#'
#' @examples

shiny_wav_to_flac <- function() {

  # UI for the shiny app
  ui <- shiny::fluidPage(

    shinyjs::useShinyjs(),  # Initialize shinyjs

    shiny::titlePanel("MAMU WAV to FLAC converter"),

    # options to select SD card/hard drive/local paths
    # and buttons to run/reset app
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          "sd_card_path", "SD card path:",
          choices = c("D:/" = "D:/", "E:/" = "E:/", "F:/" = "F:/"),
          selected = "D:/"
        ),
        shiny::textInput("desktop_path", "Local path:", value = "C:/Users/jmwin/OneDrive/Desktop/temp"),
        shiny::selectInput(
          "hard_drive_path", "External hard drive path:",
          choices = c("D:/" = "D:/", "E:/" = "E:/", "F:/" = "F:/"),
          selected = "E:/"
        ),
        shiny::actionButton("run_button", "Run FLAC compression", style = "background-color: #458B74; color: white;", icon = shiny::icon('play')),
        shiny::actionButton("reset_button", "Reset app", icon = shiny::icon('refresh'), style = "background-color: #FFA07A; color: white;")
      ),

      # outputs that are printed
      shiny::mainPanel(
        shiny::htmlOutput("swift"),
        shiny::htmlOutput("visit"),
        shiny::htmlOutput("mamu_site"),
        shiny::htmlOutput("mamu_station"),
        shiny::htmlOutput("cell"),
        shiny::htmlOutput("dates"),
        shiny::htmlOutput("n_wavs"),
        shiny::htmlOutput("pre_function_message"),
        shiny::htmlOutput("n_flacs"),
        shiny::htmlOutput("runtime"),
        shiny::htmlOutput("post_function_message")
      )
    )
  )

  # Server logic for the shiny app
  server <- function(input, output) {

    # Initially, hide the pre-/post-function messages
    output$pre_function_message <- shiny::renderText({
      ""
    })

    output$post_function_message <- shiny::renderText({
      ""
    })

    output$swift <- shiny::renderText({
      ""
    })

    output$visit <- shiny::renderText({
      ""
    })

    output$mamu_site <- shiny::renderText({
      ""
    })

    output$mamu_station <- shiny::renderText({
      ""
    })

    output$cell <- shiny::renderText({
      ""
    })

    output$n_wavs <- shiny::renderText({
      ""
    })

    output$dates <- shiny::renderText({
      ""
    })

    # When the 'Run compression' button is clicked, run the function
    shiny::observeEvent(input$run_button, {

      deployment_df <-
        read_epicollect(
          project_slug = 'mamu-arus',
          token = '1fc5154632994f179f7d8b17214e26cb'
        )

      # get deployment information from SD card
      val <- get_deployment_info(input$sd_card_path, deployment_df)

      # get list of wav paths
      sd_wavs <- fs::dir_ls(input$sd_card_path, recurse = TRUE, glob = '*.wav')
      # get dates of wav files to create daily subfolders on external hard drive
      wav_dates <- unique(stringr::str_extract(sd_wavs, '[0-9]{8}'))

      # get deployment info
      site_id <- val$site_id
      station_id <- val$station_id
      visit_id <- val$visit_id
      cell_id <- val$cell_id

      # desktop/hard drive paths
      desktop_path <- input$desktop_path
      hard_drive_path <- input$hard_drive_path

      # Display pre-function messages immediately
      # swift ID
      output$swift <- shiny::renderText({
        shiny::HTML(stringr::str_glue("Swift ID: <b>{val$swift_id}</b>"))
      })

      output$visit <- shiny::renderText({
        shiny::HTML(stringr::str_glue("Visit ID: <b>{val$visit_id}</b>"))
      })

      # site
      output$mamu_site <- shiny::renderText({
        shiny::HTML(stringr::str_glue("Site ID: <b>{val$site_id}</b>"))
      })

      # station
      output$mamu_station <- shiny::renderText({
        shiny::HTML(stringr::str_glue("Station ID: <b>{val$station_id}</b>"))
      })

      # cell
      output$cell <- shiny::renderText({
        shiny::HTML(stringr::str_glue("Cell ID: <b>{val$cell_id}</b>"))
      })

      # ARU recording dates
      output$dates <- shiny::renderText({
        shiny::HTML(stringr::str_glue("ARU recording dates: <b>{val$min_date} to {val$max_date}</b>"))
      })

      # number of wav files on SD card
      output$n_wavs <- shiny::renderText({
        shiny::HTML(stringr::str_glue("Number of WAV files on SD card: <b>{val$n_wavs}</b>"))
      })

      # print compression message
      output$pre_function_message <- shiny::renderText({
        shiny::HTML(stringr::str_glue("<i>FLAC compression is running, please wait...</i>"))
      })

      # Simulate a delay to show the "please wait" message before continuing
      shinyjs::delay(100, {

        # calculate run time
        start_time <- Sys.time()

        # set cores for parallel flac compression
        future::plan(future::multisession, workers = parallelly::availableCores() - 1)

        # create date folders on external hard drive to store FLACs
        wav_dates |>
          furrr::future_walk(\(x) MAMUbioacoustics:::create_subfolders(x, site_id, visit_id, station_id, cell_id, hard_drive_path))

        # compress flacs
        sd_wavs |>
          furrr::future_walk(\(x) MAMUbioacoustics:::wav_to_flac(x, wav_path, site_id, visit_id, station_id, cell_id, desktop_path, hard_drive_path))

        # count # of flacs compressed
        n_flacs <-
          fs::dir_ls(
            stringr::str_glue('{hard_drive_path}/{site_id}_{visit_id}/{site_id}_{visit_id}_{cell_id}_{station_id}'),
            recurse = TRUE,
            glob = '*.flac'
          ) |>
          length()

        # get runtime now
        run_time <- round((as.numeric(Sys.time() - start_time)), 1)

        # After the task completes, display post-function messages
        # number of flacs created
        output$n_flacs <- shiny::renderText({
          shiny::HTML(stringr::str_glue("Number of FLAC files compressed: <b>{n_flacs}</b>"))
        })

        # runtime
        output$runtime <- shiny::renderText({
          shiny::HTML(stringr::str_glue("Runtime (minutes): <b>{run_time}</b>"))
        })

        # done message
        output$post_function_message <- shiny::renderText({
          shiny::HTML(stringr::str_glue("<b>Finished compressing!</b>"))
        })

        # also make a beeping sound
        beepr::beep('ping')

      })

    })

    # When the 'Reset App' button is clicked, reset the app state
    shiny::observeEvent(input$reset_button, {

      # Reset pre-function and post-function messages
      output$pre_function_message <- shiny::renderText({
        ""
      })

      output$post_function_message <- shiny::renderText({
        ""
      })

      output$swift <- shiny::renderText({
        ""
      })

      output$visit <- shiny::renderText({
        ""
      })

      output$mamu_site <- shiny::renderText({
        ""
      })

      output$cell <- shiny::renderText({
        ""
      })

      output$mamu_station <- shiny::renderText({
        ""
      })

      output$n_wavs <- shiny::renderText({
        ""
      })

      output$dates <- shiny::renderText({
        ""
      })

      output$n_flacs <- shiny::renderText({
        ""
      })

      output$runtime <- shiny::renderText({
        ""
      })

      # Optionally, reset the buttons' states (like re-enabling them if disabled)
      shinyjs::enable("run_button")

      # You can also add any additional reset behavior here, if needed.

    })

  }

  # Run the application
  shiny::shinyApp(ui = ui, server = server)

}

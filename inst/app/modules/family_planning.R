familyPlanningUI <- function(id, i18n) {
  ns <- NS(id)

  tagList(
    contentHeader(ns('fpet_projection'), i18n$t("title_fpet"), i18n = i18n),
    contentBody(
      box(
        title = i18n$t("fpet_projection_option"),
        width = 12,
        solidHeader = TRUE,
        fluidRow(
          column(
            12,
            fileInput(
              inputId = ns('fpet_file'),
              label = i18n$t('btn_upload_fpet_data'),
              buttonLabel = i18n$t('btn_browse_or_drop'),
              placeholder = 'Supported formats: .csv',
              accept = c('.csv')
            ),

            messageBoxUI(ns('feedback'))
          )
        )
      ),
      box(
        title = i18n$t("title_fpet"),
        width = 12,
        fluidRow(
          column(12, plotCustomOutput(ns('fpet'))),
          column(3, downloadButtonUI(ns('fpet_download')))
        )
      )
    )
  )
}

familyPlanningServer <- function(id, cache, i18n) {
  stopifnot(is.reactive(cache))

  moduleServer(
    id = id,
    module = function(input, output, session) {

      messageBox <- messageBoxServer('feedback', i18n = i18n)

      data <- eventReactive(input$fpet_file, {
        req(input$fpet_file)

        file_path <- input$fpet_file$datapath
        file_name <- input$fpet_file$name
        file_type <- tools::file_ext(file_name)

        valid_types <- c('csv')
        if (!file_type %in% valid_types) {
          messageBox$update_message('error_upload_failed_unsupported_format', 'error')
          return(NULL)
        }

        tryCatch({
          dt <- generate_fpet_summary(file_path)
          messageBox$update_message('msg_upload_success', 'success', list(file_name = file_name))

          return(dt)
        },
        error = function(e) {
          clean_message <- clean_error_message(e)
          messageBox$update_message('error_upload_failed', 'error', list(clean_message = clean_message))
          NULL
        })
      })

      output$fpet <- renderCustomPlot({
        req(data())
        plot(data())
      })

      downloadPlot(
        id = 'fpet_download',
        filename = reactive('fpet_plot'),
        data = data,
        i18n = i18n,
        plot_function = function() {
          plot(data())
        }
      )
    }
  )
}

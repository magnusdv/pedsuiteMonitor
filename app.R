suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(httr)
  library(jsonlite)
  library(gt)
})


gh_logo = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png"

PACKAGES = c("dvir",
             "forrel",
             "ibdsim2",
             "KLINK",
             "pedbuildr",
             "pedFamilias",
             "pedmut",
             "pedprobr",
             "pedtools",
             "ribd",
             "segregatr",
             "verbalisr"
             )

DEBUG = F
if(DEBUG)
  PACKAGES = PACKAGES[1:3]

OWNER = rep("magnusdv", length(PACKAGES))
names(OWNER) = PACKAGES


# Build UI ----------------------------------------------------------------

header = dashboardHeader(title = "PEDSUITE MONITOR")

body = dashboardBody(
  tags$style(HTML("
    .box-header {padding-top: 3px; padding-bottom: 3px}
    .box-header .btn-xs {margin-left: 5px; padding: 0px 5px}
    .github-icon {position:absolute; top:3px; right:10px; height: 20px;}
    .box-body {padding: 8px}
    .tabbable {margin-top:5px}
    .nav-tabs > li > a {padding:0px 10px;}
    .tab-content {padding:0px !important}
  ")),
  useShinyjs(),
  useBusyIndicators(spinners = FALSE, pulse = TRUE),

  tags$script(HTML("
    // Stop app when browser is closed
    window.onbeforeunload = function(){ Shiny.onInputChange('browserClosed', Math.random()); };
    // Copy text to clipboard
    Shiny.addCustomMessageHandler('copyText',function(text){navigator.clipboard.writeText(text);});
  ")),

  fluidRow(
    lapply(PACKAGES, function(package) {

      pst = function(s) paste(s, package, sep = "_")

      box(id = pst("box"),
        width = 3, height = "240px", status = "primary", solidHeader = TRUE,
        title = tagList(package,
          actionButton(pst("refresh"), "", icon = icon("refresh"), class = "btn-xs", title = "Update"),
          tags$div(tags$a(href = sprintf("https://github.com/%s/%s", OWNER[package], package),
                          target = "_blank",
                          tags$img(src = gh_logo, class="github-icon")))),
        gt_output(pst("versions")),
        div(style = "position: relative;",
          tabsetPanel(
            tabPanel(title = pst("commits_header"), gt_output(pst("commits"))),
            tabPanel(title = uiOutput(pst("issues_header")), gt_output(pst("issues")))
          ),
          actionButton(pst("copy"), label = tagList(icon("copy")), class = "btn-sm",
                       style="position:absolute; top:0; right:0; padding:0 2px;")
        ),
      )

    })
  )
)

ui = dashboardPage(header, dashboardSidebar(disable = T), body)


# Server function --------------------------------------------------------

server = function(input, output, session) {

  # Close app when browser closes
  observeEvent(input$browserClosed, stopApp())

  week = reactive(format(Sys.Date(), "%Y-%U"))
  pal = colorRampPalette(c("greenyellow","#FFB84C","#FF6F61"))(21)

  lapply(PACKAGES, function(package) {

    pst = function(s) paste(s, package, sep = "_")

    # Update info -------------------------------------------------------------
    data = reactive(updateInfo(package, OWNER[package], debug = DEBUG)) |>
      bindCache(package, input[[pst("refresh")]], week())

    ncommits = reactive(nrow(data()$commits))

    output[[pst("versions")]] = render_gt(data()$versions |> style_versions())

    # Commits tab title: hack to add dynamic background color
    observe({
      n = ncommits()
      col = if(n>20) pal[21] else pal[n+1]
      runjs(sprintf("$('a[data-value=\"%s\"]').text('Commits: %s').css('background-color','%s');",
                    pst("commits_header"), n, col))
    })

    output[[pst("issues_header")]]  = renderText(paste("Issues:", nrow(data()$issues)))

    output[[pst("commits")]] = render_gt(data()$commits |> style_commits(),
                                            height = "130px")

    output[[pst("issues")]] = render_gt(data()$issues |> style_issues(),
                                        height = "130px")

    # Copy commit messages to clipboard
    observeEvent(input[[pst("copy")]], {
      commitMess = data()$commits$Message
      txt = paste0("* ", commitMess, collapse = "\n")
      session$sendCustomMessage("copyText", txt)
    })
  })

}


shinyApp(ui, server, options = list(launch.browser = TRUE))

suppressPackageStartupMessages({
  library(shiny)
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
if(DEBUG) PACKAGES = PACKAGES

OWNER = rep("magnusdv", length(PACKAGES))
names(OWNER) = PACKAGES
OWNER["dvir"] = "thoree"


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

  fluidRow(
    lapply(PACKAGES, function(package) {

      pst = function(s) paste(s, package, sep = "_")

      box(id = pst("box"),
        width = 3, height = "240px", status = "primary", solidHeader = TRUE,
        title = tagList(package,
          actionButton(pst("refresh"), "", icon = icon("refresh"), class = "btn-xs", title = "Update"),
          tags$div(tags$a(href = sprintf("https://github.com/%s/%s", OWNER[package], package),
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


server = function(input, output) {

  week = reactive(format(Sys.Date(), "%Y-%U"))

  lapply(PACKAGES, function(package) {

    pst = function(s) paste(s, package, sep = "_")

    # Update info -------------------------------------------------------------
    data = reactive(updateInfo(package, OWNER[package], debug = DEBUG))|>
      bindCache(package, input[[pst("refresh")]], week())

    output[[pst("versions")]] = render_gt(data()$versions |> style_versions())

    output[[pst("issues_header")]]  = renderText(paste("Open issues:", nrow(data()$issues)))

    output[[pst("commits_header")]] = renderText(paste("Commits:", nrow(data()$commits)))

    output[[pst("commits")]] = render_gt(data()$commits |> style_commits(),
                                            height = "130px")

    output[[pst("issues")]] = render_gt(data()$issues |> style_issues(),
                                        height = "130px")

    # Copy commit messages to clipboard
    observeEvent(input[[pst("copy")]], {
      commitMess = data()$commits$Message
      write.table(commitMess, "clipboard", col.names = F, quote = F,
                  row.names = rep("*", length(commitMess)))
    })
  })

}


shinyApp(ui, server)

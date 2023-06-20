library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(gt)

`%||%` = function(x, y) {
  if (is.null(x)) y else x
}

asDateSafe = function(x, default = "") {
  if(!length(x)) return(default)
  as.Date(x)
}

gh_logo = "https://github.githubassets.com/images/modules/logos_page/GitHub-Mark.png"

DEBUG = F

PACKAGES = c("dvir",
             "forrel",
             "ibdsim2",
             "paramlink2",
             "pedmut",
             "pedprobr",
             "pedsuite",
             "pedtools",
             "pedbuildr",
             "ribd",
             "segregatr",
             "verbalisr")

if(DEBUG) PACKAGES = PACKAGES[1:2]

OWNER = rep("magnusdv", length(PACKAGES))
names(OWNER) = PACKAGES
OWNER["dvir"] = "thoree"

PALETTE = colorRampPalette(c("green", "orange", "red"))(365)


# Build UI ----------------------------------------------------------------

header = dashboardHeader(title = "Pedsuite Monitor")

body = dashboardBody(
  tags$style(HTML("
    .box-header {padding-top: 3px; padding-bottom: 3px}
    .box-header .btn-xs {margin-left: 5px; padding: 0px 5px}
    .github-icon {position:absolute; top:3px; right:10px; height: 20px;}
    .box-body {padding: 8px}
  ")),

  fluidRow(
    lapply(PACKAGES, function(package) {
      box(id = paste0("box_", package),
        width = 3, height = "245px", status = "primary", solidHeader = TRUE,
        title = tagList(package,
                        actionButton(paste0("refresh_", package), "", icon = icon("refresh"),
                                     class = "btn-xs", title = "Update"),
                        tags$div(tags$a(href=sprintf("https://github.com/%s/%s", OWNER[package], package),
                                        tags$img(src = gh_logo, class="github-icon")))),

        gt_output(paste0("versions_", package)),
        gt_output(paste0("commit_log_", package))
      )
    })
  )
)

ui = dashboardPage(header, dashboardSidebar(disable = T), body)


server = function(input, output) {

  week = reactive(format(Sys.Date(), "%Y-%U"))

  lapply(PACKAGES, function(package) {
    owner = OWNER[package]
    refreshVar = paste0("refresh_", package)

    observeEvent(input[[refreshVar]], message("Update: ", package))

    # Update info -------------------------------------------------------------

    cran = reactive({message("Updating ", package)
      if(DEBUG) {message("CRAN-", package); return(list())}
      url = paste0("https://crandb.r-pkg.org/", package)
      fromJSON(content(GET(url), "text", encoding = "UTF-8"))
    }) |> bindCache(refreshVar, input[[refreshVar]], week())


    ghRelease = reactive({
      if(DEBUG) {message("GH-", package); return(list())}
      url = sprintf("https://api.github.com/repos/%s/%s/releases/latest",
                    owner, package)
      fromJSON(content(GET(url), "text", encoding = "UTF-8"))
    }) |> bindCache(refreshVar, input[[refreshVar]], week())

    currentDev = reactive({
      if(DEBUG) {message("DEV-", package); return("")}
      url = sprintf("https://raw.githubusercontent.com/%s/%s/master/DESCRIPTION",
                    owner, package)
      DESCR = content(GET(url), "text", encoding = "UTF-8")
      m = regexpr("(?<=Version: )[0-9.-]+", DESCR, perl = TRUE)
      v = regmatches(DESCR, m)
      sub("00$", "", v)
    }) |> bindCache(refreshVar, input[[refreshVar]], week())

    commits = reactive({
      if(DEBUG) {message("commits-", package); return(NULL)}
      url = sprintf("https://api.github.com/repos/%s/%s/compare/%s...master",
                    owner, package, ghRelease()$tag_name)
      if(!length(url))
        return(NULL)

      a = fromJSON(content(GET(url), "text", encoding = "UTF-8"))
      a$commits$commit %||% list()
    })|> bindCache(format(Sys.Date(), "%Y-%U"), refreshVar, input[[refreshVar]])


    # Version table ------------------------------------------------------------

    output[[paste0("versions_", package)]] = render_gt({
      cran = cran()
      ghRel = ghRelease()
      dev = currentDev()

      tb = data.frame(
        CRAN = cran$Version %||% "?",
        date_CRAN = asDateSafe(cran$`Date/Publication`),
        GITHUB = ghRel$tag_name %||% "-",
        date_GH = asDateSafe(ghRel$published_at, ""),
        DEV = dev)

      # Color of CRAN date
      cranAge = as.numeric(Sys.Date() - as.Date(tb$date_CRAN))
      cranCol = PALETTE[min(length(PALETTE), cranAge, na.rm = TRUE)]
      cranOld = !is.na(cranAge) && cranAge >= 365

      tb |> gt() |>
        opt_stylize(6, color = "gray") |>
        tab_options(
          table.width = "100%",
          table.align = "left",
          column_labels.padding = px(10),
          data_row.padding = px(1),
          table.border.top.style = "hidden",
          container.padding.y = px(0),
          container.overflow.x = FALSE
        ) |>
        tab_style(style = cell_text(size = pct(80), weight = "bold"),
                  locations = cells_column_labels()) |>
        cols_align(align = "left",
                   columns = c(CRAN, GITHUB, DEV)) |>
        cols_align(align = "center",
                   columns = starts_with("date")) |>
        tab_style(style = list(
                    cell_text(size = "small"),
                    cell_borders(sides = "right", style = "solid"),
                    "padding-left: 0px; padding-right: 1px"
                  ),
                  locations = list(
                    cells_body(columns = starts_with("date")),
                    cells_column_labels(columns = starts_with("date")))) |>
        tab_style(style = "padding-right: 0px",
                  locations = list(cells_body(columns = c(CRAN, GITHUB)),
                                   cells_column_labels(columns = c(CRAN, GITHUB)))) |>
        tab_style(style = cell_text(whitespace = "nowrap"),
                  locations = cells_body()) |>
        cols_label(starts_with("date") ~ "") |>
        tab_style(style = cell_text(color = "brown", weight = "bolder"),
                  locations = cells_body(columns = GITHUB, rows = sub("v", "", GITHUB) != CRAN)) |>
        tab_style(style = cell_text(color = cranCol, weight = if(cranOld) "bold" else "normal"),
                  locations = cells_body(columns = date_CRAN))
    })


    # Commit log --------------------------------------------------------------

    output[[paste0("commit_log_", package)]] = render_gt({
      commits = commits()

      if(is.null(commits)) {
        tb = data.frame(Date = as.Date(NULL), Message = character(0))
        ncom = "?"
      }
      else {
        tb = data.frame(Date = rev(as.Date(commits$committer$date)),
                        Message = rev(sub("\n.*", "", commits$message)))
        ncom = nrow(tb)
      }

      tb |> gt() |>
        tab_header(title = paste("Commits since last release:", ncom)) |>
        tab_options(
          table.width = "100%",
          table.align = "left",
          heading.align = "left",
          table.font.size = "small",
          data_row.padding = px(1),
          column_labels.hidden = TRUE,
          table.border.top.style = "hidden",
          container.padding.y = px(0)
        ) |>
        opt_row_striping() |>
        cols_align(align = "left",
                   columns = everything()) |>
        tab_style(style = cell_text(whitespace = "nowrap"),
                  locations = cells_body()) |>
        tab_style(style = cell_text(size = "small", weight = "bolder"),
                  locations = cells_title())
    }, height = "150px")
  })
}


shinyApp(ui, server)

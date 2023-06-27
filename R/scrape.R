
updateInfo = function(package, owner, debug = FALSE) {
  if(debug)
    return(list(
      versions = data.frame(CRAN = "?", date_CRAN = "",
                            GITHUB = "?", date_GH = "", DEV = "?"),
      issues = data.frame(Date = Sys.Date(), By = rep("mdv", sample.int(10, 1)),
                          `#` = "#?", Title = "Issue title", check.names = FALSE),
      commits = data.frame(Date = Sys.Date(), Message = rep("Commit message", sample.int(10, 1)))
    ))

  # CRAN -------------------------------------------------------------

  cran_url = paste0("https://crandb.r-pkg.org/", package)
  cran_data = fromJSON(content(GET(cran_url), "text", encoding = "UTF-8"))

  CRAN = cran_data$Version %||% "?"
  date_cran0 = cran_data$`Date/Publication`
  date_CRAN = if(length(date_cran0)) as.Date(date_cran0) else ""

  # GitHub -------------------------------------------------------------

  # Latest release
  release_url = sprintf("https://api.github.com/repos/%s/%s/releases/latest",
                  owner, package)
  release_data = fromJSON(content(GET(release_url), "text", encoding = "UTF-8"))

  GH = release_data$tag_name %||% "?"
  date_GH0 = release_data$published_at
  date_GH = if(length(date_GH0)) as.Date(date_GH0) else ""

  # Current dev version
  dev_url = sprintf("https://raw.githubusercontent.com/%s/%s/master/DESCRIPTION",
                  owner, package)
  DESCR = content(GET(dev_url), "text", encoding = "UTF-8")
  m = regexpr("(?<=Version: )[0-9.-]+", DESCR, perl = TRUE)
  v = regmatches(DESCR, m)
  dev = if(length(v)) sub("00$", "", v) else "?"


  # Version data frame ------------------------------------------------------

  versions = data.frame(CRAN = CRAN,
                        date_CRAN = date_CRAN,
                        GITHUB = GH,
                        date_GH = date_GH,
                        DEV = dev)

  # Issues ------------------------------------------------------------------

  issues_url = sprintf("https://api.github.com/repos/%s/%s/issues", owner, package)
  issues_data = fromJSON(content(GET(issues_url), "text", encoding = "UTF-8"))
  issues = list(Date = as.Date(issues_data$created_at),
                By = as.character(issues_data$user$login),
                `#` = sprintf("#%d", as.integer(issues_data$number)),
                Title = as.character(issues_data$title)) |>
    as.data.frame(check.names = FALSE)

  # Commits -----------------------------------------------------------------

  commits_url = sprintf("https://api.github.com/repos/%s/%s/compare/%s...master",
                        owner, package, GH)
  commits_data_all = fromJSON(content(GET(commits_url), "text", encoding = "UTF-8"))
  commits_data = commits_data_all$commits$commit
  date = commits_data$committer$date
  message = sub("\n.*", "", commits_data$message)

  commits = data.frame(Date = rev(as.Date(date)),
                       Message = rev(as.character(message)))

  list(versions = versions, issues = issues, commits = commits)
}


asDateSafe = function(x, default = "") {
  if(!length(x)) return(default)
  as.Date(x)
}

`%||%` = function(x, y) {
  if (is.null(x)) y else x
}

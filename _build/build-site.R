# ---------------------------------------------------------------------------
# build-site.R
# Builds the course website from _build/sessions.yml and _build/about.html.
#
# Run from the website folder (2026-eui-int-quant/):   Rscript _build/build-site.R
#
# Writes (all generated; do not edit by hand):
#   index.html               home page: course info + schedule
#   sessions/week-NN.html    one page per *released* session
#   slides/                  copies of the released slide PDFs
#   labs/                    copies of the released student labs, each with a
#                            navigation bar back to the website
# Hand-edited: style.css, _build/sessions.yml, _build/about.html
# ---------------------------------------------------------------------------

suppressMessages(library(yaml))

site <- normalizePath(".")
if (!file.exists(file.path(site, "_build", "sessions.yml"))) stop("Run this from the website folder (2026-eui-int-quant/).")
repo <- normalizePath("..")

cfg <- yaml::read_yaml(file.path(site, "_build", "sessions.yml"))
about <- paste(readLines(file.path(site, "_build", "about.html"), encoding = "UTF-8"), collapse = "\n")
course <- cfg$course
sessions <- cfg$sessions
released <- Filter(function(s) isTRUE(s$released), sessions)

# --- helpers -----------------------------------------------------------------

esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

# *italics*, **bold**, [text](url) -> HTML (after escaping)
md <- function(x) {
  x <- esc(x)
  x <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", '<a href="\\2">\\1</a>', x, perl = TRUE)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "<b>\\1</b>", x, perl = TRUE)
  gsub("\\*([^*]+)\\*", "<i>\\1</i>", x, perl = TRUE)
}

wk <- function(s) sprintf("%02d", s$week)
session_url <- function(s, up = "") paste0(up, "sessions/week-", wk(s), ".html")
mb <- function(path) sprintf("%.1f MB", file.size(path) / 1024^2)
note_html <- function(s, tag = "div") {
  if (is.null(s$note) || !nzchar(s$note)) return("")
  sprintf('<%s class="roomnote">%s</%s>', tag, md(s$note), tag)
}

write_utf8 <- function(text, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  con <- file(path, "w", encoding = "UTF-8")
  writeLines(text, con)
  close(con)
}

page <- function(title, body, up = "", tab = "home") {
  tab_link <- function(href, label, id) {
    sprintf('<a href="%s%s"%s>%s</a>', up, href, if (id == tab) ' class="here"' else "", label)
  }
  quick <- if (length(released)) {
    paste0("<ul>", paste(vapply(released, function(s)
      sprintf('<li><a href="%s">Week %d</a></li>', session_url(s, up), s$week), ""), collapse = ""), "</ul>")
  } else "<p>Nothing yet.</p>"
  latest <- if (length(released)) {
    s <- released[[length(released)]]
    sprintf('<span class="new">NEW!</span> <a href="%s">Week %d</a> slides and lab are online.', session_url(s, up), s$week)
  } else "Materials appear here after each session."
  people <- paste(vapply(cfg$people, function(p)
    sprintf('%s<br><a href="mailto:%s">%s</a>', esc(p$name), p$email, p$email), ""), collapse = "<br>")

  sprintf('<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>%s</title>
<link rel="stylesheet" href="%sstyle.css">
</head>
<body>
<div id="page">
  <div id="header">
    <h1>%s</h1>
    <div class="sub">%s &middot; %s &middot; %s</div>
  </div>
  <div id="tabs">%s %s %s</div>
  <div id="wrap">
    <div id="side">
      <div class="box"><div class="boxtitle">What&#39;s new</div><div class="boxbody">%s</div></div>
      <div class="box"><div class="boxtitle">Sessions online</div><div class="boxbody">%s</div></div>
      <div class="box"><div class="boxtitle">Contact</div><div class="boxbody">%s</div></div>
    </div>
    <div id="main">
%s
    </div>
  </div>
  <div id="footer">Last updated: %s &middot; %s &middot; Best viewed in any browser, at any resolution</div>
</div>
</body>
</html>',
    esc(title), up, esc(course$title), esc(course$term), esc(course$department), esc(course$institution),
    tab_link("index.html", "Home", "home"), tab_link("index.html#schedule", "Schedule", "schedule"),
    tab_link("index.html#about", "About the course", "about"),
    latest, quick, people, body, esc(course$updated), esc(course$institution))
}

# --- copy materials ----------------------------------------------------------

for (d in c("sessions", "slides", "labs")) {
  unlink(file.path(site, d), recursive = TRUE)
  dir.create(file.path(site, d))
}

for (s in released) {
  for (sl in s$slides) {
    src <- file.path(repo, sl$src)
    if (!file.exists(src)) stop("Missing slides for week ", s$week, ": ", sl$src)
    dest <- file.path(site, "slides", sl$as)
    # big PDFs (screen-printed decks) are shrunk with Ghostscript, if installed
    if (file.size(src) > 4 * 1024^2 && nzchar(Sys.which("gs"))) {
      system2("gs", c("-q", "-sDEVICE=pdfwrite", "-dPDFSETTINGS=/ebook", "-dNOPAUSE", "-dBATCH",
                      paste0("-sOutputFile=", shQuote(dest)), shQuote(src)), stdout = FALSE, stderr = FALSE)
    }
    if (!file.exists(dest) || file.size(dest) == 0) file.copy(src, dest, overwrite = TRUE)
  }
  if (!is.null(s$lab)) {
    src <- file.path(repo, s$lab$src)
    if (!file.exists(src)) stop("Missing lab for week ", s$week, ": ", s$lab$src)
    html <- paste(readLines(src, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    bar <- sprintf(paste0(
      '<div style="font-family:Verdana,Tahoma,sans-serif;font-size:12px;line-height:1.4;',
      'background:#003399;color:#ffffff;padding:7px 16px;border-bottom:3px solid #c6273b;position:relative;z-index:2000;">',
      '&larr; <a href="../index.html" style="color:#ffffff;">%s</a> &raquo; ',
      '<a href="../sessions/week-%s.html" style="color:#ffffff;">Week %d</a> &raquo; %s</div>'),
      esc(course$title), wk(s), s$week, esc(sub(":.*", "", s$lab$title)))
    html <- sub("(<body[^>]*>)", paste0("\\1\n", bar), html, perl = TRUE)
    write_utf8(html, file.path(site, "labs", basename(s$lab$src)))
  }
}

# --- session pages -----------------------------------------------------------

readings <- function(x) {
  if (!length(x)) return("<p class=\"muted\">None.</p>")
  paste0('<ol class="readings">', paste(sprintf("<li>%s</li>", vapply(x, md, "")), collapse = "\n"), "</ol>")
}

for (i in seq_along(released)) {
  s <- released[[i]]
  b <- cfg$blocks[[as.character(s$block)]]

  slides_html <- paste(vapply(s$slides, function(sl) {
    f <- file.path(site, "slides", sl$as)
    sprintf('<div class="material"><b>Slides:</b> %s <span class="small">(PDF, %s)</span><br>
<a class="button" href="../slides/%s" target="_blank" rel="noopener">Open</a><a class="button" href="../slides/%s" download>Download</a></div>',
            esc(sl$label), mb(f), sl$as, sl$as)
  }, ""), collapse = "\n")

  lab_html <- if (!is.null(s$lab)) sprintf(
    '<div class="material"><b>Lab:</b> %s <span class="small">(Monday, %s, with Martín Alberdi)</span><br>
<a class="button" href="../labs/%s">Open the lab</a></div>',
    md(s$lab$title), esc(s$lab$date), basename(s$lab$src)) else ""

  prev <- if (i > 1) sprintf('<a href="week-%s.html">&laquo; Week %d</a>', wk(released[[i - 1]]), released[[i - 1]]$week) else "<span></span>"
  nxt <- if (i < length(released)) sprintf('<a href="week-%s.html">Week %d &raquo;</a>', wk(released[[i + 1]]), released[[i + 1]]$week) else "<span></span>"

  body <- sprintf('<div class="crumbs"><a href="../index.html">Home</a> &raquo; <a href="../index.html#schedule">Schedule</a> &raquo; Week %d</div>
<h2>Week %d &mdash; %s</h2>
<div class="meta"><b>Friday, %s</b> &middot; %s &middot; Block %d: %s</div>
%s
%s
<h3>Materials</h3>
%s
%s
<h3>Compulsory readings</h3>
%s
<h3>Optional readings</h3>
%s
<div class="prevnext">%s %s</div>',
    s$week, s$week, md(s$title), esc(s$date), esc(s$by), s$block, esc(b$title), note_html(s, "p"),
    if (!is.null(s$description) && nzchar(s$description)) paste0("<p>", md(s$description), "</p>") else "",
    slides_html, lab_html, readings(s$compulsory), readings(s$optional), prev, nxt)

  write_utf8(page(sprintf("Week %d: %s", s$week, s$title), body, up = "../", tab = "schedule"),
             file.path(site, "sessions", paste0("week-", wk(s), ".html")))
}

# --- home page ---------------------------------------------------------------

rows <- character(0)
last_block <- 0
for (s in sessions) {
  if (s$block != last_block) {
    b <- cfg$blocks[[as.character(s$block)]]
    rows <- c(rows, sprintf('<tr class="block"><td colspan="4">Block %d &mdash; %s (%s)</td></tr>',
                            s$block, esc(b$title), esc(b$by)))
    last_block <- s$block
  }
  if (isTRUE(s$released)) {
    mats <- c(sprintf('<a href="%s">Session page</a>', session_url(s)),
              vapply(seq_along(s$slides), function(j) sprintf('<a href="slides/%s" target="_blank" rel="noopener">Slides%s</a>', s$slides[[j]]$as,
                                                         if (length(s$slides) > 1) paste0(" ", j) else ""), ""),
              if (!is.null(s$lab)) sprintf('<a href="labs/%s">Lab %d</a>', basename(s$lab$src), s$week))
    rows <- c(rows, sprintf('<tr><td>%d</td><td>%s%s</td><td><a href="%s">%s</a><br><span class="small">%s</span></td><td>%s</td></tr>',
                            s$week, esc(s$date), note_html(s), session_url(s), md(s$title), esc(s$by), paste(sprintf('<span class="nowrap">%s</span>', mats), collapse = " &middot; ")))
  } else {
    rows <- c(rows, sprintf('<tr class="locked"><td>%d</td><td>%s%s</td><td>%s<br><span class="small">%s</span></td><td><i>coming soon</i></td></tr>',
                            s$week, esc(s$date), note_html(s), md(s$title), esc(s$by)))
  }
}

info_rows <- c(
  vapply(cfg$people, function(p) sprintf('<tr><td>%s</td><td>%s (<a href="mailto:%s">%s</a>)</td></tr>',
                                         esc(p$role),
                                         if (!is.null(p$url)) sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>', p$url, esc(p$name)) else esc(p$name),
                                         p$email, p$email), ""),
  vapply(cfg$logistics, function(l) sprintf("<tr><td>%s</td><td>%s</td></tr>", esc(l[[1]]), esc(l[[2]])), "")
)

home <- sprintf('<h2>Welcome!</h2>
<p>This is the home page of the PhD seminar <b>%s</b> at the %s, %s. Here you will find the schedule, the readings, the slides, and the labs for every session. Materials are posted after each session.</p>
<table class="grid info">
%s
</table>

<h2><a name="schedule"></a>Schedule</h2>
<p class="small">Seminars are on Fridays; each week&#39;s lab is on the following Monday. Click a session for its slides, lab, and readings.</p>
<table class="grid">
<tr><th>Week</th><th>Date</th><th>Session</th><th>Materials</th></tr>
%s
</table>

%s',
  esc(course$title), esc(course$institution), esc(course$department),
  paste(info_rows, collapse = "\n"), paste(rows, collapse = "\n"), about)

write_utf8(page(course$title, home), file.path(site, "index.html"))
if (!file.exists(file.path(site, ".nojekyll"))) invisible(file.create(file.path(site, ".nojekyll")))

message("Built ", length(released), " session pages; ", length(sessions) - length(released), " sessions listed as coming soon.")

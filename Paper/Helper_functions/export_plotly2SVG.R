# Remarks:
#
# The export is done using the automated testing framework [Selenium](https://
# de.wikipedia.org/wiki/Selenium) which results in opening a browser window
# (Google Chrome) that might has to be closed by hand. Other than Plotly's
# own `export()` function this one also allows to set the `width` and `height`
# of the exported plot (in the former it's hardcoded to 800x600 pixels). If
# `incl_PDF_copy`/`incl_PNG_copy` is set to `TRUE`, the exported SVG additionally
# gets converted to a PDF/PNG using the R package [`rsvg`](https://github.com/
# jeroen/rsvg/tree/40576ac326621b40224db344b09158f4ff717433) which relies on
# [`librsvg`](https://de.wikipedia.org/wiki/Librsvg). On Linux distributions
# the development package of `librsvg` must be installed. On macOS the required
# dependency (`librsvg`) can be installed using [Homebrew](https://brew.sh/).
# Optional PNG auto-cropping is done using the `imager` R package.

ensure_package <- Vectorize(
  FUN =
    function(package,
             load = TRUE)
    {
      installed_packages <- rownames(installed.packages())
      
      if ( !(package %in% installed_packages) )
      {
        install.packages(package)
      }
      if ( load ) library(package, character.only = TRUE)
    }
)

export_plotly2SVG <- function(plotly_graph,
                              filename = NULL,
                              parent_path = paste0(getwd(), "/output"),
                              width = 800,
                              height = 600,
                              remove_title = FALSE,
                              font_family = "Arial",
                              incl_PDF_copy = FALSE,
                              incl_PNG_copy = FALSE,
                              png_scaling_factor = 1.8,
                              autocrop_png = TRUE,
                              port_number = 4445L)
{
  ensure_package(package = c("dplyr",
                             "plotly",
                             "readr",
                             "RSelenium",
                             "rsvg",
                             "stringr"),
                 load = FALSE)
  
  ensure_package("magrittr")
  
  # remove trailing slash in `parent_path`
  parent_path %<>% normalizePath()
  
  # ensure `parent_path` exists
  if ( !dir.exists(parent_path) ) dir.create(path = parent_path,
                                             recursive = TRUE)
  
  # generate sensible filename
  if ( is.null(filename) )
  {
    auto_name <- deparse(substitute(plotly_graph))
    
    filename <- dplyr::if_else(
      condition = auto_name == ".",
      true = "plotly_graph.svg",
      false = paste0(deparse(substitute(plotly_graph)), ".svg")
    )
  } else
  {
    filename %<>%
      stringr::str_replace(pattern = "([^\\.svg])$", 
                           replacement = "\\1.svg")
  }
  
  filepath <- paste0(parent_path, "/", filename)
  
  # delete old SVG file
  if ( file.exists(filepath) )
  {
    unlink(x = filepath)
  }
  
  if ( remove_title )
  {
    plotly_graph %<>%
      plotly::layout(title = "",
                     margin = list(t = 0))
  }
  
  if ( !is.null(font_family) )
  {
    plotly_graph %<>%
      plotly::layout(font = list(family = font_family))
  }
  
  # temporarily export plot to a HTML file
  tempfile <- tempfile(pattern = "plotly_temp_",
                       tmpdir = parent_path,
                       fileext = ".html")
  
  export_plotly2HTML(plotly_graph = plotly_graph,
                     filename = basename(tempfile),
                     parent_path = parent_path)
  
  on.exit(unlink(tempfile),
          add = TRUE)
  
  # get <div> ID of exported htmlwidget
  htmlwidget_id <-
    stringr::str_extract(string = readr::read_file(file = tempfile),
                         pattern = "(?<=<div id=\")htmlwidget-[^\"]+")
  
  # initialize Chrome as RSelenium driver
  selenium_driver <-
    RSelenium::rsDriver(browser = "chrome",
                        port = netstat::free_port(),
                        chromever = "90.0.4430.24",
                        extraCapabilities = list(
                          chromeOptions = list(
                            prefs = list(
                              "profile.default_content_settings.popups" = 0L,
                              "download.prompt_for_download" = FALSE,
                              "download.default_directory" = parent_path
                            )
                          )
                        ),
                        verbose = FALSE)
  
  # navigate to temporary HTML file
  selenium_driver$client$navigate(url = paste0("file://", normalizePath(tempfile)))
  
  # download plot as SVG using the native
  # [`Plotly.downloadImage`](https://plot.ly/javascript/plotlyjs-function-reference/#plotlydownloadimage) function
  selenium_driver$client$executeScript(
    script = paste0("Plotly.downloadImage(document.getElementById('", htmlwidget_id, "'), ",
                    "{format: 'svg', width: ", width, ", height: ", height, ", filename: '",
                    tools::file_path_sans_ext(x = filename), "'});"),
    args = list(NULL)
  )
  
  # wait for SVG to be saved to disk
  Sys.sleep(time = 1)
  
  # convert to PDF
  if ( incl_PDF_copy )
  {
    rsvg::rsvg_pdf(svg = filepath,
                   file = paste0(tools::file_path_sans_ext(parent_path), ".pdf"))
  }
  
  # convert to PNG
  if ( incl_PNG_copy )
  {
    filepath_png <- paste0(tools::file_path_sans_ext(parent_path), ".png")
    
    rsvg::rsvg_png(svg = filepath,
                   file = filepath_png,
                   width = png_scaling_factor * width,
                   height = png_scaling_factor * height)
    
    if ( autocrop_png ) autocrop_png(path_to_png = filepath_png)
  }
  system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
}

export_plotly2HTML <- function(plotly_graph,
                               filename = NULL,
                               parent_path = paste0(getwd(), "/output"),
                               selfcontained = FALSE,
                               libdir = "plotly_files",
                               disable_legend_toggling = NULL,
                               # you can provide a link to webfonts to be used like this:
                               # add_web_font = "https://fonts.googleapis.com/css?family=Work+Sans:200,300,400,600,700"
                               add_web_font = NULL)
{
  ensure_package(package = c("checkmate",
                             "dplyr",
                             "readr",
                             "stringr"),
                 load = FALSE)
  
  ensure_package("magrittr")
  ensure_package("netstat")
  
  # remove trailing slash in `parent_path`
  parent_path %<>% normalizePath()
  
  # ensure `parent_path` exists
  if ( !dir.exists(parent_path) ) dir.create(path = parent_path,
                                             recursive = TRUE)
  
  # generate sensible filename
  if ( is.null(filename) )
  {
    auto_name <- deparse(substitute(plotly_graph))
    
    filename <- dplyr::if_else(
      condition = auto_name == ".",
      true = "plotly_graph.html",
      false = paste0(deparse(substitute(plotly_graph)), ".html")
    )
  }
  
  filepath <- paste0(parent_path, "/", filename)
  
  htmlwidgets::saveWidget(
    widget = plotly_graph,
    file = filepath,
    selfcontained = selfcontained,
    libdir = libdir
  )
  
  if ( !is.null(disable_legend_toggling) )
  {
    test_char <- checkmate::check_choice(x = disable_legend_toggling,
                                         choices = "all")
    test_num <- checkmate::check_numeric(x = disable_legend_toggling,
                                         lower = 1,
                                         upper = length(plotly_graph$x$attrs),
                                         min.len = 1,
                                         max.len = length(plotly_graph$x$attrs),
                                         unique = TRUE,
                                         any.missing = FALSE,
                                         all.missing = FALSE)
    
    if ( !isTRUE(test_char) & !isTRUE(test_num) )
    {
      stop("Invalid argument provided: disable_legend_toggling\n",
           ifelse(!isTRUE(test_char) & is.character(disable_legend_toggling),
                  paste0(test_char, ". Or alternatively can also be a vector of integers >= 1 and <= number of traces."),
                  ""),
           ifelse(!isTRUE(test_num) & is.numeric(disable_legend_toggling),
                  paste0(test_num, ". Or alternatively can also be \"all\"."),
                  ""))
      
    } else if ( isTRUE(test_char) )
    {
      css_rules <-
        c("",
          "/* hides the svg dom element that has the click handler responsible for toggling */",
          ".legend .traces .legendtoggle {",
          "  display: none;",
          "}",
          "/* just for presentation: shows the default cursor instead of the text cursor */",
          ".legend .traces .legendtext {",
          "  cursor: default;",
          "}",
          "")
    } else
    {
      disable_legend_toggling %<>% as.integer()
      
      css_rules <-
        c("",
          "/* hides the svg dom element that has the click handler responsible for toggling */")
      
      for ( i in disable_legend_toggling )
      {
        css_rules %<>%
          c(paste0(".legend .groups:nth-of-type(", i, ") .legendtoggle",
                   dplyr::if_else(i == last(disable_legend_toggling),
                                  " {",
                                  ","), " "))
      }
      
      css_rules %<>%
        c("  display: none;",
          "}",
          "/* just for presentation: shows the default cursor instead of the text cursor */")
      
      for ( i in disable_legend_toggling )
      {
        css_rules %<>%
          c(paste0(".legend .groups:nth-of-type(", i, ") .legendtext",
                   dplyr::if_else(i == last(disable_legend_toggling),
                                  " {",
                                  ","), " "))
      }
      
      css_rules %<>%
        c("  cursor: default;",
          "}",
          "")
    }
    
    # write modified .css file
    plotly_dir <-
      list.dirs(path = paste0(parent_path, "/", libdir),
                full.names = TRUE,
                recursive = FALSE) %>%
      stringr::str_subset(pattern = "plotlyjs")
    
    readr::read_lines(file = paste0(plotly_dir, "/plotly-htmlwidgets.css")) %>%
      c(css_rules) %>%
      readr::write_lines(path = paste0(plotly_dir, "/plotly_htmlwidgets_custom.css"),
                         append = FALSE)
    
    # modify dependency path in HTML file
    readr::read_file(file = filepath) %>%
      stringr::str_replace(pattern = "plotly-htmlwidgets\\.css",
                           replacement = "plotly_htmlwidgets_custom.css") %>%
      readr::write_file(path = filepath,
                        append = FALSE)
  }
  
  if ( !is.null(add_web_font) )
  {
    webfont_tag <-
      "<link href=\"" %>%
      paste0(checkmate::assert_character(x = add_web_font,
                                         pattern = "^https?://\\w.*",
                                         ignore.case = TRUE,
                                         any.missing = FALSE,
                                         all.missing = FALSE,
                                         unique = TRUE)) %>%
      paste0("\" rel=\"stylesheet\" />")
    
    readr::read_file(file = filepath) %>%
      stringr::str_replace(pattern = "<link href=",
                           replacement = paste0(webfont_tag, "\n<link href=")) %>%
      readr::write_file(path = filepath,
                        append = FALSE)
  }
}

autocrop_png <- function(path_to_png)
{
  ensure_package("magrittr")
  ensure_package(package = "imager",
                 load = FALSE)
  
  imager::load.image(file = path_to_png) %>%
    imager::autocrop() %>%
    imager::pad(nPix = 4,
                axes = "xy",
                pos = 0) %>%
    imager::flatten.alpha() %>%
    imager::save.image(file = path_to_png)
}


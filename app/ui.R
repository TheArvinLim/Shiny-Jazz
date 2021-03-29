soloChoices = tidyr::unite(tables$solo_info, "Name", c("melid", "title", "performer"), sep=" - ") %>%
  dplyr::pull(Name)


ui <- fluidPage(
  useShinyjs(),

  tags$audio(id="audio-player", src=NA, type="audio/wav", controls="TRUE"),

  titlePanel("YA lIKE JAZZ?"),

    sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      actionButton("playMusic", "Play"),
      actionButton("pauseMusic", "Pause"),
      actionButton("resumeMusic", "Resume"),
      actionButton("rewindMusic", "Rewind"),
      selectInput("selectedMusic", "Select Solo",
                  choices = soloChoices)
    ),

    # Main panel for displaying outputs ----
    mainPanel(
      uiOutput("bar"),
      uiOutput("notePlaying"),
      uiOutput("chordPlaying"),
      uiOutput("beatPlaying"),
      uiOutput("songTime")
    )
  )
)

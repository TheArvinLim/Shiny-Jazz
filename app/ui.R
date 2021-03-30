soloChoices = tidyr::unite(tables$solo_info, "Name", c("melid", "title", "performer"), sep=" - ") %>%
  dplyr::pull(Name)


ui <- fluidPage(
  useShinyjs(),

  tags$audio(id="audio-player", src=NA, type="audio/wav", controls="TRUE"),

  titlePanel("YA lIKE JAZZ?"),

    sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("selectedMusic", "Select Solo",
                  choices = soloChoices),

      tags$audio(id="audio-player", src=NA, type="audio/wav", controls="TRUE"),
    ),

    # Main panel for displaying outputs ----
    mainPanel(
    )
  )
)

server <- function(input, output) {
  melid = reactiveVal()

  observeEvent(input$selectedMusic, {
    runjs("document.getElementById('audio-player').pause();")
    runjs(paste0("document.getElementById('audio-player').setAttribute('src', 'NA');"))
    melid(strsplit(input$selectedMusic, " - ")[[1]][1])

    filepath = file.path("www", paste0(melid(), ".wav"))

    if (!file.exists(filepath)) {
      createWavFile(melid(), "www")
    }

    runjs(paste0("document.getElementById('audio-player').setAttribute('src','", melid(), ".wav');"))
    runjs("document.getElementById('audio-player').play();")
  })
}

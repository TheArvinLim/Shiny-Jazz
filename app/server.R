server <- function(input, output) {
  audioInstance = reactiveVal(NULL)
  barNum = reactiveVal(NULL)
  chordPlaying = reactiveVal(NULL)
  beatPlaying = reactiveVal(NULL)
  melid = reactiveVal(NULL)
  startTime = reactiveVal(NULL)
  pauseStartTime = reactiveVal(NULL)
  playing = reactiveVal(FALSE)
  notePlaying = reactiveVal(NULL)
  songTime = reactiveVal(NULL)
  songMetadata = reactiveVal(NULL)

  melodyNotes = reactiveVal(NULL)
  beats = reactiveVal(NULL)
  startOnset = reactiveVal(NULL)

  observeEvent(input$playMusic, {
    if (!is.null(audioInstance())) {
      pause(audioInstance())
      audioInstance(NULL)
    }

    melid(strsplit(input$selectedMusic, " - ")[[1]][1])
    melodyNotes(generateMelodyNotes(melid()))
    beats(getBeats(melid()))
    startOnset(getStartOnset(melid()))
    songMetadata(getSoloMetaData(melid()))

    filepath = file.path("www", paste0(melid(), ".wav"))

    if (!file.exists(filepath)) {
      createWavFile(melid(), "www")
    }

    runjs(paste0("document.getElementById('audio-player').setAttribute('src','", melid(), ".wav');"))
    runjs("document.getElementById('audio-player').play();")
    #audioInstance(playWavFile(filepath))
    #startTime(Sys.time())
    playing(TRUE)
  })

  observeEvent(input$pauseMusic, {
    if (!is.null(audioInstance())) {
      pause(audioInstance())
      pauseStartTime(Sys.time())
      playing(FALSE)
    }
  })

  observeEvent(input$resumeMusic, {
    if (!is.null(audioInstance()) && !is.null(pauseStartTime())) {
      resume(audioInstance())
      pauseTime = Sys.time() - pauseStartTime()
      startTime(startTime() + pauseTime)
      playing(TRUE)
      pauseStartTime(NULL)
    }
  })

  observeEvent(input$rewindMusic, {
    if (!is.null(audioInstance())) {
      rewind(audioInstance())
      startTime(Sys.time())
    }
  })

  output$bar = renderUI({
    as.character(barNum())
  })

  output$notePlaying = renderUI({
    as.character(notePlaying())
  })

  output$beatPlaying = renderUI({
    as.character(beatPlaying())
  })

  output$chordPlaying = renderUI({
    as.character(chordPlaying())
  })


  output$songTime = renderUI({
    as.character(input$timePlayed)
  })

  observe ({
    invalidateLater(1000)
    isolate ({
      if (playing()) {

        runjs("Shiny.onInputChange('timePlayed', document.getElementById('audio-player').currentTime);")

        if (length(input$timePlayed) > 0) {
          timeElapsed = input$timePlayed + startOnset()

          barNum (
            beats() %>%
              filter(onset <= timeElapsed) %>%
              slice_tail() %>%
              pull(bar)
          )

          beatPlaying (
            beats() %>%
              filter(onset <= timeElapsed) %>%
              slice_tail() %>%
              pull(beat)
          )

          chordPlaying (
            beats() %>%
              filter(onset <= timeElapsed) %>%
              filter(chord != "") %>%
              slice_tail() %>%
              pull(chord)
          )

          key = songMetadata()$key %>% strsplit("-")
          key = key[[1]][1]

          notePlaying (
            melodyNotes() %>%
              filter(onset <= timeElapsed) %>%
              filter(Note != "X") %>%
              slice_tail() %>%
              mutate(Note = as.character(Note)) %>%
              pull(Note) %>%
              convertToKeyEnharmonic(key)
          )
        }
      }
    })
  })
}

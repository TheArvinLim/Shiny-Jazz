server <- function(input, output) {
  audioInstance = reactiveVal(NULL)
  barNum = reactiveVal(NULL)
  melid = reactiveVal(NULL)
  startTime = reactiveVal(NULL)
  pauseStartTime = reactiveVal(NULL)
  playing = reactiveVal(FALSE)
  notePlaying = reactiveVal(NULL)

  melodyNotes = reactiveVal(NULL)
  barTimes = reactiveVal(NULL)
  startOnset = reactiveVal(NULL)

  observeEvent(input$playMusic, {
    if (!is.null(audioInstance())) {
      pause(audioInstance())
      audioInstance(NULL)
    }

    melid(strsplit(input$selectedMusic, " - ")[[1]][1])
    melodyNotes(generateMelodyNotes(melid()))
    barTimes(getBarTimes(melid()))
    startOnset(getStartOnset(melid()))

    filepath = file.path("audio", paste0(melid(), ".wav"))

    if (!file.exists(filepath)) {
      createWavFile(melid(), "audio")
    }

    audioInstance(playWavFile(filepath))
    startTime(Sys.time())
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

  observe ({
    invalidateLater(1)
    isolate ({
      if (playing()) {
        timeElapsed = as.numeric(difftime(Sys.time(), startTime()), units="secs") + startOnset()

        print(timeElapsed)

        barNum (
          barTimes() %>%
            filter(onset <= timeElapsed) %>%
            slice_tail() %>%
            pull(bar)
        )

        notePlaying (
          melodyNotes() %>%
            filter(onset <= timeElapsed) %>%
            slice_tail() %>%
            pull(Note)
        )
      }
    })
  })
}

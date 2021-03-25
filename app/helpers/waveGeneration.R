sine <- function(freq, duration, note_shortness, tempo, sample_rate) {
  wave <- sin(seq(0, duration/tempo * 60, 1/sample_rate) *
                freq * 2 * pi)

  n = max(2/(length(wave)-2)*sample_rate, note_shortness)

  fade <- seq(0, 1, n/sample_rate)

  wave * c(fade, rep(1, max(0, length(wave) - 2 * length(fade))),
           rev(fade))

}

whiteNoise <- function(freq, duration, note_shortness, tempo, sample_rate, cutoff=0.02) {
  if (freq == 0) {
    wave <- rep(0, length(seq(0, duration/tempo * 60, 1/sample_rate)))
  }
  else {
    wave <- rnorm(seq(0, duration/tempo * 60, 1/sample_rate))
    wave = wave * (1 / max(wave, na.rm = T))  # normalise
    wave = pmin(cutoff, wave)
    wave = pmax(-cutoff, wave)  # apply filter

  }

  n = max(2/(length(wave)-2)*sample_rate, note_shortness)

  fade <- seq(0, 1, n/sample_rate)

  wave * c(fade, rep(1, max(0, length(wave) - 2 * length(fade))),
           rev(fade))
}

getWave = function(notes_pitch = NULL, notes_duration = NULL, tempo = 240, note_shortness=100, wave_func=sine)
{
  . <- NULL
  note <- NULL
  octave <- NULL
  pitch <- NULL
  notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

  tune <-
    tibble::data_frame(
      pitch = notes_pitch,
      duration = notes_duration
    ) %>%
    dplyr::mutate(
      octave = substring(pitch, nchar(as.character(pitch))) %>%
        {
          suppressWarnings(as.numeric(.))
        } %>%
        ifelse(is.na(.), 4, .),
      note = ifelse(notes_pitch!="X",
                    notes[substr(pitch, 1, 1)] +
                      grepl("#", pitch) -
                      grepl("b", pitch) +
                      octave * 12 +
                      12 * (notes[substr(pitch, 1, 1)] < 3),
                    0),
      freq = ifelse(note!=0,
                    2^((note - 60) / 12) * 440,
                    0
      )
    )

  sample_rate <- 44100

  tune_wave <- mapply(wave_func, tune$freq, tune$duration, note_shortness, tempo, sample_rate,
                      SIMPLIFY = FALSE) %>% do.call("c", .)
}

generateMelodyNotes <- function(melid_) {
  soloMelody = tables$melody %>%
    filter(melid==melid_) %>%
    left_join(midiNoteTable, by=c("pitch"="MIDI Note"))

  melody = soloMelody %>%
    mutate(Note = paste0(Note, Octave)) %>%
    select(onset, duration, Note)

  melodyWithRests = data.frame(onset=as.numeric(), duration=as.numeric(), Note=as.character())
  currentTime = 0
  for (n in 1:nrow(melody)) {
    nextNoteTime = melody$onset[n] - currentTime

    if (nextNoteTime > 0) {
      restRow = data.frame(onset=currentTime, duration=nextNoteTime, Note="X")
      melodyWithRests = rbind(melodyWithRests, restRow)
    }

    noteRow = melody[n,]
    melodyWithRests = rbind(melodyWithRests, noteRow)

    currentTime = melody$onset[n] + melody$duration[n]

  }

  return (melodyWithRests)
}

generateBassNotes <- function(melid_, bassNoteLength=0.8) {
  soloMetaData = getSoloMetaData(melid_)
  bpm = soloMetaData$avgtempo
  beatLengthSeconds = 1 / (bpm / 60)
  bassNoteLengthSeconds = beatLengthSeconds * bassNoteLength
  restLengthSeconds = beatLengthSeconds * (1-bassNoteLength)


  beats = tables$beats %>%
    filter(melid == melid_) %>%
    left_join(midiNoteTable, by=c("bass_pitch"="MIDI Note")) %>%
    mutate(Note = paste0(Note, Octave)) %>%
    select(onset, Note)

  melodyWithRests = data.frame(onset=as.numeric(), duration=as.numeric(), Note=as.character())

  nextNoteTime = beats$onset[1]
  restRow = data.frame(onset=0, duration=nextNoteTime, Note="X")
  melodyWithRests = rbind(melodyWithRests, restRow)

  for (n in 1:nrow(beats)) {
    nextNoteTime = beats$onset[n+1] - beats$onset[n]
    if (is.na(nextNoteTime)) {
      nextNoteTime=1
    }
    noteRow = data.frame(onset=beats$onset[n], duration=bassNoteLengthSeconds, Note=beats$Note[n])
    restRow = data.frame(onset=beats$onset[n] + bassNoteLengthSeconds, duration=restLengthSeconds, Note="X")
    melodyWithRests = rbind(melodyWithRests, noteRow, restRow)
  }

  return (melodyWithRests)
}

generateSnareNotes <- function(melid_, snareBeats=c(2, 4), snareLengthSeconds=0.05) {
  beats = tables$beats %>%
    filter(melid == melid_) %>%
    filter(beat %in% snareBeats) %>%
    select(onset)

  melodyWithRests = data.frame(onset=as.numeric(), duration=as.numeric(), Note=as.character())

  nextNoteTime = beats$onset[1]
  restRow = data.frame(onset=0, duration=nextNoteTime, Note="X")
  melodyWithRests = rbind(melodyWithRests, restRow)

  for (n in 1:nrow(beats)) {
    nextNoteTime = beats$onset[n+1] - beats$onset[n]
    if (is.na(nextNoteTime)) {
      nextNoteTime=1
    }
    noteRow = data.frame(onset=beats$onset[n], duration=snareLengthSeconds, Note="C")
    restRow = data.frame(onset=beats$onset[n]+snareLengthSeconds, duration=nextNoteTime-snareLengthSeconds, Note="X")
    melodyWithRests = rbind(melodyWithRests, noteRow, restRow)
  }

  return (melodyWithRests)
}

getSoloMetaData <- function(melid){
  soloMetadata = tables$solo_info %>%
    filter(melid==!!melid) %>%
    left_join(tables$track_info, by=c("trackid", "recordid", "compid"))

  return(soloMetadata)
}

getSoloMelody <- function(melid) {
  soloMetadata = getSoloMetaData(melid)

  soloKey = soloMetadata$key
  soloKeyCenter = substr(soloKey, 1, 1)

  scaleDegreesLonger = scaleDegrees %>%
    pivot_longer(cols = !Degree, names_to="Key", values_to="Note")

  soloScaleDegrees = scaleDegreesLonger %>%
    filter(Key==soloKeyCenter) %>%
    left_join(enharmonicNotes, by="Note")

  soloMelody = tables$melody %>%
    filter(melid==!!melid) %>%
    left_join(midiNoteTable, by=c("pitch"="MIDI Note")) %>%
    left_join(soloScaleDegrees %>%
                select(c("Degree", "Enharmonic")),
              by=c("Note"="Enharmonic")) %>%
    mutate(Degree = fct_relevel(Degree, "1", "b2", "2", "b3", "3", "4", "#4", "5", "b6", "6", "b7", "7"))

  return(soloMelody)
}

getStartOnset <- function(melid) {
  beats = tables$beats %>%
    filter(melid == !!melid)
  return (min(beats$onset))
}

getBarTimes <- function(melid) {
  barDurs = tables$beats %>%
    filter(melid == !!melid) %>%
    filter(beat == 1) %>%
    select(onset, bar) %>%
    mutate(duration = lead(onset, 1) - onset)

  return (barDurs)
}

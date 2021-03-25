library(RSQLite)
library(tidyverse)
library(audio)
library(shiny)

source("helpers/metadata.R")
source("helpers/musicTheoryFuncs.R")
source("helpers/noteGeneration.R")
source("helpers/waveGeneration.R")
source("helpers/playMusic.R")

filename <- "data/wjazzd.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

tables = list()
for (table in dbListTables(db)) {
  tables[[table]] = dbReadTable(db, table)
}

# Import the midi note table
midiNoteTable = read_csv("data/midiNoteTable.csv")
scaleDegrees = read_csv("data/scaleDegrees.csv")
enharmonicNotes = read_csv("data/enharmonicNotes.csv")

# melid = 222  # giant steps
# melid = 323  # so what
# melid = 93 # joy spring
# melid = 431  # footprints
# melid = 133  # anthropology
# melid = 63  # ornithology
# melid = 74  # there will never be another you
# melid = 227  # my favourite things
# melid = 52  # billie's bounce


# wavFile = createWavFile(342, "audio")
# audioInstance = playWavFile(wavFile)
#
# pause(audioInstance)


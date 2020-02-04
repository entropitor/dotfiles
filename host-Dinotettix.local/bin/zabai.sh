#!/bin/bash

function fix() {
  yabai -m space 1 --label "@:1"
  yabai -m space 2 --label "@:2"
  yabai -m space 3 --label "@:3"
  yabai -m space 4 --label "@:4"
  yabai -m space 5 --label "@:5"
  yabai -m space 6 --label "@:6"
  yabai -m space 7 --label "@:7"
  yabai -m space 8 --label "@:8"
  yabai -m space 9 --label "@:9"
  yabai -m space 10 --label "@:0"
}

function reload() {
  # osascript -e 'tell application id "tracesOf.Uebersicht" to refresh widget id "nibar-spaces-primary-jsx"'
  # osascript -e 'tell application id "tracesOf.Uebersicht" to refresh widget id "nibar-spaces-secondary-jsx"'
  # osascript -e 'tell application id "tracesOf.Uebersicht" to refresh widget id "nibar-spaces-tertiary-jsx"'

  osascript -e 'tell application id "tracesOf.Uebersicht" to refresh widget id "yabar-primary-display-jsx"'
  osascript -e 'tell application id "tracesOf.Uebersicht" to refresh widget id "yabar-secondary-display-jsx"'
  osascript -e 'tell application id "tracesOf.Uebersicht" to refresh widget id "yabar-tertiary-display-jsx"'
}

"$@"

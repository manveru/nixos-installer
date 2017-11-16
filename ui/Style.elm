module Style exposing (..)

import Material.Grid exposing (Device(..), cell, grid, size)
import Material.Options as Options


fullWidth : Options.Style a
fullWidth =
    Material.Grid.size All 12

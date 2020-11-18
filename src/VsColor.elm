module VsColor exposing (..)

import Element
import Html.Attributes

type VsColor
  = Background
  | Foreground
  | Black
  | Blue
  | BrightBlack
  | BrightBlue
  | BrightCyan
  | BrightGreen
  | BrightMagenta
  | BrightRed
  | BrightWhite
  | BrightYellow
  | Cyan
  | Green
  | Magenta
  | Red
  | White
  | Yellow

vsColor : VsColor -> String
vsColor color =
  case color of 
    Background    ->
        "var(--vscode-terminal-background, var(--vscode-editor-background, black))"
    Foreground    ->
        "var(--vscode-terminal-foreground, var(--vscode-editor-foreground, white))"
    Black         -> "var(--vscode-terminal-ansiBlack, black)"
    Blue          -> "var(--vscode-terminal-ansiBlue, blue)"
    BrightBlack   -> "var(--vscode-terminal-ansiBrightBlack, black)"
    BrightBlue    -> "var(--vscode-terminal-ansiBrightBlue, blue)"
    BrightCyan    -> "var(--vscode-terminal-ansiBrightCyan, cyan)"
    BrightGreen   -> "var(--vscode-terminal-ansiBrightGreen, green)"
    BrightMagenta -> "var(--vscode-terminal-ansiBrightMagenta, magenta)"
    BrightRed     -> "var(--vscode-terminal-ansiBrightRed, red)"
    BrightWhite   -> "var(--vscode-terminal-ansiBrightWhite, white)"
    BrightYellow  -> "var(--vscode-terminal-ansiBrightYellow, yellow)"
    Cyan          -> "var(--vscode-terminal-ansiCyan, cyan)"
    Green         -> "var(--vscode-terminal-ansiGreen, green)"
    Magenta       -> "var(--vscode-terminal-ansiMagenta, magenta)"
    Red           -> "var(--vscode-terminal-ansiRed, red)"
    White         -> "var(--vscode-terminal-ansiWhite, white)"
    Yellow        -> "var(--vscode-terminal-ansiYellow, yellow)"

fontColor : VsColor -> Element.Attribute msg
fontColor color =
  Element.htmlAttribute <|
    Html.Attributes.style "color" (vsColor color)

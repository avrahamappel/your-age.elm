module Main exposing (..)

import Browser
import Html exposing (h1, text)

main : Program () () msg
main = Browser.sandbox { init = (), update = update, view = view }

update : a -> b -> ()
update _ _ = ()

view : a -> Html.Html msg
view _ = h1 [] [text "Hello World!"]

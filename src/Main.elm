module Main exposing (..)

import Browser
import Html exposing (div, input, label, p, text)
import Html.Attributes exposing (for, name, type_)


type alias YourBirthday =
    { name : String, birthday : Maybe String }


main : Program () YourBirthday msg
main =
    Browser.sandbox { init = init, update = update, view = view }


init : YourBirthday
init =
    { name = "", birthday = Nothing }


update : a -> b -> b
update _ yb =
    yb


view : YourBirthday -> Html.Html msg
view model =
    let
        form =
            [ p [] [ text "Type your name and birthday" ]
            , label [ for "name" ] [ text "Name" ]
            , input [ name "name" ] []
            , label [ for "birthday" ] [ text "Birthday" ]
            , input [ type_ "date", name "birthday" ] []
            ]

        years =
            ""

        months =
            ""

        days =
            ""

        hours =
            ""

        minutes =
            ""

        seconds =
            ""

        output =
            [ p [] [ text ("Hello " ++ model.name ++ "!") ]
            , p [] [ text "You are:" ]
            , p [] [ text (years ++ " years old") ]
            , p [] [ text (months ++ " months old") ]
            , p [] [ text (days ++ " days old") ]
            , p [] [ text (hours ++ " hours old") ]
            , p [] [ text (minutes ++ " minutes old") ]
            , p [] [ text (seconds ++ " seconds old") ]
            ]
    in
    div []
        (case model.birthday of
            Just _ ->
                if not (String.isEmpty model.name) then
                    form ++ output

                else
                    form

            Nothing ->
                form
        )

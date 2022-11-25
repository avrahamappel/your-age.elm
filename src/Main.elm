module Main exposing (main)

import Browser
import Html exposing (Html, div, input, label, p, text)
import Html.Attributes exposing (for, name, type_)
import Task
import Time exposing (Posix)


type alias YourAge =
    { name : String, birthday : Maybe Posix, now : Posix }


type Msg
    = UpdateCurrentTime Posix
    | UpdateName
    | UpdateBirthday


main : Program () YourAge Msg
main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }


init : () -> ( YourAge, Cmd Msg )
init _ =
    ( { name = "", birthday = Nothing, now = Time.millisToPosix 0 }
    , Task.perform UpdateCurrentTime Time.now
    )


subscriptions : YourAge -> Sub Msg
subscriptions _ =
    Time.every 1000 UpdateCurrentTime


update : a -> YourAge -> ( YourAge, Cmd msg )
update _ yb =
    ( yb, Cmd.none )


view : YourAge -> Html.Html msg
view model =
    let
        form =
            [ p [] [ text "Type your name and birthday" ]
            , label [ for "name" ] [ text "Name" ]
            , input [ name "name" ] []
            , label [ for "birthday" ] [ text "Birthday" ]
            , input [ type_ "date", name "birthday" ] []
            ]
    in
    div []
        (case model.birthday of
            Just bd ->
                if not (String.isEmpty model.name) then
                    form ++ output model.name bd model.now

                else
                    form

            Nothing ->
                form
        )


output : String -> Posix -> Posix -> List (Html msg)
output nm bd now =
    let
        duration =
            Time.posixToMillis now - Time.posixToMillis bd

        seconds =
            duration // 1000

        minutes =
            seconds // 60

        hours =
            seconds // 60

        days =
            hours // 24

        months =
            days // 30

        years =
            days // 365
    in
    [ p [] [ text ("Hello " ++ nm ++ "!") ]
    , p [] [ text "You are:" ]
    , p [] [ text (String.fromInt years ++ " years old") ]
    , p [] [ text (String.fromInt months ++ " months old") ]
    , p [] [ text (String.fromInt days ++ " days old") ]
    , p [] [ text (String.fromInt hours ++ " hours old") ]
    , p [] [ text (String.fromInt minutes ++ " minutes old") ]
    , p [] [ text (String.fromInt seconds ++ " seconds old") ]
    ]

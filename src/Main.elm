module Main exposing (main)

import Array
import Browser
import Calendar
import Clock
import DateTime exposing (DateTime)
import Html exposing (Attribute, Html, b, div, h2, input, label, p, text)
import Html.Attributes exposing (for, name, type_, value)
import Html.Events exposing (on, targetValue)
import Json.Decode
import String exposing (join, padLeft, split)
import Task
import Time exposing (Month(..), Posix)


type alias YourAge =
    { name : String, birthday : Maybe DateTime, now : Posix }


type Msg
    = UpdateCurrentTime Posix
    | UpdateName String
    | UpdateBirthday String


main : Program () YourAge Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( YourAge, Cmd Msg )
init _ =
    ( { name = "", birthday = Nothing, now = Time.millisToPosix 0 }
    , Task.perform UpdateCurrentTime Time.now
    )


subscriptions : YourAge -> Sub Msg
subscriptions _ =
    Time.every 1000 UpdateCurrentTime


update : Msg -> YourAge -> ( YourAge, Cmd msg )
update msg ya =
    case msg of
        UpdateCurrentTime now ->
            ( { ya | now = now }, Cmd.none )

        UpdateName name ->
            ( { ya | name = name }, Cmd.none )

        UpdateBirthday bd ->
            ( { ya | birthday = isoStringToDateTime bd }, Cmd.none )


dateTimeToIsoString : DateTime -> String
dateTimeToIsoString dt =
    let
        d =
            DateTime.getDate dt
    in
    join "-"
        [ Calendar.getYear d
            |> String.fromInt
            |> padLeft 4 '0'
        , Calendar.getMonth d
            |> Calendar.monthToInt
            |> String.fromInt
            |> padLeft 2 '0'
        , Calendar.getDay d
            |> String.fromInt
            |> padLeft 2 '0'
        ]


isoStringToDateTime : String -> Maybe DateTime
isoStringToDateTime =
    let
        parts =
            split "-" >> List.filterMap String.toInt

        partsTuple dt =
            case parts dt of
                [ y, m, d ] ->
                    Array.get (m - 1) Calendar.months
                        |> Maybe.map (\month -> ( y, month, d ))

                _ ->
                    Nothing

        date ( y, m, d ) =
            Calendar.fromRawParts
                { year = y
                , month = m
                , day = d
                }

        dateTime dt =
            let
                time =
                    0
                        |> Time.millisToPosix
                        |> Clock.fromPosix
            in
            DateTime.fromDateAndTime dt time
    in
    partsTuple
        >> Maybe.andThen date
        >> Maybe.map dateTime



{- "onchange" listener for input elements -}


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (Json.Decode.map tagger targetValue)


view : YourAge -> Html.Html Msg
view model =
    let
        birthdayFormValue =
            model.birthday
                |> Maybe.map dateTimeToIsoString
                |> Maybe.withDefault ""

        form =
            [ p [] [ text "Type your name and birthday" ]
            , label [ for "name" ] [ text "Name" ]
            , input [ name "name", value model.name, onChange UpdateName ] []
            , label [ for "birthday" ] [ text "Birthday" ]
            , input
                [ type_ "date"
                , name "birthday"
                , value birthdayFormValue
                , onChange UpdateBirthday
                ]
                []
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


output : String -> DateTime -> Posix -> List (Html msg)
output nm bd now =
    let
        _ =
            Debug.log "now" now

        duration =
            Time.posixToMillis now - Time.posixToMillis (DateTime.toPosix bd |> Debug.log "bd")

        seconds =
            duration // 1000

        minutes =
            seconds // 60

        hours =
            minutes // 60

        days =
            hours // 24

        years =
            days // 365

        months =
            years * 12
    in
    [ h2 [] [ text ("Hello " ++ nm ++ "!") ]
    , p [] [ text "You are:" ]
    , p [] [ b [] [ text (String.fromInt years) ], text " years old" ]
    , p [] [ b [] [ text (String.fromInt months) ], text " months old" ]
    , p [] [ b [] [ text (String.fromInt days) ], text " days old" ]
    , p [] [ b [] [ text (String.fromInt hours) ], text " hours old" ]
    , p [] [ b [] [ text (String.fromInt minutes) ], text " minutes old" ]
    , p [] [ b [] [ text (String.fromInt seconds) ], text " seconds old" ]
    ]

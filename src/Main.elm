module Main exposing (main)

import Array
import Browser
import Browser.Navigation as Nav
import Calendar
import Clock
import DateTime exposing (DateTime)
import Html exposing (Html, b, h2, input, label, p, text)
import Html.Attributes exposing (for, name, type_, value)
import Html.Events exposing (onInput)
import String exposing (join, padLeft, split)
import Task
import Time exposing (Month(..), Posix)
import Url
import Url.Builder as UB


type alias Model =
    { name : String, birthday : Maybe DateTime, now : Posix, key : Nav.Key }


type Msg
    = UpdateCurrentTime Posix
    | UpdateName String
    | UpdateBirthday String
    | None


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = \_ -> None
        , onUrlChange = \_ -> None
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        params =
            url.query
                |> Maybe.map (String.split "&" >> List.map (String.split "="))

        getParam k =
            params
                |> Maybe.map
                    (List.filter
                        (List.head
                            >> Maybe.map ((==) k)
                            >> Maybe.withDefault False
                        )
                    )
                |> Maybe.andThen List.head
                |> Maybe.map (List.drop 1)
                |> Maybe.andThen List.head
    in
    ( { name = getParam "name" |> Maybe.withDefault ""
      , birthday = getParam "birthday" |> Maybe.andThen isoStringToDateTime
      , now = Time.millisToPosix 0
      , key = key
      }
    , Task.perform UpdateCurrentTime Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 UpdateCurrentTime


update : Msg -> Model -> ( Model, Cmd msg )
update msg ya =
    let
        updateQueryString n b =
            Nav.replaceUrl ya.key
                (UB.relative []
                    (UB.string "name" n
                        :: List.filterMap (Maybe.map (UB.string "birthday")) [ b ]
                    )
                )
    in
    case msg of
        UpdateCurrentTime now ->
            ( { ya | now = now }, Cmd.none )

        UpdateName name ->
            ( { ya | name = name }
            , updateQueryString name (Maybe.map dateTimeToIsoString ya.birthday)
            )

        UpdateBirthday bd ->
            ( { ya | birthday = isoStringToDateTime bd }
            , updateQueryString ya.name (Just bd)
            )

        None ->
            ( ya, Cmd.none )


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


view : Model -> Browser.Document Msg
view model =
    let
        birthdayFormValue =
            model.birthday
                |> Maybe.map dateTimeToIsoString
                |> Maybe.withDefault ""

        form =
            [ p [] [ text "Type your name and birthday" ]
            , label [ for "name" ] [ text "Name" ]
            , input [ name "name", value model.name, onInput UpdateName ] []
            , label [ for "birthday" ] [ text "Birthday" ]
            , input
                [ type_ "date"
                , name "birthday"
                , value birthdayFormValue
                , onInput UpdateBirthday
                ]
                []
            ]
    in
    { title = "Your Age"
    , body =
        case model.birthday of
            Just bd ->
                if not (String.isEmpty model.name) then
                    form ++ output model.name bd model.now

                else
                    form

            Nothing ->
                form
    }


output : String -> DateTime -> Posix -> List (Html msg)
output nm bd now =
    let
        duration =
            Time.posixToMillis now - Time.posixToMillis (DateTime.toPosix bd)

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
    , p [] [ b [] [ text (formattedNumber years) ], text " years old" ]
    , p [] [ b [] [ text (formattedNumber months) ], text " months old" ]
    , p [] [ b [] [ text (formattedNumber days) ], text " days old" ]
    , p [] [ b [] [ text (formattedNumber hours) ], text " hours old" ]
    , p [] [ b [] [ text (formattedNumber minutes) ], text " minutes old" ]
    , p [] [ b [] [ text (formattedNumber seconds) ], text " seconds old" ]
    ]


formattedNumber : Int -> String
formattedNumber n =
    n
        |> String.fromInt
        |> String.toList
        |> List.reverse
        |> chunks 3
        |> List.intersperse [ ',' ]
        |> List.foldr List.append []
        |> List.reverse
        |> String.fromList


chunks : Int -> List a -> List (List a)
chunks n xs =
    (if List.length xs < n then
        [ xs ]

     else
        List.take n xs :: (chunks n <| List.drop n xs)
    )
        |> List.filter (not << List.isEmpty)

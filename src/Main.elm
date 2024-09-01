module Main exposing (..)

-- Input a user name and password. Make sure the password matches.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/forms.html
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , valid : Valid
    }


init : Model
init =
    Model "" "" "" Success



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String


type Valid
    = Success
    | TooShort
    | NotTheSame

isShort : String -> Valid
isShort password =
    if String.length password > 5 then
        Success

    else
        TooShort


isMatch password1 password2 =
    if password1 == password2 then
        Success

    else
        NotTheSame


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Password password ->
            { model | password = password, valid = isShort password }

        PasswordAgain password ->
            if model.valid == TooShort then
                model
            else
                { model | passwordAgain = password, valid = isMatch model.password password }

-- VIEW

view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    case model.valid of
        Success ->
            div [ style "color" "green" ] [ text "OK" ]

        NotTheSame ->
            div [ style "color" "red" ] [ text "Passwords are not the same!" ]

        TooShort ->
            div [ style "color" "red" ] [ text "Password is too short!" ]

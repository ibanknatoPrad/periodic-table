import Browser
import Debug
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

-- MODEL

type Model
  = Failure Http.Error
  | Loading
  | Success (List Element)

init : () -> (Model, Cmd Msg)
init _ =
  (Loading, loadPeriodicTable)

-- UPDATE

type Msg = Loaded (Result Http.Error (List Element))

update msg model =
  case msg of
    Loaded result ->
      case result of 
        Ok periodicTable ->
          (Success periodicTable, Cmd.none)
        
        Err blah ->
          (Failure blah, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view model =
  div []
    [ h2 [] [ text "Periodic Table"]
    , viewPeriodicTable model
    ]

viewPeriodicTable : Model -> Html Msg
viewPeriodicTable model =
  case model of
    Failure error ->
      div [] [ text (Debug.toString error)]
    
    Loading ->
      text "Loading..."
    
    Success periodicTable ->
      ul []
        (List.map (\e -> li [] [ text e.name ]) periodicTable)

-- HTTP

loadPeriodicTable : Cmd Msg
loadPeriodicTable =
  Http.get
    -- https://github.com/Bowserinator/Periodic-Table-JSON
    { url = "/data/Periodic-Table-JSON/PeriodicTableJSON.json"
    , expect = Http.expectJson Loaded periodicTableDecoder
    }


type alias Element = 
  { name : String
  , appearance : Maybe String
  , atomicMass : Float
  , boilingPoint : Maybe Float
  , category : String
  , color : Maybe String
  , density : Maybe Float
  , discoveredBy : Maybe String
  , meltingPoint : Maybe Float
  , molarHeat : Maybe Float
  , namedBy : Maybe String
  , number : Int
  , period : Int
  , phase : String
  , source : String
  , spectralImage : Maybe String
  , summary : String
  , symbol : String
  , xpos : Int
  , ypos : Int
  , shells : List Int
  , electronConfiguration : String
  , electronAffinity : Maybe Float
  , electronegativityPauling: Maybe Float
  , ionizationEnergies: List Float
  }

periodicTableDecoder : Decoder (List Element)
periodicTableDecoder =
  field "elements" (list elementDecoder)

elementDecoder : Decoder Element
elementDecoder =
  Decode.succeed Element
    |> required "name" string
    |> required "appearance" (nullable string)
    |> required "atomic_mass" float
    |> required "boil" (nullable float)
    |> required "category" string
    |> required "color" (nullable string)
    |> required "density" (nullable float)
    |> required "discovered_by" (nullable string)
    |> required "melt" (nullable float)
    |> required "molar_heat" (nullable float)
    |> required "named_by" (nullable string)
    |> required "number" int
    |> required "period" int
    |> required "phase" string
    |> required "source" string
    |> required "spectral_img" (nullable string)
    |> required "summary" string
    |> required "symbol" string
    |> required "xpos" int
    |> required "ypos" int
    |> required "shells" (list int)
    |> required "electron_configuration" string
    |> required "electron_affinity" (nullable float)
    |> required "electronegativity_pauling" (nullable float)
    |> required "ionization_energies" (list float)

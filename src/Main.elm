import Browser
import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Round


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
        
        Err error ->
          (Failure error, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

view model =
  div []
    [ h2 [ style "text-align" "center"] [ text "Periodic Table"]
    , case model of
        Failure error ->
          div [] [ text (Debug.toString error)]
    
        Loading ->
          text "Loading..."
    
        Success periodicTable ->
          viewPeriodicTable periodicTable
    ]

viewPeriodicTable : List Element -> Html Msg
viewPeriodicTable periodicTable =
  div []
    (List.map (\element -> viewElement element) periodicTable)

viewElement : Element -> Html Msg
viewElement element =
  div [ style "float" "left"
      , style "border-style" "solid"
      , style "border-width" "thin"
      , style "width" "120px"
      , style "margin" "2px"
      , style "font-family" "Arial"
      ]
      [ p
          [style "margin" "4px 4px 0px"]
          [text (String.fromInt element.number)]
      , p
          [ style "margin" "0px", style "font-weight" "bold", style "font-size" "40px", style "text-align" "center" ]
          [text element.symbol]
      , p
          [ style "margin" "0px", style "text-align" "center", style "font-size" "14px" ]
          [text (Round.round 3 element.atomicMass)]
      , p
          [ style "margin" "4px 4px 10px", style "font-weight" "bold",  style "text-align" "center" ]
          [text element.name]
      ]

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

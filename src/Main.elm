module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utils


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- Model


type alias Model =
    { items : List Item
    , item : Item
    , priceField : String
    , diameterField : String
    , nextId : Int
    }


type alias Item =
    { price : Float
    , diameter : Float
    , isBestDeal : Bool
    , id : Int
    }


type Msg
    = AddItem
    | DeleteItem Item
    | UpdatePriceField String
    | UpdateDiameterField String
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { items = []
    , item = initialItem
    , priceField = ""
    , diameterField = ""
    , nextId = 2
    }


initialItem : Item
initialItem =
    { price = 0
    , diameter = 0
    , isBestDeal = False
    , id = 1
    }


pricePerUnit : Item -> Float
pricePerUnit item =
    Utils.costByDiameter item.price item.diameter


getBestDeal : List Item -> Float
getBestDeal items =
    items
        |> List.map pricePerUnit
        |> List.sort
        |> List.head
        |> Maybe.withDefault 0


setIsBestDeal : Float -> Item -> Item
setIsBestDeal bestDeal item =
    { item | isBestDeal = pricePerUnit item == bestDeal }


newItemsWithBestDeal : List Item -> List Item
newItemsWithBestDeal items =
    List.map (setIsBestDeal (getBestDeal items)) items


withoutItem : Item -> Item -> Bool
withoutItem item comparedItem =
    item.id /= comparedItem.id



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddItem ->
            ( { initialModel
                | items = newItemsWithBestDeal (model.item :: model.items)
                , item = { initialItem | id = model.nextId }
                , nextId = model.nextId + 1
              }
            , Cmd.none
            )

        DeleteItem item ->
            ( { model
                | items = newItemsWithBestDeal <| List.filter (withoutItem item) model.items
              }
            , Cmd.none
            )

        UpdatePriceField text ->
            let
                item =
                    model.item

                updatedItem =
                    { item | price = Utils.stringToFloat text }
            in
                ( { model
                    | item = updatedItem
                    , priceField = text
                  }
                , Cmd.none
                )

        UpdateDiameterField text ->
            let
                item =
                    model.item

                updatedItem =
                    { item | diameter = Utils.stringToFloat text }
            in
                ( { model
                    | item = updatedItem
                    , diameterField = text
                  }
                , Cmd.none
                )

        _ ->
            ( model, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div [ class "pizza" ]
        [ Html.form [ class "form", onSubmit AddItem ]
            [ div []
                [ label [ class "form__label" ] [ text "Diameter" ]
                , input
                    [ type_ "number"
                    , class "form__input"
                    , autofocus True
                    , value model.diameterField
                    , onInput UpdateDiameterField
                    ]
                    []
                ]
            , div []
                [ label [ class "form__label" ] [ text "Price" ]
                , input
                    [ type_ "number"
                    , class "form__input"
                    , value model.priceField
                    , onInput UpdatePriceField
                    ]
                    []
                ]
            , div []
                [ button [ type_ "submit", class "form__submit" ] [ text "Submit" ]
                ]
            ]
        , (itemTable model.items)
        ]


itemTable : List Item -> Html Msg
itemTable items =
    if List.isEmpty items then
        text ""
    else
        table []
            [ thead []
                [ th [ class "border--right align--center" ] [ text "Diameter" ]
                , th [ class "border--right align--center" ] [ text "Price" ]
                , th [ class "align--center" ] [ text "Price / square unit" ]
                ]
            , tbody []
                (List.map itemTableRow items)
            ]


itemTableRow : Item -> Html Msg
itemTableRow item =
    tr [ class "align--center" ]
        [ td [ class "border--right" ] [ text (Utils.roundFloat item.diameter) ]
        , td [ class "border--right" ] [ text (Utils.roundFloat item.price) ]
        , td [] [ text (Utils.roundFloat (pricePerUnit item)) ]
        , td []
            [ button [ onClick (DeleteItem item) ]
                [ text "Remove" ]
            ]
        , td [] [ text (bestDealText item.isBestDeal) ]
        ]


bestDealText : Bool -> String
bestDealText bool =
    if bool then
        "(best deal)"
    else
        ""

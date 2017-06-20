module Anigram.Controls exposing (..)

import Dict
import List.Extra as List
import Maybe.Extra as Maybe

import DragDrop

import Json.Decode as Json
import Json.Encode exposing (encode)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Color exposing (..)
import ColorMath exposing (colorToHex)

import FontAwesome as Icon

import Anigram.Common exposing (..)
import Anigram.Object as Obj exposing (defaultStyle, defaultTextStyle)
import Anigram.Frames as Frames
import Anigram.Store as Store
import Anigram.Change as Change
import Anigram.StyleSets as StyleSets
import Anigram.Decode as Decode exposing (decodeChange)
import Anigram.Encode as Encode exposing (encodeChange)

model =
  [ addObjectControl "Add a Circle" Icon.circle <| Shape Circle
  , addObjectControl "Add a Square" Icon.square <| Shape Square
  , addObjectControl "Add a Star" Icon.star <| Shape Star
  , addObjectControl "Add an Arrow" Icon.long_arrow_right <| Arrow
  , addObjectControl "Add an Arc Arrow" Icon.reply <| ArcArrow 100
  , addObjectControl "Add Text" Icon.file_text <| Text "Add Text here" defaultTextStyle
  , listControl "Style Sets" Icon.paint_brush defaultStyleSet <| styleSets StyleSets.sets
  , buttonControl "Modify style set" Icon.edit ModifyStyleSet
  , inputControl "Add style set" Icon.plus_square
  , buttonControl "Show" Icon.eye <| Selection <| Hide False 
  , buttonControl "Hide" Icon.eye_slash <| Selection <| Hide True
  , buttonControl "Duplicate" Icon.copy Duplicate
  , colorControl 0 "Fill color" Color.green FillSelector
  , colorControl 1 "Stroke color" Color.grey StrokeSelector
  , listControl "Stroke width" Icon.circle_o defaultStrokeWidth strokeWidths
  , colorControl 2 "Text color" Color.red TextSelector
  , listControl "Text size" Icon.text_height defaultTextSize textSizes
  , buttonControl "Add Frame" Icon.plus_square AddFrame
  , buttonControl "Save" Icon.cloud_upload SaveAnigram
  , buttonControl "Load" Icon.cloud_download LoadAnigram
  ]

buttonControl tooltip icon message =
  newButton tooltip (icon Color.black 20) message

listControl tooltip icon default choices =
  newListPicker tooltip (icon Color.black 20) default choices

addObjectControl tooltip icon object =
  newObjectAdder tooltip (icon Color.black 20) object

colorControl id tooltip color kind =
  newColorSelector id tooltip color kind

inputControl tooltip icon =
  newStyleSetInput tooltip (icon Color.black 20)

newButton tooltip icon message =
  Button
  { tooltip = tooltip
  , icon = icon
  , message = message
  }

newListPicker tooltip icon default choices =
  ListPicker
  { tooltip = tooltip
  , icon = icon
  , choices = choices
  , choice = default
  }

newObjectAdder tooltip icon object =
  ObjectAdder
  { tooltip = tooltip
  , icon = icon
  , object = object
  }

newColorSelector id tooltip color kind =
  ColorSelector
  { id = id
  , kind = kind
  , tooltip = tooltip
  , color = color
  , open = False
  }

newStyleSetInput tooltip icon =
  StyleSetInput
  { tooltip = tooltip
  , icon = icon
  , input = ""
  }

defaultStrokeWidth =
  StrokeWidth <| defaultStyle.strokeWidth

strokeWidths =
  List.map
    (\width -> (toString width, StrokeWidth width))
    [1, 2, 3, 5, 8, 12, 24, 40]

defaultTextSize =
  TextSizeTo defaultTextStyle.size

textSizes =
  List.map
    (\size -> (toString size, TextSizeTo size))
    [8, 12, 16, 24, 36, 48, 72, 106]

defaultStyleSet =
  AddStyleSet "Default"

styleSets sets =
  sets
    |> Dict.keys
    |> List.map (\key -> (key, AddStyleSet key))

currentStyleSet model =
  List.getAt 6 model -- This could be done better, maybe controls should be a record instead of a list
    |> Maybe.andThen (\control ->
      case control of
        ListPicker control ->
          case control.choice of
            AddStyleSet styleSet -> Just styleSet
            _ -> Nothing
        _ -> Nothing
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    updateCursor new old =
      -- Dragging may only be continued if dragging has already started
      case (new, old) of
        (DragMode (DragDrop.StartDrag _), _) -> new
        (DragMode (DragDrop.Drag _ _), DragMode _) -> new
        (DragMode _, _) -> old
        (DragSizeMode _ _ (DragDrop.StartDrag _), _) -> new
        (DragSizeMode _ _ (DragDrop.Drag _ _), DragSizeMode _ _ _) -> new
        (DragSizeMode _ _ _, _) -> old
        _ -> new

    setAnigram anigram model =
      { model | objects = Frames.objectIds anigram.frames |> List.map Obj.newState, frames = anigram.frames, styleSets = anigram.styleSets }

    styleSet =
      case selectionStyleSet model of
        Just "" -> Nothing
        styleSet -> styleSet
    expandStyleSet changes =
      case styleSet of
        Just name ->
          List.remove (AddStyleSet name) changes
          |> (++) (Maybe.withDefault [] <| Dict.get name model.styleSets)
        Nothing -> changes
    newStyleSet =
      currentChanges model
      |> Maybe.withDefault []
      |> expandStyleSet
    insertStyleSet styleSet =
      case styleSet of
        Just name -> Dict.insert name newStyleSet model.styleSets
        Nothing -> model.styleSets
    setChanges styleSet ids frame =
      case styleSet of
        Just name ->
          List.foldl (flip Dict.insert [AddStyleSet name]) frame ids
        Nothing ->
          frame
    deduplicateStyleSet styleSet frames =
      List.updateAt model.frameIndex (setChanges styleSet <| Obj.selectedIds model.objects) frames
        |> Maybe.withDefault frames
    modifyStyleSet model =
        { model
        | styleSets = insertStyleSet styleSet
        , frames = deduplicateStyleSet styleSet model.frames
        }
    addStyleSet name model =
      { model
      | styleSets = insertStyleSet <| Just name
      , frames = deduplicateStyleSet (Just name) model.frames
      }
      |> updateStyleSetControls name
    updateStyleSetControls styleSet model =
      { model | controls = List.map (updateStyleSetPicker styleSet model.styleSets) model.controls }
    updateStyleSetPicker choice choices control =
      case control of
        ListPicker control ->
          case control.choice of
            AddStyleSet _ ->
              ListPicker { control | choices = styleSets choices, choice = AddStyleSet choice }
            _ ->
              ListPicker control
        control -> control

    updateStyleSetInput input model =
      { model | controls = List.map (updateStyleSetControl input) model.controls }
    updateStyleSetControl input control =
      case control of
        StyleSetInput control ->
          StyleSetInput { control | input = input }
        control -> control

    setChoiceOf choice =
      { model | controls = List.map (setChoice choice) model.controls }
    setChoice choice control =
      case control of
        ListPicker picker ->
          if List.member choice <| List.map Tuple.second picker.choices then
            ListPicker { picker | choice = choice }
          else
            control
        _ -> control

    setColorOf kind color =
      { model | controls = List.map (setColor kind color) model.controls }
    setColor kind color control =
      case control of
        ColorSelector selector ->
          if selector.kind == kind then
            ColorSelector { selector | color = color }
          else
            control
        _ -> control

    openCloseAt id state model =
      { model | controls = List.map (openClose id state) model.controls }
    openClose id state control =
      case control of
        ColorSelector selector ->
          if selector.id == id then
            ColorSelector { selector | open = state }
          else
            control
        _ -> control

    closeAll model =
      { model | controls = List.map close model.controls }
    close control =
      case control of
        ColorSelector selector ->
          ColorSelector { selector | open = False }
        _ -> control
  in
    case msg of
      SetCursor mode ->
        ({ model | cursorMode = updateCursor mode model.cursorMode }, Cmd.none)
      SaveAnigram ->
        (model, Store.saveAnigram { frames = model.frames, styleSets = model.styleSets })
      LoadAnigram ->
        (model, Store.loadAnigram)
      AnigramLoaded anigram ->
        (setAnigram anigram model, Cmd.none)
      DeselectAll ->
        (closeAll model, Cmd.none)
      ModifyStyleSet ->
        (modifyStyleSet model, Cmd.none)
      NewStyleSet name ->
        (addStyleSet name model, Cmd.none)
      UpdateStyleSetInput input ->
        (updateStyleSetInput input model, Cmd.none)
      Selection (AddStyleSet styleSet) ->
        (setChoiceOf <| AddStyleSet styleSet, Cmd.none)
      Selection (Fill color) ->
        (setColorOf FillSelector color, Cmd.none)
      Selection (Stroke color) ->
        (setColorOf StrokeSelector color, Cmd.none)
      Control (OpenClose id state) ->
        (model |> closeAll |> openCloseAt id state, Cmd.none)
      PlaceObject objectType position ->
        ( { model
          | objects = model.objects ++
              [ (Obj.newState <| List.length model.objects)
              ]
          , cursorMode = DragSizeMode True (Right, Bottom) <| DragDrop.StartDrag position
          }
            |> Frames.addChangeToModelAt 0 (ChangeType objectType)
            |> Frames.addChangeToModelAt 0 (MoveTo position)
            |> Frames.addChangeToModelAt 0 (SizeTo { width = 0, height = 0 })
            |> Frames.addChangeToModelAt 0 (Hide True)
            |> Frames.addChangeToModelAt model.frameIndex (AddStyleSet <| Maybe.withDefault "Default" <| currentStyleSet model.controls)
            |> Frames.addChangeToModelAt model.frameIndex (Hide False)
        , Cmd.none)
      _ ->
        (closeAll model, Cmd.none)

currentChanges model =
  let
    getChanges ids frame = List.concat <| List.filterMap (\id -> Dict.get id frame) ids
    changesAt index frames ids = List.getAt index frames |> Maybe.map (getChanges ids)
  in
    model.objects
    |> Obj.selectedIds
    |> (\ids -> if ids == [] then Nothing else Just ids)
    |> Maybe.andThen (changesAt model.frameIndex model.frames)

selectionStyleSet model =
  currentChanges model
    |> Maybe.andThen (List.find Change.isAddStyleSet)
    |> Maybe.andThen (\change ->
      case change of
        AddStyleSet styleSet -> Just styleSet
        _ -> Nothing
    )

view model =
  nav
    [ style
      [ ("position", "absolute")
      , ("padding", "5px")
      , ("background-color", "#333")
      ]
    ]
    <| List.map (controlView model) model.controls

controlView model control =
  case control of
    Button control ->
      button
        [ title control.tooltip
        , onClick control.message 
        ]
        [ control.icon
        ]
    ListPicker control ->
      span
        [ title control.tooltip
        , style
          [ ("display", "inline-block")
          , ("box-sizing", "border-box")
          , ("height", "27px")
          , ("margin", "2px")
          , ("padding", "2px")
          , ("vertical-align", "bottom")
          , ("background-color", "#eee")
          , ("border", "1px solid grey")
          ]
        ]
        [ control.icon
        , select
          [ style
            [ ("width", "auto")
            , ("margin", "4px")
            , ("margin-top", "0px")
            , ("vertical-align", "bottom")
            , ("font-size", "14px")
            ]
          , on "change"
            <| Json.map
              (Maybe.withDefault NoOp << Maybe.map Selection << Maybe.join << Result.toMaybe << Json.decodeString decodeChange)
              (Json.at ["target", "value"] Json.string)
          ]
          <| List.map (\(label, choice) ->
            option
              [ value <| encode 0 <| encodeChange choice
              , case currentChanges model of
                  Just changes -> selected <| List.member choice changes
                  Nothing -> selected <| choice == (AddStyleSet <| Maybe.withDefault "" <| currentStyleSet model.controls)
              ]
              [ text label ]
          )
          <| control.choices
        ]
    ObjectAdder control ->
      button
        [ title control.tooltip
        , onClick <| AddObject control.object
        ]
        [ control.icon
        ]
    ColorSelector control ->
      span
        []
        [ button
          [ title control.tooltip
          , onClick <|
            case control.kind of
              FillSelector -> Selection <| Fill control.color
              StrokeSelector -> Selection <| Stroke control.color
              TextSelector -> Selection <| TextColor control.color
          , style
            [ ("margin-right", "0px")
            ]
            ]
            [ case control.kind of
              FillSelector -> Icon.dot_circle_o control.color 20
              StrokeSelector -> Icon.circle_o control.color 20
              TextSelector -> Icon.font control.color 20
            ]
        , button
          [ title control.tooltip
          , onClick (Control <| OpenClose control.id <| not control.open)
          , style
            [ ("margin-left", "0px")
            , ("padding-left", "0px")
            , ("padding-right", "0px")
            ]
          ]
          [ if control.open then
              Icon.caret_up black 20
            else
              Icon.caret_down black 20
          ]
        , if control.open then
            span
              [ style
                  [ ("position", "absolute")
                  , ("top", "34px")
                  , ("width", "320px")
                  , ("margin-left", "-26px")
                  , ("padding","5px")
                  , ("background-color", "#eee")
                  ]
              ]
              <| List.map (colorButton control) colors
          else
            text ""
        ]
    StyleSetInput control ->
      span
        [ title control.tooltip
        ]
        [ button
          [ onClick <| NewStyleSet control.input ]
          [ control.icon ]
        , input
          [ style
            [ ("width", "auto")
            , ("margin", "4px")
            , ("margin-top", "0px")
            , ("vertical-align", "bottom")
            , ("font-size", "14px")
            ]
          , placeholder "Styleset name"
          , onInput UpdateStyleSetInput
          ]
          []
        ]

colorButton control color =
  button
    [ title <| colorToHex color
    , onClick <|
      case control.kind of
        FillSelector -> Selection <| Fill color
        StrokeSelector -> Selection <| Stroke color
        TextSelector -> Selection <| TextColor color
    ]
    [ Icon.square color 20
    ]

colors =
  [ white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
  , red, orange, yellow, green, blue, purple, brown
  ]

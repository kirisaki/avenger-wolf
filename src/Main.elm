port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List
import Navigation
import String
import Task
import UrlParser
import Window exposing ( Size )

---- MODEL ----

type PageMode = PageSpread
              | PageSingle

type PageStatus = PageShow
                | PageVanish
                | PageHidden

type Language = Japanese
              | English

langEncode : Language -> String
langEncode lang =
    case lang of
        Japanese -> "jpn"
        English -> "eng"

countryDecode : String -> Language
countryDecode str =
    case str of
        "jpn" -> Japanese
        "eng" -> English
        _ -> Japanese
                
type alias Model =
    { page : Int
    , pageList : List String
    , pageMode : PageMode
    , language : Language
    , isModalShow : Bool
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init loc =
     { page = 0
     , pageList = []
     , pageMode = PageSpread
     , language = Japanese
     , isModalShow = False
     } ! [ Task.perform Resize Window.size
         , Task.perform SelectLanguage
             <| Task.succeed
             <| locationToLanguage loc ]
    
port title : String -> Cmd a

sizeToMode : Size -> PageMode
sizeToMode s =
    if (toFloat s.width) / (toFloat s.height) > 1
    then PageSpread
    else PageSingle
        
---- UPDATE ----


type Msg
    = PageNext
    | PagePrev
    | PageFirst
    | Page Int
    | ModalHide
    | ModalShow
    | Resize Size
    | Locate Navigation.Location
    | SelectLanguage Language

      
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        maxPage = List.length model.pageList - 1
        diff =
            case model.pageMode of
                PageSpread -> 2
                PageSingle -> 1
    in
        case msg of
            PageNext ->
                model ! [ Task.perform Page (Task.succeed <| model.page + diff) ]
            PagePrev ->
                model ! [ Task.perform Page (Task.succeed <| model.page - diff) ]
            Page n ->
                ( case model.pageMode of
                      PageSpread ->
                          if n <= 0
                          then { model | page = 0 }
                          else if n >= maxPage
                               then { model | page = maxPage - maxPage % 2 }
                               else { model | page = n - n % 2 }
                      PageSingle ->
                          if n <= 0
                          then { model | page = 0 }
                          else if n >= maxPage
                               then { model | page = maxPage }
                               else { model | page = n }
                ) ! []

            PageFirst ->
                { model | page = 0 } ! []
            Resize s ->
                let
                    newMode = sizeToMode s
                in
                    { model | pageMode = newMode }
                    ! [ Task.perform Page (Task.succeed model.page) ]
            ModalHide ->
                { model | isModalShow = False } ! []
            ModalShow ->
                { model | isModalShow = True } ! []
            Locate loc ->
                { model | language = locationToLanguage loc } ! []
            SelectLanguage lang ->
                { model | language = lang
                , pageList = List.map
                      ( toString
                            >> String.padLeft 3 '0'
                            >> (\x ->
                                    "/sample_" ++
                                    langEncode lang ++
                                    "/avenger_" ++
                                    x ++ ".jpg"
                               ))
                      <| List.range 3 23}
                ! [ Navigation.newUrl <| langToPath lang
                  , title (case lang of
                              Japanese ->
                                  "報復の狼"
                              English ->
                                  "AVENGER WOLF"
                          )
                  ]

locationToLanguage : Navigation.Location -> Language
locationToLanguage location =
    case (UrlParser.parsePath matchers location) of
        Just lang ->
            lang
        Nothing ->
            Japanese

matchers : UrlParser.Parser (Language -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map Japanese UrlParser.top
        , UrlParser.map English <| UrlParser.s "eng"
        ]

langToPath : Language -> String
langToPath language =
    case language of
        Japanese ->
            "/"
        English ->
            "/eng"

---- SUBSCRIPTIONS ----

subscriptions : Model -> Sub Msg
subscriptions model = Window.resizes Resize

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ langSelector model
        , logo model
        , copy1 model
        , copy2 model
        , copy3 model
        , hero model
        , info model
        , copyright
        , modal model <| imageList model 
        ]
        
langSelector : Model -> Html Msg
langSelector model =
    div [ class "langselector" ]
        [ a [ classList
                  [ ( "langselector__item", True )
                  , ( "langselector__item--active", model.language == Japanese )]
            , routerClick <| SelectLanguage Japanese
            , href ""
            ] [ text "日本語" ]
        , a [ classList
                  [ ( "langselector__item", True )
                  , ( "langselector__item--active", model.language == English )]
            , routerClick <| SelectLanguage English
            , href ""
            ] [ text "English" ]
        ]

logo : Model -> Html Msg
logo model =
    case model.language of
        Japanese->
            h1 [ classList
                     [ ( "logo", True )
                     , ( "logo--jpn", True )
                     ]
               ][ span [][ text "報復の狼" ] ]
        English->
            h1 [ classList
                     [ ( "logo", True )
                     , ( "logo--eng", True )
                     ]
               ] [ span [][ text "AVENGER WOLF" ] ]
                     
copy : Language -> String -> Html Msg
copy lang str =
    let
        suffix = langEncode lang
    in
        div [ class "copy" ]
            [ div [ classList [ ( "copy__inner", True )
                              , ( "copy__inner--" ++ suffix, True)] ]
                  [ text str ]
            ]

copy1 : Model -> Html Msg
copy1 model =
    case model.language of
        Japanese->
            copy Japanese "「貴様が背負っているのはただ一人の死んだ人間に送るちっぽけな復讐だけではない！」"
        English->
            copy English "“You don't shoulder only a tiny revenge devoted to just one died person!”"

copy2 : Model -> Html Msg
copy2 model =
    case model.language of
        Japanese->
            copy Japanese "「復讐は何も産まない。忘れるべきだ」" 
        English->
            copy English "“Revenge produces nothing. You had better to forget it.”"

copy3 : Model -> Html Msg
copy3 model =
    case model.language of
        Japanese->
            copy Japanese "「アイツは……ネ級は私が……」"
        English->
            copy English "“I shall... kill... that Ne-class...”"

                
hero : Model -> Html Msg
hero model =
    div [ class "hero" ]
        [ div
          [ classList
                [ ( "hero__inner", True )
                , ( "hero__inner--" ++ langEncode model.language, True )
                ]  ][]
        ]

info : Model -> Html Msg
info model =
    div [ class "info"]
        [ div
          [ class "info__image" ]
          [ img [ src <| "/cover_" ++ langEncode model.language ++ ".jpg" ][] ]
        , infoContent model
        ]

infoContent : Model -> Html Msg
infoContent model =
    case model.language of
        Japanese ->
            div [ class "info__content" ]
                [ h2 [] [ text "報復の狼" ]
                , p [ class "info__text"] [ text "足柄は夫を赤色のネ級に殺され、復讐のため戦っていた。ある日の作戦中、雨の中でその宿敵を見つけた足柄だったが――。Klara Worksが送る足柄の復讐譚漫画。" ]
                , div [ class "info__wrapper" ]
                    [ button
                       [ onClick ModalShow
                       , class "info__button"
                       ]
                          [ text "サンプルを見る" ]
                    ]
                , p [ class "info__line" ] [ text "2018年5月6日" ] 
                , p [ class "info__line" ] [ text "砲雷撃戦！よーい！三十八戦目発行" ]
                , p [ class "info__line" ] [ text "会場頒布価格 600円" ]
                , p [ class "info__line" ] [ text "頒布スペース" ]
                , p [ class "info__line" ] [ text "D-05 Klara Works" ]
                , p [ class "info__line" ] [ text "B5 56p 表紙箔押し"]
                , p [ class "info__line" ]
                    [ a [ href "https://www.melonbooks.co.jp/detail/detail.php?product_id=362607" , target "_blank" ] [ text "メロンブックス" ]
                    , text "にて委託頒布中" ]
                , p [ class "info__line" ]
                    [ a [ href  "https://kirisaki.booth.pm/", target "_blank" ][ text "BOOTH" ]
                    , text "にて５月６日よりPDF版の頒布開始予定"
                    , a [ href "http://twitter.com/share?text=%e5%a0%b1%e5%be%a9%e3%81%ae%e7%8b%bc&url=http://avengerwolf.klaraworks.net/"
                        , attribute "onclick" "window.open(encodeURI(decodeURI(this.href)), 'tweetwindow', 'width=650, height=470, personalbar=0, toolbar=0, scrollbars=1, sizable=1'); return false;", rel "nofollow"][ img [ class "tweet", src "/twitter.png" ][]] ]
                ]
                               
        English ->
            div [ class "info__content" ]
                [ h2 [] [ text "AVENGER WOLF" ]
                , p [ class "info__text" ][ text "Ashigara's husband was killed by a Ne-class, so she fought for her vengeance. One day in action, she found the Ne-class. But... This is the vengeance story comic presented by Klara Works." ]
                , div [ class "info__wrapper" ]
                    [ button
                       [ onClick ModalShow
                       , class "info__button"
                       ]
                          [ text "View samples" ]
                    ]
                , p [ class "info__line" ]
                    [ text "It's available on 6 May(JST) at "
                    , a [ href  "https://kirisaki.booth.pm/", target "_blank" ][ text "BOOTH" ]
                    , text "." ]
                , a [ href "http://twitter.com/share?text=Avenger%20Wolf&url=http://avengerwolf.klaraworks.net/eng/"
                        , attribute "onclick" "window.open(encodeURI(decodeURI(this.href)), 'tweetwindow', 'width=650, height=470, personalbar=0, toolbar=0, scrollbars=1, sizable=1'); return false;", rel "nofollow"][ img [ class "tweet", src "/twitter.png" ][]]]

copyright : Html Msg
copyright = div [ class "copyright" ][ text "©Akihito Kirisaki." ]
        

modal : Model -> Html Msg -> Html Msg
modal model content =
    div [ classList
          [ ( "modal", True)
          , ( "modal--hidden", not model.isModalShow)
          ]
        , onClick ModalHide
        ]
        [ content
        , span [ class "modal__close" ] [ span [][] ]
        ]

imageList : Model -> Html Msg
imageList model =
    let
        image (n, name) =
            let
                state = pageStatus model n model.page (List.length model.pageList)
                zIndex = if state == PageShow
                         then "10"
                         else if state == PageVanish
                              then "20"
                              else "0"
                opacity = if state == PageShow
                          then "1"
                          else "0"
            in
                div
                [ classList [ ( "sample__image", True )
                            , ( "sample__image--left"
                              , n % 2 == 0 && model.pageMode == PageSpread )
                            , ( "sample__image--right"
                              , n % 2 == 1 && model.pageMode == PageSpread )
                            , ( "sample__image--single"
                              , model.pageMode == PageSingle )
                            ]
                , id ("image" ++ toString n)
                , style
                      [ ("background-image", "url('"++name++"')")
                      , ("z-index", zIndex)
                           , ("opacity", opacity)
                      ]
                ]
                []
    in
        div [ classList [ ( "sample", True )
                        , ( "sample--single", model.pageMode == PageSingle )
                        ]
            ]
            ( List.map image (List.indexedMap (,) model.pageList)  ++
              [ a [ routerClick PageNext
                  , href ""
                  , classList [ ("sample__button", True )
                              , ("sample__button--next", True )
                              ]
                  ] []
              , a [ routerClick PagePrev
                  , href "#"
                  , classList [ ("sample__button", True )
                              , ("sample__button--prev", True )
                              ]
                  ] []
              , div [ class "sample__page" ]
                  [ text <| toString <| model.page + 1
                  , text "/"
                  , text <| toString <| List.length model.pageList
                  , text " "
                  , text <| case (model.pageMode, model.language) of
                             (PageSpread, Japanese) ->
                                 "左右のページをクリックで移動"
                             (PageSingle, Japanese) ->
                                 "画像の左右をタップして移動"
                             (PageSpread, English) ->
                                 "Click images to turn the page."
                             (PageSingle, English) ->
                                 "Tap sides of image to turn the page."
                  ]
              ] )


routerClick : msg -> Attribute msg
routerClick message =
    onWithOptions "click" { defaultOptions
                              | preventDefault = True
                              , stopPropagation = True }
        <| Json.Decode.succeed message

pageStatus : Model -> Int -> Int -> Int -> PageStatus
pageStatus model n show all =
    case model.pageMode of
        PageSpread ->
            if show == n || show == n + 1
            then PageShow
            else if (show - n + all) % all == 1
                 then PageVanish
                 else PageHidden            
        PageSingle ->
            if show == n
            then PageShow
            else if (show - n + all) % all == 1
                 then PageVanish
                 else PageHidden
----


main : Program Never Model Msg
main =
    Navigation.program Locate
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

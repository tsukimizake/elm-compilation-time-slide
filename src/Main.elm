module Main exposing (main)

import Browser
import Css exposing (..)
import Html.Styled exposing (Html, button, div, h1, h2, img, text)
import Html.Styled.Attributes exposing (css, src)
import Html.Styled.Events exposing (onClick)
import List.Extra



-------------
-- model
-------------


type Page
    = TitleOnly { title : String }
    | Article { title : Maybe String, document : List Document }


type Document
    = Text String
    | Code String
    | Pic (Maybe Float) String


type alias Model =
    { index : Int }


initialModel : Model
initialModel =
    { index = 0 }



-------------
-- update
-------------


type Msg
    = NextPage
    | PrevPage
    | SetPage Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mPrev =
    case msg of
        NextPage ->
            if List.length pages <= (mPrev.index + 1) then
                noCmd mPrev

            else
                update (SetPage (mPrev.index + 1)) mPrev

        PrevPage ->
            if (mPrev.index + 1) <= 1 then
                noCmd mPrev

            else
                update (SetPage (mPrev.index - 1)) mPrev

        SetPage i ->
            noCmd { mPrev | index = i }


noCmd : m -> ( m, Cmd msg )
noCmd m =
    ( m, Cmd.none )



-------------
-- view
-------------


view : Model -> Html Msg
view model =
    div []
        [ List.Extra.getAt model.index pages
            |> Maybe.map renderPage
            |> Maybe.withDefault (div [] [])
        , buttons model
        ]


renderPage : Page -> Html Msg
renderPage page =
    div []
        [ div [ css [ height (px 100), backgroundColor (hex "C0C0C0") ] ] []
        , div
            [ css
                [ height (px 600)
                ]
            ]
            [ case page of
                TitleOnly { title } ->
                    h1
                        [ css
                            [ textAlign center
                            , width (pct 100)
                            , lineHeight (px 560)
                            , display inlineBlock
                            ]
                        ]
                        [ text title ]

                Article { title, document } ->
                    div []
                        ((title |> Maybe.map (\t -> h2 [ css [ textAlign center, marginTop (px 0) ] ] [ text t ]) |> Maybe.withDefault (text ""))
                            :: List.map renderDocument document
                        )
            ]
        , div [ css [ height (px 100), backgroundColor (hex "C0C0C0") ] ] []
        ]


renderDocument : Document -> Html msg
renderDocument document =
    div []
        [ case document of
            Text s ->
                text s

            Code s ->
                Html.Styled.pre
                    [ css
                        [ borderWidth (px 1)
                        , borderStyle solid
                        , textAlign left
                        ]
                    ]
                    [ text s ]

            Pic height path ->
                case height of
                    Nothing ->
                        img [ src <| "assets/" ++ path ] []

                    Just h ->
                        img [ css [ maxHeight (px h) ], src <| "assets/" ++ path ] []
        ]


buttons : Model -> Html Msg
buttons m =
    div []
        [ button [ onClick PrevPage ] [ text "prev" ]
        , text <| String.fromInt (m.index + 1) ++ "/" ++ String.fromInt (List.length pages)
        , button [ onClick NextPage ] [ text "next" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> noCmd initialModel
        , view = view >> Html.Styled.toUnstyled
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-------------
-- content
-------------


pages : List Page
pages =
    [ TitleOnly { title = "大規模なelmプロジェクトのコンパイル時間の話" }
    , TitleOnly { title = "前提: elmのコンパイル速いですよね" }
    , Article { title = Just "elmのコンパイル速いですよね", document = [ Text "このelm製スライド(300行程度)のコンパイル時間", Pic (Just 200) "slide-compile-time.png" ] }
    , Article
        { title = Just "elmのコンパイル速いですよね(？)"
        , document =
            [ Text "弊社のSPAアプリ(50000行程度)のうち、各ページの結線を担当している1ファイル(1500行程度)だけtouchしたものの7月時点のコンパイル時間"
            , Code """
  INIT    time    0.003s  (  0.008s elapsed)
  MUT     time    3.018s  (  2.370s elapsed)
  GC      time   27.812s  ( 29.767s elapsed)
  EXIT    time    0.000s  (  0.002s elapsed)
  Total   time   30.834s  ( 32.147s elapsed)
    """
            , Text "50000行のフルビルドだと60sec程度"
            , Text "遅いだけならまだしも、メモリ不足でCIが頻繁に落ちるようになったため調査に乗り出すことに"

            -- 調査過程はめんどいので省略
            ]
        }
    , TitleOnly { title = "原因: https://github.com/elm/compiler/issues/1897" }
    , Article
        { title = Just "https://github.com/elm/compiler/issues/1897 の要約"
        , document =
            [ Text "レコードのネストの深さが増えるとコンパイル時間がO(2^n)で増える"
            , Text "issueはextensible recordについて書いているが、同じことが通常のレコードでも起きる"
            , Text "プロジェクトが大規模化するに従ってネストが増えていき、これが発生していた"
            , Text "SPAのページモジュールからexportするinit, update, viewなどをわかりやすさのためにレコードに格納していたのをやめたところ半分程度に改善した"

            -- TODO もうちょっと細かく説明したいところだがコード出したりしているとあっという間に10分超えそう
            ]
        }

    -- 以降TODO
    , TitleOnly { title = "修正後コンパイル時間" }
    , TitleOnly { title = "haskellソースコード深掘り" }
    , TitleOnly { title = "まとめとTips" }
    , TitleOnly { title = "謝辞" }
    ]



-- TODO サブ知見 Tipsとしてまとめて書くか
-- elmi消せる
-- モジュール分けてもメモリプレッシャーは改善しない
-- メモリ使用量の設定
-- elm-compilerのデバッグビルド

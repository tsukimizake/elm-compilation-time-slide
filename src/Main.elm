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
    """ -- TODO codeに等幅フォント
            , Text "50000行のフルビルドだと60sec程度"
            , Text "1500行で30秒かかるのはかなり遅い"
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
            , Text "ページごとのinit, update, viewなどをレコードにまとめていたのをやめることで改善"
            ]
        }
    , Article
        { title = Just "修正箇所(修正前)"
        , document =
            [ Code """module Page1 exposing(page)

page =
  { init = ...
  , update = ...
  , view = ...
  }
            """
            , Code """module Main exposing(..)

type Model
  = Model1 Page1.Model
  | Model2 Page2.Model

type Msg
  = Msg1 Page1.Msg
  | Msg2 Page1.Msg

init = ...
update msg mPrev =
  case (msg, mPrev) of
    (Msg1 page1msg, Model1 page1model) ->
      Page1.page.update page1msg page1model
    ...
view = ...
"""
            ]
        }
    , Article
        { title = Just "修正箇所(修正後)"
        , document =
            [ Code """module Page1 exposing(init, update, view)

init = ...
update = ...
view = ...

            """
            , Code """type Model
  = Page1 Page1.Model
  | Page2 Page2.Model

type Msg =
     Msg1 Page1.Msg
   | Msg2 Page1.Msg

init = ...
update msg mPrev =
  case (msg, mPrev) of
    (Msg1 page1msg, Model1 page1model) ->
      Page1.update page1msg page1model
    ...
view = ...
"""
            ]
        }

    -- 以降TODO
    , Article
        { title = Just "修正後のコンパイル時間"
        , document =
            [ Text "前記の修正でメモリ消費、コンパイル時間共に半分程度になった"
            , Text "加えて、コンパイル中のGC間隔等の設定を(別の人が)やってコンパイル時間はさらに縮んだ"
            , Text "最終的に、問題のファイルのみのコンパイルは30s -> 11s程度に縮んだ"
            , Text "フルビルドは90s -> 30s程度に"

            -- TODO さみしいので測定結果
            ]
        }

    -- TODO 終わらんかったら最悪ここ消す なんなら今でも時間は丁度くらいのはず
    -- TODO elmiの例示
    , TitleOnly { title = "原因の深掘り" }
    , TitleOnly { title = "まとめ: コンパイル時間を伸ばさないコツ" }
    , Article
        { title = Just "まとめ: コンパイル時間を伸ばさないコツ"
        , document =
            [ Text "レコードのネストを増やさない"
            , Text "特にプロジェクトのルートに近い部分で大量のレコードを含むレコードを増やすと大きな影響が出る"
            , Text "末端ならレコードを増やしても影響はほぼない"
            , Text ""
            , Text "余談: モジュールを分けてもメモリプレッシャーは改善しない"
            , Text "これはコンパイルのフェイズが 全モジュールパース -> 全モジュール型推論 -> 全モジュールコード生成 のように動いているため"
            ]
        }
    ]



-- TODO サブ知見 Tipsとしてまとめて書くか
-- elmi消せる
-- レコードを新に作るとその内側の型推論結果が倍に膨れてしまう
--  プロジェクトのルートに近い部分などで多重にネストしたレコードを増やすとその内側が倍になる
--  末端部なら影響は無視できる
-- モジュール分けてもメモリプレッシャーは改善しない
-- メモリ使用量の設定
-- elm-compilerのデバッグビルド

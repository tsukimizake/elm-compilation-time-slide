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
    | Divide


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

            Divide ->
                div [ css [ height (px 20) ] ] []
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
    [ -- 前置き
      TitleOnly { title = "大規模なelmプロジェクトのコンパイル時間の話" }
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
            , Text "50000行のフルビルドだと60秒程度"
            , Text "1500行で30秒かかるのはかなり遅い"
            , Text "遅いだけならまだしも、メモリ不足でCIが頻繁に落ちるようになったため調査に乗り出すことに"
            ]
        }

    -- プロファイル
    , TitleOnly { title = "何をおいてもプロファイル" }
    , Article
        { title = Just "プロファイル"
        , document =
            [ Text "elm-compilerをプロファイルモードで再ビルドして、これを使ってプロジェクトをコンパイルすると、コンパイル時間の何割が型チェックに使われているかなどが大まかにわかる"
            , Text "cf. haskell/cabal#5930 https://nikita-volkov.github.io/profiling-cabal-projects/ http://www.kotha.net/ghcguide_ja/7.6.2/profiling.html"
            , Divide
            , Text "結果は以下"
            , Text "TODO プロファイル結果 データ残ってるといいな"
            ]
        }

    -- 原因
    , TitleOnly { title = "原因: https://github.com/elm/compiler/issues/1897 (に近いことが起きていた)" }
    , Article
        { title = Just "原因: https://github.com/elm/compiler/issues/1897 (に近いことが起きていた)"
        , document =
            [ Text "elm-js会(Elmコンパイラが生成するJSの悪口を言う会)で調査したところ、各ページを格納したレコードの型推論後の型が巨大になってしまっていたことが原因だった"
            , Text "extensible-recordは特にバグがあるらしく型が太ってしまう。中身の型と新しいextensible-recordの型が別に作られている？と思われる(ちゃんと調べ切れておらず理由は曖昧)"
            , Divide
            , Text "本プロジェクトでは他の要因も合わさって巨大に"
            , Text "ページごとのinit, update, viewなどをレコードにまとめていたのをやめることで改善"
            ]
        }

    -- 修正前後のコード
    , Article
        { title = Just "修正箇所(修正前)" -- TODO PageModule型は重要なので書く
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
    , Article
        { title = Just "結果: 修正後のコンパイル時間"
        , document =
            [ Text "前記の修正でメモリ消費、コンパイル時間共に半分程度になった"
            , Text "加えて、コンパイル中のGC間隔等の設定を(別の人が)やってコンパイル時間はさらに縮んだ"
            , Text "最終的に、問題のファイルのみのコンパイルは30s -> 11s程度に縮んだ"
            , Code """
  INIT    time    0.007s  (  0.018s elapsed)
  MUT     time    5.426s  (  2.658s elapsed)
  GC      time    5.405s  (  7.322s elapsed)
  EXIT    time    0.001s  (  0.001s elapsed)
  Total   time   10.839s  (  9.999s elapsed)
            """
            , Text "また、フルビルドは60s -> 30s程度に"
            ]
        }

    -- まとめ
    , TitleOnly { title = "まとめ: 現状わかっているコンパイル時間を伸ばさないコツ" }
    , Article
        { title = Just "まとめ: 現状わかっているコンパイル時間を伸ばさないコツ"
        , document =
            [ Text "型推論の結果が大きくなるようなものをなるべく作らない"
            , Text "全ページのMsgをまとめた親Msgとかを持ち回るのは良くない"
            , Divide
            , Text "レコードは特にまずい、のかもしれないそうでもないかもしれない"
            , Text "例えば下のような単純なレコードなら1000重くらいにしても一瞬でコンパイルできる。巨大なHtml等を入れたデータ量が多いだけのレコードも同様"
            , Code """record = { k = { j = { i = { h = { g = { f = { e = { d = { c = { b = { a = "" } } } } } } } } } } } """
            , Divide
            , Text "Extensible recordはまずいのでコンパイル時間を見ながら節度を持って"
            , Text "issueのコードを試したところ21重でクラッシュした"
            , Divide
            , Text "型引数を取るレコードは型推論の結果が大きくなる場合まずいと思われる"
            , Text "本プロジェクトのPageModuleからextensible-record部分のみ消す、outerMsgだけ消すなどした場合も少しずつ改善するが、recordごと消した場合には及ばなかった"
            , Divide
            , Text "余談: モジュールを分けてもフルビルド時のメモリプレッシャーは改善しない"
            , Text "これはコンパイルのフェイズが 全モジュールパース -> 全モジュール型推論 -> 全モジュールコード生成 のように動いているため"
            ]
        }
    ]

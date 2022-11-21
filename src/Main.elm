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
  INIT    time    0.007s  (  0.017s elapsed)
  MUT     time    7.841s  (  4.005s elapsed)
  GC      time   19.404s  ( 23.977s elapsed)
  EXIT    time    0.001s  (  0.003s elapsed)
  Total   time   27.253s  ( 28.002s elapsed)
    """
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
            [ Text "elm-compilerをプロファイルモードで再ビルドしたものを使ってプロジェクトをコンパイルすると、コンパイル時間の何割が型チェックに使われているかなどが大まかにわかる"
            , Text "cf. https://github.com/haskell/cabal/issues/5930 https://nikita-volkov.github.io/profiling-cabal-projects/ http://www.kotha.net/ghcguide_ja/7.6.2/profiling.html"
            , Divide
            , Code """
\ttotal time  =       57.11 secs   (177897 ticks @ 1000 us, 16 processors)
\ttotal alloc = 14,101,603,032 bytes  (excludes profiling overheads)

COST CENTRE         MODULE                   SRC                                               %time %alloc

getUnder256         Data.Utf8                compiler/src/Data/Utf8.hs:(526,1)-(529,36)         13.0    8.0
srcFieldTypeToVar   Type.Solve               compiler/src/Type/Solve.hs:(567,1)-(568,42)        12.3    4.7
get                 Elm.Package              compiler/src/Elm/Package.hs:276:3-53               10.2    9.7
...
            """
            , Text "Data.Utf8.getUnder256はいくつか使用箇所があるが、profのcall treeを読むと特に*.elmiを読み出す部分でボトルネックになっていた"
            , Text "*.elmiファイルには型推論の結果として各シンボルにつく型情報が書かれている"
            , Text "2つ目のType.Solve.srcFieldTypeToVarも合わせて、どうやら型推論が変らしい？と推測できる"
            , Text "3つ目のElm.Package.getは使用箇所を見るとコード生成時の呼び出しらしく、無関係と思われる"
            ]
        }

    -- 原因
    , TitleOnly { title = "原因: https://github.com/elm/compiler/issues/1897 (と他の問題の複合？)" }
    , Article
        { title = Just "原因: https://github.com/elm/compiler/issues/1897 (と他の問題の複合？)"
        , document =
            [ Text "elm-js会(Elmコンパイラが生成するJSの悪口を言う会)で調査したところ、各ページを格納したレコードの型推論後の型が巨大になってしまっていたことが原因だった"
            , Text "extensible-recordは特にバグがあるらしく型が太ってしまう(これがissues/1897)(理由はちゃんと調べ切れておらず曖昧)"
            , Divide
            , Text "本プロジェクトでは単純にプロジェクトが大きいことも合わさって巨大に"
            , Text "ページごとのinit, update, viewなどをレコードにまとめていたのをやめることで改善"
            , Code """
type alias PageModule urlParams model msg outerMsg =
    { init : Shared -> urlParams -> ( HasShared model, Cmd msg )
    , update : msg -> HasShared model -> ( HasShared model, Cmd msg )
    , subscriptions : HasShared model -> Sub msg
    , subscribeToSharedUpdate : HasShared model -> ( HasShared model, Cmd msg )
    , view : (msg -> outerMsg) -> HasShared model -> Browser.Styled.Document outerMsg
    }


type alias HasShared model =
    { model | shared : Shared }
"""
            , Text "上記のPageModule型のレコードを各ページからexportしていた"
            ]
        }

    -- 修正前後のコード
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
    , Article
        { title = Just "結果: 修正後のコンパイル時間"
        , document =
            [ Text "PageModuleレコードを消す修正でメモリ消費、コンパイル時間共に半分程度になった"
            , Text "加えて、コンパイル中のGC間隔等の設定を(別の人が)やってコンパイル時間はさらに縮んだ"
            , Text "最終的に、問題のファイルのみのコンパイルは30s -> 4s程度に縮んだ"
            , Code """
  INIT    time    0.119s  (  0.271s elapsed)
  MUT     time    3.511s  (  3.573s elapsed)
  GC      time    0.004s  (  0.010s elapsed)
  EXIT    time    0.001s  (  0.002s elapsed)
  Total   time    3.635s  (  3.856s elapsed)
            """
            , Text "また、フルビルドは60s -> 30s程度に"
            , Text "改善後のプロファイルは以下"
            , Code """
\ttotal time  =       19.89 secs   (61965 ticks @ 1000 us, 16 processors)
\ttotal alloc = 6,537,550,712 bytes  (excludes profiling overheads)

COST CENTRE         MODULE                   SRC                                               %time %alloc

getUnder256         Data.Utf8                compiler/src/Data/Utf8.hs:(526,1)-(529,36)         21.9   11.2
get                 Elm.Package              compiler/src/Elm/Package.hs:276:3-53               15.6   12.4
get                 AST.Canonical            compiler/src/AST/Canonical.hs:400:3-32             11.0   10.9
            """
            , Text "*.elmiを読み出す部分は相変わらずボトルネックで、全体のうちに占める割合としては増えたが時間としては減っている"
            , Text "Type.Solve.srcFieldTypeToVarがきれいに消えた"
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
            , Text "Extensible recordは多用するとまずいのでコンパイル時間を見ながら節度を持って"
            , Text "issueのコードを試したところ21重でクラッシュした"
            , Divide
            , Text "型引数を取るレコードは型推論の結果が大きくなる場合まずいと思われる"
            , Text "本プロジェクトのPageModuleからextensible-record部分のみ消す、outerMsgだけ消すなどした場合も少しずつ改善するが、recordごと消した場合には及ばなかった"
            , Divide
            , Text "最後に免責事項: 正直中で何が起きてるのかよくわからないので理由に関しては色々嘘を書いているかもしれない"
            ]
        }
    ]

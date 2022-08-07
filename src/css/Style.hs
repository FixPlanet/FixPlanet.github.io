{-# language OverloadedStrings #-}

import Clay
import Control.Monad  ( forM_ )
import Prelude hiding ( span
                      , div
                      , rem
                      )
import qualified Data.Text      as Text
import qualified Clay.Media     as Media

-------------------------------------------------------------
--
allMargin n       = margin  n n n n
allPadding n      = padding n n n n
allBorderRadius n = borderRadius n n n n

margin0  = margin  (px 0) (px 0) (px 0) (px 0)
padding0 = padding (px 0) (px 0) (px 0) (px 0)

coreTextFont = fontFamily ["Noto Serif JP"]    [serif]
monoFont     = fontFamily ["PT Mono"]          [monospace]
titleFont    = fontFamily ["Playfair Display"] [serif]
menuFont     = titleFont
--
-------------------------------------------------------------

main :: IO ()
main = putCss $
     basics
  >> fonts
  >> mainHeader
  >> links
  >> mainContent
  >> footerStuff


footerStuff :: Css
footerStuff = do
  footer ? do
    allPadding    (px 30)
    background    azure
    borderTop     solid (px 1) lightskyblue
    display       flex

    query Clay.all [Media.maxWidth 1200] $ do
      flexDirection row

    query Clay.all [Media.maxWidth 800] $ do
      flexDirection column

    li ? do
      lineHeight (px 25)


    div # ".section" ? do
      firstChild & borderLeft solid (px 1) azure
      paddingLeft  (px 20)
      paddingRight (px 20)

      query Clay.all [Media.maxWidth 1200] $ do
        borderLeft solid (px 1) gainsboro

      query Clay.all [Media.maxWidth 800] $ do
        borderLeft solid (px 1) azure


links :: Css
links = do
  a ? do
    color black
    visited & do
      color black
    hover & do
      background lightskyblue


mainContent :: Css
mainContent = do

  div # "#content" ? do
    allPadding      (px 30)
    backgroundColor oldlace
    display         flex
    flexDirection   column


mainHeader :: Css
mainHeader = do
  "#header" ? do
    marginTop  (px 100)

    h1 # ".big" ? do
      allMargin (px 0)
      color     olive
      fontSize  (px 41)

    (ul # ".menu" <> ul # ".main-menu") ? do
      alignItems     center
      display        flex
      justifyContent flexStart
      listStyleType  none
      margin0
      marginLeft     (px 20)
      padding0

      li ? do
        padding0
        a ? do
          allMargin     (px 12)
          display       block
          fontSize      (px 16)
          menuFont
          paddingBottom (px 0)

    ul # ".main-menu" ? do
      a ? hover & do
        background (none :: Color)

      li ? do
        firstChild & do
          marginRight (px 10)


basics :: Css
basics = do
  body ? do
    margin0
    padding0

  let selectionStyle =
        do
          background mediumslateblue
          color      white

  selection          & selectionStyle
  "::-moz-selection" & selectionStyle


fonts :: Css
fonts = do
  body ? do
    fontSize (px 15)

  span # ".amount" ? do
    borderBottom solid (px 2) lightskyblue
    fontWeight   bold

  let fontSelectors
        =  span
        <> p
        <> li
        <> blockquote
        <> a
        <> small
        <> b

  fontSelectors ? do
    coreTextFont
    textAlign justify

  h1 <> h2 <> h3 <> h4 ? do
    titleFont
    fontWeight normal

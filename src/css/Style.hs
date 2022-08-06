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
main = putCss
  $  basics
    >> fonts
    >> mainHeader
    >> links
    >> mainContent
    >> footerStuff


footerStuff :: Css
footerStuff = do
  footer ? do
    borderTop solid (px 5) lightskyblue
    allPadding   (px 30)
    display      flex
    flexDirection column
    background azure


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
    backgroundColor oldlace
    marginBottom (px 10)
    allPadding   (px 30)
    display      flex
    flexDirection column


mainHeader :: Css
mainHeader = do
  "#header" ? do
    marginTop  (px 100)
    (ul # ".menu" <> ul # ".main-menu") ? do
      margin0
      padding0
      marginLeft     (px 20)
      display        flex
      alignItems     center
      listStyleType  none
      justifyContent flexStart

      li ? do
        padding0
        a ? do
          fontSize (px 16)
          allPadding (px 5)
          paddingBottom (px 0)
          menuFont
          display block
          allMargin (px 12)

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
    backgroundColor oldlace

  let selectionStyle =
        do
          background mediumslateblue
          color      white

  selection          & selectionStyle
  "::-moz-selection" & selectionStyle


fonts :: Css
fonts = do
  body ? do
    fontSize (px 14)

  let fontSelectors = p <> li <> blockquote <> a <> small <> b

  fontSelectors ? do
    coreTextFont
    textAlign justify

  h1 <> h2 <> h3 <> h4 ? do
    titleFont
    fontWeight normal

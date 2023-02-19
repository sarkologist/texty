{-# LANGUAGE OverloadedStrings #-}
module TextyTest where

import           Control.Lens            hiding ( Context )
import           Data.Vector                   as V
import           MarkdownTexty
import           Test.Hspec              hiding ( focus )
import           Texty

spec_texty :: Spec
spec_texty = do
  describe "markdown" $ do
    it "italic" $ flip set "_" (text . i . _1 . unItalic) "*i*" `shouldBe` "*_*"

  describe "combinators" $ do
    describe "||>" $ do
      it "left" $ do
        flip set "_" (text . (i ||> h 2) . _1 . _Left . unItalic) "*i*## h2\n"
          `shouldBe` "*_*## h2\n"

      it "right" $ do
        flip set "_" (text . (i ||> h 2) . _1 . _Right . content) "*i*## h2\n"
          `shouldBe` "*i*## _\n"

      it "left fails" $ do
        flip set "_" (text . (i ||> h 2) . _1 . _Left . unItalic) "not i## h2\n"
          `shouldBe` "not i## h2\n"

      it "right fails" $ do
        flip set
             "_"
             (text . (i ||> h 2) . _1 . _Left . unItalic)
             "*i* not header"
          `shouldBe` "*i* not header"

      describe "inside many" $ do
        it "_Left" $ do
          flip set
               "_"
               (text . many' (i ||> h 1) . _1 . _Left . unItalic)
               "*i*# h1\n*i*# h1\n"
            `shouldBe` "*_*# h1\n*_*# h1\n"

        it "_Right" $ do
          flip set
               "_"
               (text . many' (i ||> h 1) . _1 . _Right . content)
               "*i*# h1\n*i*# h1\n"
            `shouldBe` "*i*# _\n*i*# _\n"

    describe "||>?" $ do
      it "left fails" $ do
        flip set
             "_"
             (text . (i ||>? h 2) . _1 . _Left . unItalic)
             "not i## h2\n"
          `shouldBe` "not i## h2\n"

      it "right fails" $ do
        flip set
             "_"
             (text . (i ||>? h 2) . _1 . _Left . unItalic)
             "*i* not header"
          `shouldBe` "*_* not header"

    describe "many" $ do
      it "many" $ do
        flip set "_" (text . many' i . _1 . unItalic) "*i**i2*"
          `shouldBe` "*_**_*"

      it "empty" $ do
        flip set "_" (text . many' i . _1 . unItalic) "blah" `shouldBe` "blah"

      it "indexing into many" $ do
        flip set
             "_"
             (text . partsOf (many' i . _1) . ix 1 . unItalic)
             "*i**i2**i3*"
          `shouldBe` "*i**_**i3*"

    describe "focus" $ do
      it "focus" $ do
        flip set
             "_"
             (text . h 1 . focus content . i . _1 . unItalic)
             "# *i* not i\n not h"
          `shouldBe` "# *_* not i\n not h"

      it "focus inside ||>" $ do
        flip
            set
            "_"
            (text . ((h 1 . focus content . i) ||> i) . _1 . _Left . unItalic)
            "# *i* not i\n*i*"
          `shouldBe` "# *_* not i\n*i*"

        flip
            set
            "_"
            (text . ((h 1 . focus content . i) ||> i) . _1 . _Right . unItalic)
            "# *i* not i\n*i*"
          `shouldBe` "# *i* not i\n*_*"

      it "focus twice inside ||>" $ do
        flip
            set
            "_"
            ( text
            . (   ( h 1
                  . focus content
                  . strikethrough
                  . focus unStrikethrough
                  . i
                  )
              ||> skip ""
              )
            . _1
            . _Left
            . unItalic
            )
            "# ~~*i* s ~~ h\n unconsumed"
          `shouldBe` "# ~~*_* s ~~ h\n unconsumed"

      it "focus inside ||> inside focus" $ do
        flip
            set
            "_"
            ( text
            . (   ( h 1
                  . focus content
                  . ((strikethrough . focus unStrikethrough . i) ||> i)
                  )
              ||> i
              )
            . _1
            . _Left
            . _Left
            . unItalic
            )
            "# ~~*i* s ~~*i2* h\n*i3*"
          `shouldBe` "# ~~*_* s ~~*i2* h\n*i3*"

        flip
            set
            "_"
            ( text
            . (   ( h 1
                  . focus content
                  . ((strikethrough . focus unStrikethrough . i) ||> i)
                  )
              ||> i
              )
            . _1
            . _Left
            . _Right
            . unItalic
            )
            "# ~~*i* s ~~*i2* h\n*i3*"
          `shouldBe` "# ~~*i* s ~~*_* h\n*i3*"

        flip
            set
            "_"
            ( text
            . (   ( h 1
                  . focus content
                  . ((strikethrough . focus unStrikethrough . i) ||> i)
                  )
              ||> i
              )
            . _1
            . _Right
            . unItalic
            )
            "# ~~*i* s ~~*i2* h\n*i3*"
          `shouldBe` "# ~~*i* s ~~*i2* h\n*_*"

      it "||> inside focus" $ do
        flip
            set
            "_"
            (text . (h 1 . focus content . (i ||> i)) . _1 . _Right . unItalic)
            "# *i**i2* h\n*i3*"
          `shouldBe` "# *i**_* h\n*i3*"

      it "focus after ||>" $ do
        flip
            set
            "_"
            (text . (h 1 ||> i) . focus (_Left . content) . i . _1 . unItalic)
            "# *i inside* not i\n*i outside*"
          `shouldBe` "# *_* not i\n*i outside*"

      it "focus after many" $ do
        flip set
             "_"
             (text . many' (h 1) . focus content . i . _1 . unItalic)
             "# *i* not i\n# *i2* not i2\n# no i\n *i* not i"
          `shouldBe` "# *_* not i\n# *_* not i2\n# no i\n *i* not i"

    describe "<||>" $ do
      it "fail <||> succeed" $ do
        flip set "_" (text . (h 1 <||> i) . _1 . _Right . unItalic) "*i*# h1\n"
          `shouldBe` "*_*# h1\n"

      it "succeed <||> fail" $ do
        flip set "_" (text . (h 1 <||> i) . _1 . _Left . content) "# h1\n*i*"
          `shouldBe` "# _\n*i*"

      it "<||> inside many: left succeeds" $ do
        flip set
             "_"
             (text . many' (i <||> h 1) . _1 . _Left . unItalic)
             "*i*# h1\n"
          `shouldBe` "*_*# h1\n"

      it "<||> inside many: right succeeds" $ do
        flip set
             "_"
             (text . many' (i <||> h 1) . _1 . _Right . content)
             "*i*# h1\n"
          `shouldBe` "*i*# _\n"

    describe "can change type of focus without violating laws" $ do
      it "unindenting level zero bullet makes it not bullet" $ do
        flip
            over
            (\(Left (Bullet style lvl content@txt)) -> if lvl > 0
              then (Left $ Bullet style (lvl - 1) content)
              else Right (txt <> "\n")
            )
            (text . many' (bullet <%> skip "\n") . _1)
            "- b 1\n  - b 2\n"
          `shouldBe` "b 1\n- b 2\n"

      it "prism law" $ do
        let
          p          = text . (i <%> strikethrough)

          built_left = review p (Left (Italic "_"), Context "" V.empty 0)
          built_right =
            review p (Right (Strikethrough "_"), Context "" V.empty 0)

        preview (text . (i <%> strikethrough) . _1) built_left
          `shouldBe` (Just (Left (Italic "_")))
        preview (text . (i <%> strikethrough) . _1) built_right
          `shouldBe` (Just (Right (Strikethrough "_")))

      it "traversal law" $ do
        let p = text . (h 1 <%> i) . _1

            indentHeader (Left (Header lvl title)) =
              Left (Header (lvl + 1) title)
            indentHeader (Right x) = Right x

            changeToItalic (Left  (Header _ title)) = Right (Italic title)
            changeToItalic (Right x               ) = Right x

            changeToHeader (Left  x           ) = Left x
            changeToHeader (Right (Italic txt)) = Left (Header 1 txt)

        over p (indentHeader . changeToItalic) "# h\n" `shouldBe` "*h*"
        over p (indentHeader) "# h\n" `shouldBe` "## h\n"
        over p (indentHeader . changeToHeader) "*i*" `shouldBe` "## i\n"

        (over p indentHeader . over p changeToItalic $ "# h\n")
          `shouldBe` over p (indentHeader . changeToItalic) "# h\n"

        (over p indentHeader . over p changeToHeader $ "*i*")
          `shouldBe` over p (indentHeader . changeToHeader) "*i*"

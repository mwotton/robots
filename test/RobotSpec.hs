{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module RobotSpec where

import Control.Monad (filterM, forM_)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Network.HTTP.Robots
import System.Directory
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

-- apparently not a utility function.
myIsLeft :: Either a b -> Bool
myIsLeft (Left _) = True
myIsLeft _ = False

dirname :: String
dirname = reverse $ dropWhile (/= '/') $ reverse __FILE__

-- forgive me father, i know not what i do.

texts :: [(FilePath, BS.ByteString)]
{-# NOINLINE texts #-}
texts = unsafePerformIO $ do
  contents <- map ((dirname ++ "/examples/") ++) <$> getDirectoryContents (dirname ++ "/examples")
  files <- filterM doesFileExist contents
  mapM (\x -> BS.readFile x >>= \c -> return (x, c)) files

-- this is just an ugly burn-in test - we collect examples of
-- robots.txt and check we can read them all.

spec :: Spec
spec = do
  describe "simple parsing" $ do
    it "can read a token" $
      parseOnly tokenP "foo"
        `shouldBe` Right "foo"
    it "stringCI does something sane" $
      parseOnly (stringCI "user-agent:") "User-agent:"
        `shouldBe` Right "User-agent:"
    it "can read a user agent" $
      parseOnly agentP "User-agent: *\n"
        `shouldBe` Right Wildcard
    it "can read a specific user agent" $
      parseOnly agentP "User-agent: Buzzbot\n"
        `shouldBe` Right (Literal "Buzzbot")
    it "can read allow directives" $
      parseOnly directiveP "Allow: /\n"
        `shouldBe` Right (Allow "/")
    it "should read a full robots.txt" $
      parseOnly robotP "User-agent: *\nDisallow: /\n"
        `shouldBe` Right ([([Wildcard], [Disallow "/"])], [])
    it "should cope with end-of-line comments" $
      parseOnly robotP "User-agent: *\nDisallow: / # don't read my site\nAllow: /foo"
        `shouldBe` Right ([([Wildcard], [Disallow "/", Allow "/foo"])], [])
    it "can parse this stupid empty disallow line that the BNF suggests should be illegal" $
      parseOnly robotP "User-agent: *\nDisallow:\n"
        `shouldBe` Right ([([Wildcard], [Allow "/"])], [])
    it "ignores the sitemap extension (and any other unrecognised text" $
      parseOnly robotP "Sitemap: http:www.ebay.com/lst/PDP_US_main_index.xml\nUser-agent: *\nDisallow: /\n"
        `shouldBe` Right ([([Wildcard], [Disallow "/"])], ["Sitemap: http:www.ebay.com/lst/PDP_US_main_index.xml"])

  -- the behaviour here doesn't seem to be rigorously specified: it
  -- seems obvious that if * can access a resource but FooBot is
  -- explicitly disallowed, then FooBot should be disallowed.

  -- so we should check first the most specific bot, then go in order
  -- of Allow/Disallow statements. Anyone playing silly buggers with
  --
  -- UserAgent: *
  -- Allow: /
  -- Disallow: /
  --
  -- is fit for treasons, stratagems and spoils.
  -- ganked from http://www.robotstxt.org/norobots-rfc.txt
  describe "canAccess" $ do
    let robot =
          ( [ ( [Literal "unhipbot"],
                [Disallow "/"]
              ),
              ( [Literal "webcrawler", Literal "excite"],
                [Allow "/"]
              ),
              ( [Wildcard],
                [ Disallow "/org/plans.html",
                  Allow "/org/",
                  Allow "/serv",
                  Allow "/~mak",
                  Disallow "/"
                ]
              )
            ],
            []
          )

    --    unhipbot webcrawler other
    --             & excite
    let tests =
          [ ( "/",
              (False, True, False)
            ),
            ( "/index.html",
              (False, True, False)
            ),
            ( "/robots.txt",
              (True, True, True)
            ),
            ( "/server.html",
              (False, True, True)
            ),
            ( "/services/fast.html",
              (False, True, True)
            ),
            ( "/services/slow.html",
              (False, True, True)
            ),
            ( "/orgo.gif",
              (False, True, False)
            ),
            ( "/org/about.html",
              (False, True, True)
            ),
            ( "/org/plans.html",
              (False, True, False)
            ),
            ( "/~jim/jim.html",
              (False, True, False)
            ),
            ( "/~mak/mak.html",
              (False, True, True)
            )
          ]
    forM_ tests $ \(path, (unhip, web_and_excite, other)) -> do
      it ("unhipbot access to " ++ show path) $
        canAccess "unhipbot" robot path `shouldBe` unhip
      it ("webcrawler & excite access to " ++ show path) $ do
        canAccess "webcrawler" robot path `shouldBe` web_and_excite
        canAccess "excite" robot path `shouldBe` web_and_excite
      it ("otherbot access to " ++ show path) $ canAccess "otherbot" robot path `shouldBe` other

-- describe "allowable" $ do
--   let robot = [([Wildcard], [Disallow "/", Allow "/anyone"]),
--                ([Literal "OtherSpecial", Literal "SpecialBot"], [Allow "/", Allow "/only_special"])]
--   it "should allow access to anyone" $ do
--     canAccess "anyone"  robot "/anyone" `shouldBe` True
--   it "should deny access to root for most bots" $ do
--     canAccess "anyone"  robot "/" `shouldBe` False
--   it "should deny access to only_special for most bots" $ do
--     canAccess "anyone"  robot "/only_special" `shouldBe` False
--   it "allows access to specialbot" $ do
--     canAccess "SpecialBot"  robot "/" `shouldBe` True
--   it "allows access to specialbot special area" $ do
--     canAccess "SpecialBot"  robot "/only_special" `shouldBe` True
--   it "allows access to specialbot special area" $ do
--     canAccess "OtherSpecial"  robot "/only_special" `shouldBe` True

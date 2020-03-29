{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Weverything -Werror 
   -fno-warn-missing-import-lists 
   -fno-warn-incomplete-uni-patterns
   -fno-warn-all-missed-specialisations
   -fno-warn-unsafe #-}

module Main
  ( main,
  )
where

{- HLINT ignore "Redundant do" -}
{- HLINT ignore "Use const" -}

import qualified Control.Exception as Exception
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (parseEither)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.Foldable (for_)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Client
  ( Manager,
    Request (..),
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
    responseStatus,
    urlEncodedBody,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.HTTP.Types as HTTP
import Network.HTTP.Types (Status (..))
import Refined (SizeLessThan)
import Refined.Unsafe (unsafeRefine)
import Servant.Client (ClientEnv, ClientM, mkClientEnv, runClientM)
import Servant.Client.Core (AuthenticatedRequest)
import qualified System.Console.ANSI as ANSI
import System.Environment (getEnv, lookupEnv)
import System.Exit (die)
import qualified System.Random as Random
import Test.Hspec.Core.Spec (Item (..), mapSpecItem)
import Test.Tasty
import Test.Tasty.Hspec
import Text.Read (readMaybe)
import Web.Spotify.API
import Web.Spotify.API.Client
import Prelude

data TestEnv
  = TestEnv
      { envRandomStdGen :: Random.StdGen,
        envSpotifyClientEnv :: ClientEnv,
        envSpotifyToken :: SpotifyToken
      }

mkTestTree :: TestEnv -> IO TestTree
mkTestTree testEnv = do
  specs <- testSpecs $ do
    parallel $ specAlbumAPI testEnv
    parallel $ specArtistsAPI testEnv
    parallel $ specBrowseAPI testEnv
    parallel $ specSearchAPI testEnv
  pure (testGroup "Tests" specs)

-- Albums ----------------------------------------------------------------------
-- https://developer.spotify.com/console/albums/

specAlbumAPI :: TestEnv -> Spec
specAlbumAPI testEnv =
  describe "AlbumAPI" $ do
    specGetAlbum testEnv
    specGetAlbumsTracks testEnv
    specGetSeveralAlbums testEnv

specGetAlbum :: TestEnv -> Spec
specGetAlbum testEnv =
  describe "getAlbum" $ do
    runTestCases testEnv testCases $ \auth (description, albumID, specs) ->
      ( description,
        getAlbum auth albumID Nothing,
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith AlbumFull])]
    testCases =
      [ ( Description "Chase & Status - No More Idols",
          noMoreIdols,
          [ it "should return the correct release date" $
              \AlbumFull {release_date} ->
                release_date `shouldBe` ReleaseDate 2011 (Just 1) (Just 1)
          ]
        )
      ]

specGetAlbumsTracks :: TestEnv -> Spec
specGetAlbumsTracks testEnv =
  describe "getAlbumsTracks" $ do
    runTestCases testEnv testCases $ \auth (description, albumID, specs) ->
      ( description,
        getAlbumsTracks auth albumID Nothing Nothing Nothing,
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith (Paging TrackFull)])]
    testCases =
      [ ( Description "Tash Sultana - Flow State",
          flowState,
          [ it "should return a non-empty array" $
              \Paging {items} -> length items `shouldSatisfy` (> 0),
            it "should include \"Big Smoke\"" $
              \Paging {items} -> items `shouldSatisfy` any (\TrackFull {name} -> name == "Big Smoke")
          ]
        )
      ]

specGetSeveralAlbums :: TestEnv -> Spec
specGetSeveralAlbums testEnv =
  describe "getSeveralAlbums" $ do
    runTestCases testEnv testCases $ \auth (description, albumIDs, specs) ->
      ( description,
        getSeveralAlbums auth albumIDs Nothing,
        specs
      )
  where
    testCases ::
      [ ( Description,
          CommaSep (SizeLessThan 20) SpotifyID,
          [SpecWith (ObjectWrapper "albums" [Maybe AlbumFull])]
        )
      ]
    testCases =
      [ ( Description "No More Idols & Flow State",
          CommaSep (unsafeRefine [noMoreIdols, flowState]),
          [ it "should return an array of length two" $
              \(ObjectWrapper albums) -> length albums `shouldBe` 2
          ]
        )
      ]

-- Artists ---------------------------------------------------------------------
-- https://developer.spotify.com/console/artists/

specArtistsAPI :: TestEnv -> Spec
specArtistsAPI testEnv =
  describe "ArtistsAPI" $ do
    specGetArtist testEnv
    specGetArtistsAlbums testEnv
    specGetArtistsTopTracks testEnv
    specGetRelatedArtists testEnv
    specGetSeveralArtists testEnv

specGetArtist :: TestEnv -> Spec
specGetArtist testEnv =
  describe "getArtist" $ do
    runTestCases testEnv testCases $ \auth (description, artistID, specs) ->
      ( description,
        getArtist auth artistID,
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith ArtistFull])]
    testCases =
      [ ( Description "Red Hot Chili Peppers",
          redHotChiliPeppers,
          [ it "should return the correct artist name" $
              \ArtistFull {name} -> name `shouldBe` "Red Hot Chili Peppers",
            it "should include \"funk rock\" in genres" $
              \ArtistFull {genres} -> genres `shouldSatisfy` elem "funk rock"
          ]
        )
      ]

specGetArtistsAlbums :: TestEnv -> Spec
specGetArtistsAlbums testEnv =
  describe "getArtistsAlbums" $ do
    runTestCases testEnv testCases $ \auth (description, artistID, specs) ->
      ( description,
        getArtistsAlbums auth artistID Nothing Nothing Nothing Nothing,
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith (Paging AlbumSimplified)])]
    testCases =
      [ ( Description "Drake",
          drake,
          [ it "should return a non-empty array" $
              \Paging {items} -> length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specGetArtistsTopTracks :: TestEnv -> Spec
specGetArtistsTopTracks testEnv =
  describe "getArtistsTopTracks" $ do
    runTestCases testEnv testCases $ \auth (description, artistID, specs) ->
      ( description,
        getArtistsTopTracks auth artistID (CountryFromCountryCode (CountryCode "GB")),
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith (ObjectWrapper "tracks" [TrackFull])])]
    testCases =
      [ ( Description "Jimi Hendrix",
          jimiHendrix,
          [ it "should return a non-empty array" $
              \(ObjectWrapper tracks) -> length tracks `shouldSatisfy` (> 0),
            it "should include \"Little Wing\"" $
              \(ObjectWrapper tracks) -> tracks `shouldSatisfy` any (\TrackFull {name} -> name == "Little Wing"),
            it "should include \"Purple Haze\"" $
              \(ObjectWrapper tracks) -> tracks `shouldSatisfy` any (\TrackFull {name} -> name == "Purple Haze")
          ]
        )
      ]

specGetRelatedArtists :: TestEnv -> Spec
specGetRelatedArtists testEnv =
  describe "getRelatedArtists" $ do
    runTestCases testEnv testCases $ \auth (description, artistID, specs) ->
      ( description,
        getRelatedArtists auth artistID (CountryFromCountryCode (CountryCode "GB")),
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith (ObjectWrapper "artists" [ArtistFull])])]
    testCases =
      [ ( Description "Craig David",
          craigDavid,
          []
        )
      ]

specGetSeveralArtists :: TestEnv -> Spec
specGetSeveralArtists testEnv =
  describe "getSeveralArtists" $ do
    runTestCases testEnv testCases $ \auth (description, artistIDs, specs) ->
      ( description,
        getSeveralArtists auth artistIDs,
        specs
      )
  where
    testCases ::
      [ ( Description,
          CommaSep (SizeLessThan 50) SpotifyID,
          [SpecWith (ObjectWrapper "artists" [Maybe ArtistFull])]
        )
      ]
    testCases =
      [ ( Description "Craig David & Jimi Hendrix",
          CommaSep (unsafeRefine [craigDavid, jimiHendrix]),
          [ it "should return an array of length two" $
              \(ObjectWrapper artists) -> length artists `shouldBe` 2
          ]
        )
      ]

-- Browse ---------------------------------------------------------------------
-- https://developer.spotify.com/console/browse/

specBrowseAPI :: TestEnv -> Spec
specBrowseAPI testEnv =
  describe "BrowseAPI" $ do
    specGetCategory testEnv
    specGetCategorysPlaylists testEnv
    specGetListCategories testEnv
    specGetListFeaturedPlaylists testEnv
    specGetListNewReleases testEnv
    specGetRecommendations testEnv
    specGetAvailableGenreSeeds testEnv

specGetCategory :: TestEnv -> Spec
specGetCategory testEnv =
  describe "getCategory" $ do
    runTestCases testEnv testCases $ \auth (description, categoryID, specs) ->
      ( description,
        getCategory auth categoryID Nothing Nothing,
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith Category])]
    testCases =
      [ ( Description "\"edm_dance\"",
          "edm_dance",
          [ it "returns the name \"Electronic/Dance\"" $
              \Category {name} -> name `shouldBe` "Electronic/Dance"
          ]
        )
      ]

specGetCategorysPlaylists :: TestEnv -> Spec
specGetCategorysPlaylists testEnv =
  describe "getCategorysPlaylists" $ do
    runTestCases testEnv testCases $ \auth (description, categoryID, specs) ->
      ( description,
        getCategorysPlaylists auth categoryID Nothing Nothing Nothing,
        specs
      )
  where
    testCases :: [(Description, SpotifyID, [SpecWith (ObjectWrapper "playlists" (Paging PlaylistSimplified))])]
    testCases =
      [ ( Description "\"indie_alt\"",
          "indie_alt",
          [ it "returns a non-empty array" $
              \(ObjectWrapper Paging {items}) -> length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specGetListCategories :: TestEnv -> Spec
specGetListCategories testEnv =
  describe "getListCategories" $ do
    runTestCases testEnv testCases $ \auth (description, specs) ->
      ( description,
        getListCategories auth Nothing Nothing Nothing Nothing,
        specs
      )
  where
    testCases :: [(Description, [SpecWith (ObjectWrapper "categories" (Paging Category))])]
    testCases =
      [ ( Description "Check the response is sensible",
          [ it "returns a non-empty array " $
              \(ObjectWrapper Paging {items}) -> length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specGetListFeaturedPlaylists :: TestEnv -> Spec
specGetListFeaturedPlaylists testEnv =
  describe "getListFeaturedPlaylists" $ do
    runTestCases testEnv testCases $ \auth (description, specs) ->
      ( description,
        getListFeaturedPlaylists auth Nothing Nothing Nothing Nothing Nothing,
        specs
      )
  where
    testCases :: [(Description, [SpecWith FeaturedPlaylists])]
    testCases =
      [ ( Description "Check there are featured playlists",
          [ it "returns a non-empty array " $
              \FeaturedPlaylists {playlists = Paging {items}} ->
                length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specGetListNewReleases :: TestEnv -> Spec
specGetListNewReleases testEnv =
  describe "getListNewReleases" $ do
    runTestCases testEnv testCases $ \auth (description, specs) ->
      ( description,
        getListNewReleases auth Nothing Nothing Nothing,
        specs
      )
  where
    testCases :: [(Description, [SpecWith NewReleases])]
    testCases =
      [ ( Description "Check there are new releases",
          [ it "returns a non-empty array " $
              \NewReleases {albums = Paging {items}} ->
                length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specGetRecommendations :: TestEnv -> Spec
specGetRecommendations testEnv =
  describe "getRecommendations" $ do
    runTestCases testEnv testCases $ \auth (description, attributes, seeds, specs) ->
      ( description,
        getRecommendations auth Nothing Nothing attributes seeds,
        specs
      )
  where
    testCases :: [(Description, TrackAttributes, TrackSeeds, [SpecWith Recommendations])]
    testCases =
      [ ( Description "Single artist seed (Chase & Status)",
          TrackAttributes [],
          TrackSeeds (unsafeRefine [SeedArtist chaseAndStatus]),
          [ it "returns a non-empty array " $
              \Recommendations {tracks} ->
                length tracks `shouldSatisfy` (> 0)
          ]
        ),
        ( Description "Single artist seed (Tash Sultana) with min_tempo=120",
          TrackAttributes
            [ (Min, Tempo 120.0)
            ],
          TrackSeeds (unsafeRefine [SeedArtist tashSultana]),
          [ it "returns a non-empty array " $
              \Recommendations {tracks} ->
                length tracks `shouldSatisfy` (> 0)
          ]
        )
      ]

specGetAvailableGenreSeeds :: TestEnv -> Spec
specGetAvailableGenreSeeds testEnv =
  describe "getAvailableGenreSeeds" $ do
    runTestCases testEnv testCases $ \auth (description, specs) ->
      ( description,
        getAvailableGenreSeeds auth,
        specs
      )
  where
    testCases :: [(Description, [SpecWith (ObjectWrapper "genres" [Text])])]
    testCases =
      [ ( Description "Check the response is sensible",
          [ it "returns a non-empty array " $
              \(ObjectWrapper genres) -> length genres `shouldSatisfy` (> 0)
          ]
        )
      ]

-- Search ----------------------------------------------------------------------
-- https://developer.spotify.com/console/search/

specSearchAPI :: TestEnv -> Spec
specSearchAPI testEnv =
  describe "SearchAPI" $ do
    specSearchAlbum testEnv
    specSearchArtist testEnv
    specSearchPlaylist testEnv
    specSearchTrack testEnv
    specSearchShow testEnv
    specSearchEpisode testEnv

specSearchAlbum :: TestEnv -> Spec
specSearchAlbum testEnv =
  describe "searchAlbum" $ do
    runTestCases testEnv testCases $ \auth (description, query, specs) ->
      ( description,
        searchAlbum auth query Nothing Nothing Nothing,
        specs
      )
  where
    testCases ::
      [ ( Description,
          Text, -- query
          [SpecWith (ObjectWrapper "albums" (Paging AlbumSimplified))]
        )
      ]
    testCases =
      [ ( Description "\"stadium arcadium\"",
          "stadium arcadium",
          [ it "should return the Red Hot Chili Peppers album" $
              \(ObjectWrapper Paging {items}) ->
                items
                  `shouldSatisfy` any
                    ( \AlbumSimplified {artists} ->
                        any (\ArtistSimplified {name} -> name == "Red Hot Chili Peppers") artists
                    )
          ]
        )
      ]

specSearchArtist :: TestEnv -> Spec
specSearchArtist testEnv =
  describe "searchArtist" $ do
    runTestCases testEnv testCases $ \auth (description, query, specs) ->
      ( description,
        searchArtist auth query Nothing Nothing Nothing,
        specs
      )
  where
    testCases ::
      [ ( Description,
          Text, -- query
          [SpecWith (ObjectWrapper "artists" (Paging ArtistFull))]
        )
      ]
    testCases =
      [ ( Description "\"ludovico einaudi\"",
          "ludovico einaudi",
          [ it "should return a non-empty array" $
              \(ObjectWrapper Paging {items}) -> length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specSearchPlaylist :: TestEnv -> Spec
specSearchPlaylist testEnv =
  describe "searchPlaylist" $ do
    runTestCases testEnv testCases $ \auth (description, query, specs) ->
      ( description,
        searchPlaylist auth query Nothing Nothing Nothing,
        specs
      )
  where
    testCases ::
      [ ( Description,
          Text, -- query
          [SpecWith (ObjectWrapper "playlists" (Paging PlaylistSimplified))]
        )
      ]
    testCases =
      [ ( Description "\"this is young money\"",
          "this is young money",
          [ it "should return a non-empty array" $
              \(ObjectWrapper Paging {items}) -> length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specSearchTrack :: TestEnv -> Spec
specSearchTrack testEnv =
  describe "searchTrack" $ do
    runTestCases testEnv testCases $ \auth (description, query, specs) ->
      ( description,
        searchTrack auth query Nothing Nothing Nothing,
        specs
      )
  where
    testCases ::
      [ ( Description,
          Text, -- query
          [SpecWith (ObjectWrapper "tracks" (Paging TrackFull))]
        )
      ]
    testCases =
      [ ( Description "\"i am very good at organizing meetings\"",
          "i am very good at organizing meetings",
          [ it "should return a non-empty array" $
              \(ObjectWrapper Paging {items}) -> length items `shouldSatisfy` (> 0)
          ]
        )
      ]

specSearchShow :: TestEnv -> Spec
specSearchShow _testEnv =
  xdescribe "searchShow" $ do
    pure ()

specSearchEpisode :: TestEnv -> Spec
specSearchEpisode _testEnv =
  xdescribe "searchEpisode" $ do
    pure ()

-- IDs -------------------------------------------------------------------------

redHotChiliPeppers :: SpotifyID
redHotChiliPeppers = SpotifyID "0L8ExT028jH3ddEcZwqJJ5"

drake :: SpotifyID
drake = "3TVXtAsR1Inumwj472S9r4"

jimiHendrix :: SpotifyID
jimiHendrix = "776Uo845nYHJpNaStv1Ds4"

craigDavid :: SpotifyID
craigDavid = "5OaansXwVygq37JIriJFti"

chaseAndStatus :: SpotifyID
chaseAndStatus = SpotifyID "3jNkaOXasoc7RsxdchvEVq"

noMoreIdols :: SpotifyID
noMoreIdols = "07kFkDwh0cNB1Mhce4wBUE"

tashSultana :: SpotifyID
tashSultana = "6zVFRTB0Y1whWyH7ZNmywf"

flowState :: SpotifyID
flowState = "12APhHuuzwZGSy2sDehOzW"

--------------------------------------------------------------------------------

runTestCases ::
  Show b =>
  TestEnv ->
  [a] ->
  (AuthenticatedRequest SpotifyAuth -> a -> (Description, ClientM b, [SpecWith b])) ->
  Spec
runTestCases TestEnv {..} testCases prepare =
  for_ testCases $ \testCase -> do
    let auth = mkAuthenticatedRequest envSpotifyToken
    let (description, clientM, spec) = prepare auth testCase
    describeClientM description clientM envSpotifyClientEnv spec

newtype Description = Description String

describeClientM :: Show a => Description -> ClientM a -> ClientEnv -> [SpecWith a] -> Spec
describeClientM (Description description) clientM clientEnv specs =
  describe description $ do
    result <- runIO (runClientM clientM clientEnv)
    it "should respond without error" $
      result `shouldSatisfy` isRight
    case result of
      Left _ -> pure ()
      Right a ->
        for_ specs $ \spec ->
          mapSpecItem
            (\action _ -> action a)
            ( \item ->
                item
                  { itemExample = \params _ cb ->
                      itemExample item params (\action -> action a) cb
                  }
            )
            spec

main :: IO ()
main = do
  -- Get credentials from the environment
  -- (failing early if they're missing)
  clientCredentials <- lookupCredentials
  httpsManager <- newManager tlsManagerSettings
  seed <- lookupEnv "SEED" >>= \case
    Nothing ->
      round <$> getPOSIXTime
    Just seedString ->
      maybe (die "invalid $SEED") pure (readMaybe seedString)
  putStrLn $
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
      <> "Seed"
      <> ANSI.setSGRCode [ANSI.Reset]
      <> " = "
      <> show seed
  let randomStdGen = Random.mkStdGen seed
  spotifyToken@(BearerToken token _) <- getSpotifyToken httpsManager clientCredentials
  putStrLn $
    ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
      <> "Bearer Token"
      <> ANSI.setSGRCode [ANSI.Reset]
      <> " = "
      <> Text.unpack token
  testTree <-
    mkTestTree
      TestEnv
        { envRandomStdGen = randomStdGen,
          envSpotifyClientEnv = mkClientEnv httpsManager spotifyAPIBaseUrl,
          envSpotifyToken = spotifyToken
        }
  defaultMain testTree

lookupCredentials :: IO ClientCredentials
lookupCredentials =
  ClientCredentials
    <$> fmap Char8.pack (getEnv "HSPOTIFY_TEST_CLIENT_ID")
    <*> fmap Char8.pack (getEnv "HSPOTIFY_TEST_CLIENT_SECRET")

data ClientCredentials
  = ClientCredentials
      { clientID :: ByteString,
        clientSecret :: ByteString
      }

getSpotifyToken :: Manager -> ClientCredentials -> IO SpotifyToken
getSpotifyToken httpsManager clientCredentials = do
  initialRequest <- parseRequest "https://accounts.spotify.com/api/token"
  let credentialsHeader :: HTTP.Header
      credentialsHeader =
        ( "Authorization",
          "Basic " <> Base64.encode (clientID clientCredentials <> ":" <> clientSecret clientCredentials)
        )
  let request :: Request
      request =
        urlEncodedBody [("grant_type", "client_credentials")] $
          initialRequest
            { method = "POST",
              requestHeaders = credentialsHeader : requestHeaders initialRequest
            }
  response <- httpLbs request httpsManager
  case statusCode (responseStatus response) of
    200 ->
      case parseResponseBody (responseBody response) of
        Left err -> Exception.throw (BadResponseBody err (responseBody response))
        Right (accessToken, "Bearer", expiresIn) -> do
          now <- getCurrentTime
          let expires = addUTCTime expiresIn now
          pure (BearerToken accessToken expires)
        Right (_, tokenType, _) -> do
          Exception.throw (BadTokenType tokenType)
    code ->
      Exception.throw (BadResponseStatus code)
  where
    parseResponseBody :: LBS.ByteString -> Either String (Text, Text, NominalDiffTime)
    parseResponseBody = Aeson.parseEither $ \body -> do
      value <- either (\err -> fail $ "invalid JSON: " <> err) pure (Aeson.eitherDecode body)
      object <- Aeson.parseJSON value
      accessToken <- object .: "access_token"
      tokenType <- object .: "token_type"
      expiresIn <- object .: "expires_in"
      pure (accessToken, tokenType, fromInteger expiresIn)

data GetSpotifyTokenError
  = -- | Non-200 response
    BadResponseStatus Int
  | -- | Aeson error and response body
    BadResponseBody String LBS.ByteString
  | -- | Unknown token type
    BadTokenType Text
  deriving stock (Show)
  deriving anyclass (Exception.Exception)

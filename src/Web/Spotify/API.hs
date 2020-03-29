{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Weverything -Werror 
   -fno-warn-missing-export-lists
   -fno-warn-missing-import-lists 
   -fno-warn-all-missed-specialisations
   -fno-warn-unsafe
   -funbox-strict-fields #-}

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/>
module Web.Spotify.API where

import Control.Arrow (first)
import Data.Aeson
  ( (.:),
    FromJSON (parseJSON),
    GFromJSON,
    Options (..),
    Zero,
    defaultOptions,
    genericParseJSON,
    withObject,
    withText,
  )
import Data.Aeson.Types (Parser)
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import GHC.Generics (Generic (Rep))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Refined hiding (NonEmpty)
import Refined.Orphan.Aeson ()
import Servant.API
import Servant.Client.Core (AuthClientData, HasClient (..), Request, appendToQueryString)
import Text.Read (readMaybe)
import Prelude

-- * Albums
--------------------------------------------------------------------------------

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/albums/>
type AlbumsAPI =
  GetAlbum
    :<|> GetAlbumsTracks
    :<|> GetSeveralAlbums

-- |
-- @GET https://api.spotify.com/v1/albums/{id}@
--
-- <https://developer.spotify.com/documentation/web-api/reference/albums/get-album/>
type GetAlbum =
  SpotifyAuth :> "v1" :> "albums" :> Capture "id" SpotifyID
    :> OptionalQueryParam "market" Country
    :> Get '[JSON] AlbumFull

-- |
-- @GET https://api.spotify.com/v1/albums/{id}/tracks@
--
-- <https://developer.spotify.com/documentation/web-api/reference/albums/get-albums-tracks/>
type GetAlbumsTracks =
  SpotifyAuth :> "v1" :> "albums" :> Capture "id" SpotifyID :> "tracks"
    :> OptionalQueryParam "limit" (Limit 50)
    :> OptionalQueryParam "offset" Offset
    :> OptionalQueryParam "market" Country
    :> Get '[JSON] (Paging TrackFull)

-- |
-- @GET https://api.spotify.com/v1/albums@
--
-- <https://developer.spotify.com/documentation/web-api/reference/albums/get-several-albums/>
type GetSeveralAlbums =
  SpotifyAuth :> "v1" :> "albums"
    :> RequiredQueryParam "ids" (CommaSep (SizeLessThan 20) SpotifyID)
    :> OptionalQueryParam "market" Country
    :> Get '[JSON] (ObjectWrapper "albums" [Maybe AlbumFull])

-- * Artists
--------------------------------------------------------------------------------

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/artists/>
type ArtistsAPI =
  GetArtist
    :<|> GetArtistsAlbums
    :<|> GetArtistsTopTracks
    :<|> GetRelatedArtists
    :<|> GetSeveralArtists

-- |
-- @GET https://api.spotify.com/v1/artists/{id}@
--
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-artist/>
type GetArtist =
  SpotifyAuth :> "v1" :> "artists" :> Capture "id" SpotifyID
    :> Get '[JSON] ArtistFull

-- |
-- @GET https://api.spotify.com/v1/artists/{id}/albums@
--
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-artists-albums/>
type GetArtistsAlbums =
  SpotifyAuth :> "v1" :> "artists" :> Capture "id" SpotifyID :> "albums"
    :> OptionalQueryParam "include_groups" (CommaSep IdPred AlbumGroup)
    :> OptionalQueryParam "country" Country
    :> OptionalQueryParam "limit" (Limit 50)
    :> OptionalQueryParam "offset" Offset
    :> Get '[JSON] (Paging AlbumSimplified)

-- |
-- @GET https://api.spotify.com/v1/artists/{id}/top-tracks@
--
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-artists-top-tracks/>
type GetArtistsTopTracks =
  SpotifyAuth :> "v1" :> "artists" :> Capture "id" SpotifyID :> "top-tracks"
    :> RequiredQueryParam "country" Country
    :> Get '[JSON] (ObjectWrapper "tracks" [TrackFull])

-- |
-- @GET https://api.spotify.com/v1/artists/{id}/related-artists@
--
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-related-artists/>
type GetRelatedArtists =
  SpotifyAuth :> "v1" :> "artists" :> Capture "id" SpotifyID :> "related-artists"
    :> RequiredQueryParam "country" Country
    :> Get '[JSON] (ObjectWrapper "artists" [ArtistFull])

-- |
-- @GET https://api.spotify.com/v1/artists@
--
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-several-artists/>
type GetSeveralArtists =
  SpotifyAuth :> "v1" :> "artists"
    :> RequiredQueryParam "ids" (CommaSep (SizeLessThan 50) SpotifyID)
    :> Get '[JSON] (ObjectWrapper "artists" [Maybe ArtistFull])

-- * Browse
--------------------------------------------------------------------------------

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/>
type BrowseAPI =
  GetCategory
    :<|> GetCategorysPlaylists
    :<|> GetListCategories
    :<|> GetListFeaturedPlaylists
    :<|> GetListNewReleases
    :<|> GetRecommendations
    :<|> GetAvailableGenreSeeds

-- |
-- @GET https://api.spotify.com/v1/browse/categories/{category_id}@
--
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-category/>
type GetCategory =
  SpotifyAuth :> "v1" :> "browse" :> "categories" :> Capture "category_id" SpotifyID
    :> OptionalQueryParam "country" CountryCode
    :> OptionalQueryParam "locale" Locale
    :> Get '[JSON] Category

-- |
-- @GET https://api.spotify.com/v1/browse/categories/{category_id}/playlists@
--
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-categorys-playlists/>
type GetCategorysPlaylists =
  SpotifyAuth :> "v1" :> "browse" :> "categories" :> Capture "category_id" SpotifyID :> "playlists"
    :> OptionalQueryParam "country" CountryCode
    :> OptionalQueryParam "limit" (Limit 50)
    :> OptionalQueryParam "offset" Offset
    :> Get '[JSON] (ObjectWrapper "playlists" (Paging PlaylistSimplified))

-- |
-- @GET https://api.spotify.com/v1/browse/categories@
--
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-list-categories/>
type GetListCategories =
  SpotifyAuth :> "v1" :> "browse" :> "categories"
    :> OptionalQueryParam "country" CountryCode
    :> OptionalQueryParam "locale" Locale
    :> OptionalQueryParam "limit" (Limit 50)
    :> OptionalQueryParam "offset" Offset
    :> Get '[JSON] (ObjectWrapper "categories" (Paging Category))

-- |
-- @GET https://api.spotify.com/v1/browse/featured-playlists@
--
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-list-featured-playlists/>
type GetListFeaturedPlaylists =
  SpotifyAuth :> "v1" :> "browse" :> "featured-playlists"
    :> OptionalQueryParam "locale" Locale
    :> OptionalQueryParam "country" CountryCode
    :> OptionalQueryParam "timestamp" Timestamp
    :> OptionalQueryParam "limit" (Limit 50)
    :> OptionalQueryParam "offset" Offset
    :> Get '[JSON] FeaturedPlaylists

-- |
-- @GET https://api.spotify.com/v1/browse/new-releases@
--
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-list-new-releases/>
type GetListNewReleases =
  SpotifyAuth :> "v1" :> "browse" :> "new-releases"
    :> OptionalQueryParam "country" CountryCode
    :> OptionalQueryParam "limit" (Limit 50)
    :> OptionalQueryParam "offset" Offset
    :> Get '[JSON] NewReleases

-- |
-- @GET https://api.spotify.com/v1/recommendations@
--
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-recommendations/>
type GetRecommendations =
  SpotifyAuth :> "v1" :> "recommendations"
    :> OptionalQueryParam "limit" (Limit 100)
    :> OptionalQueryParam "market" Country
    :> TrackAttributes
    :> TrackSeeds
    :> Get '[JSON] Recommendations

-- |
-- @GET https://api.spotify.com/v1/recommendations/available-genre-seeds@
--
-- <https://developer.spotify.com/console/get-available-genre-seeds/>
type GetAvailableGenreSeeds =
  SpotifyAuth :> "v1" :> "recommendations" :> "available-genre-seeds"
    :> Get '[JSON] (ObjectWrapper "genres" [Text])

-- * Search
--------------------------------------------------------------------------------

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/search/search/>
type SearchAPI =
  SearchAlbum
    :<|> SearchArtist
    :<|> SearchPlaylist
    :<|> SearchTrack
    :<|> SearchShow
    :<|> SearchEpisode

type SearchAlbum =
  SearchType "album" (ObjectWrapper "albums" (Paging AlbumSimplified))

type SearchArtist =
  SearchType "artist" (ObjectWrapper "artists" (Paging ArtistFull))

type SearchPlaylist =
  SearchType "playlist" (ObjectWrapper "playlists" (Paging PlaylistSimplified))

type SearchTrack =
  SearchType "track" (ObjectWrapper "tracks" (Paging TrackFull))

type SearchShow =
  SearchType "show" (ObjectWrapper "shows" (Paging ShowSimplified))

type SearchEpisode =
  SearchType "episode" (ObjectWrapper "episodes" (Paging EpisodeSimplified))

type SearchType t response =
  SpotifyAuth :> "v1" :> "search"
    :> RequiredQueryParam "q" Text
    :> SearchTypeParam t
    :> OptionalQueryParam "market" Country
    :> OptionalQueryParam "limit" (Limit 50)
    :> OptionalQueryParam "offset" Offset
    -- "include_external" is also a query param but doesn't seem useful...
    :> Get '[JSON] response

data SearchTypeParam (t :: Symbol)

instance (KnownSymbol t, HasClient m api) => HasClient m (SearchTypeParam t :> api) where
  type Client m (SearchTypeParam t :> api) = Client m api

  clientWithRoute :: Proxy m -> Proxy (SearchTypeParam t :> api) -> Request -> Client m api
  clientWithRoute pm _ request =
    let typeText = Text.pack (symbolVal (Proxy @t))
     in clientWithRoute pm (Proxy @api) (appendToQueryString "type" (Just typeText) request)

  hoistClientMonad pm _ =
    hoistClientMonad pm (Proxy @api)

-- * Auth
--------------------------------------------------------------------------------

type SpotifyAuth = AuthProtect "spotify"

type instance AuthClientData SpotifyAuth = SpotifyToken

data SpotifyToken
  = BearerToken
      { bearerAccessToken :: Text,
        bearerExpires :: UTCTime
      }
  deriving stock (Show)

-- * Object Model
--------------------------------------------------------------------------------

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#album-object-full>
data AlbumFull
  = AlbumFull
      { album_type :: AlbumType,
        artists :: [ArtistSimplified],
        available_markets :: [CountryCode],
        copyrights :: [Copyright],
        external_ids :: ExternalID,
        external_urls :: ExternalURL,
        genres :: [Text],
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        label :: Text,
        name :: Text,
        popularity :: Refined (FromTo 0 100) Int,
        release_date :: ReleaseDate,
        -- release_date_precision :: _  <-- we can work this out from release_date
        restrictions :: Maybe Restrictions,
        tracks :: Paging TrackSimplified,
        type_ :: TypeTag "album",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON AlbumFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#album-object-simplified>
data AlbumSimplified
  = AlbumSimplified
      { album_group :: Maybe AlbumGroup,
        album_type :: AlbumType,
        artists :: [ArtistSimplified],
        available_markets :: Maybe [CountryCode],
        external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        name :: Text,
        release_date :: ReleaseDate,
        -- release_date_precision :: _  <-- we can work this out from release_date
        restrictions :: Maybe Restrictions,
        type_ :: TypeTag "album",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON AlbumSimplified

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-full>
data ArtistFull
  = ArtistFull
      { external_urls :: ExternalURL,
        followers :: Followers,
        genres :: [Text],
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        name :: Text,
        popularity :: Refined (FromTo 0 100) Int,
        type_ :: TypeTag "artist",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON ArtistFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#artist-object-simplified>
data ArtistSimplified
  = ArtistSimplified
      { external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        name :: Text,
        type_ :: TypeTag "artist",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON ArtistSimplified

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#audio-features-object>
data AudioFeatures
  = AudioFeatures
      { acousticness :: Double,
        analysis_url :: Text,
        danceability :: Double,
        duration_ms :: Int,
        energy :: Double,
        id :: SpotifyID,
        instrumentalness :: Double,
        key :: Int,
        liveness :: Double,
        loudness :: Double,
        mode :: Int,
        speechiness :: Double,
        tempo :: Double,
        time_signature :: Int,
        track_href :: Text,
        type_ :: TypeTag "audio_features",
        uri :: SpotifyURI,
        valence :: Double
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON AudioFeatures

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#category-object>
data Category
  = Category
      { href :: Text,
        icons :: [Image],
        id :: SpotifyCategoryID,
        name :: Text
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#context-object>
data Context
  = ArtistContext Text ExternalURL SpotifyURI
  | PlaylistContext Text ExternalURL SpotifyURI
  | AlbumContext Text ExternalURL SpotifyURI
  deriving stock (Generic, Show)

instance FromJSON Context where
  parseJSON = withObject "Context" $ \obj ->
    obj .: "type" >>= \case
      "artist" -> ArtistContext <$> (obj .: "href") <*> (obj .: "external_urls") <*> (obj .: "uri")
      "playlist" -> PlaylistContext <$> (obj .: "href") <*> (obj .: "external_urls") <*> (obj .: "uri")
      "album" -> AlbumContext <$> (obj .: "href") <*> (obj .: "external_urls") <*> (obj .: "uri")
      other -> fail ("unknown type: " <> Text.unpack other)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#copyright-object>
data Copyright
  = Copyright
      { text :: Text,
        type_ :: Maybe CopyrightType
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON Copyright

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#cursor-object>
newtype Cursor
  = Cursor {after :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#disallows-object>
data Disallows
  = Disallows
      { interrupting_playback :: Maybe Bool,
        pausing :: Maybe Bool,
        resuming :: Maybe Bool,
        seeking :: Maybe Bool,
        skipping_next :: Maybe Bool,
        skipping_prev :: Maybe Bool,
        toggling_repeat_context :: Maybe Bool,
        toggling_shuffle :: Maybe Bool,
        toggling_repeat_track :: Maybe Bool,
        transferring_playback :: Maybe Bool
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#error-object>
data Error
  = Error
      { status :: Int,
        message :: Text
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#player-error-object>
data PlayerError
  = PlayerError
      { status :: Int,
        message :: Text,
        reason :: Text
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#external-id-object>
type ExternalID = HashMap Text Text

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#external-url-object>
type ExternalURL = HashMap Text Text

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#followers-object>
data Followers
  = Followers
      { href :: Maybe Text,
        total :: Int
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#image-object>
data Image
  = Image
      { height :: Maybe Int,
        url :: Text,
        width :: Maybe Int
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#paging-object>
data Paging a
  = Paging
      { href :: Text,
        items :: [a],
        limit :: Int,
        next :: Maybe Text,
        offset :: Int,
        previous :: Maybe Text,
        total :: Int
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#paging-object>
data PartialPaging a
  = PartialPaging
      { href :: Text,
        items :: Maybe [a],
        limit :: Maybe Int,
        next :: Maybe Text,
        offset :: Maybe Int,
        previous :: Maybe Text,
        total :: Int
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#cursor-based-paging-object>
data CursorPaging a
  = CursorPaging
      { href :: Text,
        items :: Maybe [a],
        limit :: Maybe Int,
        next :: Maybe Text,
        cursor :: Cursor,
        total :: Int
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#play-history-object>
data PlayHistory
  = PlayHistory
      { track :: TrackSimplified,
        played_at :: Timestamp,
        context :: Context
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#playlist-object-full>
data PlaylistFull
  = PlaylistFull
      { collaborative :: Bool,
        description :: Text,
        external_urls :: ExternalURL,
        followers :: Followers,
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        name :: Text,
        owner :: UserPublic,
        public :: Maybe Bool,
        snapshot_id :: Text,
        tracks :: PartialPaging PlaylistTrack,
        type_ :: TypeTag "playlist",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON PlaylistFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#playlist-object-simplified>
data PlaylistSimplified
  = PlaylistSimplified
      { collaborative :: Bool,
        description :: Text,
        external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        name :: Text,
        owner :: UserPublic,
        public :: Maybe Bool,
        snapshot_id :: Text,
        tracks :: PartialPaging PlaylistTrack,
        type_ :: TypeTag "playlist",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON PlaylistSimplified

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#playlist-track-object>
data PlaylistTrack
  = PlaylistTrack
      { added_at :: Timestamp,
        added_by :: UserPublic,
        is_local :: Bool,
        track :: Either TrackFull EpisodeFull
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#recommendations-object>
data Recommendations
  = Recommendations
      { seeds :: [RecommendationsSeed],
        tracks :: [TrackSimplified]
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#recommendations-seed-object>
data RecommendationsSeed
  = RecommendationsSeed
      { afterFilteringSize :: Int,
        afterRelinkingSize :: Int,
        href :: Text,
        id :: Text,
        initialPoolSize :: Int,
        type_ :: Text
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON RecommendationsSeed

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#saved-track-object>
data SavedTrack
  = SavedTrack
      { added_at :: Timestamp,
        track :: TrackFull
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#saved-album-object>
data SavedAlbum
  = SavedAlbum
      { added_at :: Timestamp,
        album :: AlbumFull
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#saved-show-object>
data SavedShow
  = SavedShow
      { added_at :: Timestamp,
        show_ :: ShowFull
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-full>
data TrackFull
  = TrackFull
      { album :: Maybe AlbumSimplified,
        artists :: [ArtistSimplified],
        available_markets :: Maybe [CountryCode],
        disc_number :: Int,
        duration_ms :: Int,
        explicit :: Bool,
        external_ids :: Maybe ExternalID,
        href :: Text,
        id :: SpotifyID,
        is_playable :: Maybe Bool,
        linked_from :: Maybe TrackLink,
        restrictions :: Maybe Restrictions,
        name :: Text,
        popularity :: Maybe (Refined (FromTo 0 100) Int),
        preview_url :: Maybe Text,
        track_number :: Int,
        type_ :: TypeTag "track",
        uri :: SpotifyURI,
        is_local :: Bool
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON TrackFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#track-object-simplified>
data TrackSimplified
  = TrackSimplified
      { artists :: [ArtistSimplified],
        available_markets :: [CountryCode],
        disc_number :: Int,
        duration_ms :: Int,
        explicit :: Bool,
        external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        is_playable :: Maybe Bool,
        linked_from :: Maybe TrackLink,
        restrictions :: Maybe Restrictions,
        name :: Text,
        preview_url :: Maybe Text,
        track_number :: Int,
        type_ :: TypeTag "track",
        uri :: SpotifyURI,
        is_local :: Bool
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON TrackSimplified

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#track-link-object>
data TrackLink
  = TrackLink
      { external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        type_ :: TypeTag "track",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON TrackLink

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#episode-object-full>
data EpisodeFull
  = EpisodeFull
      { audio_preview_url :: Maybe Text,
        description :: Text,
        duration_ms :: Int,
        explicit :: Bool,
        external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        is_externally_hosted :: Bool,
        is_playable :: Bool,
        -- language :: LanguageCode <-- deprecated
        languages :: [LanguageCode],
        name :: Text,
        release_date :: ReleaseDate,
        -- release_date_precision :: _  <-- we can work this out from release_date
        resume_point :: ResumePoint,
        show_ :: ShowSimplified,
        type_ :: TypeTag "episode",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON EpisodeFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#episode-object-simplified>
data EpisodeSimplified
  = EpisodeSimplified
      { audio_preview_url :: Maybe Text,
        description :: Text,
        duration_ms :: Int,
        explicit :: Bool,
        external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        is_externally_hosted :: Bool,
        is_playable :: Bool,
        -- language :: LanguageCode <-- deprecated
        languages :: [LanguageCode],
        name :: Text,
        release_date :: ReleaseDate,
        -- release_date_precision :: _  <-- we can work this out from release_date
        resume_point :: ResumePoint,
        type_ :: TypeTag "episode",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON EpisodeSimplified

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#resume-point-object>
data ResumePoint
  = ResumePoint
      { fully_played :: Bool,
        resume_position_ms :: Int
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#show-object-full>
data ShowFull
  = ShowFull
      { available_markets :: [CountryCode],
        copyrights :: [Copyright],
        description :: Text,
        explicit :: Bool,
        episodes :: Paging EpisodeSimplified,
        external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        is_externally_hosted :: Bool,
        languages :: [LanguageCode],
        media_type :: Text,
        name :: Text,
        publisher :: Text,
        type_ :: TypeTag "show",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON ShowFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#show-object-simplified>
data ShowSimplified
  = ShowSimplified
      { available_markets :: [CountryCode],
        copyrights :: [Copyright],
        description :: Text,
        explicit :: Bool,
        external_urls :: ExternalURL,
        href :: Text,
        id :: SpotifyID,
        images :: [Image],
        is_externally_hosted :: Bool,
        languages :: [LanguageCode],
        media_type :: Text,
        name :: Text,
        publisher :: Text,
        type_ :: TypeTag "show",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON ShowSimplified

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#user-object-private>
data UserPrivate
  = UserPrivate
      { country :: CountryCode,
        display_name :: Maybe Text,
        email :: Text,
        external_urls :: ExternalURL,
        followers :: Followers,
        href :: Text,
        images :: [Image],
        product :: Text,
        type_ :: TypeTag "user",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON UserPrivate

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/object-model/#user-object-public>
data UserPublic
  = UserPublic
      { display_name :: Text,
        external_urls :: ExternalURL,
        followers :: Maybe Followers,
        href :: Text,
        id :: SpotifyUserID,
        images :: Maybe [Image],
        type_ :: TypeTag "user",
        uri :: SpotifyURI
      }
  deriving stock (Generic, Show)
  deriving (FromJSON) via GenericJSON UserPublic

-- * Object Model Fields
--------------------------------------------------------------------------------

data AlbumType
  = Album
  | Single
  | Compilation
  deriving stock (Show)

instance FromJSON AlbumType where
  parseJSON = withText "AlbumType" $ \case
    "album" -> pure Album
    "single" -> pure Single
    "compilation" -> pure Compilation
    other -> fail ("unknown value: " <> Text.unpack other)

data AlbumGroup
  = AlbumGroup
  | SingleGroup
  | CompilationGroup
  | AppearsOnGroup
  deriving stock (Show)

instance FromJSON AlbumGroup where
  parseJSON = withText "AlbumGroup" $ \case
    "album" -> pure AlbumGroup
    "single" -> pure SingleGroup
    "compilation" -> pure CompilationGroup
    "appears_on" -> pure AppearsOnGroup
    other -> fail ("unknown value: " <> Text.unpack other)

instance ToHttpApiData AlbumGroup where
  toQueryParam = \case
    AlbumGroup -> "album"
    SingleGroup -> "single"
    CompilationGroup -> "compilation"
    AppearsOnGroup -> "appears_on"

data ReleaseDate
  = ReleaseDate
      { year :: Int,
        month :: Maybe Int,
        day :: Maybe Int
      }
  deriving stock (Eq, Show)

instance FromJSON ReleaseDate where
  parseJSON = withText "ReleaseDate" $ \txt ->
    case Text.splitOn "-" txt of
      [] -> fail "empty input"
      [yearText] -> do
        y <- readYear yearText
        pure (ReleaseDate y Nothing Nothing)
      [yearText, monthText] -> do
        y <- readYear yearText
        m <- readMonth monthText
        pure (ReleaseDate y (Just m) Nothing)
      [yearText, monthText, dayText] -> do
        y <- readYear yearText
        m <- readMonth monthText
        d <- readDay dayText
        pure (ReleaseDate y (Just m) (Just d))
      _ -> fail ("too many date parts: " <> Text.unpack txt)
    where
      readYear :: Text -> Parser Int
      readYear = maybe (fail "invalid year") pure . readMaybe . Text.unpack
      readMonth :: Text -> Parser Int
      readMonth = maybe (fail "invalid month") pure . readMaybe . Text.unpack
      readDay :: Text -> Parser Int
      readDay = maybe (fail "invalid day") pure . readMaybe . Text.unpack

newtype Restrictions
  = Restrictions {reason :: Text}
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data CopyrightType
  = -- | The copyright
    C
  | -- | the sound recording (performance) copyright.
    P
  deriving stock (Show)

instance FromJSON CopyrightType where
  parseJSON = withText "CopyrightType" $ \case
    "C" -> pure C
    "P" -> pure P
    other -> fail ("unknown value: " <> Text.unpack other)

-- * URIs and IDs
--------------------------------------------------------------------------------

-- |
-- <https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids>
newtype SpotifyURI = SpotifyURI Text
  deriving stock (Show)
  deriving newtype (FromJSON, IsString)

-- |
-- <https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids>
newtype SpotifyID = SpotifyID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, ToHttpApiData, FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids>
newtype SpotifyCategoryID = SpotifyCategoryID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, ToHttpApiData, FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids>
newtype SpotifyUserID = SpotifyUserID Text
  deriving stock (Show)
  deriving newtype (Eq, Ord, IsString, ToHttpApiData, FromJSON)

-- |
-- <https://developer.spotify.com/documentation/web-api/#spotify-uris-and-ids>
newtype SpotifyURL = SpotifyURL Text
  deriving stock (Show)
  deriving newtype (FromJSON, IsString)

-- * Other
--------------------------------------------------------------------------------

-- |
-- ISO 3166-1 alpha-2 country code.
--
-- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2>
newtype CountryCode = CountryCode {countryCodeText :: Text}
  deriving stock (Show)
  deriving newtype (ToHttpApiData, FromJSON)

data Country
  = CountryFromToken
  | CountryFromCountryCode CountryCode

instance ToHttpApiData Country where
  toQueryParam = \case
    CountryFromToken -> "from_token"
    CountryFromCountryCode countryCode -> toQueryParam countryCode

-- |
-- ISO 639-1 language code.
--
-- <https://en.wikipedia.org/wiki/ISO_639-1>
newtype LanguageCode = LanguageCode {languageCodeText :: Text}
  deriving stock (Show)
  deriving newtype (ToHttpApiData, FromJSON)

data Locale = Locale LanguageCode CountryCode
  deriving stock (Show)

instance ToHttpApiData Locale where
  toQueryParam (Locale (LanguageCode l) (CountryCode c)) = l <> "_" <> c

-- |
-- ISO 8601 timestamp.
--
-- <https://en.wikipedia.org/wiki/ISO_8601>
newtype Timestamp = Timestamp UTCTime
  deriving stock (Show)

instance ToHttpApiData Timestamp where
  toQueryParam (Timestamp utc) = toQueryParam (iso8601Show utc)

instance FromJSON Timestamp where
  parseJSON = withText "Timestamp" $ \txt ->
    Timestamp <$> iso8601ParseM (Text.unpack txt)

newtype Limit max = Limit (Refined (FromTo 1 max) Int)

instance ToHttpApiData (Limit max) where
  toQueryParam (Limit refined) = toQueryParam (unrefine refined)

newtype Offset = Offset (Refined Positive Int)

instance ToHttpApiData Offset where
  toQueryParam (Offset refined) = toQueryParam (unrefine refined)

data FeaturedPlaylists
  = FeaturedPlaylists
      { message :: Text,
        playlists :: Paging PlaylistSimplified
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

data NewReleases
  = NewReleases
      { message :: Maybe Text,
        albums :: Paging AlbumSimplified
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON)

newtype TrackAttributes
  = TrackAttributes
      [ ( TrackAttributeQualifier,
          TuneableTrackAttribute
        )
      ]

trackAttributesToQueryParams :: TrackAttributes -> [(Text, Maybe Text)]
trackAttributesToQueryParams (TrackAttributes attrs) =
  fmap
    ( \(qualifier, attr) ->
        first (prefixWithQualifier qualifier) (attrToQueryParam attr)
    )
    attrs
  where
    attrToQueryParam :: TuneableTrackAttribute -> (Text, Maybe Text)
    attrToQueryParam = \case
      Acousticness double -> ("acousticness", Just (toQueryParam double))
      Danceability double -> ("danceability", Just (toQueryParam double))
      DurationMilliseconds int -> ("duration_ms", Just (toQueryParam int))
      Energy double -> ("energy", Just (toQueryParam double))
      Instrumentalness double -> ("instumentalness", Just (toQueryParam double))
      Key int -> ("key", Just (toQueryParam int))
      Liveness int -> ("liveness", Just (toQueryParam int))
      Loudness double -> ("loudness", Just (toQueryParam double))
      Mode int -> ("mode", Just (toQueryParam int))
      Popularity int -> ("popularity", Just (toQueryParam int))
      Speechiness double -> ("speechiness", Just (toQueryParam double))
      Tempo double -> ("tempo", Just (toQueryParam double))
      TimeSignature int -> ("time_signature", Just (toQueryParam int))
      Valence double -> ("valence", Just (toQueryParam double))
    prefixWithQualifier :: TrackAttributeQualifier -> Text -> Text
    prefixWithQualifier = \case
      Max -> ("max_" <>)
      Min -> ("min_" <>)
      Target -> ("target_" <>)

instance (HasClient m api) => HasClient m (TrackAttributes :> api) where
  type Client m (TrackAttributes :> api) = TrackAttributes -> Client m api

  clientWithRoute :: Proxy m -> Proxy (TrackAttributes :> api) -> Request -> TrackAttributes -> Client m api
  clientWithRoute pm _ request trackAttributes =
    clientWithRoute pm (Proxy @api) $
      foldr (uncurry appendToQueryString) request (trackAttributesToQueryParams trackAttributes)

  hoistClientMonad pm _ f client trackAttributes =
    hoistClientMonad pm (Proxy @api) f (client trackAttributes)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-recommendations/#tuneable-track-attributes>
data TuneableTrackAttribute
  = Acousticness Double
  | Danceability Double
  | DurationMilliseconds Int
  | Energy Double
  | Instrumentalness Double
  | Key Int
  | Liveness Int
  | Loudness Double
  | Mode Int
  | Popularity Int
  | Speechiness Double
  | Tempo Double
  | TimeSignature Int
  | Valence Double

data TrackAttributeQualifier
  = Max
  | Min
  | Target

type SizeBetween min max = And (SizeGreaterThan min) (SizeLessThan max)

newtype TrackSeeds
  = TrackSeeds (Refined (SizeBetween 0 5) [TrackSeed])

data TrackSeed
  = SeedArtist SpotifyID
  | SeedGenre Text
  | SeedTrack SpotifyID

partitionTrackSeeds :: TrackSeeds -> ([SpotifyID], [Text], [SpotifyID])
partitionTrackSeeds (TrackSeeds refined) =
  foldr
    ( \seed (as, gs, ts) ->
        case seed of
          SeedArtist a -> (a : as, gs, ts)
          SeedGenre g -> (as, g : gs, ts)
          SeedTrack t -> (as, gs, t : ts)
    )
    ([], [], [])
    (unrefine refined)

instance (HasClient m api) => HasClient m (TrackSeeds :> api) where
  type Client m (TrackSeeds :> api) = TrackSeeds -> Client m api

  clientWithRoute :: Proxy m -> Proxy (TrackSeeds :> api) -> Request -> TrackSeeds -> Client m api
  clientWithRoute pm _ request trackSeeds =
    clientWithRoute
      pm
      (Proxy @api)
      ( request
          & appendToQueryString "seed_artists" (commaSepQueryParam artists')
          & appendToQueryString "seed_genres" (commaSepQueryParam genres')
          & appendToQueryString "seed_tracks" (commaSepQueryParam tracks')
      )
    where
      (artists', genres', tracks') = partitionTrackSeeds trackSeeds
      commaSepQueryParam :: ToHttpApiData a => [a] -> Maybe Text
      commaSepQueryParam = Just . Text.intercalate "," . fmap toQueryParam

  hoistClientMonad pm _ f client trackSeeds =
    hoistClientMonad pm (Proxy @api) f (client trackSeeds)

-- * Type Junk
--------------------------------------------------------------------------------

-- |
-- Used for @-XDerivingVia@ JSON instances with custom generic parsing
-- options.
newtype GenericJSON a = GenericJSON a

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (GenericJSON a) where
  parseJSON = fmap GenericJSON . genericParseJSON options
    where
      options =
        defaultOptions
          { fieldLabelModifier = \case
              "type_" -> "type"
              "show_" -> "show"
              other -> other
          }

type OptionalQueryParam = QueryParam' '[Optional, Strict]

type RequiredQueryParam = QueryParam' '[Required, Strict]

newtype ObjectWrapper (key :: Symbol) a = ObjectWrapper {unwrapObject :: a}
  deriving stock (Show)

instance (KnownSymbol key, FromJSON a) => FromJSON (ObjectWrapper key a) where
  parseJSON = withObject "ObjectWrapper" $ \obj -> do
    let k = Text.pack (symbolVal (Proxy @key))
    ObjectWrapper <$> obj .: k

newtype CommaSep p x = CommaSep (Refined p [x])

instance ToHttpApiData x => ToHttpApiData (CommaSep p x) where
  toQueryParam (CommaSep refined) =
    Text.intercalate "," (toQueryParam <$> unrefine refined)

data TypeTag (t :: Symbol) = TypeTag

instance KnownSymbol s => Show (TypeTag s) where
  show _ = "TypeTag \"" <> symbolVal (Proxy @s) <> "\""

instance KnownSymbol s => FromJSON (TypeTag s) where
  parseJSON = withText "TypeTag" $ \txt ->
    if txt == Text.pack (symbolVal (Proxy @s))
      then pure TypeTag
      else fail ("bad \"type\": " <> Text.unpack txt)

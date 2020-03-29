{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Weverything -Werror 
   -fno-warn-missing-export-lists
   -fno-warn-missing-import-lists 
   -fno-warn-all-missed-specialisations
   -fno-warn-unsafe #-}

module Web.Spotify.API.Client where

import qualified Data.CaseInsensitive as CI
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Refined (IdPred, SizeLessThan)
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv)
import Servant.Client.Core
  ( AuthenticatedRequest,
    BaseUrl (..),
    Scheme (Https),
    addHeader,
    mkAuthenticatedRequest,
  )
import Web.Spotify.API
import Prelude

-- * Albums

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/albums/get-album/>
getAlbum ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  Maybe Country ->
  ClientM AlbumFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/albums/get-albums-tracks/>
getAlbumsTracks ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  Maybe Country ->
  ClientM (Paging TrackFull)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/albums/get-several-albums/>
getSeveralAlbums ::
  AuthenticatedRequest SpotifyAuth ->
  CommaSep (SizeLessThan 20) SpotifyID ->
  Maybe Country ->
  ClientM (ObjectWrapper "albums" [Maybe AlbumFull])
(getAlbum :<|> getAlbumsTracks :<|> getSeveralAlbums) =
  client (Proxy @AlbumsAPI)

-- * Artists

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-artist/>
getArtist ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  ClientM ArtistFull

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-artists-albums/>
getArtistsAlbums ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  Maybe (CommaSep IdPred AlbumGroup) ->
  Maybe Country ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (Paging AlbumSimplified)

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-artists-top-tracks/>
getArtistsTopTracks ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  Country ->
  ClientM (ObjectWrapper "tracks" [TrackFull])

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-related-artists/>
getRelatedArtists ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  Country ->
  ClientM (ObjectWrapper "artists" [ArtistFull])

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/artists/get-several-artists/>
getSeveralArtists ::
  AuthenticatedRequest SpotifyAuth ->
  CommaSep (SizeLessThan 50) SpotifyID ->
  ClientM (ObjectWrapper "artists" [Maybe ArtistFull])
( getArtist :<|>
    getArtistsAlbums :<|>
    getArtistsTopTracks :<|>
    getRelatedArtists :<|>
    getSeveralArtists
  ) =
    client (Proxy @ArtistsAPI)

-- * Browse

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-category/>
getCategory ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  Maybe CountryCode ->
  Maybe Locale ->
  ClientM Category

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-categorys-playlists/>
getCategorysPlaylists ::
  AuthenticatedRequest SpotifyAuth ->
  SpotifyID ->
  Maybe CountryCode ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "playlists" (Paging PlaylistSimplified))

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-list-categories/>
getListCategories ::
  AuthenticatedRequest SpotifyAuth ->
  Maybe CountryCode ->
  Maybe Locale ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "categories" (Paging Category))

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-list-featured-playlists/>
getListFeaturedPlaylists ::
  AuthenticatedRequest SpotifyAuth ->
  Maybe Locale ->
  Maybe CountryCode ->
  Maybe Timestamp ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM FeaturedPlaylists

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-list-new-releases/>
getListNewReleases ::
  AuthenticatedRequest SpotifyAuth ->
  Maybe CountryCode ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM NewReleases

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/browse/get-recommendations/>
getRecommendations ::
  AuthenticatedRequest SpotifyAuth ->
  Maybe (Limit 100) ->
  Maybe Country ->
  TrackAttributes ->
  TrackSeeds ->
  ClientM Recommendations

-- |
-- <https://developer.spotify.com/console/get-available-genre-seeds/>
getAvailableGenreSeeds ::
  AuthenticatedRequest SpotifyAuth ->
  ClientM (ObjectWrapper "genres" [Text])
( getCategory :<|>
    getCategorysPlaylists :<|>
    getListCategories :<|>
    getListFeaturedPlaylists :<|>
    getListNewReleases :<|>
    getRecommendations :<|>
    getAvailableGenreSeeds
  ) =
    client (Proxy @BrowseAPI)

-- * Search

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/search/search/>
searchAlbum ::
  AuthenticatedRequest SpotifyAuth ->
  Text ->
  Maybe Country ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "albums" (Paging AlbumSimplified))

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/search/search/>
searchArtist ::
  AuthenticatedRequest SpotifyAuth ->
  Text ->
  Maybe Country ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "artists" (Paging ArtistFull))

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/search/search/>
searchPlaylist ::
  AuthenticatedRequest SpotifyAuth ->
  Text ->
  Maybe Country ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "playlists" (Paging PlaylistSimplified))

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/search/search/>
searchTrack ::
  AuthenticatedRequest SpotifyAuth ->
  Text ->
  Maybe Country ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "tracks" (Paging TrackFull))

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/search/search/>
searchShow ::
  AuthenticatedRequest SpotifyAuth ->
  Text ->
  Maybe Country ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "shows" (Paging ShowSimplified))

-- |
-- <https://developer.spotify.com/documentation/web-api/reference/search/search/>
searchEpisode ::
  AuthenticatedRequest SpotifyAuth ->
  Text ->
  Maybe Country ->
  Maybe (Limit 50) ->
  Maybe Offset ->
  ClientM (ObjectWrapper "episodes" (Paging EpisodeSimplified))
( searchAlbum :<|>
    searchArtist :<|>
    searchPlaylist :<|>
    searchTrack :<|>
    searchShow :<|>
    searchEpisode
  ) =
    client (Proxy @SearchAPI)

-- * Helpers

mkAuthenticatedRequest :: SpotifyToken -> AuthenticatedRequest SpotifyAuth
mkAuthenticatedRequest spotifyToken =
  Servant.Client.Core.mkAuthenticatedRequest spotifyToken $ \case
    BearerToken token _ ->
      addHeader (CI.mk "Authorization") ("Bearer " <> token)

spotifyAPIBaseUrl :: BaseUrl
spotifyAPIBaseUrl =
  BaseUrl
    { baseUrlScheme = Https,
      baseUrlHost = "api.spotify.com",
      baseUrlPort = 443,
      baseUrlPath = ""
    }

mkSpotifyClientEnv :: IO ClientEnv
mkSpotifyClientEnv = do
  httpsManager <- newManager tlsManagerSettings
  pure (mkClientEnv httpsManager spotifyAPIBaseUrl)

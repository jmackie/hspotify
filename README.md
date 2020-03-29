# hspotify

[![Build Status](https://travis-ci.org/jmackie/hspotify.svg?branch=master)](https://travis-ci.org/jmackie/hspotify)

Haskell client library for the [Spotify Web API](https://developer.spotify.com/documentation/web-api/).

## Progress

- [x] Albums
- [x] Artists
- [x] Browse
- [ ] Episodes
- [ ] Follow
- [ ] Library
- [ ] Personalization
- [ ] Player
- [ ] Playlists
- [x] Search
- [ ] Tracks
- [ ] Shows
- [ ] Users Profile

## Developing

Common tasks are provided as `make` targets.

```bash
make           # build and test
make format    # run ormolu
make docs      # build haddocks
make ghcid-lib # run ghcid
```

See `make help` for the full list.

Note in order to run the tests you'll need to create an application in your
[Spotify developer dashboard](https://developer.spotify.com/dashboard) and set
the client ID and secret in your environment:

```bash
export HSPOTIFY_TEST_CLIENT_ID=<your-client-id>
export HSPOTIFY_TEST_CLIENT_SECRET=<your-client-secret>
```

If you use [Nix](https://nixos.org/nix/) there's also a `shell.nix` that
provides all the necessary development tools.

```bash
nix-shell
```

## General noteso

- The modules are pretty hefty and take a while to compile. I think that's fine for now.
- I used `-XDuplicateRecordFields` because it was easier to write+read. This is
  fine if, as a consumer of the library, you use `-XNamedFieldPuns` (see the tests for examples).
- The user currently has to obtain their own `Bearer` token because I'm not
  sure there's a nice way of handling it from within this library.
  See the [Authorization Guide](https://developer.spotify.com/documentation/general/guides/authorization-guide/)
  for more details.

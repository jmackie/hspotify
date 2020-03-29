# hspotify

<!-- TODO: build badge -->

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

If you use [Nix](https://nixos.org/nix/) there's also a `shell.nix` that
provides all the necessary development tools.

```bash
nix-shell
```

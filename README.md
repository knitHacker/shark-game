# Shark Game

A menu-driven management sim where you play as a shark researcher. Plan trips to different marine habitats,
collect data on the shark species you encounter, and publish research papers to fund your next expedition.
The species facts and research methods in the game are based on real shark science.

## Gameplay

Plan research trips from your lab using your boat and equipment. Each piece of equipment you bring gives you
attempts to encounter sharks, with effectiveness varying by location.

Back at the lab, review your data and publish research papers once you have collected enough on a species.
Papers earn grant money, which funds further expeditions, better equipment, and bigger boats to reach new
habitats. Along the way you learn real facts about each species' habitat, diet, and behavior.

Fundraising (coming soon) will supplement your research budget through activities like collection boxes and
gala events.

## Building & Running

Requires [Stack](https://docs.haskellstack.org/) and the SDL2, SDL2_image, and SDL2_ttf libraries:

```bash
# Linux (Arch)
sudo pacman -S sdl2 sdl2_image sdl2_ttf

# Linux (Debian/Ubuntu)
sudo apt-get install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev

# macOS
brew install sdl2 sdl2_image sdl2_ttf
```

On Windows, the CI builds handle the SDL2 dependencies automatically. For local development, download the
SDL2 development libraries from the official releases.

Then build and run the game with:

```bash
stack build
stack run
```

`stack run` launches the game executable (`a-shark-game`). You can also run it directly with
`stack exec a-shark-game`.

Pre-built packages are attached to each tagged version on the
[releases page](https://github.com/knitHacker/shark-game/releases).

### Running from a Windows Release

Download and extract `shark-game-windows.zip`, then run the exe. This is a personal project without code signing, so
Windows will caution against running the executable — select "more info" to unlock a "run anyway" button.
If a console (terminal) window opens alongside the game, leave it open; closing it ends the game.

### Running from an Arch Linux Release

The Arch Linux release is a portable folder containing the game binary (`a-shark-game`), the SDL2 shared
libraries it was built against, and the game's `data/` and `assets/` folders. Download and extract
`shark-game-arch-linux.tar.gz`, then run the game from inside the folder — it looks for `data/` and `assets/` relative to the current directory:

```bash
cd shark-game-arch-linux
LD_LIBRARY_PATH="$PWD" ./a-shark-game
```

`LD_LIBRARY_PATH` points the binary at the bundled SDL2 libraries. If the system `sdl2`, `sdl2_image`, and
`sdl2_ttf` packages are installed, plain `./a-shark-game` works too. Saves are written to
`~/.local/share/shark-game/`, so they survive replacing the game folder with a newer release.

#### Steam Deck / Steam

SteamOS is Arch-based, so the Arch Linux release runs on the Steam Deck. To add it to Steam:

1. In Desktop Mode, download and extract the release somewhere permanent, e.g. `~/Games/shark-game`.
2. In Steam, go to **Games → Add a Non-Steam Game to My Library... → Browse** and select the
`a-shark-game` binary in the extracted folder.
3. Right-click the new library entry, open **Properties**, and:
    - set **Start In** to the extracted folder, so the game can find `data/` and `assets/`
    - set **Launch Options** to `LD_LIBRARY_PATH="$PWD" %command%`, so it uses the bundled SDL2 libraries
4. The game will now also appear in Gaming Mode.

The game has no native controller support yet, so in Gaming Mode use a controller layout that maps keys:
the game is played with the arrow keys, Enter, Escape, Backspace, Space, and I. A keyboard-style community
layout or a custom layout with those keys bound works well.

## Preview Tool

The `preview` executable renders a single game state in a window, which is useful for checking visuals
without playing through the game to reach that state.

```bash
stack exec preview -- <STATE> [SAVE-FILE] [--sharks N]
```

- `STATE` - the game state to render (see list below)
- `SAVE-FILE` - optional save file to load; without one the preview uses default (new game) data. Some
states show more when loaded with a save that has shark finds or research progress.
- `--sharks N` - number of caught sharks to show (only used by `trip-result`)

For example:
```bash
stack exec preview -- main-menu
stack exec preview -- trip-result --sharks 3
stack exec preview -- data-review-shark my-game.save
```

Menu selections work as normal within the state, but transitions to other states are not followed;
they are printed to the console instead.

Available states:

| Area | States |
|------|--------|
| Menus & intro | `main-menu`, `intro-welcome`, `intro-mission`, `intro-boat`, `intro-equip`, `intro-research`, `intro-funds`, `intro-end`, `research-center` |
| Trips | `trip-map`, `trip-equip`, `trip-review`, `trip-progress`, `trip-shark-found`, `trip-no-shark`, `trip-result` |
| Lab | `lab-management-top`, `lab-fundraiser-top`, `lab-donor-list`, `lab-fundraising`, `lab-fleet-management`, `lab-boat-store`, `lab-equip-management`, `lab-equip-store` |
| Data review | `data-review-top`, `data-review-sharks`, `data-review-shark`, `data-review-research`, `data-review-open-research`, `data-review-completed-research`, `data-review-investigate`, `data-review-award-grant`, `data-review-completed-review` |

## Roadmap

### Beta Release
- [ ] **Game balancing** - Adjust costs, grant amounts, encounter rates
- [ ] **Check accuracy of facts** - Verify all educational content for academic use
- [ ] **Fail states** - Can you run out of money? Should fundraising be unlimited?
- [ ] **Replayability** - Random events? Different starting regions?
- [ ] **Load other save files** - Allow user to load more than just the last saved game
- [ ] **Help menu** - Show how controls work and suggest next steps / how play works

### Future Feature Ideas
- [ ] Finish fundraiser feature
- [ ] Mini-games during trip
- [ ] Trip to fish market / docks for dead samples
- [ ] Different tag type deployments
- [ ] Triggered events
    - [ ] Fundraised money coming in over time
    - [ ] Tag updates
    - [ ] Washed up dead specimens
- [ ] Staff management
    - [ ] Hire / pay over time
    - [ ] Skills needed / pay to train
    - [ ] Allow multiple trips at once
- [ ] Lab / evaluation step between collecting shark information and publishing
- [ ] Lab live specimens for experiments?
- [ ] Transition from menu game to 2.5D sim?
- [ ] Animation of sharks
- [ ] Native controller support
- [ ] Change keybindings
- [ ] Glossary / general shark facts

## License

See [LICENSE](LICENSE) file for details.

# Shark Game

An educational game where you play as a shark researcher exploring different marine habitats, discovering shark species, and publishing research findings.

## Features

- **Multiple Regions & Biomes**: Explore the Tropical West Atlantic and Pacific Ocean, visiting various habitats including reefs, mangroves, kelp forests, estuaries, and open ocean.
- **Diverse Shark Species**: Encounter 30+ shark species, from nurse sharks to great whites, each with accurate biological information.
- **Research Mechanics**: Collect data through observation (snorkeling, scuba, BRUV) or catch-and-release methods, then publish research papers to earn grants.
- **Equipment Progression**: Start with basic fishing and snorkel gear, upgrade to advanced equipment like SCUBA gear, long lines, and underwater cameras.
- **Boat Management**: Purchase different boats to access various biomes - from small boats for shallow waters to large vessels for deep ocean exploration.
- **Educational Content**: Learn real facts about shark biology, behavior, diet, and conservation status through in-game research descriptions.

## Building & Running

### Prerequisites

- Stack (Haskell build tool)
- SDL2, SDL2_image, and SDL2_ttf libraries

#### Linux (Arch)
```bash
sudo pacman -S sdl2 sdl2_image sdl2_ttf
```

#### Linux (Debian/Ubuntu)
```bash
sudo apt-get install libsdl2-dev libsdl2-image-dev libsdl2-ttf-dev
```

#### macOS
```bash
brew install sdl2 sdl2_image sdl2_ttf
```

#### Windows
The CI builds handle SDL2 dependencies automatically. For local development, download the SDL2 development libraries from the official releases.

### Build & Run
```bash
stack build
stack run
```

## Game Mechanics

### Trips
Plan research trips to different locations using your available equipment and boat. Each trip gives you multiple attempts to encounter sharks based on your equipment's effectiveness.

### Research
Publish research papers by collecting sufficient data on shark species. Each paper earns grant money to fund further expeditions and equipment upgrades.

### Fundraising
Supplement your research budget through fundraising activities like collection boxes and gala events.

## Development

Built with:
- **Haskell** - Core game logic
- **SDL2** - Graphics and rendering
- **Stack** - Build system and dependency management

## Beta Release TODO

### Critical for Playability
- [ ] **Tutorial/First-time player guidance** - Simple text explaining game flow on first launch
- [ ] **Controls documentation** - In-game help screen showing keyboard/mouse controls
- [ ] **Save/load confirmation messages** - Let players know when saves succeed/fail
- [ ] **Goal indicators** - Show progress toward next research paper or equipment unlock
- [ ] **Feedback on actions** - Success/failure messages for trips, purchases, fundraising

### Art Assets
- [ ] Fill in entry page art
- [ ] Fill in research institute art
- [ ] Fill in equipment images
- [ ] Fill in shark species images
- [ ] Add music/sound effects

### Polish
- [ ] Come up with game name
- [ ] **Credits screen** - Cite educational sources (important for academic use!)
- [ ] **Settings menu** - Volume controls (if adding music), fullscreen toggle
- [ ] **Research dependency visualization** - Help players see what unlocks what
- [ ] **End-game content** - What happens when all research is complete? Congratulations screen?

### Game Design
- [ ] **Game balancing** - Adjust costs, grant amounts, encounter rates
- [ ] **Check accuracy of facts** - Verify all educational content for academic use
- [ ] **Difficulty curve** - Does the Pacific region unlock later? Should some equipment be gated by research progress?
- [ ] **Fail states** - Can you run out of money? Should fundraising be unlimited?
- [ ] **Replayability** - Random events? Different starting regions?

### Pre-release Testing
- [ ] **Playtest with someone unfamiliar** - Watch them play without helping
- [ ] **Bug pass** - Test all menu transitions, edge cases (buying with no money, etc.)
- [ ] **Typo check** - Proofread all text since it's educational

## Future Feature Ideas
- [ ] Finish fundraiser feature
- [ ] Mini-games during trip
- [ ] Trip to fish market / docks for
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

## License

See [LICENSE](LICENSE) file for details.

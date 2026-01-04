# Shark Game

An educational game where you play as a shark researcher exploring different marine habitats, discovering shark species,
and publishing research findings.

## Features

- **Multiple Regions & Biomes**: Explore various habitats including reefs, mangroves, estuaries, and open ocean.
- **Diverse Shark Species**: Encounter many shark species, from nurse sharks to great whites, each with accurate
biological information.
- **Research Mechanics**: Collect data through observation (snorkeling, scuba, BRUV) or catch-and-release methods,
then publish research papers to earn grants.
- **Equipment Progression**: Start with basic fishing and snorkel gear, upgrade to advanced equipment like SCUBA gear,
long lines, and underwater cameras.
- **Boat Management**: Purchase different boats to access various biomes - from small boats for shallow waters to
large vessels for deep ocean exploration.
- **Educational Content**: Learn real facts about shark biology, behavior, diet, and research methods.

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

### Running from Release

#### Windows
Download zip file then extract the zip file. Execute exe file. This is a personal project without certifications so Windows
will caution the user from running the executable. To run the executable select "more info" which will unlock a "run anyway" button. If a console (terminal) opens when running the game do not close or it will end the game.

## Game Mechanics

### Trips
Plan research trips to different locations using your available equipment and boat. Each trip gives you multiple attempts
to encounter sharks based on your equipment's effectiveness.

### Research
Publish research papers by collecting sufficient data on shark species. Each paper earns grant money to fund further
expeditions and equipment upgrades. Learn real facts about different species of sharks including their habitat, feeding
habits, and behaviors.

### Fundraising (coming soon)
Supplement your research budget through fundraising activities like collection boxes and gala events.

## Development

Built with:
- **Haskell** - Core game logic
- **SDL2** - Graphics and rendering
- **Stack** - Build system and dependency management

## Beta Release TODO

### Game Design
- [ ] **Game balancing** - Adjust costs, grant amounts, encounter rates
- [ ] **Check accuracy of facts** - Verify all educational content for academic use
- [ ] **Fail states** - Can you run out of money? Should fundraising be unlimited?
- [ ] **Replayability** - Random events? Different starting regions?
- [ ] **Load other save files** - Allow user to load more than just the last saved game
- [ ] **Help menu** - Show how controls work and suggest next steps / how play works

## Future Feature Ideas
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

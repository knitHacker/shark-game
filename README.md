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

## License

See [LICENSE](LICENSE) file for details.

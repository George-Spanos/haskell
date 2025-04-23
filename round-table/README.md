# Round Table

A Haskell implementation of the classic Minesweeper game with a web-based interface.

## Overview

This project implements a Minesweeper game using:
- Haskell (backend logic)
- Scotty (web server)
- JavaScript (frontend interaction)
- Cypress (end-to-end testing)

## Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/) for Haskell build management
- [Node.js](https://nodejs.org/) (v14+) and [pnpm](https://pnpm.io/) for running Cypress tests

## Running the Game

To run the Minesweeper game locally:

```bash
# Clone the repository (if you haven't already)
git clone <repository-url>
cd round-table

# Build and run the project
stack build
stack run
```

The game will be available at http://localhost:3001 in your web browser.

## Game Controls

- **Left Click**: Reveal a cell
- **Right Click**: Place or remove a flag
- **New Game Button**: Start a new game

## Running Tests

The project includes Cypress end-to-end tests to verify game functionality:

```bash
# Navigate to the Cypress directory
cd cypress

# Install dependencies (first time only)
pnpm install

# Run the tests
./scripts/tests.sh
```

### Test Features

- **Automated Testing**: The test script will:
  - Start the Haskell application
  - Verify all static resources load correctly (no 404 errors)
  - Run Cypress tests
  - Clean up processes when done

- **404 Detection**: The tests automatically fail if any static resources (CSS, JS) fail to load, providing early detection of configuration issues.

- **Test 404 Detection**: There's also a script to verify the 404 detection is working:

```bash
./scripts/test_404_detection.sh
```

## Project Structure

- `/src/Minesweeper/Game.hs`: Core game logic
- `/src/Minesweeper/Web.hs`: Web server and API
- `/static/`: CSS and JavaScript files for the frontend
- `/cypress/e2e/`: End-to-end tests
- `/cypress/scripts/`: Test automation scripts

## Development

This project follows modern Haskell best practices, including:
- Pure functional approach to game state
- Efficient data structures (Maps and Sets)
- Non-recursive algorithms for operations like revealing empty cells
- Comprehensive test coverage

---

Playing with Haskell and AI.
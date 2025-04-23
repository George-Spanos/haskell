#!/bin/bash
set -e

# Change to the cypress directory
cd "$(dirname "$0")/.."

# Display commands being executed
set -x

# Check if port 3001 is already in use
if lsof -Pi :3001 -sTCP:LISTEN -t >/dev/null ; then
    echo "Port 3001 is already in use. Killing the process..."
    lsof -Pi :3001 -sTCP:LISTEN -t | xargs kill
    sleep 1
fi

# Change to the project root directory to run the Haskell app
cd ..

# Start the Haskell application in the background
stack run &

APP_PID=$!

# Give the app time to start, but reduce the waiting time
echo "Waiting for application to start..."
sleep 2

# Change back to the cypress directory
cd cypress

# Skip the install step if node_modules exists
if [ -d "node_modules" ]; then
    echo "Node modules already installed, skipping install step"
else
    echo "Installing Node.js dependencies..."
    pnpm install --frozen-lockfile
fi

# Run Cypress tests
echo "Running Cypress tests..."
pnpm exec cypress run

# Store the exit code from Cypress
CYPRESS_EXIT_CODE=$?

# Kill the application process
kill $APP_PID 2>/dev/null || true

# Return the Cypress exit code
exit $CYPRESS_EXIT_CODE
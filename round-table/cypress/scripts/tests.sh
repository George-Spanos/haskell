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

# Check for 404 errors when loading the main page
echo "Checking for 404 errors on main page..."

# Fetch the main page HTML
curl -s http://localhost:3001/ > main_page.html

# Flag to track if any 404 errors are found
ANY_404_FOUND=0

# Extract ALL resource URLs (not just CSS and JS)
# Function to check a resource URL
check_resource() {
  local resource=$1
  local resource_type=$2
  
  if [[ $resource == /* ]]; then
    # Absolute path
    STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://localhost:3001$resource")
  else
    # Relative path
    STATUS=$(curl -s -o /dev/null -w "%{http_code}" "http://localhost:3001/$resource")
  fi
  
  echo "Checking $resource_type: $resource (Status: $STATUS)"
  
  if [ "$STATUS" -eq 404 ]; then
    echo "ERROR: 404 Not Found for $resource_type: $resource"
    return 1
  fi
  
  return 0
}

# 1. Check all href attributes (CSS, links, etc.)
if grep -q "href=" main_page.html; then
  HREF_RESOURCES=$(grep -o 'href="[^"]*"' main_page.html | sed 's/href=//g' | tr -d '"' | grep -v "^#" | grep -v "^mailto:" | grep -v "^http")
  for resource in $HREF_RESOURCES; do
    check_resource "$resource" "linked resource"
    if [ $? -ne 0 ]; then
      ANY_404_FOUND=1
    fi
  done
fi

# 2. Check all src attributes (JS, images, etc.)
if grep -q "src=" main_page.html; then
  SRC_RESOURCES=$(grep -o 'src="[^"]*"' main_page.html | sed 's/src=//g' | tr -d '"' | grep -v "^http")
  for resource in $SRC_RESOURCES; do
    check_resource "$resource" "embedded resource"
    if [ $? -ne 0 ]; then
      ANY_404_FOUND=1
    fi
  done
fi

# 3. Check any other attributes that might contain resources (data-src, etc.)
# Add more checks here if needed for other resource types

# Clean up
rm -f main_page.html

# Exit if any 404 errors were found
if [ $ANY_404_FOUND -eq 1 ]; then
  echo "ERROR: One or more resources returned 404 Not Found!"
  echo "Aborting tests."
  kill $APP_PID 2>/dev/null || true
  exit 1
fi

echo "All resources are loading correctly!"

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

# Check if we have a non-zero exit code from either static resource check or Cypress
if [ $CYPRESS_EXIT_CODE -ne 0 ]; then
  echo "Tests failed with exit code: $CYPRESS_EXIT_CODE"
  exit $CYPRESS_EXIT_CODE
fi

# If we got here, all tests passed!
echo "All tests completed successfully!"
exit 0
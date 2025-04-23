#\!/bin/bash
set -e

cd "$(dirname "$0")/../.."

# Backup the CSS file
mv static/css/minesweeper.css static/css/minesweeper.css.bak

# Run the tests (should fail due to 404)
echo "Running tests with missing CSS file (should fail with 404 detection)..."
./cypress/scripts/tests.sh
TEST_RESULT=$?

# Restore the CSS file
mv static/css/minesweeper.css.bak static/css/minesweeper.css

# Check if the test failed as expected
if [ $TEST_RESULT -ne 0 ]; then
  echo "SUCCESS: Test properly detected the 404 error\!"
  exit 0
else
  echo "FAILURE: Test did not detect the 404 error\!"
  exit 1
fi

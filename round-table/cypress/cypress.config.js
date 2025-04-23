const { defineConfig } = require('cypress');
const path = require('path');

module.exports = defineConfig({
  e2e: {
    baseUrl: 'http://localhost:3001',
    screenshotsFolder: 'screenshots',
    supportFile: path.resolve(__dirname, 'support', 'e2e.js'),
    specPattern: 'e2e/**/*.cy.js',
    setupNodeEvents(on, config) {
      // implement node event listeners here
    },
  },
});
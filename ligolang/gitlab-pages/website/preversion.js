// This script is used to remove 'next' from the versions.json file in order to re-create in the CI
const versions = require('./versions.json')
const fs = require('fs');

const versionsWithoutNext = versions
    .filter(version => version !== "next");

fs.writeFileSync("./versions.json", JSON.stringify(versionsWithoutNext));

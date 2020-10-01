const fs = require('fs');

// directory path
const dir = 'www';

// delete directory recursively
try {
  fs.rmdirSync(dir, {recursive: true});

  console.log(`${dir} is deleted!`);
} catch (err) {
  console.error(`Error while deleting ${dir}.`);
}

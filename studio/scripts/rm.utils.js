const fs = require('fs');
const path = require('path');

// delete directory recursively
module.exports.rmDir = (dir) => {
  try {
    fs.rmSync(dir, {recursive: true});

    console.log(`${dir} is deleted!`);
  } catch (err) {
    console.error(`Error while deleting ${dir}.`);
  }
};

module.exports.rmFiles = (dir, extension) => {
  try {
    let files = [];
    findFiles(dir, files, extension);

    for (const file of files) {
      fs.unlinkSync(file);
    }

    console.log(`${extension} files are deleted!`);
  } catch (err) {
    console.error(`Error while deleting ${extension} files.`);
  }
};

function findFiles(dir, files, extension) {
  fs.readdirSync(dir).forEach((file) => {
    const fullPath = path.join(dir, file);
    if (fs.lstatSync(fullPath).isDirectory()) {
      findFiles(fullPath, files, extension);
    } else if (path.extname(fullPath) === extension) {
      files.push(fullPath);
    }
  });
}

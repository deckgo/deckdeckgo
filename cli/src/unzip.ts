import fs from "fs";
import path from "path";

import { fromBuffer } from "yauzl";

export function unZipBuffer(buffer: Buffer, projectName: string) {
  return new Promise((resolve, reject) => {
    fromBuffer(
      buffer,
      { lazyEntries: true },
      handleZipFile(projectName, resolve, reject)
    );
  });
}

function handleZipFile(projectName: string, resolve: any, reject: any) {
  return (err: any, zipfile: any) => {
    if (err) {
      throw err;
    }

    // track when we've closed all our file handles
    zipfile.readEntry();
    zipfile.on("entry", (entry: any) => {
      let fileName: string;

      if (entry.fileName.indexOf("/") > -1) {
        const segments = entry.fileName.split("/");
        segments[0] = projectName;
        fileName = segments.join(path.sep);
      } else {
        fileName = projectName + path.sep + entry.fileName;
      }

      if (fileName[fileName.length - 1] === path.sep) {
        // Directory file names end with '/'.
        // Note that entires for directories themselves are optional.
        // An entry's fileName implicitly requires its parent directories to exist.
        zipfile.readEntry();
      } else {
        // ensure parent directory exists
        mkdirp(path.dirname(fileName), () => {
          zipfile.openReadStream(entry, (errL: any, readStream: any) => {
            if (errL) {
              throw errL;
            }
            readStream.on("end", () => {
              zipfile.readEntry();
            });
            // pump file contents
            readStream.pipe(fs.createWriteStream(fileName));
          });
        });
      }
    });
    zipfile.once("error", reject);
    zipfile.once("end", () => {
      resolve();
    });
  };
}

function mkdirp(dir: string, cb: any) {
  if (dir === ".") return cb();
  fs.stat(dir, (err) => {
    if (err == null) return cb(); // already exists

    const parent = path.dirname(dir);
    mkdirp(parent, () => {
      fs.mkdir(dir, cb);
    });
  });
}

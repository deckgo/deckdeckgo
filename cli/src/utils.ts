import { red, yellow } from "colorette";

import { ChildProcess, spawn } from "child_process";
import fs from "fs";
import { join } from "path";

const childrenProcesses: ChildProcess[] = [];

export function cleanup() {
  killChildren();
}

export function killChildren() {
  if (childrenProcesses && childrenProcesses.length > 0) {
    childrenProcesses.forEach((p) => p.kill("SIGINT"));
  }
}

export function npm(
  command: string,
  projectPath: string,
  stdio: any = "ignore"
) {
  return new Promise((resolve, reject) => {
    const p = spawn("npm", [command], {
      shell: true,
      stdio,
      cwd: projectPath,
    });
    p.once("exit", () => resolve());
    p.once("error", reject);
    childrenProcesses.push(p);
  });
}

export function rimraf(dir_path: string) {
  if (fs.existsSync(dir_path)) {
    fs.readdirSync(dir_path).forEach((entry) => {
      const entry_path = join(dir_path, entry);
      if (fs.lstatSync(entry_path).isDirectory()) {
        rimraf(entry_path);
      } else {
        fs.unlinkSync(entry_path);
      }
    });
    fs.rmdirSync(dir_path);
  }
}

export function nodeVersionWarning() {
  try {
    const v = process.version.replace("v", "").split(".");
    const major = parseInt(v[0], 10);
    const minor = parseInt(v[1], 10);
    if (major < 8 || (major === 8 && minor < 9)) {
      console.log(
        yellow(
          `Your current version of Node is ${process.version}, however the recommendation is a minimum of Node 8.x LTS.`
        )
      );
    }
  } catch (e) {
    console.error(`\n${red("✖")} ${e.message}\n`);
  }
}

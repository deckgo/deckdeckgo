// @ts-ignore
import { cyan, red, bold } from "colorette";

import { Spinner } from "cli-spinner";

import { downloadStarterMaster } from "./download";
import { installFont } from "./fonts";
import { unZipBuffer } from "./unzip";
import { cleanup, nodeVersionWarning, npm, rimraf } from "./utils";
import { getPkgVersion } from "./version";

const USAGE_DOCS = `Usage:
npm init deckdeckgo
`;

interface Answers {
  folder: string;
  title: string;
  description: string;
  author: string;
}

async function run() {
  const args = process.argv.slice(2);

  const help = args.indexOf("--help") >= 0 || args.indexOf("-h") >= 0;
  const info = args.indexOf("--info") >= 0;

  if (info) {
    console.log("create-deckdeckgo:", getPkgVersion(), "\n");
    return 0;
  }

  if (help) {
    console.log(USAGE_DOCS);
    return 0;
  }

  nodeVersionWarning();

  try {
    const questions = [
      {
        type: "input",
        name: "folder",
        message:
          "What's your project name (will be use to create a new folder)?",
        default: "deckdeckgo",
        validate: (input: string) => {
          if (input && input.length > 0) {
            return true;
          } else {
            return "Please provide a presentation's name";
          }
        },
      },
      {
        type: "input",
        name: "title",
        message:
          "What's your presentation name (max. 45 characters, will be use for meta tags and manifest information)?",
        default: "DeckDeckGo",
        validate: (input: string) => {
          if (input && input.length > 0 && input.length <= 45) {
            return true;
          } else {
            return "Your presentation's name should be at least one character and max. 45";
          }
        },
      },
      {
        type: "input",
        name: "description",
        message: "What's your presentation about (its description)?",
        default: "Create a lightweight presentation using Web Components 🚀",
      },
      {
        type: "input",
        name: "author",
        message:
          "What's your name (will be use for the author meta information)?",
        default: "David",
      },
    ];

    console.log(
      "\nCool, let's kick start a new " + cyan("DeckDeckGo") + " presentation\n"
    );

    const inquirer = require("inquirer");

    inquirer.prompt(questions).then(async (answers: Answers) => {
      await createPresentation(answers);

      console.log("Coolio, Your presentation is initialized.\n");

      await installFont(answers.folder);

      console.log(
        "Run " +
          cyan("npm run start") +
          " in the newly created folder " +
          cyan(answers.folder) +
          " to serve your presentation locally at the address " +
          cyan("http://localhost:3000") +
          "\n"
      );

      console.log(
        "Find this presentation in the remote control with the keyword: " +
          cyan(answers.title) +
          "\n"
      );
      console.log(
        "If you rather like not to use the remote control while developing your deck, run " +
          cyan("npm run start-no-remote") +
          " instead of the previous command\n"
      );

      console.log(
        'Dive deeper with the "Getting Started" guide at ' +
          cyan("https://docs.deckdeckgo.com") +
          "\n"
      );
    });
  } catch (e) {
    console.error(`\n${red("✖")} ${e.message}\n`);
  }

  cleanup();
}

function createPresentation(answers: Answers) {
  return new Promise(async (resolve, reject) => {
    try {
      await downloadInstallPresentation(answers);

      await installDependencies(answers);

      await updatePresentation(answers);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function downloadInstallPresentation(answers: Answers) {
  return new Promise(async (resolve, reject) => {
    try {
      const loading = new Spinner(bold("[1/3] Creating your presentation..."));
      loading.setSpinnerString(18);
      loading.start();

      // 1. Remove dir
      rimraf(answers.folder);

      // 2. Download starter
      const buffer = await downloadStarterMaster();
      await unZipBuffer(buffer, answers.folder);

      loading.stop(true);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function installDependencies(answers: Answers) {
  return new Promise(async (resolve, reject) => {
    try {
      const loading = new Spinner(bold("[2/3] Installing dependencies..."));
      loading.setSpinnerString(18);
      loading.start();

      // 3. Install dependencies
      await npm("ci", answers.folder);

      loading.stop(true);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function updatePresentation(answers: Answers) {
  return new Promise(async (resolve, reject) => {
    try {
      const loading = new Spinner(bold("[3/3] Updating presentation..."));
      loading.setSpinnerString(18);
      loading.start();

      // 4. Replace values in starter
      replaceAnswers(answers);

      loading.stop(true);

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

function replaceAnswers(answers: Answers) {
  const replaceResources = [
    answers.folder + "/src/index.html",
    answers.folder + "/src/manifest.json",
    answers.folder + "/webpack.config.js",
  ];

  const replace = require("replace");

  replace({
    regex: "{{DECKDECKGO_TITLE}}",
    replacement: answers.title,
    paths: replaceResources,
    recursive: false,
    silent: true,
  });

  replace({
    regex: "{{DECKDECKGO_SHORT_NAME}}",
    replacement:
      answers.title && answers.title.length > 12
        ? answers.title.substr(0, 12)
        : answers.title,
    paths: replaceResources,
    recursive: false,
    silent: true,
  });

  replace({
    regex: "{{DECKDECKGO_DESCRIPTION}}",
    replacement: answers.description,
    paths: replaceResources,
    recursive: false,
    silent: true,
  });

  replace({
    regex: "{{DECKDECKGO_AUTHOR}}",
    replacement: answers.description,
    paths: replaceResources,
    recursive: false,
    silent: true,
  });
}

(async () => {
  try {
    await run();
  } catch (e) {
    console.error(e);
  }
})();

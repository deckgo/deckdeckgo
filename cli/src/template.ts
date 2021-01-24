import {bold, cyan} from 'colorette';
import {Spinner} from 'cli-spinner';

import {rimraf} from './utils/utils';
import {downloadTemplateMaster} from './utils/download';
import {unZipBuffer} from './utils/unzip';
import {installDependencies} from './utils/install';

interface Answers {
  name: string;
}

export const initTemplate = async () => {
  const answers: Answers = await prompt();

  await downloadTemplate(answers);

  await installDependencies(answers.name, '2/2');

  info(answers);
};

const info = (answers: Answers) => {
  console.log('\nThe template has been cloned in the newly created folder ' + cyan(answers.name) + '\n');

  console.log('Dive deeper with the "Getting Started" guide of its ' + cyan('README.md') + '\n');
};

const prompt = (): Promise<Answers> => {
  const questions = [
    {
      type: 'input',
      name: 'name',
      message: "What's your template name (will be use to create a new folder)?",
      validate: (input: string) => {
        const reservedKeywords: boolean = /(?:deckgo|deckdeckgo|ddg)/.test(input);

        if (input && input.length > 0 && !reservedKeywords) {
          return true;
        } else if (reservedKeywords) {
          return 'The name should not contain any of the reserved keywords: deckgo, deckdeckgo or ddg';
        } else {
          return 'Please provide a template name';
        }
      }
    }
  ];

  console.log("\nSweet, let's start a new " + cyan('DeckDeckGo') + ' template\n');

  const inquirer = require('inquirer');

  return inquirer.prompt(questions);
};

const downloadTemplate = async (answers: Answers) => {
  const loading = new Spinner(bold('[1/2] Creating your template...'));
  loading.setSpinnerString(18);
  loading.start();

  // 1. Remove dir
  rimraf(answers.name);

  // 2. Download starter
  const buffer = await downloadTemplateMaster();
  await unZipBuffer(buffer, answers.name);

  loading.stop(true);
};

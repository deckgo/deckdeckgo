import {red} from 'colorette';

import {cleanup, nodeVersionWarning} from './utils/utils';
import {getPkgVersion} from './utils/version';
import {initPresentation} from './presentation';

const USAGE_DOCS = `Usage:
npm init deckdeckgo
`;

async function run() {
  const args = process.argv.slice(2);

  const help = args.indexOf('--help') >= 0 || args.indexOf('-h') >= 0;
  const info = args.indexOf('--info') >= 0;

  if (info) {
    console.log('create-deckdeckgo:', getPkgVersion(), '\n');
    return 0;
  }

  if (help) {
    console.log(USAGE_DOCS);
    return 0;
  }

  nodeVersionWarning();

  try {
    await initPresentation();
  } catch (e) {
    console.error(`\n${red('✖')} ${e.message}\n`);
  }

  cleanup();
}

(async () => {
  try {
    await run();
  } catch (e) {
    console.error(e);
  }
})();

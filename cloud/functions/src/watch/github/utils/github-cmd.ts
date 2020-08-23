import simpleGit, {SimpleGit} from 'simple-git';

import * as os from 'os';
import * as path from 'path';

import {deleteDir} from './github-fs';

export async function clone(url: string) {
  // TODO replace test with project name
  // TODO prefix tmp dir test with username or a uuid? just in case
  const localPath: string = path.join(os.tmpdir(), 'test');

  // Just in case, tmp directory are not shared across functions
  await deleteDir(localPath);

  const git: SimpleGit = simpleGit();

  await git.clone(url, localPath);
}

export async function checkoutBranch() {
  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');
  const git: SimpleGit = simpleGit(localPath);

  // TODO: Branch name? Reuse same branch name if PR is merged?
  await git.checkout(['-B', 'deckdeckgo']);
}

export async function pull(url: string) {
  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');
  const git: SimpleGit = simpleGit(localPath);

  const result: string | undefined = await git.listRemote([url, 'deckdeckgo']);

  if (!result || result === undefined || result === '') {
    // The branch does not exist yet, therefore we should not perform a pull (it would throw an error "fatal: couldn't find remote ref deckdeckgo")
    return;
  }

  // TODO: Branch name? Reuse same branch name if PR is merged?
  await git.pull(url, 'deckdeckgo');
}

export async function commit(name: string, email: string) {
  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');
  const git: SimpleGit = simpleGit(localPath);

  await git.addConfig('user.name', name);
  await git.addConfig('user.email', email);

  //  TODO replace test with project name
  const indexPath: string = path.join(localPath, 'src', 'index.html');

  // TODO: commit msg
  await git.commit('feat: last changes', [indexPath]);
}

export async function push(githubToken: string, name: string, email: string, login: string, project: string) {
  try {
    //  TODO replace test with project name
    const localPath: string = path.join(os.tmpdir(), 'test');
    const git: SimpleGit = simpleGit(localPath);

    await git.addConfig('user.name', name);
    await git.addConfig('user.email', email);

    await git.push(`https://${login}:${githubToken}@github.com/${login}/${project}.git`, 'deckdeckgo');
  } catch (err) {
    // We catch the errors and parse a custom error instead in order to not print the token in the logs
    // TODO branch name
    throw new Error(`Error while pushing changes to branch deckdeckgo for ${login}/${project}`);
  }
}

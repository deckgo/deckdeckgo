import simpleGit, {SimpleGit} from 'simple-git';

import {deleteDir, getLocalFilePath, getLocalPath} from './github-fs';

export async function clone(url: string, login: string, project: string) {
  const localPath: string = getLocalPath(login, project);

  // Just in case, tmp directory are not shared across functions
  await deleteDir(localPath);

  const git: SimpleGit = simpleGit();

  await git.clone(url, localPath);
}

export async function checkoutBranch(login: string, project: string, branch: string) {
  const git: SimpleGit = getSimpleGit(login, project);

  await git.checkout(['-B', branch]);
}

export async function pull(url: string, login: string, project: string, branch: string) {
  const git: SimpleGit = getSimpleGit(login, project);

  const result: string | undefined = await git.listRemote([url, branch]);

  if (!result || result === undefined || result === '') {
    // The branch does not exist yet, therefore we should not perform a pull (it would throw an error "fatal: couldn't find remote ref deckdeckgo")
    return;
  }

  await git.pull(url, branch);
}

export async function commit(name: string, email: string, login: string, project: string) {
  const git: SimpleGit = getSimpleGit(login, project);

  await git.addConfig('user.name', name);
  await git.addConfig('user.email', email);

  const indexPath: string = getLocalFilePath(login, project, 'src', 'index.html');
  const manifestPath: string = getLocalFilePath(login, project, 'src', 'manifest.json');

  const msg: string = 'feat: slides update';

  await git.commit(msg, [indexPath, manifestPath]);
}

export async function push(githubToken: string, name: string, email: string, login: string, project: string, branch: string) {
  try {
    const git: SimpleGit = getSimpleGit(login, project);

    await git.addConfig('user.name', name);
    await git.addConfig('user.email', email);

    await git.push(`https://${login}:${githubToken}@github.com/${login}/${project}.git`, branch);
  } catch (err) {
    // We catch the errors and parse a custom error instead in order to not print the token in the logs
    throw new Error(`Error while pushing changes to branch ${branch} for ${login}/${project}`);
  }
}

export function getSimpleGit(login: string, project: string): SimpleGit {
  const localPath: string = getLocalPath(login, project);
  return simpleGit(localPath);
}

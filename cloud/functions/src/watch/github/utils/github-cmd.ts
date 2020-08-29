import * as functions from 'firebase-functions';

import simpleGit, {SimpleGit} from 'simple-git';

import {deleteDir, getLocalFilePath, getLocalPath} from './github-fs';

interface GitHubDeckGoUser {
  email: string;
  name: string;
}

export async function clone(url: string, login: string, project: string) {
  const localPath: string = getLocalPath(login, project);

  // Just in case, tmp directory are not shared across functions
  await deleteDir(localPath);

  const exist: boolean = await waitForRepoExist(url);

  if (!exist) {
    throw new Error('Repo not found and cannot be cloned');
  }

  const git: SimpleGit = simpleGit();

  await git.clone(url, localPath);
}

function waitForRepoExist(url: string): Promise<boolean> {
  return new Promise<boolean>((resolve) => {
    const git: SimpleGit = simpleGit();

    const interval = setInterval(async () => {
      const exist: boolean = await repoExist(git, url);

      if (exist) {
        clearInterval(interval);

        resolve(true);
      }
    }, 500);

    setTimeout(() => {
      clearInterval(interval);

      resolve(false);
    }, 10000);
  });
}

async function repoExist(git: SimpleGit, url: string): Promise<boolean> {
  const result: string | undefined = await git.listRemote([url, 'master']);

  return result !== null && result !== undefined && result !== '';
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

export async function commitDeck(login: string, project: string) {
  const user: GitHubDeckGoUser = getGitHubUser();

  const git: SimpleGit = getSimpleGit(login, project);

  await git.addConfig('user.name', user.name);
  await git.addConfig('user.email', user.email);

  const indexPath: string = getLocalFilePath(login, project, 'src', 'index.html');
  const manifestPath: string = getLocalFilePath(login, project, 'src', 'manifest.json');

  const msg: string = 'feat: slides update';

  await git.commit(msg, [indexPath, manifestPath]);
}

export async function commit(login: string, project: string, msg: string, ...files: string[]) {
  const user: GitHubDeckGoUser = getGitHubUser();

  const git: SimpleGit = getSimpleGit(login, project);

  await git.addConfig('user.name', user.name);
  await git.addConfig('user.email', user.email);

  const localPath: string = getLocalFilePath(login, project, ...files);

  await git.commit(msg, [localPath]);
}

export async function push(githubToken: string, login: string, project: string, branch: string) {
  try {
    const user: GitHubDeckGoUser = getGitHubUser();

    const git: SimpleGit = getSimpleGit(login, project);

    await git.addConfig('user.name', user.name);
    await git.addConfig('user.email', user.email);

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

function getGitHubUser(): GitHubDeckGoUser {
  // DeckDeckGo friendly robot / GitHub user information
  const email: string = functions.config().github.email;
  const name: string = functions.config().github.name;

  return {
    email,
    name,
  };
}

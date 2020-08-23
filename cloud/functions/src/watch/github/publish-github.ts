import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import * as admin from 'firebase-admin';

import simpleGit, {SimpleGit} from 'simple-git';

import * as rimraf from 'rimraf';

import {promises as fs} from 'fs';
import * as os from 'os';
import * as path from 'path';

import {DeckData} from '../../model/deck';
import {Token, TokenData} from '../../model/token';

import {isDeckPublished} from '../screenshot/utils/update-deck';

import {createPR, findOrCreateRepo, getUser, GitHubRepo, GitHubUser} from './utils/github-api';

export async function publishToGitHub(change: functions.Change<DocumentSnapshot>) {
  const newValue: DeckData = change.after.data() as DeckData;

  const previousValue: DeckData = change.before.data() as DeckData;

  if (!newValue || !newValue.meta || !newValue.meta.published || !newValue.meta.pathname) {
    return;
  }

  if (!newValue.owner_id || newValue.owner_id === undefined || newValue.owner_id === '') {
    return;
  }

  if (!newValue.meta.title || newValue.meta.title === '') {
    return;
  }

  const update: boolean = await isDeckPublished(previousValue, newValue);

  if (!update) {
    return;
  }

  try {
    const userToken: Token = await findToken(newValue.owner_id);

    if (!userToken || !userToken.data || !userToken.data.github || !userToken.data.github.token) {
      return;
    }

    // For the user with her/his token

    const user: GitHubUser = await getUser(userToken.data.github.token);

    if (!user) {
      return;
    }

    // TODO
    // const project: string = newValue.meta.title.replace(' ', '-');

    const repo: GitHubRepo | undefined = await findOrCreateRepo(userToken.data.github.token, user);

    if (!repo || repo === undefined || !repo.url) {
      return;
    }

    //TODO: In the future, if the repo is an existing one, sync dependencies within the PR aka compare these with source repo and provide change to upgrade repo.

    // As DeckDeckGo
    const email: string = functions.config().github.email;
    const name: string = functions.config().github.name;

    await clone(repo.url);

    await checkoutBranch();

    await pull(repo.url);

    await parseDeck();

    await commit(name, email);

    await push(userToken.data.github.token, name, email, user.login, 'test');

    await createPR(userToken.data.github.token, repo.id);
  } catch (err) {
    console.error(err);
  }
}

function findToken(userId: string): Promise<Token> {
  return new Promise<Token>(async (resolve, reject) => {
    try {
      const snapshot: admin.firestore.DocumentSnapshot = await admin.firestore().doc(`/tokens/${userId}/`).get();

      if (!snapshot.exists) {
        reject('Token not found');
        return;
      }

      const tokenData: TokenData = snapshot.data() as TokenData;

      resolve({
        id: snapshot.id,
        ref: snapshot.ref,
        data: tokenData,
      });
    } catch (err) {
      reject(err);
    }
  });
}

async function clone(url: string) {
  // TODO replace test with project name
  // TODO prefix tmp dir test with username or a uuid? just in case
  const localPath: string = path.join(os.tmpdir(), 'test');

  // Just in case, tmp directory are not shared across functions
  await deleteDir(localPath);

  const git: SimpleGit = simpleGit();

  await git.clone(url, localPath);
}

function deleteDir(localPath: string): Promise<void> {
  return new Promise<void>((resolve) => {
    rimraf(localPath, () => {
      resolve();
    });
  });
}

async function checkoutBranch() {
  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');
  const git: SimpleGit = simpleGit(localPath);

  // TODO: Branch name? Reuse same branch name if PR is merged?
  await git.checkout(['-B', 'deckdeckgo']);
}

async function pull(url: string) {
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

async function commit(name: string, email: string) {
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

async function push(githubToken: string, name: string, email: string, login: string, project: string) {
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

// https://cloud.google.com/functions/docs/concepts/exec#file_system

function parseDeck(): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      // TODO use and replace real and all content
      // TODO update all files not just index.html

      //  TODO replace test with project name
      const localPath: string = path.join(os.tmpdir(), 'test');

      const indexPath: string = path.join(localPath, 'src', 'index.html');

      const data = await fs.readFile(indexPath, 'utf8');

      let result = data.replace(/\{\{DECKDECKGO_TITLE\}\}/g, 'test');
      result = result.replace(/\{\{DECKDECKGO_AUTHOR\}\}/g, 'david');

      await fs.writeFile(indexPath, result, 'utf8');

      resolve();
    } catch (err) {
      reject(err);
    }
  });
}

// (async () => {
//   try {
//     const userToken = '';
//     const ddgToken = '';
//
//     const user = await getUser(userToken);
//     console.log('User', user);
//     const repo: GitHubRepo | undefined = await findOrCreateRepo(userToken, user);
//     console.log('Repo', repo);
//     await forkRepo(ddgToken, (repo as GitHubRepo).nameWithOwner);
//   } catch (e) {
//     console.error(e);
//   }
// })();

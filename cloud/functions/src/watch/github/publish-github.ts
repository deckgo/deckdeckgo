import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import * as admin from 'firebase-admin';

import fetch, {Response} from 'node-fetch';

import simpleGit, {SimpleGit} from 'simple-git';

import * as rimraf from 'rimraf';

import {promises as fs} from 'fs';
import * as os from 'os';
import * as path from 'path';

import {DeckData} from '../../model/deck';
import {Token, TokenData} from '../../model/token';

import {isDeckPublished} from '../screenshot/utils/update-deck';

import {GitHubForkResponse} from '../../types/github';

interface GitHubUser {
  id: string;
  login: string;
}

interface GitHubRepo {
  id: string;
  url: string;
  nameWithOwner: string;
}

export async function publishToGitHub(change: functions.Change<DocumentSnapshot>) {
  const newValue: DeckData = change.after.data() as DeckData;

  const previousValue: DeckData = change.before.data() as DeckData;

  if (!newValue || !newValue.meta || !newValue.meta.published || !newValue.meta.pathname) {
    return;
  }

  if (!newValue.owner_id || newValue.owner_id === undefined || newValue.owner_id === '') {
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

    const repo: GitHubRepo | undefined = await findOrCreateRepo(userToken.data.github.token, user);

    if (!repo || repo === undefined || !repo.url) {
      return;
    }

    //TODO: In the future, if the repo is an existing one, sync dependencies within the PR aka compare these with source repo and provide change to upgrade repo.

    // As DeckDeckGo

    // const token: string = functions.config().github.token;
    const email: string = functions.config().github.email;
    const name: string = functions.config().github.name;

    // TODO: do we need a fork?
    // const fork: GitHubForkResponse = await forkRepo(token, repo.nameWithOwner);

    await clone(repo.url); // fork.clone_url

    await createBranch();

    await parseDeck();

    await commit(name, email);

    await push(userToken.data.github.token, name, email, user.login, 'test'); // token, name, email, fork.name

    // TODO create PR
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

// https://stackoverflow.com/questions/43853853/does-cloud-functions-for-firebase-support-file-operation

// https://cloud.google.com/functions/docs/concepts/exec#file_system

// https://developer.github.com/v4/explorer/

// https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-template-repository

function getUser(githubToken: string): Promise<GitHubUser> {
  return new Promise<GitHubUser>(async (resolve, reject) => {
    try {
      const query = `
        query {
          viewer {
            id,
            login
          }
        }
      `;

      const response: Response = await queryGitHub(githubToken, query);

      const result = await response.json();

      resolve(result.data.viewer);
    } catch (err) {
      console.error('Cannot retrieve user id.', err);
      reject(err);
    }
  });
}

function findOrCreateRepo(githubToken: string, user: GitHubUser): Promise<GitHubRepo | undefined> {
  return new Promise<GitHubRepo | undefined>(async (resolve, reject) => {
    try {
      if (!user) {
        resolve(undefined);
        return;
      }

      // TODO replace "test" (the repo name) with deck title formatted

      const query = `
        query {
          repository(owner:"${user.login}", name:"test") {
            id,
            url,
            nameWithOwner
          }
        }
      `;

      const response: Response = await queryGitHub(githubToken, query);

      const repo = await response.json();

      // Repo already exists
      if (repo && repo.data && repo.data.repository) {
        resolve(repo.data.repository);
        return;
      }

      // Create a new repo
      const newRepo: GitHubRepo | undefined = await createRepo(githubToken, user);

      // TODO setInterval  resolve until ready aka queryGitHub until ready
      setTimeout(() => {
        resolve(newRepo);
      }, 2000);
    } catch (err) {
      console.error('Unexpected error while finding the repo.', err);
      reject(err);
    }
  });
}

function createRepo(githubToken: string, user: GitHubUser): Promise<GitHubRepo | undefined> {
  return new Promise<GitHubRepo>(async (resolve, reject) => {
    try {
      if (!user) {
        resolve(undefined);
        return;
      }

      // TODO: Update const from new repo, that's the ID of the starter kit
      const repositoryId: string = 'MDEwOlJlcG9zaXRvcnkxNTM0MDk2MTg=';

      // TODO: Description and title from deck data

      const query = `
        mutation CloneTemplateRepository {
          cloneTemplateRepository(input:{description:"Hello",includeAllBranches:false,name:"test",repositoryId:"${repositoryId}",visibility:PUBLIC,ownerId:"${user.id}"}) {
            clientMutationId,
            repository {
              id,
              url,
              nameWithOwner
            }
          }
        }
      `;

      const response: Response = await queryGitHub(githubToken, query);

      const result = await response.json();

      if (!result || !result.data || !result.data.cloneTemplateRepository || result.errors) {
        resolve(undefined);
        return;
      }

      resolve(result.data.cloneTemplateRepository.repository);
    } catch (err) {
      console.error('Unexpected error while creating the repo.', err);
      reject(err);
    }
  });
}

async function queryGitHub(githubToken: string, query: string): Promise<Response> {
  const githubApiV4: string = 'https://api.github.com/graphql';

  const rawResponse: Response = await fetch(`${githubApiV4}`, {
    method: 'POST',
    headers: {
      Accept: 'application/json',
      'Content-Type': 'application/json',
      Authorization: `token ${githubToken}`,
    },
    body: JSON.stringify({query}),
  });

  if (!rawResponse || !rawResponse.ok) {
    console.error(rawResponse);
    throw new Error('Cannot perform GitHub query.');
  }

  return rawResponse;
}

// https://github.com/steveukx/git-js

export function forkRepo(githubToken: string, nameWithOwner: string): Promise<GitHubForkResponse> {
  return new Promise<GitHubForkResponse>(async (resolve, reject) => {
    try {
      const githubApiV3: string = 'https://api.github.com/repos';

      console.log('FORKING', `${githubApiV3}/${nameWithOwner}/forks`);

      const rawResponse: Response = await fetch(`${githubApiV3}/${nameWithOwner}/forks`, {
        method: 'POST',
        headers: {
          Accept: 'application/json',
          'Content-Type': 'application/json',
          Authorization: `token ${githubToken}`,
        },
      });

      if (!rawResponse || !rawResponse.ok) {
        console.error(rawResponse);
        reject(new Error('Error forking the repo.'));
        return;
      }

      const results: GitHubForkResponse = await rawResponse.json();

      if (!results) {
        reject(new Error('Error no response when forking.'));
        return;
      }

      // TODO: fork might take some times aka queryGitHub until ready like every setInterval 100ms max 5 seconds throw error
      setTimeout(() => {
        resolve(results);
      }, 2000);
    } catch (err) {
      console.error('Unexpected error forking the repo.');
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

async function createBranch() {
  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');
  const git: SimpleGit = simpleGit(localPath);

  // TODO: Branch name? Reuse same branch name if PR is merged?
  await git.checkoutLocalBranch('deckdeckgo');

  console.log('CHECKOUT');
}

async function commit(name: string, email: string) {
  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');
  const git: SimpleGit = simpleGit(localPath);

  await git.addConfig('user.name', name);
  await git.addConfig('user.email', email);

  console.log('CONFIG', await git.listConfig());

  // TODO: commit msg
  await git.commit('feat: last changes');

  console.log('COMMIT');
}

async function push(githubToken: string, name: string, email: string, login: string, project: string) {
  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');
  const git: SimpleGit = simpleGit(localPath);

  await git.addConfig('user.name', name);
  await git.addConfig('user.email', email);

  await git.push(`https://${login}:${githubToken}@github.com/${login}/${project}.git`, 'deckdeckgo');

  console.log('PUSH');
}

function parseDeck(): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      // TODO use and replace real and all content
      // TODO update all files not just index.html

      //  TODO replace test with project name
      const localPath: string = path.join(os.tmpdir(), 'test');

      const indexPath: string = path.join(localPath, 'src', 'index.html');

      const data = await fs.readFile(indexPath, 'utf8');

      const result = data.replace(/\{\{DECKDECKGO_TITLE\}\}/g, 'test');

      await fs.writeFile(indexPath, result, 'utf8');

      console.log('DECK PARSED', result);

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

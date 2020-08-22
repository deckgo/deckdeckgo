import {Change} from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';
import * as admin from 'firebase-admin';

import fetch, {Response} from 'node-fetch';

import simpleGit, {SimpleGit} from 'simple-git';
const git: SimpleGit = simpleGit();

import * as rimraf from 'rimraf';

import * as os from 'os';
import * as path from 'path';

import {DeckData} from '../../model/deck';
import {Token, TokenData} from '../../model/token';

import {isDeckPublished} from '../screenshot/utils/update-deck';

interface GitHubUser {
  id: string;
  login: string;
}

interface GitHubRepo {
  id: string;
  url: string;
}

export async function publishToGitHub(change: Change<DocumentSnapshot>) {
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
    const token: Token = await findToken(newValue.owner_id);

    if (!token || !token.data || !token.data.github || !token.data.github.token) {
      return;
    }

    const user: GitHubUser = await getUser(token.data.github.token);

    const repo: GitHubRepo | undefined = await findOrCreateRepo(token.data.github.token, user);

    //TODO: In the future, if the repo is an existing one, sync dependencies within the PR aka compare these with source repo and provide change to upgrade repo.

    await clone(repo);

    // TODO: create branch
    // TODO: parse deck
    // TODO: create PR
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

      // TODO replace test with deck title formatted

      const query = `
        query {
          repository(owner:"${user.login}", name:"test") {
            id,
            url
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

      resolve(newRepo);
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
              url
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

async function clone(repo: GitHubRepo | undefined) {
  if (!repo || repo === undefined || !repo.url) {
    return;
  }

  //  TODO replace test with project name
  const localPath: string = path.join(os.tmpdir(), 'test');

  await deleteDir(localPath);

  await git.clone(repo.url, localPath);
}

function deleteDir(localPath: string): Promise<void> {
  return new Promise<void>((resolve) => {
    rimraf(localPath, () => {
      resolve();
    });
  });
}

// (async () => {
//   try {
//     const token = '';
//
//     const user = await getUser(token);
//     console.log('User', user);
//     const repo: GitHubRepo | undefined = await findOrCreateRepo(token, user);
//     console.log('Repo', repo);
//   } catch (e) {
//     console.error(e);
//   }
// })();

// https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-template-repository

// https://developer.github.com/v4/explorer/

import * as functions from 'firebase-functions';

import fetch, {Response} from 'node-fetch';

import {DeckGitHubRepo} from '../../../../model/data/deck';

export interface GitHubUser {
  id: string;
  login: string;
}

export function getUser(githubToken: string): Promise<GitHubUser> {
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

export function findOrCreateRepo(githubToken: string, user: GitHubUser, project: string, description: string): Promise<DeckGitHubRepo | undefined> {
  return new Promise<DeckGitHubRepo | undefined>(async (resolve, reject) => {
    try {
      const repo: DeckGitHubRepo | undefined = await findRepo(githubToken, user, project);

      // Repo already exists
      if (repo) {
        resolve(repo);
        return;
      }

      // Create a new repo otherwise
      const newRepo: DeckGitHubRepo | undefined = await createRepo(githubToken, user, project, description);

      resolve(newRepo);
    } catch (err) {
      console.error('Unexpected error while finding the repo.', err);
      reject(err);
    }
  });
}

export function findRepo(githubToken: string, user: GitHubUser, project: string): Promise<DeckGitHubRepo | undefined> {
  return new Promise<DeckGitHubRepo | undefined>(async (resolve, reject) => {
    try {
      if (!user) {
        resolve(undefined);
        return;
      }

      const query = `
        query {
          repository(owner:"${user.login}", name:"${project}") {
            id,
            url,
            name,
            nameWithOwner
          }
        }
      `;

      const response: Response = await queryGitHub(githubToken, query);

      const repo = await response.json();

      if (!repo || !repo.data || !repo.data.repository) {
        resolve(undefined);
        return;
      }

      resolve(repo.data.repository);
    } catch (err) {
      console.error('Unexpected error while fetching the repo.', err);
      reject(err);
    }
  });
}

export function createRepo(githubToken: string, user: GitHubUser, project: string, description: string): Promise<DeckGitHubRepo | undefined> {
  return new Promise<DeckGitHubRepo | undefined>(async (resolve, reject) => {
    try {
      if (!user) {
        resolve(undefined);
        return;
      }

      const repositoryId: string = functions.config().github.template.id;

      const query = `
        mutation CloneTemplateRepository {
          cloneTemplateRepository(input:{description:"${description}",includeAllBranches:false,name:"${project}",repositoryId:"${repositoryId}",visibility:PUBLIC,ownerId:"${user.id}"}) {
            clientMutationId,
            repository {
              id,
              url,
              name,
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

export function createPR(githubToken: string, repositoryId: string, branch: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      const title: string = 'feat: update from DeckDeckGo';
      const body: string = `Hi 👋

Here are the recent changes you made to your slides on [DeckDeckGo](https://deckdeckgo.com).`;

      const query = `
        mutation CreatePullRequest {
          createPullRequest(input:{baseRefName:"master",body:"${body}",headRefName:"${branch}",repositoryId:"${repositoryId}",title:"${title}"}) {
            pullRequest {
              id
            }
          }
        }
      `;

      const response: Response = await queryGitHub(githubToken, query);

      const result = await response.json();

      if (!result || !result.data || !result.data.createPullRequest || result.errors) {
        resolve(undefined);
        return;
      }

      resolve();
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

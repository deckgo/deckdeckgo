// https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/creating-a-template-repository

// https://developer.github.com/v4/explorer/

import fetch, {Response} from 'node-fetch';

export interface GitHubUser {
  id: string;
  login: string;
}

export interface GitHubRepo {
  id: string;
  url: string;
  nameWithOwner: string;
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

export function findOrCreateRepo(githubToken: string, user: GitHubUser): Promise<GitHubRepo | undefined> {
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

export function createRepo(githubToken: string, user: GitHubUser): Promise<GitHubRepo | undefined> {
  return new Promise<GitHubRepo | undefined>(async (resolve, reject) => {
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

export function createPR(githubToken: string, repositoryId: string): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      // TODO: Description and title from deck data
      // TODO: branch name

      const query = `
        mutation CreatePullRequest {
          createPullRequest(input:{baseRefName:"master",body:"Hello",headRefName:"deckdeckgo",repositoryId:"${repositoryId}",title:"Hello World"}) {
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

import * as functions from 'firebase-functions';

import {DeckData, DeckGitHubRepo} from '../../../model/data/deck';
import {Token} from '../../../model/data/token';

import {getUser, GitHubUser} from './utils/github-api';
import {clone} from './utils/github-cmd';
import {findToken} from './utils/github-db';
import {getRepo, updateGitHubDeck, updateProject} from './utils/github-utils';
import {failureDeploy, successfulDeploy} from '../../../utils/data/deck-deploy-utils';

export async function publishToGitHub(deckId: string, deckData: DeckData): Promise<void> {
  return new Promise<void>(async (resolve, reject) => {
    try {
      if (!deckData || !deckData.meta || !deckData.meta.published || !deckData.meta.pathname) {
        reject('No deck meta data.');
        return;
      }

      if (!deckData.owner_id || deckData.owner_id === undefined || deckData.owner_id === '') {
        reject('No deck owner.');
        return;
      }

      if (!deckData.meta.title || deckData.meta.title === '') {
        reject('No deck title.');
        return;
      }

      const gitHubSkip: string = functions.config().github.skip;

      if (gitHubSkip === 'true') {
        await successfulDeploy(deckId, 'github');

        resolve();
        return;
      }

      // Has the user a GitHub token?

      const platform: Token = await findToken(deckData.owner_id);

      if (!platform || !platform.data || !platform.data.github || !platform.data.github.token) {
        reject('No token to access GitHub.');
        return;
      }

      // Get GitHub user information such as id and username (login)

      const user: GitHubUser = await getUser(platform.data.github.token);

      if (!user) {
        reject('No GitHub user resolved.');
        return;
      }

      // Get or create GitHub repo / project

      const repo: DeckGitHubRepo | undefined = await getRepo(platform.data.github.token, user, deckData.owner_id, deckId, deckData);

      if (!repo || repo === undefined || !repo.url || !repo.name) {
        reject('No GitHub repo found or created.');
        return;
      }

      //TODO: In the future, if the repo is an existing one, sync dependencies within the PR aka compare these with source repo and provide change to upgrade repo.

      await clone(repo.url, user.login, repo.name);

      await updateProject(platform.data.github.token, user.login, repo.name, repo.url, deckData.meta);

      await updateGitHubDeck(platform.data.github.token, user, repo, deckData.meta);

      await successfulDeploy(deckId, 'github');

      resolve();
    } catch (err) {
      await failureDeploy(deckId, 'github');

      reject(err);
    }
  });
}

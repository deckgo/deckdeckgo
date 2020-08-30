import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData} from '../../model/deck';
import {Token} from '../../model/token';
import {DeployGitHubRepo} from '../../model/deploy';

import {isDeckPublished} from '../screenshot/utils/update-deck';

import {getUser, GitHubUser} from './utils/github-api';
import {clone} from './utils/github-cmd';
import {findToken} from './utils/github-db';
import {getRepo, updateDeck, updateProject} from './utils/github-utils';

export async function publishToGitHub(change: functions.Change<DocumentSnapshot>, context: functions.EventContext) {
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

  if (newValue.meta.github !== true) {
    return;
  }

  const update: boolean = await isDeckPublished(previousValue, newValue);

  if (!update) {
    return;
  }

  try {
    // Has the user a GitHub token?

    const platform: Token = await findToken(newValue.owner_id);

    if (!platform || !platform.data || !platform.data.github || !platform.data.github.token) {
      return;
    }

    // Get GitHub user information such as id and username (login)

    const user: GitHubUser = await getUser(platform.data.github.token);

    if (!user) {
      return;
    }

    // Get or create GitHub repo / project

    const deckId: string = context.params.deckId;

    const repo: DeployGitHubRepo | undefined = await getRepo(platform.data.github.token, user, newValue.owner_id, deckId, newValue.meta);

    if (!repo || repo === undefined || !repo.url || !repo.name) {
      return;
    }

    //TODO: In the future, if the repo is an existing one, sync dependencies within the PR aka compare these with source repo and provide change to upgrade repo.

    await clone(repo.url, user.login, repo.name);

    await updateProject(platform.data.github.token, user.login, repo.name, repo.url, newValue.meta);

    await updateDeck(platform.data.github.token, user, repo, newValue.meta);
  } catch (err) {
    console.error(err);
  }
}

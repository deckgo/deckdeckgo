import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData} from '../../model/deck';
import {Token} from '../../model/token';

import {isDeckPublished} from '../screenshot/utils/update-deck';

import {createPR, findOrCreateRepo, getUser, GitHubRepo, GitHubUser} from './utils/github-api';
import {checkoutBranch, clone, commit, pull, push} from './utils/github-cmd';
import {parseDeck} from './utils/github-fs';
import {findToken} from './utils/github-db';

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
    // Has the useer a GitHub token?

    const userToken: Token = await findToken(newValue.owner_id);

    if (!userToken || !userToken.data || !userToken.data.github || !userToken.data.github.token) {
      return;
    }

    // Get GitHub user information such as id and username (login)

    const user: GitHubUser = await getUser(userToken.data.github.token);

    if (!user) {
      return;
    }

    const project: string = newValue.meta.title.replace(' ', '-');

    // Get or create GitHub repo / project

    const repo: GitHubRepo | undefined = await findOrCreateRepo(userToken.data.github.token, user, project);

    if (!repo || repo === undefined || !repo.url) {
      return;
    }

    //TODO: In the future, if the repo is an existing one, sync dependencies within the PR aka compare these with source repo and provide change to upgrade repo.

    // DeckDeckGo friendly robot / GitHub user information
    const email: string = functions.config().github.email;
    const name: string = functions.config().github.name;

    // Working branch name
    const branch: string = functions.config().github.branch;

    await clone(repo.url, user.login, project);

    await checkoutBranch(user.login, project, branch);

    await pull(repo.url, user.login, project, branch);

    await parseDeck(user.login, project);

    await commit(name, email, user.login, project);

    await push(userToken.data.github.token, name, email, user.login, project, branch);

    await createPR(userToken.data.github.token, repo.id, branch);
  } catch (err) {
    console.error(err);
  }
}

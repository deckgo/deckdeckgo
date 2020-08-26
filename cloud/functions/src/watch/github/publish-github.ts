import * as functions from 'firebase-functions';
import {DocumentSnapshot} from 'firebase-functions/lib/providers/firestore';

import {DeckData, DeckMeta} from '../../model/deck';
import {Platform} from '../../model/platform';
import {GitHubRepo, PlatformDeck, PlatformDeckData} from '../../model/platform-deck';

import {isDeckPublished} from '../screenshot/utils/update-deck';

import {createPR, createRepo, findOrCreateRepo, findRepo, getUser, GitHubUser} from './utils/github-api';
import {checkoutBranch, clone, commit, pull, push} from './utils/github-cmd';
import {parseDeck} from './utils/github-fs';
import {findPlatform, findPlatformDeck, updatePlatformDeck} from './utils/github-db';

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

  const update: boolean = await isDeckPublished(previousValue, newValue);

  if (!update) {
    return;
  }

  try {
    // Has the user a GitHub token?

    const platform: Platform = await findPlatform(newValue.owner_id);

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

    const repo: GitHubRepo | undefined = await getRepo(platform.data.github.token, user, newValue.owner_id, deckId, newValue.meta);

    if (!repo || repo === undefined || !repo.url || !repo.name) {
      return;
    }

    //TODO: In the future, if the repo is an existing one, sync dependencies within the PR aka compare these with source repo and provide change to upgrade repo.

    // DeckDeckGo friendly robot / GitHub user information
    const email: string = functions.config().github.email;
    const name: string = functions.config().github.name;

    // Working branch name
    const branch: string = functions.config().github.branch;

    await clone(repo.url, user.login, repo.name);

    await checkoutBranch(user.login, repo.name, branch);

    await pull(repo.url, user.login, repo.name, branch);

    await parseDeck(user.login, repo.name, newValue.meta);

    await commit(name, email, user.login, repo.name);

    await push(platform.data.github.token, name, email, user.login, repo.name, branch);

    await createPR(platform.data.github.token, repo.id, branch);
  } catch (err) {
    console.error(err);
  }
}

async function getRepo(githubToken: string, user: GitHubUser, userId: string, deckId: string, deckMeta: DeckMeta): Promise<GitHubRepo | undefined> {
  const project: string = deckMeta.title.replace(' ', '-');
  const description: string = deckMeta.description ? (deckMeta.description as string) : '';

  const platformDeck: PlatformDeck | undefined = await findPlatformDeck(userId, deckId);

  if (platformDeck) {
    const existingRepo: GitHubRepo | undefined = await findRepo(githubToken, user, platformDeck.data.github.repo.name);

    if (existingRepo) {
      // We update our information because the user may have renamed its repo. For example, the new repo name ("hello world world") is returned when looking with the old repo name ("hello world")
      await updatePlatformDeck(userId, deckId, platformDeck.data, existingRepo);

      return existingRepo;
    }

    // The user may have delete its repo

    const createdRepo: GitHubRepo | undefined = await createRepo(githubToken, user, project, description);
    await updatePlatformDeck(userId, deckId, platformDeck.data, createdRepo);

    return createdRepo;
  }

  const repo: GitHubRepo | undefined = await findOrCreateRepo(githubToken, user, project, description);

  if (!repo) {
    return undefined;
  }

  const data: PlatformDeckData = {
    github: {
      repo,
    },
  };

  await updatePlatformDeck(userId, deckId, data, repo);

  return repo;
}

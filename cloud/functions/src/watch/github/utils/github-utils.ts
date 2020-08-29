import {DeckMeta} from '../../../model/deck';
import {GitHubRepo, PlatformDeck, PlatformDeckData} from '../../../model/platform-deck';

import {createPR, createRepo, findOrCreateRepo, findRepo, GitHubUser} from './github-api';
import {findPlatformDeck, updatePlatformDeck} from './github-db';
import {parseDeck, parseInfo, shouldUpdate} from './github-fs';
import {checkoutBranch, commitDeck, commit, pull, push} from './github-cmd';
import * as functions from 'firebase-functions';

export async function getRepo(githubToken: string, user: GitHubUser, userId: string, deckId: string, deckMeta: DeckMeta): Promise<GitHubRepo | undefined> {
  const project: string = deckMeta.title.replace(' ', '-').toLowerCase();
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

export async function updateProject(githubToken: string, login: string, project: string, url: string, meta: DeckMeta) {
  // README.md
  await updateInfo(githubToken, login, project, url, meta, 'docs: update presentation info', 'README.md');

  // Webpack dev server configuration
  await updateInfo(githubToken, login, project, url, meta, 'chore: update project settings', 'webpack.config.js');

  // Icons path
  await updateInfo(githubToken, login, project, url, meta, 'chore: update icons path', 'src', 'scripts', 'menu.js');
  await updateInfo(githubToken, login, project, url, meta, 'chore: update icons path', 'src', 'scripts', 'modalNotes.js');
  await updateInfo(githubToken, login, project, url, meta, 'chore: update icons path', 'src', 'scripts', 'remotePopover.js');

  await push(githubToken, login, project, 'master');
}

async function updateInfo(githubToken: string, login: string, project: string, url: string, meta: DeckMeta, msg: string, ...files: string[]) {
  const needUpdate: boolean = await shouldUpdate(login, project, ...files);
  if (!needUpdate) {
    return;
  }

  await parseInfo(login, project, url, meta, ...files);

  await commit(login, project, msg, ...files);
}

export async function updateDeck(githubToken: string, user: GitHubUser, repo: GitHubRepo, meta: DeckMeta) {
  // Working branch name
  const branch: string = functions.config().github.branch;

  await checkoutBranch(user.login, repo.name, branch);

  await pull(repo.url, user.login, repo.name, branch);

  await parseDeck(user.login, repo.name, meta);

  await commitDeck(user.login, repo.name);

  await push(githubToken, user.login, repo.name, branch);

  await createPR(githubToken, repo.id, branch);
}

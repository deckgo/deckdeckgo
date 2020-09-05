import * as functions from 'firebase-functions';

import {DeckData, DeckGitHubRepo, DeckMeta} from '../../../../model/data/deck';

import {createPR, createRepo, findOrCreateRepo, findRepo, GitHubUser} from './github-api';
import {updateDeckGitHub} from './github-db';
import {parseDeck, parseInfo, shouldUpdate} from './github-fs';
import {checkoutBranch, commitDeck, commit, pull, push} from './github-cmd';

export async function getRepo(githubToken: string, user: GitHubUser, userId: string, deckId: string, deckData: DeckData): Promise<DeckGitHubRepo | undefined> {
  if (!deckData || !deckData.meta) {
    return undefined;
  }

  const project: string = deckData.meta.title.replace(' ', '-').toLowerCase();
  const description: string = deckData.meta.description ? (deckData.meta.description as string) : '';

  if (deckData.github && deckData.github.repo) {
    const existingRepo: DeckGitHubRepo | undefined = await findRepo(githubToken, user, deckData.github.repo.name);

    if (existingRepo) {
      // We update our information because the user may have renamed its repo. For example, the new repo name ("hello world world") is returned when looking with the old repo name ("hello world")
      await updateDeckGitHub(deckId, existingRepo);

      return existingRepo;
    }

    // The user may have delete its repo

    const createdRepo: DeckGitHubRepo | undefined = await createRepo(githubToken, user, project, description);
    await updateDeckGitHub(deckId, createdRepo);

    return createdRepo;
  }

  const repo: DeckGitHubRepo | undefined = await findOrCreateRepo(githubToken, user, project, description);

  if (!repo) {
    return undefined;
  }

  await updateDeckGitHub(deckId, repo);

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

export async function updateGitHubDeck(githubToken: string, user: GitHubUser, repo: DeckGitHubRepo, meta: DeckMeta) {
  // Working branch name
  const branch: string = functions.config().github.branch;

  await checkoutBranch(user.login, repo.name, branch);

  await pull(repo.url, user.login, repo.name, branch);

  await parseDeck(user.login, repo.name, meta);

  await commitDeck(user.login, repo.name);

  await push(githubToken, user.login, repo.name, branch);

  await createPR(githubToken, repo.id, branch);
}

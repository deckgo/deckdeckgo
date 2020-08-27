import {DeckMeta} from '../../../model/deck';
import {GitHubRepo, PlatformDeck, PlatformDeckData} from '../../../model/platform-deck';

import {createRepo, findOrCreateRepo, findRepo, GitHubUser} from './github-api';
import {findPlatformDeck, updatePlatformDeck} from './github-db';
import {parseReadme, shouldUpdateReadme} from './github-fs';
import {commitReadme, push} from './github-cmd';

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

export async function updateReadme(githubToken: string, name: string, email: string, login: string, project: string, url: string, meta: DeckMeta) {
  const needUpdate: boolean = await shouldUpdateReadme(login, project);
  if (!needUpdate) {
    return;
  }

  await parseReadme(login, project, url, meta);

  await commitReadme(name, email, login, project);

  await push(githubToken, name, email, login, project, 'master');
}
